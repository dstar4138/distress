%%% The Distress API Handler
%%%
%%%     This will take the decoded messages from the network and process
%%%     them. There are three primary messages, Insert/Select/Delete.
%%%
%%% @author Alexander Dean
-module( distress_api ).

%% General Debuggery
-include("debug.hrl").

-export([fun_desc/0, handle/5]).

%% Private Exports
-export([f/3,g/1,composefg/3]).


%%%===================================================================
%%% Callback API functions
%%%===================================================================

%% @doc To start a swarm acceptor pool we need a handler function, this
%%   function returns that handler function description.
%% @end
fun_desc() -> {?MODULE, handle, []}.


%% @doc Start off the Handle loop.
handle( Socket, _Name, Transport, Info, Args ) ->
    ?DEBUG("HANDLER STARTED: (~p,~p,~p,~p)",[Socket, _Name, Transport, Info]),
    case distress_conn:validate_connection( Info ) of
        true -> handle_loop( {Socket, Transport, Args, <<>>} );
        false -> ok
    end.


%%%===================================================================
%%% Local Magic Value Handling
%%%===================================================================

%% @private
%% @doc Magic value generation.
f( Oid, Key, Value ) ->
    HashedKey = distress_util:hash( sha256, Key ), % Get Key to right length
    S = crypto:aes_ctr_stream_init(HashedKey, Oid), % Generate cypher
    {_,Magic} = crypto:aes_ctr_stream_encrypt(S, Value), % Make Magic
    distress_util:hash( sha, Magic ).    % Compress encryption to CID length

%% @private
%% @doc Magic value local masking.
g( Magic ) ->
    C1 = crypto:hmac_init(sha, distressd:get_myid() ),
    C2 = crypto:hmac_update( C1, Magic ),
    crypto:hmac_final( C2 ).

%% @private
%% @doc Local magic value checking.
composefg( Oid, Key, Value ) -> g(f(Oid,Key,Value)).

%%%===================================================================
%%% Internal Message Handling
%%%===================================================================

%% @hidden
%% @doc Handle the message that comes in or close the handle loop. The message
%%   handling will jump back into this loop after processing.
%% @end
handle_loop( Dat ) ->
    case recv( Dat ) of
        {error, closed} -> close( Dat );
        {error, Reason} -> ?ERROR("Socket Error: ~p",[Reason]);
        {ok, Message}   -> handle_msg( Message, Dat )
    end.

%% @hidden
%% @doc Handles messages once an Add-File message appears, will stay in this
%%   mode until client closes or we hit the end of the block count. This will
%%   then drop back to handle_loop/1.
%% @end
handle_loop_add_block( 0, Dat, _State ) ->
    ?DEBUG("Exiting add-block mode"),
    handle_loop( Dat );
handle_loop_add_block( NumBlocks, Dat, State ) ->
    case recv( Dat ) of
        {error,closed} ->
           ?DEBUG("Client closed before block count was met.");
        {ok, Packet} ->
            (case handle_msg_add_block( Packet, Dat, State, 0 ) of
                 {ok, NewDat, N} ->
                     handle_loop_add_block( NumBlocks-N, NewDat, State );
                 {get,NewDat, N} ->
                     handle_loop_add_block( NumBlocks-N, NewDat, State )
            end);
        {error,R} ->
            send( Dat, distress_cmsg:encode_err(R))
    end.

%% @hidden
%% @doc Handle each of the API messages and either return to the loop or close
%%   out.
%% @end
handle_msg( Msg, Dat ) ->
    Buffer = get_buffer( Dat ),
    case distress_cmsg:decode( Msg, Buffer ) of
        {ok, Val, NextBuff} ->
            NewDat = set_buffer(Dat, NextBuff),
            case Val of
                {error,Error} ->
                    send( NewDat, distress_cmsg:encode_err(Error) );
%                    handle_loop( NewDat );
                Term -> do( Term, NewDat )
            end,
            handle_msg(<<>>,NewDat);%Got a message, so check if we have more.
        {get, NextBuff} ->
            NewDat = set_buffer(Dat, NextBuff),
            handle_loop( NewDat )
    end.

%%%===================================================================
%%% Server Side Block API
%%%===================================================================

%% @hidden
%% @doc Update the locally read buffer.
get_buffer( {_Socket, _Transport, _, Buff} ) -> Buff.
set_buffer( {S,T,A,_}, Buff ) -> {S,T,A,Buff}.

%% @hidden
%% @doc Send/Recv the message on the provided socket and transport.
send(  {Socket, Transport, _, _}, Msg ) -> Transport:send( Socket, Msg ).
recv(  {Socket, Transport, _, _} ) -> Transport:recv( Socket, 0, infinity ).
close( {Socket, Transport, _, _} ) -> Transport:close( Socket ).

%% @hidden
%% @doc Based on message type, do an action or set of actions.
do( Msg, Dat ) ->
    case distress_cmsg:get_type( Msg ) of
        add -> do_add( Msg, Dat );
        get -> do_get( Msg, Dat );
        del -> do_del( Msg, Dat );
        Type -> ?ERROR("Bad message type: ~p",[Type])
    end.

%% @hidden
%% @doc File accessing via a hash value.
do_get( Msg, Dat ) ->
    case distress_cmsg:get_value( Msg, key ) of
        undefined ->
            ?DEBUG("Get message missing 'key' value: ~p",[Msg]),
            send( Dat, distress_cmsg:encode_err( badarg ) );
        Key ->
            (case get_block( Key ) of
                {error, R} -> send( Dat, distress_cmsg:encode_geterr( Key, R ) );
                {ok, Val}  ->
                     ?DEBUG("Success on GET KEY: ~p",[Key]),
                     send( Dat, distress_cmsg:encode_get( Key, Val ) )
            end)
    end.
get_block( Key ) ->
    case distress_db:select_block( Key ) of
        {error, Reason} -> {error, Reason};
        [] -> {error, missing}; %LATER: Check with P2P Overlay for hash.
        Val -> {ok, Val}
    end.

%% @hidden
%% @doc Handles the add file logic
do_add( Msg, Dat ) ->
    case distress_cmsg:get_value( Msg, numblocks ) of
        NumBlocks when is_integer(NumBlocks) andalso NumBlocks > 0 ->
            Oid = distress_util:uuid(),
            send( Dat, distress_cmsg:encode_ack( Oid ) ),
            Expires = get_if_expires( Msg ),
            Removable = get_if_removable( Msg ),
            Magic = gen_magic(Oid, Removable),
            State = {Magic, Expires},
            handle_loop_add_block( NumBlocks, Dat, State );
        _ ->
            send( Dat, distress_cmsg:encode_err(badarg) )
    end.
handle_msg_add_block( Packet, Dat, State, N ) ->
    Buffer = get_buffer( Dat ),
    case distress_cmsg:decode( Packet, Buffer ) of
        {ok, Msg, NextBuff} ->
            case clean_keyval( Msg ) of
                {error, R} ->
                    send( Dat, distress_cmsg:encode_err(R)),
                    {ok, set_buffer(Dat, NextBuff), N};
                Pair ->
                    add_block( Pair, State ),
                    send( Dat, distress_cmsg:encode_scs() ),
                    handle_msg_add_block( <<>>,
                                          set_buffer(Dat, NextBuff),
                                          State,
                                          N+1)
            end;
        {get,NextBuff} -> {get, set_buffer( Dat, NextBuff ), N}
    end.

add_block( {Key, Block} = BlockPair, {Magic, Expires} ) ->
    {MyMagic, _Pass} = Magic( BlockPair ),
    %LATER: Broadcast adds into P2P Overlay for replication with Pass as magic.
    case distress_db:insert_block( Key, Expires, Block, MyMagic ) of
        [] -> ?DEBUG("Successful adding.");
        Errors -> {error,Errors}
    end.
gen_magic( _Oid, false ) ->
    fun({_Key,_Val})-> {nil,nil} end;
gen_magic( Oid, true ) ->
    fun({Key,Value})->
            Magic = f( Oid, Key, Value ),
            MyMagic = g( Magic ),
            {MyMagic, Magic}
    end.

%% @hidden
%% @doc Block Removal logic, check if key/oid given then send to DB.
do_del( Msg, Dat ) ->
    case clean_del( Msg ) of
        {error, Err} -> send( Dat, distress_cmsg:encode_err( Err ) );
        {Oid, Key} ->
            (case del_block( Oid, Key ) of
                {error, R} ->
                     send( Dat, distress_cmsg:encode_err( R ) );
                _ ->
                     ?DEBUG("Successful Del of ~p",[Key])
            end)
    end.
del_block( Oid, Key ) ->
    case
       distress_db:delete_block( Key, Oid, fun ?MODULE:composefg/3 )
    of
        {error, Reason} -> {error, Reason};
        ok -> ok %LATER: Broadcast dels into P2P Overlay since it was success.
    end.

%% @hidden
%% @doc Clean the delete messages in normal packet mode.
clean_del( Msg ) ->
    case distress_cmsg:get_value( Msg, key ) of
        undefined ->
            ?DEBUG("Del message missing 'key' value: ~p",[Msg]);
        Key ->
            (case distress_cmsg:get_value( Msg, oid ) of
                undefined ->
                    ?DEBUG("Del message missing 'oid' value: ~p",[Msg]);
                Oid -> {Oid, Key}
             end)
    end.

%% @hidden
%% @doc Clean the block messages in a add-block mode. See
%%  handle_loop_add_block/3.
clean_keyval( {error, _Err} = Err) ->
    ?DEBUG( "Add_packet message invalid: ~p",[_Err] ), Err;
clean_keyval( Msg ) ->
    case distress_cmsg:get_value( Msg, key ) of
        undefined ->
            ?DEBUG("Add_packet message missing 'key' value: ~p",[Msg]),
            {error,badarg};
        Key ->
            (case distress_cmsg:get_value( Msg, val ) of
                undefined ->
                    ?DEBUG("Add_packet message missing 'val' value: ~p",[Msg]),
                    {error,badarg};
                Val -> {Key, Val}
            end)
    end.

%% @hidden
%% @doc Removable defaults to 'false' if the message does not include it.
get_if_removable( Msg ) ->
    case distress_cmsg:get_value( Msg, removable ) of
        undefined -> false; R -> clean_removable( R )
    end.
clean_removable( true )  -> true;
clean_removable( false ) -> false;
clean_removable( Error ) -> ?ERROR("Bad removable value: ~p",[Error]).

%% @hidden
%% @doc Expiration defaults to 'infinity' if the message does not include it.
get_if_expires( Msg ) ->
    case distress_cmsg:get_value( Msg, expires ) of
       undefined -> infinity; R -> clean_expires( R )
    end.
clean_expires( <<"infinity">> ) -> infinity;
clean_expires( <<"inf">> ) -> infinity;
clean_expires( Time ) when is_integer( Time ) andalso Time > 0 -> Time;
clean_expires( Err ) -> ?ERROR("Bad Expires value: ~p",[Err]).

