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
handle( Socket, _Name, Transport, _Info, Args ) ->
    ?DEBUG("HANDLER STARTED: (~p,~p,~p,~p)",[Socket, _Name, Transport, _Info]),
    handle_loop( {Socket, Transport}, Args ).


%%%===================================================================
%%% Local Magic Value Handling
%%%===================================================================

%% @private
%% @doc Magic value generation.
f( Oid, Key, Value ) ->
    State = crypto:stream_init(aes_ctr,Key,Oid),
    {_,Magic} = crypto:stream_encrypt(State,crypto:exor(Oid,Value)),
    Magic.

%% @private
%% @doc Magic value local masking.
g( Magic ) -> crypto:exor( Magic, distress:get_myid() ).

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
handle_loop( {Socket, Transport} = Dat, Args ) ->
    case Transport:recv( Socket, 0, infinity ) of %TODO: Args should have timeout.
        {error, closed} -> Transport:close( Socket );
        {error, Reason} -> ?ERROR("Socket Error: ~p",[Reason]);
        {ok, Message}   -> handle_msg( Message, Args, Dat )
    end.

%% @hidden
%% @doc Handle each of the API messages and either return to the loop or close
%%   out.
%% @end  
handle_msg( Msg, Args, Dat ) ->
    case distress_cmsg:decode( Msg ) of
        {error,invalid_json,_} -> 
            send( Dat, distress_cmsg:encode_err(invalid_json) ),
            handle_loop( Dat, Args );
        Term -> do( Term, Dat )
    end.

%%%===================================================================
%%% Server Side Block API
%%%===================================================================

%% @hidden
%% @doc Send the message on the provided socket and transport.
send( {Socket,Transport}, Msg ) -> Transport:send( Socket, Msg ).

%% @hidden
%% @doc 
do( Msg, Dat ) ->
    case distress_cmsg:get_type( Msg ) of
        add ->
            Oid = distress_util:uuid(),
            send( Dat, distress_cmsg:encode_ack( Oid ) ), 
            Expires = case distress_cmsg:get_value( Msg, expires ) of
                undefined -> infinity; E -> clean_expires( E )
            end,
            Removable = case distress_cmsg:get_value( Msg, removable ) of
                undefined -> false; R -> clean_removable( R )
            end, 
            handle_add_loop( Oid, Expires, Removable, Dat, [] ); 
        get -> 
            case distress_cmsg:get_value( Msg, key ) of
                undefined -> 
                    ?DEBUG("Get message missing 'key' value: ~p",[Msg]),
                    send( Dat, distress_cmsg:encode_err( badarg ) );
                Key -> do_get( Key, Dat )
            end;
        del -> 
            case distress_cmsg:get_value( Msg, key ) of
                undefined ->
                    ?DEBUG("Del message missing 'key' value: ~p",[Msg]),
                    send( Dat, distress_cmsg:encode_err( badarg ) );
                Key -> 
                    (case distress_cmsg:get_value( Msg, oid ) of
                        undefined ->
                            ?DEBUG("Del message missing 'oid' value: ~p",[Msg]),
                            send( Dat, distress_cmsg:encode_err( badarg ) );
                        Oid -> do_del( Oid, Key, Dat )
                    end)
            end;
        Type -> ?ERROR("Bad message type: ~p",[Type])
    end.

clean_expires( <<"infinity">> ) -> infinity;
clean_expires( <<"inf">> ) -> infinity;
clean_expires( Time ) when is_integer( Time ) andalso Time > 0 -> Time;
clean_expires( Err ) -> ?ERROR("Bad Expires value: ~p",[Err]).
clean_removable( true )  -> true;
clean_removable( false ) -> false;
clean_removable( Error ) -> ?ERROR("Bad removable value: ~p",[Error]).
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
                Val -> {Key,Val}
            end)
    end.


%% @hidden
%% @doc File accessing via a hash value.
do_get( Key, Dat ) ->
    case get_block( Key ) of
        {error, R} -> send( Dat, distress_cmsg:encode_err( R ) );
        {ok, Val}  -> send( Dat, distress_cmsg:encode_get( Key, Val ) )
    end.
get_block( Key ) ->
    case distress_db:select_block( Key ) of
        {error, Reason} -> {error, Reason};
        [] -> {error, missing};
        Val -> {ok, Val}
    end.

%% @hidden
%% @doc Handles the add file logic
handle_add_loop( Oid, Expires, Removable, {Socket, Transport}, Blocks ) -> 
    case Transport:recv( Socket, 0, infinity ) of
        {error,closed} -> add_file(Oid, Blocks, Expires, Removable );
        {ok, Packet} ->
            case clean_keyval( Packet ) of
                {error, R} -> 
                    send( {Socket,Transport}, distress_cmsg:encode_err(R));
                Pair ->
                    handle_add_loop( Oid, Expires, Removable, 
                                     {Socket, Transport}, [Pair|Blocks] )
            end;
        {error,R} -> send( {Socket,Transport}, distress_cmsg:encode_err(R))
    end.
add_file( Oid, KeyBlockMap, Expires, Removable ) ->
    KeyBlockMagicMap = lists:map(gen_magic(Oid,Removable), KeyBlockMap),
    Results = lists:map(add_file_db(Expires), KeyBlockMagicMap),
    %LATER: Broadcast adds into P2P Overlay for replication
    case return_any_errors( Results ) of
        [] -> ok;
        Errors -> {error,Errors}
    end.
add_file_db(Expires) ->
    fun({Key,Block,MyMagic,_}) ->
            distress_db:insert_block( Key, Expires, Block, MyMagic )
    end.
gen_magic( _Oid, false ) ->
    fun({Key,Value})->{Key,Value,nil,nil} end;
gen_magic( Oid, true ) ->
    fun({Key,Value})->
            Magic = f( Oid, Key, Value ),
            MyMagic = g( Magic ),
            {Key, Value, MyMagic, Magic}
    end.

%% @hidden
%% @doc Block Removal
do_del( Oid, Key, Dat ) ->
    case del_block( Oid, Key ) of
        {error, R} -> send( Dat, distress_cmsg:encode_err( R ) );
        _ -> ok
    end.
del_block( Oid, Key ) ->
    case
       distress_db:delete_block( Key, Oid, fun ?MODULE:composefg/3 )
    of
        {error, Reason} -> {error, Reason};
        ok -> ok
    end.

%% @hidden
%% @doc Filters a list for any error tuples.
return_any_errors( [] ) -> [];
return_any_errors( [{error, Reason}|R] ) -> [Reason|return_any_errors(R)];
return_any_errors( [_|R] ) -> return_any_errors( R ).

