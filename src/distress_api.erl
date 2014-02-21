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
handle_msg( _Msg, Args, Dat ) ->
    ?DEBUG("TODO: Handle Msg: ~p",[_Msg]), %TODO: Actually handle the API.
    handle_loop( Dat, Args ).


%%%===================================================================
%%% Server Side Block API
%%%===================================================================

%% @hidden
%% @doc File accessing via a hash value.
get_block( Key ) ->
    case distress_db:select_block( Key ) of
        {error, Reason} -> {error, Reason};
        [] -> {error, missing};
        Val -> {ok, Val}
    end.

%% @hidden
%% @doc Handles the add file logic
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
rm_block( Oid, Key ) ->
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

