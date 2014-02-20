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

%% @doc To start a swarm acceptor pool we need a handler function, this 
%%   function returns that handler function description.
%% @end  
fun_desc() -> {?MODULE, handle, []}.


%% @doc Start off the Handle loop.
handle( Socket, _Name, Transport, _Info, Args ) ->
    ?DEBUG("HANDLER STARTED: (~p,~p,~p,~p)",[Socket, _Name, Transport, _Info]),
    handle_loop( {Socket, Transport}, Args ).


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
