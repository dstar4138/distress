%%% The Distress Connection Manager
%%%
%%%     Currently this module does very little, and only serves to validate
%%%     the connections being made to the server (e.g. are whitelisted). In
%%%     the future, it may be worth it to do more analysis of traffic to 
%%%     prune particular poorly acting IPs, etc.
%%%
%%% @author Alexander Dean
-module( distress_conn ).

%% General Debuggery
%-include("debug.hrl"). % Imported from swarm.

%% For Swam connection Info details
-include_lib("swarm/include/swarm.hrl").

-export([validate_connection/1]).

%%%===================================================================
%%% Connection Validation
%%%===================================================================

%% @doc Runs over the list of checks to validate the connection. 
validate_connection( Info ) ->
    Res = validate_by( Info, [ 
                               fun is_localhost/1,
                               fun on_whitelist/1
                               %% ...
                             ]),
    ?DEBUG("HANDLER VALIDATION = ~p, ~p~n",[Res, if Res -> "allowing."; 
                                                    true -> "booting!" end]),
    Res.

%% @hidden
%% @doc Will run over a list of functions and return true if any return true,
%%   otherwise it will return false.
%% @end
validate_by( _, [] ) -> false;
validate_by( Arg, [H|R] ) ->
    case H(Arg) of
        true -> true;
        false -> validate_by( Arg, R )
    end.

%%%===================================================================
%%% Validation Functions
%%%===================================================================

%% @hidden
%% @doc Will check if the connection is localhost. Always will allow.
is_localhost( #swarm_info{ peer_addr=Addr } ) ->
    case Addr of
        {127,0,0,1} -> true;
        "localhost" -> true;
        _ -> false
    end.

%% @hidden
%% @doc Will validate with the loaded whitelist to see if the IP is
%%   listed.
%% @end
on_whitelist( #swarm_info{ peer_addr=Addr } ) ->
    Whitelist = get_whitelist(),
    lists:member( Addr, Whitelist ).
get_whitelist() ->
    case application:get_env(distress, whitelist) of
        undefined -> [];
        {ok, WL}  -> WL
    end.

