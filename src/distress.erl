%%% Usability Module for DISTRESS
%%%
%%%  Adds ease of use functions for interacting with a running 
%%%  DISTRESS service.
%%%
%%% @author Alexander Dean
-module(distress).

%% General Debuggery
-include("debug.hrl").

%% Public API
-export([start/0, start/1, stop/0]).

-define(DEFAULT_CONFIG, []).

%% @doc Starts a DISTRESS Service on the running VM.
start( ) -> start( default ).
start( ConfigFilePath ) -> 
    load_config( ConfigFilePath ),
    application:start( distress ).

%% @doc Stops a running DISTRESS Service on the running VM.
stop() -> application:stop( distress ).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Reads in a DISTRESS RC file and sets the DISTRESS environment.
load_config( default ) -> set_config( ?DEFAULT_CONFIG );
load_config( Path ) -> 
    case file:consult( Path ) of
        {ok, Terms} -> 
            Config = lists:keymerge( 1, ?DEFAULT_CONFIG, Terms ),
            set_config(Config);
        {error, Reason} ->
            ?ERROR(Reason)
    end.

%% @hidden
%% @doc Sets the global DISTRESS application environment.
set_config( [] ) -> ok;
set_config( [{Par,Val}|R] ) ->
    application:set_env( ?MODULE, Par, Val ),
    set_config( R ).

