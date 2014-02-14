%%% The DISTRESS Application Monitor
%%%
%%% Starts the DISTRESS application in the Erlang VM and monitors the process 
%%% tree.
%%%
%%% @author Alexander Dean
-module(distress_app).
-behaviour(application).

%% General Debuggery
-include("debug.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
%% @doc Starts off the primary application supervisor.
start(_StartType, _StartArgs) -> 
    ?DEBUG("Starting DISTRESS Service..."),
    p2pfs_sup:start_link().

%% @private
%% @doc Called by the behaviour right before shutting down.
prep_stop( _State ) -> 
    ?DEBUG("Stopping DISTRESS Service...").

%% @private
%% @doc Cleans up the Application state.
stop(_State) -> ok.

