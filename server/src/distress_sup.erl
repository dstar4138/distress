%%% The Distress Application Supervisor
%%%
%%%   The Distress application is currently (without the P2P overlay
%%%   installed) a simple database system wrapped around a TCP
%%%   acceptor pool.
%%%
%%% @author Alexander Dean
-module(distress_sup).

%% Types

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

-define(TCP_OPTS, [{port,65501}]).

-define(POOL_SIZE, 10).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor and child subtree for the client acceptor pool.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Supervisor callback called from start_link/0.
init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        client_listener_child()
    ]}}.

%%%===================================================================
%%% Internal Functionality
%%%===================================================================

%% @hidden
%% @doc Creates a Swarm acceptor pool for listening to client actions.
%%   These take the current distress_api action handler.
%% @end
client_listener_child() ->
    swarm:child_spec( clients, ?POOL_SIZE, swarm_tcp, ?TCP_OPTS,
                      distress_api:fun_desc() ).
