%%% Simple Utility module
%%%
%%% @author Alexander Dean
-module(distress_util).

-export([clean_path/1,get_rootdir/0]).

-define(DEFAULT_ROOT_DIR, "~/.config/distress").

%% @doc Clean a file path for the local OS.
-spec clean_path( string() ) -> string().
clean_path( Path ) -> filename:nativename( tilde_expand( Path ) ).

%% @doc Get the directory DISTRESS will use as it's root application directory.
get_rootdir() ->
    case application:get_env( distress, root_dir ) of
        undefined -> ?DEFAULT_ROOT_DIR;
        {ok, Dir} -> Dir
    end.

%% ===========================================================================
%% Private Functions
%% ===========================================================================

%% @hidden
%% @doc Checks for a '~' character at the beginning and replaces with home url.
-spec tilde_expand( string() ) -> string().
tilde_expand( [ $~ | RestOfPath ] ) -> 
    {ok, [[Home]]} = init:get_argument(home), %TODO: check on non-linux os
    string:concat( Home, RestOfPath );
tilde_expand( Path ) -> Path.

