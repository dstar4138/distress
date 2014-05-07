%%% Simple Utility module
%%%
%%% @author Alexander Dean
-module(distress_util).

-export([hash/2]).
-export([uuid/0]).
-export([clean_path/1,get_rootdir/0]).

-define(DEFAULT_ROOT_DIR, "~/.config/distress").


%% @doc Follows RFC4122 for generating UUIDs version 4 via Random Numbers.
%% This function will perform fairly slowly as it uses the crypto module,
%% as apposed to hand running rand:uniform/1. But I feel this is more readable
%% and we don't actually call uuid/0 frequently.
%% @end
uuid()->
    <<A:48,B:12,C:62,_:6>>=crypto:rand_bytes(16),
    <<A:48,4:4,B:12,2:2,C:62>>.

%% @doc Clean a file path for the local OS.
-spec clean_path( string() ) -> string().
clean_path( Path ) -> filename:nativename( tilde_expand( Path ) ).

%% @doc Get the directory DISTRESS will use as it's root application directory.
get_rootdir() ->
    case application:get_env( distress, root_dir ) of
        undefined -> ?DEFAULT_ROOT_DIR;
        {ok, Dir} -> Dir
    end.

%% @doc Accomodate older versions without hash function.
hash( Type, Value ) ->
    case erlang:function_exported( crypto, hash, 2 ) of
        true  -> crypto:hash( Type, Value );
        false -> erlang:apply( crypto, Type, [ Value ] )
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

