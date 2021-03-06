%%% The Distress Client Protocol
%%%
%%%     The Client messaging API encoding and decoding wrapper. Currently our
%%%     protocol is implemented in JSON using the Erlang JSONx library.
%%%
-module( distress_cmsg ).

-include("debug.hrl").

-export([decode/2]).
-export([encode_ack/1,encode_err/1,encode_get/2,encode_geterr/2,encode_scs/0]).
-export([get_type/1, get_value/2]).

% We want a proplist back rather than a stuct or eep18.
-define(JSONX_OPTIONS, [{format,proplist}]).
-define(M,<<"msg">>).
-define(A,<<"ack">>).
-define(O,<<"oid">>).
-define(E,<<"err">>).
-define(V,<<"val">>).
-define(K,<<"key">>).
-define(G,<<"get">>).
-define(S,<<"scs">>).

%%%===================================================================
%%% Messaging API
%%%===================================================================

%% @doc Use the JSONx Library for decoding a binary Json message.
%%   But simulate streaming by returning both the current term and
%%   a buffer. TODO: FIX THIS AS IT WILL KEEP BUFFING FOREVER.
%% @end
-spec decode( binary(), binary() ) -> {term(), binary()}.
decode( Binary, CurBuffer ) ->
    ?DEBUG("RECV{~n--------~p~n-------~p~n=========~n~n",[Binary,CurBuffer]),
    WorkingBlock = <<CurBuffer/binary,Binary/binary>>, %% ASSUMES ORDER
    case scan_until_endpacket(WorkingBlock,<<>>) of
        {ok, Block, NextBuff} ->
            ?DEBUG("SPLITTING[~n~p,~n~p~n]~n",[Block,NextBuff]),
            Term = secondary_decode( jsonx:decode( Block, ?JSONX_OPTIONS ) ),
            {ok,Term,NextBuff};
        {get, NextBuff} -> {get,NextBuff}
    end.
scan_until_endpacket(<<$},X/binary>>,<<A/binary>>)->
    {ok, <<A/binary,$}>>, X};
scan_until_endpacket(<<C:8,X/binary>>,<<A/binary>>)->
    scan_until_endpacket(X,<<A/binary,C:8>>);
scan_until_endpacket(<<>>,A) -> {get,A}.

%% @doc Instead of a single encode function, we have one per message type.
encode_ack( Oid ) -> jsonx:encode( [{?M,?A},{?O, e_uuid( Oid )}] ).
encode_err({error,Err}) -> jsonx:encode( [{?M,?E},{?V,Err}] );
encode_err( Err )       -> jsonx:encode( [{?M,?E},{?V,Err}] ).
encode_geterr( Key, Err ) -> jsonx:encode( [{?M,?E},{?K,Key},{?V,Err}] ).
encode_get( Key, Block ) -> jsonx:encode( [{?M,?G},{?K,Key},{?V,Block}] ).
encode_scs() -> jsonx:encode( [{?M,?S}] ).

%% @doc Get what type of event the message is.
get_type( Msg ) when is_list( Msg )->
    catch erlang:binary_to_atom( get_value( Msg, msg ), latin1 ).

%% @doc A quick accessor function to abstract away the proplist in case we
%%   want to move to maps or structs. Note we convert the key from an atom
%%   to a binary string to abstract away the message type too.
%% @end
get_value( Msg, Key ) ->
    BKey = erlang:atom_to_binary( Key, latin1 ),
    proplists:get_value( BKey, Msg, undefined ).


%%%===================================================================
%%% Internal Functionality
%%%===================================================================

%% @hidden
%% @doc Check if there is a uuid which needs decoding.
secondary_decode( {error, Err, _ } ) -> {error, Err};
secondary_decode( Msg ) ->
    case get_value( Msg, oid ) of
        undefined -> Msg;
        Oid -> lists:keyreplace(?O, 1, Msg, {?O, d_uuid( Oid )} )
    end.

%% @hidden
%% @doc Encode our internal Binary representation to a nice binary string form.
e_uuid( <<A:32, B:16, C:16, D:8, E:8, F:48>> ) ->
    erlang:list_to_binary(
        lists:flatten(
            io_lib:format(
                "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                [A,B,C,D,E,F]))).

%% @hidden
%% @doc Decode the string format to the internal binary representation.
d_uuid( S ) ->
    tobin( lists:filter(fun(C)-> C/=$- end,
                        erlang:binary_to_list(S)),
           [] ).
tobin( [], A )-> erlang:list_to_binary(lists:reverse(A));
tobin( [X,Y|R], A )-> {ok,[B],_}=io_lib:fread("~16u", [X,Y]), tobin(R,[B|A]);
tobin( _, A) -> A.

