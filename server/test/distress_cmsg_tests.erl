%%% Client Messaging Tests
%%%
%%% @author Alexander Dean
-module(distress_cmsg_tests).

%% EUnit Libraries and definitions
-include_lib("eunit/include/eunit.hrl").
-define(zero_uuid,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).

%% @doc Encoding Tests
encoding_test_() ->
    [{"Ack Test1",
        ?_assertEqual( <<"{\"msg\":\"ack\",\"oid\":\"00000000-0000-0000-0000-000000000000\"}">>,
                       distress_cmsg:encode_ack( ?zero_uuid ) ) },

     {"Error Test1",
        ?_assertEqual( <<"{\"msg\":\"err\",\"val\":\"badarg\"}">>,
                       distress_cmsg:encode_err( badarg ) ) },

     {"Error Test2",
        ?_assertEqual( <<"{\"msg\":\"err\",\"val\":\"badarg\"}">>,
                       distress_cmsg:encode_err( {error, badarg} ) ) },

     {"Get Test1",
        ?_assertEqual( <<"{\"msg\":\"get\",\"key\":\"A\",\"val\":\"B\"}">>,
                       distress_cmsg:encode_get( <<"A">>, <<"B">> ) )} ].

decoding_test_() -> [].

access_test_() -> [].
