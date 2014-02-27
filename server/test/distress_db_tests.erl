%%% Database API Tests
%%%
%%% @author Alexander Dean
-module(distress_db_tests).

%% EUnit Libraries and definitions
-include_lib("eunit/include/eunit.hrl").
-define(TEST_DIR,"db"). %Will be in $(CURDIR)/.eunit/db
-define(BLOCKSIZE, 16). %The System does not enforce this, its client enforced.
-define(KEY_HASH, sha384). 

setup_mnesia_connection() ->
    distress_db:verify_install( ?TEST_DIR ).
shutdown_mnesia_connection(_) ->
    application:stop(mnesia),
    clear_test_dir().
clear_test_dir() -> 
    os:cmd("rm -r "++?TEST_DIR).
     

%% @doc Returns a set of EUnit tests to execute.
all_test_() ->
    {setup, fun setup_mnesia_connection/0,
            fun shutdown_mnesia_connection/1,
            fun all_tests/1}.
all_tests(_) ->
    setup_tests() ++ insert_tests() ++ select_tests() ++ delete_tests().


setup_tests() ->
    [{"verify mnesia", 
        ?_assert( lists:member(mnesia_locker, erlang:registered()) )},
     {"verify t_block",
        ?_assertEqual( 5, mnesia:table_info(t_block,arity) )},
     {"verify double-start",
        ?_assertEqual( ok, distress_db:verify_install( ?TEST_DIR ) )}
    ].

insert_tests() -> 
    {_, Key1, Expires1, Block1, Magic1} = gen_entry( infinity, false ),
    {_, Key2, Expires2, Block2, Magic2} = gen_entry( infinity, true ),
    {_, Key3, Expires3, Block3, Magic3} = gen_entry( now, false ),
    {_, Key4, Expires4, Block4, Magic4} = gen_entry( now, true ),
    [{"simple insert1",
        ?_assertEqual( ok, distress_db:insert_block( Key1, Expires1, Block1, Magic1 ) )},
     {"simple insert2",
        ?_assertEqual( ok, distress_db:insert_block( Key2, Expires2, Block2, Magic2 ) )},
     {"simple insert3",
        ?_assertEqual( ok, distress_db:insert_block( Key3, Expires3, Block3, Magic3 ) )},
     {"simple insert4",
        ?_assertEqual( ok, distress_db:insert_block( Key4, Expires4, Block4, Magic4 ) )},
     {"attempt reinsert valid1",
        ?_assertEqual( ok, distress_db:insert_block( Key3, Expires3, Block3, Magic3 ) )},
     {"attempt reinsert valid2",
        ?_assertEqual( ok, distress_db:insert_block( Key4, Expires4, Block4, Magic4 ) )},
     {"attempt reinsert invalid1",
        ?_assertEqual( {error,badarg}, distress_db:insert_block( Key1, Expires1, Block1, Magic1 ) )},
     {"attempt reinsert invalid2",
        ?_assertEqual( {error,badarg}, distress_db:insert_block( Key2, Expires2, Block2, Magic2 ) )}
    ].

select_tests() ->
    {_, Key1, Expires1, Block1, Magic1} = gen_entry( infinity, false ),
    {_, Key2, Expires2, Block2, Magic2} = gen_entry( infinity, true ),
    {_, Key3, Expires3, Block3, Magic3} = gen_entry( now, false ),
    {_, Key4, Expires4, Block4, Magic4} = gen_entry( now, true ),
    InsertThenLookup = fun( K, E, B, M ) -> 
                          ok = distress_db:insert_block( K, E, B, M ),
                          distress_db:select_block( K )
                       end,

    [{"lookup missing",
        ?_assertEqual( [], distress_db:select_block( Key1 ) )},
     {"lookup after insert1",
        ?_assertEqual( Block1, InsertThenLookup(Key1,Expires1,Block1,Magic1) )},
     {"lookup after insert2",
        ?_assertEqual( Block2, InsertThenLookup(Key2,Expires2,Block2,Magic2) )},
     {"lookup after expires1",
        ?_assertEqual( [], InsertThenLookup(Key3,Expires3,Block3,Magic3) )},
     {"lookup after expires2",
        ?_assertEqual( [], InsertThenLookup(Key4,Expires4,Block4,Magic4) )}
    ].

delete_tests() -> 
    {Oid1, Key1, Expires1, Block1, Magic1} = gen_entry( infinity, false ),
    {Oid2, Key2, Expires2, Block2, Magic2} = gen_entry( infinity, true ),
    {Oid3, Key3, Expires3, Block3, Magic3} = gen_entry( now, false ),
    {Oid4, Key4, Expires4, Block4, Magic4} = gen_entry( now, true ),
    BadOid = crypto:hash(?KEY_HASH, crypto:rand_bytes(16)),
    InsertThenDelete = fun( O, K, E, B, M ) ->
                            ok = distress_db:insert_block( K, E, B, M ),
                            distress_db:delete_block( K, O, fun f/3 )
                       end, 
    [{"delete missing",
        ?_assertEqual( ok, distress_db:delete_block( Key1, Oid1, fun f/3 ) )},
     {"delete when R=false,E=inf,O=valid",
        ?_assertEqual( {error,badarg}, InsertThenDelete(Oid1,Key1,Expires1,Block1,Magic1))},
     {"delete when R=true,E=inf,O=valid",
        ?_assertEqual( ok, InsertThenDelete(Oid2,Key2,Expires2,Block2,Magic2))},
     {"delete when R=false,E=old,O=valid",
        ?_assertEqual( ok, InsertThenDelete(Oid3,Key3,Expires3,Block3,Magic3))},
     {"delete when R=true,E=old,O=valid",
        ?_assertEqual( ok, InsertThenDelete(Oid4,Key4,Expires4,Block4,Magic4))},

     {"delete when R=false,E=inf,O=invalid",
        ?_assertEqual( {error,badarg}, % Not removable, can't delete. Already added in test 2.
                       distress_db:delete_block( Key1, BadOid, fun f/3 ) )},
     {"delete when R=true,E=inf,O=invalid",
        ?_assertEqual( {error,badarg}, % Only time we truely hint it's invalid oid
                       InsertThenDelete(BadOid,Key2,Expires2,Block2,Magic2))},
     {"delete when R=false,E=old,O=invalid",
        ?_assertEqual( ok, % Expires was old, take the opportunity to lie about oid
                       InsertThenDelete(BadOid,Key3,Expires3,Block3,Magic3))},
     {"delete when R=true,E=old,O=invalid",
        ?_assertEqual( ok, % Expires was old, take the opportunity to lie about oid
                       InsertThenDelete(BadOid,Key4,Expires4,Block4,Magic4))}
    ].


%%% ======================================================================
%%% Example Block Generation
%%% ======================================================================

%% @hidden
%% @doc Generate an entry for insert.
gen_entry( now, Removable ) ->
    {Key,Block} = gen_rand_block(),
    {Oid,Magic} = gen_magic( Removable, Key, Block ),
    {Oid, Key, unixtime(), Block, Magic};
gen_entry(infinity, Removable ) ->
    {Key,Block} = gen_rand_block(),
    {Oid,Magic} = gen_magic( Removable, Key, Block ),
    {Oid, Key, infinity, Block, Magic}.

%% @hidden
%% @doc Generate a random block of bytes, and hash it to get a random key.
gen_rand_block() ->
    Block = crypto:rand_bytes( ?BLOCKSIZE ),
    Key = crypto:hash( ?KEY_HASH, Block ),
    {Key, Block}. 

%% @hidden
%% @doc Generate a magic value using the 
gen_magic( false, _, _ ) -> {uuid(), nil};
gen_magic( true, Key, Block ) ->
    Oid = uuid(),
    Magic = f( Oid, Key, Block ), 
    {Oid, Magic}.

%% @hidden
%% @doc A fake Oid verification function.
f( Oid, _, _ ) -> Oid.

%% @hidden
%% @doc Returns the Unix timestamp for erlang:now/0.
unixtime() -> {Macro,Sec,_} = now(),(Macro*1000000+Sec).

%% @hidden
%% @doc Returns a UUID version 4.
uuid() -> 
   <<A:48,B:12,C:62,_:6>>=crypto:rand_bytes(16),
   <<A:48,4:4,B:12,2:2,C:62>>.

