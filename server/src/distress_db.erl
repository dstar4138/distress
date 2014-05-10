%%% Local Database Storage for DISTRESS
%%%
%%%     This module adds and manipulates the tables used for block storage
%%%     of the DISTRESS database. Note the database does not do any encryption
%%%     or decryption, all of that is client side.
%%%
-module(distress_db).

%% General Debuggery
-include("debug.hrl").

%% Query Language
-include_lib("stdlib/include/qlc.hrl").

%% Record for Block storage                        %%
%% ----------------------------------------------- %%
%% The Block Storage Table
%%      Key     - The special hashed key to the encrypted partial file block.
%%      Expires - When this block can be purged from the db.
%%      Block   - The Encrypted file block stored in the database.
%%      Magic   - A masked value used to verify the ownership id of a block.
-record( t_block, {
           key     :: binary(),
           expires :: infinity | pos_integer(),
           block   :: binary(),
           magic   :: nil | binary()
        } ).
%% *Note: If Magic is nil, then it can only be removed upon expiration.
%% ------------------------------------------- %%
%% My State Table
%%      Simple key-value store that Distress will save critical values it will
%%      need upon crash/reboot.
%%
-record( t_me, {
           key :: atom(),
           val :: term()
          }).

-define(TABLE_LIST,[t_block,t_me]).
-define(TABLES,[ {t_block, record_info(fields, t_block), []},
                 {t_me, record_info(fields, t_me), []}
               ]).


-export([ verify_install/1 ]).
-export([ set_state_key/2, get_state_key/1 ]).
-export([ insert_block/4, select_block/1, delete_block/3 ]).

%%% ==========================================================================
%%% API
%%% ==========================================================================

%% @doc Install the Database on the local system based on the passed in options.
verify_install( Dir ) ->
    create_schema( Dir ),           % Sets up disk
    ok = start_mnesia(),            % Load to memory
    ok = make_tables( ),            % Create disc copies
    connect_to_mnesia().            % Wait for locks on tables, get init state

%% @doc Set a value in the state's key-value store. Should be used sparingly!!
set_state_key( Key, Val ) when is_atom( Key ) ->
    Transaction = fun() ->
        mnesia:write(#t_me{key=Key, val=Val})
    end,
    run_tran( Transaction, ok ).


%% @doc Grab a value from the state key-value store.
get_state_key( Key ) when is_atom( Key ) ->
    Transaction = fun() ->
        qlc:e( qlc:q( [ {Key, U#t_me.val} || U <- mnesia:table(t_me),
                                             U#t_me.key =:= Key ] ) )
    end,
    run_tran( Transaction, ok ).

%% @doc Insert a block with a given Key and Expires/Magic pair. Will fail if
%%   a block with the same key exists of a different value if the expiration
%%   has not passed.
%% @end
insert_block( Key, Expires, Block, Magic ) ->
   Transaction = fun() ->
        Q = qlc:q( [ ok || Row <- mnesia:table( t_block ),
                           Row#t_block.key =:= Key,
                           valid_time( Row#t_block.expires )
                   ] ),
        case qlc:e( Q ) of
            [_] -> {error,badarg}; % Exists and hasn't expired
            []  ->
               mnesia:write( #t_block{ key=Key,
                                       expires=Expires,
                                       block=Block,
                                       magic=Magic } )
        end
    end,
    run_tran( Transaction, ok ).

%% @doc Looks up a key in the block store and will return its value.
select_block( Key ) ->
    Transaction = fun() ->
        Q = qlc:q( [ Row#t_block.block ||
                        Row <- mnesia:table(t_block),
                        Row#t_block.key =:= Key,
                        valid_time( Row#t_block.expires )
                   ] ),
        qlc:e( Q )
    end,
    run_tran( Transaction, [] ).


%% @doc Removes a block from the system given a test at it's Ownership ID and
%%   the Key. If the OID does not reflect what's stored in the Key's magic
%%   value, then it will error out.
%% @end
delete_block( Key, OID, F ) ->
    case select_block( Key ) of
        {error, E} -> {error, E};
        []         -> ok;
        Value      ->
            TestMagic = F( OID, Key, Value ),
            Transaction = fun() ->
                Q = qlc:q( [ ok || Row <- mnesia:table(t_block),
                                   Row#t_block.key =:= Key,
                                   Row#t_block.magic =:= TestMagic ] ),
                case qlc:e( Q ) of
                    [] -> {error,badarg}; % Failed to match, due to missing or no magic.
                    [ ok ] -> % We matched.
                        mnesia:delete({t_block,Key})
                end
            end,
            run_tran( Transaction, ok )
    end.


%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

%% @hidden
%% @doc Creates the local files on disc to store the persistant state.
create_schema( Dir ) ->
    try
        ok = filelib:ensure_dir( Dir ),          % Ensure the directory exits.
        application:set_env( mnesia, dir, Dir ), % Override save location.
        mnesia:create_schema([ node() ])         % Save to the local erl node.
    catch
        exit:{_,{already_exists,_}} -> ok;
        Exit:Reason ->
            ?ERROR("Failed to create system schema: ~p:~p", [Exit,Reason]),
            {error, Reason}
    end.

%% @hidden
%% @doc Creates each table in the database.
make_tables() ->
    F = fun(Tab = {TabName, _, _}, Acc) ->
                I = try mnesia:table_info(TabName, all) catch _:_ -> [] end,
                case length(I) of
                    0 ->
                        create_table( Tab ),
                        [Tab|Acc];
                    _ ->
                        Acc
                end
        end,
    NewlyMadeTables = lists:foldl(F, [], ?TABLES),
    if length(NewlyMadeTables) > 0 ->
           ?DEBUG("Finished building local storage for the first time.");
       true -> ok
    end.

%% @hidden
%% @doc Creates an individual table according to the persist options. Taken from
%%   EMPDB project.
%% @end
create_table( {TabName, Info, Index} ) ->
    ?DEBUG("Building table ~p in mnesia.",[TabName]),
    DefaultOpts = [{attributes, Info},
                   {type, ordered_set},
                   {disc_copies, [node()]}],
    CrOption = if
                   length(Index) > 0 -> [{index, Index} | DefaultOpts ];
                   true -> DefaultOpts
               end,
    case mnesia:create_table( TabName, CrOption ) of
        {atomic, ok} -> fill_defaults( TabName );
        {aborted, Reason} ->
            ?ERROR("Failed to create table ~p because: ~p", [TabName, Reason])
    end.

%% @hidden
%% @doc Starts up the mnesia application and masks any warnings.
start_mnesia() ->
    case application:start(mnesia) of
        {error, {already_started, mnesia}} -> ok;
        ok -> ok;
        Err -> Err
    end.

%% @hidden
%% @doc Assumes Mnesia application is running and will wait for table access.
%%   Will fail if another Distress application is currently running on the same
%%   node.
%% @end
connect_to_mnesia() ->
    case mnesia:wait_for_tables( ?TABLE_LIST, 5000 ) of
        ok -> ok;
        {timeout, BadTabList} ->
            (case mnesia:wait_for_tables( BadTabList, 5000 ) of
                 {timeout, _} -> {error, timeout};
                 ok -> ok;
                 M -> M
             end);
        {error, Reason} -> {error, Reason}
    end.

%% @hidden
%% @doc A wrapper for running a transaction on the Distress Persistant store.
%%   Will return 'Ret' upon successful completion (unless it returns a value
%%   explicitly), {error, Reason} otherwise.
%% @end
run_tran( F, Ret ) ->
    try mnesia:transaction(F) of
        {aborted, Reason} -> {error, Reason};
        {atomic, [Res]}   -> Res;
        {atomic, ok}      -> Ret;
        {atomic, Drop}    -> Drop
    catch error: Reason -> {error, Reason} end.

%% @hidden
%% @doc Will return true if the expiration is in the future (by checking unix
%%   timestamps) or if it's infinity.
%% @end
valid_time( infinity ) -> true;
valid_time( Time ) ->
    {Macro, Sec, _ } = now(),
    (Macro*1000000+Sec) < Time.

%% @hidden
%% @doc Fill particular tables with their default values, which only happens
%%   if the table has never been created.
%% @end
fill_defaults( t_me ) ->
    Vals = [ % Distress, when it first starts up generates a UUID for itself to
             % send to friends. This will be its personal peer ID.
            { my_id, distress_util:uuid() }
           ],
    Transaction = fun() ->
           lists:foreach( fun( {Key,Val} ) ->
                   mnesia:write( #t_me{ key=Key, val=Val } )
                          end, Vals )
                  end,
    run_tran( Transaction, ok );
fill_defaults( _ ) -> ok.

