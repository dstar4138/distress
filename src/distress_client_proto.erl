%%% Distress message protocol for Clients
%%% 
%%%     In order to send back and forth Binary effectively and keep our
%%%     messages on the smaller end, we are experimenting with a simple 
%%%     custom protocol.
%%% 
%%% @author Alexander Dean
-module(distress_client_proto).
-include("distress_msgs.hrl").
-define(BLOCKSIZE,512).%in bytes

-export([encode/1,decode/1]).

%% @doc Decode's Client query messages.
decode( <<ID:4,Payload/bitstring>> ) ->
    case ID of
        0 -> % Add File
            decode_add(Payload);
        1 -> % Add Block
            decode_adb(Payload);
        2 -> % Del Block
            decode_del(Payload);
        4 -> % Get Block
            decode_get(Payload);
        _ -> % Empty Space
            undefined
    end.
decode_add(<<Count:16,_/bitstring>>) -> 
    #add_file{block_count = erlang:binary_to_integer( Count )}.
decode_adb(<<Key:16,Val:?BLOCKSIZE,_/bitstring>>) -> 
    #add_block{key=Key,value=Val}.
decode_del(<<Key:16,Oid:16,_/bitstring>>) -> 
    #del_block{key=Key,oid=Oid}.
decode_get(<<Key:16,_/bitstring>>) -> #get_block{key=Key}.


%% @doc Encode's Server response messages.
encode( #add_ack{oid=Oid} )         -> <<0:4,Oid:16, 0:4>>;
encode( #block{key=Key,value=Val} ) -> <<1:4,Key:16,Val:?BLOCKSIZE,0:4>>;
encode( #error{value=Val} )         -> <<2:4,Val:?BLOCKSIZE,0:4>>;
encode( #success{value=Val} )       -> <<3:4,Val:?BLOCKSIZE,0:4>>.

