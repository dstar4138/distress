##
## DISTRESS Messaging wrapper
##
import json

HASHSIZE = 96 # 384 bits / 4 bit chars = 96, since SHA384 returns str.
BLOCKSIZE = 684 # 512 in bytes, but base64 encoded.

def add( NumBlocks, Expires="infinity", Removable=False ):
    assert( type(NumBlocks) is int )
    assert( NumBlocks > 0 )
    if not Expires in ["infinity", "inf"]: assert( Expires > 0 )
    assert( type(Removable) is bool )
    return json.dumps( { 'msg' : 'add',
                         'expires' : Expires,
                         'removable' : Removable,
                         'numblocks' : NumBlocks } )


def addblock( Key, Value ):
    assert( type(Key) is str )
    assert( len(Key) == HASHSIZE )
    assert( type(Value) is str )
    assert( len(Value) == BLOCKSIZE )
    return json.dumps( { 'key' : Key, 'val' : Value } )

def delblock( Oid, Key ):
    assert( type(Oid) is str )
    assert( type(Key) is str )
    assert( len(Key) == HASHSIZE )
    return json.dumps( { 'msg' : 'del', 'key' : Key, 'oid' : Oid } )

def getblock( Key ):
    assert( type(Key) is str )
    assert( len(Key) == HASHSIZE )
    return json.dumps( { 'msg' : 'get', 'key' : Key } )


##
## Decode the messages from the Server.
##
def decode( Msg ): return json.loads( Msg )
