##
## DISTRESS Messaging wrapper
##
import json

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
    return json.dumps( { 'key' : Key,
                         'val' : Value } )


def del( Oid, Key ):
    return json.dumps( { 'msg' : 'del', 
                         'key' : Key,
                         'oid' : Oid } )

def get( Key ):
    return json.dumps( { 'msg' : 'get', 'key' : Key } )


##
## Decode the messages from the Server.  
##
def decode( Msg ): return json.loads( Msg )
