"""
Export a file's meta-data (Receipt) from the local client's library to give
to another person. You can restrict the permissions you want to give them too.
Note that you can lock this Receipt with a separate password before you pass it
off to a friend.

Usage: 
    distress export [options] <nameid> <savepath>

Arguments:
    <nameid>    The name or ID of the file to export, see 'list' command.
    <savepath>  The path without a file name for the exported receipt.

Options:
  -r --read     allow read permissions to receipt copy
  -d --delete    allow deletion permissions to receipt copy
  -l LOCK --lock LOCK   add a lock passphrase to the exported receipt
  -R --relative     relatively pulls out each receipt to put it side by side
"""
from os import path
from _config import build_cmd_args

def determine_read( args ):
    """ By default an exported receipt has no read permission. This will
        check if the arguments provided an override to this.
    """
    return args['--read'] # TODO: Should we make this a negation? Default=True?

def determine_delete( args ):
    """ By default an exported receipt has no delete permissions. This will
        check if the arguments provided an override to this.
    """
    return args['--delete']

def export(socket, args, config, library, cmd=False):
    """ Handles user input to add a file's receipt to the local library.
    This functionality can be overridden via the configuration file.
    """
    nameid = args['<nameid>']
    savepath = args['<savepath>']
    lock = args['--lock']
    relative = args['--relative']
    canread = determine_read( args )
    candelete = determine_delete( args )

    receipt = library.get_receipt( nameid )
    if not receipt:
        if cmd: print "Could not find receipt with nameid:",nameid
        return False

    newpath = receipt.save( savepath, canread, candelete, lock, relative )
    if newpath:
        if cmd: print "Success!", newpath
        return True
    else:
        if cmd: print "Save failed!"
        return False

if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__, conn=False )
    export( socket, args, config, library, cmd=True )

