"""
Imports a Receipt file into the local client's library. This is useful if you
are trading receipts with another person.

Usage: 
    distress import [options] <receipt path>

Options:
    -l LOCK --lock LOCK     The receipt may be locked, and needs a passphrase.

"""
from os import path
from _config import build_cmd_args
from api.distress_receipt import load_receipt_file 

def import_(socket, args, config, library, cmd=False):
    """ Handles user input to add a file's receipt to the local library.
    This functionality can be overridden via the configuration file.
    """
    receiptpath = args['<receipt path>']
    lock = args['--lock']

    if not path.exists( receiptpath ):
        print "Receipt does not exist at:", receiptpath
        return

    if cmd: print "Loading receipt file..."
    receipt = load_receipt_file( receiptpath, lock )

    if cmd: print "Importing Receipt..."
    fileid = library.import_receipt( receipt )

    # If we are running via a shell, return the new file ID.
    # Else, print success.
    if fileid and cmd: print "Success, file's id is now:",fileid
    if not cmd: return fileid

if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__, conn=False )    
    import_( socket, args, config, library, cmd=True )

