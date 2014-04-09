""" 
Verifies that the file is the one represented in a particular receipt.

Usage:
    distress verify <receipt_id> <file_path>

Arguments:
    <receipt_id>    the receipt id you wish to verify
    <file_path>     the file you wish to verify
"""

from os import path
from _config import build_cmd_args
from api.distress_API import verify_receipt

def verify(args, config, library, cmd=False):
    """ Calls the verify_file API to check file vs. receipt association.
    """

    file_path  = args['<file_path>']
    receipt_id = args['<receipt_id>']
    
    receipt = library.get_receipt(receipt_id)

    if not receipt:
        raise Exception( 'Receipt does not exist!' )

    if not path.exists(file_path):
        raise Exception( 'File path does not exist!' )

    if verify_receipt(receipt, file_path):
        print 'The receipt does describe that file!'
    else:
        print "Sorry, that's not the right receipt/file combo!"

## If run by command-line, we generate config and grab arguments
## then run the addition function. Otherwise this module just provides
## a function of the same name, which can be used via shell.
if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__ )
    verify(args, config, library, cmd=True )


