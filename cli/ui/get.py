"""
Get's a file from the DISTRESS network, as long as you have the receipt stored
in your library.

Usage:
    distress get [-i] [-p] <nameid>
    distress get [-i] <nameid> [ <storepath> ]

Arguments:
    <nameid>    the name or id of the file in the library. See 'list' cmd.
    <storepath> override the path to store the received path.

Options:
    -i --ignore     ignore the loss of a block and keep going.
    -p --path-preserve  preserve the path in which the file was added.
"""
from os.path import basename
from _config import build_cmd_args
from api.distress_API import recieve

def get(socket, args, config, library, cmd=False):
    """ Handles user input to get a file from the distress network. """

    nameid = args['<nameid>']
    receipt = library.get_receipt( nameid )
    if receipt is None:
        raise Exception("Receipt does not exist for that name or id.")

    # Use the overrided store path.
    path = receipt.get_filename()
    if args['<storepath>']: path = args['<storepath>']
    elif not args['--path-preserve']: path = basename( path )

    # Check if to ignore
    ignore = args['--ignore']

    try:
        read_access = recieve( socket, receipt, path, ignore )

        if not cmd: return (True, read_access)

        if read_access:
            print "Downloaded and Decrypted!"
        else:
            print "File downloaded, but you are missing read access."
    except Exception as e:
        print e

    # We broke out of the recieve block, so it must have been an error
    return (False, False)

if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__ )
    get( socket, args, config, library, cmd=True )

