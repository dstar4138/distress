"""
Delete's a file from the DISTRESS network if it has been set as removable, and
you have access to remove it. See 'add' command for more information. Note that
if the file has been set to expire only, then there is no way for a forcable
removal.

Usage:
    distress del [options] <nameid>...

Options:
    -i --ignore    keep going if there are issues deleting a file in the list.

"""
from _config import build_cmd_args
from api.distress_API import delete_file

def delete(socket, args, config, library, cmd=False):
    """ Handles user input to remove a file or set of files from the selected
        DISTRESS network. This functionality can all be overriden via the
        configuration file.
    """
    files=args['<nameid>']
    ignore=args['--ignore']

    for nameid in files:
        receipt = library.get_receipt( nameid )
        if not receipt:
            if cmd: print "Could not find receipt for:",nameid
            if not ignore: return False
            continue

        if receipt.get_oid() == None:
            if cmd: print "You do not have deletion permission for:",nameid
            if not ignore: return False
            continue

        if cmd: print "Delete", receipt.get_filename(), "?"

        response = raw_input("Are you sure? [y/N]")
        if response.lower() not in ['yes','y']:
            print "File was not deleted."
            return False

        if delete_file( socket, receipt ):
            #Succeeded, so remove receipt from library
            library.remove_receipt( nameid )

            if cmd: print "Deletion succeeded!"
        elif cmd: print "Deletion failed!"

    # Return Success.
    return True

if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__ )
    delete( socket, args, config, library, cmd=True )

