"""
Adds a file or set of file's to the selected DISTRESS network. Will save them
to the local library for later retrieval.

Usage:
    distress add [options] <filepath>

Arguments:
    <filepath>    the file to add or the directory to recurse into.

Options:
    -h --help    show this help menu
    -k KEY --key=KEY    override key in default configuration
    -r --recursive    recursively add all files in directory
    -f --force    force overwrite of existing file    [not done]
    -t TIMEOUT --timeout TIMEOUT    the unix time when this file expires [not done]
"""
from os import path, listdir

from _config import build_cmd_args
from api.distress_API import encrypt_file

def determine_filepaths( args ):
    """ Check if the given filepath is valid and if we need to walk the path
        for all files within the directory.
    """
    filepath = args['<filepath>']
    if not path.exists( filepath ):
        raise Exception( "File path does not exist" )
    if path.isfile( filepath ):
        return [filepath]
    elif args['--recursive']:
        return listdir( filepath )
    else:
        raise Exception("File path is not a file, did you want recursive add?")

def determine_key( args, config ):
    """ Check if the user provided an override key, otherwise we'll default to
        the global key stored in the configuration.
    """
    return args['--key'] if args['--key'] else config['global']['key']

def add(socket, args, config, library, cmd=False):
    """ Handles user input to add a file or set of files to the selected
        distress network. This functionality can all be overridden via the
        configuration file.
    """
    ret = []
    try:
        files = determine_filepaths( args )
        key = determine_key( args, config )

        if key == '':
            print 'No key found: first run ./distress config global.key <key>'
            return ret

        for filename in files:
            try:
                receipt = encrypt_file(socket, library, filename, key)
                if cmd: print receipt.get_filename()+" added!"
                else: ret.append( receipt )
            except IOError as err:
                print "Error: file '" + filename + "' is not readable"

    except Exception as e: print e

    # Return set of receipts if called via shell
    if cmd: return ret


## If run by command-line, we generate config and grab arguments
## then run the addition function. Otherwise this module just provides
## a function of the same name, which can be used via shell.
if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__ )
    add( socket, args, config, library, cmd=True )

