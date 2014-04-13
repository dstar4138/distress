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
    -R --recursive    recursively add all files in directory
    -f --force    force overwrite of existing file    [not done]
    -t TIMEOUT --timeout=TIMEOUT    the unix time when this file expires
    -r --removable  turn on the ability to delete the file from the network
    -p --pause  if pause is turned on, it will ask you if your settings are correct before adding

TIMEOUT Formatting:
    The timestamp must be given as either an integer, or in RFC 2822 format.
    See the unix 'date' command for more information on these formatting types,
    which can be given respectively:
        date +%s
        date -R
    Note all timestamps are considered to be UTC, unless otherwise stated.

    Example:
        ./distress -p -t "$(date --date '09:00 next Fri' -R)" destruct.txt
"""
from os import path, walk

from _config import build_cmd_args
from api.distress_API import encrypt_file
from email.utils import parsedate_tz, mktime_tz

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
        return __expand( filepath )
    else:
        raise Exception("File path is not a file, did you want recursive add?")

def __expand( dirpath ):
    """Expand a directory path and list all files."""
    return [ path.join(dp,f) for dp,dn,fn in walk( path.expanduser( dirpath ) ) for f in fn ]


def determine_key( args, config ):
    """ Check if the user provided an override key, otherwise we'll default to
        the global key stored in the configuration.
    """
    return args['--key'] if args['--key'] else config['global']['key']

def determine_expires( args ):
    """ Expires defaults to infinity if not provided. It can be overridden using
        a unix timestamp.
    """
    expires = "infinity"
    if args['--timeout']:
        timeout = args['--timeout']
        try: expires = int(timeout)
        except: # Not an integer, so RFC 2822
            try: expires = int( mktime_tz( parsedate_tz( timeout ) ) )
            except: pass
    return expires

def determine_delete( args ):
    """ Removable defaults to False if not provided. It can be overridden
        by simply toggling --removable.
    """
    return args['--removable']

def add(socket, args, config, library, cmd=False):
    """ Handles user input to add a file or set of files to the selected
        distress network. This functionality can all be overridden via the
        configuration file.
    """
    ret = []
    try:
        files = determine_filepaths( args )
        key = determine_key( args, config )
        expires = determine_expires( args )
        removable = determine_delete( args )

        if key == '':
            print 'No key found: first run ./distress config global.key <key>'
            return ret

        if args['--pause']: #assumes cmd=True
            print "Adding", len(files),"file(s): ",files
            print "> Expires: ",expires
            print "> Removable: ",removable
            if not raw_input("Continue? [y/N] ").lower() in ["y","yes"]:
                print "Canceled!"
                return ret

        for filename in files:
            try:
                receipt = encrypt_file(socket, library, filename, key,
                                        expires, removable, cmd)
                if cmd: print receipt.get_filename()+" added!"
                else: ret.append( receipt )
            except IOError as err:
                print "Error: file '" + filename + "' is not readable"

    except Exception as e: print e

    # Return set of receipts if called via shell
    if not cmd: return ret


## If run by command-line, we generate config and grab arguments
## then run the addition function. Otherwise this module just provides
## a function of the same name, which can be used via shell.
if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__ )
    add( socket, args, config, library, cmd=True )

