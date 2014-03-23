##
## Private Local configuration setup for commands.
##
__version__='0.1.0'
from socket import socket
from os.path import expanduser,exists
from ext.docopt import docopt
from ext.configobj import ConfigObj
from api.distress_receipt import Library

__LOADED_CONFIG__=None
DEFAULT_FILENAME=expanduser("~/.config/distress/config")
DEFAULT_CONFIGSPEC =                         \
    { 'library' :                            \
            { 'dir' : '~/.config/distress/', \
              'file': 'library'              \
            },                               \
      'network' :                            \
            { 'hostip' : '127.0.0.1',        \
              'hostport' : 65501             \
            },                               \
      'global' :                             \
            { 'key' : '' }                   \
    }

def build_cmd_args( doc ):
    """ Build a common argument tuple that is used in almost all of the
    distress interactive commands. Namely, this will return a socket
    connected to the host distress node, the command arguments, the
    global configurations, and the local client library object.
    """
    args = getargs( doc )
    config = getconfig()
    library = getlib()
    sock = socket()
    sock.connect( (config['network']['hostip'],
                   int(config['network']['hostport'])) )
    return (sock, args, config, library)

def getargs( doc, **kwargs ):
    """ Generate the DOC-OPT object for the calling module. Note we just
    override the version with the ui package version.
    """
    return docopt(doc, version=__version__, **kwargs)

def getconfig( path=None ):
    """ Build and generate the global configuration object. This is
      a singleton which will be loaded each time we need to run.
    """
    global DEFAULT_FILENAME
    global DEFAULT_CONFIGSPEC
    global __LOADED_CONFIG__

    ## If singleton is not loaded, build it.
    if not __LOADED_CONFIG__:
        default = ConfigObj( DEFAULT_CONFIGSPEC )
        user = ConfigObj( path if path else DEFAULT_FILENAME )
        default.merge( user )
        __LOADED_CONFIG__ = default
        _check_if_firstload()

    return __LOADED_CONFIG__

def getlib():
    """ Builds the library from the global configuration. """
    config  = getconfig()
    return Library(config["library"]["dir"] + config["library"]["file"])


def _check_if_firstload():
    global __LOADED_CONFIG__
    global DEFAULT_FILENAME
    __LOADED_CONFIG__.filename = DEFAULT_FILENAME
    if not exists( DEFAULT_FILENAME ):
        __LOADED_CONFIG__.write()

