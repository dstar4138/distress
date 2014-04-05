"""
Allows for updating the configuration file from the command line. See below on
naming conventions and the particular variables you are able to change.

Usage: 
    distress config <name> <value>

Variable Information:

    This is a possibly incomplete listing, but will contain most of the more 
    important configuration variables. To see a complete listing, please view 
    the manual.

    global.key  (default '')
        This is the default key used when encrypting your file chunks and
        uploading. 

    library.dir (default '~/.config/distress/')
        This is the default location to place the library file. 

    network.hostip (default '127.0.0.1')
        This is the IP the DISTRESS client will use to contact the DISTRESS
        server.

    network.hostport (default 65501)
        This is the Port the DISTRESS client will use to connect to the 
        DISTRESS server.
"""
from _config import build_cmd_args

def config(socket, args, config, library):
    """Handle's user input to configure the DISTRESS config file. 
    """
    setting, value = args['<name>'], args['<value>'] 
    (section, name) = setting.split ('.')

    if section not in config or name not in config[section]:
        print "'" + setting + "' is an invalid setting"
        return

    config[section][name] = value
    config.write()

if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__, conn=False)
    config(socket, args, config, library)

