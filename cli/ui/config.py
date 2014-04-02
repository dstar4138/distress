"""
Returns list of all local reciepts

Usage: 
    distress config <name> <value>

"""
from _config import getconfig
from _config import build_cmd_args


def is_arg (config, setting, value):

    (section, name) = setting.split ('.')

    if section not in config or name not in config[section]:
        print "'" + setting + "' is an invalid setting"
        return
        
    config[section][name] = value
    config.write ()

            
if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__ )
    config = getconfig ()

    is_arg (config, args['<name>'], args['<value>'])