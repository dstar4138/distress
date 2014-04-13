"""
Returns list of all local reciepts

Usage:
    distress list [options]

Options:
    -p --no-pretty-print    Toggle pretty print off, useful for piping
    -v --verbose    Print as much information as possible
"""
from _config import build_cmd_args

def printmeta( ida, meta, pp ):
    ID,FN,E,DEL,FIN='ID: ','\t Filename: ','\n',': ','------\n'
    if not pp: ID,FN,E,DEL,FIN='','\t','','\t','\n'

    print ID,ida,FN,meta['name'],E,
    for x,y in meta.items():
        if x == 'name': continue
        if pp: print x,
        print DEL,y,E,
    print FIN,

def list_( socket, args, config, library, cmd=False ):
    """ Handles user input to list all files the client stored in the
        DISTRESS network. It does so by checking the library only.
    """
    verbose = args['--verbose']
    pp = not args['--no-pretty-print']

    if not verbose:
        names = library.get_names()
        for fname in names: print fname
    else:
        info = library.meta()
        for ida,meta in info.items():
            printmeta( ida, meta, pp )

if __name__ == '__main__':
    (socket, args, config, library) = build_cmd_args( __doc__, conn=False )
    list_( socket, args, config, library, cmd=True )

