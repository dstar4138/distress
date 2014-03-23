"""
usage: distress export [permissions] <reciept> <dest path>

Permissions:
  -r, --read 	add read permissions to reciept copy
  -d, --delete  add deletion permissions to reciept copy

Exports a copy of local <reciept> to <dest path>
to share file with others

"""
from docopt import docopt


if __name__ == '__main__':
	args = docopt (__doc__,
                   options_first=False)

	print args


