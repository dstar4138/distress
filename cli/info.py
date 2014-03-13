"""
usage: distress info <file id>

"""
from docopt import docopt


if __name__ == '__main__':
	args = docopt (__doc__,
                   options_first=False)

	print args


