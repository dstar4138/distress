"""
usage: distress add [options] <filepath>

Generic options
  -h, --help    show this help menu
  -r, --recursuve  recursively add all files in directory [not done]
  -f, --force     force overwrite of existing file       [not done]

"""
import os
import uuid

from docopt import docopt
from socket import socket

from api.distress_API     import distress_API
from api.distress_receipt import Receipt
from api.distress_receipt import Library
from ext.configobj        import ConfigObj


config  = ConfigObj ("conf/cli.conf")
library = Library   (config["library"]["dir"] + config["library"]["file"])

    # hashs = ["abc","def","ghi","jkl","mno","pqr"]
    # salts = ["a","d","g","j","m","p"]
    # key = "abracadabra"
    # oid = "1092830-19238-7492-8429-9120380923"
    # r = l.make_receipt("testfile.txt", hashs, salts, oid, key)

if __name__ == '__main__':
    args = docopt (__doc__,
                   version='distress 0.1',
                   options_first=False)
    
    try:
        key         = uuid.uuid4 ()
        packet, oid, hashes = distress_API.encrypt (args['<filepath>'], key)
        salts       = create_salts (hashes)
        receipt     = library.make_receipt (config["library"]["dir"] + key + ".rcpt",
                                            hashes,
                                            salts,
                                            oid,
                                            key)
    except IOError as err:
        print "Error: file '" + args['<filepath>'] + "' is not readable"


def create_salts (hashes):
  salts = []
  for i in hashes:
      salts[i] = os.urandom (512)

  return salts