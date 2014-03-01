##
## Receipts and File Library Object Definitions
##
from os import path
from zipfile import ZipFile, ZIP_DEFLATED

## EXTENSIONS
OID_EXT=".oid"
KEY_EXT=".keys"
HASH_EXT=".hshs"

class Receipts(object):
    """ Simple accessor object that gets pulled from a Library. """

    def __init__(self, oid, key, hashs, salts): 
        self.__oid = oid
        self.__key = key
        self.__hashs = hashs
        self.__salts = salts

    def get_oid(self): 
        """ Returns the OID for the file. Note this will be None if the file 
          does not have write/delete access.
        """
        return self.__oid

    def get_key(self):
        """ Returns the Encryption Key for the file. Note this will be None if
          the file does not have read access.
        """
        return self.__key

    def get_hashs(self):
        """ Returns the Hashes in their correct order. """
        return self.__hashs

    def get_salts(self):
        """ Returns the Salts in their correct order. Note this will be None if
          the file does not have read access.
        """
        return self.__salts
    

class Library(object):
    """ An encrypted zip file which holds a set of receipts. """
    LIBRARY_REFERENCE = b'DISTRESSLIBv1'
    META_FILE = 'META' 
    META_NAMES= ['name','date','size']

    def __init__(self, path, password=None):
        self.__path = path
        self.__password = password
        self.__verify()

    def __verify(self):
        if not path.exists( self.__path ):
            with ZipFile( self.__path, mode='a', compression=ZIP_DEFLATED ) \
                as ref: 
                    ref.comment = Library.LIBRARY_REFERENCE
                    ref.writestr(Library.META_FILE, b'')
        else:
            valid = False
            with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) \
                as ref: 
                    valid = (ref.comment == Library.LIBRARY_REFERENCE) and \
                            (Library.META_FILE in ref.namelist())
            if not valid: raise RuntimeError("Invalid Library File")

    def get_names(self):
        """ List all files stored in the library. It does so by looking up
         all the hash files stored. Useful for quick checking if a file exists.
        """
        global HASH_EXT
        names = []
        with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) as ref:
            names = [ path.splitext( f )[0]     \
                        for f in ref.namelist() \
                            if f.endswith( HASH_EXT ) \
                    ]
        return names

    def meta(self):
        """ Returns a map of IDs to the rest of their metadata. """
        def parse_meta( data ):
            ids={}
            for line in data.decode("utf-8").split():
                s = line.split(',')
                ids[splits[0]]=dict(zip(Library.META_NAMES,splits[1:]))
            return ids
             
        idmap={}
        with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) as ref:
            dat = b''
            with ref.open(Library.META_FILE) as meta: dat = meta.read()
            idmap=parse_meta(dat)
        return idmap

    def list(self):            
        """ Returns a map of IDs to their filenames. """
        ids=self.meta()
        for ida,meta in ids.items():
            ids[ida] = meta['name']
        return ids
 
    def make_receipt( HashList, Salts=[], Oid=None, Key=None ):
        """ Generate a Receipt object when a file has been added to the Distress
         network then add it to the database. This will return the object when 
         it's done.
        """
        pass
   
    def get_receipt(self, nameid):
        """ Get a receipt for a particular file for retreival from the network.
        """
        pass

    def import_receipt(self, receipt): 
        """ Imports the Receipt into the Library and returns the ID it's given.
        """
        pass

    def export_receipt(self, nameid):
        """ Exports the Receipt from the Library given a name or ID. """
        pass

    def remove_receipt(self, nameid):
        """ Removes the Receipt from the Library. """
        pass

