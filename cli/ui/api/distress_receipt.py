##
## Receipts and File Library Object Definitions
##
import time
import shutil
import base64
import tempfile
from os import path
from zipfile import ZipFile, ZipInfo, ZIP_DEFLATED

## EXTENSIONS
OID_EXT=".oid"
KEY_EXT=".keys"
HASH_EXT=".hshs"

def test_lib():
    l = Library("example.library")
    hashs = ["abc","def","ghi","jkl","mno","pqr"]
    salts = ["a","d","g","j","m","p"]
    key = "abracadabra"
    oid = "1092830-19238-7492-8429-9120380923"
    r = l.make_receipt("testfile.txt", hashs, salts, oid, key)
    return (l,r)

def load_receipt_file( path, lock=None ):
    """ Loads an exported receipt for importing into a Library. If the
        receipt was locked, it will need a password. This is the opposite
        operation of Receipt.save(). Note the path must be a full path 
        including filename.
    """
    ret = True
    with ZipFile( path, mode='r', compression=ZIP_DEFLATED ) as ref:
        files = ref.infolist()
        oid = [f for f in files if f.filename.endswith(OID_EXT)]
        key = [f for f in files if f.filename.endswith(KEY_EXT)]
        hashs = [f for f in files if f.filename.endswith(HASH_EXT)]
        if oid: self.__oid = ref.read(oid[0],lock)
        if key:
            with ref.open(key[0],'r',lock) as tmp:
                self.__key = tmp.readline().strip()
                while tmp.peek(): self.__salts.append( tmp.readline().strip() )
            self.__salts=map(base64.b64decode,self.__salts)
        if hashs: self.__hashs = ref.read(hashs[0],lock).split()
        else: ret = False
    return ret

class Receipt(object):
    """ Simple accessor object that gets pulled from a Library. """

    def __init__(self, filename, oid, key, hashs, salts):
        self.__name = filename
        self.__oid = oid
        self.__key = key
        self.__hashs = hashs
        self.__salts = salts

    def get_filename(self):
        """ Returns the filename for which this receipt is for. """
        return self.__name

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

    def save(self, path, readable=True, removable=False, lock=None):
        """ Saves this receipt on it's own so that you can transfer it to
         another computer separately, aka exporting the Receipt. Note if you
         set readable to False, this means no one can decrypt the file once
         they have all the pieces. If it is removable, then they will be able
         to delete the chunks of the file from the network. You can also lock
         the receipt zip using a password. Also note that the path will be
         appended with the receipt filename.
        """
        filepath = path+self.__name+".receipt"
        with ZipFile( filepath, mode='w', compression=ZIP_DEFLATED ) as ref:
            if lock: ref.setpassword( lock )
            if removable and self.__oid:
                ref.writestr(self.__name+OID_EXT,self.__oid)
            if readable and self.__key:
                salts = map(base64.b64encode,self.__salts)
                cnt_key = self.__key + "\n" + "\n".join( salts )
                ref.writestr(self.__name+KEY_EXT,cnt_key)
            ref.writestr(self.__name+HASH_EXT, "\n".join( self.__hashs ))
        return filepath


class Library(object):
    """ An encrypted zip file which holds a set of receipts. """
    LIBRARY_REFERENCE = b'DISTRESSLIBv1'

    def __init__(self, libpath, password=None):
        self.__path = path.expanduser( libpath )
        self.__password = password
        self.__verify()

    def __verify(self):
        if not path.exists( self.__path ):
            with ZipFile( self.__path, mode='a', compression=ZIP_DEFLATED ) \
                as ref:
                    ref.comment = Library.LIBRARY_REFERENCE
        else:
            valid = False
            with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) \
                as ref:
                    valid = (ref.comment == Library.LIBRARY_REFERENCE)
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
        idmap={}
        with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) as ref:
            fs = [x for x in ref.filelist if x.filename.endswith( HASH_EXT ) ]
            for f in fs:
                i,s = f.comment.split(',')
                idmap[i] = {'date': f.date_time,
                            'name': path.splitext( f.filename )[0],
                            'size': s}
        return idmap

    def list(self):
        """ Returns a map of IDs to their filenames. """
        ids=self.meta()
        for ida,meta in ids.items():
            ids[ida] = meta['name']
        return ids

    def make_receipt(self, filename, HashList, Salts=[], Oid=None, Key=None ):
        """ Generate a Receipt object when a file has been added to the Distress
         network then add it to the database. This will return the object when
         it's done.
        """
        def write_file( ref, name, comment, dat ):
            info = ZipInfo( name, time.localtime()[0:6] )
            info.comment = comment
            ref.writestr( info, dat )

        cnt_hashs = "\n".join( HashList )
        cnt_keys,cnt_oid = None, None
        if Key is not None or len(Salts) > 0:
            cnt_keys = (Key if Key is not None else "")+"\n"
            cnt_keys += "\n".join(map(base64.b64encode,Salts))
        if Oid is not None: cnt_oid = Oid
        with ZipFile( self.__path, mode='a', compression=ZIP_DEFLATED ) as ref:
            name = filename+HASH_EXT
            comment = self.__newid(ref)+","+str(len(HashList))
            write_file( ref, name, comment, cnt_hashs )
            if cnt_keys: write_file( ref, filename+KEY_EXT, comment, cnt_keys )
            if cnt_oid:  write_file( ref, filename+OID_EXT, comment, cnt_oid  )
        return self.get_receipt( filename )

    def get_receipt(self, nameid):
        """ Get a receipt for a particular file for retreival from the network.
        """
        tmp = None
        fsl = self.list()
        for k,l in fsl.items():
            if nameid in [ k, l ]:
                tmp = l
                break
        if not tmp: return None

        with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) as ref:
            filename = tmp
            hashs = ref.read(filename+HASH_EXT).split()
            oid,key,salts = None,None,None
            filelist = [f.filename for f in ref.filelist]
            if filename+OID_EXT in filelist:
                oid = ref.read(filename+OID_EXT)
            if filename+KEY_EXT in filelist:
                key,salts='',[]
                with ref.open(filename+KEY_EXT) as f:
                    key = f.readline().strip()
                    while f.peek(): salts.append(f.readline().strip())
                salts=map(base64.b64decode,salts)
            tmp = Receipt( filename, oid, key, hashs, salts )
        return tmp

    def import_receipt(self, receipt):
        """ Imports the Receipt into the Library and returns the ID it's given.
        """
        newid = None
        with ZipFile( self.__path, mode='a', compression=ZIP_DEFLATED ) as ref:
            filename = receipt.get_filename()
            if receipt.get_oid() and filename+OID_EXT not in ref.filelist:
                ref.writestr(filename+OID_EXT, receipt.get_oid())
            Key = receipt.get_key()
            Salts = receipt.get_salts()
            filelist = [f.filename for f in ref.filelist]
            if (Key or Salts) and filename+KEY_EXT not in filelist:
                cnt_keys = (Key if Key is not None else "")+"\n"
                cnt_keys += "\n".join(Salts)
                ref.writestr(filename+KEY_EXT, cnt_keys)
            if receipt.get_hashs() and filename+HASH_EXT not in filelist:
                newid = self.__newid(ref)
                info = ZipInfo(filename+HASH_EXT, time.localtime()[0:6])
                info.comment = newid+","+len(receipt.get_hashs())
                ref.writestr( info, receipt.get_hashs() )
            elif filename+HASH_EXT in filelist:
                for k,v in self.list().items(): #File exists, so return its id
                    if v == filename:
                        newid = k
                        break
        return newid

    def __newid(self, ref):
        """ Generates a new ID given an open zipfile reference. """
        ids = [ f.comment.split(',')[0]
                    for f in ref.infolist()
                    if f.filename.endswith(HASH_EXT) ]
        myid = len(ids)+1
        while (str(myid) in ids): myid+=1
        return str(myid)

    def remove_receipt(self, nameid):
        """ Removes the Receipt from the Library. """
        def getid( zipinfo ): return zipinfo.comment.split(',')[0]
        
        fileid = None
        fsl = self.list()
        for k,l in fsl.items():
            if nameid in [ k, l ]:
                fileid = k
                break
        if not fileid: return True
        tempdir = tempfile.mkdtemp()
        try:
            tempname = path.join( tempdir, 'test.zip' )
            with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) \
            as ref:
                with ZipFile( tempname, mode='w', compression=ZIP_DEFLATED ) \
                as tmpref:
                    tmpref.comment = Library.LIBRARY_REFERENCE
                    for item in ref.infolist():
                        if not getid(item) == fileid:
                            data = ref.read(item.filename)
                            tmpref.writestr(item, data)
            shutil.move( tempname, self.__path )
        finally:
            shutil.rmtree( tempdir )

