##
## Receipts and File Library Object Definitions
##
import time
import shutil
import base64
import tempfile
from os import path,chmod
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
    ret = None
    _oid,_key,_hashs,_salts,_filename=None,None,None,[],None
    with ZipFile( path, mode='r', compression=ZIP_DEFLATED ) as ref:
        files = ref.infolist()
        oid = [f for f in files if f.filename.endswith(OID_EXT)]
        key = [f for f in files if f.filename.endswith(KEY_EXT)]
        hashs = [f for f in files if f.filename.endswith(HASH_EXT)]
        if hashs:
            _hashs = ref.read(hashs[0],lock).split()
            _filename = hashs[0].filename[:-len(HASH_EXT)]
        else: return None
        if oid: _oid = ref.read(oid[0],lock)
        if key:
            with ref.open(key[0],'r',lock) as tmp:
                _key = tmp.readline().strip()
                while tmp.peek(): _salts.append( tmp.readline().strip() )
            _salts=map(base64.b64decode,_salts)

    return Receipt( _filename, _oid, _key, _hashs, _salts )

class Receipt(object):
    """ Simple accessor object that gets pulled from a Library. """

    def __init__(self, filename, oid, key, hashs, salts):
        self.__name = filename
        self.__oid = oid
        self.__key = key
        self.__hashs = hashs
        self.__salts = salts

    def set_filename(self, newname):
        """ Rename the file, only allowed during imports."""
        self.__name=newname

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

    def save(self, savepath, readable=True, removable=False, lock=None, relative=False):
        """ Saves this receipt on it's own so that you can transfer it to
         another computer separately, aka exporting the Receipt. Note if you
         set readable to False, this means no one can decrypt the file once
         they have all the pieces. If it is removable, then they will be able
         to delete the chunks of the file from the network. You can also lock
         the receipt zip using a password. Also note that the path will be
         appended with the receipt filename.
        """
        name = self.__name if relative else path.basename( self.__name )
        filepath = path.join(savepath, name+".receipt")
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
            chmod( self.__path, 0600 )
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
        return self.get_receipt( filename, reverse=True )

    def get_receipt(self, nameid, reverse=False):
        """ Get a receipt for a particular file for retreival from the network.
        """
        fileid,filename = None,None
        fsl = self.list()
        items = fsl.items()
        if reverse: items.reverse()
        for k,l in items:
            if nameid in [ k, l ]:
                fileid,filename = k,l
                break
        if not fileid: return None

        hashs,oid,key,salts = [],None,'',[]
        with ZipFile( self.__path, mode='r', compression=ZIP_DEFLATED ) as ref:
            checkfor = [filename+OID_EXT, filename+KEY_EXT, filename+HASH_EXT]
            for f in ref.filelist:
                if f.filename not in checkfor: continue
                if f.comment.split(',')[0] == fileid:
                    if f.filename.endswith( HASH_EXT ):
                        hashs=ref.read(f).split()
                    elif f.filename.endswith( OID_EXT ):
                        oid = ref.read(f)
                    elif f.filename.endswith( KEY_EXT ):
                        with ref.open(f) as kf:
                            key=kf.readline().strip()
                            while kf.peek(): salts.append(kf.readline().strip())
                            salts=map(base64.b64decode,salts)
        return Receipt( filename, oid, key, hashs, salts )

    def import_receipt(self, receipt):
        """ Imports the Receipt into the Library and returns the ID it's given.
        """
        def write_file( ref, name, comment, dat ):
            info = ZipInfo( name, time.localtime()[0:6] )
            info.comment = comment
            ref.writestr( info, dat )

        filename = receipt.get_filename()
        newid = None
        with ZipFile( self.__path, mode='a', compression=ZIP_DEFLATED ) as ref:
            Key = receipt.get_key()
            Salts = map(base64.b64encode,receipt.get_salts())
            filelist = [f.filename for f in ref.filelist]
            comment = self.__newid(ref)+","+str(len(receipt.get_hashs()))
            if receipt.get_oid() and filename+OID_EXT not in ref.filelist:
                write_file(ref, filename+OID_EXT, comment, receipt.get_oid())
            if (Key or Salts) and filename+KEY_EXT not in filelist:
                cnt_keys = (Key if Key is not None else "")+"\n"
                cnt_keys += "\n".join(Salts)
                write_file(ref,filename+KEY_EXT, comment, cnt_keys)
            if receipt.get_hashs() and filename+HASH_EXT not in filelist:
                hashs = '\n'.join(receipt.get_hashs())
                write_file(ref, filename+HASH_EXT, comment, hashs)

        for k,v in self.list().items():
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

