#! usr/bin/env python2

import os
import math
import socket
import base64
import binascii
import json
import distress_cmsg
import distress_receipt
import time


# REQUIRES PyCrypto 2.6 or later
# PyCrypto can be found at https://www.dlitz.net/software/pycrypto/
from Crypto.Cipher import AES
from Crypto.Hash import SHA384
from Crypto.Random import random

CHUNK_SIZE = 512

def encrypt_file(socket, library, filename, key):
	""" 
	Encrypts the file using key. 
	"""
	# 1) Chunk up the file into equal sized blocks
	blocks = __chunk(filename)

    # 2) Record the block's order and make random masks
	masks = [os.urandom(CHUNK_SIZE) for _ in blocks] 

    # 3) Encrypt each block using the key and masks
	encrypted_blocks = [__encrypt(b,m,key) for b,m in zip(blocks,masks)]
 
	# 4) Get the Hash of each block, and shuffle them. 
	plaintext_hashes = [__SHA384(b) for b in blocks]
	encrypted_hashes = [__SHA384(b) for b in encrypted_blocks]
	plaintext_hashes_shuffled = list( plaintext_hashes ) #make copy.
	random.shuffle( plaintext_hashes_shuffled )

	# 4b) At this point we consolidate the encrypted blocks if there are
	#     duplicate plaintext hashes. This is rare!
	packet, masks = __purify( plaintext_hashes, 
 								plaintext_hashes_shuffled,
 								encrypted_blocks, 
 								masks )

    # 5) Send the encrypted blocks with a random plaintext hash as it's key.
	oid = __send_add(socket, packet)

	# 6) Using the returned Owner ID, save the hashes and key in a Receipt.
	#    The shuffled hashes still designate order of the encrypted blocks on
	#    our side.
	return library.make_receipt( filename, plaintext_hashes_shuffled, 
									masks, oid, key )
	

def __purify(pths, spths, blocks, masks):
	"""
	Returns the packet to send to DISTRESS and an updated list of masks which
	overrides the duplicate values. This does two things, it first reduces the
	number of blocks stored on the network, but it also makes sure the masks
	are aligned with the shuffled location.
	"""
	seen,packet,nl = {}, [], []
	keyvals = zip(spths,blocks)
	for pth,kv,m in zip(pths,keyvals,masks):
	 	if not pth in seen: 
	 		seen[pth] = m
			packet.append( kv )
		nl.append( seen[pth] )
	return packet,nl

def __chunk(file_path):
	"""
	Returns a list of chunks, XX-bit lengths of the file.
	"""
	filesize = os.path.getsize(file_path)

	# Ensure the proper number of chunks regardless of Python version
	num_chunks = math.ceil(filesize / float(CHUNK_SIZE))
	num_chunks = int(num_chunks)
	
	object_file = open(file_path, 'rb')
	chunks = []
	for i in range(num_chunks):
		chunks.append(object_file.read(CHUNK_SIZE))

	return chunks


def __SHA384(object):
	"""
	Computes the hash of object. Uses crytographically secure
	SHA384 from PyCrypto. 
	"""
	object_hash = SHA384.new()
	object_hash.update(object)
	return object_hash.hexdigest()


def __encrypt(block, mask, key):
	"""
	Encrypts the block using AES scheme and key.
	"""
	def sxor(b,m): # String XOR, adds NULL padding if needed.
	    def o(x): return ord(x) if x is not None else 0
	    return ''.join(chr(o(x)^o(y)) for x,y in map(None,b,m))

	myCipher = AES.new(key)
	padded_block = sxor(block,mask)
	encrypted_block = myCipher.encrypt(padded_block)
	return encrypted_block

def __decrypt(block, mask, key):
	"""
	Decrypts the block using AES scheme and key.
	"""
	def sxor(b,m): # String XOR
	    def o(x): return ord(x) if x is not None else 0
	    return ''.join(chr(o(x)^o(y)) for x,y in map(None,b,m))

	decr = AES.new(key)
	masked_value = decr.decrypt(block)
	padded_value = sxor( masked_value, mask ) ## possibly padded.
	return padded_value.rstrip('\0')

def __send_add(socket, packet, expires="infinity", removable=False):
	"""
	Send the encrypted hashes with their respective blocks to the
	network, and returns the OID that provides proof of ownership.
	"""
	global CHUNK_SIZE
	num_blocks = len(packet)

    # Send notification of block adds.
	add_message = distress_cmsg.add(num_blocks, expires, removable)
	socket.send(add_message.encode())

	# OID shouldn't be larger than 64, right?
	response = distress_cmsg.decode(socket.recv(64))

	assert(response['msg'] == 'ack')
	oid = response['oid']
	
	# send the key/value pairs for each chunk
	for key,chunk in packet:
		assert ( len(chunk) == CHUNK_SIZE )
		block = base64.b64encode( chunk )
		send_message = distress_cmsg.addblock(key,block)
		print "send:", send_message
		socket.send( send_message )
        msg = socket.recv(13) #wait for response, we can ignore if error.
        print "\trecv:", msg

	return oid

def recieve(socket, receipt, file_location):
	"""
	Fetches and writes the file described in receipt. File file_location
	will be download_directory, and the file will be decrypted if possible.
	receive() will return True if it is decypted, otherwise False.
	"""
	# Get each of the chunks, depending on hash order
	# Decrypt then append each to the file in download_directory

	# Gets the key and tests if the file is readable
	key = receipt.get_key()
	read_access = (key != None)

	hashes = receipt.get_hashs()
	salts = receipt.get_salts()

	with open(file_location,'w') as out_file:
		for i in range(len(hashes)):
			# Get the block
			current_key = hashes[i]

			block_request = distress_cmsg.getblock(current_key)

			socket.send(block_request)
			value = distress_cmsg.decode(socket.recv(CHUNK_SIZE*2))['val']
                		
			if value == 'missing':
				raise Exception('The chunk was not in the network!')
			else:
				value = base64.b64decode(value)
			
			# Decrypt if possible
			if read_access: value = __decrypt(value, salts[i], key)

			out_file.write(value)

	return read_access

def __test_recieve():
	# This is to test the recieve function. You can edit the particular
	# OID, hashes, and keys depending on what files you've put into the 
	# network. 
	test_receipt = distress_receipt.Receipts('93aab4f6-1e51-4719-93eb-64f727da451b',
						'abcdefghijklmnop',
						['a23e6fffd0a22e288aafa3dfe7deafe53ff8ff533b456e74f539fbfbf4f222d8d9c6ebb9d88fbca6d6e5fa612eb62bf1'],
						None)
	download_directory = '/'
	sock = socket.socket()
	sock.connect(('127.0.0.1', 65501))
	recieve(sock, test_receipt, download_directory)

def delete(socket, receipt):
	""" Delete all the chunks of the file in receipt, using oid
	as the delte key. """

	if receipt.get_oid == None:
		print('You do not have deletion permission for this file!')
		return

	for block_hash in receipt.get_hashs():
		delete_mesage = distress_cmsg.delblock(receipt.get_oid(), block_hash)
		socket.send(delete_message)

	# Send {msg: Delete, key: myKey, oid: myOid}
	socket.send(something)
	delete_message = distress_cmsg.delete(receipt.get_oid(), key)

	return

# TEST_SOCKET = socket.socket()
# TEST_SOCKET.connect(('127.0.0.1', 65501))
# lib = getlib()
# encrypt_file(TEST_SOCKET, lib,'C:\\test_file.txt','abcdefghijklmnop')
# encrypt_file(TEST_SOCKET, lib, '/home/john/test_file.txt','abcdefghijklmnop')
