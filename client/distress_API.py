#! usr/bin/env python3

import os
import socket
import base64
import binascii
import json

# REQUIRES PyCrypto 2.6 or later
# PyCrypto can be found at https://www.dlitz.net/software/pycrypto/
from Crypto.Cipher import AES
from Crypto.Hash import SHA384
from Crypto.Random import random

CHUNK_SIZE = 16

def encrypt_file(file, key):
	""" 
	Encrypts the file using key. 
	"""
	blocks = chunk(file)
	plaintext_hashes = [custom_hash(b) for b in blocks]
	order = [i for i in range(len(blocks))]

	encrypted_blocks = [encrypt(block,key) for block in blocks]
	plaintext_hashes_shuffled = hash_shuffle(plaintext_hashes)

	# TODO: Convert to MessagePack, or whatever we decide
	packet = list(zip(plaintext_hashes_shuffled,encrypted_blocks))

	oid = send(packet)
	make_receipt(oid, key, plaintext_hashes, order, masks = 0)


def chunk(file_path):
	"""
	Returns a list of chunks, XX-bit lengths of the file.
	"""
	filesize = os.path.getsize(file_path)
	num_chunks = filesize // CHUNK_SIZE
	object_file = open(file_path, 'rb')

	chunks = []
	for i in range(num_chunks + 1):
		chunks.append(object_file.read(CHUNK_SIZE))

	return chunks


def custom_hash(object):
	"""
	Computes the hash of object. Uses crytographically secure
	SHA384 from PyCrypto. 
	"""
	object_hash = SHA384.new()
	object_hash.update(object)
	return object_hash.hexdigest()


def encrypt(block, key):
	"""
	Encrypts the block using AES scheme and key.
	"""
	# TODO:	Salting

	# Add padding if neccesary 
	padding = AES.block_size - len(block) % AES.block_size
	if padding == AES.block_size:
		padding = 0
	block += (bytes(0) * padding)

	# encrypt a single block chunk
	myCipher = AES.new(key)
	return myCipher.encrypt(block)


def hash_shuffle(list):
	"""
	Returns a shuffled list of hashes. Uses the cryptographically
	secure shuffle from PyCrypto. 
	"""
	random.shuffle(list)
	return list


def send(packet):
	"""
	Send the packet to the Erlang listener, returns the OID.
	"""
	# TODO: Implement

	print(packet)
	sock = socket.socket()
	sock.connect(('127.0.0.1', 65501))

	message = { 'msg': 'Add', 'expires': 0, 'removable': 'true'}
	js = json.dumps(message)
	print('js:',js)
	# Send {msg: 'Add', expires: ??, removeable: true}
	sock.send(js)

	# Get the OID in return
	oid = sock.recv(1024)
	print('oid:',oid)
	while True:
		# loop through and keep sending key/values
		break

	sock.close()


def recieve(receipt):
	pass
	# send {msg: recieve, key: mykey}
	sock = socket.socket()
	sock.connect(('127.0.0.1', 65501))

	# Send {msg: Recieve, key: myKey}
	sock.send(someKey)

	# How big are the blocks?
	value = sock.recv(1024)

	sock.close()
	return
	

def delete():
	# send {msg: delete, key: mykey, oid: myoid}
	pass

	sock = socket.socket()
	sock.connect(('127.0.0.1', 65501))

	# Send {msg: Delete, key: myKey, oid: myOid}
	sock.send(something)

	sock.close()
	return


# encrypt_file('C:\\test_file.txt','abcdefghijklmnop')

def test_encryption(block, key):
	print('block:',block)
	padding = AES.block_size - len(block) % AES.block_size
	block += (bytes([padding]) * padding)

	# encrypt a single block chunk
	myCipher = AES.new(key)
	ciphertext = myCipher.encrypt(block)

	print('encrypted:',ciphertext)
	decr = AES.new(key)
	value = decr.decrypt(ciphertext).rstrip()
	
	print(len(value))
	print('decrypted:',value[:16])
	#TODO: Work out the padding situation


def test_file(file,key):
	blocks = chunk(file)
	plaintext_hashes = [custom_hash(b) for b in blocks]
	order = [i for i in range(len(blocks))]

	encrypted_blocks = [test_encryption(block,key) for block in blocks]

