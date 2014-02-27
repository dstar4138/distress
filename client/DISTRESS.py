#! usr/bin/env python3

import os

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

	# TODO: Implement masks
	#masks = [random() for i in range(blocks)]

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
	# Add padding if neccesary 
	padding = AES.block_size - len(block) % AES.block_size
	block += (bytes([padding]) * padding)

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
	pass


def make_receipt(oid, key, hash_plaintexts, order, masks):
	"""
	Create a 'Receipt' object and save to disk
	"""		
	# TODO: Implement
	pass


# encrypt_file('C:\\test_file.txt','abcdefghijklmnop')
