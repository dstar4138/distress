#
# Build system for DISTRESS client and service. See subsequent 
# make files for individual build and tests.
#

CLIENT_DIR=$(CURDIR)/client
SERVER_DIR=$(CURDIR)/server

.PHONY: all build_client build_service test clean

all: build_server build_client

build_client:
	@cd $(CLIENT_DIR) ; echo "Client build not implemented."

build_server:
	@cd $(SERVER_DIR) ; make compile

test:
	@cd $(SERVER_DIR) ; make test

clean:
	@cd $(SERVER_DIR) ; make clean

