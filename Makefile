#
# Build for the DISTRESS P2P File System service.
#

ERL=$(shell which erl)
DOPTS=-Ddebug
REBAR=$(CURDIR)/bin/rebar
RELDIR=$(CURDIR)/rel
RELTOOL_CFG=$(CURDIR)/conf/reltool.config

.PHONY: compile clean distclean release scripts all

## Compile the Daemon code.
debug: 
	$(REBAR) $(DOPTS) get-deps compile 
compile:
	$(REBAR) get-deps compile

## Run the EUnit tests over the Daemon.
test: debug
	$(REBAR) eunit

## Superficial clean of workspace
clean:
	$(REBAR) clean

## Total whipe of all releases and generated scripts.
distclean: clean
	-rm -rf rel

## Build a daemon release.
release: compile
	-mkdir $(RELDIR)
	cd $(RELDIR); $(REBAR) create-node nodeid=distress
	cp $(RELTOOL_CFG) $(RELDIR)
	$(REBAR) generate

