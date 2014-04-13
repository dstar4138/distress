#
# Build for the DISTRESS P2P File System service.
#
APP=distressd
ERL=$(shell which erl)
DOPTS=-Ddebug
REBAR=$(CURDIR)/bin/rebar
RELDIR=$(CURDIR)/rel
RELTOOL_CFG=$(CURDIR)/conf/reltool.config
OLD_REBAR_CONF=$(CURDIR)/conf/pre15rebar.config
SCRIPT_DEPS=-pa $(CURDIR)/ebin $(CURDIR)/deps/jsonx/ebin $(CURDIR)/deps/swarm/ebin
SCRIPT_EVAL="-eval '$(APP):start().'"

.PHONY: compile clean distclean release scripts all rm-script gen-script

## Compile the Daemon code.
debug: gen-script
	$(REBAR) $(DOPTS) get-deps compile
compile: gen-script
	$(REBAR) get-deps compile
oldcompile: gen-script
	$(REBAR) $(DOPTS) -C $(OLD_REBAR_CONF) get-deps compile

## Run the EUnit tests over the Daemon.
test: debug
	$(REBAR) skip_deps=true eunit

## Superficial clean of workspace
clean: rm-script
	$(REBAR) clean

## Total whipe of all releases and generated scripts.
distclean: clean
	-rm -rf rel

## Build a daemon release.
release: compile
	-mkdir $(RELDIR)
	cd $(RELDIR); $(REBAR) create-node nodeid=$(APP)
	cp $(RELTOOL_CFG) $(RELDIR)
	$(REBAR) generate

## Generate or remove a simple starter script for the app.
rm-script:
	-rm $(APP)

gen-script: rm-script
	touch $(APP)
	echo "#!/bin/sh" > $(APP)
	echo $(ERL) $(SCRIPT_DEPS) $(SCRIPT_EVAL) >> $(APP)
	-chmod +x $(APP)

