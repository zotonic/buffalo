ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := buffalo

REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

.PHONY: compile deps xref test clean distclaen test docs

all: deps compile

$(REBAR):
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)

compile: $(REBAR)
	$(REBAR) compile

xref: compile
	./rebar3 xref

deps: $(REBAR)
	$(REBAR) get-deps

clean: $(REBAR)
	$(REBAR) clean

distclean: clean $(REBAR)
	rm -rf _build

test: $(REBAR)
	$(REBAR) get-deps compile
	$(REBAR) eunit -v skip_deps=true

##
## Doc targets
##
docs: $(REBAR)
	$(REBAR) doc

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.riak_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS)

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin | \
		fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	@echo 
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo 
	sleep 5
	rm $(COMBO_PLT)

