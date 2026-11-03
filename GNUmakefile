ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := buffalo

# Prefer rebar3 on PATH, falling back to a local download.
REBAR := $(shell command -v rebar3 2>/dev/null)
ifeq ($(REBAR),)
REBAR := ./rebar3
endif
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

.PHONY: compile deps xref dialyzer test clean distclaen test edoc

all: deps compile

./rebar3:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$@"}])' \
	  -s init stop
	chmod +x $@

compile: $(REBAR)
	$(REBAR) compile

xref: compile
	$(REBAR) xref

dialyzer: compile
	$(REBAR) dialyzer

deps: $(REBAR)
	$(REBAR) get-deps

clean: $(REBAR)
	$(REBAR) clean

distclean: clean $(REBAR)
	rm -rf _build

test: $(REBAR)
	$(REBAR) get-deps compile
	$(REBAR) eunit -v skip_deps=true
	$(REBAR) as test proper

##
## Doc targets
##
edoc: $(REBAR)
	$(REBAR) edoc

edoc_private: $(REBAR)	
	$(REBAR) as edoc_private edoc

exdoc: $(REBAR)	
	$(REBAR) ex_doc --logo doc/img/logo.png --output edoc
