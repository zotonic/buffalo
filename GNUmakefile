ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := buffalo

REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

.PHONY: compile deps xref dialyzer test clean distclaen test docs

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

dialyzer: compile
	./rebar3 dialyzer

deps: $(REBAR)
	$(REBAR) get-deps

clean: $(REBAR)
	$(REBAR) clean

distclean: clean $(REBAR)
	rm -rf _build
	rm -rf doc

test: $(REBAR)
	$(REBAR) get-deps compile
	$(REBAR) eunit -v skip_deps=true
	$(REBAR) proper

##
## Doc targets
##
docs: $(REBAR)
	$(REBAR) edoc

edoc_private: $(REBAR)	
	$(REBAR) as edoc_private edoc

