
.PHONY: deps

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

update:
	@./rebar update-deps

tests:
	@./rebar skip_deps=true eunit

docs:
	@./rebar skip_deps=true doc

