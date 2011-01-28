
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

docs:
	@./rebar skip_deps=true doc

