#
# Makefile, mostly for convenience.
#
.PHONY: deps precompile release test

deps:
	rebar get-deps
	rebar prepare-deps

precompile:
	@(test -d deps || $(MAKE) deps)

release: precompile
	rebar clean
	rebar compile
	rebar escriptize

test:
	rebar compile
	rebar escriptize
	rebar ct
