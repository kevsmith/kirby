DEPS = deps/automeck deps/meck

all: compile eunit

clean:
	@rebar clean

distclean:
	@rebar skip_deps=true clean
	@rm -rf deps

compile: $(DEPS)
	@rebar compile
	@dialyzer -Wrace_conditions -Wunderspecs -I include -r ebin

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit