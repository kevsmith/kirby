DEPS = deps/automeck deps/meck

all: dialyzer eunit

clean:
	@rebar clean

distclean:
	@rebar skip_deps=true clean
	@rm -rf deps

compile: $(DEPS)
	@rebar compile

$(DEPS):
	@rebar get-deps

dialyzer: compile
	@dialyzer -Wrace_conditions -Wunderspecs -I include -r ebin

strict_dialyzer: compile
	@dialyzer -Wrace_conditions -Wunderspecs -Wspecdiffs -I include -r ebin

eunit: compile
	@rebar skip_deps=true eunit

test: eunit