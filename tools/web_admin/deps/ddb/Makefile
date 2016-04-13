REBAR = ./rebar
DIALYZER = dialyzer
TOUCH = touch

.PHONY: all deps compile escripize clean doc eunit ct test \
	run plt analyze get-deps compile-deps

all: deps compile

deps: get-deps compile-deps

compile: 
	@$(REBAR) compile skip_deps=true

escriptize: 
	@$(REBAR) escriptize

clean: 
	@$(REBAR) clean
	@rm -f test/*.beam erl_crash.dump ./deps/.compile-deps 

eunit: deps compile 
	@$(REBAR) skip_deps=true eunit

ct: deps compile 
	@$(REBAR) skip_deps=true ct 

test: eunit ct

## dialyzer
PLT_FILE = ~/ddb.plt
PLT_APPS ?= kernel stdlib erts compiler crypto inets ssl public_key \
		xmerl deps/*
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns \
		-Wunderspecs --verbose --fullpath -n

.PHONY: dialyze
dialyze: all
	@[ -f $(PLT_FILE) ] || $(MAKE) plt
	@dialyzer --plt $(PLT_FILE) $(DIALYZER_OPTS) ebin || [ $$? -eq 2 ];

## In case you are missing a plt file for dialyzer,
## you can run/adapt this command
.PHONY: plt
plt:
	@echo "Building PLT, may take a few minutes"
	@dialyzer --build_plt --output_plt $(PLT_FILE) --apps \
		$(PLT_APPS) || [ $$? -eq 2 ];
		
docs:
	@$(REBAR) doc skip_deps=true

get-deps:
	@$(REBAR) get-deps

compile-deps:
	@$(REBAR) compile
