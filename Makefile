
REBAR = $(shell which rebar3)

.PHONY: help check_rebar compile clean distclean dialyzer benchmark5000 benchmark10000 benchmark50000

help: ## Show this help.
	@perl -e '$(HELP_FUN)' $(MAKEFILE_LIST)

check_rebar:
ifeq ($(REBAR),)
ifeq ($(wildcard rebar3),)
	curl -O https://s3.amazonaws.com/rebar3/rebar3
	chmod a+x rebar3
	./rebar3 update
	$(eval REBAR=./rebar3)
else
	$(eval REBAR=./rebar3)
endif
endif

compile: check_rebar ## same as rebar3 compile
	@$(REBAR) compile

eunit: check_rebar ## same as rebar3 eunit
	@$(REBAR) eunit

dialyzer: check_rebar ## rebar3 dialyzer
	$(REBAR) dialyzer

distclean: clean ## clean all
	@echo "rm -rf _build *.dump *_plt *.crashdump..."
	$(REBAR) clean --all
	rm -rf _build *.dump *_plt *.crashdump

shell: perpare ## rebar3 shell
	$(REBAR) shell

benchmark: perpare ## make benchmark 10000 chart
	@echo "Starting benchmark..."
	@erl -pa _build/default/lib/eenv/ebin -smp -eval "eenv_benchmark:test(10000), halt(0)"

benchmark5000: perpare ## make benchmark 5000 chart
	@echo "Starting benchmark count:5000..."
	@erl -pa _build/default/lib/eenv/ebin -smp -eval "eenv_benchmark:test(5000), halt(0)"

benchmark50000: perpare ## make benchmark 50000 chart
	@echo "Starting benchmark count:50000..."
	@erl -pa _build/default/lib/eenv/ebin -smp -eval "eenv_benchmark:test(50000), halt(0)"

perpare: compile
	@cd benchmark && erlc -o ../_build/default/lib/eenv/ebin *.erl && cd ..

#####COLORS#######
GREEN  := $(shell tput -Txterm setaf 2)
WHITE  := $(shell tput -Txterm setaf 7)
YELLOW := $(shell tput -Txterm setaf 3)
RESET  := $(shell tput -Txterm sgr0)

# Add the following 'help' target to your Makefile
# And add help text after each target name starting with '\#\#'
# A category can be added with @category
HELP_FUN = \
    %help; \
    while(<>) { push @{$$help{$$2 // 'options'}}, [$$1, $$3] if /^([a-zA-Z\-]+)\s*:.*\#\#(?:@([a-zA-Z\-]+))?\s(.*)$$/ }; \
    print "usage: make [target]\n\n"; \
    for (sort keys %help) { \
    print "${WHITE}$$_:${RESET}\n"; \
    for (@{$$help{$$_}}) { \
    $$sep = " " x (32 - length $$_->[0]); \
    print "  ${YELLOW}$$_->[0]${RESET}$$sep${GREEN}$$_->[1]${RESET}\n"; \
    }; \
    print "\n"; }
