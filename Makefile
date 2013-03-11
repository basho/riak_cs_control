REPO		?= riak_cs_control

PKG_REVISION    ?= $(shell git describe --tags)
PKG_VERSION	?= $(shell git describe --tags | tr - .)
PKG_ID           = riak-cs-control-$(PKG_VERSION)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

.PHONY: rel deps test

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps
	@rm -rf $(PKG_ID).tar.gz

test: all
	@./rebar skip_deps=true eunit

##
## Release targets
##
rel: deps compile
	@./rebar generate skip_deps=true $(OVERLAY_VARS)

relclean:
	rm -rf rel/riak-cs-control

##
## Developer targets
##
stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak-cs-control/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/riak-cs-control/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/riak-cs-control/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/riak-cs-control/lib;)

##
## Doc targets
##
docs:
	./rebar skip_deps=true doc

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool eunit syntax_tools compiler
PLT = $(HOME)/.riak-cs-control_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(PLT) --apps $(APPS)

build_plt: compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS)

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return -Wunmatched_returns --plt $(PLT) ebin

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(PLT)

##
## Packaging targets
##
.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE
package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	make -C package/$(PKG_ID) deps
	for dep in package/$(PKG_ID)/deps/*; do \
		echo "Processing dep: $${dep}"; \
		mkdir -p $${dep}/priv; \
		git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git; \
	done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	make -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package
