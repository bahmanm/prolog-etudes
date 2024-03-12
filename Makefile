SHELL := /usr/bin/env bash
.DEFAULT_GOAL := test

####################################################################################################

root.dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

####################################################################################################

.PHONY : bmakelib/bmakelib.mk
include  bmakelib/bmakelib.mk

####################################################################################################

etudes := basic-list-techniques
etudes.makefiles := $(etudes:%=$(root.dir)%/Makefile)

.PHONY : $(etudes.makefiles)
include  $(etudes.makefiles)

####################################################################################################

$(build.dir) :
	mkdir -p $(@)

####################################################################################################

.PHONY : clean

clean :
	-rm -rf $(build.dir)

####################################################################################################

test : $(etudes:%=%.test)

####################################################################################################

test.coverage-report.dir := $(root.dir)/.coverage/

####################################################################################################

# 1: etude
define etude.modules
$(subst $(bmakelib.space),$(bmakelib.comma),$(strip $($(1).modules)))
endef

####################################################################################################

# 1: etude
define etude.test.with-coverage
cd $($(1).root.dir) \
&& swipl \
	-g 'coverage(run_tests).'\
	-g 'show_coverage([ \
		  all(false) \
		, color(false) \
		, modules([$(call root.etude.modules,$(1))]) \
		, dir("$(test.coverage-report.dir)") \
            ]).' \
	-t 'halt.' \
	$($(1).sources:%=%.prolog)
endef

####################################################################################################

# 1: etude
define etude.test.without-coverage
cd $($(1).root.dir) \
&& swipl \
	-g 'run_tests.'\
	-t 'halt.' \
	$($(1).sources:%=%.prolog)
endef

####################################################################################################
