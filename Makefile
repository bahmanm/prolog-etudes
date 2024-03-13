SHELL := /usr/bin/env bash
.DEFAULT_GOAL := test

####################################################################################################

root.dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
build.dir := $(root.dir)_build/

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

test : | $(build.dir)
test : $(etudes:%=%.test)

####################################################################################################

test.coverage-report.dir := $(build.dir)test-coverage-reports/
test.coverage-data.file := $(build.dir)test-coverage.data

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
	-g 'coverage(run_tests).' \
	-g 'cov_save_data("$(test.coverage-data.file)", [append(true)]).' \
	-g 'show_coverage([ \
		  all(false) \
		, color(false) \
		, annotate(false) \
		, line_numbers(true) \
		, width(140) \
		, modules([$(call root.etude.modules,$(1))]) \
            ]).' \
	-t 'halt.' \
	$($(1).sources:%=%.prolog) 2 > $(test.coverage-report.dir)$(1).txt
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
