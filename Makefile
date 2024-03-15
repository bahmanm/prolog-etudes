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
	mkdir -p $(@) $(test.coverage-report.dir)

####################################################################################################

.PHONY : clean

clean :
	-rm -rf $(build.dir)

####################################################################################################

test : | $(build.dir)
test : $(etudes:%=%.test)

####################################################################################################

test.coverage-report.dir := $(build.dir)test-coverage-reports/
test.coverage-report.processor := $(root.dir)bin/coverage-report-converter.pl
test.coverage-data.file := $(build.dir)test-coverage.data

####################################################################################################

# 1: etude
define etude.modules
$(subst $(bmakelib.space),$(bmakelib.comma),$(strip $($(1).modules)))
endef

####################################################################################################

# 1: etude
define etude.test.coverage-report-file
$(test.coverage-report.dir)$(1)/coverage.txt
endef

####################################################################################################

# 1: etude
define etude.test.with-coverage
cd $($(1).root.dir) \
&& mkdir -p $(test.coverage-report.dir)$(1) \
&& swipl \
	-g 'coverage(run_tests).' \
	-g 'cov_save_data("$(test.coverage-data.file)", [append(true)]).' \
	-g 'show_coverage([ \
		  all(false) \
		, color(false) \
		, annotate(true) \
		, dir("$(test.coverage-report.dir)$(1)") \
		, line_numbers(true) \
		, width(140) \
		, modules([$(call root.etude.modules,$(1))]) \
            ]).' \
	-t 'halt.' \
	$($(1).sources:%=%.prolog) \
$(foreach cov-report,$($(1).sources:%=$(1)/%.prolog.cov),\
	&& $(test.coverage-report.processor) $(test.coverage-report.dir) $(cov-report))
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
