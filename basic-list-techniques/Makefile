####################################################################################################

basic-list-techniques.root.dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

####################################################################################################

basic-list-techniques.sources := recursion

####################################################################################################

.PHONY : basic-list-techniques.test

basic-list-techniques.test :
	cd $(basic-list-techniques.root.dir) \
	&& swipl -g run_tests -t halt $(basic-list-techniques.sources:%=%.prolog)
