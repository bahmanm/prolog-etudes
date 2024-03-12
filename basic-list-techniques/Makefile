####################################################################################################

basic-list-techniques.root.dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

####################################################################################################

define basic-list-techniques.sources :=
	recursion
	mapping
	mapping_nested
	diff_list
endef

define basic-list-techniques.modules :=
	basic_list_techniques_recursion
	basic_list_techniques_mapping
	basic_list_techniques_mapping_nested
	basic_list_techniques_diff_list
endef

####################################################################################################

.PHONY : basic-list-techniques.test

basic-list-techniques.test : bmakelib.default-if-blank( test-coverage-report,no )
basic-list-techniques.test :
	$(if $(filter-out no,$(test-coverage-report)),\
		$(call etude.test.with-coverage,basic-list-techniques),\
		$(call etude.test.without-coverage,basic-list-techniques))
