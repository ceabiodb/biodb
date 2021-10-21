include inst/templates/make_file

# Check all extensions
EXTS=$(filter-out biodb,$(patsubst ../%,%,$(wildcard ../biodb*)))

debug::
	$(info EXTS=$(EXTS))

# Generic target for checking package
# $(1): Target name.
# $(2): Package name to check.
# $(3): Path to package folder.
# $(4): Packages targets to run.
define make_check_target =
$(1):
	@echo -ne "Checking $(2)... "
	@log=$$$$(mktemp -t biodb.XXXXXX) ; \
	TIMEFORMAT="%0R" ; \
	t=$$$$(time (make -C $(3) $(4) >$$$$log 2>&1) 2>&1) ; \
	status=$$$$? ; \
	echo "$$$${t}s" ; \
	[[ $$$$status -eq 0 ]] || cat $$$$log ; \
	$(RM) $$$$log ; \
	test $$$$status -eq 0
endef

$(eval $(call make_check_target,check.biodb,biodb,.,doc check.all install))
$(foreach ext,$(EXTS),$(eval $(call make_check_target,$(ext),$(ext),../$(ext),doc check.all)))

check.ext: check.biodb $(EXTS)
	@echo
	@echo -e "\nbiodb:" ; git status -su
	@for ext in $(EXTS) ; do echo -e "\n$$ext:" ; cd  ../$$ext ; git status -su ; cd $(CURDIR) ; done
