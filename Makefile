include inst/templates/make_file

# Get all packages (biodb and pkgensions)
PKGS=$(sort $(patsubst ../%/DESCRIPTION,%,$(wildcard ../biodb*/DESCRIPTION)))

debug::
	$(info PKGS=$(PKGS))

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

#$(eval $(call make_check_target,check.biodb,biodb,.,doc check.all install))
$(foreach pkg,$(PKGS),$(eval $(call make_check_target,check.$(pkg),$(pkg),../$(pkg),doc check.all install)))
$(foreach pkg,$(PKGS),$(eval $(call make_check_target,long.check.$(pkg),$(pkg),../$(pkg),test.all doc check.all install)))

check.pkgs: $(PKGS:%=check.%) git.status

long.check.pkgs: $(PKGS:%=long.check.%) git.status

git.status:
	@echo
	@for pkg in $(PKGS) ; do echo -e "\n$$pkg:" ; cd  ../$$pkg ; git status -su ; cd $(CURDIR) ; done
