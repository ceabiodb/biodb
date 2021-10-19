include inst/templates/make_file

# Check all extensions
EXTS=$(filter-out biodb,$(patsubst ../%,%,$(wildcard ../biodb*)))
$(info EXTS=$(EXTS))

$(EXTS):
	@echo
	@echo
	@echo "================================================================"
	@echo "================================================================"
	@echo "CHECKING $@ ..."
	@echo "================================================================"
	@echo "================================================================"
	@echo
	@echo
	make -C ../$@ doc check.all

check.ext: doc check.all install $(EXTS)
	@pwd
	@echo -e "\nbiodb:" ; git status -su
	@for ext in $(EXTS) ; do echo -e "\n$$ext:" ; cd  ../$$ext ; git status -su ; cd $(CURDIR) ; done
