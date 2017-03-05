all:

check:
	R -q -e "devtools::check('$(CURDIR)');warnings()"
#./check-pkg

test:
	
clean:

.PHONY: all clean
