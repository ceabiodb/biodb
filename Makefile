all:

check:
	R -q -e "devtools::check('$(CURDIR)');warnings()"
#./check-pkg

test:
	R -q -e "devtools::test('$(CURDIR)');warnings()"
	
clean:
	$(RM) src/*.o src/*.so src/*.dll

.PHONY: all clean
