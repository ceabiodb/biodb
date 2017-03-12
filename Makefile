all:

check:
	@R -q -e "devtools::check('$(CURDIR)');warnings()"

test:
	@R -q -e "devtools::test('$(CURDIR)');warnings()" | sed -e "s!(@\(.*\)#\([0-9]*\)).*\$$!at $(CURDIR)/tests/testthat/\1:\2!"

win:
	@R -q -e "devtools::build_win('$(CURDIR)');warnings()"

clean:
	$(RM) src/*.o src/*.so src/*.dll

.PHONY: all clean win test check
