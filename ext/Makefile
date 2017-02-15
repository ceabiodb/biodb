all:

pkg:
	Rscript makePackage.R

test: test-with-sources

test-with-sources:
	__SOURCE_BIODB=yes ./test-wrapper-peakforest

test-with-package:
	./test-wrapper-peakforest

clean:
	$(RM) result.txt

.PHONY: all test clean
