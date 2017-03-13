all:

test:
	Rscript wrapperPeakforest.R -file tspec.txt -precursor 254 -mztol 0.1 -ppm 3 -mode neg >result.txt

clean:
	$(RM) result.txt

.PHONY: all test clean
