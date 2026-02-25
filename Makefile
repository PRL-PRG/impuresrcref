R = R
RSCRIPT = Rscript

.PHONY: install test clean

install:
	$(R) CMD INSTALL .

test:
	$(RSCRIPT) -e "testthat::test_check('imputesrcref')"

clean:
	rm -rf *.tar.gz *.Rcheck
