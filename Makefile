R = R
RSCRIPT = Rscript

.PHONY: install test clean

install:
	$(R) CMD INSTALL .

test:
	$(RSCRIPT) -e "testthat::test_dir('tests/testthat', load_package = 'source')"

clean:
	rm -rf *.tar.gz *.Rcheck
