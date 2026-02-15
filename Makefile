R ?= R
RSCRIPT ?= Rscript
TEST_SCRIPT := tests/test-srcref-imputation.R

.PHONY: install test clean

install:
	$(R) CMD INSTALL .

test:
	$(RSCRIPT) $(TEST_SCRIPT)

clean:
	rm -rf *.tar.gz *.Rcheck
