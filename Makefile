PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

build:
	R CMD BUILD .

check: build
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	rm $(PKGNAME)_$(PKGVERS).tar.gz
	rm -fr "genthat.Rcheck"
	rm -f src/*.so src/*.o
	rm -fr man

install:
	R CMD INSTALL .

test:
	Rscript -e 'options(genthat.run_integration_test=TRUE); devtools::test()'

