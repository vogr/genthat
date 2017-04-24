NEWS     = NEWS
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

build:
	cd ..; \
	R CMD build $(PKGSRC)

install: build
	cd ..; \
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..; \
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

test:
	Rscript tests/testthat.R

clean:
	rm -f ./src/*.o

build-dev:
	cd ..; \
	R CMD build $(PKGSRC) --no-manual --no-build-vignettes

install-dev: build-dev
	cd ..; \
	R CMD INSTALL $(PKGSRC) --no-multiarch --with-keep.source

test-dev: install-dev
	Rscript tests/testthat.R