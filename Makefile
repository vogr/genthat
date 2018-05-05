PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

build:
	Rscript -e "devtools::document()"
	R CMD build \
      --no-manual \
      --no-build-vignettes \
      .

check: build
	R CMD check \
      --no-codoc \
      --no-examples \
      --no-manual \
      --no-vignettes \
      --no-build-vignettes \
      $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	rm -f $(PKGNAME)_$(PKGVERS).tar.gz
	rm -fr "genthat.Rcheck"
	rm -f src/*.so src/*.o

install: build
	R CMD INSTALL .

test:
	Rscript -e 'options(genthat.run_integration_test=TRUE); devtools::test()'

docker: 
	docker build --rm -t genthat .
