CHECK_OUTPUT_DIR := /tmp
PKG_NAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKG_VERSION := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
ARCHIVE := $(PKG_NAME)_$(PKG_VERSION).tar.gz

.PHONY: build document check clean install test install-dependencies docker

all: install

build: document
	R CMD build .

document:
	Rscript -e "devtools::document()"
	Rscript -e "rmarkdown::render('README.Rmd')"

check: build
	R CMD check --as-cran --output=$(CHECK_OUTPUT_DIR) $(ARCHIVE)

clean:
	rm -f $(ARCHIVE)
	rm -f src/*.so src/*.o

install: build
	R CMD INSTALL --preclean --with-keep.source $(PKGNAME)_$(PKGVERS).tar.gz

test:
	Rscript -e 'options(genthat.run_integration_test=TRUE); devtools::test(stop_on_failure=TRUE, reporter="summary")'

install-dependencies:
	Rscript -e 'devtools::install_dev_deps()'

docker:
	docker build --rm -t prlprg/genthat .
