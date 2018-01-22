TEST_LIB=../test-pkgs-lib
PKGS=samplepkg emptypkg
BINS=$(addsuffix _1.0.0.tar.gz,$(PKGS))

all: install clean

clean:
	rm -f $(BINS)

build: clean
	R CMD build $(PKGS)

install: build
	[ -d $(TEST_LIB) ] || mkdir -p $(TEST_LIB)
	R CMD INSTALL --library=$(TEST_LIB) --example --install-tests --with-keep.source $(BINS)

