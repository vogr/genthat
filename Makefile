build:
	R CMD BUILD .

check:
	cd ..; \
	R CMD CHECK . --as-cran

clean:
	rm -f src/*.so src/*.o
	rm -fr man

install:
	cd ..; \
	R CMD INSTALL .

test:
	Rscript -e 'options(genthat.run_integration_test=TRUE); devtools::test()'

