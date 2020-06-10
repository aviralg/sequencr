.PHONY: all build check document test

all: document build check

build: document
	R CMD build .

check: build
	R CMD check sequencr*tar.gz

clean:
	-rm -f sequencr*tar.gz
	-rm -fr sequencr.Rcheck
	-rm -rf src/*.o src/*.so

document:
	Rscript -e 'devtools::document()'

test:
	Rscript -e 'devtools::test()'

lintr:
	R --slave -e "lintr::lint_package()"
