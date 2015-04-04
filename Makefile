# prepare the package for release
# taken from https://github.com/yihui/knitr/blob/master/Makefile
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC := $(shell basename `pwd`)

# all: doc install

# be careful when generating documentation, it can a very long time!
doc: man pdf html

man:
	Rscript -e "library(bnstruct); roxygen2::roxygenize()"

pdf: vignettes/bnstruct.Rnw vignettes/bibtex.bib
	cd vignettes; \
	Rscript -e "library(knitr); knit('bnstruct.Rnw')"; \
	pdflatex bnstruct.tex; \
	bibtex bnstruct.aux; \
	pdflatex bnstruct.tex; \
	pdflatex bnstruct.tex
    
html: vignettes/bnstruct.html

vignettes/bnstruct.html: vignettes/bnstruct.Rmd vignettes/custom.css
	cd vignettes; \
	Rscript -e "library(knitr); knit2html('bnstruct.Rmd', stylesheet='custom.css')";


install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)

build:
	cd ..;\
	R CMD build $(PKGNAME)

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran  $(PKGNAME)_$(PKGVERS).tar.gz
