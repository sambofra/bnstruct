# prepare the package for release
# taken from https://github.com/yihui/knitr/blob/master/Makefile
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC := $(shell basename `pwd`)
WINBUILD_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-devel\nput $(PKGNAME)_$(PKGVERS).tar.gz\nquit\n"


# all: doc install

# be careful when generating documentation, it can a very long time!
doc: man pdf

man:
	Rscript -e "library(bnstruct); roxygen2::roxygenize()"; \
        cd .. ; \
        R CMD Rd2pdf bnstruct

pdf: vignettes/bnstruct.Rnw vignettes/bibtex.bib
	cd vignettes; \
	Rscript -e "library(knitr); knit('bnstruct.Rnw')"; \
	pdflatex bnstruct.tex; \
	bibtex bnstruct.aux; \
	pdflatex bnstruct.tex; \
	pdflatex bnstruct.tex
    
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

test:
	Rscript -e "devtools::test()"

winbuild:
	@echo "Winbuild: http://win-builder.r-project.org/"
	cd .. && echo $(WINBUILD_FTP_COMMANDS) | ftp -v -p -e -g -i -n win-builder.r-project.org

