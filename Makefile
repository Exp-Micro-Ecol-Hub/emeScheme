## R parts based on https://github.com/yihui/knitr/blob/master/Makefile

PKGNAME := $(shell sed -n "s/^Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/^Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)


SRCDIR = source
OUTDIR = docs
DATADIR = $(OUTDIR)/data
INSTDIR = inst

VIGDIR = vignettes
VIGHTMLDIR = doc

DOCDIR = doc

VIGRMD = $(wildcard $(VIGDIR)/*.Rmd)
TMP1  = $(VIGRMD:.Rmd=.html)
VIGHTML = ${subst $(VIGDIR),$(VIGHTMLDIR),$(TMP1)}
VIGHTMLOUT = ${subst $(VIGDIR),$(OUTDIR),$(TMP1)}

EXAMPLEXML = $(wildcard $(INSTDIR)/emeScheme_example.xml)

RMD = $(wildcard $(SRCDIR)/*.Rmd)
TMP2  = $(RMD:.Rmd=.html)
HTML = ${subst $(SRCDIR),$(OUTDIR),$(TMP2)}

READMERMD = Readme.Rmd
READMEMD = Readme.md
READMEHTML = Readme.html


SCHEMEMAKE = library(dmdScheme); \
	setwd('SCHEME_PACKAGE'); \
	scheme_make( \
		schemeDefinition = 'emeScheme.xlsx', \
		examples = list.dirs('examples/', recursive = FALSE), \
		install_R_package = 'install_R_package.R', \
		path = '.', \
		overwrite = TRUE \
	)


#############

all: check example web build

####

clean: clean_web  clean_example

########### scheme package ###########

scheme_package:
	Rscript -e "$(SCHEMEMAKE)"

########### Website ###########

####

readme: $(READMEMD)
Readme.md: $(READMERMD)
	@Rscript -e "rmarkdown::render('$(READMERMD)', output_format = 'rmarkdown::github_document')"
	rm -f $(READMEHTML)

clean_readme:
	rm -f $(READMEMD)

####

vignettes: $(VIGHTML)

$(VIGHTML): $(VIGRMD)
	@Rscript -e "devtools::build_vignettes()"

clean_vignettes:
	@Rscript -e "devtools::clean_vignettes()"

#####

example: vignettes
	cp ./$(DOCDIR)/user_manual.html ./SCHEME_PACKAGE/examples/basic/basic.html

clean_example:
	rm ./SCHEME_PACKAGE/examples/basic/basic.html

#####

pkgdown:
	@Rscript -e "pkgdown::build_site()"

clean_pkgdown:
	@Rscript -e "pkgdown::clean_site()"

####

web: readme pkgdown

clean_web: clean_readme clean_pkgdown

####

########### Package  ###########
####

docs:
	Rscript -e "devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))"
	Rscript -e "codemetar::write_codemeta()"

####

update:
	Rscript -e "devtools::load_all(here::here()); emeScheme:::updateFromNewSheet()"

####

updateForce:
	@Rscript -e "devtools::load_all(here::here()); emeScheme:::updateFromNewSheet(force = TRUE)"

###


build: docs
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

####

build-cran:
	cd ..;\
	R CMD build $(PKGSRC)

####

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

####

check: build-cran
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

clean_check:
	$(RM) -r ./../$(PKGNAME).Rcheck/

####

check-rhub-bkg: build-cran
	@Rscript -e "x <- rhub::check_for_cran(email = 'Rainer@krugs.de', path = './../$(PKGNAME)_$(PKGVERS).tar.gz')"

check-rhub: build-cran
	@Rscript -e "x <- rhub::check_for_cran(email = 'Rainer@krugs.de', path = './../$(PKGNAME)_$(PKGVERS).tar.gz', show_status = TRUE)"

####

test:
	@Rscript -e "devtools::test()"

####

############# Help targets #############

list_variables:
	@echo
	@echo "#############################################"
	@echo "## Variables ################################"
	make -pn | grep -A1 "^# makefile"| grep -v "^#\|^--" | sort | uniq
	@echo "#############################################"
	@echo ""

## from https://stackoverflow.com/a/26339924/632423
list_targets:
	@echo
	@echo "#############################################"
	@echo "## Targets    ###############################"
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$'
	@echo "#############################################"
	@echo

list: list_variables list_targets

#############

.PHONY: list files update clean clean_vignettes clean_web clean_html publish docs scheme_package
