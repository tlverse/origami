TITLE = paper
.PHONY : pdf

pdf:
	Rscript -e "knitr::knit('$(TITLE).Rmd')"
	pandoc $(TITLE).md -o $(TITLE).pdf --bibliography $(TITLE).bib
