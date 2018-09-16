TEXFILE := dissertation
SECTIONS := $(wildcard results/tex/*.tex)
TABLES := $(wildcard results/tables/*.tex)

.PHONY: build clean

build: results/$(TEXFILE).pdf 

results/$(TEXFILE).pdf: $(TEXFILE).tex $(SECTIONS) $(TABLES) references.bib
	@echo "Atualizando $*..."
	latexmk -pdf $(TEXFILE).tex
	@make clean

clean:
	@rm -f *.log
	@rm -f *.aux
	@rm -f *.bbl
	@rm -f *.blg
	@rm -f *.fls
	@rm -f *.out
	@rm -f *.toc
	@rm $(TEXFILE).fdb_latexmk
