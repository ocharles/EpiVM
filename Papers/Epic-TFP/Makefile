PAPER = epic

all: ${PAPER}.pdf

TEXFILES = ${PAPER}.tex intro.tex language.tex example.tex \
           implementation.tex performance.tex conclusions.tex bigexample.tex

DIAGS = 

SOURCES = ${TEXFILES} ${DIAGS} macros.ltx comments.sty library.ltx llncs.cls literature.bib

DITAA = java -jar ~/Downloads/ditaa.jar

${PAPER}.pdf: ${SOURCES}
#	dvipdf ${PAPER}.dvi
	pdflatex ${PAPER}
	-bibtex ${PAPER}
	-pdflatex ${PAPER}
	-pdflatex ${PAPER}

${PAPER}.ps: ${PAPER}.dvi
	dvips -o ${PAPER}.ps ${PAPER}

${PAPER}.dvi: $(SOURCES)
	-latex ${PAPER}
	-bibtex ${PAPER}
	-latex ${PAPER}
	-latex ${PAPER}

progress: .PHONY
	wc -w ${TEXFILES}

%.png : %.diag
	$(DITAA) -o -E $<

distrib: all
	cp ${PAPER}.pdf ${SOURCES} tfp9 
.PHONY:
