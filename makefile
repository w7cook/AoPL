.SUFFIXES: .lhs .mkd .htm .tex .pdf

PANDOC := pandoc --no-wrap -sS
HSCOLOUR := hscolour -lit

.lhs.mkd:
	cat $< | sed "s/^ #/#/"  > $@

.lhs.htm:
	cat $< | sed "s/^ #/#/" | $(PANDOC) -f markdown+lhs -t html -c hscolour.css | sed "s/.eps/.png/" > $@

.lhs.tex:
	cat $< | sed "s/^ #/#/" | $(PANDOC) -f markdown+lhs -t latex> $@

.tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<
