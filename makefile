.SUFFIXES: .lhs .mkd .htm .tex .pdf

PANDOC := pandoc --no-wrap -sS
HSCOLOUR := hscolour -lit

main: notes.pdf 
	open notes.pdf

update: notes.pdf
	scp notes.pdf envy.cs.utexas.edu:public_html/Courses/345/pl.pdf
	
.lhs.mkd:
	cat $< | sed "s/^ #/#/" | sed "s/'[0-9][0-9]*//g" | sed '/--BEGIN-HIDE--/,/--END-HIDE--/d' > $@

.mkd.htm:
	cat $< | $(PANDOC) --toc -f markdown+lhs -t html -c hscolour.css | sed "s/.eps/.png/" > $@

.mkd.tex:
	cat $< | $(PANDOC) --toc -f markdown+lhs -t latex> $@
  # cat $< | $(PANDOC) --to=latex+lhs --template=template.tex  > temp.lhs
  # lhs2TeX temp.lhs > $@

.tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<
