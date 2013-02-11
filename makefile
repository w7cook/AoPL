.SUFFIXES: .lhs .mkd .htm .tex .pdf

PANDOC := pandoc --no-wrap -sS
HSCOLOUR := hscolour -lit

main: anatomy.pdf
	open anatomy.pdf

update: anatomy.pdf anatomyVerbatim.pdf
	cp anatomyVerbatim.pdf ~/Public/web/Courses/345/anatomyVerbatim.pdf 
	cp anatomy.pdf ~/Public/web/Courses/345/Anotomy.pdf 
	scp anatomyVerbatim.pdf envy.cs.utexas.edu:public_html/Courses/345/anatomyVerbatim.pdf
	scp anatomy.pdf envy.cs.utexas.edu:public_html/Courses/345/anotomy.pdf
	
anatomy.mkd: anatomy.lhs makefile
	cat anatomy.lhs \
	 | sed "s/^ #/#/" \
	 | sed "s/'[0-9][0-9a-z]*//g" \
	 | sed '/--BEGIN-HIDE--/,/--END-HIDE--/d' \
	 > anatomy.mkd

anatomy.htm: anatomy.mkd
	cat anatomy.mkd \
	 | $(PANDOC) --toc -f markdown+lhs -t html -c hscolour.css --chapters \
	 | sed "s/.eps/.png/" \
	 > anatomy.htm

temp.lhs: anatomy.mkd template.tex
	cat anatomy.mkd \
		| $(PANDOC) -f markdown+lhs -t latex+lhs --template=template.tex --chapters \
		| sed "s/{verbatim}/{spec}/g" \
		| sed "s/@/@@/g" \
		| sed "s/\\\\\textbar{}/|/g" \
		| sed "s/\\\\\textless{}/</g" \
		| sed "s/\\\\\textgreater{}/>/g" \
    | sed "s/\\\\\ldots{}/.../g" \
    | sed "s/\\\\\\textbackslash{}/\\\\/g" \
		| sed "s/{\[}/[/g" \
		| sed "s/{\]}/]/g" \
		| sed "s/BAR/||/g" \
		> temp.lhs
		 
anatomyVerbatim.pdf: temp.lhs
	lhs2TeX --tt temp.lhs \
	| sed "s/\\\\char'31/\\\\char45{}\\\\char62{}/g" \
	| sed "s/\\\\char'10/\\\\char92{}/g" \
	| sed "s/\\\\\char'06/\\\\char60{}\\\\char45{}/g" \
	| sed "s/\\\\\char'36/==/g" \
	| sed "s/\\\\\char'00/./g" \
	| sed "s/\\\\\char'05/not/g" \
	| sed "s/\\\\\char'04/and/g" \
	| sed "s/\\\\\char'37/or/g" \
	| sed "s/\\\\\char'24/forall/g" \
	> anatomyVerbatim.tex
	pdflatex anatomyVerbatim.tex && pdflatex anatomyVerbatim.tex && pdflatex anatomyVerbatim.tex

anatomyCheck.tex: temp.lhs
	lhs2TeX --verb temp.lhs > anatomyCheck.tex
	
anatomy.pdf: temp.lhs
	lhs2TeX --poly temp.lhs > anatomy.tex
	pdflatex anatomy.tex && pdflatex anatomy.tex && pdflatex anatomy.tex
	