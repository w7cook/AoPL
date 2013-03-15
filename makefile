.SUFFIXES: .lhs .mkd .htm .tex .pdf

.PHONY: verb pretty update clean

PANDOC := pandoc --no-wrap -sS --bibliography=anatomy.bib 
HSCOLOUR := hscolour -lit
NEWLINE!=cat foo.txt

verb: anatomyVerbatim.pdf
	open anatomyVerbatim.pdf

pretty: anatomy.pdf
	open anatomy.pdf

update: anatomy.pdf anatomyVerbatim.pdf
	cp anatomyVerbatim.pdf ~/Public/web/anatomy/anatomyVerbatim.pdf 
	cp anatomy.pdf ~/Public/web/anatomy/anatomy.pdf 
	scp anatomyVerbatim.pdf envy.cs.utexas.edu:public_html/anatomy/anatomyVerbatim.pdf
	scp anatomy.pdf envy.cs.utexas.edu:public_html/anatomy/anatomy.pdf
	
anatomy.mkd: anatomy.lhs makefile template.tex anatomy.bib
	cat anatomy.lhs \
	 | sed "s/^ #/#/" \
	 | sed "s/'[0-9][0-9a-z]*//g" \
	 | sed "s/^> test[^=]* =/>/g" \
	 | sed '/--BEGIN-HIDE--/,/--END-HIDE--/d' \
	 > anatomy.mkd

anatomy.htm: anatomy.mkd
	cat anatomy.mkd \
   | sed "s/||/VERTICAL_BAR/g" \
   | sed "/|/s/\\\\//g" \
   | perl -pe 's/\|(.*?)\|/\`$$1\`/g;' \
   | sed "s/VERTICAL_BAR/||/g" \
	 | $(PANDOC) --toc -f markdown+lhs -t html -c hscolour.css --chapters \
	 | sed "s/\\.eps/.png/" \
	 | sed "s/<p>/~<p>/g" | sed "s/<pre/~<pre/g" | tr "~" "\n" \
	 > anatomy.htm

temp.lhs: anatomy.mkd template.tex
	cat anatomy.mkd \
    | sed "/> -- %[a-zA-Z0-9][a-zA-Z0-9]*/d" \
    | sed "s/%[a-zA-Z0-9][a-zA-Z0-9]*//g" \
		| $(PANDOC) -f markdown+lhs -t latex+lhs --template=template.tex --chapters \
		| sed "s/{verbatim}/{spec}/g" \
		| sed "s/@/@@/g" \
		| sed "s/\\\\textbar{}/|/g" \
		| sed "s/\\\\textless{}/</g" \
		| sed "s/\\\\textgreater{}/>/g" \
		| sed "s/\\\\ldots{}/.../g" \
		| sed "s/\\\\textbackslash{}/\\\\/g" \
    | sed "/|/s/\\\\_/_/g" \
		| sed "s/{\[}/[/g" \
		| sed "s/{\]}/]/g" \
		| sed "s/BAR/||/g" \
		> temp.lhs
		 
anatomyVerbatim.pdf: temp.lhs
	lhs2TeX --tt temp.lhs \
	| sed "s/\\\\char'31/\\\\char45{}\\\\char62{}/g" \
	| sed "s/\\\\char'10/\\\\char92{}/g" \
	| sed "s/\\\\char'06/\\\\char60{}\\\\char45{}/g" \
	| sed "s/\\\\char'36/==/g" \
	| sed "s/\\\\char'00/./g" \
	| sed "s/\\\\char'05/not/g" \
	| sed "s/\\\\char'04/and/g" \
	| sed "s/\\\\char'37/or/g" \
	| sed "s/\\\\char'24/forall/g" \
	> anatomyVerbatim.tex
	pdflatex anatomyVerbatim.tex && pdflatex anatomyVerbatim.tex && pdflatex anatomyVerbatim.tex

anatomyCheck.tex: temp.lhs
	lhs2TeX --verb temp.lhs > anatomyCheck.tex
	
anatomy.pdf: temp.lhs
	lhs2TeX --poly temp.lhs > anatomy.tex
	pdflatex anatomy.tex && pdflatex anatomy.tex && pdflatex anatomy.tex

clean:
	rm -rf abstract_syntax-eps-converted-to.pdf anatomy.mkd \
		anatomyVerbatim.aux anatomyVerbatim.log anatomyVerbatim.out \
		anatomyVerbatim.pdf anatomyVerbatim.tex anatomyVerbatim.toc \
		scopes-eps-converted-to.pdf temp.lhs anatomy.pdf anatomy.tex \
		anatomy.toc anatomy.aux anatomy.log anatomy.out anatomy.ptb \
		anatomyCheck.tex anatomy.htm
	
