.SUFFIXES: .lhs .mkd .htm .tex .pdf .hs

.PHONY: verb pretty update clean

PANDOC := pandoc --no-wrap -sS --bibliography=anatomy.bib
HSCOLOUR := hscolour -lit
NEWLINE!=cat foo.txt

TESTS=SimpleTest.hs \
	SubstituteTest.hs \
	IntBoolTest.hs \
	TopLevelFunctionsTest.hs \
	StatefulTest.hs

SOURCES=$(TESTS) \
	Base.hs \
	Lexer.hs \
	CheckedMonad.hs \
	ErrorChecking.hs \
	Examples.hs \
	FirstClassFunctions.hs \
	FunExamples.hs \
	FunctionalEnvironment.hs \
	IncorrectFunctions.hs \
	IntBool.hs \
	Let.hs \
	LetSubstitute.hs \
	Stateful.hs \
	StatefulMonad.hs \
	Simple.hs \
	SimpleParse.hs \
	Substitute.hs \
	SubstituteParse.hs \
	TopLevelFunctions.hs

verb: anatomyVerbatim.pdf
	open anatomyVerbatim.pdf

pretty: anatomy.pdf
	open anatomy.pdf

updates: new.lhs

diff: new.lhs
	diff anatomy.lhs new.lhs

new.lhs: anatomy.lhs execute
	ruby tags.rb \
		| ruby includes.rb "src/*.hs" "src/*.y" "output/*.out" ">" \
		> sed -E 's/ +$//' \
		> new.lhs

fixup: anatomy.lhs new.lhs
	cp anatomy.lhs backup/archive`date "+%m%d%H%M%Y%S"`.lhs
	cp new.lhs anatomy.lhs

code/%.hs: src/%.hs makefile
	@mkdir -p code
	cat $< \
		| sed "/BEGIN:/d" \
		| sed "/END:/d" \
		> $@
	(echo '<pre>'; cat $@; echo '</pre>') \
		| perl -pe "s/import ([a-zA-Z]+) *\$$/import <a href=\$$1.hs.htm>\$$1<\\/a>/ if !/Prelude/" \
		| perl -pe "s/import ([a-zA-Z]+) /import <a href=\$$1.hs.htm>\$$1<\\/a> / if !/Prelude/" \
		> $@.htm

code: $(addprefix code/,$(SOURCES))

code/%.hs: code/%.hs makefile
	(echo '<pre>'; cat $@; echo '</pre>') \
		| perl -pe "s/import ([a-zA-Z]+) *\$$/import <a href=\$$1.hs.htm>\$$1<\\/a>/ if !/Prelude/" \
		| perl -pe "s/import ([a-zA-Z]+) /import <a href=\$$1.hs.htm>\$$1<\\/a> / if !/Prelude/" \
		> $@.htm

output/%.out: code/%.hs makefile
	@mkdir -p output
	cd code; runghc $(notdir $<) > ../output/$(addsuffix .out, $(basename $(notdir $<)))

execute: code $(addprefix output/, $(addsuffix .out, $(basename $(notdir $(TESTS)))))

update: anatomy.pdf anatomyVerbatim.pdf	anatomy.htm code
	cp anatomyVerbatim.pdf ~/Public/web/anatomy/anatomyVerbatim.pdf
	cp anatomy.pdf ~/Public/web/anatomy/anatomy.pdf
	cp anatomy.htm ~/Public/web/anatomy/anatomy.htm
	mkdir -p	~/Public/web/anatomy/figures
	mkdir -p	~/Public/web/anatomy/code
	cp figures/*.png ~/Public/web/anatomy/figures
	cp code/* ~/Public/web/anatomy/code
	cp -r cc ~/Public/web/anatomy
	scp -r ~/Public/web/anatomy envy.cs.utexas.edu:public_html

anatomy.mkd: anatomy.lhs makefile template.tex anatomy.bib figures/*.eps execute
	ruby includes.rb "src/*.hs" "src/*.y" "output/*.out" ">" < anatomy.lhs \
		| sed "/^INCLUDE:/d" \
		| sed "s/^ #/#/" \
		| sed "s/'[0-9][0-9a-z]*//g" \
		| sed "s/^> test[^= ][^= ]* =/>/g" \
		| sed '/--BEGIN-HIDE--/,/--END-HIDE--/d' \
		> anatomy.mkd

anatomy.htm: anatomy.mkd
	cat anatomy.mkd \
		| sed "s/||/VERTICAL_BAR/g" \
		| perl -pe 's/\|([^ ][^|]*)\|/\`$$1\`/g;' \
		| sed "s/VERTICAL_BAR/||/g" \
		> foo.mkd
	cat foo.mkd \
		| $(PANDOC) --mathjax --number-sections --toc -f markdown+lhs -t html --css cc/commentCloud.css --chapters \
		| sed "s/\\.eps/.png/" \
		> foo2.mkd
	cat foo2.mkd \
		| perl -pe "s/ %([a-zA-Z0-9][a-zA-Z0-9]*)/ <a href='' id='Comment:\$$1' ><\\/a>/g" \
		| perl -pe "s/^%([a-zA-Z0-9][a-zA-Z0-9]*)/<a href='' id='Comment:\$$1' ><\\/a>/g" \
		| sed "s|</head>|<script src="cc/parse.js"></script><script src="cc/commentCloud.js"></script></head>|" \
		| sed "s|<body>|<body onLoad=\"CommentSetup('g7Ukr5GXnqtS6jqM5gUkSwfY4eyWHxERMkrhurR0','qK1pZ7VXZv8eNtULnMcIzcLy2pIBgvIG9YyO9pu7','Anatomy')\">|" \
		> anatomy.htm
	mkdir -p cc
	cp ../CommentCloud/*.js cc
	cp ../CommentCloud/*.css cc

temp.lhs: anatomy.mkd template.tex
	cat anatomy.mkd \
		| sed "/> -- %[a-zA-Z0-9][a-zA-Z0-9]*/d" \
		| sed "/^%[a-zA-Z0-9][a-zA-Z0-9]*/d" \
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
		| sed "s/OPENB/{/g" \
		| sed "s/CLOSEB/}/g" \
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
		anatomyCheck.tex anatomy.htm \
		code output
