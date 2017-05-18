main.pdf : *.lhs *.bib Makefile
	lhs2TeX main.lhs -o main.tex
	latexmk -pdf main

clean :
	rm -f main.pdf
	latexmk -pdf -C main
	rm -f main.tex
