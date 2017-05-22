main.pdf : *.tex *.bib Makefile
	lhs2TeX main.tex -o output.tex
	latexmk -pdflatex="pdflatex -file-line-error -synctex=1" -pdf output

clean :
	rm -f output.pdf
	latexmk -pdf -C output
	rm -f output.tex
