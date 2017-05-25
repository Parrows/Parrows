BUILD_FOLDER=build

MAIN_FILE=main.tex

main.pdf : *.tex *.bib Makefile
	rm -r ${BUILD_FOLDER} || true
	mkdir ${BUILD_FOLDER}
	cp -r $(filter-out build, $(wildcard *)) ${BUILD_FOLDER}
	for texFile in $(wildcard *.tex) ; do \
		buildFile=${BUILD_FOLDER}/$$texFile; \
		lhs2TeX $${texFile} -o $${buildFile}.0 --poly -i polycode.fmt ; \
		if [ "$${buildFile}" != "${BUILD_FOLDER}/prelude.tex" ]; then \
			echo "removing lhs2TeX preamble for $${buildFile}"; \
			perl -0777 -pe "s/(?s).*\\EndFmtInput//" $${buildFile}.0 > $${buildFile}; \
		else \
			echo "keeping lhs2TeX preamble for $${buildFile}"; \
			cp $${buildFile}.0 $${buildFile}; \
		fi \
	done
	cd ${BUILD_FOLDER} && latexmk -pdflatex="pdflatex -file-line-error -synctex=1" -pdf main
	cp `ls ${BUILD_FOLDER}/main.* | grep -v 'main.tex'` .

clean :
	rm -r ${BUILD_FOLDER}
