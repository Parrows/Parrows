BUILD_FOLDER=build
SOURCE_FOLDER=sources

MAIN_FILE=main.tex

main.pdf : $(SOURCE_FOLDER)/*.tex $(SOURCE_FOLDER)/*.bib Makefile
	cp -r ${SOURCE_FOLDER} ${BUILD_FOLDER}
	for texFile in $(wildcard $(SOURCE_FOLDER)/*.tex) ; do \
		buildFile=`echo $$texFile | sed -r 's/${SOURCE_FOLDER}/${BUILD_FOLDER}/g'`; \
		lhs2TeX $${texFile} -o $${buildFile}.0 --poly -i polycode.fmt ; \
		if [ "$${buildFile}" != "${BUILD_FOLDER}/prelude.tex" ]; then \
			echo "removing lhs2TeX preamble for $${buildFile}"; \
			perl -0777 -pe "s/(?s).*\\EndFmtInput//" $${buildFile}.0 > $${buildFile}; \
		else \
			echo "keeping lhs2TeX preamble for $${buildFile}"; \
			cp $${buildFile}.0 $${buildFile}; \
		fi \
	done
	cd build && latexmk -pdflatex="pdflatex -file-line-error -synctex=1" -pdf main

clean :
	rm -r ${BUILD_FOLDER}
