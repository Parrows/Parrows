BACKUP_FOLDER=backup

main.pdf : *.tex *.bib Makefile
	mkdir ${BACKUP_FOLDER} || true
	cp -r $(filter-out backup build, $(wildcard *.tex)) ${BACKUP_FOLDER}
	
	for texFile in $(wildcard *.tex) ; do \
		buildFile=$$texFile; \
        if ( [ "$${buildFile}" != "lhs2TeXSafeCommands.tex" ] && [ "$${buildFile}" != "bestAndWorstBenchmarks.tex" ] ); then \
            lhs2TeX $${texFile} -o $${buildFile}.0 --poly -i custom.fmt ; \
            if [ "$${buildFile}" != "prelude.tex" ]; then \
                echo "removing lhs2TeX preamble for $${buildFile}"; \
                perl -0777 -pe "s/(?s).*\\EndFmtInput//" $${buildFile}.0 > $${buildFile}; \
            else \
                echo "keeping lhs2TeX preamble for $${buildFile}"; \
                cp $${buildFile}.0 $${buildFile}; \
            fi \
        fi \
	done
	latexmk -pdflatex="pdflatex -file-line-error -synctex=1" -pdf main || true
	cd ${BACKUP_FOLDER} && cp -r `ls . ` ..

clean :
	latexmk -C
	cd ${BACKUP_FOLDER} && cp -r `ls . ` ..
	rm -r ${BACKUP_FOLDER} || true
	rm -r *.0 || true
