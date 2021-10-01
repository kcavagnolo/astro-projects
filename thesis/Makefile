FILE = thesis

view:	${FILE}.pdf
	open ${FILE}.pdf
#	acroread ${FILE}.pdf

clean:
	rm -f ${FILE}.aux ${FILE}.log ${FILE}.bbl \
	${FILE}.blg ${FILE}.brf ${FILE}.cb ${FILE}.ind \
	${FILE}.idx ${FILE}.ilg ${FILE}.inx ${FILE}.ps \
	${FILE}.lot ${FILE}.lof ${FILE}.los \
	${FILE}.dvi ${FILE}.pdf ${FILE}.toc ${FILE}.out \
	*.aux

${FILE}.dvi:	${FILE}.tex
	latex ${FILE}.tex
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
	latex ${FILE}.tex
	latex ${FILE}.tex

${FILE}.ps:	${FILE}.dvi
	dvips -f -tletter -Ppdf -G0 ${FILE}.dvi -o ${FILE}.ps

${FILE}.pdf:	${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf
