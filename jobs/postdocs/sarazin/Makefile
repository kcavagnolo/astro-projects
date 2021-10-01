#FILE = cover
FILE = cv
#FILE = publications
#FILE = summary
#FILE = brief

view:	${FILE}.pdf
	open ${FILE}.pdf

clean:
	rm -f ${FILE}.aux ${FILE}.log ${FILE}.bbl \
	${FILE}.blg ${FILE}.brf ${FILE}.cb \
	${FILE}.idx ${FILE}.ilg ${FILE}.inx \
	${FILE}.lot ${FILE}.lof ${FILE}.ind \
	${FILE}.dvi ${FILE}.toc ${FILE}.out

cleanall:
	rm -f *.aux *.log *.bbl \
	*.blg *.brf *.cb *.ind \
	*.idx *.ilg *.inx \
	*.dvi *.toc *.out \
	*.lot *.lof

${FILE}.dvi:	${FILE}.tex
	latex ${FILE}.tex
	latex ${FILE}.tex

ifeq ($(strip ${FILE}),proposal)
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
endif

${FILE}.ps:	${FILE}.dvi
	dvips -t letter ${FILE}.dvi -o ${FILE}.ps

${FILE}.pdf:	${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf
