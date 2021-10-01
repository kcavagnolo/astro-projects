FILE = chan_prop
#FILE = abstract

view:	${FILE}.pdf
	acroread ${FILE}.pdf

clean:
	rm -f ${FILE}.aux ${FILE}.log \
	${FILE}.blg ${FILE}.brf ${FILE}.cb ${FILE}.ind \
	${FILE}.idx ${FILE}.ilg ${FILE}.inx ${FILE}.ps \
	${FILE}.dvi ${FILE}.pdf ${FILE}.toc ${FILE}.out

${FILE}.dvi:	${FILE}.tex
#	latex ${FILE}.tex
#	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex

${FILE}.ps:	${FILE}.dvi
#	dvips -t letter ${FILE}.dvi -o ${FILE}.ps
	dvips -Ppdf -G0 -o ${FILE}.ps ${FILE}.dvi

${FILE}.pdf:	${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf
