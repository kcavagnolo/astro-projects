ARCH := $(shell uname)

#FILE = cover
#FILE = cv
#FILE = passport
#FILE = publications
#FILE = summary
FILE = statement

# linux machine
ifeq (${ARCH}, Linux)
OPROG = acroread
endif

# Mac build
ifeq (${ARCH}, Darwin)
OPROG = open
endif

view:	${FILE}.pdf
	${OPROG} ${FILE}.pdf

clean:
	rm -f ${FILE}.aux ${FILE}.log ${FILE}.bbl \
	${FILE}.blg ${FILE}.brf ${FILE}.cb ${FILE}.ind \
	${FILE}.idx ${FILE}.ilg ${FILE}.inx ${FILE}.ps \
	${FILE}.lot ${FILE}.lof \
	${FILE}.dvi ${FILE}.pdf ${FILE}.toc ${FILE}.out

cleanall:
	rm -f *.aux *.log *.bbl \
	*.blg *.brf *.cb *.ind \
	*.idx *.ilg *.inx \
	*.dvi *.toc *.out \
	*.lot *.lof

${FILE}.dvi:	${FILE}.tex
	latex ${FILE}.tex

ifeq ($(strip ${FILE}),summary)
	latex ${FILE}.tex
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
endif

ifeq ($(strip ${FILE}),statement)
	latex ${FILE}.tex
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
endif

${FILE}.ps:	${FILE}.dvi
	dvips -t letter ${FILE}.dvi -o ${FILE}.ps

${FILE}.pdf:	${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf
