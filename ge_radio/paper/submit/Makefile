#FILE = ms
FILE = arxiv
ARCH := $(shell uname)

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
	${FILE}.dvi ${FILE}.pdf ${FILE}.toc ${FILE}.out

${FILE}.dvi:	${FILE}.tex
	latex ${FILE}.tex
	bibtex ${FILE}
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
	latex ${FILE}.tex

${FILE}.ps:	${FILE}.dvi
	dvips -t letter ${FILE}.dvi -o ${FILE}.ps

${FILE}.pdf:	${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf
