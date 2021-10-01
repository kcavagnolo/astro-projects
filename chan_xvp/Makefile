FILE = herca

ARCH := $(shell uname)
ifeq (${ARCH}, Linux)
OPROG = acroread
endif
ifeq (${ARCH}, Darwin)
OPROG = open
endif

view:	${FILE}.pdf
	${OPROG} ${FILE}.pdf

clean:
	rm -f ${FILE}.aux ${FILE}.blg ${FILE}.dvi ${FILE}.log ${FILE}.bbl ${FILE}.out

${FILE}.pdf:	${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf

${FILE}.ps:	${FILE}.dvi
	dvips -t letter ${FILE}.dvi -o ${FILE}.ps

${FILE}.dvi:	${FILE}.tex
	latex ${FILE}.tex
