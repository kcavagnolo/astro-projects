#FILE = r797
FILE = minihalo

# linux machine
ARCH := $(shell uname)
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
	rm -f ${FILE}.aux ${FILE}.log \
	${FILE}.blg ${FILE}.brf ${FILE}.cb ${FILE}.ind \
	${FILE}.idx ${FILE}.ilg ${FILE}.inx ${FILE}.ps \
	${FILE}.dvi ${FILE}.toc ${FILE}.out

${FILE}.dvi:	${FILE}.tex
	latex ${FILE}.tex
	latex ${FILE}.tex
	latex ${FILE}.tex
#	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex

${FILE}.ps:	${FILE}.dvi
	dvips -t letter -Ppdf -G0 -o ${FILE}.ps ${FILE}.dvi

${FILE}.pdf:	${FILE}.ps
	ps2pdf14 ${FILE}.ps ${FILE}.pdf
