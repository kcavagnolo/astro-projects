ARCH := $(shell uname)

#FILE = cover
#FILE = cv
#FILE = publications
#FILE = summary
#FILE = summary_nobib
#FILE = proposal
FILE = proposal_nobib

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
	rm -f ${FILE}.aux ${FILE}.log \
	${FILE}.blg ${FILE}.brf ${FILE}.cb ${FILE}.ind \
	${FILE}.idx ${FILE}.ilg ${FILE}.inx ${FILE}.ps \
	${FILE}.lot ${FILE}.lof \
	${FILE}.dvi ${FILE}.pdf ${FILE}.toc ${FILE}.out

${FILE}.dvi:	${FILE}.tex
	latex ${FILE}.tex

ifeq ($(strip ${FILE}),statement)
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
endif

ifeq ($(strip ${FILE}),summary)
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
endif

ifeq ($(strip ${FILE}),proposal)
	bibtex ${FILE}
	latex ${FILE}.tex
	latex ${FILE}.tex
endif

${FILE}.ps:	${FILE}.dvi
	dvips -t letter ${FILE}.dvi -o ${FILE}.ps

${FILE}.pdf:	${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf
