view	: chan_prop.pdf
	gv chan_prop.pdf
chan_prop.pdf:	chan_prop.ps
	ps2pdf14 chan_prop.ps chan_prop.pdf
chan_prop.ps:	chan_prop.dvi
	dvips -Ppdf -G0 -o chan_prop.ps chan_prop.dvi
chan_prop.dvi:	chan_prop.tex
	latex chan_prop.tex
	bibtex chan_prop
	latex chan_prop.tex
	latex chan_prop.tex
