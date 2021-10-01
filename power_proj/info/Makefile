view	: procedure.pdf
	gv procedure.pdf
procedure.pdf:	procedure.ps
	ps2pdf14 procedure.ps procedure.pdf
procedure.ps:	procedure.dvi
	dvips -Ppdf -G0 -o procedure.ps procedure.dvi
procedure.dvi:	procedure.tex
	latex procedure.tex
