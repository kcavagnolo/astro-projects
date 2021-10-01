view:	archive5.pdf
#	gv archive5.ps
	open archive5.pdf
clean:
	rm -f archive5.aux archive5.log archive5.bbl\
	 archive5.blg archive5.brf archive5.cb archive5.ind\
	 archive5.idx archive5.ilg archive5.inx archive5.ps\
	 archive5.lot archive5.lof\
	 archive5.dvi archive5.pdf archive5.toc archive5.out
archive5.pdf:	archive5.ps
	ps2pdf14 archive5.ps archive5.pdf
archive5.ps:	archive5.dvi
	dvips -Ppdf -t letter -o archive5.ps archive5.dvi
archive5.dvi:	archive5.tex
	latex archive5.tex
