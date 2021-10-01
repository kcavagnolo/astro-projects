rm -f response2.aux  response2.log  response2.pdf  response2.dvi   response2.out  response2.ps
latex response2.tex
latex response2.tex
latex response2.tex
dvips -t letter response2.dvi -o response2.ps
ps2pdf response2.ps response2.pdf
acroread response2.pdf
#open response2.pdf
