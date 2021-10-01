rm -f response.aux  response.log  response.pdf  response.dvi   response.out  response.ps
latex response.tex
dvips -t letter response.dvi -o response.ps
ps2pdf response.ps response.pdf
acroread response.pdf
