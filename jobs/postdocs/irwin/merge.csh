#! /bin/tcsh 

set mysys=`uname -a`
if ( `echo $mysys | grep -c Darwin` == "1" ) then
 set pdfopen = open
endif
if ( `echo $mysys | grep -c Linux` == "1" ) then
 set pdfopen = acroread
endif

gs -dBATCH -sDEVICE=pdfwrite -dNOPAUSE -sOUTPUTFILE=resume.pdf cv.pdf publications.pdf

exit 0
