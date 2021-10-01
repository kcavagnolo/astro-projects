#! /bin/tcsh

set outfile = cavagnolo_grainger_application.pdf

set mysys=`uname -a`
if ( `echo $mysys | grep -c Darwin` == "1" ) then
 set pdfopen = open
endif
if ( `echo $mysys | grep -c Linux` == "1" ) then
 set pdfopen = acroread
endif

gs -dBATCH -sDEVICE=pdfwrite -dNOPAUSE -sOUTPUTFILE=$outfile cover.pdf cv.pdf publications.pdf proposal_nobib.pdf

$pdfopen $outfile

exit 0
