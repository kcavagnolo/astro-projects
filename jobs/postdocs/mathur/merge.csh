#! /bin/tcsh

set mysys=`uname -a`
if ( `echo $mysys | grep -c Darwin` == "1" ) then
 set pdfopen = open
endif
if ( `echo $mysys | grep -c Linux` == "1" ) then
 set pdfopen = acroread
endif

gs -dBATCH -sDEVICE=pdfwrite -dNOPAUSE -sOUTPUTFILE=cavagnolo_application.pdf cover.pdf cv.pdf publications.pdf summary.pdf

$pdfopen cavagnolo_application.pdf

exit 0
