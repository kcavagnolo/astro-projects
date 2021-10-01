#!/bin/tcsh -f

# Encapsulates postscript files en masse

# $Id: encap.csh,v 1.1 2009-02-23 22:51:08 cavagnolo Exp $

set scriptname = $0:t
set version = `echo '$Revision: 1.1 $' | cut -d' ' -f2`

echo "\n---- ${scriptname:au} version ${version} ----\n"

if($#argv == 0) then
cat <<EOF
${scriptname}: Specify all the postscript files you wish to encapsulate

EOF
exit 1
endif

foreach file ($argv[*])
    if( $file:e != ps && $file:e != cps ) then
	echo $file does not seem to be a Postscript file
	continue
    endif
    echo Encapsulating $file
    (/star/bin/psmerge -e -r-90 ${file} >! ${file:r}.eps) && echo Done
end

# $Log: encap.csh,v $
# Revision 1.1  2009-02-23 22:51:08  cavagnolo
# *** empty log message ***
#
# Revision 1.4  2003/07/15 13:52:22  dbh
# No longer nices itself to 19.
#
# Revision 1.3  2002/09/11 12:22:35  dbh
# Incorporated RCS keywords Id and Log. Minor cosmetic changes.
#
