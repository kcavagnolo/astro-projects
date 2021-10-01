#! /bin/tcsh

foreach inst ("m1" "m2" "pns")
    if ($inst == 'pns') then 
	set ins = 'PN'
	set bsiz = 256
    else if ($inst == 'm1') then
	set ins = 'M1'
	set bsiz = 200
    else 
	set ins = 'M2'
	set bsiz = 200
    endif

    cd $inst
    rbnpha infile=reb.pha outfile=\!rebb${bsiz}.pha fchan=0 \
    finchan=${bsiz} cmpmode=linear

    rbnrmf infile=../../annspec/resp/${ins}.rsp fchan=0 nchan=${bsiz} \
    cmpmode=linear outfile=\!../../annspec/resp/${ins}b${bsiz}.rsp 

    fparkey ../../annspec/resp/${ins}b${bsiz}.rsp rebb${bsiz}.pha RESPFILE

    cd ..

end    
