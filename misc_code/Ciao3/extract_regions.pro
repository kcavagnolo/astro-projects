pro extract_regions,eventfile,regionfile,srcstring=srcstring,chip=chip,    $
                    exptime=exptime,make_arfs=make_arfs,grating=grating,   $
		    aspfile=aspfile,rmffile=rmffile,exproot=exproot,       $
                    arffile=arffile,backfile=backfile,deproject=deproject, $
                    chantype=chantype,cmdroot=cmdroot,logfile=logfile,     $
                    grpcnts=grpcnts
;-----------------------------------------------------------------------
; Name: EXTRACT_REGIONS
;
; Purpose: Reads in a list of regions and creates PHA and ARF files
;          for that region.
;          
; Inputs: 
;         
; Comments: 
;           
; Revision history:
;       written by Michael Wise		10-24-99
;	updated to include ARF code	11-16-99
;	changed pointer to EXPMAP	11-19-99
;       adjusted calling sequence       02-11-00
;	of MKARF to reflect HETG
;       added BACKFILE,RESPFILE,
;       and ARFFILE keywords
;       added deproject keywords	02-12-00
;       added GRPPHA call
;       made EXPTIME optional
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 5)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'extract_regions,eventfile,regionfile,srcstring=srcstring,chip=chip,',  $
   '                exptime=exptime,make_arfs=make_arfs,grating=grating,', $
   '                aspfile=aspfile,rmffile=rmffile,exproot=exproot,',     $
   '                arffile=arffile,backfile=backfile,deproject=deproject,', $
   '                chantype=chantype,cmdroot=cmdroot,logfile=logfile,',   $
   '                grpcnts=grpcnts'
   return   
endif


;
; Define location of ASCDS tools and EXPMAP tools
;
if (n_elements(cmdroot) eq 0) then cmdroot='source /iraf/ciao-2.2/bin/ciao.csh ; '
if (n_elements(exproot) eq 0) then exproot='source /iraf/ciao-2.2/bin/ciao.csh ; '
cmdrootl='source $LHEASOFT/lhea-init.csh ; '

;
; Define label for created files
;
if (n_elements(srcstring) eq 0) then srcstring='src'


;
; Define default CCD
;
if (n_elements(chip) eq 0) then chip=7


;
; Default value of ARFFILE keyword
;
if (n_elements(arffile) eq 0) then arffile='NONE'


;
; Default value of BACKFILE keyword
;
if (n_elements(backfile) eq 0) then backfile='NONE'


;
; Default extraction type (PI)
;
if (n_elements(chantype) eq 0) then begin
   chantype='PI'
endif else begin
   if ((chantype ne 'PI') and (chantype ne 'pi') and   $
       (chantype ne 'PHA') and (chantype ne 'pha')) then begin
       print,string(7B),'ERROR: Must select PI or PHA channel type'
       return   
   endif   
   chantype=strupcase(chantype)
endelse


;
; Set minimum grouping if not specified
;
if keyword_set(grpcnts) then begin
   if (grpcnts le 1.0) then grpcnts=30
   grpstring=strtrim(string(grpcnts),2)
endif


;
; Define name of log file
;
if (n_elements(logfile) eq 0) then logfile='extract.log'


;
; Require an aspect file if MAKE_ARFS flag is set
;
if keyword_set(make_arfs) then begin
   if (n_elements(aspfile) eq 0) then begin
       print,string(7B),'ERROR: You must specify an aspect file to make ARFs'
       return   
   endif
endif else begin
  if (n_elements(aspfile) eq 0) then aspfile='NONE'
endelse


;
; Set default RMF file if not specified
;
if (n_elements(rmffile) eq 0) then $
   rmffile='/nfs/apocrypha/d2/wise/data/caldata/ccd7_aim_rmf.fits'



;
; Parse the regions file (currently hard-wired for annuli)
;
readcol,regionfile,id,type,xc,yc,r1,r2,area,f='i,a,f,f,f,f,f',/silent


;
; Open logfile 
;
get_lun,unit
openw,unit,logfile
printf,unit,' '
printf,unit,'Date                : '+!stime
printf,unit,'Eventfile           : '+eventfile
printf,unit,'Regions file        : '+regionfile
printf,unit,'Source              : '+srcstring
printf,unit,'Aspect Histogram    : '+aspfile
printf,unit,'Chip ID             : ',chip


;
; Define detector sub-system string
;
ccd=strtrim(string(fix(chip)),2)        ; chip designation
if (chip lt 4) then begin
   detsubsys='ACIS-I'+ccd
endif else begin
   detsubsys='ACIS-S'+strtrim(string(fix(chip-4)),2)
endelse
printf,unit,'Detector Sub-system : '+detsubsys
printf,unit,' '
printf,unit,' '


;
; Define filenames
;
num=n_elements(id)
ndigits=fix(alog10(float(num))+1.0)
spectfiles=strarr(num)
groupfiles=strarr(num)
backfiles=strarr(num)
arffiles=strarr(num)
for i=0,num-1 do begin
    root='0000000'+strtrim(string(id(i)),2)
    root='reg'+strmid(root,strlen(root)-ndigits,ndigits)
    spectfiles(i)=srcstring+'_'+root+'.'+strlowcase(chantype)
    groupfiles(i)=srcstring+'_'+root+'_g'+grpstring+ $
                  '.'+strlowcase(chantype)
    if keyword_set(make_arfs) then arffiles(i)=srcstring+'_'+root+'.arf'  $
                              else arffiles(i)=arffile
endfor
for i=0,num-2 do begin
    if keyword_set(deproject) then backfiles(i)=spectfiles(i+1)  $
                              else backfiles(i)=backfile
endfor
backfiles(num-1)=backfile


;
; Loop over regions
;
result=' '
space='     '
for i=0,num-1 do begin

;
;   Note in logfile
;
    printf,unit,'Region          : ',id(i)
    printf,unit,' '
    printf,unit,'Spectrum file   : '+spectfiles(i)
    printf,unit,'Background file : '+backfiles(i)   
    printf,unit,'RMF file        : '+rmffile
    printf,unit,'ARF file        : '+arffiles(i)
    printf,unit,' '

;
;   Extract PHA/PI file
;

    filter='[EVENTS][(x,y)='+type(i)+'('+strtrim(string(xc(i)),2)+','+  $
           strtrim(string(yc(i)),2)+','+strtrim(string(r1(i)),2)+','+   $
           strtrim(string(r2(i)),2)+')]'
   
    cmdstring='dmextract "'+eventfile+filter+                           $
              '[bin '+strlowcase(chantype)+']" '+spectfiles(i)

    spawn,cmdroot+cmdstring,result

    printf,unit,space+cmdstring
    printf,unit,space+result
    printf,unit,' '


;
;   Compute ARF file if indicated
;
    if keyword_set(make_arfs) then begin

       xp=xc(i)+0.5*(r1(i)+r2(i))
       yp=yc(i)

       printf,unit,'Calculating ARF for Region' +root
       printf,unit,' '
       printf,unit,'ARF file         : '+arffiles(i)
       printf,unit,' '
       printf,unit,'X Pixel Pointing for ARF: ',xp
       printf,unit,'Y Pixel Pointing for ARF: ',yp
       printf,unit,' '

       parms='outfile="'+arffiles(i)+'" '+'asphistfile="'+aspfile+'" '+ $
             'sourcepixelx='+strtrim(string(xp),2)+' '+                 $
	     'sourcepixely='+strtrim(string(yp),2)+' '+                 $
             'detsubsys="'+detsubsys+'" mirror="HRMA" '+                $
             'ardlibparfile="ardlib.par" verbose=1 '+                   $
             'obsfile="'+eventfile+'[1]"'
       
       if keyword_set(grating) then begin
          printf,unit,'Grating 0th order efficiency included'
          printf,unit,' '
          cmdstring='mkarf '+parms+' grating="NONE"'
       endif else begin
          cmdstring='mkarf '+parms+' grating="HETG"'
       endelse

       spawn,exproot+cmdstring,result

       printf,unit,space+cmdstring
       printf,unit,space+result
       printf,unit,' '

;
;   Done with this ARF file
;
    endif


;
;   Set keywords in PHA/PI header
;
    get_lun,unit2
    openw,unit2,'temp_keywords.txt'
    if (n_elements(exptime) ne 0) then begin
        printf,unit2,'EXPOSURE = ',exptime,' / Exposure time (s)'
    endif
    printf,unit2,'BACKSCAL = ',area(i),' / Region area in sq. pixels'
    printf,unit2,'RESPFILE = ',rmffile,' / Redistribution matrix file (RMF)'
    printf,unit2,'ANCRFILE = ',arffiles(i),' / Anc. Resp. file (ARF)'
    printf,unit2,'BACKFILE = ',backfiles(i),' / bgrd FITS file'
    close,unit2
    free_lun,unit2

    cmdstring='fmodhead "'+spectfiles(i)+'[1]" temp_keywords.txt'

    spawn,cmdrootl+cmdstring,result

    printf,unit,space+cmdstring
    printf,unit,space+result
    printf,unit,' '
    printf,unit,' '
    printf,unit,' '


;
;   Use GRPPHA to group channels for fitting
;
    if keyword_set(grpcnts) then begin

       printf,unit,'Grouping spectrum with GRPPHA'
       printf,unit,' '
       printf,unit,'Input file     : ',spectfiles(i)
       printf,unit,'Output file    : ',groupfiles(i)
       printf,unit,'Counts per bin : ',grpstring
       printf,unit,' '

       cmdstring='grppha '+spectfiles(i)+' '+groupfiles(i)+' '+  $
                 '"group min '+grpstring+'" "exit"'

       spawn,cmdrootl+cmdstring,result

       printf,unit,space+cmdstring
       printf,unit,space+result
       printf,unit,' '
       printf,unit,' '
       printf,unit,' '

    endif


;
;   Record different RMF regions included in this PI/ARF pair
;


;
; Done with this region
;
endfor


;
; Close up logfile
;
printf,unit,'Extraction complete'
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
