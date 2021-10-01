pro filter_images,objstring,regfile,mapdir=mapdir,cmdroot=cmdroot, $
                  logfile=logfile,outfile=outfile
;-----------------------------------------------------------------------
; Name: FILTER_IMAGES
;
; Purpose: Apply a REGION file to  set of images using DMCOPY
;          
; Inputs:  objstring -- template search string
;            regfile -- name of region file to apply
;
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 10-19-00
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'filter_images,objstring,regfile,mapdir=mapdir,cmdroot=cmdroot,',$
   '              logfile=logfile,outfile=outfile'
   return   
endif


;
; Handle defaults
;
if (n_elements(mapdir) eq 0) then mapdir='.'
if (n_elements(logfile) eq 0) then logfile='filter.log'
if (n_elements(outfile) eq 0) then outfile='filter_img.fits'


;
; Define location of ASCDS tools 
;
if (n_elements(cmdroot) eq 0) then cmdroot='source $HOME/.cxcds_test.csh ; '


;
; Define miscellaneous quantities
;
space=''
space1='          '
space5=space1+space1+space1+space1+space1
ciaoversion='2.0b'
progversion='1.0b1'


;
; Open logfile 
;
get_lun,unit
openw,unit,logfile
printf,unit,' '
printf,unit,'Date             : '+!stime
printf,unit,'Program Version  : '+progversion
printf,unit,'CXCDS Version    : '+ciaoversion
printf,unit,' '
printf,unit,'File filter      : ',objstring
printf,unit,'Region file      : ',regfile
printf,unit,'Map directory    : ',mapdir
printf,unit,'Output file      : ',outfile
printf,unit,' '



;
; Locate all images in the MAPDIR directory which match OBJSTRING
;
files=findfile(mapdir+'/'+objstring)
numfiles=n_elements(files)


;
; Make sure we actually found something
;
if (files(0) eq '') then begin
   print,string(7B),'ERROR: ', $
   'No images matching OBJSTRING were detected'
   return   
endif



;
; Create set of output filenames
;
ofiles=strarr(numfiles)
for i=0,numfiles-1 do begin
    j1=strlen(mapdir+'/')
    j2=strlen(files(i))
    curname=strmid(files(i),j1,j2)

    j3=strpos(curname,'emap')

    if (j3 eq -1) then begin
       ofiles(i)=curname+'_filt'
    endif else begin
       ofiles(i)=strmid(curname,0,j3)+'filt_emap.fits'
    endelse

endfor


;
; Record which files will be filtered into the logfile
;
printf,unit,' '
printf,unit,'Files to be filtered:'
printf,unit,' '
format0='$(a5,2(3x,a40))'
format1='$(i5,2(3x,a40))'
printf,unit,format0,'Index','Input file'+space5,'Output file'+space5
printf,unit,format0,'-----','----------'+space5,'-----------'+space
for i=0,numfiles-1 do begin
    printf,unit,format1,i,files(i)+space5,ofiles(i)+space5
endfor
printf,unit,' '
printf,unit,' '


;
; Filter source images using DMCOPY
;
for i=0,numfiles-1 do begin

    printf,unit,' '
    printf,unit,'Filtering image: ',files(i)
    printf,unit,' '
    filter='"'+files(i)+'[(x,y)=region('+regfile+')]" '+ofiles(i)

    cmdstring='dmcopy '+filter
    spawn,cmdroot+cmdstring,result
    result='Done'

    printf,unit,space+cmdstring
    printf,unit,space+result
    printf,unit,' '

endfor


;
; Close logfile
;
printf,unit,'Image filtering complete'
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
