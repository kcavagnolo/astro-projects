pro create_flux_images,objstring,mapdir=mapdir,cmdroot=cmdroot, $
                  logfile=logfile,outfile=outfile
;-----------------------------------------------------------------------
; Name: CREATE_FLUX_IMAGES
;
; Purpose: Compute flux images
;          
; Inputs:  objstring -- template search string
;
; Comments: 
;           
; Revision history:
;
;       written by Michael Wise, 10-30-00
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'create_flux_images,objstring,mapdir=mapdir,cmdroot=cmdroot,',$
   '              logfile=logfile,outfile=outfile'
   return   
endif


;
; Handle defaults
;
if (n_elements(mapdir) eq 0) then mapdir='.'
if (n_elements(logfile) eq 0) then logfile='flux.log'
if (n_elements(outfile) eq 0) then outfile='flux_img.fits'


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
; Separate IMG and EMAP filenames
;
i=strpos(files,'img')
j=where(i ne -1)
ifiles=files(j)

i=strpos(files,'emap')
j=where(i ne -1)
efiles=files(j)

if (n_elements(ifiles) ne n_elements(efiles)) then begin
   print,string(7B),'ERROR: ', $
   'IMG and EMAP files do not match'
   return   
endif

numfiles=n_elements(ifiles)


;
; Create set of output filenames
;
ofiles=strarr(numfiles)
for i=0,numfiles-1 do begin
    j1=strlen(mapdir+'/')
    j2=strlen(efiles(i))
    curname=strmid(efiles(i),j1,j2)

    j3=strpos(curname,'emap')

    if (j3 eq -1) then begin
       ofiles(i)=curname+'_flux'
    endif else begin
       ofiles(i)=strmid(curname,0,j3)+'flux.fits'
    endelse

endfor


;
; Record which files will be filtered into the logfile
;
printf,unit,' '
printf,unit,'Flux files to be generated:'
printf,unit,' '
format0='$(a5,2(3x,a40))'
format1='$(i5,2(3x,a40))'
printf,unit,format0,'Index','Input file'+space5,'Output file'+space5
printf,unit,format0,'-----','----------'+space5,'-----------'+space
for i=0,numfiles-1 do begin
    printf,unit,format1,i,ifiles(i)+space5,ofiles(i)+space5
endfor
printf,unit,' '
printf,unit,' '


;
; Create flux images using FARITH
;
for i=0,numfiles-1 do begin

    printf,unit,' '
    printf,unit,'Creating flux image: ',ofiles(i)
    printf,unit,' '

    filter=ifiles(i)+' '+efiles(i)+' '+ofiles(i)+' / '
    cmdstring='farith datatype=e '+filter
    spawn,cmdroot+cmdstring,result

    printf,unit,space+cmdstring
    printf,unit,space+result
    printf,unit,' '

endfor


;
; Close logfile
;
printf,unit,'Creation of flux images complete'
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
