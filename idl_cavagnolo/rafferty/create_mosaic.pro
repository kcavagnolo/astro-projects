pro create_mosaic,objstring,mapdir=mapdir,cmdroot=cmdroot,weight=weight,   $
                  logfile=logfile,outfile=outfile
;-----------------------------------------------------------------------
; Name: CREATE_MOSAIC 
;
; Purpose: Mosaic together the images for several chips 
;          into one image
;          
; Inputs:  objtring -- string containing the file filter (e.g. perseus*.fits)
;         
; Comments: 
;           
;           
; Revision history:
;       written by Michael Wise, 10-13-90
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'create_mosaic,objstring,mapdir=mapdir,cmdroot=cmdroot,weight=weight',$
   '              logfile=logfile,outfile=outfile'
   return   
endif


;
; Handle defaults
;
if (n_elements(mapdir) eq 0) then mapdir='.'
if (n_elements(logfile) eq 0) then logfile='mosaic.log'
if (n_elements(outfile) eq 0) then outfile='mosaic_img.fits'


;
; Define location of ASCDS tools 
;
if (n_elements(cmdroot) eq 0) then cmdroot='source /iraf/ciao-2.2/bin/ciao.csh ; '


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
files=findfiles(objstring,root=mapdir,/recurse)
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
; Check the sizes of the images and make sure they the same
;
sizes=intarr(numfiles)
for i=0,numfiles-1 do begin
       curfile=files(i)
       spawn,'fitsdump -H -e 0 '+curfile+' | grep NAXIS1',result
       sizes(i)=sxpar(result,'NAXIS1')
endfor
if (n_elements(uniq(sizes)) ne 1) then begin
   print,string(7B),'WARNING: Component images are not all the same size'
   printf,unit,' '
   printf,unit,'WARNING: Component images are not all the same size'
   printf,unit,' '
endif 
;
; Just to be safe, select largest size
;
npix=strtrim(string(max(sizes)),2)



;
; Create weights based on relative exposures if indicated
;
weights=fltarr(numfiles)
if keyword_set(weight) then begin

   for i=0,numfiles-1 do begin
;
;      Get values of EXP keyword from header
;
       curfile=files(i)
       spawn,'fitsdump -H '+curfile+' | grep EXPOSURE',result

;
;      Convert EXPOSURE to floating point value
;
       weights(i)=sxpar(result,'EXPOSURE')

   endfor

;
;  Normalize by average exposure
;
   exposure=total(weights)/n_elements(weights)
   weights=weights/exposure

endif else begin
;
;  Otherwise set uniform weights
;
   exposure=1.0
   weights(*)=1.0

endelse


;
; Record which files will be mosaiced into the logfile
;
printf,unit,' '
printf,unit,'Files to be mosaiced:'
printf,unit,' '
format0='$(a5,3x,a40,3x,a5,3x,a10)'
format1='$(i5,3x,a40,3x,i5,3x,f10.7)'
printf,unit,format0,'Index','Filename'+space5,'Size','Weight'
printf,unit,format0,'-----','--------'+space5,'----','------'
for i=0,numfiles-1 do begin
    printf,unit,format1,i,files(i)+space5,sizes(i),weights(i)
endfor
printf,unit,' '
printf,unit,'Mean Exposure    : ',exposure
printf,unit,' '
printf,unit,' '


;
; Create input image stack file
;
get_lun,unit1
openw,unit1,'list_img.txt'

if keyword_set(weight) then begin

   for i=0,numfiles-1 do begin
;
;      Create weighted file
;
       curfile=files(i)
       wgtfact=strtrim(string(weights(i)),2)
       wgtfile='weight_'+strtrim(string(i),2)+'_img.fits'
       cmdstring='fcarith '+curfile+' '+wgtfact+' '+wgtfile+' MUL'
       spawn,cmdstring,result

       printf,unit,space+cmdstring
       printf,unit,space+result
       printf,unit,' '

       printf,unit1,wgtfile

   endfor

endif else begin

   for i=0,numfiles-1 do begin
       printf,unit1,files(i)
   endfor

endelse

close,unit1
free_lun,unit1



;
; Create source image using DMREGRID
;
printf,unit,' '
printf,unit,'Creating mosaic:'
printf,unit,' '
filter='infile="@list_img.txt" outfile="'+outfile+'" '+     $
       'bin="1:'+npix+':1,1:'+npix+':1" '+                  $
       'rotangle=0 rotxcenter=0 rotycente=0 '+              $
       'xoffset=0 yoffset=0 '+                              $
       'clobber="yes" verbose=0 npts=1'
           
cmdstring='dmregrid '+filter
spawn,cmdroot+cmdstring,result

printf,unit,space+cmdstring
printf,unit,space+result
printf,unit,' '


;
; Finally, update HEADER
;
printf,unit,' '
printf,unit,'Updating EXPOSURE keyword:'
printf,unit,' '

get_lun,unit1
openw,unit1,'header.txt'
printf,unit1,'COMMENT '
printf,unit1,'COMMENT Date             : '+!stime
printf,unit1,'COMMENT Program Version  : '+progversion 
printf,unit1,'COMMENT CXCDS Version    : '+ciaoversion 
printf,unit1,'COMMENT '
printf,unit1,'COMMENT File filter      : ',objstring
printf,unit1,'COMMENT Map directory    : ',mapdir
printf,unit1,'COMMENT Output file      : ',outfile
printf,unit1,'COMMENT '
if keyword_set(weight) then begin
   printf,unit1,'EXPOSURE ',exposure,' / Mean exposure for all CCDs'
   printf,unit1,'COMMENT Mean Exposure    : ',exposure
endif
printf,unit1,'COMMENT '

close,unit1
free_lun,unit1

cmdstring='fmodhead "'+outfile+'[0]" header.txt'
spawn,cmdstring,result

printf,unit,space+cmdstring
printf,unit,space+result
printf,unit,' '



;
; A little housecleaning
;
printf,unit,' '
printf,unit,'Removing temporary files:'
printf,unit,' '

if keyword_set(weight) then begin
   cmdstring='unalias rm ; rm -f header.txt list_img.txt weight_*_img.fits'
endif else begin 
   cmdstring='unalias rm ; rm -f header.txt list_img.txt'
endelse

spawn,cmdstring,result,/sh

printf,unit,space+cmdstring
printf,unit,space+result
printf,unit,' '


;
; Close logfile
;
printf,unit,'Image mosaic complete'
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
