pro fix_backscal,sou_file,bg_file
;-----------------------------------------------------------------------
;
; Name: FIX_BACKSCAL
;
; Purpose: Fixes improper background scaling by fixing the BACKSCAL 
;          keyword in "bg.fits" and changing the EXPOSURE keyword
;          
; Inputs:  sou_file - source file
;          bg_file - background file
;         
; Comments: Only necessary for Ciao 3.0.2 data
;           
;           
; Revision history:
;       written by DR, 2004-7-27
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 2)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'fix_backscal, sou_file, bg_file'
   return   
endif


;
; Define root directory for CIAO
;
get_lun,unit
openr,unit,'ciao_info.txt'
ciao_root=' '
readf,unit,ciao_root
cmdroot='source '+ciao_root+'/bin/ciao.csh ; '
close,unit
free_lun,unit


;
; Update the background header with the BACKSCAL and EXPOSURE keywords
;
print,'Normalizing the background...'

hdbg=headfits('./background/bg.fits',exten=1)
bg_exp=sxpar(hdbg,'EXPOSURE','Parameter EXPOSURE not found')
bgexposure=strtrim(string(bg_exp),2)
cmdstring='dmhedit '+bg_file+' filelist=none operation=add key=EXPOSURE value='+bgexposure+' datatype=float'
spawn,cmdroot+cmdstring,result

hdsou=headfits(sou_file,exten=1)
backscal=sxpar(hdsou,'BACKSCAL','Parameter BACKSCAL not found')
cmdstring='dmhedit '+bg_file+' filelist=none operation=add key=BACKSCAL value='+strtrim(string(backscal),2)+' datatype=float'
spawn,cmdroot+cmdstring,result

print,'Background exposure time = '+strtrim(string(bg_exp),2)
print,'BACKSCAL keyword = '+strtrim(string(backscal),2)

print,'...done.'


;
; Return to IDL
;
return
end