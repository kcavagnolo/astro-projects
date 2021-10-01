pro locate_fef,chipx,chipy,ccdtemp=ccdtemp,chip=chip
;-----------------------------------------------------------------------
; Name: LOCATE_FEF
;
; Purpose: 
;          
; Inputs: 
;         
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 1-18-00
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 2)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'locate_fef,chipx,chipy,[ccdtemp=ccdtemp,chip=chip]'
   return   
endif


;
; Set defaults
;
if (n_elements(ccdtemp) eq 0) then ccdtemp=-110
if (n_elements(chip) eq 0) then chip=7


;
; Define some miscellaneous stuff
;
node=['a','b','c','d']
datestring='D1999-12-09'


;
; Define grid of chip binsizes
;
case ccdtemp of
     -110: begin
           xbin=[256,256,256,256,256,64,256,32,256,256]
           ybin=[32,32,32,32,32,64,32,32,32,32]
           end
     -100: begin
           xbin=[-1,-1,-1,-1,-1,64,-1,32,-1,-1]
           ybin=[-1,-1,-1,-1,-1,64,-1,32,-1,-1]
           end
      -90: begin
           xbin=[256,256,256,256,256,64,256,32,256,256]
           ybin=[1024,1024,1024,1024,1024,64,1024,32,1024,1024]
           end
     else: begin
           xbin=[256,256,256,256,256,64,256,32,256,256]
           ybin=[32,32,32,32,32,64,32,32,32,32]
           end
endcase


;
; Do some parameter checking
;
if ((chip lt 0) or (chip gt 9)) then begin
   print,string(7B),'ERROR: Value of CHIP must be between 0-9'
   return
endif
if ((ccdtemp ne -90) and (ccdtemp ne -100) and (ccdtemp ne -110)) then begin
   print,string(7B),'ERROR: Value of CCDTEMP must be -90, -100, or -110'
   return
endif
if ((chipx lt 0) or (chipx gt 1023)) then begin
   print,string(7B),'ERROR: Value of CHIPX must be between 0-1023'
   return
endif
if ((chipy lt 0) or (chipy gt 1023)) then begin
   print,string(7B),'ERROR: Value of CHIPY must be between 0-1023'
   return
endif


;
; Test for whether files exist for the specified chip
;
if ((xbin(chip) eq -1) or (ybin(chip) eq -1)) then begin
   print,string(7B),$
   'ERROR: No FEF files exist for the specified CHIP and CCDTEMP'
   return
endif


;
; Construct name of appropriate FEF files
;
xmin=chipx
xcbin=xbin(chip)
inode=fix(xmin/256)
xindex=(fix(xmin/xcbin))-(inode*(256/xcbin))

ymin=chipy
ycbin=ybin(chip)
yindex=fix(ymin/ycbin)

fptemp='FP'+strtrim(string(ccdtemp),2)
chip=strtrim(string(chip),2)
ns=node(inode)
if (xindex lt 10) then xs='0'+strtrim(string(xindex),2) $
                  else xs=strtrim(string(xindex),2) 
if (yindex lt 10) then ys='0'+strtrim(string(yindex),2) $
                  else ys=strtrim(string(yindex),2)
xbs=strtrim(string(xcbin),2)
ybs=strtrim(string(ycbin),2)

feffile1='acis'+chip+ns+'_x'+xs+'_y'+ys+'_'+fptemp+'_'+datestring+ $
               'fef_piN0001.fits'
feffile2='acis'+chip+ns+'_x'+xs+'_y'+ys+'_'+fptemp+'_'+datestring+ $
               'fef_phaN0001.fits'


;
; Print filenames
;
print,feffile1,feffile2


;
; Return to IDL
;
return
end
