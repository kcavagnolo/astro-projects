pro locate_chip,ix,iy,node,ccdtemp=ccdtemp,chip=chip,box=box
;-----------------------------------------------------------------------
; Name: LOCATE_CHIP
;
; Purpose: 
;          
; Inputs: 
;         
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 02-28-00
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 3) or (np gt 3)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'locate_chip,ix,iy,node,[ccdtemp=ccdtemp,chip=chip,box=box]'
   return   
endif


;
; Set defaults
;
if (n_elements(ccdtemp) eq 0) then ccdtemp=-110
if (n_elements(chip) eq 0) then chip=7


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
if ((ix lt 0) or (ix gt 31)) then begin
   print,string(7B),'ERROR: Value of IX must be between 0-31'
   return
endif
if ((iy lt 0) or (iy gt 31)) then begin
   print,string(7B),'ERROR: Value of IY must be between 0-31'
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
; Find CHIPX, CHIPY rectangle
;
xcbin=xbin(chip)
xmin=(ix*xcbin)+(node*256)+1
xmax=xmin+xcbin-1

ycbin=ybin(chip)
ymin=(iy*ycbin)+1
ymax=ymin+ycbin-1

;
; Store values in return array
;
box=[xmin,xmax,ymin,ymax]


;
; Return to IDL
;
return
end
