pro check_aspect,inpfile,outfile=outfile,data=data
;-----------------------------------------------------------------------
;
; Name: CHECK_ASPECT
;
; Purpose: Reads in an Chandra AOFF file and makes a diagnostic
;          plot of the aspect quality.
;          
;          
; Inputs: 
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by Michael Wise, 1-26-89
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 3)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'check_aspect,inpfile,[outfile=outfile,data=data]'
   return   
endif

;
; Read in data file
;
data=mrdfits(inpfile,1,hd)


;
; Make sure this is an AOFF file
;
type=sxpar(hd,'EXTNAME','Parameter EXTNAME not found')
type=strtrim(type,2)
if (type ne 'ASPOFF') then begin
   print,string(7B),'ERROR: This file is not an ASPOFF file'
   return   
endif


;
; Get OBSID out of header
;
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')

;
; Set default output file name
;
if (n_elements(outfile) eq 0) then begin
   outfile='aspect_qual_obsid'+strtrim(string(obsid),2)+'.ps'
endif


;
; Extract the plot variables
;
t=data.time-min(data.time)
t=t/1000.				; Convert to ksecs
x=data.x_offsets
y=data.y_offsets
r=data.roll_offsets


;
; Make the plot
;
!p.multi=[0,2,2]
open,'/landscape',outfile
plot,x,y,psym=3,xtitle='!6X_OFFSETS [pix]',ytitle='!6Y_OFFSETS [pix]',$
                title='OBSID '+strtrim(string(obsid),2)
plot,t,x,xtitle='Time [ksec]',ytitle='X_OFFSETS [pix]'
plot,t,y,xtitle='Time [ksec]',ytitle='Y_OFFSETS [pix]'
plot,t,r,xtitle='Time [ksec]',ytitle='ROLL_OFFSETS [deg]'
shut
!p.multi(*)=0


;
; Return to IDL
;
return
end
