;-----------------------------------------------------------------------
; Name:
; Purpose: 
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
if ((np lt 10) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   ' '
   return   
endif


;
; Return to IDL
;
return
end
