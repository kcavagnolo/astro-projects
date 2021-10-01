pro beta_model,x,a,f,pder
;-----------------------------------------------------------------------
; Name: BETA_MODEL
;
; Purpose: Calculates a Beta model profile with constant background
;          
; Inputs:      a(0) -- Peak surface brightness
;	       a(1) -- Core radius
;	       a(2) -- Beta value
;              a(3) -- Constant background
;
; Outputs:     pder -- partial derivative with respect to a(*)
;
;
; Revision history:
;
;       written by Amalia Hicks			07/24/00
;	modified to include constant		10/25/00
;
;-----------------------------------------------------------------------
;
;
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 4)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'beta_model,x,a,[f,pder]'
   return
endif


;
; Calculate function values
;
f=a[0]*((1+(x/a[1])^2)^(-3*a[2]+.5))+a[3]


;
; Calculate partial derivatives
;
;    a(0) -- Peak surface brightness
;    a(1) -- Core radius
;    a(2) -- Beta value
;    a(3) -- Constant background
;
if (np ge 4) then begin 

   pder[*,0]=[((1+(x/a[1])^2)^(-3*a[2]+.5))]
   pder[*,1]=[((-3)*a[2]+.5)*a[0]*((1+(x/a[1])^2)^(-3*a[2]-.5))*   $
              (-2)*(x^2)*(a[1]^(-3))]
   pder[*,2]=[((-3)*a[0]*((1+(x/a[1])^2)^(-3*a[2]+.5))*(alog(1+(x/a[1])^2)))]
   pder[*,3]=1.0

endif


;
; Return to IDL
;
end




