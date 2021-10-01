pro dbeta, rad, a, f
;-----------------------------------------------------------------------
;
; Name: 
;
; Purpose: 
;
; Inputs:
;
; Comments: 
;
; Author: Clif Kirkpatrick
;
;-----------------------------------------------------------------------

   i1 = a(0)
   r1 = a(1)
   al1 = a(2)

   i2 = a(3)
   r2 = a(4)
   al2 = a(5)

   const = a(6)

   f = i1*(1+(rad/r1)^2.)^(-al1) + i2*(1+(rad/r2)^2.)^(-al2) + const

end
