PRO viktx, x, a, f, pder

;mintx = 1.0
;t0    = 5.
;rt    = 1000.
;rcool = 50.0
;acool = 4.0
;ap    = 0.10
;b     = 5.00
;c     = 10.0

np = n_params(0)
mintx = double(a[0])
t0 = double(a[1])
rt = double(a[2])
rcool = double(a[3])
acool = double(a[4])
ap = double(a[5])
b = double(a[6])
c = double(a[7])
num1  = ((x/rcool)^acool)+(mintx/t0)
den1  = 1.+((x/rcool)^acool)
num2  = (x/rt)^(-ap)
den2  = (1.+(x/rt)^b)^(c/b)
term1 = num1/den1
term2 = num2/den2
f     = t0*term1*term2

IF (np GE 4) THEN BEGIN
   MESSAGE, 'You are shitting me, right? Calculate partial derivatives for this function? HA!'
ENDIF

END
