PRO allentx, x, a, f, pder

;# the function
;# T(r) = T0 + T1[(r/rc)^n/(1+r/rc)^n]

np = n_params(0)
t0 = double(a[0])
t1 = double(a[1])
rc = double(a[2])
n = double(a[3])

f = t0 + t1*((x/rc)^n/(1.0+(x/rc))^n)

IF (np GE 4) THEN BEGIN
   pder[*,0] = 1.0
   pder[*,1] = (x/(rc+x))^n
   pder[*,2] = -n*t1*x^n*(c+x)^(-n-1)
   pder[*,3] = (x/(rc+x))^n*t1*alog(x/(rc+x))
ENDIF

END
