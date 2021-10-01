PRO trprof, x, a, f

tmin  = a[0]
t0    = a[1]
rcool = a[2]
acool = a[3]
rt    = a[4]
ap    = a[5]
b     = a[6]
c     = a[7]
num1  = ((x/rcool)^acool)+(tmin/t0)
den1  = 1.+(x/rcool)^acool
num2  = (x/rt)^(-ap)
den2  = (1.+(x/rt)^b)^(c/b)
term1 = num1/den1
term2 = num2/den2
f     = t0*term1*term2

END
