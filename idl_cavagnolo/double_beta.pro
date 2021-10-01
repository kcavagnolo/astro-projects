PRO double_beta, x, a, f

s01   = a[0]
s02   = a[1]
rc1   = a[2]
rc2   = a[3]
beta1 = a[4]
beta2 = a[5]
bgd   = a[6]
term1 = s01*(1.0+(x/rc1)^2.0)^(0.5-3.0*beta1)
term2 = s02*(1.0+(x/rc2)^2.0)^(0.5-3.0*beta2)
f     = term1+term2+bgd

END
