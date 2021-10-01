PRO sbprof, x, a, f

s0    = a[0]
s02   = a[1]
rc    = a[2]
rs    = a[3]
rc2   = a[4]
alpha = a[5]
beta  = a[6]
beta2 = a[7]
gamma = a[8]
epsil = a[9]
term1 = (s0*(x/rc)^(-alpha))*((1+(x/rc)^2.)^(0.5+0.5*alpha-3*beta))
term2 = (1+(x/rs)^gamma)^(-epsil/gamma)
term3 = (s02)*((1+(x/rc2)^2.)^(-3*beta2+0.5))
f     = (term1*term2)+term3

END
