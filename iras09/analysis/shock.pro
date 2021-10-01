PRO shock

r1 = 1.34e-2
r2 = 2.32e-2
ga = 5./3.
Mr = sqrt(2.0/(((r1/r2)*(ga+1.0))-(ga-1.0)))
print, 'Mach from rho: ',Mr

p1 = 7.15966E-02
p2 = 1.19472E-01
Mp = sqrt(((p2*(ga+1)/p1)+(ga-1))/(2*ga))
print, 'Mach from p: ',Mp

t1 = 6.39
t2 = 4.91
phi = ((t2/t1)+(((ga-1)/(ga+1))^2.)-((4*ga)/(ga+1)^2.))*(ga+1)^2.
Mt = (((2*(ga-1))+phi)/(2*ga))^(1./4.)
print, 'Mach from tx: ',Mt

END
