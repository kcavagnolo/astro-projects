PRO shock

r1 = 3.00952E-02
r2 = 4.16828E-02
ga = 5./3.
Mr = sqrt(2.0/(((r1/r2)*(ga+1.0))-(ga-1.0)))
print, 'Mach from rho: ',Mr

p1 = 1.0
p2 = 1.0
Mp = sqrt(((p2*(ga+1)/p1)+(ga-1))/(2*ga))
print, 'Mach from p: ',Mp

t1 = 1.0
t2 = 1.0
phi = ((t2/t1)+(((ga-1)/(ga+1))^2.)-((4*ga)/(ga+1)^2.))*(ga+1)^2.
Mt = (((2*(ga-1))+phi)/(2*ga))^(1./4.)
print, 'Mach from tx: ',Mt

END
