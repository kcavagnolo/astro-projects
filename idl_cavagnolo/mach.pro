PRO mach, temp, rho

g = 5./3.

M = sqrt((g*sqrt(a^2*g^2+2*a^2*g+a^2+2*a*g^2-12*a*g+2*a+g^2+2*g+1))/(2*(2*g^2-2*g))+sqrt(a^2*g^2+2*a^2*g+a^2+2*a*g^2-12*a*g+2*a+g^2+2*g+1)/(2*(2*g^2-2*g))+(a*g^2)/(2*(2*g^2-2*g))+(a*g)/(2*g^2-2*g)+a/(2*(2*g^2-2*g))+g^2/(2*(2*g^2-2*g))-(3*g)/(2*g^2-2*g)+1/(2*(2*g^2-2*g)))

print, 'Mach from T2/T1 ', M

M = sqrt((2.0*b)/(g+1.-b*(g-1.)))

print, 'Mach from rho2/rho1: ', M

END
