PRO nfw, x, a, f, pder

;# the function
;# mdm = 4*pi*rs^3.*rho_c*delta_c*(ln(1+r/rs)-((r/rs)/(1+r/rs)))
;# rho = (rho_c*delta_c)/((r/rs)*(1+(r/rs))^2)
;# rho_c = (3*Hz^2)/(8*pi*G)
;# delta_c = (delta/3)*(c^3/[ln(1+c)-c/(1+c)])

;# constants
G = 6.67259d-8                  ;# gravitational constant [cm^3/g/s^2]
h0 = 70.1/3.08568025d19         ;# Hubble constant at z=0 in 1/s
omega_m = 0.279                 ;# matter density
omega_l = 0.721                 ;# lambda density
omega_k = 0.0                   ;# radiation/curvature density

;# parameters
np = n_params(0)
rs = double(a[0])
c  = double(a[1])
z  = double(a[2])
delta = double(a[3])

;# the actual functions
hz = h0*sqrt(omega_m*(1.0+z)^3. + omega_l + omega_k*(1.0+z)^2.)
rhoc = (3.0*hz^2.)/(8.0*!PI*G)
delta_c = (delta/3.0)*(c^3./(alog(1.0+c)-(c/(1.0+c))))
f = 4.0*!PI*rs^3.*rhoc*delta_c*(alog(1.0+(x/rs)-((x/rs)/(1.0+(x/rs)))))

END
