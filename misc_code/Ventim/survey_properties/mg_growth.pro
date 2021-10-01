;function to find value of H(a)
function hubble_constant,z,theta
common constants, G, c, H_0, gm_per_Msun, cm_per_Mpc, delta_c
a=1.0/(1.0+z)
omega_0=theta.Omega_M_0+theta.Omega_lambda_0+theta.Omega_R_0
hubble_constant=theta.h*H_0*SQRT((theta.Omega_M_0*(a^(-3.0))+theta.Omega_R_0*(a^(-4.0))+theta.Omega_lambda_0+(1-omega_0)*(a^(-2.0))))
return,hubble_constant
end

function mg_integrand, a,theta
common constants
common mg_integrand_block, theta_dummy
theta = theta_dummy
z = 1./a - 1.
; a=1.0/(1.0+z)l0_)
return, (((theta.Omega_M_0*(a^(-3.0))*((theta.h*H_0/hubble_constant(z,theta))^2.0))^theta.gamma)-1.0)/a
end

;this function finds the exponential factor in growth function by
;numerical integration
function mg_growth_exp,z,theta, amin=amin
common constants, G, c, H_0, gm_per_Msun, cm_per_Mpc, delta_c
common mg_integrand_block, theta_dummy
theta_dummy = theta
amin=n_elements(amin) eq 0 ? 0.001 : amin
a=1.0/(1.0+z)
; integrand=(((theta.Omega_M_0*(a^(-3.0))*((theta.h*H_0/hubble_constant(z,theta))^2.0))^theta.gamma)-1.0)/a
integrand = 'mg_integrand'
mg_growth_exp=QROMB(integrand,amin,a,eps=1E-2)
return,mg_growth_exp
end

;To find Growth function D(a) using growth index gamma, which takes
;wider range of values in modified gravity models than GTR. For GTR,
;gamma is approx. 5/11 or 0.55
;D(a)=a.g(a)=a.exp{integrate[(omega_m(a)^gamma)-1).da/a] [0,a]}
function mg_growth,z,theta, amin=amin
common constants, G, c, H_0, gm_per_Msun, cm_per_Mpc, delta_c
a=1.0/(1.0+z)
amin=n_elements(amin) eq 0 ? 0.001 : amin
growth_func=a*EXP(mg_growth_exp(z,theta, amin=amin))
return,growth_func
end

; function mg_growth_test, theta
;   amin = 10^(-1*findgen(30)/29*10 - 1)
;   growth = make_array(30, /float)
;   for i = 0, 9 do begin
;      growth[i] = mg_growth(0.5, theta, amin=amin[i])
;   endfor
;   return, [[amin], [growth]]
; end

