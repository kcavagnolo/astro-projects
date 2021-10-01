function integ,a
gamma=0.55
omega_m_0=0.3
omega_l_0=0.7
h_0=70.0
; a=1.0/(1.0+z)
h_a=h_0*sqrt(omega_m_0*a^(-3.0)+omega_l_0)
omega_m_a=omega_m_0*(a^(-3.0))*((h_0/h_a)^2.0)
;print,h_a,omega_m_a
integ_val=((omega_m_a^gamma)-1.0)/a
return, integ_val
end

function testq,z
a=1.0/(1.0+z)
growth_f_exp=qromb('integ',0.0,a,eps=1E-2)
growth_fn=a*exp(growth_f_exp)
return, growth_fn
end
