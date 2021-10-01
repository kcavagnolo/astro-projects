PRO fit_form_100, x, a, f

k0      = a[0]
k100    = a[1]
beta    = a[2]
f       = k0 + k100*(x/0.100)^beta

END
