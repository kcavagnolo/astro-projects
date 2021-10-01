PRO corrfeka, val, nhi, nlo

nhi = (nhi-val)
nlo = (val-nlo)
corr = val*(1.0+0.4418)
nhi = corr*sqrt((nhi/val)^2.)
nlo = corr*sqrt((nlo/val)^2.)

print, 'Red. corr:', corr, '+', nhi, '-', nlo

END
