PRO logmean, mu, sig

amean = 10.0^(mu)+0.5*(sig^2.0)
dev = sqrt(amean^2.0*10.0^(sig^2.0-1.0))
print, format='(A-10,F10.3,A4,F8.3)','Mean:',amean,'+/-',dev

END
