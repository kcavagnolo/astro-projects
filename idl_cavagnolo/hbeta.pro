PRO hbeta, inmb

mb = [-12.68,-13.93,-15.18,-16.43,-17.68,-18.93,-20.18,-21.43]
p25 = [0.400,0.371,0.401,0.349,0.426,0.490,0.639,0.819]
p50 = [0.449,0.397,0.418,0.399,0.495,0.547,0.767,0.892]
p75 = [0.539,0.435,0.497,0.451,0.649,0.670,0.890,1.032]

xx = maken(min(mb),max(mb),1d6)
yy25 = interpol(p25,mb,xx)
yy50 = interpol(p50,mb,xx)
yy75 = interpol(p75,mb,xx)
idx = closest(inmb,xx)
idx = idx[0]
print, 'Conversion factor (25th percentile): ', 1/((10.^(yy25[idx]))*1d41), ' [M_sol yr**-1]/ergs s**-1'
print, 'Conversion factor (50th percentile): ', 1/((10.^(yy50[idx]))*1d41), ' [M_sol yr**-1]/ergs s**-1'
print, 'Conversion factor (75th percentile): ', 1/((10.^(yy75[idx]))*1d41), ' [M_sol yr**-1]/ergs s**-1'

END
