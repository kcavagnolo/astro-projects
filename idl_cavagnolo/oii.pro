PRO oii, inmb

mb = [-13.93,-15.18,-16.43,-17.68,-18.93,-20.18,-21.43]
p25 = [-0.028,-0.136,-0.140,-0.046,0.056,0.297,0.517]
p50 = [0.098,0.039,-0.074,0.092,0.201,0.530,0.762]
p75 = [0.414,0.291,0.111,0.282,0.395,0.781,0.972]

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
