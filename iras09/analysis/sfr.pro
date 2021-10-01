PRO SFR

z = 0.441844
nu0 = 1.4
alpha = -0.8
cosmology, z, result, /silent
dl = result[2]

snu = 0.2395/1000.
sfr = (snu)*(0.066/(1+z))*(dl^2.)*((1+z)*nu0/1.4)^(alpha)
print, FORMAT='(A-20,F10.2,A+10)','Spur',sfr,'Msol/yr'

snu = 0.12/1000.
sfr = (snu)*(0.066/(1+z))*(dl^2.)*((1+z)*nu0/1.4)^(alpha)
print, FORMAT='(A-20,F10.2,A+10)','WBlob',sfr,'Msol/yr'

snu = 0.16/1000.
sfr = (snu)*(0.066/(1+z))*(dl^2.)*((1+z)*nu0/1.4)^(alpha)
print, FORMAT='(A-20,F10.2,A+10)','NNWblob',sfr,'Msol/yr'

END
