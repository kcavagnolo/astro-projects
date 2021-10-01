pro tcool

dat1 = 'dat/culled_r2500-50_7-7.dat'

restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"
dat = read_ascii(dat1, template = xspectemp_rin_normerr_src)

get_lun, file1
openw, file1, "tfrac_tcool.dat"
printf, file1, format='(A-20,A20,3A12,3A10)',"#Name", "Obsid", "ne", "nerrlo", "nerrhi", "tcool", "tclo", "tchi"
printf, file1, format='(A-20,A20,3A12,3A10)',"#Name", "Obsid", "cm**-3", "--", "--", "Gyr", "--", "--"
FOR i=0,n_elements(dat.obsid)-1 DO BEGIN
    z = dat.z[i]
    cosmology, z, result, /silent
    da = result[3]
    nmek = dat.norm[i]
    nmekhi = dat.normhi[i]-nmek
    nmeklo = nmek-dat.normlo[i]
    tx = dat.tx[i]
    tlo = tx-dat.tlo[i]
    thi = dat.thi[i]-tx
    r = (dat.rout[i]*60.*result[4])/1000.
    V = (4.*!pi*r^3.)/3.

; quick fix for err on V, call it 10%
    Verr    = 0.1*V
    nelec   = sqrt((15.07*da^2.*(1.+z)^2.*nmek)/(3.08e10*V))
    nerrhi  = nelec*sqrt((1./4.)*(nmekhi/nmek)^2.+(1./4.)*(Verr/V)^2.)
    nerrlo  = nelec*sqrt((1./4.)*(nmeklo/nmek)^2.+(1./4.)*(Verr/V)^2.)
    tc   = 8.5e10*(nelec/1.2e-3)^(-1.)*sqrt(tx*0.116)/1e9
    tcerrhi = tc*sqrt((nerrhi/nelec)^2.+(1./4.)*(thi/tx)^2.)
    tcerrlo = tc*sqrt((nerrlo/nelec)^2.+(1./4.)*(tlo/tx)^2.)
    name = dat.cluster[i]
    obsid = dat.obsid[i]
    printf, file1, format='(A-20,A20,3E12.2,3F10.3)', name, obsid, nelec, nerrlo, nerrhi, tc, tcerrlo, tcerrhi
ENDFOR

free_lun, file1

END
