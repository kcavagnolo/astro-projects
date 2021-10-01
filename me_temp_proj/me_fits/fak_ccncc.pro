pro fak_ccncc

sig = 2.0

dat1 = 'dat/culled_r2500-50_7-7.dat'
dat2 = 'dat/inner50_cash.dat'
dat3 = '../../redux/scripts/fak_inner50chi_nhfro_fefree_7-7.dat'

; restore the fit template and read some variables
restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"
realr25 = read_ascii(dat1, template = xspectemp_rin_normerr_src)
reali50 = read_ascii(dat2, template = xspectemp_rin_normerr_src)
fakei50 = read_ascii(dat3, template = xspectemp_rin_normerr_src)

names  = realr25.cluster
obsids = realr25.obsid
FOR jj=0,n_elements(obsids)-1 DO BEGIN
    tobs = obsids[jj]
    get = where(reali50.obsid EQ tobs)
    get = get[0]
    push, reali50tx, reali50.tx[get]
    push, reali50lo, reali50.tx[get]  - reali50.tlo[get]
    push, reali50hi, reali50.thi[get] - reali50.tx[get]
ENDFOR
realr25tx = realr25.tx
realr25lo = realr25tx - realr25.tlo
realr25hi = realr25.thi - realr25tx
dec   = reali50tx/realr25tx
dechi = reali50tx/realr25tx*(sqrt((reali50hi/reali50tx)^2.+(realr25hi/realr25tx)^2.))
declo = reali50tx/realr25tx*(sqrt((reali50lo/reali50tx)^2.+(realr25lo/realr25tx)^2.))

FOR ss=0,n_elements(dec)-1 DO BEGIN
    IF (dec[ss]+sig*dechi[ss] LT 1.) THEN push, ctype, 'CC' ELSE push, ctype, 'NCC'
;    print, format='(A-20,A15,F10.2,A10)',names[ss],obsids[ss],dec[ss]+sig*dechi[ss],ctype[ss]
ENDFOR

openw, /get_lun, log, 'fak_ccncc.log'
printf, log, format='(A-20,A15,A10,A10,A10,A10)','#Name','Obsid','RType','FCC','FNCC','%'
FOR jj=0,n_elements(obsids)-1 DO BEGIN
    tname = names[jj]
    tobs = obsids[jj]
    trealr25tx = realr25tx[jj]
    trealr25lo = realr25lo[jj]
    trealr25hi = realr25hi[jj]
    tctype = ctype[jj]
    get = where(fakei50.obsid EQ tobs)
    tfakei50tx = fakei50.tx[get]
    tfakei50lo = fakei50.tx[get]  - fakei50.tlo[get]
    tfakei50hi = fakei50.thi[get] - fakei50.tx[get]
    numcc = 0.0
    numncc = 0.0
    FOR ss=0,n_elements(tfakei50tx)-1 DO BEGIN
        tdec   = tfakei50tx[ss]/trealr25tx
        tdechi = tdec*(sqrt((tfakei50hi[ss]/tfakei50tx[ss])^2.+(trealr25hi/trealr25tx)^2.))
        tdeclo = tdec*(sqrt((tfakei50lo[ss]/tfakei50tx[ss])^2.+(trealr25lo/trealr25tx)^2.))
        IF (tdec+sig*tdechi LT 1.) THEN numcc++ ELSE numncc++
    ENDFOR
    IF tctype EQ 'CC' THEN prob = numcc/(numcc+numncc) ELSE prob = numncc/(numcc+numncc)
    printf, log, format='(A-20,A15,A10,I10,I10,F10.2)',tname,tobs,tctype,numcc,numncc,prob
ENDFOR

close, log

END

