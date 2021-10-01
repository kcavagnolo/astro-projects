PRO catslx, in1, output

Jy     = 1.0d-23                ;# 1Jy = 10**-23 ergs sec**-1 cm**-2 Hz**-1
cmMpc  = 3.08568025d24          ;# 1 Mpc = 3.08x10**24 cm
alpha = 0.8                     ;# canonical value of alpha to use for integration

;# Read in file
readcol, in1, format='A,I,A,A,F,F,F,F,F,F,A,F,A,A', $
         name, obsid, ra, dec, sep, major, minor, theta, flux, err, type, z, freq, notes, alldl
flux = flux/1000.
err = err/1000.

;# write to a file
openw, /get_lun, LUN, output
printf, LUN, format='(A-25,A14,A6,A14,6A16,A10)','#Name','Obsid','type','dl','flux','err','power','err','lum','err','surv'
printf, LUN, format='(A-25,A14,A6,A14,6A16,A10)','# --', '--','--','Mpc','Jy','Jy','10^40e/s','10^40e/s','10^40e/s','10^40e/s','--'

;# start loop
prevname = 'fsdfsdsdf'
tflux = 0.0
tferr = 0.0
tlum = 0.0
tlerr = 0.0
tpow = 0.0
tperr = 0.0
FOR i=0,n_elements(obsid)-1 DO BEGIN

   ;# sum or print
   IF ((i NE 0) AND (name[i] NE prevname)) THEN BEGIN
      printf, LUN, format='(A-25,A14,A6,F14.6,6F16.7,A10)',$
              name[i-1], obsid[i-1], type[i-1], alldl[i-1], tflux, tferr, tpow, tperr, tlum, tlerr, 'CATS'
      tflux = 0.0
      tferr = 0.0
      tlum = 0.0
      tlerr = 0.0
      tpow = 0.0
      tperr = 0.0
   ENDIF

   ;# get power law index
   dl = alldl[i]
   nuval = 0.0
   nu0 = freq[i]*1d6
   IF (flux[i] GT 0) THEN BEGIN
      lum = 4 * !PI * (dl * cmMpc)^2 * flux[i] * Jy * nuval
      lum = lum/1d40
      lumerr = 4 * !PI * (dl * cmMpc)^2 * err[i] * Jy * nuval
      lumerr = lumerr/1d40
      pow = 4 * !PI * (dl * cmMpc)^2 * flux[i] * Jy * nu0 * (1.0+z[i])^(alpha-1)
      pow = pow/1d40
      powerr = 4 * !PI * (dl * cmMpc)^2 * err[i] * Jy * nu0 * (1.0+z[i])^(alpha-1)
      powerr = powerr/1d40
   ENDIF ELSE BEGIN
      lum = -0.00
      lumerr = lum
      pow = lum
      powerr = lum
   ENDELSE

   ;# sum same objects
   tflux = tflux + flux[i]
   tferr = tferr + err[i]
   tlum = tlum + lum
   tlerr = tlerr + lumerr
   tpow = tpow + pow
   tperr = tperr + powerr

   ;# set name
   prevname = name[i]

ENDFOR

;# print last entry and close-out
printf, LUN, format='(A-25,A14,A6,F14.6,6F16.7,A10)',$
        name[i-1], obsid[i-1], type[i-1], alldl[i-1], tflux, tferr, tpow, tperr, tlum, tlerr, 'CATS'
FREE_LUN, LUN
END
