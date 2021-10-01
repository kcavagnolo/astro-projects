PRO lum14, z, flux, freq

ON_ERROR, 2
IF n_params() LT 3. THEN BEGIN
   print, 'Syntax - LUM14, z, flux, freq'
   print, 'z -- dimensionless redshift'
   print, 'flux -- flux in Janskys'
   print, 'freq -- frequency in Hz'
   print, 'prints the power at frequency'
   RETURN
ENDIF
cmMpc = 3.08d24
Jy = 1.0d-23
alpha = 0.8
cosmology, z, result, /silent
dl = result[2]
pow = 4 * !PI * (dl * cmMpc)^2 * flux * Jy * freq * (1.0+z)^(alpha-1)

print, format='(E10.3,A20)',pow,'ergs/sec'

END
