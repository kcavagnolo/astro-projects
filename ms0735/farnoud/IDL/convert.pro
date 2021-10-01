pro convert

lambda = 2.159
Dl = 18.736	
f0 = 2.283E-7
;freq = 3E8/(lambda*1E-10)
D = double (Dl * 3.0856E24)
parlum = double (!PI * 4 * D^2)
tab_read, '87k.tab', tcb, table, header
rad = tab_val(tcb,table,1)
rad = rad*1
s = size(rad, /dimensions)
tmag = tab_val(tcb,table,24)

	openw,1,'87klight',/append
	printf,1,'; 1. Aperture Size (arcsec)'
	printf,1,'; 2. Total Enclosed Magnitude'
	printf,1,'; 3. Flux (ergs/cm^2/s)'
	printf,1,'; 4. Luminosity (ergs/s)'
;	printf,1,'; 5. SFR (Msun/yr)'
	printf,1
	close,1	

for i = 0, (s[0]-1) do begin
	flux = f0*lambda * 10^(-tmag[i]*0.4)
	lum = parlum * flux
;	lumf = lum / freq
;	sfr = 1.4E-28 * lumf
	openw, 1,'87klight',/append
	printf, 1, rad[i], tmag[i], flux, lum;, sfr
	close,1
endfor

end