pro flux, imname,outfile,lumdist,rad,f1,exptime

Dl = lumdist	; luminosity distance (Mpc)
D = double (Dl * 3.0856E24) ; luminosity distance (cm)
lambda = 2318 ; central wavelength of filter used (angstroms)
parlum = double(!PI * 4 * D^2)
f0 = 2.05E-16
m0 = 18.12
freq = 3E8 / (lambda*1E-10)

	openw,1,outfile,/append
	printf,1,'; 1. Aperture Size (arcsec)'
	printf,1,'; 2. Total Count in Aperture'
	printf,1,'; 3. Total Count in Aperture Error'
	printf,1,'; 4. Total Enclosed Magnitude'
	printf,1,'; 5. Total Enclosed Magnitude Error'
	printf,1,'; 6. Flux (ergs/cm^2/s)'
	printf,1,'; 7. Flux Error '
	printf,1,'; 8. Luminosity (ergs/s)'
	printf,1,'; 9. Luminosity Error '
	printf,1,'; 10. SFR (Mo/yr)'
	printf,1,'; 11. SFR Error '
	printf,1
	close,1	

im = readfits (imname,h)
s = size(im, /dimensions)

	print,''
	print, 'Pointing at the Target...'
	read, 'Enter Target X: ', X
	read, 'Enter Target Y: ', Y

	getrot, h, rot, cdelt
	cdelt = cdelt * 3600
	cdelt = abs(cdelt)
	print,''
	print, 'The Resolution of the Image is: ', cdelt[0]
	print,''
	N = [s[0],s[1]]
	dist_circle, circ, N, X, Y	; defining the region of interest
	circ = circ * cdelt[0]
for i = 1, rad, 1 do begin
	good = where(circ lt i)
	subim = im[good]
	total = 0
	ggood = where(subim ge 0)
	subsubim = subim[ggood]
	total = total(subsubim)
	totalerr = sqrt(total*exptime)
	totalerr = totalerr / exptime
	error = totalerr/total
	flux = lambda * f1 * f0 * total
	fluxerr = flux * error
	apmag = - m0 - 2.5 * alog10(flux)
	apmagerr = apmag * error
	lum = parlum * flux
	lumerr = lum * error
	lumf = lum / freq
	sfr = 1.4E-28 * lumf
	sfrerr = sfr * error
	openw, 1, outfile, /append
	printf, 1,i,total,totalerr,apmag,apmagerr,flux,fluxerr,lum,lumerr,sfr,sfrerr
	close, 1
endfor


end