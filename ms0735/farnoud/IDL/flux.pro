pro flux, imname,immsk,lumdist,rad

Dl = lumdist	; luminosity distance (Mpc)
D = double (Dl * 3.0856E24) ; luminosity distance (cm)
;lambda = 21500 ; central wavelength of filter used (angstroms)
parlum = double(!PI * 4 * D^2)
re = rad

m0 = 20.08
f0 = 2.965E-8
lambda = 2320

;if (band eq 'f') then begin
;m0 = 18.82
;f0 = 9.289E-9
;lambda = 1540
;goto, START
;endif

START:
im = readfits (imname,h)
s = size(im, /dimensions)

	readcol,immsk, xl,yl,xh,yh
	l = size(xh,/dimensions)
	l = l[0]
	for n = 0,(l-1),1 do begin
		for x = xl[n],xh[n],1 do begin
			for y = yl[n],yh[n],1 do begin
				im[x,y] = -888
			endfor
		endfor
	endfor

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
	good = where(circ lt re)
	subim = im[good]
	ss = size(subim,/dimensions)
	total = 0
	ggood = where(subim ge 0)
	subsubim = subim[ggood]
	total = total(subsubim)
	apmag = m0 - 2.5 * alog10(total)
	flux = lambda * f0 * 10^(-apmag * 0.4)
	lum = parlum * flux
	absmag = -(5*alog10(Dl*1E6)-5-apmag)

print, apmag, lum, absmag

end