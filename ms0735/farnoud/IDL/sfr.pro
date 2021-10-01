pro sfr,imname,rad

if (N_params() EQ 0) then begin
	print, 'Syntax - sfr, imagename, radius  '
	return
endif

Dl = 1068.3	; luminosity distance (Mpc)
D = double (Dl * 3.0856E24) ; luminosity distance (cm)
parlum = double(!PI * 4 * D^2)
lambda = 2120	; Angstroms
f0m0 = 1.1406
f0 = 5.71E-15	; ergs/s/cm^2/A/cps
exptime = 19997
freq = 3E8 / (lambda*1E-10)

	print,''
	outfile= ' '
	read, 'Enter Output Filename: ', outfile
	openw,1,outfile,/append
	printf,1,'; 1. Radius (")'
	printf,1,'; 2. Integrated Count Rate'
	printf,1,'; 3. Count Rate Error'
	printf,1,'; 4. Flux (erg/s/cm^2)'
	printf,1,'; 5. Flux Error'
	printf,1,'; 6. Luminosity (erg/s)'
	printf,1,'; 7. Luminosity Error'
	printf,1,'; 8. SFR (Msun / year)'	
	printf,1,'; 9. SFR Error'	
	printf,1
	close,1	

im = readfits (imname,h)
s = size(im, /dimensions)
N = [s[0],s[1]]

;readcol,immsk, xl,yl,xh,yh
;l = size(xh,/dimensions)
;l = l[0]
;for m = 0,(l-1),1 do begin
;	for x = xl[m],xh[m],1 do begin
;		for y = yl[m],yh[m],1 do begin
;			im[x,y] = -888
;		endfor
;	endfor
;endfor

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

dist_circle, circ, N, X, Y	
circ = circ * cdelt[0]

for i = 1, 6, 1 do begin
	ap = rad * i
	good = where(circ lt ap)
	subim = im[good]
	ss = size(subim,/dimensions)
	total = 0
	ggood = where(subim ge 0)
	subsubim = subim[ggood]
	total = total(subsubim)
	tfore = total * exptime
	totalerr = sqrt(tfore)
	totalerr = totalerr / exptime
	error = totalerr/total
	flux = total * f0 * f0m0 * lambda
	fluxerr = flux * error
	lum = parlum * flux
	lumerr = lum * error
	lumf = lum / freq
	sfr = 1.4E-28 * lumf
	sfrerr = sfr * error
	print, 'R: ',ap,' SFR: ',sfr
	print,''
	openw, 1, outfile, /append
	printf, 1,ap,total,totalerr,flux,fluxerr,lum,lumerr,sfr,sfrerr
	close, 1
endfor

end

