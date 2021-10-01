pro ellaphot, imname
;**************************************************************************
; This program performs user defined aperture photometry, the aperture size  is provided by the user and the results will be published.
; By: FK, 2007
;**************************************************************************
if (N_params() EQ 0) then begin
	print, 'Syntax - ellaphot, imagename'
	return
endif

; INPUTS: These variable differ depending on the object observed and the telescope use! 
	Dl = 1068.29E6	; luminosity distance (pc)
	D = double (Dl * 3.0856E16 * 100) ; luminosity distance (cm)
	photflam = 1.5074E-19 ; ergs/cm^2/wavelength/electron 
	lambda = 9054.768 ; central wavelength of filter used (angstroms)
	parlum = double(!PI * 4 * D^2)

; Read in the image and it's header attachement, and find the size of it
	im = readfits (imname,h)
	s = size(im, /dimensions)

; User defined output file name, and priming the output file
	outfile= ' '
	read, 'Enter Output Filename: ', outfile
	print,''
	openw,1,outfile,/append
	printf,1,'; 1. X Center'
	printf,1,'; 2. Y Center'
;	printf,1,'; 3. Major to Minor Axis Ratio'
;	printf,1,'; 4. Position Angle'
	printf,1,'; 5. Aperture Size [SMA] (")'
	printf,1,'; 6. Number of Pixels'
	printf,1,'; 7. Mean'
	printf,1,'; 8. Median'
	printf,1,'; 9. Mode'	
	printf,1,'; 10. Standard Deviation'
	printf,1,'; 11. Total Number of Count (electrons)'
	printf,1,'; 12. Area (arcsecond^2)
	printf,1,'; 13. Flux (ergs/cm^2/s)'
	printf,1,'; 14. Luminosity (ergs/s)'
	printf,1
	close,1	

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Ask for radius of aperture in arcseconds and defines the region to be examined
	getrot, h, rot, cdelt
	cdelt = cdelt * 3600
	cdelt = abs(cdelt)
	print, 'The Resolution of the Image is: ', cdelt[0]
	print,''
	N = [s[0],s[1]]
START:
	print,''
	print, 'Pointing at the Target...'
	read, 'Enter Target X: ', X
	read, 'Enter Target Y: ', Y
	print,''
	print, 'Properties of Target Region...'
	read, 'Enter Target Position Angle: ', PA
	read, 'Enter Ratio of Major to Minor Axis: ', R
	print, ''
	print, 'Region to be Examined...'
	read, 'Enter Aperture Size in Arcseconds (SMA): ', ap
; defining the region of interest
	dist_ellipse, ell, N, X, Y, R, PA	
	ell = ell * cdelt[0]
	good = where(ell lt ap)
	subim = im[good]
; Finding the 'Tau' pixels
	tau = where (subim gt 0)
	imtau = subim[tau]

; Defined photometry is done here and is printed to the output file
	image_statistics, imtau, count=count, mean=mean, stddev=std
	median = median(imtau)
	void = max(histogram(imtau,omin=mn),mxpos)
	mode = mn +mxpos
	total = total(imtau)
	area = !PI * ap * (ap/R)	;Arcsec^2
	flux = total * photflam * lambda
	lum = parlum * flux
	print,''
	print, 'SMA: ',ap, 'Median: ',median, 'Total: ',total, 'Flux: ', flux, 'Luminosity: ',lum
	openw, 1, outfile, /append
	printf, 1, X,Y,R,PA,ap,count,mean,median,mode,std,total,area,flux,lum
	close, 1

	cont=' '
	read,'Change Aperture Size? [y-More Apertures / Enter-Exit] ', cont
	if (cont eq 'y') then goto, START

end
