pro tau, imname
;**************************************************************************
; This program performs user defined aperture photometry, the aperture size  is provided by the user and the results will be published.
; By: FK, 2007
;**************************************************************************
if (N_params() EQ 0) then begin
	print, 'Syntax - tau, imagename'
	return
endif

; Read in the image and it's header attachement, and find the size of it
	im = readfits (imname,h)
	s = size(im, /dimensions)

; User defined output file name, and priming the output file
	outfile= ' '
	read, 'Enter Output Filename: ', outfile
	print,''
	openw,1,outfile,/append
	printf,1,'; 1. Region'
	printf,1,'; 2. X Center'
	printf,1,'; 3. Y Center'
	printf,1,'; 4. Major to Minor Axis Ratio'
	printf,1,'; 5. Position Angle'
	printf,1,'; 6. Aperture Size [SMA] (")'
	printf,1,'; 7. Number of Pixels'
	printf,1,'; 8. Area of Region'
	printf,1,'; 9. Average Tau'
	printf,1,'; 10. Average Tau Error'
	printf,1,'; 11. Amount of H Atoms'
	printf,1,'; 12. Amount of H Atoms Error'
	printf,1,'; 13. Total Gas Mass (Msun)'
	printf,1,'; 14. Total Gas Mass Error (Msun)'
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
	image_statistics, subim,stddev=std
	print,'sigma: ', std
; Finding the 'Tau' pixels
	tau = where (subim gt 0)
	imtau = subim[tau]
;	sigma = std
;	sigma = 2*std
	sigma = 3*std
	cuttau = where (imtau gt sigma)
	px = size(cuttau,/dimensions)
	if (px eq 0) then begin
		print, 'The cut is too large! Try another region.'
		goto, START
	endif
	imcuttau = imtau[cuttau]

; Defined photometry is done here and is printed to the output file
	image_statistics, imcuttau, count=count, mean=cutmean, stddev=cutstd
	area = double(4.21E20 * count)
	area = double(area * 4.21E20)
	error = cutstd/cutmean
	cutmeanerr = error * cutmean
	NH = double(2.057E21 * cutmean)
	NHerr = double(NH * error)
	MH = double(1.6735E-27 * NH)
	TMH = double(area * MH)
	TMHsun = double(TMH / 1.98E30)
	TMHsunerr = double(TMHsun * error)
	print,''
	print, 'SMA: ',ap, ', Mean: ',cutmean, ', TMHsun: ',TMHsun
	openw, 1, outfile, /append
	printf, 1, X,Y,R,PA,ap,count, area,cutmean,cutmeanerr,NH,NHerr,TMHsun,TMHsunerr
	close, 1

	cont=' '
	read,'Change Aperture Size? [y-More Apertures / Enter-Exit] ', cont
	if (cont eq 'y') then goto, START

end
