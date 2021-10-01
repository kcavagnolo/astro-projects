pro circaphot, imname
;**************************************************************************
; This program performs user defined aperture photometry, the aperture size  is provided by the user and the results will be published.
; By: FK, 2007
;**************************************************************************

if (N_params() EQ 0) then begin
   print, 'Syntax - circaphot, imagename'
   return
endif

; INPUTS: These variable differ depending on the object observed and the telescope used! 
cosmology, 0.354, result, /silent
Dl = result[2]
D = double (Dl * 3.08568025d24)           ; luminosity distance (cm)
photflam = 7.8625d-20                     ; ergs/cm^2/wavelength/electron 
lambda = 5907.
parlum = double(!PI * 4 * D^2)

; Read in the image and it's header attachement, and find the size of it
im = readfits (imname,h)
s = size(im, /dimensions)

; User defined output file name, and priming the output file
print,''
outfile= ' '
read, 'Enter Output Filename: ', outfile
openw,1,outfile,/append
printf,1,'; 1. Aperture Size (arcsec)'
printf,1,'; 2. Number of Pixels'
printf,1,'; 3. Mean'
printf,1,'; 4. Median'
printf,1,'; 5. Mode'	
printf,1,'; 6. Standard Deviation'
printf,1,'; 7. Total Number of Count (electrons)'
printf,1,'; 8. Area (arcsecond^2)'
printf,1,'; 9. Flux (ergs/cm^2/s)'
printf,1,'; 10. Luminosity (ergs/s)'
printf,1
close,1	

; User input, target RA and DEC, and X and Y pixel values
print,''
print, 'Pointing at the Target...'
read, 'Enter Target X: ', X
read, 'Enter Target Y: ', Y

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Ask for radius of aperture in arcseconds and defines the region to be examined
getrot, h, rot, cdelt
cdelt = cdelt * 3600
cdelt = abs(cdelt)
print,''
print, 'The Resolution of the Image is: ', cdelt[0]
print,''
N = [s[0],s[1]]
dist_circle, circ, N, X, Y	; defining the region of interest
circ = circ * cdelt[0]
START:
print,''
read, 'Enter Aperture Size in Arcseconds: ', ap
print,''
good = where(circ lt ap)
subim = im[good]

; Defined photometry is done here and is printed to the output file
image_statistics, subim, count=count, mean=mean, stddev=std
median = median(subim)
void = max(histogram(subim,omin=mn),mxpos)
mode = mn +mxpos
total = total(subim)
area = !PI * ap * ap            ;Arcsec^2
flux = total * photflam * lambda
lum = parlum * flux
lumstar = lum / 3.84E33
print, 'R: ',ap,'  Median: ',median,'  Total: ',total
print, 'Flux: ', flux,'  Luminosity: ',lum
print,''
openw, 1, outfile, /append
printf, 1,ap,count,mean,median,mode,std,total,area,flux,lum,lumstar
close, 1
cont=' '
read,'Change Aperture Size? [y-More Apertures / Enter-Exit] ', cont
if (cont eq 'y') then goto, START

end
