pro circaphot, imname

if (N_params() EQ 0) then begin
   print, 'Syntax - circaphot, imagename'
   return
endif

;# read img type
id = 'nan'
read, 'Enter HST filter: ', id
outfile = 'text'
read, 'Enter output name: ', outfile
read, 'Enter redshift: ', z
IF id EQ '606' THEN BEGIN
   photflam = 7.8625d-20        ;# ergs/cm^2/Ang/electron 
   lambda = 5907.               ;# Angstrom
ENDIF ELSE IF id EQ '814' THEN BEGIN
   photflam = 7.0723600d-20
   lambda = 8332.
ENDIF

;# cosmo
c = 2.99792458d18               ;# speed of light in Ang/s
cosmology, z, result, /silent
dl = result[2]*3.08568025d24    ;# luminosity distance (cm)

;# Read image
im = readfits (imname,h)
s = size(im, /dimensions)
getrot, h, rot, cdelt
cdelt = cdelt * 3600
cdelt = abs(cdelt)
print, 'The Resolution of the Image is: ', cdelt[0]
N = [s[0],s[1]]

;# User defined output file name, and priming the output file
openw, /get_lun, OUTLUN, outfile, /append
printf, OUTLUN,format='(A-15,14A15)', ';ID','X','Y','Aperture','Pixels','Mean','Median','Mode','StdDev','Counts','Area','Flux','err','Lumin','err'
printf, OUTLUN,format='(A-15,14A15)', ';-','img','img','arcsec','pix','cts','cts','cts','cts','e-','arcsec2','e/cm2/s','e/cm2/s','e/s','e/s'

;# User input, target RA and DEC, and X and Y pixel values
START:
read, 'Enter Target X: ', X
read, 'Enter Target Y: ', Y
read, 'Enter Aperture Size in Arcseconds: ', ap
dist_circle, circ, N, X, Y
circ = circ * cdelt[0]
good = where(circ lt ap)
subim = im[good]

;# Defined photometry is done here and is printed to the output file
image_statistics, subim, count=count, mean=mean, stddev=std
median = median(subim)
void = max(histogram(subim,omin=mn),mxpos)
mode = mn +mxpos
total = total(subim)
area = !PI * ap * ap
flux = total * photflam * lambda
ferr = flux*(std/total)
lum = 4.0*!PI*dl^2*flux
lerr = lum*(ferr/flux)
mag = -2.5*alog10(total * photflam * lambda^2./c)-48.60
print, 'Flux: ', flux, ' +/- ', ferr
print, 'Lumin: ', lum, ' +/- ', lerr
print, 'm_app: ', mag
printf, OUTLUN,format='(A-15,10F15.4,4E15.4)',id,x,y,ap,count,mean,median,mode,std,total,area,flux,ferr,lum,lerr
cont = ' '
read,'Enter to continue; "x" to exit', cont
if (cont ne 'x') then goto, START
close, OUTLUN

end
