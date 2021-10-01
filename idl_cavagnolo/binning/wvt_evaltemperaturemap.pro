PRO WVT_EVALTEMPERATUREMAP, linenum, maskfile, mapname, binfile, evt2root

; Specify the extension that should get evaluated. If there is more
; than one line with data, specify the number of the line with
; "lineno".
lineno = linenum
binnumber = mrdfits(binfile+'_abinned_binnum.fits', 0)
mask = mrdfits(maskfile,0)
data = mrdfits(binfile+'_abinned_data.fits',1)
area = HISTOGRAM(data.class, REVERSE_INDICES=r ,min=0, max=n_elements(data.xnode)-1)

;# generate the bins for making a map
WVT_GENERATEBINNING, data.xnode, data.ynode, data.weight, binnumber, mask

; Loop through all bins, evaluate the spectral parameter for that bin
; and store it in temp
nbins = max(binnumber)

; get the best fit temperature
temp = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
    file = strcompress(evt2root+string(i)+'.temp', /remove_all)
    IF file_test(file) THEN readcol, file, a, skipline=lineno-1, numline=1, /silent, comment='#' ELSE a=-1. & temp[i] = a
ENDFOR

; get the low temp fit
tlo = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
    file = strcompress(evt2root+string(i)+'.temperr', /remove_all)
    IF file_test(file) THEN readcol, file, a, skipline=lineno-1, numline=1, /silent, comment='#' ELSE a=-1. & tlo[i] = a
ENDFOR

; get the high temp fit
thi = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
    file = strcompress(evt2root+string(i)+'.temperr', /remove_all)
    IF file_test(file) THEN readcol, file, a, skipline=lineno, numline=1, /silent, comment='#' ELSE a=-1. & thi[i] = a
ENDFOR

; get the best fit temperature
fe = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
    file = strcompress(evt2root+string(i)+'.fe', /remove_all)
    IF file_test(file) THEN readcol, file, a, skipline=lineno-1, numline=1, /silent, comment='#' ELSE a=-1. & fe[i] = a
ENDFOR

; get the low fe fit
felo = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
  file = strcompress(evt2root+string(i)+'.feerr', /remove_all)
  IF file_test(file) THEN readcol, file, a, skipline=lineno-1, numline=1, /silent, comment='#' ELSE a=-1. & felo[i] = a
ENDFOR

; get the high fe fit
fehi = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
  file = strcompress(evt2root+string(i)+'.feerr', /remove_all)
  IF file_test(file) THEN readcol, file, a, skipline=lineno, numline=1, /silent, comment='#' ELSE a=-1. & fehi[i] = a
ENDFOR

; get the norm squared value
norm = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
   file = strcompress(evt2root+string(i)+'.norm', /remove_all)
   IF file_test(file) THEN readcol, file, a, skipline=lineno-1, numline=1, /silent, comment='#' ELSE a=-1. & chisqr[i] = a
ENDFOR

; get the low norm fit
normlo = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
  file = strcompress(evt2root+string(i)+'.normerr', /remove_all)
  IF file_test(file) THEN readcol, file, a, skipline=lineno-1, numline=1, /silent, comment='#' ELSE a=-1. & normlo[i] = a
ENDFOR

; get the high norm fit
normhi = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
  file = strcompress(evt2root+string(i)+'.normerr', /remove_all)
  IF file_test(file) THEN readcol, file, a, skipline=lineno, numline=1, /silent, comment='#' ELSE a=-1. & normhi[i] = a
ENDFOR

; get the chi squared value
chisqr = dblarr(nbins+1)
FOR i=1L,nbins DO BEGIN
   file = strcompress(evt2root+string(i)+'.chi', /remove_all)
   IF file_test(file) THEN readcol, file, a, skipline=lineno-1, numline=1, /silent, comment='#' ELSE a=-1. & chisqr[i] = a
ENDFOR

; set the minimum and maximum temperatures if the stray below/above a
; given value
maxT = max(temp)
IF maxT GT 20.0 THEN maxT = 50.0
minT = min(temp)
IF minT LT 0.1 THEN minT = 0.05

; Get the header to preserve WCS information
c = mrdfits(binfile+'_abinned.fits',0,header)

; Produce the temperature map, binnumber starts at 1, therefore, tempmap[0]=0
tempmap = temp[binnumber]
tlomap = tlo[binnumber]
thimap = thi[binnumber]
femap = fe[binnumber]
felomap = felo[binnumber]
fehimap = fehi[binnumber]
normmap = norm[binnumber]
normlomap = normlo[binnumber]
normhimap = normhi[binnumber]
chisqmap = chisqr[binnumber]

; Save the temperature maps
outfile = mapname+'.fits'
file_delete, outfile, /quiet
mwrfits, tempmap, outfile, header
outfile = mapname+'_tlo.fits'
file_delete, outfile, /quiet
mwrfits, tlomap, outfile, header
outfile = mapname+'_thi.fits'
file_delete, outfile, /quiet
mwrfits, thimap, outfile, header

;# Save the abundance maps
outfile = mapname+'_fe.fits'
file_delete, outfile, /quiet
mwrfits, femap, outfile, header
outfile = mapname+'_felo.fits'
file_delete, outfile, /quiet
mwrfits, felomap, outfile, header
outfile = mapname+'_fehi.fits'
file_delete, outfile, /quiet
mwrfits, fehimap, outfile, header

;# Save the normalization maps
outfile = mapname+'_norm.fits'
file_delete, outfile, /quiet
mwrfits, normmap, outfile, header
outfile = mapname+'_normlo.fits'
file_delete, outfile, /quiet
mwrfits, normlomap, outfile, header
outfile = mapname+'_normhi.fits'
file_delete, outfile, /quiet
mwrfits, normhimap, outfile, header

;# save the stats map
outfile = mapname+'_chi.fits'
file_delete, outfile, /quiet
mwrfits, chisqmap, outfile, header

; Linearly interpolate between bin generators to get a smooth temperature map
triangulate,data.xnode,data.ynode,trbin, b
origdim = size(c,/dimensions)
dimx = origdim[0]
dimy = origdim[1]
bingrid = trigrid(data.xnode,data.ynode,data.binvalue,trbin, [0,0], [0,0,dimx,dimy], nx=dimx, ny=dimy, missing=0)
gridnat = GRIDDATA(data.xnode, data.ynode, temp[1:nbins]>minT, dimension=origdim, start=0,/natural_neighbor, triangle=trbin, delta=1., missing=minT)

; Save the interpolated temperature map. Looks cute, but shouldn't be used, as it could create artifacts
inter_outfile = mapname+'_interpol.fits'
file_delete, inter_outfile,/quiet
maxT = max(temp)
IF maxT GT 20.0 THEN maxT = 50.0
minT = min(temp)
IF minT LT 0.1 THEN minT = 0.05
mwrfits, gridnat* (bingrid GT 0)<maxT>minT, inter_outfile, header

END

