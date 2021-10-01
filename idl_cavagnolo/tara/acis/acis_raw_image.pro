PRO Acis_Raw_Image, infile, ccdid, exposureid

; Range checking
while (ccdid LT 0 OR ccdid GT 10) do begin
	READ,"Enter a CCD ID (0-9):  ", ccdid
endwhile

while (exposureid LT 0) do begin
	READ,"Enter an exposure number (> 0):  ", exposureid
endwhile

; Read the primary header of the input file
hh = HeadFits(infile)
ss = size(hh)
if (ss(0) NE 1 OR ss(2) NE 7) then begin
	print, "Error reading input file.  Is it a FITS file?"
	print, "Exiting..."
	RETURN
endif

datamode = strtrim( SxPar(hh, "DATAMODE") )

if(datamode EQ 'TE_RAW') then begin
	; Extract some keywords
	startrow = SxPar(hh, "STARTROW")
	if (startrow LT 0 OR startrow GT 1023) then begin
		print, "Invalid value for keyword STARTROW:  ", startrow
		print, "Exiting..."
		RETURN
	endif
	print, "Zero-based index of the first row is:  ", startrow

	rowcount = SxPar(hh, "ROWCOUNT") + 1
	if (rowcount LT 1 OR rowcount GT 1024) then begin
		print, "Invalid value for keyword ROWCOUNT:  ", rowcount
		print, "Exiting..."
		RETURN
	endif
endif else if (datamode EQ 'CC_RAW') then begin
	; CC Raw data is sent in batches of 512 row images
	startrow = 0
	rowcount = 512
endif else begin
	print, "Invalid mode.  No output produced"
	return
endelse

windowing = SxPar(hh, "WINDOWID")
if(windowing NE '0xFFFF  ') then begin
	print, "This data is windowed."
	print, "The reconstructed image is probably garbled."
endif
                                                           ;Data clocked from
ormode = SxPar(hh, "ORMODE")                               ;   Amplifier(s)
if(ormode EQ 0) then begin                                 ;    ALL
	namps = 4                                          ;
endif else if (ormode EQ 1) then begin                     ;Diagnostic Mode
	print, "Output registers are in diagnostic mode."  ;Clock pixels AWAY
	namps = 4                                          ;FROM all 4 amps
endif else if (ormode EQ 2 OR ormode EQ 3) then begin      ;   AC or BC
	namps = 2                                          ;
endif else begin                                           ; Invalid Mode
	print, "Invalid output register mode:  ", ormode
	print, "Exiting..."
	RETURN
endelse

oclock_pairs = SxPar(hh, "OVERPAIR")

; We may have to do something with this variable.  We'll see.
summing = SxPar(hh, '2X2SUMNG')

; Now calculate the size of each image
ncols = 1024 + (namps * oclock_pairs * 2)
nrows = rowcount
print, "Expecting ", nrows, " rows..."
print, "Expecting ", ncols, " columns..."

; Now find the image
get_lun,unit
extname = 'CCD' + strtrim(ccdid,2) + '_DATA'
err = ''
fxbopen,unit,infile,extname,hh
if( err NE '') then begin
	print, "Error finding FITS extension named ", extname
	print, "Exiting..."
	RETURN
endif

fxbfind,unit, "TTYPE", colnums, colnames, nfound

phacol = (colnums( where (colnames EQ 'PHAS    ') ))[0]
if(phacol LT 0) then begin
	print, "Error finding the PHAS column."
	print, "Exiting..."
	RETURN
endif

expcol = colnums( where (colnames EQ 'EXPOSURE') )
if(expcol(0) LT 0) then begin
	print, "Error finding the EXPOSURE column."
	print, "Exiting..."
	RETURN
endif

; Find the index of the correct exposure
nexp= fxbdimen( unit, expcol(0))
exposures = lonarr(nexp(0))
fxbread, unit, exposures, expcol(0)
fitsrow = (where( exposures EQ exposureid ))[0] + 1
if(fitsrow EQ -1) then begin
	print, "Error finding exposure number:  ", exposureid
	print, "Exiting..."
	RETURN
endif

; Finally, extract the image
npix = (fxbdimen( unit, phacol))[0]
if (npix NE nrows * ncols) then begin
	print, "WARNING! Image size mismatch found."
	
	nrows = fix( npix / ncols )
	if (npix NE nrows * ncols) then begin
	  print, npix, " pixels found, which is not an integer number of rows.'
	  print, "Exiting..."
	  RETURN
	endif else begin
	  print, 'Only ', nrows, ' rows worth of data were found.'
	endelse
endif

image = intarr(npix)
fxbread, unit, image, phacol, fitsrow, ERRMSG=err
if (err NE '') then begin
	print, "Error reading raw image from FITS file (row,col):  ", $
	       phacol, fitsrow
	print, "Exiting..."
	RETURN
endif

fxbclose,unit
free_lun,unit

image = reform( image, ncols, nrows )

; Write the image
filename = "ccd" + strtrim(ccdid,2) +  $
	   "_exposure" + strtrim(exposureid,2) + ".fits"
WriteFits, filename, image


RETURN
END
