;----------------------------------------------------------------------

FUNCTION XY_OF_SKY, sky_x,sky_y,param	
  ; Version 1.1, 02/06/03		
  ; Transforms matrix indizes into sky coordinates as used for example in the
  ; evt2 files or ds9. The parameters depend on yor chip and/or on your way 
  ; of defining your images.
  ; the -1 is inserted because fitsfiles are starting from 1 instead of 0
  x=1/param.x_binning*(sky_x-param.x_offset)+param.x_del-1	
  y=1/param.y_binning*(sky_y-param.y_offset)+param.y_del-1	
  IF n_elements(sky_x) EQ 1 THEN $
    RETURN, [x,y] $
    ELSE RETURN, [transpose(reform(x)),transpose(reform(y))]
END

;----------------------------------------------------------------------

PRO WVT_EVTFILTER, evtfile, outfile, header, mask
  ;Version 1.1, updated 12/17/04
  ;Update 1.1: Updating the BACKSCAL keyword according to the fractional
  ;            area with regard to the whole field of view (8x1024)^2
  ;            This is necessary in order for XSPEC or SHERPA to work 
  ;            correctly. This has to be changed in the PI header of the 
  ;            spectrum, not in the evt2 file. Therefore, the standard
  ;            output is into the text file outfile.BACKSCAL

  IF n_elements(evtfile) LT 1 THEN evtfile='../Reduction/evt2_final.fits'
  IF n_elements(outfile) LT 1 THEN outfile='evt2_masked.fits'

  evt2=mrdfits(evtfile, 1, evt2header)

  ;Get all the header information for converting into pixels
  dim = sxpar(header ,'NAXIS*')      
  x_offset=sxpar(header, 'CRVAL1P')
  x_binning=sxpar(header, 'CDELT1P')
  x_del=sxpar(header, 'CRPIX1P')
  y_offset=sxpar(header, 'CRVAL2P')
  y_binning=sxpar(header, 'CDELT2P')
  y_del=sxpar(header, 'CRPIX2P')

  param={x_offset:x_offset, x_binning:x_binning, x_del:x_del, $
    y_offset:y_offset, y_binning:y_binning, y_del:y_del, dimension:dim}

  temp_matrix=dblarr(dim[0],dim[1])
  temp_matrix=xy_of_sky(evt2.x,evt2.y,param)
  evt2_xy=round(temp_matrix)

  temp=mask[reform(evt2_xy[0,*]), reform(evt2_xy[1,*])]
  index=where(temp)

  spawn, '/bin/rm -rf ' + outfile
  cd, current=cwd
  IF index[0] NE -1 THEN BEGIN
      print, 'Writing output file ',outfile,' in ',cwd
      mwrfits, evt2[index], outfile, evt2header
  ENDIF

  spawn, '/bin/rm -rf ' + outfile+'.BACKSCAL'
  IF index[0] NE -1 THEN BEGIN
    temp=where(mask, npix)
    openw, 10, outfile+'.BACKSCAL'
    printf, 10, double(npix)/(8.*1024.)^2
    close, 10
  ENDIF
END

;----------------------------------------------------------------------

PRO WVT_EVTSPLIT, evtfile, binnumber, header, root=root
  ; Version 1.0, updated 09/15/2005

  IF n_elements(root) EQ 0 THEN root=evtfile

  FOR i=1,max(binnumber) DO BEGIN 
    outfile=strcompress(root+string(i), /remove_all) 
    inmask=binnumber EQ i 
    WVT_EVTFILTER, evtfile, outfile, header, inmask 
  ENDFOR

END
