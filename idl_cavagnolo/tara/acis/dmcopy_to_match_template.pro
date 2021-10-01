;;; $Id: dmcopy_to_match_template.pro,v 1.2 2008-10-15 19:35:50 cavagnolo Exp $
;;; Use dmcopy to make an image from an event list so that the bin
;;; size & phase matches that of a template image.
;;; The template image would commonly be a PSF image.

PRO dmcopy_to_match_template, event_filename, template_filename, img_filename, $
			      XMIN=xmin, XMAX=xmax, YMIN=ymin, YMAX=ymax

spawn, 'punlearn dmcopy'

header = headfits(template_filename)

; Extract the keywords for the index to PHYSICAL coordinate conversion.
; We cannot use xy2ad.pro for such conversions to PHYSICAL (sky) system.
crvalP = [sxpar(header, 'CRVAL1P'), sxpar(header, 'CRVAL2P')]
crpixP = [sxpar(header, 'CRPIX1P'), sxpar(header, 'CRPIX2P')]
cdeltP = [sxpar(header, 'CDELT1P'), sxpar(header, 'CDELT2P')]

print, cdeltP, F='(%"template pixel size is %5.2f X %5.2f sky pixels")'

; Find the PHYSICAL coordinates of any corner of any template pixel.
; We arbitrarily choose the lower left corner of the image which is 
; (-0.5, -0.5) in 0-based index coordinates.
index_x = -0.5
index_y = -0.5
sky_x = crvalP[0] + cdeltP[0] * (index_x+1 - crpixP[0])
sky_y = crvalP[1] + cdeltP[1] * (index_y+1 - crpixP[1])


; Determine maximum range of the event data to keep.
if NOT (keyword_set(xmin) AND keyword_set(xmax) AND keyword_set(ymin) AND keyword_set(ymax)) then begin
  tb = mrdfits(event_filename, 1)
  if NOT keyword_set(xmin) then xmin = min(tb.x)
  if NOT keyword_set(xmax) then xmax = max(tb.x)
  if NOT keyword_set(ymin) then ymin = min(tb.y)
  if NOT keyword_set(ymax) then ymax = max(tb.y)
endif 

; Find smaller range which matches phase of template.
xmin = sky_x + cdeltP[0] *  ceil( (xmin - sky_x)/cdeltP[0] )
xmax = sky_x + cdeltP[0] * floor( (xmax - sky_x)/cdeltP[0] )
ymin = sky_y + cdeltP[1] *  ceil( (ymin - sky_y)/cdeltP[1] )
ymax = sky_y + cdeltP[1] * floor( (ymax - sky_y)/cdeltP[1] )

; Determine dimensions of image.
xdim = round( (xmax-xmin)/cdeltP[0] )
ydim = round( (ymax-ymin)/cdeltP[1] )

; Spawn dmcopy,
cmd = string(event_filename, xmin,xmax,xdim, ymin,ymax,ydim, img_filename, $
             F="(%'dmcopy ""%s[bin x=%9.4f:%9.4f:#%d,y=%9.4f:%9.4f:#%d]"" %s clobber=yes')")
print, cmd
spawn, cmd

return
end
