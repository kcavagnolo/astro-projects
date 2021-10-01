pro calc_esb_image,imgfile,r1,r2,sb,esb,area,expfile=expfile,           $
                   outfile=outfile,xcen=xcen,ycen=ycen,xybin=xybin,     $
		   axis_ratio=axis_ratio, tilt_deg=tilt_deg,            $
                   maskfile=maskfile
;-----------------------------------------------------------------------
; Name: CALC_ESB_IMAGE
;
; Purpose: Calculates the surface brightness profile for 
;          a specified FITS image in elliptical annuli.
;          
; Inputs: imgfile -- Raw counts image
;         expfile -- Exposure map 
;         
; Comments: By default, the routine measures the radial surface
;           brightness from the center of the image out to the edge.
;	    If an exposure map is specified, the surface brightness
;	    is in phot/cm^2/s/pix. If no exposure is specified, the
;	    surface brightness is phot/s/pix.
;           
; Revision history:
;       written by Michael Wise			11-13-99
;	added exposure map code			11-17-99
;	modified error calculation		11-18-99
;	fixed problem with no expmap case	12-02-99
;       fixed area bug for exposure map case	08-28-00
;	added exposure keyword code		10-15-00
;	modified by J. Houck to handle		03-01-01
;	elliptical annuli
;	added MASKFILE parameter		03-15-01
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 6)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'calc_ell_sb_image,imgfile,r1,r2,sb,esb,area,expfile=expfile,',       $
   '                  outfile=outfile,xcen=xcen,ycen=ycen,xybin=xybin,', $
   '                  axis_ratio=axis_ratio,tilt_deg=tilt_deg,',         $
   '                  maskfile=maskfile'
   return   
endif

;
; Handle defaults
;
if (n_elements(xybin) eq 0) then xybin=1
if (n_elements(axis_ratio) eq 0) then axis_ratio=1.0
if (n_elements(tilt_deg) eq 0) then tilt_deg=0.0


;
; Read in the image
;
im=readfits(imgfile,hd)


;
; Get pixel scale parameters out of the header
;
CRVAL1P=sxpar(hd,'CRVAL1P','Parameter CRVAL1P not found')
CRPIX1P=sxpar(hd,'CRPIX1P','Parameter CRPIX1P not found')
CDELT1P=sxpar(hd,'CDELT1P','Parameter CDELT1P not found')
CRVAL2P=sxpar(hd,'CRVAL2P','Parameter CRVAL2P not found')
CRPIX2P=sxpar(hd,'CRPIX2P','Parameter CRPIX2P not found')
CDELT2P=sxpar(hd,'CDELT2P','Parameter CDELT2P not found')


;
; A little checking
;
s=size(im)
xsize=s(1)
ysize=s(2)
if (s(0) ne 2) then begin
   print,string(7B),'ERROR: Image must be 2 dimensional'
   return
endif


;
; If provided, read in the exposure map
;
if (n_elements(expfile) ne 0) then begin
   em=readfits(expfile)
;
;  Check to make sure two images are commiserate
;
   se=size(em)
   if (total(s(0:2)-se(0:2)) ne 0.0) then begin
      print,string(7B),'ERROR: Exposure map must match input image'
      return   
   endif
;
;  Convert into a bit map
;
   bmap=em
   bmap(where(bmap ne 0.0))=1.0
endif else begin
   bmap=fltarr(xsize,ysize)
   bmap(*)=1.0
endelse


;
; Define center if necessary
;
if (n_elements(xcen) eq 0) then xcen=xsize/2
if (n_elements(ycen) eq 0) then ycen=xsize/2


;
; Calculate the Sky X and Y of the image center
;
xc=CRVAL1P+CRPIX1P+CDELT1P*xcen
yc=CRVAL2P+CRPIX2P+CDELT2P*ycen


;
; Define the EXPOSURE time value
;
if (n_elements(expfile) ne 0) then begin
    spawn,'fitsdump -H '+expfile+' | grep EXPOSURE',result
endif else begin
    spawn,'fitsdump -H '+imgfile+' | grep EXPOSURE',result
endelse
exptime=sxpar(result,'EXPOSURE')


;
; Create distance mask
;
dist_ellipse,mask, [xsize, ysize] ,xcen, ycen, axis_ratio, tilt_deg


;
; Multiply by exposure mask
;
mask=bmap*mask
mask(xcen,ycen)=0.01


;
; If indicated, write out MASK image
;
if (n_elements(maskfile) ne 0) then begin
   writefits,maskfile,mask
endif


;
; Determine number of radial bins and define some arrays
;
nrad=fix(max(mask)/xybin)
r1=fltarr(nrad)
r2=fltarr(nrad)
sb=fltarr(nrad)
esb=fltarr(nrad)
area=fltarr(nrad)


;
; Loop over bins and calculate the surface brightness
;
for i=0,nrad-1 do begin

    r1(i)=i*xybin
    r2(i)=(i+1)*xybin
    if (i gt 0) then j=where( (mask gt r1(i)) and (mask le r2(i)) ) $
                else j=where( (mask gt 0.0) and (mask le r2(i)) )
    area(i)=n_elements(j)
    totcnts=total(float(im(j)))

    if (n_elements(expfile) ne 0) then begin
       totexp=total(em(j))
       sb(i)=(totcnts/totexp)/exptime
       esb(i)=(sqrt(totcnts)/totexp)/exptime
    endif else begin
       sb(i)=(totcnts/area(i))/exptime
       esb(i)=(sqrt(totcnts)/area(i))/exptime
    endelse

endfor


;
; Write the output file
;
if (n_elements(outfile) ne 0) then begin
   space='         '
   fmt='$(2(3x,f9.3),3(3x,e13.5))'
   fmt2='$(2(3x,a9),3(3x,a13))'
   get_lun,unit
   openw,unit,outfile
   printf,unit,' '
   printf,unit,'Extracted radial surface brightness profile'
   printf,unit,' '
   printf,unit,'Date       : ',!stime
   printf,unit,'Input file : ',imgfile
   if (n_elements(expfile) ne 0) then begin
      printf,unit,'Exp. file  : ',expfile
   endif else begin
      printf,unit,'Exp. file  : NONE'   
   endelse
   printf,unit,'Exp. time  : ',exptime   
   printf,unit,'Sky X      : ',xc
   printf,unit,'Sky Y      : ',yc
   printf,unit,'X Centroid : ',xcen
   printf,unit,'Y Centroid : ',ycen
   printf,unit,'Binsize    : ',xybin
   printf,unit,'Axis Ratio : ',axis_ratio
   printf,unit,'Angle      : ',tilt_deg
   printf,unit,' '
   printf,unit,fmt2,' Radius 1',' Radius 2','      SB     ', $
                    '    Error    ','     AREA    '
   for i=0,nrad-1 do begin
       printf,unit,fmt,r1(i),r2(i),sb(i),esb(i),area(i)
   endfor
   close,unit
   free_lun,unit
endif


;
; Return to IDL
;
return
end
