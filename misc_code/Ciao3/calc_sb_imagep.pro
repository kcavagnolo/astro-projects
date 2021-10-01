pro calc_sb_imagep,imgfile,r1,r2,sb,esb,area,expfile=expfile,       $
                   outfile=outfile,xc=xc,yc=yc,xybin=xybin,ellip=ellip,pa=pa,$
                   regfile=regfile,ccdregfile=ccdregfile,ciao_path=ciao_path
;-----------------------------------------------------------------------
; Name: CALC_SB_IMAGEP
;
; Purpose: Calculates the surface brightness profile for 
;          a specified FITS image
;          
; Inputs: imgfile -- Raw counts image
;         expfile -- Exposure map (made using normalize=no).  This file is
;	 	     also used as a mask, so you may exclude regions 
;		     (such as point sources) by removing the regions from
;		     the exposure map.
;         
; Comments: By default the routine measures the radial surface
;           brightness from the center of the image out to the edge.
;	    The surface brightness is output in both [phot/cm^2/s/pix]
;	    and in [phot/s/pix]. NOTE: The exposure map is now required!
;           
; Revision history:
;       written by Michael Wise			11-13-99
;	added exposure map code			11-17-99
;	modified error calculation		11-18-99
;	fixed problem with no expmap case	12-02-99
;       fixed area bug for exposure map case	08-28-00
;	added exposure keyword code		10-15-00
;	changed xcen and ycen to xc and yc	03-06-03
;		in physical coordinates (DR)
;       changed exposure map code (DR)          11-18-04
;	added elliptical code (DR)		11-19-04
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 6)) then begin
    print,string(7B),'CALLING SEQUENCE: ', $
      'calc_sb_imagep,imgfile,r1,r2,sb,esb,area,expfile=expfile,', $
      '              outfile=outfile,xc=xc,yc=yc,xybin=xybin'
    return   
endif

;
; Handle defaults
;
if (n_elements(xybin) eq 0) then xybin=1
if (n_elements(pa) eq 0) then pa=0.0 else pa=2.0*!PI*pa/360.0
if (n_elements(ellip) eq 0) then ellip=0.0
if (n_elements(ciao_path) eq 0) then cmdroot='source /export/home/Rafferty/Applications/ciao_3.2/bin/ciao.csh ; ' else cmdroot='source '+ciao_path+'/bin/ciao.csh ; '



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
; Read in exposure map
;
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
bmap(where(bmap eq 0.0))=1.0


;
; Divide by exposure map
;
im_ec=im/bmap


;
; Define center if necessary
;
;if (n_elements(xcen) eq 0) then xcen=xsize/2
;if (n_elements(ycen) eq 0) then ycen=xsize/2


;
; Calculate the Sky X and Y of the image center
;
;xc=CRVAL1P+CRPIX1P+CDELT1P*xcen
;yc=CRVAL2P+CRPIX2P+CDELT2P*ycen
xcen=(xc-CRVAL1P-CRPIX1P)/CDELT1P
ycen=(yc-CRVAL2P-CRPIX2P)/CDELT2P


;
; Define the EXPOSURE time value
;
;if (n_elements(expfile) ne 0) then begin
;    spawn,'fitsdump -H '+expfile+' | grep EXPOSURE',result
;endif else begin
;    spawn,'fitsdump -H '+imgfile+' | grep EXPOSURE',result
;endelse
;exptime=sxpar(result,'EXPOSURE')
exptime=sxpar(hd,'EXPOSURE','Parameter EXPOSURE not found')



;
; Create distance mask
;
mask=fltarr(xsize,ysize)

xdist=findgen(xsize)
xcoord=xdist-float(xcen)
xdist=abs(xdist-float(xcen))

ydist=findgen(ysize)
ycoord=ydist-float(ycen)
ydist=abs(ydist-float(ycen))

if (ellip eq 0.0) then begin
    for i=0,xsize-1 do begin
        for j=0,ysize-1 do begin
            mask(i,j)=sqrt(xdist(i)*xdist(i)+ydist(j)*ydist(j))
        endfor
    endfor
endif else begin
                                ;
                                ; If ellipses are used, redefine the mask in terms
                                ; of the semimajor axis
                                ;
    for i=0,xsize-1 do begin
        for j=0,ysize-1 do begin
            x_ell=xcoord(i)*cos(pa)+ycoord(j)*sin(pa) ; change to the ellipse's 
            y_ell=ycoord(j)*cos(pa)-xcoord(i)*sin(pa) ; coordinate system
            a_ell=sqrt(x_ell^2.0+y_ell^2.0/(1.0-ellip)^2.0) ; solve for semimajor axis
            mask(i,j)=a_ell
        endfor
    endfor
endelse


;
; Multiply by exposure mask
;
;mask=bmap*mask
;
; Instead, set mask to zero where the exposure map is zero,
; so that those pixels are not counted in the area or totcounts
;
for i=0,xsize-1 do begin
    for j=0,ysize-1 do begin
        if (em[i,j] eq 0.0) then mask[i,j]=0.0
    endfor
endfor
mask(xcen,ycen)=0.01
writefits,'mask.fits',mask,hd


;
; Trim mask to fit the image (if ccdregfile is specified)
;
if (n_elements(ccdregfile) ne 0) then begin
    print,'Triming CCD...'
    cmdstring='dmcopy "mask.fits[sky=region('+ccdregfile+')]" mask_clip.fits clobber=yes'
    spawn,cmdroot+cmdstring
    print,'...done.'
    if (n_elements(regfile) eq 0) then mask=readfits('mask_clip.fits') 
endif


;
; Exclude point sources that were removed (if regfile is specified)
;
if (n_elements(regfile) ne 0) then begin
    print,'Removing point sources...'
    cmdstring='dmcopy "mask_clip.fits[exclude sky=region('+regfile+')]" mask_clip_no_ptsrc.fits clobber=yes'
    spawn,cmdroot+cmdstring,result
    print,'...done.'
    mask=readfits('mask_clip_no_ptsrc.fits') 
endif


;
; Determine number of radial bins and define some arrays
;
nrad=fix(max(mask)/xybin)
r1=fltarr(nrad)
r2=fltarr(nrad)
a1=fltarr(nrad)
a2=fltarr(nrad)
b1=fltarr(nrad)
b2=fltarr(nrad)
sb=fltarr(nrad)
esb=fltarr(nrad)
sb_ec=fltarr(nrad)
esb_ec=fltarr(nrad)
area=fltarr(nrad)


;
; Loop over bins and calculate the surface brightness
;
if (ellip eq 0.0) then begin
    for i=0,nrad-1 do begin
        r1(i)=i*xybin
        r2(i)=(i+1)*xybin
        if (i gt 0) then j=where( (mask gt r1(i)) and (mask le r2(i)) ) $
        else j=where( (mask gt 0.0) and (mask le r2(i)) )
        area(i)=n_elements(j)
        totcnts=total(float(im(j)))
        totcnts_ec=total(float(im_ec(j)))
        
        sb_ec(i)=totcnts_ec/area(i)
        esb_ec(i)=sqrt(totcnts)*totcnts_ec/totcnts/area(i)
        sb(i)=(totcnts/area(i))/exptime
        esb(i)=(sqrt(totcnts)/area(i))/exptime
    endfor
endif else begin
    for i=0,nrad-1 do begin
        a1(i)=i*xybin
        a2(i)=(i+1)*xybin
        if (i gt 0) then j=where( (mask gt a1(i)) and (mask le a2(i)) ) $
        else j=where( (mask gt 0.0) and (mask le a2(i)) )
        area(i)=n_elements(j)
        totcnts=total(float(im(j)))
        totcnts_ec=total(float(im_ec(j)))
        
        sb_ec(i)=totcnts_ec/area(i)
        esb_ec(i)=sqrt(totcnts)*totcnts_ec/totcnts/area(i)
        sb(i)=(totcnts/area(i))/exptime
        esb(i)=(sqrt(totcnts)/area(i))/exptime
    endfor
endelse


;
; Write the output file
;
if (n_elements(outfile) ne 0) then begin
    space='         '
    fmt='$(2(3x,f9.3),5(3x,e20.5))'
    fmt2='$(2(3x,a9),5(3x,a20))'
    get_lun,unit
    openw,unit,outfile
    printf,unit,' '
    printf,unit,'Extracted radial surface brightness profile'
    printf,unit,' '
    printf,unit,'Date: ',!stime
    printf,unit,'Input file          : ',imgfile
    printf,unit,'Exp. file           : ',expfile
    printf,unit,'Exp. time           : ',exptime   
    printf,unit,'Sky X               : ',xc
    printf,unit,'Sky Y               : ',yc
    printf,unit,'X Centroid          : ',xcen
    printf,unit,'Y Centroid          : ',ycen
    printf,unit,'Ellipticity (1-b/a) : ',ellip
    printf,unit,'Position angle (deg): ',pa*360.0/(2.0*!PI)
    printf,unit,'Binsize             : ',xybin
    printf,unit,' '
    if (ellip eq 0.0) then begin
        printf,unit,fmt2,' Radius 1',' Radius 2','SB [phot/s/cm^2/pix]','       Error        ','   SB [phot/s/pix]  ', $
          '       Error        ','        AREA        '
        for i=0,nrad-1 do begin
            printf,unit,fmt,r1(i),r2(i),sb_ec(i),esb_ec(i),sb(i),esb(i),area(i)
        endfor
    endif else begin
        printf,unit,fmt2,'   SMA 1 ','   SMA 2 ','SB [phot/s/cm^2/pix]','       Error        ','   SB [phot/s/pix]  ', $
          '       Error        ','        AREA        '
        for i=0,nrad-1 do begin
            printf,unit,fmt,a1(i),a2(i),sb_ec(i),esb_ec(i),sb(i),esb(i),area(i)
        endfor
    endelse
    close,unit
    free_lun,unit
endif


;
; Return to IDL
;
return
end
