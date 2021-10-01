;;;;;;;;;;;
; IXO PSF ;
;;;;;;;;;;;

pro ixopsf,e,imx,imy,kern

;
; Sebastian Heinz, 02/24/2009
;
; return a 2D psf for a given energy and image scale. For now, this is simply
; a monochromatic PSF taken from a SIMX simulation. PSF is image-centered.
;
; input:
;
;        e:        energy at which psf is evaluated
;
;        imx:      coordinates of image x-axis (must be unformly gridded) in
;                  arcseconds
;
;        imy:      coordinates of image y-axis (must be unformly gridded) in
;                  arcseconds
;
; output:
;
;        kern:     PSF image
;

common telescope,dpixx,dpixy,resolution

; load data array
restore,'$XIMPATH/ixopsf.dat'

; dimensions of output kernel
xbins=n_elements(imx)-1
ybins=n_elements(imy)-1

; set up radial coorindate
xx1=imx/2.57831*2.
yy1=imy/2.57831*2.
xx1-=median(xx1)
yy1-=median(yy1)

; regrid PSF to image grid
regrid,psf,xx,kern1,xx1,1
regrid,kern1,yy,kern,yy1,2

end
