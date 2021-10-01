;;;;;;;;;;;;;;
; CHANDRAPSF ;
;;;;;;;;;;;;;;

pro chandrapsf,e,imx,imy,kern

;
; Sebastian Heinz, 02/16/2009
;
; Return the Chandra PSF for an input set of coordinate axes and energy. Taken
; from MARX simulations at 20 different energies and made axisymmetric (i.e.,
; this is a 1D representation of the PSF). Returns interpolated to energy
; e. Includes read-streak.
;
; input:
;
;        e:          energy at which PSF is to be evaluated
;
;        imx:        image-x coordinates
;
;        imy:        image-y coordinages
;
; output:
;
;        kern:       2D image of the PSF
;

common telescope,dpixx,dpixy,resolution


; load data array
restore,'$XIMPATH/chandrapsf.dat'

; dimensions of output kernel
xbins=n_elements(imx)-1
ybins=n_elements(imy)-1

; set up radial coorindate
xx=(imx(1:*)-median(imx))#replicate(1.,ybins)/0.492
yy=replicate(1.,xbins)#(imy(1:*)-median(imy))/0.492
rr=floor(sqrt(xx^2 + yy^2) + 0.5)

; determine energy planes to interpolate between
enum=n_elements(egrid)
e1=min([enum-2,max([0,max(where(egrid le e))])])
e2=e1+1
ex=min([max(egrid),max([egrid(0),e])])

; linear interpolation coefficients
dele=egrid(e2)-egrid(e1)
delex1=(ex-egrid(e1))/dele
delex2=(egrid(e2)-ex)/dele

; any pixels outside of data range? use outermost available bin
outer=where(rr gt 1023)
if outer(0) ne -1 then rr(outer)=1023.

; calculate both kernels from nearest radial bin (radial bins are integer
; pixel values)
kern1=(psfc.(4*e1 + 2))(rr)
kern2=(psfc.(4*e2 + 2))(rr)
kern=kern1*delex1 + kern2*delex2

; add read-out streak
regrid,psfc.(4*e1 + 4),findgen(1025)-512.,streak1,imx-median(imx),1
regrid,psfc.(4*e2 + 4),findgen(1025)-512.,streak2,imx-median(imx),1
streak=streak1*delex1 + streak2*delex2
kern1=streak#replicate(1.,ybins)
kern+=kern1

; normalize kernel
kern/=total(kern)

end
