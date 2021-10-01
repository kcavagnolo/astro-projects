;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LINE_IMAGE: SPECTRAL PROJECTION ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro line_image,img,d,t,v,f,m,xx,yy,zz,redshift,specgrid,specgridzero,$
               logegrid,egrid,tgrid,imx,imy,$
               filename=filename,$
               verbose=verbose,center=center,rdcenter=rdcenter

;
; Sebastian Heinz, 12/15/2008
;
; input:
;        d:        electron density (in cm^-3)
;        t:        temperature (in Kelvin)
;        f:        volume filling fraction of emitting gas in voxel
;        v:        velocity along the line of sight
;        metallicity: array (same dimension as d) or scalar, in solar units
;        xx,yy,zz: axis values (in cm)
;        specgrid: output from specgrid routine for metals > He
;        specgridzero: output from specgrid routine for H and He
;        logegrid: output from specgrid routine
;        tgrid:    output from specgrid routine
;        redshift: cosmological redshift z
; output: 
;        img: spectral data cube
; keywords:
;        filename: output file name. If not set, no file will be written
;        cosmo:    vector of cosmological parameters: 
;                  cosmo=[Omega_{m,0},Omega_{Lambda,0},Omega_{0},H_0]
;                  if not provided, concordance values of 
;                  cosmo=[0.7,0.3,1.0,70 km/s/Mpc] are assumed
;        center:   reference grid point for coordinate system;
;                  two-element vector, units: cm
;        rdcenter: right-ascention and declination for reference grid
;                  point (center); two-element vector, untis: degrees
;

common cosmo,omegam,omegal,omegar,omega,h0,pdist,adist,ldist

; initialize keyword variables
if keyword_set(verbose) then verbose=verbose else verbose=1

x=xx
y=yy
z=zz
nx=n_elements(x)-1
ny=n_elements(y)-1
nz=n_elements(z)-1

; setup problem
; do all calculations in floats (memory)
specgrid=float(specgrid)
specgridzero=float(specgridzero)
logegrid=float(logegrid)
tgrid=float(tgrid)

; full temperature range of grid for interpolation
tdel=alog10(max(tgrid))-alog10(min(tgrid)) 

; minimum temperature
tlm=alog10(min(tgrid)) 

; ebergy grid spacing
de=logegrid(1)/logegrid(0)

; no speeds larger than c
vbad=where(v ge 3.e10)
if vbad(0) ne -1 then v(vbad)=2.99999e10

; no speeds smaller than -c
vbad=where(v le -3.e10)
if vbad(0) ne -1 then v(vbad)=-2.99999e10

; grid coordinates
x=float(x/3.08e21) ; in kpc
y=float(y/3.08e21) ; in kpc
z=float(z/3.08e21) ; in kpc
dx=x(1:nx)-x(0:nx-1)
dy=y(1:ny)-y(0:ny-1)
dz=z(1:nz)-z(0:nz-1)

; voxel volume: in kpc^3
; normalization and cosmological correction
fac=float(3.08e21/(4.*!pi*ldist^2*(1.0 + redshift)))*$
  reform(reform((dx#dy),nx*ny)#dz,nx,ny,nz)

; re-shuffle according to projection direction
n1=(size(d))(1)
n2=(size(d))(2)
n3=(size(d))(3)

; energy and temperature grid size
neg=(size(logegrid))(1)
nt=(size(tgrid))(1)

; setup for output array (note: energy values are bin boundaries)
img=fltarr(n1,n2,neg-1)

; actual loop for spectral projection
;
if verbose ge 1 then $
  print,format='($,"Spectral projection... 000.00% complete")'
; x-axis loop of image
for i=0,n1 - 1 do begin
    if verbose eq 1 then $
      print,format='($,A16,f6.2,"% complete")',$
      string(replicate(8b,16)),$
      float(i*n2*n3)/max([1.e,float(n1*n2*n3)])*100.e
    ; y-axis loop of image
    for j=0,n2 - 1 do begin
        ; projection axis
        for k=0,n3 - 1 do begin
            ; how far along are we? Verbose=1 should not be set if output is logged!
            if verbose ge 2 then $
              print,format='($,A16,f6.2,"% complete")',$
              string(replicate(8b,16)),$
              float(i*n2*n3 + j*n3 + k)/max([1.e,float(n1*n2*n3)])*100.e

            ; calculate Doppler factor for LOS velocity
            shiftv=float(v(i,j,k))/3.e10
            dopp=sqrt((1.e + shiftv)/(1.e - shiftv))

            ; Doppler-shift spectrum
            shift=min([neg-3,max([-neg+2,$
                                  alog10(dopp)/alog10(de)])])
            shift1=min([neg-3,max([-neg+2,$
                                  floor(alog10(dopp)/alog10(de))])])
            shift2=shift1+1
            f2=float(shift-shift1)
            f1=float(shift2-shift)

            ; find appropriate temperature bin in spectrum 
            ; (nearest neighbor)
            temp=max([0,$
                      min([nt-1,$
                           (alog10(t(i,j,k))-tlm)/tdel*nt])])
            temp1=max([0,$
                      min([nt-2,$
                           floor((alog10(t(i,j,k))-tlm)$
                                 /tdel*nt)])])            
            temp2=temp1+1
            ft2=float(temp-temp1)
            ft1=float(temp2-temp)

            if n_elements(m) gt 1 then zval=m(i,j,k) $
              else zval=m
            if n_elements(f) gt 1 then fval=f(i,j,k) else fval=f

            ; make sure temperature and velocity data exist
            if (finite(temp1) ne 0 and finite(temp2) ne 0 $
                and finite(shift1) ne 0 and $
                finite(shift2) ne 0) then begin
                ; Doppler-shifted spectrum. Note: discrete
                ; shifts mean that we have to oversample,
                ; which is why we use nerg=3*enum
                dummy=$
                  (ft1*shift(specgridzero(*,temp1) + $
                             zval*specgrid(*,temp1),shift1) + $
                   ft2*shift(specgridzero(*,temp2) + $
                             zval*specgrid(*,temp2),shift1))*f1 + $
                  (ft1*shift(specgridzero(*,temp1) + $
                             zval*specgrid(*,temp1),shift2) + $
                   ft2*shift(specgridzero(*,temp2) + $
                             zval*specgrid(*,temp2),shift2))*f2
                ; multiply by n^2*fill*vol/4pi^2
                dummy*=fval*float(d(i,j,k))^2*float(fac(i,j,k))

                ; Make sure there is no NAN problem, then add
                good=where(finite(dummy) eq 1)
                if good(0) ne -1 then img(i,j,good)+=dummy(good)

            endif
        endfor
    endfor
endfor

if verbose ne 0 then $
  print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.00

; re-assign image coordinates depending on LOS
; convert image coordinates to ra-dec in degrees
imx=(x-replicate(center(0)/3.08d21,n_elements(x)))*!radeg/adist + rdcenter(0)
imy=(y-replicate(center(1)/3.08d21,n_elements(y)))*!radeg/adist + rdcenter(1)

; save and return output data
if keyword_set(filename) then $
  save,img,imx,imy,redshift,n1,n2,logegrid,egrid,$
  pdist,ldist,adist,center,rdcenter,$
  filename=filename

end
