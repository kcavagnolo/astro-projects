;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; XRAY_PROJ: X-RAY RESPONSE PROJECTION ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro xray_proj,xrayspec,bgspec,img,imx,imy,$
              xpix,ypix,ra,dec,nh,logegrid,egrid,$
              dpixx,dpixy,resolution,$
              telescope,response,arf,channel,$
              aimpoint=aimpoint,filename=filename, verbose=verbose,$
              vturb=vturb,chmin=chmin,chmax=chmax,native=native,$
              psf=psf,background=background

;
; Sebastian Heinz, 12/15/2008
;
; Convolve spectral data cube with instrument response and arf
;
; input:
;        img:   data cube (units: counts per second per cm^2)
;        imx:   image x-axis (physical)
;        imy:   image y-axis (physical)
;        xpix:  output image coordinates (detector) - x
;        ypix:  output image coordinates (detector) - y
;        ra:    output image coordinates (sky) - RA
;        dec:   output image coordinates (sky) - DEC
;        adist: angular diameter distance
;        n1:    x-size
;        n2:    y-size
;        logegrid: logarithmic energy grid from spectral shift
;        egrid: energy bins for spectral dimension
;        dpixx: x-pixel size of detector in arcsec
;        dpixy: y-pixel size of detector in arcsec
;        resolution: telescope resolution in arcsec
;        telescope: name of telescope (see allowed values in xim.pro)
;        response: name of response file
;        arf:   name of ancilliary response file (if "", blank)
;
; output:
;        consxspec: output data cube (units: counts per second)
;
; keywords:
;        filename:  output filename if save is desired
;        telescope: specify if Chandra PSF approximation should be used
;        verbose:   text output?
;        vturb:     convolve with turbulent velocity?
;        aimpoint:  RA and DEC of telescope aimpoint
;        chmin:     minimum channel number used
;        chmax:     maximum channel number used
;        psf:       string containing the namer of a procedure to return
;                   a 2D PSF for a given energy
;        native:    set if output spectrum is supposed to be calculated on
;                   native energy grid of the response file
;        background: string containing the name of a procedure to return 
;                   two sets of background spectr (instrument and sky
;                   backgrounds), each for a given energy grid.
;
 
; first task: re-grid image dimensions to instrument resolution and sampling

if keyword_set(verbose) then verbose=verbose else verbose=1
if keyword_set(vturb) then vturb=vturb else vturb=0.0
if keyword_set(native) then native=native else native=0
if keyword_set(psf) then psf=psf else psf="psf_gauss"

; Energy grid sizes
enum=n_elements(egrid)-1

; Rescale to acrsec
imx=(imx-aimpoint(0))*3600.
imy=(imy-aimpoint(1))*3600.

; Sanity check
if max([abs(imx/dpixx),abs(imy/dpixy)]) gt 2.e31 then begin
    print,'WARNING: detector coordinates out of range'
    print,'Check aimpoint coordinates'
    imx=imx-imx(0)
    imy=imy-imy(0)
endif

; Grid not uniform? Need to re-grid before psf convolution
delx=imx(1:*)-imx(0:n_elements(imx)-2)
dely=imy(1:*)-imy(0:n_elements(imy)-2)

if (((max(abs(delx))-min(abs(delx)))/max(abs(delx)) gt 1.e-4) or $
    ((max(abs(dely))-min(abs(dely)))/max(abs(dely)) gt 1.e-4)) then begin
    
    if verbose gt 0 then print,$
      'Stand by: Regridding to uniform resolution for PSF convolution'
    
; if so, convolve with PSF on an oversampled grid.
    xbins=ceil((max(imx)-min(imx))/max([min(abs(delx)),dpixx/2.]))
    ybins=ceil((max(imy)-min(imy))/max([min(abs(dely)),dpixy/2.]))
    imxr=min(imx) + (max(imx)-min(imx))*findgen(xbins+1)/float(xbins)
    imyr=min(imy) + (max(imy)-min(imy))*findgen(ybins+1)/float(ybins)
    
; re-grid to uniform grid, oversampling telescope
    regrid,img,imx,xrayspec,imxr,1,$
      message='Regrid x to uniform...',verbose=verbose
    regrid,xrayspec,imy,img,imyr,2,$
      message='Regrid y to uniform...',verbose=verbose
    imx=imxr
    imy=imyr
    
endif  

;;;;;;;;;;;;;
; apply PSF ;
;;;;;;;;;;;;;

; oversample grid for edge effects
nov=11 & delx=imx(1)-imx(0) & dely=imy(1)-imy(0)
pimx=[(findgen(nov) - float(nov))*delx + min(imx),imx,max(imx)+(findgen(nov) + 1.)*delx]
pimy=[(findgen(nov) - float(nov))*dely + min(imy),imy,max(imy)+(findgen(nov) + 1.)*dely]

; array size
xbins=n_elements(imx)
ybins=n_elements(imy)

; loop over energy
for i=0,enum-2 do begin
    call_procedure,psf,0.5*(egrid(i)+egrid(i+1)),pimx,pimy,kern
    nkx=n_elements(kern(*,0)) & nky=n_elements(kern(0,*))
    fk=fft(shift(kern,ceil(nkx/2.),ceil(nky/2.)),/double)*$
      (float(xbins + 2*nov)*float(ybins + 2*nov))

    dummy=fltarr(xbins+2*nov-1,ybins+2*nov-1)
    dummy(nov:nov+xbins-2,nov:nov+ybins-2)=img(*,*,i)

    fimg=fft(dummy,/double)
    img(*,*,i)=(real_part(fft(fimg*fk,/double,/inverse)))(nov:nov+xbins-2,nov:nov+ybins-2)
    if (verbose ne 0 and i eq 0) then $
      print,format='($,"PSF convolution....... 000.00% complete")'
    if verbose ne 0 then $
      print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
      float(i)/max([1.,float(enum)])*100.
endfor
if verbose ne 0 then $
  print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.0

; Rescale to detector coordinates (pixels) relative to telescope aimpoint
imx/=dpixx
imy/=dpixy


; Use integer detector coordinates
xbins=ceil(max(imx)) - floor(min(imx))
ybins=ceil(max(imy)) - floor(min(imy))
xpix=dindgen(xbins+1) + floor(min(imx))
ypix=dindgen(ybins+1) + floor(min(imy))

; Output ra and dec in degrees:
ra=(xpix)*dpixx/3600. + aimpoint(0)
dec=(ypix)*dpixy/3600. + aimpoint(1)

; re-grid to instrument pixel scale
regrid,img,imx,xrayspec,xpix,1,$
  message='Regrid x to detector..',verbose=verbose
regrid,xrayspec,imy,dspec,ypix,2,$
  message='Regrid y to detector..',verbose=verbose
xrayspec=fltarr(xbins,ybins,enum)

; for one-band images, need to reform to 3D for smoothing
if enum eq 1 then xrayspec=reform(xrayspec,xbins,ybins,1)

; include Gaussian turbulent broadening if indicated
if (vturb gt 0.0) then begin
    ; calculate Doppler factor for LOS velocity
    shiftv=float(vturb)/3.e10
    dopp=sqrt((1.e + shiftv)/(1.e - shiftv))
    de=logegrid(1)/logegrid(0)
    sigma=alog10(dopp)/alog10(de)
    nkern=ceil(10.0*sigma)
    kernx=findgen(nkern)-float(nkern-1)/2.0
    kern=exp(-kernx^2/(2.0*sigma^2))
    kern/=total(kern)
    kern=reform(kern,1,1,nkern)
    if verbose gt 0 then begin
        print,'Smoothing with turbulent Gaussian of velocity: ',vturb
        print,'Energy width: ',sigma
    endif
    dspec=convol(dspec,kern,/edge_truncate,/center)
endif

; absorption?
if nh gt 0. then begin
    restore,'wabs.dat'
    netau=n_elements(etau)
    detau=(etau(1:netau-1)-etau(0:netau-2))/2.
    regrid,exp(nh*tau),[etau-detau,etau(netau-1)+detau(netau-2)],$
      abs,logegrid,1,/average,message="Regridding abs model.."
    for i=0l,long(xbins-1) do for j=0l,long(ybins-1) do dspec(i,j,*)*=abs
endif    

if native ne 0 then iegrid=logegrid

; pick response based on resolution requirement
if verbose eq 2 then print,'Reading response file ',response
if verbose eq 2 then print,'Reading ancillary response file ',arf
read_resp,response,arf,logegrid,egrid,mat,channel,verbose=verbose,$
  chmin=chmin,chmax=chmax,native=native

; if operating at instrument resolution, regrid input data cube to save time
; and space

if (native ne 0) then begin
    regrid,dspec,iegrid,dspecout,logegrid,3,$
      message="Regrid to spec res....",verbose=verbose
    dspec=dspecout
    dspecout=0
endif
lenum=n_elements(logegrid)-1

; convolve input spectrum with response to get output spectrum
if verbose ne 0 then $
  print,format='($,"Spectral convolution.. 000.00% complete")'

for k=0l,long(lenum)-1l do begin
    if verbose ne 0 then $
      print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
      float(k)/max([1.,float(lenum)])*100.
    nin=n_elements(mat.(4 + 2*k))
    xrayspec(*,*,mat.(4 + 2*k))+=$
      reform(replicate(1.,xbins*ybins)#$
             mat.(5 + 2*k),xbins,ybins,nin)*$
      reform(reform(dspec(*,*,k),xbins*ybins)#$
             replicate(1.,nin),xbins,ybins,nin)
endfor
if verbose ne 0 then $
  print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.0

; calculate background spectrum
call_procedure,background,logegrid,skybg,egrid,instbg
skybg*=dpixx*dpixy
instbg*=dpixx*dpixy
bgspec=fltarr(enum)

; convolve sky background spectrum with response to get output background
if verbose gt 1 then $
  print,format='($,"Background convolution 000.00% complete")'
for k=0l,long(lenum)-1l do begin
    if verbose ne 0 then $
      print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
      float(k)/max([1.,float(lenum)])*100.
    nin=n_elements(mat.(4 + 2*k))
    bgspec(mat.(4 + 2*k))+=$
      mat.(5 + 2*k)*replicate(skybg(k),nin)
endfor
bgspec+=instbg

if verbose gt 1 then $
  print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.0

; Save?
if keyword_set(filename) then $
  save,xrayspec,bgspec,kern,xpix,ypix,ra,dec,egrid,filename=filename

end 
