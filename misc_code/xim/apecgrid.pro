;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SPECGRID: READ IN ATOMDB AND APEC DATA ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro apecgrid,tgrid,legrid,specgrid,specgridzero,$
             filename=filename,verbose=verbose

;
; Sebastian Heinz, 12/15/2008
; 
; APECGRID: REAF IN APEC MODEL AND CONSTRUCT SPECTRAL MATRIX
;
; CALLING SEQUENCE
;
;         apecgrid,tgrid,egrid,specgrid,specgridzero,$
;                  verbose=verbose
;
; input: 
;         tgrid: temperature grid for which spectra are to be calculated
;
;         egrid: energy bins for matrix (x-axis)
;
; output:
;         specgrid: emission from elements heavier than helium
;
;         specgridzero: Helium and hydrogen emission
;
; keywords:
;         verbose: print progress info
;
; NOTE: Before running this script:
;         read in ATOMDB data
;         @/path/to/atomdb_idl-2.00/init_atomdb_idl.pro
;

common spec,userparameters
common cosmo,omegam,omegal,omegar,omega,h0,pdist,adist,ldist

if keyword_set(verbose) then verbose=verbose else verbose=1

if n_elements(where(tgrid ge 1.e4 and tgrid le 7.9d8)) ge 2 then $
  tgrid=tgrid(where(tgrid ge 1.e4 and tgrid le 7.9e8))

tgrid=alog10(tgrid)

read_linelist,'$ATOMDB/apec_line.fits',linestruct,temperature,dens,power
read_coco,'$ATOMDB/apec_coco.fits',cocostruct,temperature,dens,power

; make grids to calculate spectra for
ntem2=n_elements(temperature)

; energy grid size
egrid=legrid
under=where(egrid le 0.05)
if under(0) ne -1 then $
  egrid(under)=$
  0.05-reverse(findgen(n_elements(under))/float(n_elements(under))*1.e-3)
over=where(egrid ge 30.0)
if over(0) ne -1 then $
  egrid(over)=$
  30.+(findgen(n_elements(over))/float(n_elements(over)-1))*1.e-3


nerg=n_elements(egrid)

; do calculation in log space
temperature=alog10(temperature)

; initialize spectral model
specgrid=dblarr(nerg-1)#tgrid
specgridzero=dblarr(nerg-1)#tgrid

; populate spectral matrix
if verbose ne 0 then $
  print,format='($,"Reading spectra....... 000.00% complete")'

for i=0,n_elements(tgrid)-1 do begin
  ; how far along are we?
  if verbose ne 0 then $
    print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
    float(i)/float(max([n_elements(tgrid),1.e]))*100.e

  ; Find neighbor temperature bins of model
  under=min([max([0,max(where(temperature le tgrid(i)))]),ntem2-2])
  over=max([1,min([ntem2-1,min(where(temperature gt tgrid(i)))])])

  ; linear interpolation in log-space for temperatures
  ; do temperature interpolation onto fine grid
  ; once here rather than many times 
  ; later when looping through the grid
  fac1=max([0,min([1,$
                   (tgrid(i)-temperature(under))/$
                   (temperature(over)-temperature(under))])])
  fac2=max([0,min([1,$
                   (temperature(over)-tgrid(i))/$
                   (temperature(over)-temperature(under))])])

  ; interpolate in log-t space to properly capture powerlaw temperature 
  ; dependence of emissivity. APED does not interpolate, so must do here for
  ; proper calculation...
  f1=fac2*alog10(calc_spectrum(linestruct,$
                               cocostruct,egrid,10.e^(temperature(under)),$
                               silent=1,elements=indgen(50)+3))
  f2=fac1*alog10(calc_spectrum(linestruct,$
                               cocostruct,egrid,10.e^(temperature(over)),$
                               silent=1,elements=indgen(50)+3))

  f1o=fac2*alog10(calc_spectrum(linestruct,$
                               cocostruct,egrid,10.e^(temperature(under)),$
                               silent=1,elements=[1,2]))
  f2o=fac1*alog10(calc_spectrum(linestruct,$
                               cocostruct,egrid,10.e^(temperature(over)),$
                               silent=1,elements=[1,2]))

  ; only use good points (set others to zero)
  good=(where(finite(f1) eq 1 and finite(f2) eq 1))
  good1=(where(finite(f1) eq 1 and finite(f2) ne 1))
  good2=(where(finite(f2) eq 1 and finite(f1) ne 1))
  bad=where(finite(f1) ne 1 and finite(f2) ne 1)
  if good(0) ne -1 then specgrid(good,i)=10.e^(f1(good)+f2(good))
  if good1(0) ne -1 then specgrid(good1,i)=fac2*10.e^(f1(good1)/fac2)
  if good2(0) ne -1 then specgrid(good2,i)=fac1*10.e^(f2(good2)/fac1)
  if bad(0) ne -1 then specgrid(bad,i)=0.e

  good=(where(finite(f1o) eq 1 and finite(f2o) eq 1))
  good1=(where(finite(f1o) eq 1 and finite(f2o) ne 1))
  good2=(where(finite(f2o) eq 1 and finite(f1o) ne 1))
  bad=where(finite(f1o) ne 1 and finite(f2o) ne 1)
  if good(0) ne -1 then specgridzero(good,i)=10.e^(f1o(good)+f2o(good))
  if good1(0) ne -1 then specgridzero(good1,i)=fac2*10.e^(f1o(good1)/fac2)
  if good2(0) ne -1 then specgridzero(good2,i)=fac1*10.e^(f2o(good2)/fac1)
  if bad(0) ne -1 then specgridzero(bad,i)=0.e
  
endfor
if verbose ne 0 then $
  print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),$
  100.0

; if save keyword is set, save to filename=save 
if keyword_set(filename) then save,specgrid,specgridzero,egrid,tgrid,filename=filename

end
