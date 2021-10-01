;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; POISSONSPEC: APPLY POISSON STATISTICS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro poissonspec,xrayspec,bgspec,exptime,simspec,simbgspec,simsourcespec,$
                filename=filename,verbose=verbose

;
; Sebastian Heinz, 12/15/2008
;
; add Poisson noise to simulated observation
;
; input:
;
;        xrayspec: spectral data cube (units: counts/sec)
;        exptime:  exposure time (units: ksec)
;
; output:
;
;        simspec:  simulated spectrum
;
; keywords:
;
;        filename: if set, output written into filename
;
;        verbose:  print progress info
;

if keyword_set(verbose) then verbose=verbose else verbose=1

nx=n_elements(xrayspec(*,0,0))
ny=n_elements(xrayspec(0,*,0))
nz=n_elements(xrayspec(0,0,*))
bgi=reform(replicate(1.,nx*ny)#bgspec,nx,ny,nz)

simspec=xrayspec
simbgspec=simspec
simsourcespec=simspec
if verbose ne 0 then $
  print,format='($,"Poisson noise......... 000.00% complete")'
for i=0L,n_elements(xrayspec)-1L do begin
    if verbose eq 2 then $
      print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
      float(i)/float(n_elements(xrayspec))*100.
    if (xrayspec(i) ne 0.0) then $
      simsourcespec(i)=randomn(seed,poisson=$
                         max([xrayspec(i)*exptime*1.e3 + $
                              1.e-10,1.e-10]),/double)
    if (bgi(i) ne 0.0) then $
      simbgspec(i)=randomn(seed,poisson=$
                         max([bgi(i)*exptime*1.e3 + $
                              1.e-10,1.e-10]),/double)
    simspec(i)=simsourcespec(i)+simbgspec(i)
endfor
if verbose ne 0 then $
  print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.0

if keyword_set(filename) then save,simspec,simbgspec,simsourcespec,exptime,filename=filename

end
