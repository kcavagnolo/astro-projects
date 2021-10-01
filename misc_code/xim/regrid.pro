;;;;;;;;;;
; REGRID ;
;;;;;;;;;;

pro regrid,oaro,oe,nar,nen,dir,average=average,message=message,$
           verbose=verbose

;
; Sebastian Heinz, 04/20/2008
;
; Generic regridding algorithm for 1 to 3 dimensional data that preserves the
; total content (in the case of interest, spectral counts).
;
; input:
;         oaro:   iput array to be binned
;         oe:     edge values of the pixels along the binning axis 
;                 (edge values, must be of dimension oara(...,dir,...)+1
;         dir:    direction along which array is to be re-gridded
;         nen:    new coordinate axis onto which oaro is to be gridded
;
; output:
;         nar:    re-gridded array
;
; keywords:
;         average: if set to non-zero, regridded array is
;                  interpolated. Default is for regridding to be conservative
;                  (total of both input and ouput arrays are the same).
;         message: output message if verbose is non-zero
;         verbose: output regridding progress
;

if keyword_set(message) then message=message else message=""
if keyword_set(average) then average=average else average=0
if keyword_set(verbose) then verbose=verbose else verbose=1

if n_elements(dir) eq 0 then dir=1

; don't overwrite the input array
oar=oaro
ndim=(size(oar))(0)

; if 1d or 2d input, reform to 3d
if ndim eq 1 then oar=reform(oar,(size(oar))(1),1,1)
if ndim eq 2 then oar=reform(oar,(size(oar))(1),(size(oar))(2),1)

; set permutation direction so binning axis is always x-axis
if dir eq 1 then P=[0,1,2] else $
  if dir eq 2 then P=[1,0,2] else P=[2,1,0]

; transpose array
oar=(transpose(oar,P))
nold=n_elements(oar(0,0,*))
xs=n_elements(oar(0,*,0))
ys=n_elements(oar(0,0,*))
zs=n_elements(nen)-1L

; set pixel boundary array
oel=oe(0:n_elements(oe)-2)
oer=oe(1:*)

; make output array
nar=reform(fltarr(zs,xs,ys),zs,xs,ys)

if message ne "" then $
  if verbose ne 0 then $
  print,format='($,A22," 000.00% complete")',message
for k=0L,zs-1L do begin
    ; how far along are we?
    if message ne "" then $
      if verbose ne 0 then $
      print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
      float(k)/float(max([zs,1.]))*100.

    ; variable used for average (bin width)
    del=0

    ; find original elements that overlap with energy bin k
    full=where((oel le nen(k) and oer gt nen(k+1)) or $
               (oel lt nen(k) and oer ge nen(k+1)))
    inside=where(oel ge nen(k) and oer le nen(k+1))
    left=where(oel lt nen(k) and oer gt nen(k) and oer lt nen(k+1))
    right=where(oel lt nen(k+1) and oel gt nen(k) and oer gt nen(k+1))

    ; add them all up
    if (full(0) ne -1) then begin
        nar(k,*,*)+=oar(full,*,*)*$
          reform(replicate((nen(k+1)-nen(k))/$
                           (oer(full)-oel(full)),xs*ys),1,xs,ys)
        del+=(nen(k+1)-nen(k))/$
                           (oer(full)-oel(full))
    endif
    if (inside(0) ne -1) then begin
        nar(k,*,*)+=$
          total(reform(oar(inside,*,*),n_elements(inside),xs,ys),1)
        del+=float(n_elements(inside))
    endif
    if (left(0) ne -1) then begin
        nar(k,*,*)+=oar(left,*,*)*$
          reform(replicate((oer(left)-nen(k))/$
                           (oer(left) - oel(left)),xs*ys),1,xs,ys)
        del+=(oer(left)-nen(k))/(oer(left) - oel(left))
    endif
    if (right(0) ne -1) then begin
        nar(k,*,*)+=oar(right,*,*)*$
          reform(replicate((nen(k+1)-oel(right))/$
                           (oer(right) - oel(right)),xs*ys),1,xs,ys)
        del+=(nen(k+1)-oel(right))/(oer(right) - oel(right))
    endif

    ; if average desired, divide by bin width
    if average ne 0 then nar(k,*,*)/=reform(replicate(del,xs*ys),1,xs,ys)
endfor
if message ne "" then $
  if verbose ne 0 then $
  print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.0

; re-transpose to original orientation
nar=transpose(nar,P)
if ndim eq 1 then nar=reform(nar,(size(nar))(1))
if ndim eq 2 then nar=reform(nar,(size(nar))(1),(size(nar))(2))

end
