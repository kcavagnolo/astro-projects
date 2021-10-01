;+
; NAME:
;   dav_test
;
; PURPOSE:
;; Simple test harness
;
; CALLING SEQUENCE:
;   dav_test
;
; INPUTS:
;   'tftest.dat'
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   @Revision@
;-
;; Simple test harness
function dav_test
  dav_constants
  lcdm = dav_lcdm()
  params = dav_params()
  restore, 'tftest.dat'
  plot,tf[*,0],tf[*,1],/xlog,/ylog,xrange=[Tmin, Tmax],xstyle=1,yrange=[5E-11,7E-5],ystyle=1, $
       xtitle=textoidl('T_X [keV]'),ytitle=textoidl('dn_M/dlnT_X (h^3_{70} Mpc^{-3})'),yminor=10
  hist = dav_dndt(Tmin, Tmax, 0.001, 10, lcdm, params)
  oplot,hist[*,0],hist[*,1],psym=0, linestyle=2
  hist = dav_dndt(Tmin, Tmax*2, 0.5, 10, lcdm, params)
  oplot,hist[*,0],hist[*,1],psym=0, linestyle=2
  hist = dav_dndt(Tmin, Tmax, 1.0, 10, lcdm, params)
  oplot,hist[*,0],hist[*,1],psym=0, linestyle=2
;;   f = dav_fisher_matrix(params, lcdm)
  f=1
  return, f
end
