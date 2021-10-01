pro serplay,tablename,z,inti,re,n
;-----------------------------------------------------------------------
;
; Name: SERPLAY
;
; Purpose: Manually fit a sersic profile
;
; Inputs: tablename - Name of input .tab file (String)
;         z - redshift
;         inti - central intensity
;         re - scale length
;         n - index (n=0.25 de Vaucouleur, n=1 exponential)
;
; Comments:
;
; Author: Clif Kirkpatrick
;
;-----------------------------------------------------------------------
;
; Read in data
;
   tab_read,tablename,tcb,table,header
   rad = tab_val(tcb,table,1)
   inta = tab_val(tcb,table,2)



   int=inti*2.71828182846^(-(rad/re)^n)
   resint = (inta - int)



;
; Compute angular scale
;
   angscale=1.0/zang(1.0,z,/silent)



;
; Convert to kpc
;
   rad=rad*0.28
   rad=rad*angscale



;
; Plot fit with residuals
;
   !p.multi = [0,2,1]
   plot, rad^0.25, inta ,psym=6 ,/ylog
   oplot, rad^0.25,int, color=255
   !p.multi = [1,2,1]
   plot, rad^0.25, resint ,psym=2



;
; End and return
;
   RETURN



END