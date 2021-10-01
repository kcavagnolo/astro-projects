;;;;;;;;;;;;
; IXOXMSBG ;
;;;;;;;;;;;;

pro ixoxmsbg,segrid,sb,iegrid,ib

;
; Sebastian Heinz, 02/15/2009
;
; Return instrument and sky background for input energy grids
;
; input:
;
;        segrid:     enegry grid for sky background
;
;        iegrid:     energy grid for instrument background (not convolved with
;                    response)  
;
; output:
;
;        sb:         sky background count rate per pixel in energy
;                    bin [ib(i),in()i+1)]
;
;        ib:         instrumental background count rate per pixel in energy
;                    bin [ib(i),in()i+1)]
;

restore,'$XIMPATH/ixobg_sky.dat'    
regrid,m,e,sb,segrid,1
neg=n_elements(iegrid)
ib=0.00025/(!pi*25.)*(iegrid(1:neg-1)-iegrid(0:neg-2))

end
