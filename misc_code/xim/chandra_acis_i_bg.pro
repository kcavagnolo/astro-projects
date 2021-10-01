;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CALCULATE ACIS-S BACKGROUND ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro chandra_acis_i_bg,segrid,sb,iegrid,ib

;
; Sebastian Heinz, 02/26/2009
;
; Return skybackground and instrumental background for ACIS-I, based on energy
; grid, using blank sky background file
;
; Treat background entirely as instrumental (taken from raw detector data)
;

restore,'$XIMPATH/chandra_acis_i_bg.dat'    
sb=replicate(0.,n_elements(segrid)-1)
regrid,rate,oegrid,ib,iegrid

end
