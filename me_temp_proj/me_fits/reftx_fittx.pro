pro reftx_fittx

dat=mrdfits('HSTprop_r2500-50_nhfro_fefree.fits',1)
set_plot, 'PS'
device, filename = 'txref.ps'
!fancy = 4
!linetype = 0
!p.font = 0
plotsym, 0, 0.8, /fill
plot,dat.txref,dat.tx77,$
     psym = 8 ,$
     xtitle = 'Ref Tx77',$
     ytitle = 'Fit Tx',$
     /xsty, /ysty,$
     /xlog, /ylog,$
     xran = [4.5,22],$
     yran = [4.5,22], $
     charsize = 0.7, $
     symsize = 0.5
oploterror,dat.txref,dat.tx77,dat.tx77-dat.tx77lo,/lobar,psym=3
oploterror,dat.txref,dat.tx77,dat.tx77hi-dat.tx77,/hibar,psym=3
oplot,findgen(100),findgen(100)
oplot,findgen(100),findgen(100)+0.2*findgen(100),linestyle=2
oplot,findgen(100),findgen(100)-0.2*findgen(100),linestyle=2

tx77 = dat.tx77
txref = dat.txref
names = dat.cluster
FOR i=0,n_elements(dat.cluster)-1 DO BEGIN
    IF ((tx77[i]/txref[i] GE 1.2) OR (tx77[i]/txref[i] LE 0.8)) THEN BEGIN
        push, list, names[i]
    ENDIF ELSE BEGIN
        push, list, " "
    ENDELSE
ENDFOR

xyouts, dat.txref, dat.tx77, list, charthick=0.5, charsize=0.5, align=0.5
device, /close

END
