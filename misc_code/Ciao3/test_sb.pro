pro test_sb
;
; Set plot parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='sb_test.ps',/color,landscape=1
!p.multi=[0,2,2]


readcol, './spectra/sbnew.dat',skipline=16,r1,r2,sb_ec,sb_ecerr,sb,sberr,area


;
; Plot SB vs. radius
;
scale=0.4919
ravgsb=(r1+r2)*scale/2.0
sb=sb/scale^2				; convert to cnts/s/cm^2/arcsec^2
sb_elements=where(sb gt 0.0, count)
sb_nonzero=sb[sb_elements]
plotsym,0,0.4
ploterror,ravgsb,sb_nonzero,sberr,psym=8,hatlength=100,/ylog,$
     yrange=[min(sb_nonzero),max(sb)*1.1],$
     title='Surface Brightness Profile',$
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N arcsec!E-2!N)'

;
; Plot SB_EC vs. radius
;
sb_ec=sb_ec/scale^2				; convert to cnts/s/cm^2/arcsec^2
sb_ec_elements=where(sb_ec gt 0.0, count)
sb_ec_nonzero=sb_ec[sb_ec_elements]
plotsym,0,0.4
ploterror,ravgsb,sb_ec_nonzero,sb_ecerr,psym=8,hatlength=100,/ylog,$
     yrange=[min(sb_ec_nonzero),max(sb_ec)*1.1],$
     title='Surface Brightness Profile',$
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N cm!E-2!N arcsec!E-2!N)'


device, /close

;
; Return to IDL
;
return
end
