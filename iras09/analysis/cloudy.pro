pro cloudy

z = 0.4418
d1 = 'oiiineb.ref'
readcol, d1, FORMAT='D,D,D,D,D,A', comment='#', $
         ener, reflc, refll, reflt, albed, lid
ener = ener/(1+z)
openw, /get_lun, lun, 'cl_qso.xcm'
printf, lun, 'data 1:1 IRAS_09104+4109_10445_ec_src1_grp.pi'
printf, lun, 'ignore 1:1-2,52'
printf, lun, 'cd /mnt/DROBO/10445/reprocessed'
printf, lun, 'backgrnd 1 IRAS_09104+4109_10445_ec_src1_ecbgd.pi'
printf, lun, 'statistic chi'
printf, lun, 'method leven 1000 0.001'
printf, lun, 'abund angr'
printf, lun, 'xsect bcmc'
printf, lun, 'cosmo 70 0 0.73'
printf, lun, 'xset FORCECALC  off'
printf, lun, 'model  wabs*mekal '
printf, lun, '         0.0158     -0.001          0          0     100000      1e+06'
printf, lun, '         3.8012       0.01     0.0808     0.0808       79.9       79.9'
printf, lun, '              1      -0.01      1e-06      1e-05      1e+19      1e+20'
printf, lun, '            0.5      -0.01          0          0       1000       1000'
printf, lun, '         0.4418      -0.01          0          0         10         10'
printf, lun, '              1'
printf, lun, '     2.5803e-05       0.01          0          0      1e+24      1e+24'
iter = 3
ord = where((ener GT 0.3) AND (ener LT 7.0))
maxfl = max(refll[ord])
for i=0,n_elements(refll)-1 DO BEGIN
   if ener[i] GE 0.3 AND ener[i] LE 8.0 then begin
      if refll[i] GE 1e-8 then begin
         printf, lun, 'addcomp '+num2str(iter)+' ga'
         printf, lun, num2str(ener[i])+',0'
         printf, lun, '1e-6,0'
         printf, lun, num2str(refll[i]/maxfl)+'e-6'
         iter++
      endif
      if refll[i] GE 1e-5 then $
         print, num2str(ener[i])+' keV'+' --- '+lid[i]
      endif
endfor
close, lun

;plot,ener/(1+z),refll,xran=[0.7,7.0],/xsty
;stop

end
