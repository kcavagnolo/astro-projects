PRO lcool

readcol, '../mc_ge/summaryb_loZ', format='A,D,D,D,D,D,D,D,D,D,D', comment='#', $
         name, lx, jsa, jsa, jsa, jsa, jsa, jsa, jsa, jsa, jsa
for i=0,n_elements(name)-1 DO BEGIN
   print, name[i], '    ', lx[i]/1d33
endfor

END
