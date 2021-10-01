PRO feas

readcol, 'rosat_sbr.dat', format='F,F,F,F,F,F,F,F,F,F,A,A', comment='#', $
         reg, counts, err, bgd, berr, area, sbr, sbrerr, rin, rout, ang1, ang2
expt = 1.687300d4
scale = 45.0
z = 0.0726
cosmology, z, result, /silent
dl = result[4]
rate = counts/expt
area = area*scale^2.
radius = rout*scale*dl
print, 'counts--rate--area--radius'
FOR i=0,n_elements(rate)-1 DO BEGIN
   print, counts[i], rate[i], area[i], radius[i]
ENDFOR
print, 'total rate', total(rate)
print, 'total area', total(area)
END
