PRO optbin, data, minM, maxM
bins = indgen(maxM-minM+1)+minM
num = n_elements(data)
logp = fltarr(maxM+1)
FOR m=minM,maxM DO BEGIN
   n = histogram(data,NBINS=m)
   part1 = num*alog(m) + gammln(m/2.) - gammln(num+m/2.)
   part2 = -m*gammln(1./2.) + total(gammln(n+0.5))
   logp[m] = part1+part2
ENDFOR
;multiplot,[1,2]
;set_plot,'PS'
;device, filename='optbin.eps', $
;        /color, $
;        /encapsulated, $
;        /portrait, $
;        /helvetica, $
;        bits=16
plot,bins,logp,xtitle='Number of Bins, M',ytitle='Relative Log Posterior'
ord = where(logp EQ (max(logp)))
print,'Optimal num bins: ',bins[ord]
;dfhistoplot,data,nbins=bins[ord]
;device, /close
END
