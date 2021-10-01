pro junk
rin  = [0,12,15,18,24,30,39,51,66,87,111,138,168,198,225,252,276,297,318,339]
rout = [12,15,18,24,30,39,51,66,87,111,138,168,198,225,252,276,297,318,339,372]
lumin = [6.47,2.76,2.76,4.45,3.64,4.63,4.42,4.08,4.44,4.27,4.54,2.92,3.04,2.43,2.86,2.14,1.82,1.69,1.70,2.16]

r = (rin+rout)/2.
r = r*0.492
cosmology,0.354,x,/silent
r = r*x[4]
tl = 0.
for i=0,n_elements(lumin)-1 do begin
    tl = tl+lumin[i]	    
    push, lx, tl
endfor
plot, r, lx/10., $
      /xlog, /ylog, $
      xtitle = 'R [kpc]', $
      ytitle = 'Lx [10^45 ergs/sec]'
y=replicate(3.8,20)
oplot,r,y,linestyle=3
y=replicate(4.5,20)
oplot,r,y,linestyle=2
y=replicate(5.2,20)
oplot,r,y,linestyle=3
y=replicate(0.3,20)
oplot,r,y,linestyle=5
end
