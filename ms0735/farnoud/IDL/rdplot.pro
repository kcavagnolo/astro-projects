pro rdplot, name
	
readcol, name, rah, ram, ras, decd, decm, decs
ra = (rah + ram/60 + ras/3600)*15
if (decd >= 0) then begin
dec = decd + decm/60 + decs/3600
endif else being
dec = decd - decm/60 - decs/3600

readcol, '30sGCsmatched', rah1,ram1,ras1,decd1,decm1,decs1
ra1 = (rah1 + ram1/60 + ras1/3600)*15
if (decd1 >= 0) then begin
dec1 = decd1 + decm1/60 + decs1/3600
endif else begin
dec1 = decd1 - decm1/60 - decs1/3600

readcol, '300sGCsmatched', rah2,ram2,ras2,decd2,decm2,decs2
ra2 = (rah2 + ram2/60 + ras2/3600)*15
if (decd2 >= 0) then begin
dec2 = decd2 + decm2/60 + decs2/3600
endif else begin
dec2 = decd2 - decm2/60 - decs2/3600

readcol, '600sGCsmatched', rah3,ram3,ras3,decd3,decm3,decs3
ra3 = (rah3 + ram3/60 + ras3/3600)*15
if (decd3 >= 0) then begin
dec3 = decd3 + decm3/60 + decs3/3600
endif else begin
dec3 = decd3 - decm3/60 - decs3/3600
	
set_plot, 'x'
;set_plot, 'ps'
;device, filename='dafaf.ps'

plot, ra, dec, psym=2, symsize=0.5, xtitle='DEC', ytitle='RA', xrange=[202.6,200.14], yrange=[-43.74,-42.32]
oplot, ra1, dec1, color=255, psym=2, symsize=0.5
oplot, ra2, dec2, color=255, psym=2, symsize=0.5
oplot, ra3, dec3, color=255, psym=2, symsize=0.5

;device, /close
set_plot, 'x'

end