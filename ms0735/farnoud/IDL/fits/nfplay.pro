pro nfplay, name, magi, rb, g, b, a

;	mag0 (SB at break radius) = a(0)
;	rb (break radius) = a(1)
;	g (gamma) = a(2)
;	b (betta) = a(3)
;	a (alpha) = a(4)
	
	tab_read, name, tcb, table, header 
	rad = tab_val(tcb,table,1)
	maga = tab_val(tcb,table,18)

;	r = rad/rb
;	e = (g-b)/a
	mag = 2^(-(b-g)/a)*(magi * (rad/rb)^(g) * (1+(rad/rb)^(a))^(-(g-b)/a))
	resmag = (maga - mag) 
	
	!p.multi = [0,2,1]
	ymax = max(maga)
	ymin = min(maga)
	plot, rad, maga, psym=symcat(9),/xlog, yrange=[ymax,ymin] 
	;ymax1 = max(mag)
	;ymin1 = min(mag)
        oplot, rad, mag, color=255;, yrange=[ymax1,ymin1]
	!p.multi = [1,2,1]
	plot, rad, resmag, psym=2,/xlog

end