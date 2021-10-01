pro nfplay, name, inti, rb, g, b, a

;	inti = a(0)
;	rb = a(1)
;	g = a(2)
;	b = a(3)
;	a = a(4)
	
	tab_read, name, tcb, table, header 
	rad = tab_val(tcb,table,1)
	inta = tab_val(tcb,table,2)

	r = rad/rb
	e = (g-b)/a
	int = inti * r^(-g)* (1+r^a)^e
	resint = (inta - int) 
	
	!p.multi = [0,2,1]
	plot, rad^0.25, inta ,psym=6
        oplot, rad^0.25,int, color=255
	!p.multi = [1,2,1]
	plot, rad^0.25, resint ,psym=2

end