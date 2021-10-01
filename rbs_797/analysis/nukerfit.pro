pro nukerfit, rad, a, f

	int0 = a(0)
	rb = a(1)
	g = a(2)
	b = a(3)
	al = a(4)

	r = rad/rb
	e = (g-b)/al
	f = int0 * r^(-g) * (1 + r^al)^e

end
	name = ' '
	read, name, prompt='Enter STSDAS Data File: '
	tab_read,name,tcb,table,header
	rad = tab_val(tcb,table,1)
	int = tab_val(tcb,table,2)
	mag = tab_val(tcb,table,18)
	Weights =1/int
	read, I0, prompt = 'Enter I0 value: '
	read, radb, prompt = 'Enter Rb value: '
	read, gam, prompt = 'Enter Gamma value: '
	read, bet, prompt = 'Enter Betta value: '
	read, alp, prompt = 'Enter Alpha value: '
	read, zeromag, prompt = 'Enter The Zero Point Magnitude: '

	a = [I0, radb,gam,bet,alp]
	yfit = curvefit(rad,int,Weights,a,sigma,function_name='nukerfit',/noderivative)
	yfitmag = -2.5*alog10(yfit) + zeromag
	resint = int - yfit
	resmag = mag - yfitmag
	mag0 = - 2.5*alog10(a(0)) + zeromag
	print,'MAG0=',mag0,' Rb=',a(1),' Gamma=',a(2),' Beta=',a(3),' Alpha=',a(4)
	print,'MAG0s=',sigma(0),' Rbs=',sigma(1),' Gammas=',sigma(2),' Betas=',sigma(3),' Alphas=',sigma(4)
	print, yfit

	set_plot, 'x'
	;set_plot, 'ps'
	;device, filename='Nukerfit.ps',/color
	loadct, 13
	!p.multi = [0,2,2]
	plot,rad,int,psym=6,xtitle='Radius (px)',ytitle='Intensity',/xlog
	oplot, rad, yfit,color=255
	!p.multi = [2,2,2]
	plot, rad, resint, psym=2, symsize=0.5,/xlog, xtitle='Radius (px)', ytitle='Residual Intensity'
	!p.multi = [3,2,2]
	ymin = min(mag)
	ymax = max(mag)
	plot,rad,mag,psym=6,xtitle='Radius (px)',ytitle='Apparent Magnitude',/xlog, yrange=[ymax, ymin]
	oplot, rad, yfitmag, color=255
	!p.multi = [1,2,2] 
	plot, rad, resmag, psym=2, symsize=0.5,/xlog, xtitle='Radius (px)', ytitle='Residual Magnitude'
;	legend, 'I0='+string(a(0))+', Rb='+string(a(1))+', gamma='+string(a(2))+', beta='+string(a(3))+', alpha='+string(a(4)),charsize=0.25
	;device, /close
	set_plot, 'x'

end