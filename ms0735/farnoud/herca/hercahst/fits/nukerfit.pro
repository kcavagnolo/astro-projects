pro nukerfit, rad, a, f

!p.thick = 2.5
!P.charthick = 2.5
!x.thick = 2.5
!y.thick = 2.5

;****************************************************************************************************************************************************
; 	This is a Nuker Law fitting routine, it is ran by typing ".rnew nukerfit". The background value is added manually in order to estimate the systematic errors.
; 	By: FK June, 2007
;****************************************************************************************************************************************************

; The following are the parameters that will be fitted, the program will ask for the initial guesses of these values which can be 'playfully' deduced  from the "nfply.pro" program
	mag0 = a(0)
	rb = a(1)
	g = a(2)
	b = a(3)
	al = a(4)
; Nuker Law
	f = mag0 * (rad/rb)^(g) * (1 + (rad/rb)^(al))^(-(g-b)/al)
end

; Name of the data file containing the SB info
	name = ' '
	read, name, prompt='Enter Data File Name: '
	readcol,name,rad,ell,ellerr,pa,paerr,mag,maglerr,maguerr,flux, /silent
	s = size(rad,/dimensions)
	back = 0.063
	backsig = 0.0004
	magcst = 18.842
	int = 10^(-0.4*(mag-magcst))
	maglen = magcst - 2.5 * alog10(int-back)
	maguen = magcst - 2.5 * alog10(int+back)
	maglerr = (maglerr^2 + backsig^2)^0.5
	maguerr = (maguerr^2 + backsig^2)^0.5
	magerr = (maguerr^2 + maglerr^2)^0.5
	
	w = indgen(s[0])
	w = w/w
;	weights = w			; no weighting
;	weights = 1/mag			; Poisson, statistical weighting
	weights = 1/magerr^2		; Gaussian, instrumental weighting
; I0 is the initial or intensity value as r --> 0
	read, mag0, prompt = 'Enter Mag0 value: '
; Rb is the value of radius as which the 'turnover' is observed
	read, radb, prompt = 'Enter Rb value: '
; Gamma is he slope of the inner section, 'core' slope
	read, gam, prompt = 'Enter Gamma value: '
; Beta is the slope of the wing
	read, bet, prompt = 'Enter Betta value: '
; Alpha is the sharpness of the turnover
	read, alp, prompt = 'Enter Alpha value: '
; If a hard copy is needed, answer 'yes'
	ps = ' '
	read, ps, prompt = 'Hard Copy? '


; The following code will find the best fit to the actual data and print the value of all the parameters with their standard deviations on the screen, and on the hard copy if that option is chosen. 
	a = [mag0, radb,gam,bet,alp]
	yfit = curvefit(rad,mag,weights,a,sigma,function_name='nukerfit',/noderivative,yerr=yerr)
	resmag = mag - yfit
	print, 'Mag0=',a(0), ', Rb=',a(1),', Gamma=',a(2),', Beta=',a(3),', Alpha=',a(4), f='(A,F8.5,A,F10.4,A,F6.3,A,F6.4,A,F5.3,A,F5.3)'
	print, 'Mag0s=',sigma(0), ', Rbs=',sigma(1),', Gammas=',sigma(2),', Betas=',sigma(3),', Alphas=',sigma(4), f='(A,F8.5,A,F5.3,A,F5.3,A,F6.4,A,F5.3,A,F5.3)'
	print, 'YErr=',yerr, f='(A,F7.3)'

rad = rad * 0.051

	if ps eq 'yes' then begin
		set_plot, 'x'
		set_plot, 'ps'
		device, filename='nukerfit.ps',/color,/encapsulated,/times,ys=12,xs=13.33
		loadct, 13
		!p.multi=[0,1,1,0,0]
		multiplot,[1,2]
		ymin = min(mag) - 1
		ymax = max(mag) + 2
		plot,rad,mag,psym=symcat(9),symsize=0.5,ytitle='!4l!6!DV,o!N (mag arcsec!E-2!N)',yrange=[ymax, ymin],/ystyle,/xlog,xrange=[0.02,30],/xstyle
		oploterror,rad,mag,maglerr,/lobar,psym=3,/nohat
		oploterror,rad,mag,maguerr,/hibar,psym=3,/nohat
		oplot, rad, yfit,color=255
		oplot, rad,maguen,linestyle=2
		oplot, rad,maglen,linestyle=2
		multiplot
		plot, rad, resmag, psym=symcat(16), symsize=0.3, xtitle='!6Radius (arcsec)', ytitle='!6Residual',/xlog,xrange=[0.02,30],/xstyle
		!P.linestyle = 1
		arrow, 0.02,0,30,0, fill=0,/data,shaft=0.1,width=0
		!P.linestyle = 0
		multiplot,/reset
		device, /close
		set_plot, 'x'
	endif else begin
		erase
		set_plot, 'x'
		loadct, 13
		!p.multi=[0,1,1,0,0]
		multiplot,[1,2]
		ymin = min(mag) - 1
		ymax = max(mag) + 2
		plot,rad,mag,psym=symcat(9),symsize=0.9,ytitle='!4l!6!DV,o!N (mag arcsec!E-2!N)',yrange=[ymax, ymin],/ystyle,/xlog,xrange=[0.02,30],/xstyle
		oploterror,rad,mag,maglerr,/lobar,psym=3,/nohat
		oploterror,rad,mag,maguerr,/hibar,psym=3,/nohat
		oplot, rad, yfit,color=255
		oplot, rad,maguen,linestyle=2
		oplot, rad,maglen,linestyle=2
		multiplot
		plot, rad, resmag, psym=symcat(16), symsize=0.5, xtitle='!6Radius (arcsec)', ytitle='!6Residual',/xlog,xrange=[0.02,30],/xstyle
		!P.linestyle = 1
		arrow, 0.02,0,30,0, fill=0,/data,shaft=0.1,width=0
		!P.linestyle = 0
		multiplot,/reset
		set_plot, 'x'
	endelse

end