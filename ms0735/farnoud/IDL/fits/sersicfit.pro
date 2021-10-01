pro sersicfit, rad, a, f

!p.thick= 2.5
!P.charthick = 2.5
!x.thick = 2.5
!y.thick = 2.5
!p.charsize= 1

;**************************************************************************
; This is a Sersic Law fitting routine, it is ran by typing ".rnew sersicfit"
; By: FK, Jun2008
;**************************************************************************
; The following are the parameters that will be fitted, the program will ask for the initial guesses of these values which can be 'playfully' deduced  from the "sfplay.pro" program
	mage = a(0)
	re = a(1)
	n = a(2)

; Sersic law
	pow = 1/n
	k = (2*n) - 0.331
	r = rad/re
	f = mage + k*((r^pow) - 1)
end

; Name of the data file containing the SB info
	name = ' '
	read, name, prompt='Enter Data File Name: '
	readcol,name,rad,ell,ellerr,pa,paerr,mag,maglerr,maguerr,flux,/silent
	s = size(rad,/dimensions)
	back = 379.946
	magcst = 19.9714
	int = 10^(-0.4*(mag-magcst))
	maglen = magcst - 2.5 * alog10(int-back)
	maguen = magcst - 2.5 * alog10(int+back)
	magerr = (maguerr^2 + maglerr^2)^0.5

	w = indgen(s[0])
	w = w/w
	weights = w			; no weighting
;	weights = 1/mag			; Poisson, statistical weighting
;	weights = 1/magerr^2		; Gaussian, instrumental weighting
; Mage is the initial or intensity value as r --> Re
	read, mage, prompt = 'Enter Mage value: '
; Re is the value of radius as which the half light is observed
	read, re, prompt = 'Enter Re value: '
; N is the form of the curve 
	read, n, prompt = 'Enter n value: '
; If a hard copy is needed, answer 'yes'
	ps = ' '
	read, ps, prompt = 'Hard Copy? '

; The following code will find the best fit to the actual data and print the value of all the parameters with their standard deviations on the screen, and on the hard copy if that option is chosen. 
	a = [mage, re, n]
	yfit = curvefit(rad,mag,Weights,a,sigma,function_name='sersicfit',/noderivative, chi2=chi2)
	resmag = mag - yfit
	print, 'Mage= ',a[0], ', Re= ',a[1],', N= ',a(2)
	print, 'Mages= ',sigma[0], ', Res= ',sigma(1),', Ns= ',sigma(2)
	print, 'Chi2=',chi2, f='(A,F7.3)'

rad = rad * 1

	if ps eq 'yes' then begin
		set_plot, 'x'
		set_plot, 'ps'
		device, filename='sersicfit.ps',/color,/encapsulated,/times,ys=11,xs=12
		loadct, 13
		!p.multi=[0,1,1,0,0]
		multiplot,[1,2]
		ymin = min(mag) - 1
		ymax = max(mag) + 2
		plot,rad,mag,psym=symcat(9),symsize=0.5,ytitle='!4l!6!DV,o!N (mag arcsec!E-2!N)',yrange=[ymax, ymin],/ystyle,/xlog,xrange=[0.4,500],/xstyle
		oploterror,rad,mag,maglerr,/lobar,psym=3,/nohat
		oploterror,rad,mag,maguerr,/hibar,psym=3,/nohat
		oplot, rad, yfit,color=255
;		oplot, rad,maguen,linestyle=2
;		oplot, rad,maglen,linestyle=2
		multiplot
		plot, rad, resmag, psym=symcat(16), symsize=0.5, xtitle='!6Radius (arcsec)', ytitle='!6Residual',/xlog,xrange=[0.4,500],/xstyle
		!P.linestyle = 1
		arrow, 0.4,0,500,0, fill=0,/data,shaft=0.1,width=0
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
		plot,rad,mag,psym=symcat(9),symsize=0.9,ytitle='!4l!6!DV,o!N (mag arcsec!E-2!N)',yrange=[ymax, ymin],/ystyle,/xlog,xrange=[0.4,500],/xstyle
		oploterror,rad,mag,maglerr,/lobar,psym=3,/nohat
		oploterror,rad,mag,maguerr,/hibar,psym=3,/nohat
		oplot, rad, yfit,color=255
;		oplot, rad,maguen,linestyle=2
;		oplot, rad,maglen,linestyle=2
		multiplot
		plot, rad, resmag, psym=symcat(16), symsize=0.5, xtitle='!6Radius (arcsec)', ytitle='!6Residual',/xlog,xrange=[0.4,500],/xstyle
		!P.linestyle = 1
		arrow, 0.4,0,500,0, fill=0,/data,shaft=0.1,width=0
		!P.linestyle = 0
		multiplot,/reset
		set_plot, 'x'
	endelse


end