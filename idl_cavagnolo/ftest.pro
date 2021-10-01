;===============================================
; straight line
pro fline,x,a,f,pder
f = a(0)+x*a(1)
end

;===============================================
; Test of CURVEFIT vs. LINFIT
pro ftest,noise

; sample calling sequence : ftest,0.4

if n_elements(noise) EQ 0 THEN noise = 0.4

   ; generate noisy line
f0 = (findgen(25)*3.-15)*(1.+randomn(seed,25)*noise)
x = findgen(25)
loadct,4
plot,x,f0,xstyle=2,psym=-1

   ; straight-line chi-square fitting with LINFIT.PRO
a = linfit(x,f0,chisq=csq,sigma=sig)
fline,x,a,f & oplot,x,f,color=80
fline,x,[a+sig],f & oplot,x,f,linestyle=2,color=80
fline,x,[a-sig],f & oplot,x,f,linestyle=2,color=80
print,'LINFIT parameters, sigma, and chi-square : '
print,a,sig,csq

   ; Levenberg-Marquardt chi-square fitting
   ;of straight line with CURVEFIT.PRO
a = [0.,1.]
weights = fltarr(n_elements(x))+1.
f=curvefit(x,f0,weights,a,sig,function_name='fline',chisq=csq,/noderivative)
oplot,x,f,color=200
oplot,x,a(0)+sig(0)+x*(a(1)+sig(1)),linestyle=2,color=200
oplot,x,a(0)-sig(0)+x*(a(1)-sig(1)),linestyle=2,color=200
print,'CURVEFIT parameters, sigma, and chi-square : '
print,a,sig,csq

end
;===============================================
