pro devauc_fit,rad,a,f
;-----------------------------------------------------------------------
;
; Name: DEVAUC_FIT
;
; Purpose: Fits a de Vaucouleur profile to data
;
; Inputs: NONE
;
; Comments: To run, must input as ".rnew seris_fit"
;
; Author: Clif Kirkpatrick
;
;-----------------------------------------------------------------------
;
; Define function being fit
;
   int0 = a(0)
   re = a(1)

   f = int0 * 2.71828182846^(-(rad/re)^0.25)

END

;
; Read in inputs
;
   read,redshift,prompt='		Redshift: '
   tablename=' '
   read,tablename,prompt='TAB file name: '
   tab_read,tablename,tcb,table,header
   rad=tab_val(tcb,table,1)
   int=tab_val(tcb,table,2)
   interr=tab_val(tcb,table,3)
   raderr=interr*0
   read,I0,prompt=' I0: '
   read,scl,prompt=' Re: '
   print,' '



;
; Compute angular scale
;
   angscale=1.0/zang(1.0,redshift,/silent)



;
; Convert to kpc
;
   rad=rad*0.28
   rad=rad*angscale



;
; Run CURVEFIT
;
   a = [I0,scl]
   w = 1/int
   yfit=curvefit(rad,int,w,a,sigma,function_name='devauc_fit',/noderivative)



;
; Output fit parameters
;
   print,'I0 = ',a(0)
   print,'Re = ',a(1)
   print,' '



;
; Set plot parameters
;
   plot,[0,5],[.01,100],psym=3,ystyle=1,xstyle=1,title=plottitle,ytitle='Intensity',xtitle='R!U1/4',/ylog



;
; Plot fit
;
   oploterror,rad^0.25,int,raderr,interr,psym=6
   oplot,rad^0.25,yfit,color=255



END
