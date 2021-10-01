pro fit_beta,inpfile,initvals=initvals,outfile=outfile,plotfile=plotfile,  $
             fitrange=fitrange,deriv=deriv,parms=parms,stats=stats,        $
             quiet=quiet,fitcurve=fitcurve,nocolor=nocolor
;-----------------------------------------------------------------------
; Name: FIT_BETA
;
; Purpose:  Fits a Beta model profile to the specified surface
;	    brightness profile (produced by CALC_SB_IMAGE)
;
; Inputs: inpfile  -- string containing name of input file
;         initvals -- vector containing initial parameter guesses
;         outfile  -- string containing name of output fit file
;         plotfile -- string containing name of output PS file
;         fitrange -- vector containing data limits to fit
;         deriv    -- flag controlling derivative calculation
;         parms    -- vector of fit parameters and errors
;         stats    -- fit statistics
;         quiet    -- flag controlling screen output
;         fitcurve -- vector containg fit
;         nocolor  -- flag controlling color PS option
;
; Comments: Uses IDL's CURVEFIT routine to perform the fit.
;           
; Revision history:
;
;       written by Amalia Hicks			07/24/00
;
;	modified to allow variable		10/24/00
;	fitting region (MW)
;
;       added const. to beta model		10/25/00
;
;-----------------------------------------------------------------------
;
;
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 6)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'fit_beta,inpfile,initvals=initvals,outfile=outfile,plotfile=plotfile,', $
   '         fitrange=fitrange,deriv=deriv,parms=parms,stats=stats,',       $
   '         quiet=quiet,fitcurve=fitcurve,nocolor=nocolor'
   return   
endif



;
; Read in the surface brightness profile to be fit
;
readcol,inpfile,r1,r2,sb,esb,area,/silent
r=0.5*(r1+r2)


;
; Define the range over which to perform the fit
;
if (n_elements(fitrange) eq 0) then begin
   fitrange=[min(r),max(r)]
endif else begin
   s=size(fitrange)
   if (s(0) eq 0) then fitrange=[min(r),fitrange]
endelse

i=where( (r ge fitrange(0)) and (r le fitrange(1)) )
x=r(i)
y=sb(i)
err=esb(i)



;
; Filter points which are less than 0
;
i=where(y gt 0.0)
x=x(i)
y=y(i)
err=err(i)



;
; Define weights for fitting (using instrumental weighting)
;
num=n_elements(x)
weights=1.0/(err*err)


;
; Set initial guesses
;
if (n_elements(initvals) eq 0) then begin
;
;  Estimate the core radius
;
   nums=n_elements(sb)
   t=fltarr(nums)
   for i=0,nums-1 do begin
       t(i)=total(sb(0:i))
   endfor

   t=t/max(t)
   i=where(t ge 0.5)

   a=[y(0),r(i(0)),0.6,1.0e-10]

endif else begin
   s=size(initvals)
   if (s(1) ne 4) then begin
      print,string(7B),'ERROR: Incorrect number of initial parameter values'
      return   
   endif else begin
      a=initvals  
   endelse
endelse


;
; Perform the fit
;
if keyword_set(deriv) then begin

   yfit=curvefit(x,y,weights,a,sigma,chisq=chisq,                 $
                 function_name='beta_model',tol=1.0e-12,          $
                 itmax=200,iter=niter,/noderivative)

endif else begin

   yfit=curvefit(x,y,weights,a,sigma,chisq=chisq,                 $
                 function_name='beta_model',tol=1.0e-12,          $
                 itmax=200,iter=niter) 

endelse


;
; Pass the fit parameters, errors, and statistics back
;
parms=fltarr(4,2)
parms(0,0)=a(0)
parms(1,0)=a(1)
parms(2,0)=a(2)
parms(3,0)=a(3)
parms(0,1)=sigma(0)
parms(1,1)=sigma(1)
parms(2,1)=sigma(2)
parms(3,1)=sigma(3)

stats=[chisq,niter]


;
; Use fit parameters to calculate model curve
;
beta_model,r,a,fitcurve



;
; Write the results of the fit to the screen if indicated
;
if (not keyword_set(quiet)) then begin
   print,' '
   print,'---------------------------------------------------------------'
   print,'Beta model fit results'
   print,'---------------------------------------------------------------'
   print,' '
   print,'Creation date : ',!stime
   print,'Input file    : ',inpfile
   print,' '
   print,'---------------------------------------------------------------'
   print,'Best fit parameters
   print,'---------------------------------------------------------------'
   print,'$(a16,2(3x,e11.5))','Normalization : ',parms(0,0),parms(0,1)
   print,'$(a16,2(3x,f11.5))','Core radius   : ',parms(1,0),parms(1,1)
   print,'$(a16,2(3x,f11.5))','Beta value    : ',parms(2,0),parms(2,1)
   print,'$(a16,2(3x,e11.5))','Background    : ',parms(3,0),parms(3,1)
   print,' '
   print,'Number of iterations: ',stats(1)
   print,'Reduced chi-squared : ',stats(0)
   print,'---------------------------------------------------------------'
   print,' '
endif



;
; Write the output file if indicated
;
if (n_elements(outfile) ne 0) then begin

   get_lun,unit
   openw,unit,outfile

   printf,unit,' '
   printf,unit,'---------------------------------------------------------------'
   printf,unit,'Beta model fit results'
   printf,unit,'---------------------------------------------------------------'
   printf,unit,' '
   printf,unit,'Creation date : ',!stime
   printf,unit,'Input file    : ',inpfile
   printf,unit,' '
   printf,unit,'---------------------------------------------------------------'
   printf,unit,'Best fit parameters
   printf,unit,'---------------------------------------------------------------'
   printf,unit,'$(a16,2(3x,e11.5))','Normalization : ',parms(0,0),parms(0,1)
   printf,unit,'$(a16,2(3x,f11.5))','Core radius   : ',parms(1,0),parms(1,1)
   printf,unit,'$(a16,2(3x,f11.5))','Beta value    : ',parms(2,0),parms(2,1)
   printf,unit,'$(a16,2(3x,e11.5))','Background    : ',parms(3,0),parms(3,1)
   printf,unit,' '
   printf,unit,'Number of iterations: ',stats(1)
   printf,unit,'Reduced chi-squared : ',stats(0)
   printf,unit,'---------------------------------------------------------------'
   printf,unit,' '
   printf,unit,'$(3(5x,a11))',' Radius  ','  Total  ',' Cluster '
   printf,unit,'$(3(5x,a11))','---------','---------','---------'
   
   num=n_elements(r)
   for i=0,num-1 do begin
       printf,unit,'$(5x,f11.5,2(5x,e11.5))',r(i),fitcurve(i), $
                   fitcurve(i)-parms(3,0)
   endfor

   close,unit
   free_lun,unit

endif

;
; Output beta function parameters to file
; (added by D Rafferty)
;
get_lun,unit
openw,unit,'beta_params.dat'
printf,unit,'Norm, Norm error, Core radius, radius error, Beta, Beta error'
printf,unit,parms(0,0),parms(0,1),parms(1,0),parms(1,1),parms(2,0),parms(2,1)
close,unit
free_lun,unit


;
; Create PS file of fit if indicated
;
if (n_elements(plotfile) ne 0) then begin

   i=where(sb gt 0.0)

   xmin=0.9*min(r)
   xmax=1.1*max(r)
   ymin=min(fitcurve-parms(3,0))
   ymax=2.0*max(sb)
   dy=(alog10(ymax)-alog10(ymin))/20.0
   dx=(alog10(xmax)-alog10(xmin))/20.0
   xpos=10.0^(alog10(xmin)+dx)   

   if keyword_set(nocolor) then begin

;
;     Make black and white PS file if indicated
;
;      open,'/landscape',plotfile
	!p.font=0
	set_plot,'ps'
	device,set_font='Times-Roman',file=plotfile,landscape=1

      plot_oo,r(i),sb(i),psym=10,xrange=[xmin,xmax],/xst,        $
              yrange=[ymin,ymax],/yst,                           $
              xtitle='!6R [pixels]',                             $
              ytitle='!6I!DX!N [ph s!E-1!N cm!E-2!N pix!E-2!N]', $
              title='!6'+inpfile

      oplot,r,fitcurve,line=2
      oplot,r,fitcurve-parms(3,0),line=2

      oplot,[xmin,xmax],[parms(3,0),parms(3,0)],line=1
      oplot,[fitrange(0),fitrange(0)],[ymin,ymax],line=1
      oplot,[fitrange(1),fitrange(1)],[ymin,ymax],line=1


      ypos=10.0^(alog10(ymin)+6.0*dy)
      curstring='Normalization : '+string(parms(0,*),format='$(2(3x,e11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+5.0*dy)
      curstring='Core radius   : '+string(parms(1,*),format='$(2(3x,f11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+4.0*dy)
      curstring='Beta value    : '+string(parms(2,*),format='$(2(3x,f11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+3.0*dy)
      curstring='Background   :  '+string(parms(3,*),format='$(2(3x,e11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+2.0*dy)
;      curstring='Reduced !7v!6!E2!N    : '+string(chisq,format='$(3x,f11.5)')
	curstring='Reduced ChiSq   : '+string(chisq,format='$(3x,f11.5)')
      xyouts,xpos,ypos,curstring

;      shut
	device, /close

   endif else begin

;
;     Make color PS file by default
;
      open,'/lcolor',plotfile

      set_color

      plot_oo,r,sb,xrange=[xmin,xmax],/xst,                      $
              yrange=[ymin,ymax],/yst,                           $
              xtitle='!6R [pixels]',                             $
              ytitle='!6I!DX!N [ph s!E-1!N cm!E-2!N pix!E-2!N]', $
              title='!6'+inpfile,/nodata

      oplot,r(i),sb(i),psym=10,color=3
      oplot,r,fitcurve,color=1
      oplot,r,fitcurve-parms(3,0),color=1,line=2

      oplot,[xmin,xmax],[parms(3,0),parms(3,0)],line=1,color=2
      oplot,[fitrange(0),fitrange(0)],[ymin,ymax],line=1,color=2
      oplot,[fitrange(1),fitrange(1)],[ymin,ymax],line=1,color=2

 
      ypos=10.0^(alog10(ymin)+6.0*dy)
      curstring='Normalization : '+string(parms(0,*),format='$(2(3x,e11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+5.0*dy)
      curstring='Core radius   : '+string(parms(1,*),format='$(2(3x,f11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+4.0*dy)
      curstring='Beta value    : '+string(parms(2,*),format='$(2(3x,f11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+3.0*dy)
      curstring='Background   :  '+string(parms(3,*),format='$(2(3x,e11.5))')
      xyouts,xpos,ypos,curstring

      ypos=10.0^(alog10(ymin)+2.0*dy)
      curstring='Reduced !7v!6!E2!N    : '+string(chisq,format='$(3x,f11.5)')
      xyouts,xpos,ypos,curstring

      shut

   endelse

endif



;
; Return to IDL
;
end






