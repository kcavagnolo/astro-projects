pro locate_center,events,chip=chip,window=window,outfile=outfile,     $
                  xcen=xc,ycen=yc,image=image,fit=fit,                $
                  xybin=xybin,weights=weights,ptitle=ptitle                  
;-----------------------------------------------------------------------
; Name: LOCATE_CENTER
;
; Purpose: Attempts to locate the center of a peaked surface
;          brightness distribution using a 2D surface fit.
;          
; Inputs: 
;         
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 03-06-01
;       added error output	 03-29-01
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 5)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'locate_center,events,chip=chip,window=window,outfile=outfile,',     $
   '              xcen=xc,ycen=yc,image=image,fit=fit,',                $
   '              xybin=xybin,weights=weights,ptitle=ptitle'                  
   return   
endif

;
; Handle defaults
;
if (n_elements(xybin) eq 0) then xybin=1.0		; 1 ACIS pixel
if (n_elements(window) eq 0) then window=50.0
if (n_elements(ptitle) eq 0) then ptitle='Fit Results  '


;
; First screen all events for this chip if desired
;
if (n_elements(chip) eq 0) then begin
   data=events
endif else begin
   i=where(events.ccd_id eq chip)
   data=events(i)
endelse


;
; Calculate coordinate histogams
;
plothist,data.x,xx,yx,bin=xybin,/noplot
plothist,data.y,xy,yy,bin=xybin,/noplot


;
; Fit Gaussians to determine initial guess of peak position
;
xfit=gaussfit(xx,yx,ax)
yfit=gaussfit(xy,yy,ay)
xc0=ax(1)
yc0=ay(1)


;
; Create image of central window
;
x0=xc0-window
y0=yc0-window
image=make_image(data.x,data.y,                           $
                 xrange=[xc0-window,xc0+window],          $
                 yrange=[yc0-window,yc0+window],          $
                 xbinsize=xybin,ybinsize=xybin)


;
; Define weights array 
;
if (n_elements(weights) ne 0) then begin
   weights=1.0/(float(image)+0.1)
endif else begin
   weights=image
   weights(*)=1.0
endelse


;
; Do the surface fit
;
fit=mpfit2dpeak(image,afit,weights=weights,perror=perror, $
                      /tilt,/lorentzian)


;
; Transform center back to Sky X and Y
;
xc=x0+afit(4)*xybin
yc=y0+afit(5)*xybin
dxc=afit(2)*xybin
dyc=afit(3)*xybin
ang=afit(6)*(180.0/!pi)

exc=perror(4)*xybin
eyc=perror(5)*xybin
edxc=perror(2)*xybin
edyc=perror(3)*xybin
eang=perror(6)*(180.0/!pi)




;
; Set up plotting environment
;
!p.multi=[0,2,2,0]


;
; Image
;
plotimage,bytscl(smooth(image,3)),xtitle='!7D!6X',ytitle='!7D!6Y', $
          title='Raw Image'
oplot,[afit(4),afit(4)],[0,2*window/xybin]
oplot,[0,2*window/xybin],[afit(5),afit(5)]

;
; Fit
;
plotimage,bytscl(fit),xtitle='!7D!6X',ytitle='!7D!6Y', $
          title='Fit Image'
oplot,[afit(4),afit(4)],[0,2*window/xybin]
oplot,[0,2*window/xybin],[afit(5),afit(5)]

;
; Residuals
;
plotimage,bytscl(smooth(image-fit,3)),xtitle='!7D!6X',ytitle='!7D!6Y', $
          title='Residuals'
oplot,[afit(4),afit(4)],[0,2*window/xybin]
oplot,[0,2*window/xybin],[afit(5),afit(5)]

;
; Results 
;
fmt='$(f7.2)'
underline='__________________________________________________'
dum=findgen(100)+1
plot,dum,dum,/nodata,xstyle=5,ystyle=5
xyouts,10,90,'!6'+ptitle,charsize=2.0
xyouts,10,85,'!6'+strmid(underline,0,strlen(ptitle)+4),charsize=2.0

tx=strtrim(string(xc,format=fmt),2)+'!9+!6'+strtrim(string(exc,format=fmt),2)
xyouts,10,75,'!6X!Dc!N='+tx,charsize=2.0

ty=strtrim(string(yc,format=fmt),2)+'!9+!6'+strtrim(string(exc,format=fmt),2)
xyouts,10,65,'!6Y!Dc!N='+ty,charsize=2.0

xyouts,10,50,'!7D!6X!Dc!N='+strtrim(string(dxc),2),charsize=2.0
xyouts,10,40,'!7D!6Y!Dc!N='+strtrim(string(dyc),2),charsize=2.0

e=min([dxc,dyc])/max([dxc,dyc])
ee=e*sqrt( (edxc/dxc)*(edxc/dxc) + (edyc/dyc)*(edyc/dyc) )
te=strtrim(string(e,format=fmt),2)+'!9+!6'+strtrim(string(ee,format=fmt),2)
xyouts,10,25,'!7e!6='+te,charsize=2.0

ta=strtrim(string(ang,format=fmt),2)+'!9+!6'+strtrim(string(eang,format=fmt),2)
xyouts,10,15,'!7h!6='+ta,charsize=2.0




;
; Return to default plotting environment
;
!p.multi=[0,0,0,0]



;
; If indicated, create a postscript plot
;
if (n_elements(outfile) ne 0) then begin

   ftemp=!fancy
   !fancy=1
   !p.multi=[0,2,2,0]

   open,'/psquare',outfile

   ;
   ; Image
   ;
   plotimage,bytscl(smooth(image,3)),xtitle='!7D!6X',ytitle='!7D!6Y', $
             title='Raw Image'
   oplot,[afit(4),afit(4)],[0,2*window/xybin],color=255
   oplot,[0,2*window/xybin],[afit(5),afit(5)],color=255

   ;
   ; Fit
   ;
   plotimage,bytscl(fit),xtitle='!7D!6X',ytitle='!7D!6Y', $
             title='Fit Image'
   oplot,[afit(4),afit(4)],[0,2*window/xybin],color=255
   oplot,[0,2*window/xybin],[afit(5),afit(5)],color=255

   ;
   ; Residuals
   ;
   plotimage,bytscl(smooth(image-fit,3)),xtitle='!7D!6X',ytitle='!7D!6Y', $
             title='Residuals'
   oplot,[afit(4),afit(4)],[0,2*window/xybin],color=255
   oplot,[0,2*window/xybin],[afit(5),afit(5)],color=255

   ;
   ; Results 
   ;
   dum=findgen(100)+1
   plot,dum,dum,/nodata,xstyle=5,ystyle=5

   xyouts,10,90,'!6'+ptitle,charsize=2.0
   xyouts,10,85,'!6'+strmid(underline,0,strlen(ptitle)+4),charsize=2.0
   xyouts,10,75,'!6X!Dc!N='+tx,charsize=2.0
   xyouts,10,65,'!6Y!Dc!N='+ty,charsize=2.0
   xyouts,10,50,'!7D!6X!Dc!N='+strtrim(string(dxc),2),charsize=2.0
   xyouts,10,40,'!7D!6Y!Dc!N='+strtrim(string(dyc),2),charsize=2.0
   xyouts,10,25,'!7e!6='+te,charsize=2.0
   xyouts,10,15,'!7h!6='+ta,charsize=2.0

   shut

   !fancy=ftemp
   !p.multi=[0,0,0,0]

endif


;
; Reset plotting area
;
!p.multi(*)=0



;
; Return to IDL
;
return
end
