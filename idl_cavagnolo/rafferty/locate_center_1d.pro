pro locate_center,events,chip=chip,xcen=xc,ycen=yc,xybin=xybin, $
                  window=window,outfile=outfile
;-----------------------------------------------------------------------
; Name: LOCATE_CENTER
;
; Purpose: Attempts to locate the center of a peaked surface
;          brightness distribution.
;          
; Inputs: 
;         
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 03-05-01
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 5)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'locate_center,events,chip=chip,xcen=xc,ycen=yc,xybin=xybin,', $
   '                                window=window,outfile=outfile'
   return   
endif

;
; Handle defaults
;
if (n_elements(xybin) eq 0) then xybin=1.0		; 1 ACIS pixel
if (n_elements(window) eq 0) then window=50.0


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
; Fit Gaussians to determine peak position
;
xfit=gaussfit(xx,yx,ax)
yfit=gaussfit(xy,yy,ay)

;
; Revise fit to include smaller window
;
xc=ax(1)
ix=where( (xx gt xc-window) and (xx lt xc+window) )
xx2=xx(ix)
yx2=yx(ix)
xfit2=gaussfit(xx2,yx2,ax2)
xc2=ax2(1)

yc=ay(1)
iy=where( (xy gt yc-window) and (xy lt yc+window) )
xy2=xy(iy)
yy2=yy(iy)
yfit2=gaussfit(xy2,yy2,ay2)
yc2=ay2(1)


;
; Set up plotting environment
;
!p.multi=[0,2,2,0]


;
; Define plotting limits
;
xbox=[xc-window,xc+window,0,1.2*max(yx)]
ybox=[yc-window,yc+window,0,1.2*max(yy)]


;
; Plot full range to screen
;
plot,xx,yx,psym=10,title='!6X!Dc!N='+strtrim(string(xc),2), $
     xtitle='Sky X',ytitle='Counts',yrange=[xbox(2),xbox(3)],/yst
oplot,[xc,xc],[xbox(2),xbox(3)],line=1
oplot,xx,xfit,line=2

plot,xy,yy,psym=10,title='!6Y!Dc!N='+strtrim(string(yc),2), $
     xtitle='Sky Y',ytitle='Counts',yrange=[ybox(2),ybox(3)],/yst
oplot,[yc,yc],[ybox(2),ybox(3)],line=1
oplot,xy,yfit,line=2


;
; Plot blow up to screen
;
plot,xx,yx,psym=10,title='!6X!Dc!N='+strtrim(string(xc2),2), $
     xtitle='Sky X',ytitle='Counts',yrange=[xbox(2),xbox(3)],/yst, $
     xrange=[xbox(0),xbox(1)]
oplot,[xc2,xc2],[xbox(2),xbox(3)],line=1
oplot,xx2,xfit2,line=2

plot,xy,yy,psym=10,title='!6Y!Dc!N='+strtrim(string(yc2),2), $
     xtitle='Sky Y',ytitle='Counts',yrange=[ybox(2),ybox(3)],/yst, $
     xrange=[ybox(0),ybox(1)]
oplot,[yc2,yc2],[ybox(2),ybox(3)],line=1
oplot,xy2,yfit2,line=2


;
; If indicated, create a postscript plot
;
if (n_elements(outfile) ne 0) then begin

   ftemp=!fancy
   !fancy=1

   open,'/portrait',outfile

   ;
   ; Plot full range to screen
   ;
   plot,xx,yx,psym=10,title='!6X!Dc!N='+strtrim(string(xc),2), $
        xtitle='Sky X',ytitle='Counts',yrange=[xbox(2),xbox(3)],/yst
   oplot,[xc,xc],[xbox(2),xbox(3)],line=1
   oplot,xx,xfit,line=2

   plot,xy,yy,psym=10,title='!6Y!Dc!N='+strtrim(string(yc),2), $
        xtitle='Sky Y',ytitle='Counts',yrange=[ybox(2),ybox(3)],/yst
   oplot,[yc,yc],[ybox(2),ybox(3)],line=1
   oplot,xy,yfit,line=2


   ;
   ; Plot blow up to screen
   ;
   plot,xx,yx,psym=10,title='!6X!Dc!N='+strtrim(string(xc2),2), $
        xtitle='Sky X',ytitle='Counts',yrange=[xbox(2),xbox(3)],/yst, $
        xrange=[xbox(0),xbox(1)]
   oplot,[xc2,xc2],[xbox(2),xbox(3)],line=1
   oplot,xx2,xfit2,line=2

   plot,xy,yy,psym=10,title='!6Y!Dc!N='+strtrim(string(yc2),2), $
        xtitle='Sky Y',ytitle='Counts',yrange=[ybox(2),ybox(3)],/yst, $
        xrange=[ybox(0),ybox(1)]
   oplot,[yc2,yc2],[ybox(2),ybox(3)],line=1
   oplot,xy2,yfit2,line=2

   shut

   !fancy=ftemp

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
