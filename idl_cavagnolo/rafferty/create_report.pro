pro create_report,events,file,chip=chip,xcen=xc,ycen=yc,xybin=xybin,$
       mincnts=mincnts
;-----------------------------------------------------------------------
; Name: CREATE_REPORT
;
; Purpose: Compiles some useful info about a given event list
;          
; Inputs: 
;         
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 10-22-99
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 5)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'create_report,events,file,chip=chip,xcen=xc,ycen=yc,xybin=xybin,',$
   '              mincnts=mincnts'
   return   
endif

;
; Handle defaults
;
if (np lt 2) then file='regions.dat'
if (n_elements(xybin) eq 0) then xybin=1.0		; 1 ACIS pixel
if (n_elements(mincnts) eq 0) then mincnts=3000.	; minimum number of 
							; counts in a bin

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
; Set up plotting variables
;
!p.multi=[0,2,2,0]

;
; Find centroid of data
;
plothist,data.x,xh,yh,bin=xybin,/noplot

if (n_elements(xc) eq 0) then begin
   xc=xh(where(yh eq max(yh)))
   xc=xc(0)
endif

plot,xh,yh,psym=10,title='!6X!Dc!N='+strtrim(string(xc),2), $
     xtitle='Sky X',ytitle='Counts'
oplot,[xc,xc],[!cymin,!cymax],line=2

plothist,data.y,xh,yh,bin=xybin,/noplot

if (n_elements(yc) eq 0) then begin
   yc=xh(where(yh eq max(yh)))
   yc=yc(0)
endif

plot,xh,yh,psym=10,title='!6Y!Dc!N='+strtrim(string(yc),2), $
     xtitle='Sky Y',ytitle='Counts'
oplot,[yc,yc],[!cymin,!cymax],line=2

;
; Look at radial surface brightness
;
r=sqrt( (data.x-xc)^2 + (data.y-yc)^2 )

plothist,r,xh,yh,bin=xybin,/noplot
yh=float(yh)
eyh=sqrt(yh)
nh=n_elements(yh)
d_area = !pi * ( xh[1:nh-1]^2 - xh[0:nh-2]^2 )
sh=yh[0:nh-2]/d_area[0:nh-2]
esh=eyh[0:nh-2]/d_area[0:nh-2]

plot_oo,xh,sh,psym=10,title='Radial Surface Brightness', $
     xtitle='Radius [pixels]',ytitle='Counts/(Pixel)!E2!N', $
     xrange=[0.1,2.0*max(xh)],/xst, $
     yrange=[1.0e-3*max(sh),2.0*max(sh)],/yst

oploterr,xh(0:nh-2),sh,esh,psym=3,hatlength=!D.X_VSIZE /200.


;
; Create annular array based on minimum number of counts
;
num=n_elements(xh)
th=fltarr(num)
for i=0L,num-1 do begin
    th(i)=total(yh(0:i))
endfor

plot_oo,xh,th,psym=10,title='Cumulative Distribution', $
     xtitle='Radius [pixels]',ytitle='Counts', $
     xrange=[0.1,2.0*max(xh)],/xst, $
     yrange=[1.0,2.0*max(th)],/yst
xyouts,2.0,10.0,'Min. counts = '+strtrim(string(mincnts),2)


th=th/mincnts
nrad=fix(max(th))
r1=fltarr(nrad)
r2=fltarr(nrad)
area=fltarr(nrad)

for i=0L,nrad-1 do begin
    j=where(th ge i)
    r1(i)=xh(j(0))
    j=where(th ge i+1)
    r2(i)=xh(j(0))
    area(i)=!pi*(r2(i)*r2(i)-r1(i)*r1(i))

    oplot,[r2(i),r2(i)],[1.0,2.0*max(th*mincnts)],line=1

endfor

;
; Write the annulus information file
;
fmt='$(i3,2x,a8,5(2x,f9.2))'
get_lun,unit
openw,unit,file
for i=0L,nrad-1 do begin
    printf,unit,fmt,i,'annulus',xc,yc,r1(i),r2(i),area(i)
endfor
close,unit
free_lun,unit

;
; Reset plotting area
;
!p.multi(*)=0

;
; Return to IDL
;
return
end
