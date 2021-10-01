pro plot3d,xx,yy,zz,intens,dax=dax,daz=daz,ax=ax,az=az, $
           ilog=ilog,do_circle=do_circle,_extra = e
;+
; ROUTINE:  plot3d
;
; PURPOSE:  plot a 3-D array and rotate plot interactively
;
; USEAGE:   plot3d,xx,yy,zz,[intens],[plot parameters]
;   Use mouse button left to turn around Z and center button to turn
;   arond X axis. Right button will exit.
;
; INPUT:    
;   xx,yy,zz
;     coordinates of the points
;
; OPTIONAL INPUT:
;   intens : intensity, will be related to the color.
;
; KEYWORD INPUT:
;
;   dax,daz: delta_angles (deg). Default = 1.
;   every extra keyword will be passed to plot AND surface.
;
; OUTPUT:
;
; DISCUSSION:
;
; LIMITATIONS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;  
; EXAMPLE:  
;
;  x=randomu(seed,100)
;  y=randomu(seed,100)
;  z=randomu(seed,100)
;  intens = randomu(seed,100)
;
;  plot3d,x,y,z,intens
;
;
;
; AUTHOR:   Adapted from ESRG package by C.Morisset 1997 
; morisset@iagusp.usp.br
;
; REVISIONS:
;          
;-
;



if n_params() eq 4 then begin
   print,'Min:',min(intens,/nan),' Max:',max(intens,/nan)   
   if keyword_set(ilog) then $
    COLOR = BYTSCL(alog10(intens), TOP=!D.N_COLORS-1,/nan,$
                   min=min(alog10(intens),/nan)-1) else $
    COLOR = BYTSCL(intens, TOP=!D.N_COLORS-1,/nan,min=min(intens,/nan)*.5) 
endif else color = !D.N_COLORS-1
	
print,'Left: turn/x Middel: turn/z Right: exit'
if not keyword_set(dax) then dax = 1
if not keyword_set(daz) then daz = 1

zmn=min(zz,max=zmx)
xmn=min(xx,max=xmx)
ymn=min(yy,max=ymx)

z=[[zmn,zmx],[zmn,zmx]]
x=[[xmn,xmx],[xmn,xmx]]
y=[[ymn,ymn],[ymx,ymx]]

if not keyword_set(ax) then ax = 90.
if not keyword_set(az) then az = 360.

if keyword_set(do_circle) then begin
   circle =  fltarr(3,900)
   i_circle =  findgen(300)
   r_circle =  5.
   circle[0,0:299] =  r_circle*sin(i_circle/300.*2*!pi)
   circle[1,0:299] = r_circle*cos(i_circle/300.*2*!pi)
   r_circle =  10.
   circle[0,300:599] =  r_circle*sin(i_circle/300.*2*!pi)
   circle[1,300:599] = r_circle*cos(i_circle/300.*2*!pi)
   r_circle =  15.
   circle[0,600:899] =  r_circle*sin(i_circle/300.*2*!pi)
   circle[1,600:899] = r_circle*cos(i_circle/300.*2*!pi)
endif

surface,z,x,y,/nodata,/save,az=az,ax=ax,xtitle='X',ytitle='Y',ztitle='Z',_extra=e
plots,xx,yy,zz,/t3d,psym=6,color=color,_extra=e

!mouse.button = 1

while (!mouse.button ne 4) do begin
  cursor,x_tmp,y_tmp
  if !mouse.button eq 1 then ax = ax + dax $
	else if !mouse.button eq 2 then az = az + daz
  surface,z,x,y,/nodata,/save,az=az,ax=ax,xtitle='X',ytitle='Y',ztitle='Z',_extra=e
  plots,xx,yy,zz,/t3d,psym=6,color=color,_extra=e
;  plots,0.,8.5,0.,/t3d,psym=2,color=max(color),_extra=e,symsize=2
;  plots,0.,0.,0.,/t3d,psym=2,color=max(color)/2.,_extra=e,symsize=2
  if keyword_set(do_circle) then plots,circle[0,*],circle[1,*],circle[2,*],$
   /t3d,psym=3,color=max(color),_extra=e
  
endwhile

print,'ax=',ax,'  az=',az
end

