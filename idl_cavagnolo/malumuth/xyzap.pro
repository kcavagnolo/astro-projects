;+
;                           
;*NAME:	xyzap.pro
;
;*PURPOSE:  
;           Event handler for events in TCTOOLs TRUE COLOR window while in
;           proceedure del_ray_wid.
;
;
;*CATEGORY:  Called by del_ray_wid
;           
;
;*CALLING SEQUENCE:  xyzap
;           
;
;*INPUTS:      NONE
;        
;
;*OUTPUTS:     NONE
;       
;
;*KEYWORD PARAMETERS:   NONE
;           
;
;*EXAMPLES:
;
;*PROCEDURE:
;           Remeber to press the "ZAP PIXELS" button in del_ray_wid before
;           clicking in the True Color window.  Failure to do may crash
;           TCTOOL.  HOWEVER, YOU CAN RECOVER BY TYPING "RETURN" (not retall)
;           in the idl window that launched TCTOOL.      
;
;*SUPPORT PROCEDURES: NONE
;
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;       6/99    E Malumuth/RITSS - Now handels events in True Color Window
;                                  and/or the zoom window.
;-
PRO xyzap,event
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON WVALUES4, field11,bgroup11,button51
   COMMON WVALUES3, draw14,draw51
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   COMMON zapvals, ima,zsize
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON DRAW51_Comm, DRAW51_Id
   COMMON zfact, zoomfact
IF (event.type eq 0) THEN BEGIN
  if (ima eq 'red') then begin
        xc=event.x
        yc=event.y
        if(event.id eq draw51) then begin
          xc=xc/zoomfact
          yc=yc/zoomfact
        endif
        xr=xc*fxr
        yr=yc*fyr
        im=red
        ss=size(red)
        xstart=xr-zsize
        xend=xr+zsize
        ystart=yr-zsize
        yend=yr+zsize
        if(xstart lt 1) then xstart=1
        if(ystart lt 1) then ystart=1
        if(xend gt ss(1)-1) then xend=ss(1)-1
        if(yend gt ss(2)-1) then yend=ss(2)-1
        im(xstart:xend,ystart:yend)=-999
        im=im(xstart-1:xend+1,ystart-1:yend+1)
        a=where(im gt -500)
        val=median(im(a))
        red(xstart:xend,ystart:yend)=val
        if (draw51 gt 0) then begin
          wset,draw51_id
          xyouts,xc*zoomfact,yc*zoomfact,'!20Q!3',color=255,/dev,chars=.8
        endif
        wset,draw14_id
        xyouts,xc,yc,'!20Q!3',color=255,/dev,chars=.8
   endif
   if (ima eq 'green') then begin
        xc=event.x
        yc=event.y
        if(event.id eq draw51) then begin
          xc=xc/zoomfact
          yc=yc/zoomfact
        endif
        xr=xc*fxg
        yr=yc*fyg
        im=green
        ss=size(green)
        xstart=xr-zsize
        xend=xr+zsize
        ystart=yr-zsize
        yend=yr+zsize
        if(xstart lt 1) then xstart=1
        if(ystart lt 1) then ystart=1
        if(xend gt ss(1)-1) then xend=ss(1)-1
        if(yend gt ss(2)-1) then yend=ss(2)-1
        im(xstart:xend,ystart:yend)=-999
        im=im(xstart-1:xend+1,ystart-1:yend+1)
        a=where(im gt -500)
        val=median(im(a))
        green(xstart:xend,ystart:yend)=val
        if (draw51 gt 0) then begin
          wset,draw51_id
          xyouts,xc*zoomfact,yc*zoomfact,'!20Q!3',color=256L*255,/dev,chars=.8
        endif
        wset,draw14_id
        xyouts,xc,yc,'!20Q!3',color=256L*255,/dev,chars=.8
   endif
   if (ima eq 'blue') then begin
        xc=event.x
        yc=event.y
        if(event.id eq draw51) then begin
          xc=xc/zoomfact
          yc=yc/zoomfact
        endif
        xr=xc*fxb
        yr=yc*fyb
        im=blue
        ss=size(blue)
        xstart=xr-zsize
        xend=xr+zsize
        ystart=yr-zsize
        yend=yr+zsize
        if(xstart lt 1) then xstart=1
        if(ystart lt 1) then ystart=1
        if(xend gt ss(1)-1) then xend=ss(1)-1
        if(yend gt ss(2)-1) then yend=ss(2)-1
        im(xstart:xend,ystart:yend)=-999
        im=im(xstart-1:xend+1,ystart-1:yend+1)
        a=where(im gt -500)
        val=median(im(a))
        blue(xstart:xend,ystart:yend)=val
        if (draw51 gt 0) then begin
          wset,draw51_id
          xyouts,xc*zoomfact,yc*zoomfact,'!20Q!3',color=256L*256*255,/dev,chars=.8
        endif
        wset,draw14_id
        xyouts,xc,yc,'!20Q!3',color=256L*256*255,/dev,chars=.8
   endif
ENDIF
RETURN
END
