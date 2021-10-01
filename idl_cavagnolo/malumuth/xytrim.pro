;+
;                           
;*NAME:	xytrim.pro
;
;*PURPOSE:  
;           Event handler for events in TCTOOLs TRUE COLOR window while in
;           proceedure edit_image_wid.
;
;
;*CATEGORY:  Called by edit_image_wid
;           
;
;*CALLING SEQUENCE:  xytrim
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
;           Remeber to press the "USE CURSOR TO SET THE CENTER" button in 
;           edit_image_wid before clicking in the True Color window.  Failure 
;           to do so may crash TCTOOL.  YOU CAN RECOVER BY TYPING "RETURN" 
;           (not retall) in the idl window that launched TCTOOL.      
;
;*SUPPORT PROCEDURES: NONE
;
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
PRO xytrim,event
   COMMON WVALUES50, field100,field200,field300,field400,bgroup100, $
                    button400,button500
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   COMMON trimvals, ima,xsize,ysize,xcenter,ycenter
IF (event.type eq 0) THEN BEGIN
  if (ima eq 'red') then begin
    xc=event.x
    yc=event.y
    xcenter=event.x*fxr
    ycenter=event.y*fyr
      WIDGET_CONTROL,field300, SET_VALUE=xcenter
      WIDGET_CONTROL,field400, SET_VALUE=ycenter
  endif
   if (ima eq 'green') then begin
     xc=event.x
     yc=event.y
     xcenter=event.x*fxg
     ycenter=event.y*fyg
      WIDGET_CONTROL,field300, SET_VALUE=xcenter
      WIDGET_CONTROL,field400, SET_VALUE=ycenter
   endif
   if (ima eq 'blue') then begin
     xc=event.x
     yc=event.y
     xcenter=event.x*fxb
     ycenter=event.y*fyb
      WIDGET_CONTROL,field300, SET_VALUE=xcenter
      WIDGET_CONTROL,field400, SET_VALUE=ycenter
   endif
ENDIF
RETURN
END
