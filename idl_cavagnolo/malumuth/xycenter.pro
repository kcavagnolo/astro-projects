;+
;                           
;*NAME:	xycenter.pro
;
;*PURPOSE:  
;           Event handler for events in TCTOOLs TRUE COLOR window while in
;           proceedure rotat_wid.
;
;
;*CATEGORY:  Called by Rotat_wid
;           
;
;*CALLING SEQUENCE:  xycenter
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
;           Remeber to press the "USE CURSOR" button in rotat_wid before
;           clicking in the True Color window.  Failure to do may crash
;           TCTOOL.  HOWEVER, YOU CAN RECOVER BY TYPING "RETURN" (not retall)
;           in the idl window that launched TCTOOL.      
;
;*SUPPORT PROCEDURES: NONE
;
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
PRO xycenter,event
   COMMON cursorvals, xc,yc
   COMMON WVALUES2, field1,field2,field3,field4,bgroup1,button4,button5
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   COMMON rotvals, ima,ang,magn,xcenter,ycenter
IF (event.type eq 0) THEN BEGIN
  if (ima eq 'red') then begin
    xc=event.x
    yc=event.y
    xcenter=event.x*fxr
    ycenter=event.y*fyr
      WIDGET_CONTROL,field3, SET_VALUE=xcenter
      WIDGET_CONTROL,field4, SET_VALUE=ycenter
  endif
   if (ima eq 'green') then begin
     xc=event.x
     yc=event.y
     xcenter=event.x*fxg
     ycenter=event.y*fyg
      WIDGET_CONTROL,field3, SET_VALUE=xcenter
      WIDGET_CONTROL,field4, SET_VALUE=ycenter
   endif
   if (ima eq 'blue') then begin
     xc=event.x
     yc=event.y
     xcenter=event.x*fxb
     ycenter=event.y*fyb
      WIDGET_CONTROL,field3, SET_VALUE=xcenter
      WIDGET_CONTROL,field4, SET_VALUE=ycenter
   endif
ENDIF
RETURN
END
