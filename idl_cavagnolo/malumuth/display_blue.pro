;+
;                    Display the blue image in it's window
;*NAME:	display_blue.pro
;
;*PURPOSE:  Proceedure to display the blue image in TCTOOL's blue image 
;           window with the current stretch and Scaling type.
;           
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  display_blue,blue,scale_type,bmin=bmin,bmax=bmax
;           
;
;*INPUTS:      blue       - the blue image in TCTOOL.
;              scale_type - flag that indicates which scaling type to use
;                           0 = linear
;                           1 = Log (base 10)
;                           2 = square root
;                           3 = histogram equalization
;        
;
;*OUTPUTS:     NONE
;       
;
;*KEYWORD PARAMETERS:   bmin = the minimum value mapped to 0
;                       bmax = the maximun value mapped to 255
;
;                       if bmax is missing or equal to 0 display_blue will
;                       choose min and max values from the data.
;           
;
;
;
;*EXAMPLES:
;
;*PROCEDURE:
;          TCTOOL calls this procedure when an image is loaded into the blue
;          channel using the Read Blue Image button, when any of the 
;          Blue Choose Scaling buttons are pushed, the Blue MIN or MAX fields
;          are changed of the blue image is edited, trimmed, rotated or shifted.
; 
;          
;
;*SUPPORT PROCEDURES:  NONE
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
pro display_blue,blue,scale_type,bmin=bmin,bmax=bmax
  COMMON DRAW6_Comm, DRAW6_Id
  COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
  COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,button107, $
                   button98,button108
  COMMON FLAGS, rflag,gflag,bflag
;
CASE scale_type OF
0: BEGIN                                         ; Linear Scaling
  if (not keyword_set(bmax)) then begin
    sky,blue,skymode,skysig,circlerad=1
    bluemin=skymode-skysig
    WIDGET_CONTROL,field81, SET_VALUE=bluemin
    bluemax=skymode+10.*skysig
    WIDGET_CONTROL,field82, SET_VALUE=bluemax
  endif
  wset,DRAW6_Id
  im=congrid(blue,200,200)
  tv,255b-bytscl(im,bluemin,bluemax)
END 
1: BEGIN                                         ; Log scale
   if (not keyword_set(bmax)) then begin
     s=size(blue)
     xmin=(s(1)/2)-(s(1)/4)
     xmax=(s(1)/2)+(s(1)/4)
     ymin=(s(2)/2)-(s(2)/4)
     ymax=(s(2)/2)+(s(2)/4)
     bluemax=alog10(max(blue(xmin:xmax,ymin:ymax)))
     bluemin=alog10(10^bluemax*1e-4)
     WIDGET_CONTROL,field81, SET_VALUE=bluemin
     WIDGET_CONTROL,field82, SET_VALUE=bluemax
   endif
wset,DRAW6_Id
im=congrid(blue,200,200)
im=alog10(im>10^bluemin)
tv,255b-bytscl(im,bluemin,bluemax)
END
2: BEGIN                                            ; SQUARE ROOT
   if (not keyword_set(bmax)) then begin
     s=size(blue)
     xmin=(s(1)/2)-(s(1)/4)
     xmax=(s(1)/2)+(s(1)/4)
     ymin=(s(2)/2)-(s(2)/4)
     ymax=(s(2)/2)+(s(2)/4)
     bluemax=sqrt(max(blue(xmin:xmax,ymin:ymax)))
     bluemin=sqrt((bluemax^2)*1e-4)
     WIDGET_CONTROL,field81, SET_VALUE=bluemin
     WIDGET_CONTROL,field82, SET_VALUE=bluemax
   endif
wset,DRAW6_Id
im=congrid(blue,200,200)
im=sqrt(im>bluemin^2)
tv,255b-bytscl(im,bluemin,bluemax)
END
3: BEGIN                                              ; HIST EQUAL
   if (not keyword_set(bmax)) then begin
     bluemax=511
     bluemin=0
     WIDGET_CONTROL,field81, SET_VALUE=bluemin
     WIDGET_CONTROL,field82, SET_VALUE=bluemax
   endif
wset,DRAW6_Id
im=congrid(blue,200,200)
im=hist_equal(im)
tv,255b-bytscl(im,bluemin,bluemax)
END
ENDCASE
bflag=1
return
end


