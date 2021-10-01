;+
;                    Display the red image in it's window
;*NAME:	display_red.pro
;
;*PURPOSE:  Proceedure to display the red image in TCTOOL's red image 
;           window with the current stretch and Scaling type.
;           
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  display_red,red,scale_type,rmin=rmin,rmax=rmax
;           
;
;*INPUTS:      red        - the red image in TCTOOL.
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
;*KEYWORD PARAMETERS:   rmin = the minimum value mapped to 0
;                       rmax = the maximun value mapped to 255
;
;                       if rmax is missing or equal to 0 display_red will
;                       choose min and max values from the data.
;           
;
;
;
;*EXAMPLES:
;
;*PROCEDURE:
;          TCTOOL calls this procedure when an image is loaded into the red
;          channel using the Read Red Image button, when any of the 
;          Red Choose Scaling buttons are pushed, the Red MIN or MAX fields
;          are changed of the red image is edited, trimmed, rotated or shifted.
; 
;          
;
;*SUPPORT PROCEDURES:  NONE
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
pro display_red,red,scale_type,rmin=rmin,rmax=rmax
  COMMON DRAW4_Comm, DRAW4_Id
  COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
  COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,button107, $
                   button98,button108
  COMMON FLAGS, rflag,gflag,bflag
;
if (scale_type eq 0) then begin    ; Linear Scaling
  if (not keyword_set(rmax)) then begin
    sky,red,skymode,skysig,circlerad=1
    redmin=skymode-skysig
    WIDGET_CONTROL,field71, SET_VALUE=redmin
    redmax=skymode+10.*skysig
    WIDGET_CONTROL,field72, SET_VALUE=redmax
  endif
  wset,DRAW4_Id
  im=congrid(red,200,200)
  tv,255b-bytscl(im,redmin,redmax)
endif
if (scale_type eq 1) then begin  ; Log scale
   if (not keyword_set(rmax)) then begin
     s=size(red)
     xmin=(s(1)/2)-(s(1)/4)
     xmax=(s(1)/2)+(s(1)/4)
     ymin=(s(2)/2)-(s(2)/4)
     ymax=(s(2)/2)+(s(2)/4)
     redmax=alog10(max(red(xmin:xmax,ymin:ymax)))
     redmin=alog10(10^redmax*1e-4)
     WIDGET_CONTROL,field71, SET_VALUE=redmin
     WIDGET_CONTROL,field72, SET_VALUE=redmax
   endif
wset,DRAW4_Id
im=congrid(red,200,200)
im=alog10(im>10^redmin)
tv,255b-bytscl(im,redmin,redmax)
endif
if (scale_type eq 2) then begin  ; Log scale
   if (not keyword_set(rmax)) then begin
     s=size(red)
     xmin=(s(1)/2)-(s(1)/4)
     xmax=(s(1)/2)+(s(1)/4)
     ymin=(s(2)/2)-(s(2)/4)
     ymax=(s(2)/2)+(s(2)/4)
     redmax=sqrt(max(red(xmin:xmax,ymin:ymax)))
     redmin=sqrt((redmax^2)*1e-4)
     WIDGET_CONTROL,field71, SET_VALUE=redmin
     WIDGET_CONTROL,field72, SET_VALUE=redmax
   endif
wset,DRAW4_Id
im=congrid(red,200,200)
im=sqrt(im>redmin^2)
tv,255b-bytscl(im,redmin,redmax)
endif
if (scale_type eq 3) then begin  ; Log scale
   if (not keyword_set(rmax)) then begin
     redmax=511
     redmin=0
     WIDGET_CONTROL,field71, SET_VALUE=redmin
     WIDGET_CONTROL,field72, SET_VALUE=redmax
   endif
wset,DRAW4_Id
im=congrid(red,200,200)
im=hist_equal(im)
tv,255b-bytscl(im,redmin,redmax)
endif
rflag=1
return
end


