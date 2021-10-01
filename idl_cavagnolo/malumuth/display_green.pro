;+
;                    Display the green image in it's window
;*NAME:	display_green.pro
;
;*PURPOSE:  Proceedure to display the green image in TCTOOL's green image 
;           window with the current stretch and Scaling type.
;           
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  display_green,green,scale_type,gmin=gmin,gmax=gmax
;           
;
;*INPUTS:      green      - the green image in TCTOOL.
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
;*KEYWORD PARAMETERS:   gmin = the minimum value mapped to 0
;                       gmax = the maximun value mapped to 255
;
;                       if gmax is missing or equal to 0 display_green will
;                       choose min and max values from the data.
;           
;
;
;
;*EXAMPLES:
;
;*PROCEDURE:
;          TCTOOL calls this procedure when an image is loaded into the green
;          channel using the Read Green Image button, when any of the 
;          Green Choose Scaling buttons are pushed, the Green MIN or MAX fields
;          are changed of the green image is edited, trimmed, rotated or
;          shifted.
; 
;          
;
;*SUPPORT PROCEDURES:  NONE
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
pro display_green,green,scale_type,gmin=gmin,gmax=gmax
  COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
  COMMON DRAW5_Comm, DRAW5_Id
  COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,button107, $
                   button98,button108
  COMMON FLAGS, rflag,gflag,bflag
;
if (scale_type eq 0) then begin    ; Linear Scaling
  if (not keyword_set(gmax)) then begin
    sky,green,skymode,skysig,circlerad=1
    greenmin=skymode-skysig
    WIDGET_CONTROL,field76, SET_VALUE=greenmin
    greenmax=skymode+10.*skysig
    WIDGET_CONTROL,field77, SET_VALUE=greenmax
  endif
  wset,DRAW5_Id
  im=congrid(green,200,200)
  tv,255b-bytscl(im,greenmin,greenmax)
endif
if (scale_type eq 1) then begin  ; Log scale
   if (not keyword_set(gmax)) then begin
     s=size(green)
     xmin=(s(1)/2)-(s(1)/4)
     xmax=(s(1)/2)+(s(1)/4)
     ymin=(s(2)/2)-(s(2)/4)
     ymax=(s(2)/2)+(s(2)/4)
     greenmax=alog10(max(green(xmin:xmax,ymin:ymax)))
     greenmin=alog10(10^greenmax*1e-4)
     WIDGET_CONTROL,field76, SET_VALUE=greenmin
     WIDGET_CONTROL,field77, SET_VALUE=greenmax
   endif
wset,DRAW5_Id
im=congrid(green,200,200)
im=alog10(im>10^greenmin)
tv,255b-bytscl(im,greenmin,greenmax)
endif
if (scale_type eq 2) then begin  ; Log scale
   if (not keyword_set(gmax)) then begin
     s=size(green)
     xmin=(s(1)/2)-(s(1)/4)
     xmax=(s(1)/2)+(s(1)/4)
     ymin=(s(2)/2)-(s(2)/4)
     ymax=(s(2)/2)+(s(2)/4)
     greenmax=sqrt(max(green(xmin:xmax,ymin:ymax)))
     greenmin=sqrt((greenmax^2)*1e-4)
     WIDGET_CONTROL,field76, SET_VALUE=greenmin
     WIDGET_CONTROL,field77, SET_VALUE=greenmax
   endif
wset,DRAW5_Id
im=congrid(green,200,200)
im=sqrt(im>greenmin^2)
tv,255b-bytscl(im,greenmin,greenmax)
endif
if (scale_type eq 3) then begin  ; Log scale
   if (not keyword_set(gmax)) then begin
     greenmax=511
     greenmin=0
     WIDGET_CONTROL,field76, SET_VALUE=greenmin
     WIDGET_CONTROL,field77, SET_VALUE=greenmax
   endif
wset,DRAW5_Id
im=congrid(green,200,200)
im=hist_equal(im)
tv,255b-bytscl(im,greenmin,greenmax)
endif
gflag=1
return
end


