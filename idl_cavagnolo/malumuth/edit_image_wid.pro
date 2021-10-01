;+
;                           Edit Image Tool
;*NAME:	edit_image_wid.pro
;
;*PURPOSE:  
;           Widget tool for trimming or otherwise editing the RED image, the 
;           GREEN image or the BLUE which has been loaded into TCTOOL.
;
;           At present only trimming and subtracting sky.
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  edit_image_wid
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
;          Pressing the "Edit Images button in TCTOOL will call edit_image_wid.
;          a small widget called "EDIT IMAGE TOOL" will pop up.
;          click on the image that you want to edit. (The red will default
;          if no button is pushed).  
;
;          To Trim an image: Enter the X-SIZE and Y-SIZE (it's best if they are 
;          the same) of the trimmed image (REMEMBER to hit the <CR>).  You
;          may enter the X-CENTER and Y-CENTER or use the cursor in the
;          True Color window by first pressing the "USE CURSOR TO SET THE 
;          CENTER" button.
;
;          This can be useful if you have images which are the same scale
;          but not the same size, or if there is a large shift between them.
;          Choose the red image, click of an object near the center and trim
;          the image.  Then use the same X-SIZE and Y-SIZE but click on the
;          same object in the green image and then the blue image.
;
;          Press the "TRIM" button once the image is selected and the size
;          and centering are entered.
;
;          Pressing the "SUBTRACT SKY" button will run the SKY proceedure
;          and subtract the resulting sky value from the image.
;
;          When you are finished press the "Done" button.         
;
;*SUPPORT PROCEDURES:
;          xytrim - event handler to get the cursor location for edit_image_wid.
;          sky    - determins the median sky value.
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
PRO MAIN17_Event, Event
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,button107, $
                   button98,button108
   COMMON WVALUES3, draw14, draw51
   COMMON WVALUES50, field100,field200,field300,field400,bgroup100, $
                    button400,button500
   COMMON images,redim,greenim,blueim
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON trimvals, ima,xsize,ysize,xcenter,ycenter
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'edit_image': BEGIN
      CASE Event.Value OF
      0: BEGIN
         ima='red'         
         END
      1: BEGIN
         ima='green'
         END 
      2: BEGIN
         ima='blue'
         END 
      ENDCASE
      END
  'TEXT3': BEGIN
      END
  'getsky': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      if(ima eq 'red') then begin
        sky,red,skymode,skysig,circlerad=1
        red=red-skymode
        display_red,red,red_scale_type,rmin=redmin,rmax=redmax
        redim=congrid(red,600,600,/interp)
        disp_true_wid,0
      endif
      if(ima eq 'green') then begin
        sky,green,skymode,skysig,circlerad=1
        green=green-skymode
        display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
        greenim=congrid(green,600,600,/interp)
        disp_true_wid,0
      endif
      if(ima eq 'blue') then begin
        sky,blue,skymode,skysig,circlerad=1
        blue=blue-skymode
        display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
        blueim=congrid(blue,600,600,/interp)
        disp_true_wid,0
      endif
     END
  'trim': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      if(ima eq 'red') then begin
        if(xsize gt 0 and ysize gt 0) then begin
        ss=size(red)
        x2=fix(xsize/2)
        y2=fix(ysize/2)
        xstart=xcenter-x2
        xend=xcenter+x2
        ystart=ycenter-y2
        yend=ycenter+y2
        if(xcenter-x2 lt 0) then xstart=0
        if(x2+xcenter gt ss(1)) then xend=ss(1)-1
        if(ycenter-y2 lt 0) then ystart=0
        if(y2+ycenter gt ss(2)) then yend=ss(2)-1
        red=red(xstart:xend,ystart:yend)
        ss=size(red)
        fxr=ss(1)/600.
        fyr=ss(2)/600.
        display_red,red,red_scale_type,rmin=redmin,rmax=redmax
        redim=congrid(red,600,600,/interp)
        disp_true_wid,0
        endif
      endif
      if(ima eq 'green') then begin
        if(xsize gt 0 and ysize gt 0) then begin
        ss=size(green)
        x2=fix(xsize/2)
        y2=fix(ysize/2)
        xstart=xcenter-x2
        xend=xcenter+x2
        ystart=ycenter-y2
        yend=ycenter+y2
        if(xcenter-x2 lt 0) then xstart=0
        if(x2+xcenter gt ss(1)) then xend=ss(1)-1
        if(ycenter-y2 lt 0) then ystart=0
        if(y2+ycenter gt ss(2)) then yend=ss(2)-1
        green=green(xstart:xend,ystart:yend)
        ss=size(green)
        fxg=ss(1)/600.
        fyg=ss(2)/600.
        display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
        greenim=congrid(green,600,600,/interp)
        disp_true_wid,0
        endif
      endif
      if(ima eq 'blue') then begin
        if(xsize gt 0 and ysize gt 0) then begin
        ss=size(blue)
        x2=fix(xsize/2)
        y2=fix(ysize/2)
        xstart=xcenter-x2
        xend=xcenter+x2
        ystart=ycenter-y2
        yend=ycenter+y2
        if(xcenter-x2 lt 0) then xstart=0
        if(x2+xcenter gt ss(1)) then xend=ss(1)-1
        if(ycenter-y2 lt 0) then ystart=0
        if(y2+ycenter gt ss(2)) then yend=ss(2)-1
        blue=blue(xstart:xend,ystart:yend)
        ss=size(blue)
        fxb=ss(1)/600.
        fyb=ss(2)/600.
        display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
        blueim=congrid(blue,600,600,/interp)
        disp_true_wid,0
        endif
      endif
      END
  'done': BEGIN
      WIDGET_CONTROL, DRAW14, EVENT_PRO='xycenter'
      WIDGET_CONTROL, DRAW14, /DRAW_BUTTON_EVENTS
      WIDGET_CONTROL, event.top, /DESTROY
      END
  'xcent': BEGIN
      xcenter=event.value
      WIDGET_CONTROL,field300, SET_VALUE=xcenter
      END
  'ycent': BEGIN
      ycenter=event.value
      WIDGET_CONTROL,field400, SET_VALUE=ycenter
      END
  'ysize': BEGIN
      ysize=event.value
      WIDGET_CONTROL,field200, SET_VALUE=ysize
      END
  'xsize': BEGIN
      xsize=event.value
      WIDGET_CONTROL,field100, SET_VALUE=xsize
      END
  'use_curse': BEGIN
      WIDGET_CONTROL, DRAW14, SENSITIVE=1
      WIDGET_CONTROL, DRAW14, EVENT_PRO='xytrim'
      WIDGET_CONTROL, DRAW14, /DRAW_BUTTON_EVENTS
        END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN17
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO edit_image_wid,dummy, GROUP=Group
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,button97,field73,field79,field83,button107, $
                   button98,button108
   COMMON WVALUES50, field100,field200,field300,field400,bgroup100, $
                    button400,button500
   COMMON images,redim,greenim,blueim
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON trimvals, ima,xsize,ysize,xcenter,ycenter
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON WVALUES3, draw14, draw51
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   ima='red'
   xsize=0.0
   ysize=0.0
   xcenter=512.0
   ycenter=512.0

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MAIN17 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='EDIT IMAGE TOOL', $
      UVALUE='MAIN17')

  BASE2 = WIDGET_BASE(MAIN17, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  TextVal219 = [ $
    'Choose The IMAGE to Work on (Red, Green, or Blue)', $
    'Then Choose the X-CENTER and Y-CENTER of the new,', $
    'image and the X-SIZE and Y-SIZE of the new image', $
    '', $
    'Press the USE CUSOR TO SET THE CENTER button', $
    'to use the cursor in the True Color window to get', $
    'X-CENTER and Y-CENTER.', $
    '', $
    'When you are finished press the TRIM button.']
  TEXT3 = WIDGET_TEXT( BASE2,VALUE=TextVal219, $
      UVALUE='TEXT3', $
      YSIZE=10)

   Btns3890 = [ $
              'Red', $
              'Green', $
              'Blue' ]
  BGROUP100 = CW_BGROUP( BASE2, Btns3890, $
                        FONT='9x15', $
                        ROW=1, $
                        EXCLUSIVE=1, $
                        FRAME=1, $
                        LABEL_LEFT='IMAGE', $
                        UVALUE='edit_image')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=3, $
      MAP=1, $
      FRAME=1, $
      UVALUE='BASE3')


  FieldVal3000 = [ $
                  '0.0' ]
  FIELD100 = CW_FIELD( BASE3,VALUE=FieldVal3000, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='X-SIZE  ', $
                      XSIZE=7, $
                      UVALUE='xsize')
  FieldVal3001 = [ $
                  '0.0' ]
  FIELD200 = CW_FIELD( BASE3,VALUE=FieldVal3001, $
                      FONT='9x15', $
                      ROW=1, $
                      RETURN_EVENTS=1, $
                      FLOAT=1, $
                      TITLE='Y-SIZE  ', $
                      XSIZE=7, $
                      UVALUE='ysize')

  FieldVal3002 = [ $
                  '512.0' ]
  FIELD300 = CW_FIELD( BASE3,VALUE=FieldVal3002, $
                      FONT='9x15', $
                      ROW=1, $
                      RETURN_EVENTS=1, $
                      FLOAT=1, $
                      TITLE='X-CENTER', $
                      XSIZE=7, $
                      UVALUE='xcent')

  FieldVal3003 = [ $
                  '512.0' ]
  FIELD400 = CW_FIELD( BASE3,VALUE=FieldVal3003, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Y-CENTER', $
                      XSIZE=7, $
                      UVALUE='ycent')

  BUTTON600= WIDGET_BUTTON( BASE3, $
      UVALUE='use_curse', $
      VALUE='USE CURSOR TO SET THE CENTER')


  BUTTON400= WIDGET_BUTTON( BASE2, $
      UVALUE='trim', $
      VALUE='TRIM')

  BUTTON405= WIDGET_BUTTON( BASE2, $
      UVALUE='getsky', $
      VALUE='SUBTRACT SKY')

  BUTTON500= WIDGET_BUTTON( BASE2, $
      UVALUE='done', $
      VALUE='DONE')

  WIDGET_CONTROL, DRAW14, SENSITIVE=1
  WIDGET_CONTROL, MAIN17, /REALIZE
  

  XMANAGER, 'MAIN17', MAIN17, /no_block
END
