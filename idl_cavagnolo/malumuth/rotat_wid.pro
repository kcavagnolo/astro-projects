;+
;                           Rotate Image Tool
;*NAME:	rotat_wid.pro
;
;*PURPOSE:  
;           Widget tool for rotating, magnifying or demagnifying the RED 
;           image, the GREEN image or the BLUE which has been loaded into 
;           TCTOOL.
;
;           It's a wrapper for a call to the IDL library function ROT.
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  rotat_wid
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
;          Pressing the "Rotate Images button in TCTOOL will call rotat_wid.
;          a small widget called "ROTATE TOOL" will pop up.
;          click on the image that you want to rotate, magnify or demagnify. 
;          (The red will default if no button is pushed).  
;
;          Enter the angle to rotate by (positive is clockwise - negative 
;          is counter-clockwise), the magnification ( > 1.0 - magnify, < 1.0
;          will demagnify), and the pixel X-CENTER and Y-CENTER to rotate
;          about (this pixel will be used as the pivot point.)  You
;          may enter the X-CENTER and Y-CENTER or use the cursor in the
;          True Color window by first pressing the "USE CURSOR" button.
;
;          This can be useful if you have images which are not the same scale,
;          or if there is a rotation between them (i.e. 2 HST image taken at
;          different time or with different camera - WFPC2 and STIS).
;
;          Press the "ROTATE" button once the image is selected and the 
;          other information is entered.  REMEMBER to Press the <CR> when
;          changing a value in and field.
;
;          When you are finished press the "Done" button.         
;
;*SUPPORT PROCEDURES:
;          xycenter - event handler to get the cursor location for rotat_wid.
;
;*SUPPORT FUNCTIONS:
;          rot      - rotates the image.
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
PRO MAIN15_Event, Event
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,button107, $
                   button98,button108
   COMMON WVALUES3, draw14, draw51
   COMMON WVALUES2, field1,field2,field3,field4,bgroup1,button4,button5
   COMMON images,redim,greenim,blueim
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON rotvals, ima,ang,magn,xcenter,ycenter
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON cursorvals, xc,yc
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'rotate_image': BEGIN
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
  'rotate': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      if(ima eq 'red') then begin
        red=rot(red,ang,magn,xcenter,ycenter,missing=0,interp=1,pivot=1)
        display_red,red,red_scale_type,rmin=redmin,rmax=redmax
        redim=congrid(red,600,600,/interp)
        disp_true_wid,0
      endif
      if(ima eq 'green') then begin
        green=rot(green,ang,magn,xcenter,ycenter,missing=0,interp=1,pivot=1)
        display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
        greenim=congrid(green,600,600,/interp)
        disp_true_wid,0
      endif
      if(ima eq 'blue') then begin
        blue=rot(blue,ang,magn,xcenter,ycenter,missing=0,interp=1,pivot=1)
        display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
        blueim=congrid(blue,600,600,/interp)
        disp_true_wid,0
      endif
      END
  'done': BEGIN
      WIDGET_CONTROL, DRAW14, EVENT_PRO='xycenter'
      WIDGET_CONTROL, DRAW14, /DRAW_BUTTON_EVENTS
      WIDGET_CONTROL, event.top, /DESTROY
      END
  'xcent': BEGIN
      xcenter=event.value
      WIDGET_CONTROL,field3, SET_VALUE=xcenter
      END
  'ycent': BEGIN
      ycenter=event.value
      WIDGET_CONTROL,field4, SET_VALUE=ycenter
      END
  'mag': BEGIN
      magn=event.value
      WIDGET_CONTROL,field2, SET_VALUE=magn
      END
  'angle': BEGIN
      ang=event.value
      WIDGET_CONTROL,field1, SET_VALUE=ang
      END
  'use_curse': BEGIN
      WIDGET_CONTROL, DRAW14, SENSITIVE=1
      WIDGET_CONTROL, DRAW14, EVENT_PRO='xycenter'
      WIDGET_CONTROL, DRAW14, /DRAW_BUTTON_EVENTS
        END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN15
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO rotat_wid,dummy, GROUP=Group
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,button97,field73,field79,field83,button107, $
                   button98,button108
   COMMON WVALUES2, field1,field2,field3,field4,bgroup1,button4,button5
   COMMON images,redim,greenim,blueim
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON rotvals, ima,ang,magn,xcenter,ycenter
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON WVALUES3, draw14, draw51
   COMMON cursorvals, xc,yc
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   ima='red'
   ang=0.0
   magn=1.0
   xcenter=512.0
   ycenter=512.0

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MAIN15 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='ROTATE TOOL', $
      UVALUE='MAIN15')

  BASE2 = WIDGET_BASE(MAIN15, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  TextVal219 = [ $
    'Choose The IMAGE to Rotate (Red, Green, or Blue)', $
    'Then Choose the ANGLE to rotate the image,', $
    'the MAGNIFICATION of the image, and the,', $
    'X-CENTER and Y-CENTER to rotate about.', $
    'Press the USE CURSOR to use the cursor in the', $
    'True Color window to get X-CENTER and Y-CENTER.', $
    '', $
    'When you are finished press the ROTATE button.']
  TEXT3 = WIDGET_TEXT( BASE2,VALUE=TextVal219, $
      UVALUE='TEXT3', $
      YSIZE=10)

   Btns3889 = [ $
              'Red', $
              'Green', $
              'Blue' ]
  BGROUP1 = CW_BGROUP( BASE2, Btns3889, $
                        FONT='9x15', $
                        ROW=1, $
                        EXCLUSIVE=1, $
                        FRAME=1, $
                        LABEL_LEFT='IMAGE', $
                        UVALUE='rotate_image')

  FieldVal3000 = [ $
                  '0.0' ]
  FIELD1 = CW_FIELD( BASE2,VALUE=FieldVal3000, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='ANGLE', $
                      XSIZE=7, $
                      UVALUE='angle')
  FieldVal3001 = [ $
                  '1.0' ]
  FIELD2 = CW_FIELD( BASE2,VALUE=FieldVal3001, $
                      FONT='9x15', $
                      ROW=1, $
                      RETURN_EVENTS=1, $
                      FLOAT=1, $
                      TITLE='MAGNIFICATION', $
                      XSIZE=7, $
                      UVALUE='mag')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=2, $
      MAP=1, $
      FRAME=1, $
      UVALUE='BASE3')

  FieldVal3002 = [ $
                  '512.0' ]
  FIELD3 = CW_FIELD( BASE3,VALUE=FieldVal3002, $
                      FONT='9x15', $
                      ROW=1, $
                      RETURN_EVENTS=1, $
                      FLOAT=1, $
                      TITLE='X-CENTER', $
                      XSIZE=7, $
                      UVALUE='xcent')

  FieldVal3003 = [ $
                  '512.0' ]
  FIELD4 = CW_FIELD( BASE3,VALUE=FieldVal3003, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Y-CENTER', $
                      XSIZE=7, $
                      UVALUE='ycent')

  BUTTON6= WIDGET_BUTTON( BASE3, $
      UVALUE='use_curse', $
      VALUE='USE CURSOR')


  BUTTON4= WIDGET_BUTTON( BASE2, $
      UVALUE='rotate', $
      VALUE='ROTATE')

  BUTTON5= WIDGET_BUTTON( BASE2, $
      UVALUE='done', $
      VALUE='Done')

  WIDGET_CONTROL, DRAW14, SENSITIVE=1
  WIDGET_CONTROL, MAIN15, /REALIZE
  

  XMANAGER, 'MAIN15', MAIN15, /no_block
END
