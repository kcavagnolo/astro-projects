;+
;                    Delete Cosmic Ray hits in a True Color Image
;*NAME:	del_ray_wid.pro
;
;*PURPOSE:  
;           Widget tool for zaping pixels in the RED image, the GREEN image or
;           the BLUE which has been loaded into TCTOOL
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  del_ray_wid
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
;
;
;*EXAMPLES:
;
;*PROCEDURE:
;          Pressing the "Edit Pixels button in TCTOOL will call del_ray_wid.
;          a small widget called "ZAP PIXEL TOOL" will pop up.
;          click on the image that you want to edit. (The red will default
;          if no button is pushed).  Enter the size of the Zap Box by
;          changing the number in the field (REMEMBER to hit the <CR>)
;          Press the ZAP PIXELS button, then go into the True Color window.
;          Place the cursor on any pixel and press the mouse button, a red,
;          green or blue circle will be drawn around the centeral pixel to
;          remind you that that pixel has been done.
;
;          When you are finished press the "Done" button, the tool will
;          disapear and the images will be redisplayed.
;
;          You may need to do this several times to remove all of the bad
;          pixels or to get rid of large Cosmic-Ray trails.
;          
;
;*SUPPORT PROCEDURES:
;          xyzap - event handler to get the cursor location for del_ray_wid.
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;       6/99    E Malumuth/RITSS - added ability to zap pixels using the zoom
;                                  window.
;-
PRO MAIN16_Event, Event
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,button107, $
                   button98,button108
   COMMON WVALUES3, draw14,draw51
   COMMON WVALUES4, field11,bgroup11,button51
   COMMON images,redim,greenim,blueim
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON zapvals, ima,zsize
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON DRAW51_Comm, DRAW51_Id
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'zap_image': BEGIN
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
  'done': BEGIN
      WIDGET_CONTROL, /HOURGLASS
        display_red,red,red_scale_type,rmin=redmin,rmax=redmax
        redim=congrid(red,600,600,/interp)
        display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
        greenim=congrid(green,600,600,/interp)
        display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
        blueim=congrid(blue,600,600,/interp)
        disp_true_wid,0
      if (draw51 gt 0) then begin
        WIDGET_CONTROL, DRAW51, EVENT_PRO='xyzap'
        WIDGET_CONTROL, DRAW51, /DRAW_BUTTON_EVENTS
      endif
      WIDGET_CONTROL, DRAW14, EVENT_PRO='xyzap'
      WIDGET_CONTROL, DRAW14, /DRAW_BUTTON_EVENTS
      WIDGET_CONTROL, event.top, /DESTROY
      END
  'zsize': BEGIN
      zsize=event.value
      WIDGET_CONTROL,field11, SET_VALUE=zsize
      END
  'use_curse': BEGIN
      WIDGET_CONTROL, DRAW14, SENSITIVE=1
      WIDGET_CONTROL, DRAW14, EVENT_PRO='xyzap'
      WIDGET_CONTROL, DRAW14, /DRAW_BUTTON_EVENTS
      END
  'zoom_curse': BEGIN
      if (draw51 gt 0) then begin
        WIDGET_CONTROL, DRAW51, SENSITIVE=1
        WIDGET_CONTROL, DRAW51, EVENT_PRO='xyzap'
        WIDGET_CONTROL, DRAW51, /DRAW_BUTTON_EVENTS
      endif
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN16
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO del_ray_wid,dummy, GROUP=Group
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,button97,field73,field79,field83,button107, $
                   button98,button108
   COMMON WVALUES4, field11,bgroup11,button51
   COMMON images,redim,greenim,blueim
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON zapvals, ima,zsize
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON DRAW51_Comm, DRAW51_Id
   COMMON WVALUES3, draw14,draw51
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   ima='red'
   zsize=1

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MAIN16 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='ZAP PIXEL TOOL', $
      UVALUE='MAIN16')

  BASE2 = WIDGET_BASE(MAIN16, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  TextVal219 = [ $
    'Choose The IMAGE to Clean (Red, Green, or Blue)', $
    'Then choose the Size of the Zap box,', $
    'press the ZAP PIXELS button place the ,', $
    'cursor over the pixel to zap and', $
    'press the left mouse button']

  TEXT3 = WIDGET_TEXT( BASE2,VALUE=TextVal219, $
      UVALUE='TEXT3', $
      YSIZE=7)

   Btns3889 = [ $
              'Red', $
              'Green', $
              'Blue' ]
  BGROUP11 = CW_BGROUP( BASE2, Btns3889, $
                        FONT='9x15', $
                        ROW=1, $
                        EXCLUSIVE=1, $
                        FRAME=1, $
                        LABEL_LEFT='IMAGE', $
                        UVALUE='zap_image')

  FieldVal3000 = [ $
                  '1' ]
  FIELD11 = CW_FIELD( BASE2,VALUE=FieldVal3000, $
                      FONT='9x15', $
                      ROW=1, $
                      INTEGER=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Size of Zap Box', $
                      XSIZE=7, $
                      UVALUE='zsize')

  BUTTON61= WIDGET_BUTTON( BASE2, $
      UVALUE='use_curse', $
      VALUE='ZAP PIXELS')

  BUTTON63= WIDGET_BUTTON( BASE2, $
      UVALUE='zoom_curse', $
      VALUE='ZAP PIXELS IN ZOOM WINDOW')

  BUTTON51= WIDGET_BUTTON( BASE2, $
      UVALUE='done', $
      VALUE='Done')

  WIDGET_CONTROL, MAIN16, /REALIZE
  

  XMANAGER, 'MAIN16', MAIN16, /no_block
END
