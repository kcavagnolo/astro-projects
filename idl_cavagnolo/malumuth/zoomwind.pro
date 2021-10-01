;+
;                      Zoom Tool
;*NAME:	zoomwind
;
;*PURPOSE:  
;           Widget tool to pop up a True Color zoom window within TCTOOL
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  zoomwind
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
;*PROCEDURE:  set the slide bar to the desired zoom factor then press the 
;             Display button
;          
;
;*SUPPORT PROCEDURES:  NONE
;
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
PRO MAIN18_Event, Event
   COMMON images,redim,greenim,blueim
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON zfact, zoomfact
   COMMON WVALUES3, draw14, draw51
   COMMON timages,tredimage,tgreenimage,tblueimage
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SLIDER3': BEGIN
      zoomfact=Event.Value
      END
  'DRAW51': BEGIN
      Print, 'Event for DRAW51'
      END
  'done': BEGIN
      WIDGET_CONTROL, event.top, /DESTROY
      draw51=-1
      END
  'display': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      sr=size(tredimage)
      WIDGET_CONTROL, DRAW51, $
                      draw_xsize=sr(1)*zoomfact,draw_ysize=sr(2)*zoomfact
      disp_true_zoom,zoomfact
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN18
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO zoomwind,dummy, GROUP=Group
   COMMON zfact, zoomfact
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON timages,tredimage,tgreenimage,tblueimage
   COMMON WVALUES3, draw14, draw51
   zoomfact=1
   sr=size(tredimage)


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MAIN18 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE=' ZOOM TOOL ', $
      UVALUE='MAIN18')

  BASE2 = WIDGET_BASE(MAIN18, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON550= WIDGET_BUTTON( BASE2, $
      UVALUE='display', $
      VALUE='Display')


  BUTTON650= WIDGET_BUTTON( BASE2, $
      UVALUE='done', $
      VALUE='DONE')

  SLIDER3 = WIDGET_SLIDER( BASE2, $
      MAXIMUM=4, $
      MINIMUM=1, $
      TITLE='Zoom Size', $
      UVALUE='SLIDER3', $
      VALUE=1, $
      XSIZE=512)

  DRAW51 = WIDGET_DRAW( BASE2, $
      RETAIN=2, $
      UVALUE='DRAW51', $
      XSIZE=sr(1), $
      X_SCROLL_SIZE=512, $
      YSIZE=sr(2), $
      Y_SCROLL_SIZE=512)

  WIDGET_CONTROL, MAIN18, /REALIZE

  ; Get drawable window index

  COMMON DRAW51_Comm, DRAW51_Id
  WIDGET_CONTROL, DRAW51, GET_VALUE=DRAW51_Id

  XMANAGER, 'MAIN18', MAIN18, /no_block
END
