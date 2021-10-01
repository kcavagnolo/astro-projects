;+
;                          TRUE COLOR IMAGE TOOL
;*NAME:	tctool.pro
;
;*PURPOSE:  
;           Widget tool for combining a RED image, a GREEN image and a BLUE
;           image into a pseudo true color image.  
;
;           This routine draws all of the Widget buttons, sliders, windows, etc.
;           It handles all of the widget events and calls the various other
;           routines that do the plotting, etc.
;    
;           NOTE ;
;
;*CATEGORY:  
;           WIDGET
;
;*CALLING SEQUENCE:  
;           tctool,red1=red_image,green1=green_image,blue1=blue_image, $
;                  rhead=red_header,ghead=green_header,bhead=blue_header
;
;*INPUTS:  
;           NONE
;
;*OUTPUTS: 
;           NONE
;
;*KEYWORD PARAMETERS: 
;           RED1:   If set equal to the red image there is no need to
;                   read the red image in.  This is useful if the red image
;                   isn't a fits image or if you need to process the red image
;                   before using as part of a true color image.
;
;           GREEN1: If set equal to the green image there is no need to
;                   read the green image in.  This is useful if the green image
;                   isn't a fits image or if you need to process the green image
;                   before using as part of a true color image.
;
;           BLUE1:  If set equal to the blue image there is no need to
;                   read the blue image in.  This is useful if the blue image
;                   isn't a fits image or if you need to process the blue image
;                   before using as part of a true color image.
;
;           RHEAD1: Set equal to the red image's header when using RED1.
;                   This is useful for placing N and E arrows on the image
;                   using the ARROWS procedure.  Note the header must have a
;                   correct CD matrix to get arrows.
;
;           GHEAD1: Same as RHEAD1, except not used at present.
;
;           BHEAD1: Same as RHEAD1, except not used at present.
;
;
;*NOTES:
;          TO RUN THIS WIDGET YOU MUST ENTER 24BIT MODE BEFORE ENTERING IDL.
;
;
;*EXAMPLES:
;
;*PROCEDURE:
;          Enter 24bit mode in a XTERM window by typing 24bit.
;          Enter IDL, then type tctool.
;          When the WIDGET appears, use the "Read Red Image", "Read Green Image"
;          and "Read Blue Image" buttons to load 3 FITS images of the same field
;          taken in different bands into the red, green, and blue images in
;          the program.  Note: these image need not be at the same resolution,
;          orientations or field of view, you will be able to manipulate the
;          size, magnification and roll of each image.
;
;          When each image is loaded it will be displayed in it's own small
;          window (200x200) with a linear stretch.  You can change the stretch
;          by entering the new min or max value in the appropriate field and
;          hitting a <CR>.  You must do the <CR> to get the new value.
;
;          You may also change the kind of stretch to logarithmic (base 10),
;          square root, or histogram equalization.  Each image works independent
;          of the others.  
;
;          NOTE: the automatic display of the linear stretch works only with
;          images read in using the buttons.  For images input on the command
;          line you must push one of the "Choose Scaling" buttons. 
;
;          Once all 3 images are loaded and displayed the "Make True Color",
;          "Edit Pixels", "Edit Images", "Rotate", "Zoom", "Arrows", 
;          "Annotate" buttons and "WRITE IMAGE" menu will become unghosted.
;
;          Push the "Make True Color" button to see the true color image
;          displayed in the large window (600x600).
;
;          NOTE: The nature of a 24bit IDL session is that to see the correct
;          color of displayed image the cursor must be in one of the display
;          windows.  This makes seeing the buttons difficult.  You get one or
;          the other.
;
;          Once the True Color image is displayed you can shift any of the
;          images using the "Shift" size field and direction buttons.  Enter
;          the size of the shift that you want and then press the direction.
;          DO NOT FORGET to PRESS the <CR> or the new size will not register.
;          You can also rotate any image and magnify or demagnify the image
;          Using the "Rotate" button.  This pops up a new widget with will
;          ask for the input values for a call to the IDL library procedure 
;          ROT.  You can also trim any image to any size using the "EDIT IMAGES"
;          button.  This pop up widget will also let you subtract a sky 
;          background.
;
;          The "EDIT PIXELS" button will pop up a widget that lets you replace
;          the values in a box of the size you specify with the median value
;          of the surrounding pixels.  You click on the True Color image on the
;          pixel at the center of the box that you want to replace.
;
;          The Zoom button will pop up anothe window in which the images can 
;          be zoomed up by up to a factor of 4.  This is usfull for checking
;          avignment and in Editing pixels.
;
;          The Arrows button will pop up a requester where the user can choose
;          the position (using X,Y coordinates or with the cursor) of where
;          to place the North and East Arrows using the ARROWS procedure.  The
;          last position chosen will be remembered and drawn on the postscript
;          output. (Not on the Tiff or Jpeg output though.).  Note: a header
;          with the correct CD matrix must be read into RHEAD using 
;          RHEAD1=header or in the fits header for the red immage.
;
;          The Annotate button will pop up a scaled down version of the IDL
;          Annotate widget.  The text and lines and arrows annotation will
;          work but the draw box, circles and polyfill have been removed.
;          The user can save to a fits table the annotation that he/she want
;          to appear on the postscript output.  Use the help button to get
;          a full description of how to use annotate.
;
;          NOTE: some of these action will automatically refresh the True Color
;          image while others will not.  Pressing the "Make True Color" button
;          will always refresh the True Color image.
;
;          Finally you may write a postscript file, a TIFF file or a JPEG
;          file of the True Color image, or write out the processed red, green
;          or blue image using the "WRITE IMAGE" pull down menu.
;
;*SUPPORT PROCEDURES:
;          del_ray_wid - for editing the pixels in a box.
;          disp_true_wid - for making the true color image and displaying it
;                          to the window or writing it out.
;          display_blue - displays the blue image
;          display_green - displays the green image
;          display_red - displays the red image
;          edit_image_wid - trims the image
;          rotat_wid - rotates the image
;          arrowid - Calls ARROWS to put N & E directional arrows on the figure
;          annotate2 - to annotate the image
;          xycenter - event handler to get the cursor location for rotat_wid
;          xytrim - event handler to get the cursor location for edit_image_wid
;          xyzap - event handler to get the cursor location for del_ray_wid.
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;       6/99    E Malumuth/RITSS - bug fixed, zoom window feature added
;       4/01    E Malumuth/RITSS - added Arrows and Annotate features
;
;
;-
PRO PDMENU97_Event, Event
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,BUTTON107, $
                   button98,button108
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON output,outfile
   COMMON headers,rhead,ghead,bhead
   COMMON WVALUES5,button118,button119
   COMMON arrowvals,arrowflg
   common annotatevals,annotateflg,annxcent,annycent,anntext

  CASE Event.Value OF 
  'WRITE IMAGE.True Color Post Script': BEGIN
      outfile=DIALOG_PICKFILE(FILTER=filt,/write)
      WIDGET_CONTROL, /HOURGLASS
      disp_true_wid,1
    END
  'WRITE IMAGE.True Color TIFF': BEGIN
      outfile=DIALOG_PICKFILE(FILTER=filt,/write)
      WIDGET_CONTROL, /HOURGLASS
      disp_true_wid,2
    END
  'WRITE IMAGE.True Color JPEG': BEGIN
      outfile=DIALOG_PICKFILE(FILTER=filt,/write)
      WIDGET_CONTROL, /HOURGLASS
      disp_true_wid,3
    END
  'WRITE IMAGE.Red Image FITS': BEGIN
    outfile=DIALOG_PICKFILE(FILTER=filt,/write)
      WIDGET_CONTROL, /HOURGLASS
    writefits,outfile+'.fits',red
    END
  'WRITE IMAGE.Green Image FITS': BEGIN
    outfile=DIALOG_PICKFILE(FILTER=filt,/write)
      WIDGET_CONTROL, /HOURGLASS
    writefits,outfile+'.fits',green
    END
  'WRITE IMAGE.Blue Image FITS': BEGIN
    outfile=DIALOG_PICKFILE(FILTER=filt,/write)
      WIDGET_CONTROL, /HOURGLASS
    writefits,outfile+'.fits',blue
    END
  ENDCASE
END

PRO MAIN13_Event, Event
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,BUTTON107, $
                   button98,button108
   COMMON WVALUES5,button118,button119
   COMMON WVALUES3, draw14, draw51
   COMMON BUTGROUP, bgroup70,bgroup71,bgroup72,bgroup73,bgroup75,bgroup80
   COMMON images,redim,greenim,blueim
   COMMON DRAW4_Comm, DRAW4_Id
   COMMON DRAW5_Comm, DRAW5_Id
   COMMON DRAW6_Comm, DRAW6_Id
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON FLAGS, rflag,gflag,bflag,editflg
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   COMMON output,outfile
   COMMON headers,rhead,ghead,bhead
   COMMON arrowvals,arrowflg
   common annotatevals,annotateflg,annxcent,annycent,anntext

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 
  'maketc': BEGIN
            WIDGET_CONTROL, /HOURGLASS
            disp_true_wid,0
            WIDGET_CONTROL, pdmenu97, SENSITIVE=1
            END
  'help': BEGIN
          xdisplayfile,find_with_def('tctool.hlp','SDOC',/nocur)
          END
  'red_image': BEGIN
      redfile=DIALOG_PICKFILE(FILTER=filt)
      WIDGET_CONTROL, /HOURGLASS
;       red=mrdfits(redfile)
;       fits_read,redfile,red,rhead
      red=readfits(redfile,rhead)
      WIDGET_CONTROL, BGROUP70, SENSITIVE=1,SET_VALUE=[1,0,0,0]
      red_scale_type=0 
      display_red,red,0
        ss=size(red)
        fxr=ss(1)/600.
        fyr=ss(2)/600.
        redim=congrid(red,600,600,/interp)
        if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
           WIDGET_CONTROL, BUTTON14, SENSITIVE=1
           WIDGET_CONTROL, BUTTON15, SENSITIVE=1
           WIDGET_CONTROL, BUTTON107, SENSITIVE=1
           WIDGET_CONTROL, BUTTON108, SENSITIVE=1
           WIDGET_CONTROL, BUTTON118, SENSITIVE=1
           WIDGET_CONTROL, BUTTON119, SENSITIVE=1
           WIDGET_CONTROL, BUTTON98, SENSITIVE=1
        endif
      END
  'red_scale_type': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      CASE Event.Value OF
      0: BEGIN
         WIDGET_CONTROL, BGROUP70,SET_VALUE=[1,0,0,0]
         red_scale_type=0 
         display_red,red,0
         END
      1: BEGIN
         WIDGET_CONTROL, BGROUP70,SET_VALUE=[0,1,0,0]
         red_scale_type=1 
         display_red,red,1
         END 
      2: BEGIN
         WIDGET_CONTROL, BGROUP70,SET_VALUE=[0,0,1,0]
         red_scale_type=2 
         display_red,red,2
         END 
      3: BEGIN
         WIDGET_CONTROL, BGROUP70,SET_VALUE=[0,0,0,1]
         red_scale_type=3
         display_red,red,3
         END 
      ELSE: Message,'Unknown button pressed'
      ENDCASE
        ss=size(red)
        fxr=ss(1)/600.
        fyr=ss(2)/600.
        redim=congrid(red,600,600,/interp)
        if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
           WIDGET_CONTROL, BUTTON14, SENSITIVE=1
           WIDGET_CONTROL, BUTTON15, SENSITIVE=1
           WIDGET_CONTROL, BUTTON107, SENSITIVE=1
           WIDGET_CONTROL, BUTTON108, SENSITIVE=1
           WIDGET_CONTROL, BUTTON118, SENSITIVE=1
           WIDGET_CONTROL, BUTTON119, SENSITIVE=1
           WIDGET_CONTROL, BUTTON98, SENSITIVE=1
        endif
      END

  'red_shift_direction': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      CASE Event.Value OF
      0: BEGIN
         red=fshift(red,-1*redshift,0) 
         display_red,red,red_scale_type,rmin=redmin,rmax=redmax
         END
      1: BEGIN
         red=fshift(red,redshift,0) 
         display_red,red,red_scale_type,rmin=redmin,rmax=redmax
         END 
      2: BEGIN
         red=fshift(red,0,redshift) 
         display_red,red,red_scale_type,rmin=redmin,rmax=redmax
         END 
      3: BEGIN
         red=fshift(red,0,-1*redshift) 
         display_red,red,red_scale_type,rmin=redmin,rmax=redmax
         END 
      ELSE: Message,'Unknown button pressed'
      ENDCASE
        redim=congrid(red,600,600,/interp)
        if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
           disp_true_wid,0
        endif
      END
  'red_shift': BEGIN
      redshift=event.value
      WIDGET_CONTROL,field73, SET_VALUE=redshift
      END
  'red_min1': BEGIN
      redmin=event.value
      display_red,red,red_scale_type,rmin=redmin,rmax=redmax
      END
  'red_max': BEGIN
      redmax=event.value
      display_red,red,red_scale_type,rmin=redmin,rmax=redmax   
      END
  'green_image': BEGIN
      greenfile=DIALOG_PICKFILE(FILTER=filt)
      WIDGET_CONTROL, /HOURGLASS
;       green=mrdfits(greenfile)
;       fits_read,greenfile,green
      green=readfits(greenfile,ghead)
      WIDGET_CONTROL, BGROUP75,SET_VALUE=[1,0,0,0],SENSITIVE=1
      green_scale_type=0 
      display_green,green,0
         ss=size(green)
         fxg=ss(1)/600.
         fyg=ss(2)/600.
         greenim=congrid(green,600,600,/interp)
         if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
            WIDGET_CONTROL, BUTTON14, SENSITIVE=1
            WIDGET_CONTROL, BUTTON15, SENSITIVE=1
            WIDGET_CONTROL, BUTTON107, SENSITIVE=1
            WIDGET_CONTROL, BUTTON108, SENSITIVE=1
            WIDGET_CONTROL, BUTTON118, SENSITIVE=1
            WIDGET_CONTROL, BUTTON119, SENSITIVE=1
            WIDGET_CONTROL, BUTTON98, SENSITIVE=1
         endif
      END
  'green_scale_type': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      CASE Event.Value OF
      0: BEGIN
         WIDGET_CONTROL, BGROUP75,SET_VALUE=[1,0,0,0]
         green_scale_type=0 
         display_green,green,0
         END
      1: BEGIN
         WIDGET_CONTROL, BGROUP75,SET_VALUE=[0,1,0,0]
         green_scale_type=1 
         display_green,green,1
         END 
      2: BEGIN
         WIDGET_CONTROL, BGROUP75,SET_VALUE=[0,0,1,0]
         green_scale_type=2 
         display_green,green,2
         END 
      3: BEGIN
         WIDGET_CONTROL, BGROUP75,SET_VALUE=[0,0,0,1]
         green_scale_type=3
         display_green,green,3
         END
      ELSE: Message,'Unknown button pressed'
      ENDCASE
         ss=size(green)
         fxg=ss(1)/600.
         fyg=ss(2)/600.
         greenim=congrid(green,600,600,/interp)
         if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
            WIDGET_CONTROL, BUTTON14, SENSITIVE=1
            WIDGET_CONTROL, BUTTON15, SENSITIVE=1
            WIDGET_CONTROL, BUTTON107, SENSITIVE=1
            WIDGET_CONTROL, BUTTON108, SENSITIVE=1
            WIDGET_CONTROL, BUTTON118, SENSITIVE=1
            WIDGET_CONTROL, BUTTON119, SENSITIVE=1
            WIDGET_CONTROL, BUTTON98, SENSITIVE=1
         endif
      END
  'green_shift_direction': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      CASE Event.Value OF
      0: BEGIN
         green=fshift(green,-1*greenshift,0) 
         display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
         END
      1: BEGIN
         green=fshift(green,greenshift,0) 
         display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
         END 
      2: BEGIN
         green=fshift(green,0,greenshift) 
         display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
         END 
      3: BEGIN
         green=fshift(green,0,-1*greenshift) 
         display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
         END 
      ELSE: Message,'Unknown button pressed'
      ENDCASE
        greenim=congrid(green,600,600,/interp)
        if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
           disp_true_wid,0
        endif
      END
  'green_shift': BEGIN
      greenshift=event.value
      WIDGET_CONTROL,field79, SET_VALUE=greenshift
      END
  'green_min1': BEGIN
      greenmin=event.value
      display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
      END
  'green_max': BEGIN
      greenmax=event.value
      display_green,green,green_scale_type,gmin=greenmin,gmax=greenmax
      END
  'blue_image': BEGIN
      bluefile=DIALOG_PICKFILE(FILTER=filt)
      WIDGET_CONTROL, /HOURGLASS
;      blue=mrdfits(bluefile)
;      fits_read,bluefile,blue
      blue=readfits(bluefile,bhead)
      WIDGET_CONTROL, BGROUP80,SET_VALUE=[1,0,0,0], SENSITIVE=1
      blue_scale_type=0 
      display_blue,blue,0
         ss=size(blue)
         fxb=ss(1)/600.
         fyb=ss(2)/600.
         blueim=congrid(blue,600,600,/interp)
         if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
            WIDGET_CONTROL, BUTTON14, SENSITIVE=1
            WIDGET_CONTROL, BUTTON15, SENSITIVE=1
            WIDGET_CONTROL, BUTTON107, SENSITIVE=1
            WIDGET_CONTROL, BUTTON108, SENSITIVE=1
            WIDGET_CONTROL, BUTTON118, SENSITIVE=1
            WIDGET_CONTROL, BUTTON119, SENSITIVE=1
            WIDGET_CONTROL, BUTTON98, SENSITIVE=1
         endif
      END
  'blue_scale_type': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      CASE Event.Value OF
      0: BEGIN
         WIDGET_CONTROL, BGROUP80,SET_VALUE=[1,0,0,0]
         blue_scale_type=0 
         display_blue,blue,0
         END
      1: BEGIN
         WIDGET_CONTROL, BGROUP80,SET_VALUE=[0,1,0,0]
         blue_scale_type=1 
         display_blue,blue,1
         END 
      2: BEGIN
         WIDGET_CONTROL, BGROUP80,SET_VALUE=[0,0,1,0]
         blue_scale_type=2 
         display_blue,blue,2
         END 
      3: BEGIN
         WIDGET_CONTROL, BGROUP80,SET_VALUE=[0,0,0,1]
         blue_scale_type=3
         display_blue,blue,3
         END
      ELSE: Message,'Unknown button pressed'
      ENDCASE
         ss=size(blue)
         fxb=ss(1)/600.
         fyb=ss(2)/600.
         blueim=congrid(blue,600,600,/interp)
         if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
            WIDGET_CONTROL, BUTTON14, SENSITIVE=1
            WIDGET_CONTROL, BUTTON15, SENSITIVE=1
            WIDGET_CONTROL, BUTTON107, SENSITIVE=1
            WIDGET_CONTROL, BUTTON108, SENSITIVE=1
            WIDGET_CONTROL, BUTTON118, SENSITIVE=1
            WIDGET_CONTROL, BUTTON119, SENSITIVE=1
            WIDGET_CONTROL, BUTTON98, SENSITIVE=1
         endif
      END
  'blue_shift_direction': BEGIN
      WIDGET_CONTROL, /HOURGLASS
      CASE Event.Value OF
      0: BEGIN
         blue=fshift(blue,-1*blueshift,0) 
         display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
         END
      1: BEGIN
         blue=fshift(blue,blueshift,0) 
         display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
         END 
      2: BEGIN
         blue=fshift(blue,0,blueshift) 
         display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
         END 
      3: BEGIN
         blue=fshift(blue,0,-1*blueshift) 
         display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
         END 
      ELSE: Message,'Unknown button pressed'
      ENDCASE
        blueim=congrid(blue,600,600,/interp)
        if (rflag eq 1 and gflag eq 1 and bflag eq 1) then begin
           disp_true_wid,0
        endif
      END
  'blue_shift': BEGIN
      blueshift=event.value
      WIDGET_CONTROL,field83, SET_VALUE=blueshift
      END
  'blue_min1': BEGIN
      bluemin=event.value
      display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
      END
  'blue_max': BEGIN
      bluemax=event.value
      display_blue,blue,blue_scale_type,bmin=bluemin,bmax=bluemax
      END
  'edit_pixels': BEGIN
        del_ray_wid,dummy
      END
  'edit_image': BEGIN
        edit_image_wid,dummy
      END
  'rotate_image': BEGIN
        rotat_wid,dummy
      END
  'write_image': BEGIN
      PDMENU97_Event, Event
      END
  'zoom_wind': BEGIN
        zoomwind,dummy
      END
  'add_arrows': BEGIN
        arrowid,dummy
      END
  'annotate': BEGIN
        WIDGET_CONTROL, DRAW14, DRAW_BUTTON_EVENTS=0
        annotate2
        annotateflg=1
        WIDGET_CONTROL, DRAW14, /DRAW_BUTTON_EVENTS
      END
  'DRAW4': BEGIN
      Print, 'Event for DRAW4'
      END
  'DRAW5': BEGIN
      Print, 'Event for DRAW5'
      END
  'DRAW6': BEGIN
      Print, 'Event for DRAW6'
      END
  'DRAW14': BEGIN
      END
  'done': BEGIN
      WIDGET_CONTROL, event.top, /DESTROY 
      END
  ENDCASE
END


;  END MAIN13

PRO tctool,  GROUP=Group, red1=red1, green1=green1, blue1=blue1, $
             rhead1=rhead1,ghead1=ghead1,bhead1=bhead1
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON WVALUES, field71,field72,field76,field77,field81,field82,button14, $
                   button15,pdmenu97,field73,field79,field83,BUTTON107, $
                   button98,button108
   COMMON WVALUES5,button118,button119
   COMMON BUTGROUP, bgroup70,bgroup71,bgroup72,bgroup73,bgroup75,bgroup80
   COMMON FLAGS, rflag,gflag,bflag,editflg
   COMMON images,redim,greenim,blueim
   COMMON WVALUES3, draw14, draw51
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   COMMON output,outfile
   COMMON headers,rhead,ghead,bhead
   COMMON arrowvals,arrowflg
   common annotatevals,annotateflg,annxcent,annycent,anntext
   rflag=0
   gflag=0
   bflag=0
   annotateflg=0
   arrowflg=0
   redshift=0.5
   blueshift=0.5
   greenshift=0.5
   draw51=-1
   if(keyword_set(rhead1)) then rhead=rhead1
   if(keyword_set(ghead1)) then ghead=ghead1
   if(keyword_set(bhead1)) then bhead=bhead1

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=2, $
      MAP=1, $
      TITLE=' TRUE COLOR IMAGE TOOL ', $
      UVALUE='MAIN13')

  BASE30 = WIDGET_BASE(MAIN13, $
                       Row=1, $
                       MAP=1, $
                       UVALUE='BASE30')

  BUTTON12 = WIDGET_BUTTON( BASE30, $
                            FONT='9x15', $
                            UVALUE='done', $
                            XSIZE=100, $
                            VALUE='Quit')

  BUTTON16 = WIDGET_BUTTON( BASE30, $
                            FONT='9x15', $
                            UVALUE='help', $
                            XSIZE=90, $
                            VALUE='Help')

  BUTTON14 = WIDGET_BUTTON( BASE30, $
                            FONT='9x15', $
                            UVALUE='maketc', $
                            XSIZE=145, $
                            VALUE='Make True Color')

  BUTTON15 = WIDGET_BUTTON( BASE30, $
                            FONT='9x15', $
                            XSIZE=130, $
                            VALUE='Edit Pixels', $
                            UVALUE='edit_pixels')

  BUTTON107 = WIDGET_BUTTON( BASE30, $
                             FONT='9x15', $
                             XSIZE=130, $
                             VALUE='Edit Images', $
                             UVALUE='edit_image')

  BUTTON98 = WIDGET_BUTTON( BASE30, $
                            FONT='9x15', $
                            XSIZE=130, $
                            VALUE='Rotate Images', $
                            UVALUE='rotate_image')

  BUTTON108 = WIDGET_BUTTON( BASE30, $
                             FONT='9x15', $
                             XSIZE=90, $
                             VALUE='Zoom', $
                             UVALUE='zoom_wind')

  BUTTON118 = WIDGET_BUTTON( BASE30, $
                             FONT='9x15', $
                             XSIZE=90, $
                             VALUE='Arrows', $
                             UVALUE='add_arrows')

  BUTTON119 = WIDGET_BUTTON( BASE30, $
                             FONT='9x15', $
                             XSIZE=100, $
                             VALUE='Annotate', $
                             UVALUE='annotate')

  MenuDesc4706 = [ $
      { CW_PDMENU_S,       3, 'WRITE IMAGE' }, $ ;                0
        { CW_PDMENU_S,       0, 'True Color Post Script' }, $ ;   1
        { CW_PDMENU_S,       0, 'True Color TIFF' }, $ ;          2
        { CW_PDMENU_S,       0, 'True Color JPEG' }, $ ;          3
        { CW_PDMENU_S,       0, 'Red Image FITS' }, $ ;           4
        { CW_PDMENU_S,       0, 'Green Image FITS' }, $ ;         5
        { CW_PDMENU_S,       2, 'Blue Image FITS' } $  ;          6
  ]


  PDMENU97 = CW_PDMENU( BASE30, MenuDesc4706, /RETURN_FULL_NAME, $
                        FONT='9x15', $
                        UVALUE='write_image')



 
  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=3, $
      MAP=1, $
      UVALUE='BASE2')

  BASE68 = WIDGET_BASE(BASE2, $
                       COLUMN=1, $
                       MAP=1, $
                       BASE_ALIGN_LEFT=1, $
                       UVALUE='BASE68')

  BASE73 = WIDGET_BASE(BASE68, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE73', $
      BASE_ALIGN_LEFT=1, $
      YSIZE=195)

  BUTTON69 = WIDGET_BUTTON( BASE73, $
                            FONT='9x15', $
                            UVALUE='red_image', $
                            XSIZE=250, $
                            VALUE='Read Red Image')

  Btns3778 = [ $
              'Linear', $
              'Logarithmic', $
              'Square Root', $
              'Histogram Eq.' ]
  BGROUP70 = CW_BGROUP( BASE73, Btns3778, $
                        FONT='9x15', $
                        ROW=2, $
                        NONEXCLUSIVE=1, $
                        FRAME=1, $
                        NO_RELEASE=1, $
                        LABEL_TOP='Choose Scaling', $
                        UVALUE='red_scale_type')

  BASE74 = WIDGET_BASE(BASE73, $
      COLUMN=3, $
      MAP=1, $
      UVALUE='BASE74', $
      BASE_ALIGN_LEFT=1, $
      YSIZE=195)


 FieldVal3780 = [ $
                  '0.0' ]
  FIELD71 = CW_FIELD( BASE74,VALUE=FieldVal3780, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Min', $
                      XSIZE=7, $
                      UVALUE='red_min1')

  FieldVal3782 = [ $
                  '0.0' ]
  FIELD72 = CW_FIELD( BASE74,VALUE=FieldVal3782, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Max', $
                      XSIZE=7, $
                      UVALUE='red_max')

  FieldVal3781 = [ $
                  '0.5' ]
  FIELD73 = CW_FIELD( BASE74,VALUE=FieldVal3781, $
                      FONT='9x15', $
                      COLUMN=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Shift', $
                      XSIZE=5, $
                      UVALUE='red_shift')

  Btns3779 = [ $
              'Left', $
              'Right', $
              ' Up ', $
              'Down ' ]
  BGROUP71 = CW_BGROUP( BASE74, Btns3779, $
                        FONT='9x15', $
                        ROW=2, $
                        NO_RELEASE=1, $
                        UVALUE='red_shift_direction')




  BASE78 = WIDGET_BASE(BASE68, $
                       COLUMN=1, $
                       FRAME=1, $
                       MAP=1, $
                       UVALUE='BASE78', $
                       BASE_ALIGN_LEFT=1, $
                       YSIZE=195)

  BUTTON74 = WIDGET_BUTTON( BASE78, $
                            FONT='9x15', $
                            UVALUE='green_image', $
                             XSIZE=250, $
                           VALUE='Read Green Image')

  Btns3786 = [ $
              'Linear', $
              'Logarithmic', $
              'Square Root', $
              'Histogram Eq.' ]
  BGROUP75 = CW_BGROUP( BASE78, Btns3786, $
                        FONT='9x15', $
                        ROW=2, $
                        NONEXCLUSIVE=1, $
                        NO_RELEASE=1, $
                        FRAME=1, $
                        LABEL_TOP='Choose Scaling', $
                        UVALUE='green_scale_type')

  BASE79 = WIDGET_BASE(BASE78, $
      COLUMN=3, $
      MAP=1, $
      UVALUE='BASE79', $
      BASE_ALIGN_LEFT=1, $
      YSIZE=195)


  FieldVal3788 = [ $
                  '0.0' ]
  FIELD76 = CW_FIELD( BASE79,VALUE=FieldVal3788, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Min', $
                      XSIZE=7, $
                      UVALUE='green_min1')

  FieldVal3790 = [ $
                  '0.0' ]
  FIELD77 = CW_FIELD( BASE79,VALUE=FieldVal3790, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Max', $
                      XSIZE=7, $
                      UVALUE='green_max')

  FieldVal3791 = [ $
                  '0.5' ]
  FIELD79 = CW_FIELD( BASE79,VALUE=FieldVal3791, $
                      FONT='9x15', $
                      COLUMN=1, $
                      FLOAT=1, $
                      TITLE='Shift', $
                      RETURN_EVENTS=1, $
                      XSIZE=5, $
                      UVALUE='green_shift')

  Btns3792 = [ $
              'Left', $
              'Right', $
              ' Up ', $
              'Down ' ]
  BGROUP72 = CW_BGROUP( BASE79, Btns3792, $
                        FONT='9x15', $
                        NO_RELEASE=1, $
                        ROW=2, $
                        UVALUE='green_shift_direction')

  BASE83 = WIDGET_BASE(BASE68, $
                       COLUMN=1, $
                       FRAME=1, $
                       MAP=1, $
                       UVALUE='BASE83', $
                       BASE_ALIGN_LEFT=1, $
                       YSIZE=195)

  BUTTON79 = WIDGET_BUTTON( BASE83, $
                            FONT='9x15', $
                            UVALUE='blue_image', $
                            XSIZE=250, $
                            VALUE='Read Blue Image')

  Btns3794 = [ $
              'Linear', $
              'Logarithmic', $
              'Square Root', $
              'Histogram Eq.' ]
  BGROUP80 = CW_BGROUP( BASE83, Btns3794, $
                        FONT='9x15', $
                        ROW=2, $
                        NONEXCLUSIVE=1, $
                        FRAME=1, $
                        NO_RELEASE=1, $
                        LABEL_TOP='Choose Scaling', $
                        UVALUE='blue_scale_type')

  BASE84 = WIDGET_BASE(BASE83, $
      COLUMN=3, $
      MAP=1, $
      UVALUE='BASE84', $
      BASE_ALIGN_LEFT=1, $
      YSIZE=195)

  FieldVal3796 = [ $
                  '0.0' ]
  FIELD81 = CW_FIELD( BASE84,VALUE=FieldVal3796, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Min', $
                      XSIZE=7, $
                      UVALUE='blue_min1')

  FieldVal3798 = [ $
                  '0.0' ]
  FIELD82 = CW_FIELD( BASE84,VALUE=FieldVal3798, $
                      FONT='9x15', $
                      ROW=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Max', $
                      XSIZE=7, $
                      UVALUE='blue_max')
  FieldVal3793 = [ $
                  '0.5' ]
  FIELD83 = CW_FIELD( BASE84,VALUE=FieldVal3793, $
                      FONT='9x15', $
                      COLUMN=1, $
                      FLOAT=1, $
                      RETURN_EVENTS=1, $
                      TITLE='Shift', $
                      XSIZE=5, $
                      UVALUE='blue_shift')

  Btns3792 = [ $
              'Left', $
              'Right', $
              ' Up ', $
              'Down ' ]
  BGROUP73 = CW_BGROUP( BASE84, Btns3792, $
                        FONT='9x15', $
                        NO_RELEASE=1, $
                        ROW=2, $
                        UVALUE='blue_shift_direction')




  BASE3 = WIDGET_BASE(BASE2, $
                      COLUMN=1, $
                      MAP=1, $
                      UVALUE='BASE3')

  DRAW4 = WIDGET_DRAW( BASE3, $
                       FRAME=1, $
                       RETAIN=2, $
                       UVALUE='DRAW4', $
                       XSIZE=200, $
                       YSIZE=200)

  DRAW5 = WIDGET_DRAW( BASE3, $
                       FRAME=1, $
                       RETAIN=2, $
                       UVALUE='DRAW5', $
                       XSIZE=200, $
                       YSIZE=200)

  DRAW6 = WIDGET_DRAW( BASE3, $
                       FRAME=1, $
                       RETAIN=2, $
                       UVALUE='DRAW6', $
                       XSIZE=200, $
                       YSIZE=200)


  BASE13 = WIDGET_BASE(BASE2, $
                       ROW=1, $
                       FRAME=1, $
                       MAP=1, $
                       UVALUE='BASE13')

  DRAW14 = WIDGET_DRAW( BASE13, $
                        RETAIN=2, $
                        UVALUE='DRAW14', $
                        BUTTON_EVENTS=1, $
                        XSIZE=600, $
                        YSIZE=600)



  WIDGET_CONTROL, MAIN13, /REALIZE
  WIDGET_CONTROL, BUTTON14, SENSITIVE=0
  WIDGET_CONTROL, BUTTON15, SENSITIVE=0
  WIDGET_CONTROL, BUTTON107, SENSITIVE=0
  WIDGET_CONTROL, BUTTON108, SENSITIVE=0
  WIDGET_CONTROL, BUTTON118, SENSITIVE=0
  WIDGET_CONTROL, BUTTON119, SENSITIVE=0
  WIDGET_CONTROL, BUTTON98, SENSITIVE=0
  WIDGET_CONTROL, BGROUP70, SENSITIVE=0
  WIDGET_CONTROL, BGROUP75, SENSITIVE=0
  WIDGET_CONTROL, BGROUP80, SENSITIVE=0
  WIDGET_CONTROL, pdmenu97, SENSITIVE=0
   if (keyword_set(red1)) then begin
     red=red1   
     WIDGET_CONTROL, BGROUP70, SENSITIVE=1
   endif
   if (keyword_set(green1)) then begin
     green=green1
     WIDGET_CONTROL, BGROUP75, SENSITIVE=1
   endif
   if (keyword_set(blue1)) then begin
     blue=blue1
     WIDGET_CONTROL, BGROUP80, SENSITIVE=1
   endif


  ; Get drawable window index

  COMMON DRAW4_Comm, DRAW4_Id
  WIDGET_CONTROL, DRAW4, GET_VALUE=DRAW4_Id

  ; Get drawable window index

  COMMON DRAW5_Comm, DRAW5_Id
  WIDGET_CONTROL, DRAW5, GET_VALUE=DRAW5_Id

  ; Get drawable window index

  COMMON DRAW6_Comm, DRAW6_Id
  WIDGET_CONTROL, DRAW6, GET_VALUE=DRAW6_Id

  ; Get drawable window index

  COMMON DRAW14_Comm, DRAW14_Id
  WIDGET_CONTROL, DRAW14, GET_VALUE=DRAW14_Id

  XMANAGER, 'MAIN13', MAIN13, /no_block, cleanup=0
END
