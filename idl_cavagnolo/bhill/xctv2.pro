; $Id: xctv2.pro,v 1.1.1.1 2007-02-03 17:10:49 cavagnolo Exp $
;+
;   NAME:              XCTV2
;
;   PURPOSE:
;      This set of procedures implements a display widget for
;      roaming, zooming, scaling, and examining pixel values in two
;      images simultaneously.
;
;   CALLING SEQUENCE:
;      XCTV2, Array1, Array2 [, H1, H2]
;      XCTV2, Array1, Array2, H1               ; first header only
;      XCTV2, Array1, Array2, 0, H2            ; second header only
;
;   POSITIONAL PARAMETERS:
;      Input:
;         Array1:  First image to be displayed.
;         Array2:  Second image to be displyed.  Generally, the two
;                  images should be the same size, since direct
;                  comparison is the point.
;         H1:      Header of first image (optional)
;         H2:      Header of second image (optional)
;
;   KEYWORD PARAMETERS:
;      Input:
;         GROUP_LEADER:  Widget group leader, if this widget is
;            called from another widget program.
;         Of the following size keywords, the most useful are
;            XLIM and YLIM; the least useful are XSIZE and YSIZE.
;         XLIM, YLIM:  Biggest size of image permitted without
;            resorting to scrollbars.
;         X_SCROLL_SIZE, Y_SCROLL_SIZE:  Size of visible portion
;            of main image displays.
;         XSIZE, YSIZE:  Size of main image displays, if not same
;            as input array sizes.
;         ZOOM_SIZE:  Size of zoom window (always square), pixels.
;         PSFILE:  Initial file name for hardcopy.  
;         PSTITLE:  Initial title for hardcopy.
;         PAN_SIZE:  Size of pan window (always square), pixels.
;         NO_COPY:  Flag to cause input arrays to be deallocated
;            and kept only in the display widget internal storage.
;      Output:
;         DSP1:    Object reference for the first display.
;         DSP2:    Object reference for the second display.  These
;                  will in general only be useful if you are trying
;                  to do something fancy or to use help,/obj to
;                  see how the objects are laid out.
;         GWIN:  Two-element array containing the graphics window
;            identifiers for the left and right main windows,
;            respectively.
;
;   METHOD:
;      Uses the GANGEXAM object class, which inherits from both
;      IMGDISPLAY and REPEATER.  Although the coding is object-oriented,
;      these classes use IDL direct graphics, not object graphics.
;      IMGDISPLAY provides the capabilities for display, roaming,
;      hardcopy, etc., while REPEATER enables one display to broadcast
;      its cursor events to the other, so you can operate both displays
;      simultaneously.
;
;   USAGE NOTES:
;      Layout is two side-by-side panels.  Each panel contains a 
;      main window, a pan window, and a zoom window:
;            pan window - a boxaveraged version of the whole image
;            main window - the full resolution image, with scroll
;               bars if necessary
;            zoom window - a subimage at the cursor position,
;               enlarged
;      To pan, you put the mouse cursor in the pan window, hold down
;      the left mouse button, and move around.  To stop, let go of 
;      the mouse button.
;      To move the zoom window, you do the same maneuver, except with
;      the mouse cursor in the main window.
;      The left and right images will do the same thing if you are
;      panning/zooming in the left panel; if you pan/zoom in the
;      right panel, then the left panel will not follow along.  You
;      can change this and choose the right panel to be the
;      controller:  just double click any of the three windows in the
;      right panel.  
;
;      The following widget buttons are provided:
;         Top left:
;            QUIT WIDGET
;            Display Controls -- creates a popup to do image scaling,
;               hardcopy for left panel only
;            Zoom x.x  -- droplist to choose a zoom factor for left
;               panel only:  click it, hold mouse button down, and 
;               drag highlight to desired value
;         Top right:
;            Display Controls -- creates a popup to do image scaling,
;               hardcopy for right panel only
;            Zoom x.x  -- dropllist to choose a zoom factor for right
;               panel only
;            Same Scale -- push to make the image scaling in absolute
;               units the same for both panels (the pan/zoom controlling
;               panel is also the controlling panel for this).
;
;   EXAMPLE:
;      stis_read, 10001, h1, d1, readout=1
;      stis_read, 10001, h2, d2, readout=2
;      xctv2, d1, d2
;
;   NOTES:
;      The latest version and supporting procedures are 
;      available in http://idlastro.gsfc.nasa.gov/ftp/contrib/bhill/
;
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, Raytheon ITSS, 26 May 1999    
;      Same-scale button.  RSH, RITSS, 11 May 1999
;      Made H1 and H2 positional.  Eliminated some unused code. 
;        RSH, 12 Oct. 1999
;      Square brackets on subscripts.  MESSAGE error handling.  
;        RSH, 28 June 2000
;      Only one quit button.  RSH, 17 Nov 2000
;      Pan window keeps aspect ratio as close as possible to image.
;      Scale byte images 0 - 255 on initial display.  RSH, SSAI, 26 Sep 2001
;      Set retain=2 by default.  RSH, 26 Sep. 2001
;
;-

PRO xctv2_cleanup, tlb

widget_control, tlb, get_uvalue=info
IF obj_valid((*info).dsp1) THEN obj_destroy, (*info).dsp1
IF obj_valid((*info).dsp2) THEN obj_destroy, (*info).dsp2

IF ptr_valid(info) THEN ptr_free, info

RETURN
END

PRO XCTV2, Array1, Array2, H1, H2, $
    DSP1=dsp1, DSP2=dsp2, GWIN=gwin, $
    GROUP_LEADER=group_leader,XSIZE=xsize,YSIZE=ysize, $
    PSFILE=psfile,PSTITLE=pstitle,X_SCROLL_SIZE=x_scroll_size, $
    Y_SCROLL_SIZE=y_scroll_size,XLIM=xlim,YLIM=ylim, $
    ZOOM_SIZE=zoom_size, PAN_SIZE=pan_size, NO_COPY=no_copy, $
    PAN_LIMITS=pan_limits, RETAIN=retain

on_error, 2

IF n_params(0) LT 2 THEN BEGIN
    print,'CALLING SEQUENCE:  XCTV2, Array1, Array2, H1, H2'
    print,'            OR     XCTV2, Array1, Array2, H1'
    print,'            OR     XCTV2, Array1, Array2, 0, H2'
    print,'            OR     XCTV2, Array1, Array2'
    print,'KEYWORD PARAMETERS:  ' 
    print,'  DSP1, DSP2, GWIN, GROUP_LEADER, XSIZE, YSIZE, '
    print,'  PSFILE, PSTITLE, X_SCROLL_SIZE, Y_SCROLL_SIZE, '
    print,'  XLIM, YLIM, ZOOM_SIZE, PAN_YSIZE, PAN_LIMITS, '
    print,'  RETAIN, NO_COPY'
    RETURN
ENDIF


;
;  Check arguments.
sz = size(array1)
ndim = sz[0]
nel = sz[ndim+2]
type1 = sz[n_elements(sz)-2]
IF nel GT 0 AND ndim EQ 2 THEN BEGIN
    xdim = sz[1] & ydim = sz[2]
ENDIF ELSE BEGIN
    message, 'Bad ARRAY1.'
ENDELSE

sz = size(array2)
ndim = sz[0]
nel = sz[ndim+2]
type2 = sz[n_elements(sz)-2]
IF nel GT 0 AND ndim EQ 2 THEN BEGIN
    xdim2 = sz[1] & ydim2 = sz[2]
ENDIF ELSE BEGIN
    message, 'Bad ARRAY2.'
ENDELSE

IF keyword_set(h1) THEN BEGIN
    nelh1 = n_elements(h1)
    szh1 = size(h1)
    ndimh1 = szh1[0]
    typeh1 = szh1[n_elements(szh1)-2]
    IF (nelh1 GT 0) AND (ndimh1 NE 1 OR typeh1 NE 7) THEN BEGIN
        message, 'Bad HEADER1.'
    ENDIF
    IF nelh1 GT 0 THEN hu1 = h1
ENDIF

nelh2 = n_elements(h2)
szh2 = size(h2)
ndimh2 = szh2[0]
typeh2 = szh2[n_elements(szh2)-2]
IF (nelh2 GT 0) AND (ndimh2 NE 1 OR typeh2 NE 7) THEN BEGIN
    message, 'Bad HEADER2.'
ENDIF
IF nelh2 GT 0 THEN hu2 = h2

;
;  Boil down the window size parameters, using defaults as necessary.
IF n_elements(xsize) LE 0 THEN xsize=xdim
IF n_elements(ysize) LE 0 THEN ysize=ydim
IF n_elements(xlim)  LE 1 THEN xlim=500
IF n_elements(ylim)  LE 1 THEN ylim=500
IF n_elements(zoom_size) LE 1 THEN zoom_size=200
IF n_elements(pan_limits) LE 1 THEN pan_limits=[500,200]
IF n_elements(pan_size) EQ 1 OR n_elements(pan_size) GT 2 THEN $
    message, 'Explicit pan_size must have two elements.'
IF n_elements(pan_size) LE 1 THEN   $
    pan_size=[round(200.0d0*double(xdim)/double(ydim)), 200]
pan_fill_fac = max(double(pan_size)/double(pan_limits))
pan_size = round(pan_size/pan_fill_fac)
IF n_elements(x_scroll_size) LT 1 THEN BEGIN
    IF xsize GT xlim THEN x_scroll_size=xlim $
                     ELSE x_scroll_size=xsize
ENDIF
IF n_elements(y_scroll_size) LT 1 THEN BEGIN
    IF ysize GT ylim THEN y_scroll_size=ylim $
                     ELSE y_scroll_size=ysize
ENDIF

IF n_elements(retain) LT 1 THEN retain=2
device, retain=retain

;
;  Create top level base widget (overall frame).
tlb = widget_base(title='XCTV2',row=1,group_leader=group_leader)

;dfltfont='6x13'
;widget_control, default_font=dfltfont

;
;  Create image displays.
dsp1 = obj_new('GANGEXAMINE', tlb, xsize=xsize, ysize=ysize, $
                         /kill_button, log_tag='xctv2a', $
                         x_scroll_size=x_scroll_size, $
                         y_scroll_size=y_scroll_size, $
                         /ps_dialog,zoomwin=zoom_size, $
                         panwin=pan_size,/readout,/vertical, $
                         header=hu1)
dsp2 = obj_new('GANGEXAMINE', tlb, dsp1, dsp1, $
                         xsize=xsize, ysize=ysize, $
                         kill_button=0, log_tag='xctv2b', $
                         x_scroll_size=x_scroll_size, $
                         y_scroll_size=y_scroll_size, $
                         /ps_dialog,zoomwin=zoom_size, $
                         panwin=pan_size,/readout,/vertical, $
                         /sync, header=hu2, /blink)

;
;  Generate on screen.
widget_control, tlb, /realize

;
;  Set up Postscript file name defaults.
IF n_elements(pstitle) LE 0 THEN pstitle=''
IF n_elements(psfile) LE 0 THEN psfile='idl.ps'

dsp1->psfile, psfile, pstitle
dsp2->psfile, psfile, pstitle

;
;  Display images.
IF type1 EQ 1 THEN dsp1->setdisplay, minval=0, maxval=255
IF type2 EQ 1 THEN dsp2->setdisplay, minval=0, maxval=255
IF keyword_set(no_copy) THEN BEGIN
    IF n_params(0) GT 0 THEN dsp1->draw, temporary(array1)
    IF n_params(0) GT 1 THEN dsp2->draw, temporary(array2)
ENDIF ELSE BEGIN
    IF n_params(0) GT 0 THEN dsp1->draw, array1
    IF n_params(0) GT 1 THEN dsp2->draw, array2
ENDELSE

;
;  Get graphics window IDs.
dsp1->id, draw_wid1
dsp2->id, draw_wid2
widget_control, draw_wid1, get_value=gwin1
widget_control, draw_wid2, get_value=gwin2
widget_control, tlb, tlb_set_title='Windows '+strn(gwin1) + $
                                   ' and '+strn(gwin2)

wset, gwin1
gwin = [gwin1,gwin2]

;
;  Set up shared information.
info = ptr_new({tlb:tlb, dsp1:dsp1, dsp2:dsp2})

widget_control, tlb, set_uvalue=info

;
;  Hand over to widget manager.
xmanager, 'xctv2', tlb, /no_block, cleanup='xctv2_cleanup'

RETURN
END
