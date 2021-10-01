; $Id: xctv.pro,v 1.1.1.1 2007-02-03 17:10:49 cavagnolo Exp $
;+
;   NAME:              XCTV
;
;   PURPOSE:
;      This set of procedures implements a display widget for
;      roaming, zooming, scaling, and examining pixel values in
;      an image.    
;
;   CALLING SEQUENCE:
;      XCTV, Array [, Header]
;
;   POSITIONAL PARAMETERS:
;      Input:
;         Array:  Image to be displayed.
;         Header: FITS header (optional, if you want RA, Dec output)
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
;            of main image display.
;         XSIZE, YSIZE:  Size of main image display, if not same
;            as input array size.
;         ZOOM_SIZE:  Size of zoom window (always square), pixels.
;         PSFILE:  Initial file name for hardcopy.  
;         PSTITLE:  Initial title for hardcopy.
;         PAN_SIZE:  Size of pan window (always square), pixels.
;         NO_COPY:  Flag to cause input array to be deallocated
;            and kept only in the display widget internal storage.
;      Output:
;         DSP:   Object reference for display.
;         GWIN:  Graphics window identifier for the main window.
;
;   METHOD:
;      Uses the EXAMDISPLAY object class, which inherits from
;      IMGDISPLAY.  Although the coding is object-oriented,
;      this class uses IDL direct graphics, not object graphics.
;      IMGDISPLAY provides the capabilities for display, roaming,
;      hardcopy, etc.
;
;   USAGE NOTES:
;      Layout is a panel containing a  main window, a pan window, 
;      and a zoom window:
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
;
;      The following widget buttons are provided:
;         QUIT WIDGET
;         Display Controls -- creates a popup to do image scaling,
;            hardcopy
;         Zoom x.x  -- droplist to choose a zoom factor:
;            click it, hold mouse button down, and 
;            drag highlight to desired value
;
;   EXAMPLE:
;      stis_cr, 10001, h, d
;      xctv, d
;   NOTES:
;      The latest version and supporting procedures are 
;      available in http://idlastro.gsfc.nasa.gov/ftp/contrib/bhill/
;
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, Raytheon ITSS, 26 May 1999    
;      Switched from IMGDISPLAY to EXAMDISPLAY.  RSH, 5 Aug 99
;      Added RA, Dec readout.  RSH, 14 Sep 99
;      Made header more robust.  Eliminated some unused code.
;        RSH, 12 Oct. 1999
;      Checked for valid header if input.  RSH, 9 May 2000
;      Square brackets on subscripts.  MESSAGE error handling.  
;        RSH, 28 June 2000
;      Attempt to preserve aspect ratio in pan image.  RSH, 25 Sep 2001
;      Do not scale byte image on first display.  RSH, SSAI, 25 Sep 2001
;      Set retain=2 by default.  RSH, 26 Sep. 2001
;
;-
PRO xctv_cleanup, tlb

widget_control, tlb, get_uvalue=info
IF obj_valid((*info).dsp) THEN obj_destroy, (*info).dsp

IF ptr_valid(info) THEN ptr_free, info

RETURN
END

PRO XCTV, Array, Header, DSP=dsp, GWIN=gwin, $
    GROUP_LEADER=group_leader,XSIZE=xsize,YSIZE=ysize, $
    PSFILE=psfile,PSTITLE=pstitle,X_SCROLL_SIZE=x_scroll_size, $
    Y_SCROLL_SIZE=y_scroll_size,XLIM=xlim,YLIM=ylim, $
    NO_COPY=no_copy,ZOOM_SIZE=zoom_size, $
    PAN_SIZE=pan_size, PAN_LIMITS=pan_limits, RETAIN=retain

on_error, 2
    
IF n_params(0) LT 1 THEN BEGIN
    print,'CALLING SEQUENCE:  XCTV, Array [, Header]'
    print,'KEYWORD PARAMETERS:  ' 
    print,'  GWIN,GROUP_LEADER, XSIZE, YSIZE, PSFILE, PSTITLE, '
    print,'  X_SCROLL_SIZE,Y_SCROLL_SIZE,XLIM,YLIM, '
    print,'  ZOOM_SIZE,PAN_SIZE,PAN_LIMITS,NO_COPY,RETAIN' 
    RETURN
ENDIF

;
;  Check arguments.
nel = n_elements(array)
sz = size(array)
ndim = sz[0]
type = sz[n_elements(sz)-2]
IF nel GT 0 AND ndim EQ 2 THEN BEGIN
    xdim = sz[1] & ydim = sz[2]
ENDIF ELSE BEGIN
    message, 'Bad ARRAY.'
ENDELSE
nelh = n_elements(header)
szh = size(header)
ndimh = szh[0]
typeh = szh[n_elements(szh)-2]
IF (nelh GT 0) AND (ndimh NE 1 OR typeh NE 7) THEN BEGIN
    message, 'Bad HEADER.'
ENDIF
IF nelh GT 0 THEN hu = header

;
;  Boil down size specs and defaults.
IF n_elements(xsize) LE 0 THEN xsize=xdim
IF n_elements(ysize) LE 0 THEN ysize=ydim
IF n_elements(xlim)  LE 0 THEN xlim=800
IF n_elements(ylim)  LE 0 THEN ylim=600
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
;  Create top level base (overall frame).
tlb = widget_base(title='XCTV',col=1,group_leader=group_leader)

;dfltfont='6x13'
;widget_control, default_font=dfltfont

;
;  Create display.
dsp = obj_new('EXAMDISPLAY', tlb, xsize=xsize, ysize=ysize, $
                         /kill_button, log_tag='xctv', $
                         x_scroll_size=x_scroll_size, $
                         y_scroll_size=y_scroll_size, $
                         /ps_dialog,zoomwin=zoom_size, $
                         panwin=pan_size,/readout, $
                         header=hu)

;
;  Generate on screen.
widget_control, tlb, /realize

;
;  Set up Postscript file name default.
IF n_elements(pstitle) LE 0 THEN pstitle=''
IF n_elements(psfile) LE 0 THEN psfile='idl.ps'

dsp->psfile, psfile, pstitle
IF type EQ 1 THEN dsp->setdisplay, minval=0, maxval=255
IF keyword_set(no_copy) THEN BEGIN
    dsp->draw, temporary(array)
ENDIF ELSE BEGIN
    dsp->draw, array
ENDELSE

;
;  Get graphics window ID.
dsp->id, draw_wid
widget_control, draw_wid, get_value=gwin
widget_control, tlb, tlb_set_title='Window '+strn(gwin)

wset, gwin

;
;  Set up shared information.
info = ptr_new({tlb:tlb, dsp:dsp})

widget_control, tlb, set_uvalue=info

;
;  Hand off to widget manager.
xmanager, 'xctv', tlb, /no_block, cleanup='xctv_cleanup'

RETURN
END
