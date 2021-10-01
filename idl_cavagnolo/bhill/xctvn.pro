; $Id: xctvn.pro,v 1.1.1.1 2007-02-03 17:10:49 cavagnolo Exp $
;+
;   NAME:              XCTVN
;
;   PURPOSE:   This procedure displays a series of images in a widget
;              for examination and comparison.  Each image is
;              displayed in one plane.  Only one plane is visible at
;              a time, and the others remain unmapped.  Each plane
;              has its own zoom and pan window and image examination
;              buttons.  Blinking is available.
;
;   CALLING SEQUENCE:
;  
;       xctvn, image0, image1, image2, image3, image4, $
;              image5, image6, image7, image8, image9
;
;   POSITIONAL PARAMETERS:
;
;       image0 - image9     Ten or fewer images to be displayed.
;                           If they aren't the same size, reasonable
;                           results are NOT guaranteed.
;
;       image0              An alternative way of specifying the images
;                           is for image0 to be an image cube containing
;                           all of them.  In this case, you could
;                           probably have more than ten, but you couldn't
;                           specify headers for all of them.
;
;   KEYWORD PARAMETERS:
;
;       H1 - H9             FITS headers corresponding to image0 to image9.
;
;       XSIZE, YSIZE        Pixel size of main display window.
;       XLIM, YLIM          Maximum pixel size of scrolling viewport of
;                           main display window.
;       X_SCROLL_SIZE, Y_SCROLL_SIZE    Pixel size of scrolling viewport
;                                       of main display window.
;
;       The [XY]LIM, [XY]SIZE, and [XY]SCROLL_SIZE parameters interact
;       with the actual size of image0 to give the display size.  Most useful
;       for the typical user would be [XY]LIM.
;
;       ZOOM_SIZE           Pixel size of zoom window.
;       PAN_SIZE            Pixel size of pan window.
;
;   NOTES:
;      The latest version and supporting procedures are 
;      available in http://idlastro.gsfc.nasa.gov/ftp/contrib/bhill/
;   MODIFICATION HISTORY:
;       Written by R. S. Hill, RITSS, 6 Oct 2000
;       Pan window keeps aspect ratio as close as possible to image.
;	Scale byte images 0 - 255 on initial display.  RSH, SSAI, 26 Sep 2001
;       Set retain=2 by default.  RSH, 26 Sep. 2001
;-

PRO XCTVN_CLEANUP, Tlb

widget_control, tlb, get_uvalue=info

ndsp = n_elements((*info).dsp_array)
FOR i=0,ndsp-1 DO obj_destroy, ((*info).dsp_array)[i]

IF ptr_valid(info) THEN ptr_free, info

RETURN
END

PRO XCTVN_EVENT, Event

widget_control, event.top, get_uvalue=info

IF widget_info((*info).msg_text, /valid) THEN $
    widget_control, (*info).msg_text, set_value=' '

IF event.id EQ (*info).exit_button THEN BEGIN
    widget_control, (*info).tlb, /destroy
ENDIF ELSE IF event.id EQ (*info).select_bgroup THEN BEGIN
    val = strtrim(event.value, 2)
    w = (where((*info).names EQ val))[0]
    FOR i=0,n_elements((*info).map_array)-1 DO $
        widget_control, ((*info).map_array)[i], map=0
    widget_control, ((*info).map_array)[w], map=1
    ((*info).dsp_array)[w]->set_sender
ENDIF

RETURN
END


PRO XCTVN, Image0, Image1, Image2, Image3, Image4, $
           Image5, Image6, Image7, Image8, Image9, $
           H0=h0, H1=h1, H2=h2, H3=h3, H4=h4, $
           H5=h5, H6=h6, H7=h7, H8=h8, H9=h9, $
           XSIZE=xsize, YSIZE=ysize, $
           XLIM=xlim, YLIM=ylim, $
           X_SCROLL_SIZE=x_scroll_size, $
           Y_SCROLL_SIZE=y_scroll_size, $
           ZOOM_SIZE=zoom_size, PAN_SIZE=pan_size, $
           PAN_LIMITS=pan_limits, RETAIN=retain, _EXTRA=extra

IF n_params(0) LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  XCTVN, Image0, Image1, ..., Image9'
    print, 'KEYWORD PARAMETERS:  XSIZE, YSIZE, XLIM, YLIM, '
    print, 'X_SCROLL_SIZE, Y_SCROLL_SIZE, ZOOM_SIZE, PAN_SIZE, '
    print, 'PAN_LIMITS, RETAIN, _EXTRA'
    RETURN
ENDIF

nimages = n_params(0)

sz = size(image0)
ndim = sz[0]
xdim = sz[1]
ydim = sz[2]

IF ndim EQ 3 THEN nimages = sz[3]

IF ndim LE 2 THEN BEGIN
    type = intarr(nimages)
    FOR i=0, nimages-1 DO BEGIN
        cmd = 'sz = size(image' + strtrim(string(i),2) + ')'
	r = execute(cmd)
	type[i] = sz[n_elements(sz)-2]
    ENDFOR
ENDIF ELSE BEGIN
    type = replicate(sz[n_elements(sz)-2],nimages)
ENDELSE
;
;  Boil down size specs and defaults.
IF n_elements(xsize) LE 0 THEN xsize=xdim
IF n_elements(ysize) LE 0 THEN ysize=ydim
IF n_elements(xlim)  LE 0 THEN xlim=720
IF n_elements(ylim)  LE 0 THEN ylim=450
IF n_elements(zoom_size) LE 1 THEN zoom_size=200
IF n_elements(pan_limits) LE 1 THEN pan_limits=[500,200]
IF n_elements(pan_size) EQ 1 OR n_elements(pan_size) GT 2 THEN $
    message, 'Explicit pan_size must have two elements.'
IF n_elements(pan_size) LE 1 THEN   $
    pan_size=[round(200.0d0*double(xdim)/double(ydim)), 200]
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
;   Set up widget.

tlb = widget_base(col=1, title='XCTVN')

row0 = widget_base(tlb, row=1)
row1 = widget_base(tlb, row=1)
row1a = widget_base(tlb, row=1)
row1c = widget_base(tlb, row=1)
row2 = widget_base(tlb, row=1)

exit_button = widget_button(row0, value='EXIT', /no_release)
overlay = widget_base(row2)

map_array = lonarr(nimages)
title_array = lonarr(nimages)
dsp_array = objarr(nimages)
names = strarr(nimages)

image0_frame = widget_base(overlay, col=1)
image0_label = widget_label(image0_frame, value='Image 0, Window XX')
IF nimages LT 2 THEN multifunc=0 ELSE multifunc=1
image0_dsp = obj_new('GANGEXAMINE', image0_frame, $
                    xsize=xsize, ysize=ysize, $
                    x_scroll_size=x_scroll_size, $
                    y_scroll_size=y_scroll_size, $
                    /ps_dialog, zoomwin=zoom_size, $
                    panwin=pan_size, /readout, $
                    log_tag='image0', $
                    sync=multifunc, blink=multifunc, label='Image 0')

widget_control, image0_frame, map=1
map_array[0] = image0_frame
dsp_array[0] = image0_dsp
title_array[0] = image0_label
names[0] = 'Image 0'

prev_dsp = image0_dsp

FOR j=1,nimages-1 DO BEGIN

    title = 'Image '+strn(j)+ ', Window XX'
    next_frame = widget_base(overlay, col=1)
    next_label = widget_label(next_frame, value=title)
    log_tag = 'image'+strn(j)
    label = 'Image '+strn(j)
    name = 'Image '+strn(j)
    next_dsp = obj_new('GANGEXAMINE', next_frame, prev_dsp, image0_dsp, $
                        xsize=xsize, ysize=ysize, $
                        x_scroll_size=x_scroll_size, $
                        y_scroll_size=y_scroll_size, $
                        /ps_dialog, zoomwin=zoom_size, $
                        panwin=pan_size, /readout, $
                        log_tag=log_tag, sync=1, blink=1, label=label)

    prev_dsp = next_dsp

    widget_control, next_frame, map=0
    map_array[j] = next_frame
    dsp_array[j] = next_dsp
    title_array[j] = next_label
    names[j] = name

ENDFOR


IF nimages GT 1 THEN BEGIN
    select_bgroup = cw_bgroup(row1, names, /exclusive, /no_release, $
                              /return_name, label_left='Selection', $
                              row=2, set_value=0)
ENDIF ELSE BEGIN
    select_bgroup = -1L
ENDELSE

widget_control, tlb, /realize

msg_base = widget_base(/floating, group_leader=tlb, /modal, $
                       title='Loading...')
msg_text = widget_text(msg_base, xsize=64, ysize=1)
widget_control, msg_base, /realize

;   Display the images.

FOR j=0,nimages-1 DO BEGIN
    msg = 'Loading image number '+strn(j)
    widget_control, msg_text, set_value=msg
    IF type[j] EQ 1 THEN dsp_array[j]->setdisplay, minval=0, maxval=255
    IF ndim EQ 2 THEN BEGIN
        cmd = 'dsp_array[j]->draw, image'+strn(j)+', h'+strn(j)
    ENDIF ELSE BEGIN
        cmd = 'dsp_array[j]->draw, image0[*,*,j]'+', h'+strn(j)
    ENDELSE
    r = execute(cmd)
    dsp_array[j]->size, maindisp=maindisp
    widget_control, maindisp, get_value=mw
    widget_control, title_array[j], set_value='Image '+strn(j) $
                                              +', Window '+strn(mw)
ENDFOR
widget_control, msg_text, set_value='All images loaded'
wait, 1.2
widget_control, msg_base, /destroy

info = {tlb:tlb, select_bgroup:select_bgroup, $
        exit_button:exit_button, $
        names:names, map_array:map_array, $
        dsp_array:dsp_array, msg_text:msg_text}

widget_control, tlb, set_uvalue=ptr_new(info)

xmanager, 'xctvn', tlb, /no_block, cleanup='xctvn_cleanup', $
          event_handler='xctvn_event'

RETURN
END
