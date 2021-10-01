;+
;  NAME:            ADJPLOT
;
;  PURPOSE:
;    Generates a widget containing a plot with X and Y ranges
;    adjustable using sliders.
;
;  CALLING SEQUENCES (see individual routines for arguments
;    and keywords):
;
;    ADJPLOT, X, Y, GROUP_LEADER=group_leader, $
;      XSIZE=xsize, YSIZE=ysize, TOP_ID=top_id, WTITLE=wtitle, $
;      CURSOR_COLOR=cursor_color, CLICK_PRO=click_pro, $
;      BASE_ID=base_id, TITLE=title, XTITLE=xtitle, $
;      YTITLE=ytitle, CLICK_PTR=click_ptr, FREE_BLOCK=free_block, $
;      _EXTRA=extra
;
;    ADJPLOT_REPLOT, Top_id, X, Y, XSTATIC=xstatic, $
;      YSTATIC=ystatic, YRELATIVE=yrelative, _Extra=extra
;
;    ADJPLOT_CLICK, Top_id, Click_pro, Click_ptr
;
;    ADJPLOT_CLEANUP, Top_id
;
;    ADJPLOT_EVENT, Event
;
;  MODIFICATION HISTORY:
;     Written by R. S. Hill, RITSS, 6 Aug. 1999
;     Various improvements.  RSH, Aug.-Sep. 1999
;     Documented.  RSH, 5 Oct. 1999
;     Square brackets around subscripts.  RETALL changed to RETURN
;        in short doc section.  RSH, 3 July 2000
;-

PRO ADJPLOT_CLEANUP, Top_id
;
;  Actions that are done when the widget is destroyed.  Just freeing
;  up of storage to avoid memory leaks.
;  

widget_control, top_id, get_uvalue=info
set_plot,'x'
wdelete, (*info).shadowgraph
IF ptr_valid((*info).extra) THEN ptr_free, (*info).extra
IF ptr_valid((*info).title) THEN ptr_free, (*info).title
IF ptr_valid((*info).xtitle) THEN ptr_free, (*info).xtitle
IF ptr_valid((*info).ytitle) THEN ptr_free, (*info).ytitle
IF ptr_valid((*info).x) THEN ptr_free, (*info).x
IF ptr_valid((*info).y) THEN ptr_free, (*info).y
IF ptr_valid((*info).prev) THEN ptr_free, (*info).prev
IF ptr_valid((*info).click_pro) THEN ptr_free, (*info).click_pro
IF ptr_valid(info) THEN ptr_free, info
RETURN
END


PRO ADJPLOT_EVENT, Event
;
;  Event handler for DOPLOTZ
;

widget_control, event.handler, get_uvalue=info
killit=0b
postscript=0b
update_plot=1b

;
;  Handle extra keywords
IF ptr_valid((*info).extra) THEN extra=*(*info).extra

;
;  Get current values of plot limits
widget_control, (*info).xmintext, get_value=txt
xmin = float(txt[0])
widget_control, (*info).xmaxtext, get_value=txt
xmax = float(txt[0])
widget_control, (*info).ymintext, get_value=txt
ymin = float(txt[0])
widget_control, (*info).ymaxtext, get_value=txt
ymax = float(txt[0])
;
;  Get current axis scalings
widget_control, (*info).xlog, get_value=xlog
xlog = xlog[0]
widget_control, (*info).ylog, get_value=ylog
ylog = ylog[0]

CASE event.id OF
   ;
   ;  Set flag to make B/W hardcopy later
   (*info).postscript_button:  BEGIN
       postscript = 1b
   END
   ;
   ;  Process X min text value
   (*info).xmintext:  BEGIN
       xminmm = widget_info((*info).xminslider, /slider_min_max)
       val = round((xmin-(*info).abscmin)/(*info).abscrange*1000.0)
       val = (val>0)<999
       widget_control, (*info).xminslider, $
           set_slider_min=(val<xminmm[0])
       widget_control, (*info).xminslider, set_value=val
       widget_control, (*info).xmaxslider, set_slider_min=val>1
   END
   ;
   ;  Process X max text value
   (*info).xmaxtext:  BEGIN
       xmaxmm = widget_info((*info).xmaxslider, /slider_min_max)
       val = round((xmax-(*info).abscmin)/(*info).abscrange*1000.0)
       val = (val>1)<1000
       widget_control, (*info).xmaxslider, $
           set_slider_min=(val>xmaxmm[0])
       widget_control, (*info).xmaxslider, set_value=val
       widget_control, (*info).xminslider, set_slider_max=val<999
   END
   ;
   ;  Process Y min text value
   (*info).ymintext:  BEGIN
       yminmm = widget_info((*info).yminslider, /slider_min_max)
       val = round((ymin-(*info).ordmin)/(*info).ordrange*1000.0)
       val = (val>0)<999
       widget_control, (*info).yminslider, $
           set_slider_min=(val<yminmm[0])
       widget_control, (*info).yminslider, set_value=val
       widget_control, (*info).ymaxslider, set_slider_min=val>1
   END
   ;
   ;  Process Y max text value
   (*info).ymaxtext:  BEGIN
       ymaxmm = widget_info((*info).ymaxslider, /slider_min_max)
       val = round((ymax-(*info).ordmin)/(*info).ordrange*1000.0)
       val = (val>1)<1000
       widget_control, (*info).ymaxslider, $
           set_slider_min=(val>ymaxmm[0])
       widget_control, (*info).ymaxslider, set_value=val
       widget_control, (*info).yminslider, set_slider_max=val<999
   END
   ;
   ;  Process X min slider value
   (*info).xminslider:  BEGIN
       val = event.value < 999
       xmin = val/1000.0*(*info).abscrange + (*info).abscmin
       widget_control, (*info).xmintext, set_value=strtrim(xmin,2)
       widget_control, (*info).xmaxslider, set_slider_min=(val>1)
   END
   ;
   ;  Process X max slider value
   (*info).xmaxslider:  BEGIN
       val = event.value > 0
       xmax = val/1000.0*(*info).abscrange + (*info).abscmin
       widget_control, (*info).xmaxtext, set_value=strtrim(xmax,2)
       widget_control, (*info).xminslider, set_slider_max=(val<999)
   END
   ;
   ;  Process Y min slider value
   (*info).yminslider:  BEGIN
       val = event.value < 999
       ymin = val/1000.0*(*info).ordrange + (*info).ordmin
       widget_control, (*info).ymintext, set_value=strtrim(ymin,2)
       widget_control, (*info).ymaxslider, set_slider_min=(val>1)
   END
   ;
   ;  Process Y max slider value
   (*info).ymaxslider:  BEGIN
       val = event.value > 0
       ymax = val/1000.0*(*info).ordrange + (*info).ordmin
       widget_control, (*info).ymaxtext, set_value=strtrim(ymax,2)
       widget_control, (*info).yminslider, set_slider_max=(val<999)
   END
   ;
   ;  Process motion or tracking event or button click
   (*info).plotter:  BEGIN
       wset, (*info).plotgraph
       sname = tag_names(event, /struct)
       IF sname EQ 'WIDGET_TRACKING' THEN BEGIN
           IF event.enter THEN BEGIN
               device, cursor_image=intarr(16), cursor_xy=[8,8]
           ENDIF ELSE BEGIN
               device, /cursor_crosshair
           ENDELSE
       ENDIF ELSE BEGIN
           device,copy=[0,(*info).oldy,(*info).xsize,1, $
                        0,(*info).oldy, (*info).shadowgraph]
           device,copy=[(*info).oldx,0,1,(*info).ysize, $
                        (*info).oldx, 0, (*info).shadowgraph]
           plots, [event.x,event.x], [0, (*info).ysize-1], /dev, $
               color=(*info).cursor_color
           plots, [0, (*info).xsize-1], [event.y,event.y], /dev, $
               color=(*info).cursor_color
           (*info).oldy = event.y
           (*info).oldx = event.x
           coo = convert_coord(event.x,event.y,/device,/to_data)
           xx = coo[0] & yy = coo[1]
           widget_control, (*info).xreadout, set_value=strn(xx)
           widget_control, (*info).yreadout, set_value=strn(yy)
           update_plot = 0b
           IF event.type EQ 0 THEN BEGIN
               call_procedure, *(*info).click_pro, event.x, event.y, $
                   (*info).click_ptr
           ENDIF
       ENDELSE
   END
   ;
   ;  Set flag to exit
   (*info).quit:  killit = 1b
   ELSE:
ENDCASE

IF killit THEN BEGIN
    ;
    ;  Exit
    IF widget_info((*info).base_id, /valid) THEN BEGIN
        widget_control, (*info).base_id, /destroy
    ENDIF ELSE BEGIN
        widget_control, (*info).top_id, /destroy
    ENDELSE
ENDIF ELSE BEGIN
    IF update_plot THEN BEGIN
        ;
        ;  Set graphics device; if postscript, get filename, do setup
        psopen = 0b
        IF postscript THEN BEGIN
            xsize = 9.5 & ysize = 7.0 
            xoffset = 0.5 & yoffset = 10.25
            psfn = dialog_pickfile( file=*(*info).prev, $
                       group=(*info).top_id, $
                       title='Specify Output PostScript File', $
                       filter='*.ps', /write)
            IF psfn NE '' THEN BEGIN
                r = execute("openw, lun, '" + psfn + "' , /get_lun")
                IF r THEN BEGIN
                    free_lun, lun
                    svdev = !D.name
                    set_plot, 'ps'
                    device, /landscape, /inches, $
                        xoffset=xoffset, yoffset=yoffset, $
                        xsize=xsize, ysize=ysize, /isolatin1, $
                        filename=psfn
                    IF ptr_valid((*info).prev) THEN ptr_free, (*info).prev
                    (*info).prev = ptr_new(psfn)
                    psopen = 1b
                ENDIF ELSE BEGIN
                    mess = ['Cannot write file', psfn]
                    junk = dialog_message(mess, /error)
                ENDELSE
            ENDIF
        ENDIF ELSE BEGIN
            wset, (*info).plotgraph
        ENDELSE
        ;
        ;   Fix log scaling for neg values
        plot_valid=1b
        IF xlog THEN BEGIN
            IF xmin LT 0 AND xmax GT 0 THEN BEGIN
                xmin=0.0001*xmax
                val = ceil((xmin - (*info).abscmin)*1000.0/(*info).abscrange)
                val = (val>0)<1000
                widget_control, (*info).xminslider, set_value=val
                widget_control, (*info).xmintext, set_value=strn(xmin)
                widget_control, (*info).xmaxslider, set_slider_min=(val>1)
            ENDIF
            IF xmax LT 0 THEN BEGIN
                plot_valid=0b
            ENDIF
        ENDIF
        IF ylog THEN BEGIN
            IF ymin LT 0 AND ymax GT 0 THEN BEGIN
                ymin=0.0001*ymax
                val = ceil((ymin - (*info).ordmin)*1000.0/(*info).ordrange)
                val = (val>0)<1000
                widget_control, (*info).yminslider, set_value=val
                widget_control, (*info).ymintext, set_value=strn(ymin)
                widget_control, (*info).ymaxslider, set_slider_min=(val>1)
            ENDIF
            IF ymax LE 0 THEN BEGIN
                plot_valid=0b
            ENDIF
        ENDIF
        ;
        ;  Make the plot
        IF plot_valid THEN BEGIN
            plot, *(*info).x, *(*info).y, $
                xrange=[xmin,xmax], yrange=[ymin,ymax], $
                xlog=xlog, ylog=ylog, xtitle=*(*info).xtitle, $
                ytitle=*(*info).ytitle, title=*(*info).title, $
                /xstyle, /ystyle, _extra=extra
        ENDIF ELSE BEGIN
            erase
            xyouts, 0.1, 0.5, 'PLOT LIMITS OR', /norm, charsize=2
            xyouts, 0.1, 0.3, 'SCALING INVALID', /norm, charsize=2
        ENDELSE
        IF (!D.flags AND 256) GT 0 THEN BEGIN
            wset, (*info).shadowgraph
            device, copy=[0,0,(*info).xsize, (*info).ysize, 0,0, $
                          (*info).plotgraph]
            wset, (*info).plotgraph
        ENDIF
        ;
        ;  Close postscript file if necessary and reset device
        IF psopen THEN BEGIN
            device, /close
            set_plot, svdev
        ENDIF
    ENDIF
ENDELSE

RETURN
END


PRO ADJPLOT_CLICK, Top_id, Click_pro, Click_ptr
;
;   Procedure hook for clicking on plot window.
;
;   Input Arguments:
;     Top_id ............... Widget ID of ADJPLOT container,
;                            with info as uvalue.
;     Click_pro ............ (string)  Name of click-handling
;                            procedure.  Must have calling sequence
;                            schematically as follows:
;             MY_CLICK_PRO, Xcursor, Ycursor, Pointer_to_other_data
;     Click_ptr ............ Pointer to anything, to be used by
;                            your click_pro.
;
;   RSH, RITSS, 5 Oct. 1999
;
widget_control, top_id, get_uvalue=info
IF ptr_valid((*info).click_pro) THEN ptr_free, (*info).click_pro
(*info).click_pro = ptr_new(click_pro)
(*info).click_ptr = click_ptr
RETURN
END


PRO ADJPLOT_REPLOT, Top_id, X, Y, XSTATIC=xstatic, $
    YSTATIC=ystatic, YRELATIVE=yrelative, _Extra=extra
;
;   Replot in the same frame.
;
;   Input Parameters:
;      Top_id:   Widget ID of ADJPLOT container, with info
;                pointer as uvalue.
;      X, Y:     Plot vectors.
;   
;   Input Keyword Flags (1 or 0):
;      /XSTATIC:  Keep same X plot limits.
;      /YSTATIC:  Keep same Y plot limits.
;      /YRELATIVE:   Keep same Y plot limits, as expressed
;                    in standard deviations away from the mean.
;
;   Input Keyword Parameters:
;      _EXTRA:    Any other plot keywords.
;
;   RSH, RITSS, Aug.-Sep., 1999
;

xstatic = keyword_set(xstatic)
ystatic = keyword_set(ystatic)

widget_control, top_id, get_uvalue=info

;
;   Save extra
IF n_elements(extra) GE 1 THEN BEGIN
    IF ptr_valid((*info).extra) THEN ptr_free, (*info).extra
    (*info).extra = ptr_new(extra)
ENDIF ELSE BEGIN
    IF ptr_valid((*info).extra) THEN extra = *(*info).extra
ENDELSE

;   Get plot window ID
widget_control, (*info).plotter, get_value=plotgraph

;   Get plot data if necessary
IF n_elements(x) LT 1 THEN x = temporary(*(*info).x)
IF n_elements(y) LT 1 THEN y = temporary(*(*info).y)

;
;   Initialize plot
set_plot, 'x'
wset, plotgraph
actualmin = min(y,max=actualmax)
actualrange = actualmax-actualmin
ordmin = actualmin - 0.05*actualrange
ordrange = actualrange*1.1
ordmax = ordmin+ordrange
actualmin = min(x,max=actualmax)
actualrange = actualmax-actualmin
abscmin = actualmin - 0.05*actualrange
abscrange = actualrange*1.1
abscmax = abscmin+abscrange
;
;  Get current axis scalings
widget_control, (*info).xlog, get_value=xlog
xlog = xlog[0]
widget_control, (*info).ylog, get_value=ylog
ylog = ylog[0]
widget_control, (*info).xmintext, get_value=txt
xmin = float(txt[0])
widget_control, (*info).xmaxtext, get_value=txt
xmax = float(txt[0])
widget_control, (*info).ymintext, get_value=txt
ymin = float(txt[0])
widget_control, (*info).ymaxtext, get_value=txt
ymax = float(txt[0])
IF xstatic THEN BEGIN
    xrange = [xmin, xmax]
ENDIF ELSE BEGIN
    widget_control, (*info).xmintext, set_value=strtrim(abscmin,2)
    widget_control, (*info).xmaxtext, set_value=strtrim(abscmax,2)
    widget_control, (*info).xminslider, set_value=0
    widget_control, (*info).xmaxslider, set_value=1000
    xrange = [abscmin, abscmax]
ENDELSE
IF ystatic THEN BEGIN
    yrange = [ymin, ymax]
ENDIF ELSE BEGIN
    IF yrelative THEN BEGIN
        n_old_y = n_elements(*(*info).y)
        old_mean_y = total(*(*info).y)/n_old_y
        old_var_y = total((*(*info).y - old_mean_y)^2/(n_old_y - 1))
        old_stdev_y = sqrt(old_var_y)
        old_std_min = (ymin - old_mean_y)/old_stdev_y
        old_std_max = (ymax - old_mean_y)/old_stdev_y
        n_new_y = n_elements(y)
        new_mean_y = total(y)/n_new_y
        new_var_y = total((y - new_mean_y)^2/(n_new_y - 1))
        new_stdev_y = sqrt(new_var_y)
        new_ymin = new_mean_y + old_std_min*new_stdev_y 
        new_ymax = new_mean_y + old_std_max*new_stdev_y 
        yrange = [new_ymin, new_ymax]
        widget_control, (*info).ymaxtext, set_value=strtrim(new_ymax,2)
        widget_control, (*info).ymintext, set_value=strtrim(new_ymin,2)
        widget_control, (*info).yminslider, set_value=10
        widget_control, (*info).ymaxslider, set_value=990
    ENDIF ELSE BEGIN
        widget_control, (*info).ymintext, set_value=strtrim(ordmin,2)
        widget_control, (*info).ymaxtext, set_value=strtrim(ordmax,2)
        widget_control, (*info).yminslider, set_value=0
        widget_control, (*info).ymaxslider, set_value=1000
        yrange = [ordmin, ordmax]
    ENDELSE
ENDELSE
plot, x, y, xrange=xrange, /xstyle, $
            yrange=yrange, /ystyle, xlog=xlog, ylog=ylog, $
            xtitle=*(*info).xtitle, ytitle=*(*info).ytitle, $
            title=*(*info).title, _extra=extra
wset, (*info).shadowgraph
device, copy=[0,0,(*info).xsize, (*info).ysize, 0,0, $
              (*info).plotgraph]
wset, (*info).plotgraph

(*info).ordmin = ordmin
(*info).ordrange = ordrange
(*info).abscmin = abscmin
(*info).abscrange = abscrange
IF ptr_valid((*info).x) THEN ptr_free, (*info).x
IF ptr_valid((*info).y) THEN ptr_free, (*info).y
(*info).x = ptr_new(x)
(*info).y = ptr_new(y)

RETURN
END


PRO ADJPLOT, X, Y, GROUP_LEADER=group_leader, $
    XSIZE=xsize, YSIZE=ysize, TOP_ID=top_id, WTITLE=wtitle, $
    CURSOR_COLOR=cursor_color, CLICK_PRO=click_pro, $
    BASE_ID=base_id, TITLE=title, XTITLE=xtitle, $
    YTITLE=ytitle, CLICK_PTR=click_ptr, FREE_BLOCK=free_block, $
    _EXTRA=extra

;
;   NAME:   ADJPLOT
;
;   PURPOSE:
;     Make a plot with X and Y ranges adjustable using sliders.
;
;   CATEGORY:   Widgets
;
;   INPUT ARGUMENTS:
;     X, Y ........... Vectors to be plotted.
;
;   INPUT KEYWORDS:
;     GROUP_LEADER= ...... Widget group leader.
;     XSIZE=, YSIZE= ..... Size of plot window in pixels.
;                          Default:  XSIZE=520, YSIZE=360
;     BASE_ID= ........... Widget ID of parent to this widget. 
;                          Default:  This program generates its own
;                          top-level base.
;     TITLE=, XTITLE=, 
;       YTITLE= .......... Usual plot title keywords.
;                          Default:  null strings
;     CURSOR_COLOR= ...... Color of plot crosshairs.  Default=!P.color
;     CLICK_PRO= ......... (string)  Name of procedure to be used
;                          to handle mouse button clicks on plot.
;                          Default=ignore clicks.
;     CLICK_PTR= ......... (pointer)  Points to any arbitrary data
;                          to be used by click_pro.  May be omitted.
;     _EXTRA ............. Other plot keywords.
;
;   OUTPUT KEYWORDS:
;     TOP_ID ............. Widget ID of highest-level base
;                          created by this program.
;     FREE_BLOCK ......... A subordinate base widget under which some
;                          other program might put additional controls.
;   MODIFICATION HISTORY:
;     6 Aug. 1999    - Written.  RSH, RITSS
;     11 Aug. 1999   - Option to put under other base.  RSH
;     5 Oct. 1999    - Documented.  RSH
;

IF n_params(0) LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  ADJPLOT, X, Y'
    print, 'KEYWORD PARAMETERS:   GROUP_LEADER, XSIZE, YSIZE, $'
    print, 'TOP_ID, WTITLE, BASE_ID, TITLE, XTITLE, YTITLE, $'
    print, 'CURSOR_COLOR, CLICK_PRO, CLICK_PTR, FREE_BLOCK, $'
    print, '_EXTRA'
    RETURN
ENDIF

IF n_elements(wtitle) LT 1 THEN wtitle='ADJPLOT'
IF n_elements(xsize) NE 1 THEN xsize=520
IF n_elements(ysize) NE 1 THEN ysize=360
IF n_elements(title) LT 1 THEN title=''
IF n_elements(xtitle) LT 1 THEN xtitle=''
IF n_elements(ytitle) LT 1 THEN ytitle=''
IF n_elements(extra) GE 1 THEN ptr_extra=ptr_new(extra) $
                          ELSE ptr_extra=ptr_new()
IF n_elements(cursor_color) LT 1 THEN cursor_color=!P.color
IF n_elements(click_pro) LT 1 THEN click_pro=''
IF n_elements(click_ptr) LT 1 THEN click_ptr=ptr_new()

nx = n_elements(x)

IF keyword_set(base_id) THEN BEGIN
    top_id = widget_base(base_id, title=wtitle, col=1)
ENDIF ELSE BEGIN
    top_id = widget_base(title=wtitle, group_leader=group_leader,col=1)
ENDELSE

plot_block = widget_base(top_id, row=1)
control_blocks = widget_base(top_id,row=1)
block1 = widget_base(control_blocks, col=1)
row2 = widget_base(block1, row=1)
row3 = widget_base(block1, row=1)
row4 = widget_base(block1, row=1)
row5 = widget_base(block1, row=1)
row6 = widget_base(block1, row=1)
row7 = widget_base(block1, row=1)
free_block = widget_base(control_blocks, col=1)

;
;   Plot window
plotter = widget_draw(plot_block, xsize=xsize, ysize=ysize, $
                      /motion_events, /tracking_events, $
                      button_events=(click_pro NE ''))
set_plot, 'x'
window, /pixmap, /free, xsize=xsize, ysize=ysize
shadowgraph = !D.window

;
;   Buttons to change scaling type
xlog = cw_bgroup(row2, ['lin','log'], label_top='X Scaling', $
                 /exclusive, set_value=0,/row)
ylog = cw_bgroup(row2, ['lin','log'], label_top='Y Scaling', $
                 /exclusive, set_value=0,/row)
xreadout = widget_label(row2, value='',xsize=130,ysize=20)
yreadout = widget_label(row2, value='',xsize=130,ysize=20)


;
;   Fields and sliders for user to change X plot limits
xminlab = widget_label(row4, value='X min:')
xmintext = widget_text(row4, xsize=15, /editable)
xminslider = widget_slider(row4, /drag, scroll=1, xsize=300, $
    min=0, max=1000, value=0, /suppress_value)
xmaxlab = widget_label(row3, value='X max:')
xmaxtext = widget_text(row3, xsize=15, /editable)
xmaxslider = widget_slider(row3, /drag, scroll=1, xsize=300, $
    min=0, max=1000, value=1000, /suppress_value)

;
;   Y plot limits
yminlab = widget_label(row6, value='Y min:')
ymintext = widget_text(row6, xsize=15, /editable)
yminslider = widget_slider(row6, /drag, scroll=1, xsize=300, $
    min=0, max=1000, value=0, /suppress_value)
ymaxlab = widget_label(row5, value='Y max:')
ymaxtext = widget_text(row5, xsize=15, /editable)
ymaxslider = widget_slider(row5, /drag, scroll=1, xsize=300, $
    min=0, max=1000, value=1000, /suppress_value)

;
;   Hard copy buttons
postscript_button = widget_button(row7, value='PostScript', /no_rel)


;
;   Quit button
quit  = widget_button(row7, value='QUIT', /no_rel)

;
;   Make the widget
IF keyword_set(base_id) THEN BEGIN
    widget_control, base_id, /realize
ENDIF ELSE BEGIN
    widget_control, top_id, /realize
    base_id = -1L
ENDELSE

;
;   Get plot window ID (can't be done until realized)
widget_control, plotter, get_value=plotgraph

;
;   Initialize plot
set_plot, 'x'
wset, plotgraph
actualmin = min(y,max=actualmax)
actualrange = actualmax-actualmin
ordmin = actualmin - 0.05*actualrange
ordrange = actualrange*1.1
ordmax = ordmin+ordrange
actualmin = min(x,max=actualmax)
actualrange = actualmax-actualmin
abscmin = actualmin - 0.05*actualrange
abscrange = actualrange*1.1
abscmax = abscmin+abscrange
widget_control, xmintext, set_value=strtrim(abscmin,2)
widget_control, xmaxtext, set_value=strtrim(abscmax,2)
widget_control, ymintext, set_value=strtrim(ordmin,2)
widget_control, ymaxtext, set_value=strtrim(ordmax,2)
plot, x, y, xrange=[abscmin,abscmax], /xstyle, $
            yrange=[ordmin,ordmax], /ystyle, $
            xtitle=xtitle, ytitle=ytitle, $
            title=title, _extra=extra
wset, shadowgraph
device, copy=[0,0,xsize, ysize, 0,0, plotgraph]
wset, plotgraph

;
;   Info structure
info = ptr_new({top_id:top_id, quit:quit, plotter:plotter, $
               base_id:base_id, $
               postscript_button:postscript_button, $
               xlog:xlog, ylog:ylog, $
               xmintext:xmintext, xmaxtext:xmaxtext, $
               ymintext:ymintext, ymaxtext:ymaxtext, $
               xminslider:xminslider, xmaxslider:xmaxslider, $
               yminslider:yminslider, ymaxslider:ymaxslider, $
               xreadout:xreadout, yreadout:yreadout, $
               ordmin:ordmin, ordrange:ordrange, $
               abscmin:abscmin, abscrange:abscrange, $
               plotgraph:plotgraph, $
               shadowgraph:shadowgraph, $
               xsize:xsize, ysize:ysize, $
               oldx:0L, oldy:0L, $
               x:ptr_new(x), y:ptr_new(y), $
               title:ptr_new(title), $
               xtitle:ptr_new(xtitle), $
               ytitle:ptr_new(ytitle), $
               extra:ptr_extra, $
               prev:ptr_new('idl.ps'), $
               cursor_color:cursor_color, $
               click_pro:ptr_new(click_pro), $
               click_ptr:click_ptr})

widget_control, top_id, set_uvalue=info

;
;   Hand over control to xmanager
xmanager, 'adjplot', top_id, /no_block, cleanup='adjplot_cleanup'

RETURN
END
