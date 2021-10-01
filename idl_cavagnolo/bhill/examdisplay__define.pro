;+
;   NAME:
;                 EXAMDISPLAY
;
;   PURPOSE:
;      Object-oriented image display widget for image display and for
;      numerical examination via plots and listings.
;
;   EXPLANATION:
;      These routines set up an object-oriented widget for image display
;      and examination.  The object orientation is in the programming
;      method, not the graphics, which are traditional IDL direct graphics.
;      For arguments, see the individual routines below, especially the
;      init method, and also imgdisplay__define.pro, which sets up the 
;      object from which this one inherits.
;
;   INHERITS:
;      imgdisplay
;
;   CATEGORY:
;      Widgets.
;
;   CALLING SEQUENCE FOR INITIALIZATION:
;      new = obj_new('examdisplay', parent, keywords=.....)
;
;   ARGUMENT:
;      parent:          widget ID of parent base widget
;
;   KEYWORD INPUTS:
;      See imgdisplay__define.pro
;      Also there are the following:
;      header:    Image header
;
;   RESTRICTIONS:
;      8-bit displays only. 
;
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, Raytheon ITSS, 4 Aug 99
;      Various upgrades and bugfixes.  RSH, 15 Sep 99
;      Add logging of output data.  RSH, 21 Mar 2000
;      Square brackets for subscripts.  Initial log filename tag 
;          is an input.  RSH, 27 June 2000
;      Destroy help button when exam subwidget morphs.  Fixed font
;          for dataset listings.  RSH, 20 Aug 2000
;      Aperture photometry.  Style altered so the you don't have
;          to keep pressing the EXAMINE button.  More subwidgets
;          stay in existence until dismissed.  RSH, 31 Aug 2000
;      Aperture growth curve photometry.  Whole-image statistics.
;          RSH, 6 Sep 2000
;      Fixed small image display in growth curve phot for big
;          radii.  RSH, 20 Sep 2000
;      /NOEXAMINE added.  RSH, 6 Oct 2000
;      Log file changed to printing to standard output.  RSH, 8 Nov 2000
;      If logging is on, simple text display widgets are not opened
;          and the use must scroll back in the regular text window.
;          Logging is turned on at entry.  ROI photometry save file
;          names differentiated by window number.  RSH, 15 Nov 2000
;      XDISPSTR substituted for XDISPLAYFILE.  RSH, 17 Nov 2000
;-


PRO EXAMDISPLAY::DESTROYBUTTONS, Info
;
;  Destroy buttons in exam overview widget when reusing the same
;  base (for functions that use current cursor position).
;
nml = n_elements((*info).misc_labels)
FOR i=0,nml-1 DO widget_control, (*info).misc_labels[i], /destroy
RETURN
END



PRO EXAMDISPLAY::IMLIST, Info
;
;  Generate simple 11 X 11 pixel value listing.  Uses current cursor
;  position.
;
;; self->destroybuttons, info
imlist, *self.image, self.xcursor>0, self.ycursor>0, $
    textout='_examdisplay_tmp.prt', descrip='EXAMDISPLAY'
readstring, '_examdisplay_tmp.prt', strarray
widget_control, self.maindisp, get_value=mw
IF self.logging THEN BEGIN
    print, ' '
    print, '---------- (X,Y) = (', strn(self.xcursor), ',', $
           strn(self.ycursor), ') ------------------------------' $
           + '----- Window ' + strn(mw)
    FOR i=0,n_elements(strarray)-1 DO print, strarray[i]
ENDIF ELSE BEGIN
    title = 'Pixel Listing (Window '+strn(mw)+')'
    xdispstr, strarray, title=title, $
        group=(*info).maintlb, $
        font=*self.fixedfont,width=80,height=24
ENDELSE
RETURN
END


PRO EXAMDISPLAY::RSTATS, Info
;
;  Generate listing of Tukey's robust statistics, useful for
;  getting a handle on outliers.  Uses pixels in current zoom box.
;  Test for valid subscripts.  RSH, 21 Mar 2000
;
;; self->destroybuttons, info
x0 = self.xzstart
x1 = self.xzend
y0 = self.yzstart
y1 = self.yzend
IF x0 GE 0 AND x1 GE 0 AND y0 GE 0 AND y1 GE 0 THEN BEGIN
    rstat, (*self.image)[x0:x1,y0:y1], $
        textout='_examdisplay_tmp.prt', descrip='EXAMDISPLAY'
    readstring, '_examdisplay_tmp.prt', strarray
    widget_control, self.maindisp, get_value=mw
    IF self.logging THEN BEGIN
        print, ' '
        print, '---------- (X,Y) = (', strn(x0)+':'+strn(x1), ',', $
               strn(y0)+':'+strn(y1),  $
               ') -------------------------' $
               + '---- Window ' + strn(mw)
        FOR i=0,n_elements(strarray)-1 DO print, strarray[i]
    ENDIF ELSE BEGIN
        title = 'Outlier Statistics in Zoom Box (Window '+strn(mw)+')'
        xdispstr, strarray, title=title, $
            group=(*info).maintlb, $
            font=*self.fixedfont,width=65,height=25
    ENDELSE
ENDIF
RETURN
END


PRO EXAMDISPLAY::ALLRSTATS, Info
;
;  Generate listing of Tukey's robust statistics, useful for
;  getting a handle on outliers.  Uses whole image as input.
;  RSH, 6 Sep 2000
;
;; self->destroybuttons, info
x0 = self.xzstart
x1 = self.xzend
y0 = self.yzstart
y1 = self.yzend
IF ptr_valid(self.image) THEN BEGIN
    rstat, *self.image, $
        textout='_examdisplay_tmp.prt', descrip='EXAMDISPLAY'
    readstring, '_examdisplay_tmp.prt', strarray
    widget_control, self.maindisp, get_value=mw
    IF self.logging THEN BEGIN
        print, ' '
        print, '---------- Whole Image', $
               ' ------------------------------' $
               + '--------- Window ' + strn(mw)
        FOR i=0,n_elements(strarray)-1 DO print, strarray[i]
    ENDIF ELSE BEGIN
        title = 'Outlier Statistics in Whole Image (Window '+strn(mw)+')'
        xdispstr, strarray, title=title, $
            group=(*info).maintlb, $
            font=*self.fixedfont,width=65,height=25
    ENDELSE
ENDIF
RETURN
END



PRO EXAMDISPLAY::STATS, Info
;
;  Generate listing of the usual statistics:  mean, std dev, etc.
;  Includes a simple little histogram by sigma-sized bins.  Uses
;  pixels in current zoom box.
;  Test for valid subscripts.  RSH, 21 Mar 2000
;
;; self->destroybuttons, info
x0 = self.xzstart
x1 = self.xzend
y0 = self.yzstart
y1 = self.yzend
IF x0 GE 0 AND x1 GE 0 AND y0 GE 0 AND y1 GE 0 THEN BEGIN
    qstat, (*self.image)[x0:x1,y0:y1], $
        textout='_examdisplay_tmp.prt', descrip='EXAMDISPLAY'
    readstring, '_examdisplay_tmp.prt', strarray
    widget_control, self.maindisp, get_value=mw
    IF self.logging THEN BEGIN
        print, ' '
        print, '---------- (X,Y) = (', strn(x0)+':'+strn(x1), ',', $
               strn(y0)+':'+strn(y1),  $
               ') -------------------------' $
               + '---- Window ' + strn(mw)
        FOR i=0,n_elements(strarray)-1 DO print, strarray[i]
    ENDIF ELSE BEGIN
        title = 'Statistics in Zoom Box (Window '+strn(mw)+')'
        xdispstr, strarray, title=title, $
            group=(*info).maintlb, $
            font=*self.fixedfont,width=80,height=21
    ENDELSE
ENDIF
RETURN
END



PRO EXAMDISPLAY::ALLSTATS, Info
;
;  Generate listing of the usual statistics:  mean, std dev, etc.
;  on the whole image.  Includes a simple little histogram by 
;  sigma-sized bins.
;  RSH, 6 Sep 2000
;
IF ptr_valid(self.image) THEN BEGIN
    qstat, *self.image, $
        textout='_examdisplay_tmp.prt', descrip='EXAMDISPLAY'
    readstring, '_examdisplay_tmp.prt', strarray
    widget_control, self.maindisp, get_value=mw
    IF self.logging THEN BEGIN
        print, ' '
        print, '---------- Whole Image', $
               ' ------------------------------' $
               + '--------- Window ' + strn(mw)
        FOR i=0,n_elements(strarray)-1 DO print, strarray[i]
    ENDIF ELSE BEGIN
        title = 'Statistics in Whole Image (Window '+strn(mw)+')'
        xdispstr, strarray, title=title, $
            group=(*info).maintlb, $
            font=*self.fixedfont,width=80,height=21
    ENDELSE
ENDIF
RETURN
END


PRO EXAMDISPLAY::PROFILE2, Event
;
;  One of the routines to generate an profile whereby the user clicks
;  the endpoints.  This one gets the last point and puts up the plot.
;
IF event.type NE 0 THEN BEGIN
    self->xy_sensitive, 'examdisplay::profile2'
    RETURN
ENDIF

info = self.popup_info

;; text = widget_info((*info).results, /child)
text = (*info).instruc
widget_control, text, /destroy

;  Code adapted from RSI's profile.pro

sz = size(*self.image)
sx = sz[1] & sy = sz[2]
dx = float(self.xpos-(*info).x0)                ;delta x
dy = float(self.ypos-(*info).y0)
n = abs(dx) > abs(dy)
IF n EQ 0 THEN BEGIN
    junk = dialog_message('Zero length line.')
    RETURN
ENDIF
;
r = fltarr(n+1)
;
IF abs(dx) GT abs(dy) THEN BEGIN
    IF self.xpos GE (*info).x0 THEN s=1 ELSE s=-1
    sy = (self.ypos-(*info).y0)/abs(dx)
ENDIF ELSE BEGIN
    IF self.ypos GE (*info).y0 THEN sy=1 ELSE sy=-1
    s = (self.xpos-(*info).x0)/abs(dy)
ENDELSE
;
xx = long(findgen(n+1l)*s+(*info).x0)    ;X values, make into longwords.
yy = long(findgen(n+1l)*sy+(*info).y0)   ;Y values
pp = (*self.image)[long(yy)*sx + xx]
pd = sqrt((xx-xx[0])^2 + (yy-yy[0])^2)

n_e = n_elements(xx)
xtitle='Relative Pixel Distance'
ytitle='Pixel Value'
title='['+strn(xx[0])+','+strn(yy[0])+'] - ' $
          +'['+strn(xx[n_e-1])+','+strn(yy[n_e-1])+']'

widget_control, self.maindisp, get_value=mw
wtitle = 'Profile (Window '+strn(mw)+')'
adjplot, pd, pp, top_id=top_id, group_leader=(*info).maintlb, $
    wtitle=wtitle, title=title, ytitle=ytitle, xtitle=xtitle, $
    charsize=1

wset, (*info).maingraph
device, copy=[0, 0, $
    self.xzend-self.xzstart+1, $
    self.yzend-self.yzstart+1, $
    self.xzstart, self.yzstart, $
    self.main_shad]

plots, self.xpos, self.ypos, psym=1, symsize=1, /dev, $
    color=self.boxcolor
plots, [(*info).x0,self.xpos],[(*info).y0,self.ypos], /dev, $
    color=self.boxcolor

wset, self.main_shad
device, copy=[self.xzstart,self.yzstart,self.xzend-self.xzstart+1, $
              self.yzend-self.yzstart+1,0,0,(*info).maingraph]

;; widget_control, (*info).tlb, /destroy

RETURN
END




PRO EXAMDISPLAY::PROFILE1, Event
;
;  One of the routines to generate an profile whereby the user clicks
;  the endpoints.  This one gets the first point and sets up to get the
;  second point.
;

IF event.type NE 0 THEN RETURN

info = self.popup_info

(*info).x0 = self.xpos
(*info).y0 = self.ypos

widget_control, self.maindisp, get_value=maingraph
wset, maingraph

device, copy=[0, 0, $
    self.xzend-self.xzstart+1, $
    self.yzend-self.yzstart+1, $
    self.xzstart, self.yzstart, $
    self.main_shad]

(*info).maingraph = maingraph

plots, (*info).x0, (*info).y0, psym=1, symsize=1, /dev, $
    color=self.boxcolor

wset, self.main_shad
device, copy=[self.xzstart,self.yzstart,self.xzend-self.xzstart+1, $
              self.yzend-self.yzstart+1,0,0,(*info).maingraph]

self->xy_sensitive, 'examdisplay::profile2'

RETURN
END


PRO EXAMDISPLAY::PROFILE, Info
;
;  One of the routines to plot an interactive profile whereby the
;  user clicks on the endpoints.  This one sets up to get the first
;  point.
;

;; self->destroybuttons, info

self->xy_sensitive, 'examdisplay::profile1'

instruc = [ $
   'Click on each end of the profile in turn', $
   'in either the zoom or the main window.']

IF widget_info((*info).instruc,/valid) THEN $
    widget_control, (*info).instruc, /destroy
base = widget_base(group=(*info).maintlb,col=1)
junk = widget_text(base, value=instruc, xsize=42, ysize=2, $
                   font=*self.fixedfont)
(*info).instruc = base
widget_control, base, /realize

;; text = widget_text((*info).results, value=instruc, xsize=42, ysize=2)

RETURN
END


PRO EXAMDISPLAY::ROWCOL_TOTAL, Rc, X0, Y0, Wid, Xx, Tot, TITLE=title
;
;  Totals up a strip of rows or columns.
;
;  Input Positional Parameters:
;    Rc:        0 for rows or 1 for columns.
;    X0, Y0:    Position of cursor.
;    Wid:       Width of strip in pixels.
;  Output Positional Parameters:
;    Xx:        Abscissa vector giving pixel number.
;    Tot:       Total of wid rows or columns.
;  Output Keyword Parameter:
;    TITLE:     Plot title.
;

sz = size(*self.image)
xdim = sz[1] & ydim = sz[2]

IF rc EQ 0 THEN BEGIN
    xx = indgen(xdim)
    y1 = ((y0 - long(wid/2.0))>0)<(ydim-1)
    y2 = ((y1 + long(wid) - 1)>y1)<(ydim-1)
    IF y1 NE y2 THEN tot = total((*self.image)[*, y1:y2], 2) $
                ELSE tot = (*self.image)[*, y1:y2]
    title = 'Rows ' + strn(y1) + ':' + strn(y2) 
ENDIF ELSE BEGIN
    xx = indgen(ydim)
    x1 = ((x0 - long(wid/2.0))>0)<(xdim-1)
    x2 = ((x1 + long(wid) - 1)>x1)<(xdim-1)
    IF x1 NE x2 THEN tot = total((*self.image)[x1:x2, *], 1) $
                ELSE tot = transpose((*self.image)[x1:x2, *])
    title = 'Cols ' + strn(x1) + ':' + strn(x2)
ENDELSE

RETURN
END

PRO EXAMDISPLAY_ROWCOL_CLEANUP, Tlb
;
;  Deallocates info structure from row/column plot subwidget.
;
widget_control, tlb, get_uvalue=info
IF ptr_valid(info) THEN ptr_free, info
RETURN
END


PRO EXAMDISPLAY::ROWCOL_EVENT, Event, Pinfo
;
;  Object event handler for row/column plot widget.  Called by
;  widget event handler.
;

widget_control, (*pinfo).width_text, get_value=wstring
wid = long(wstring[0])

widget_control, (*pinfo).rowcol_bgroup, get_value=rc
IF rc THEN BEGIN
    ;
    ;  Column sum.
    right = (*pinfo).xdim-(*pinfo).x0
    left  = (*pinfo).x0
    ;
    ;  Compute maximum allowed width of strip.
    IF left LE right THEN BEGIN
        maxw = left * 2
    ENDIF ELSE BEGIN
        maxw = (right-1)*2 + 1
    ENDELSE
ENDIF ELSE BEGIN
    ;
    ;  Row sum.
    top = (*pinfo).ydim-(*pinfo).y0
    bot = (*pinfo).y0
    ;
    ;  Compute maximum allowed width of strip.
    IF bot LE top THEN BEGIN
        maxw = bot * 2
    ENDIF ELSE BEGIN
        maxw = (top-1)*2 + 1
    ENDELSE
ENDELSE

yrel = 0b
CASE event.id OF
    (*pinfo).width_text:  BEGIN
        ;
        ;  User has enter a width using text.
        text_number, (*pinfo).width_text, wid, /long, $
            minv=1, maxv=maxw
        yrel = 1b
    END
    (*pinfo).incr_button:  BEGIN
        ;
        ;  User has incremented width with button.
        wid = (wid + 1)<maxw
        widget_control, (*pinfo).width_text, set_value=strn(wid)
        yrel = 1b
    END
    (*pinfo).decr_button:  BEGIN
        ;
        ;  User has decremented width with button.
        wid = (wid - 1)>1
        widget_control, (*pinfo).width_text, set_value=strn(wid)
        yrel = 1b
    END
    ELSE:  wid = (wid>1)<maxw
ENDCASE

;
;  Total it up.
self->rowcol_total, rc, (*pinfo).x0, (*pinfo).y0, wid, xx, tot, $
    title=title

;
;  Replot.
adjplot_replot, (*pinfo).plot_id, xx, tot, title=title, $
    /xstatic, yrel=yrel, charsize=1

RETURN
END


PRO EXAMDISPLAY_ROWCOL_EVENT, Event
;
;  Widget event handler for row/column plot widget.  Calls
;  object event handler.
;
widget_control, event.top, get_uvalue=pinfo
(*pinfo).this->rowcol_event, event, pinfo
RETURN
END


PRO EXAMDISPLAY::ROWCOL, Info
;
;  Generates row/column plot subwidget.
;
(*info).x0 = self.xcursor
(*info).y0 = self.ycursor
sz = size(*self.image)
xdim = sz[1] & ydim = sz[2]

widget_control, self.maindisp, get_value=mw
wtitle = 'Row/Column Plot (Window '+strn(mw)+')'
tlb = widget_base(/col, title=wtitle, group=(*info).maintlb)
row1 = widget_base(tlb, /row)

rowcol_bgroup = cw_bgroup(row1, ['ROW','COLUMN'], set_value=0, $
                         /no_rel, /exclusive, /row)

width_label = widget_label(row1, value='Width:')
width_text = widget_text(row1, xsize=6, value='1', /editable)
incr_button = widget_button(row1, value='+1', /no_rel)
decr_button = widget_button(row1, value='-1', /no_rel)

self->rowcol_total, 0, (*info).x0, (*info).y0, 1, xx, tot, $
    title=title

adjplot, xx, tot, top_id=plot_id, base_id=tlb, title=title, $
    charsize=1

popup_info = ptr_new({tlb:tlb, plot_id:plot_id, $
                this:(*info).this, x0:(*info).x0, $
                y0:(*info).y0, $
                xdim:xdim, ydim:ydim, $
                rowcol_bgroup:rowcol_bgroup, $
                width_text:width_text, incr_button:incr_button, $
                decr_button:decr_button})

widget_control, tlb, set_uvalue=popup_info

xmanager, 'examdisplay_rowcol', tlb, $
    event_handler='examdisplay_rowcol_event', $
    cleanup='examdisplay_rowcol_cleanup', /no_block

;; widget_control, (*info).tlb, /destroy

RETURN
END


PRO EXAMDISPLAY_ROIPHOT_CLEANUP, Tlb
;
;  Deallocates info structure for r.o.i. photometry subwidget.
;
widget_control, tlb, get_uvalue=info
IF ptr_valid((*info).sky_roi) THEN ptr_free, (*info).sky_roi
IF ptr_valid((*info).obj_roi) THEN ptr_free, (*info).obj_roi
IF ptr_valid(info) THEN ptr_free, info
RETURN
END

PRO EXAMDISPLAY::ROIPHOT_EVENT, Event, Pinfo
;
;  Event handler for region-of-interest photometry.
;
destroy=0b
change=0b
CASE event.id OF
    (*pinfo).meas_sky_button:  BEGIN
        widget_control, (*pinfo).msg_text, $
            set_value='Define sky region'
        IF ptr_valid((*pinfo).sky_roi) THEN $
            ptr_free,(*pinfo).sky_roi
        self->id, drawid
        (*pinfo).sky_roi $
            = ptr_new(cw_defroi2(drawid,$
                image_size=[(*pinfo).xdim, (*pinfo).ydim], $
                group_leader=event.top,/floating))
        widget_control, (*pinfo).msg_text, $
            set_value='Just measured a sky region'
        change=1b
    END
    (*pinfo).meas_obj_button:  BEGIN
        widget_control, (*pinfo).msg_text, $
            set_value='Define object'
        IF ptr_valid((*pinfo).obj_roi) THEN $
            ptr_free,(*pinfo).obj_roi
        self->id, drawid
        (*pinfo).obj_roi $
            = ptr_new(cw_defroi2(drawid,$
                image_size=[(*pinfo).xdim, (*pinfo).ydim], $
                group_leader=event.top,/floating))
        widget_control, (*pinfo).msg_text, $
            set_value='Just measured an object'
        change=1b
    END
    (*pinfo).save_button:  BEGIN
        widget_control, (*pinfo).object_text, get_value=tmp
        objtot = float(tmp[0])
        widget_control, (*pinfo).sky_text, get_value=tmp
        skyavg = float(tmp[0])
        widget_control, (*pinfo).objsky_text, get_value=tmp
        objsky = float(tmp[0])
        widget_control, (*pinfo).net_text, get_value=tmp
        objnet = float(tmp[0])
        widget_control, (*pinfo).avnet_text, get_value=tmp
        avobjnet = float(tmp[0])
        widget_control, (*pinfo).save_text, get_value=savetag
        self.log_count = self.log_count + 1
        log_count = self.log_count
        sky_roi = *(*pinfo).sky_roi
        obj_roi = *(*pinfo).obj_roi
        save_filename = 'save_' + savetag[0] + '_' + strn(log_count) $
            + '.idl'
        save, log_count, objtot, skyavg, $
              objsky, objnet, avobjnet, $
              sky_roi, obj_roi, $
              file=save_filename
        msg = 'SAVED IN ' + save_filename
        widget_control, (*pinfo).msg_text, set_value=msg
        widget_control, self.maindisp, get_value=maingraph
        print, ' '
        print, '----------------------', $
               '-------------------------------' $
               + '--------- Window ' + strn(maingraph)
        print, 'R.O.I. Photometry'
        print, msg
        sky_roi = 0
        obj_roi = 0
    END
    (*pinfo).quit_button:  BEGIN
        destroy=1b
    END
    ELSE:
ENDCASE

valid_obj = 0b
valid_sky = 0b
IF ptr_valid((*pinfo).obj_roi) THEN BEGIN
    IF (*(*pinfo).obj_roi)[0] NE -1 THEN valid_obj = 1b
ENDIF
IF ptr_valid((*pinfo).sky_roi) THEN BEGIN
    IF (*(*pinfo).sky_roi)[0] NE -1 THEN valid_sky = 1b
ENDIF

IF change THEN BEGIN
    IF valid_obj THEN BEGIN
        objtot = total((*self.image)[*(*pinfo).obj_roi])
        nobjpix = n_elements(*(*pinfo).obj_roi)
    ENDIF ELSE BEGIN
        objtot = 0
        nobjpix = 0L
    ENDELSE
    IF valid_sky THEN BEGIN
        nskypix = n_elements(*(*pinfo).sky_roi) 
        skyavg = total((*self.image)[*(*pinfo).sky_roi]) $
            / nskypix
        objsky = skyavg*nobjpix
    ENDIF ELSE BEGIN
        skyavg = 0
        objsky = 0
        nskypix = 0
    ENDELSE
    objnet = objtot - objsky
    IF nobjpix GT 0 THEN avobjnet = objnet/nobjpix ELSE avobjnet = 0
    
    IF valid_obj AND (NOT destroy) THEN BEGIN
        multi_sub, [(*pinfo).xdim, (*pinfo).ydim], *(*pinfo).obj_roi, xyroi
        avx = mean(xyroi[*,0])
        avy = mean(xyroi[*,1])
        IF self.logging THEN BEGIN
            widget_control, self.maindisp, get_value=maingraph
            print, ' '
            print, '----------------------', $
                   '-------------------------------' $
                   + '--------- Window ' + strn(maingraph)
            print, 'R.O.I. Photometry'
            print, 'Mean X of object      = ',avx
            print, 'Mean Y of object      = ',avy
            print, 'Number of obj pixels  = ',nobjpix
            print, 'Number of sky pixels  = ',nskypix
            print, 'Object total          = ',objtot
            print, 'Sky average           = ',skyavg
            print, 'Sky in object         = ',objsky
            print, 'Net in object         = ',objnet
            print, 'Average net in object = ',avobjnet
        ENDIF
    ENDIF
    widget_control, (*pinfo).object_text, set_value=strn(objtot)
    widget_control, (*pinfo).sky_text, set_value=strn(skyavg)
    widget_control, (*pinfo).objsky_text, set_value=strn(objsky)
    widget_control, (*pinfo).net_text, set_value=strn(objnet)
    widget_control, (*pinfo).avnet_text, set_value=strn(avobjnet)
ENDIF

IF destroy THEN widget_control, event.top, /destroy

RETURN
END

PRO EXAMDISPLAY_ROIPHOT_EVENT, Event
widget_control, event.top, get_uvalue=pinfo
(*pinfo).this->roiphot_event, event, pinfo
RETURN
END


PRO EXAMDISPLAY::ROIPHOT, Info
;
;  Popup for region-of-interest photometry.
;

(*info).x0 = self.xcursor
(*info).y0 = self.ycursor
sz = size(*self.image)
xdim = sz[1] & ydim = sz[2]

widget_control, self.maindisp, get_value=mw
wtitle = 'ROI Photometry (Window '+strn(mw)+')'
tlb = widget_base(/col, title=wtitle, group=(*info).maintlb, /floating)
row2 = widget_base(tlb, /row)
row3 = widget_base(tlb, /row)
row4 = widget_base(tlb, /row)
row5 = widget_base(tlb, /row)
row6 = widget_base(tlb, /row)
row7 = widget_base(tlb, /row)
row8 = widget_base(tlb, /row)
row9 = widget_base(tlb, /row)

sky_label = widget_label(row2, value='Sky average:')
sky_text = widget_text(row2, xsize=20, value='0')
object_label = widget_label(row3, value='Object total:')
object_text = widget_text(row3, xsize=20, value='0')
objsky_label = widget_label(row4, value='Sky in object:')
objsky_text = widget_text(row4, xsize=20, value='0')
net_label = widget_label(row5, value='Net in object:')
net_text = widget_text(row5, xsize=20, value='0')
avnet_label = widget_label(row6, value='Mean net in object:')
avnet_text = widget_text(row6, xsize=20, value='0')
meas_sky_button = widget_button(row7, value='Measure sky', /no_rel)
meas_obj_button = widget_button(row7, value='Measure object', /no_rel)
msg_text = widget_text(row8, xsize=30,ysize=1)
save_lab = widget_label(row9, value='Save Tag:')
svtag = 'roiph' + strn(mw[0])
save_text = widget_text(row9, xsize=10,ysize=1, value=svtag, /edit)
save_button = widget_button(row9, value='Save Data', /no_rel)
quit_button = widget_button(row9, value='DISMISS', /no_rel)

widget_control, tlb, /realize

popup_info = ptr_new({tlb:tlb, $
                this:(*info).this, $
                xdim:xdim, ydim:ydim, $
                sky_text:sky_text, $
                object_text:object_text, $
                objsky_text:objsky_text, $
                net_text:net_text, $
                avnet_text:avnet_text, $
                meas_sky_button:meas_sky_button, $
                meas_obj_button:meas_obj_button, $
                save_button:save_button, $
                save_text:save_text, $
                quit_button:quit_button, $
                msg_text:msg_text, $
                sky_roi:ptr_new(), $
                obj_roi:ptr_new() })

widget_control, tlb, set_uvalue=popup_info

xmanager, 'examdisplay_roiphot', tlb, $
    event_handler='examdisplay_roiphot_event', $
    cleanup='examdisplay_roiphot_cleanup', /no_block

;; widget_control, (*info).tlb, /destroy

RETURN
END

PRO EXAMDISPLAY_APERGROWTH_CLEANUP, Tlb
;
;  Deallocates info structure for r.o.i. photometry subwidget.
;
widget_control, tlb, get_uvalue=info
IF ptr_valid(info) THEN ptr_free, info
RETURN
END

PRO EXAMDISPLAY::APERGROWTH_EVENT, Event, Pinfo
;
;  Event handler for aperture photometry.
;
destroy=0b

IF event.id EQ (*pinfo).curs_button THEN BEGIN
    widget_control, (*pinfo).x_text, $
        set_value=strn(self.xcursor,format='(F10.2)')
    widget_control, (*pinfo).y_text, $
        set_value=strn(self.ycursor,format='(F10.2)')
ENDIF
IF event.id EQ (*pinfo).cntrd_button $
    OR event.id EQ (*pinfo).cntrd_text THEN BEGIN
    text_number, (*pinfo).cntrd_text, cbox, format='(F10.2)'
    text_number, (*pinfo).x_text, xcoo, format='(F10.2)'
    text_number, (*pinfo).y_text, ycoo, format='(F10.2)'
    cntrd, *self.image, xcoo, ycoo, xcen, ycen, (cbox-1)/(0.637*2)
    IF xcen GT 0 AND ycen GE 0 THEN BEGIN
        xcoo=xcen
        ycoo=ycen
    ENDIF
    widget_control, (*pinfo).x_text, set_value=strn(xcoo,format='(F10.2)')
    widget_control, (*pinfo).y_text, set_value=strn(ycoo,format='(F10.2)')
ENDIF

IF  event.id EQ (*pinfo).cntrd_button $
    OR event.id EQ (*pinfo).cntrd_text $
    OR event.id EQ (*pinfo).x_text $
    OR event.id EQ (*pinfo).y_text $
    OR event.id EQ (*pinfo).minrad_text $
    OR event.id EQ (*pinfo).curs_button $
    OR event.id EQ (*pinfo).badmax $
    OR event.id EQ (*pinfo).badmin $
    OR event.id EQ (*pinfo).gain $
    OR event.id EQ (*pinfo).fskyval $
    OR event.id EQ (*pinfo).iskyann $
    OR event.id EQ (*pinfo).oskyann THEN BEGIN

    text_number, (*pinfo).iskyann, sky1, format='(F10.2)'
    text_number, (*pinfo).oskyann, sky2, format='(F10.2)'
    text_number, (*pinfo).minrad_text, minrad, format='(F10.2)'
    text_number, (*pinfo).gain, gain, format='(F10.2)'
    text_number, (*pinfo).badmin, badmin, format='(G15.7)'
    text_number, (*pinfo).badmax, badmax, format='(G15.7)'
    text_number, (*pinfo).x_text, xcoo, format='(F10.2)'
    text_number, (*pinfo).y_text, ycoo, format='(F10.2)'

    force_sky = 0
    widget_control, (*pinfo).fskyval, get_value=fskyval
    fskytrim = strtrim(fskyval[0],2)
    If fskytrim NE '' THEN BEGIN
        text_number, (*pinfo).fskyval, fsky, format='(E20.7)'
        force_sky = 1
    ENDIF
    widget_control, (*pinfo).fskyval, get_value=fskyval
    fskytrim = strtrim(fskyval[0],2)
    IF fskytrim EQ '' THEN fskytrim = 'none'

    sz = size(*self.image)
    xdim = sz[1] & ydim = sz[2]

    half = round(1.1*max([sky1,sky2,minrad]))

    x0 = (round(xcoo)-half)>0
    x1 = (round(xcoo)+half-1)<(xdim-1)
    y0 = (round(ycoo)-half)>0
    y1 = (round(ycoo)+half-1)<(ydim-1)
    xdim2 = x1-x0+1
    ydim2 = y1-y0+1
    dsz = float((*pinfo).dispsize)
    fac = min(dsz/[xdim2,ydim2])
    xredim = round(xdim2*fac) & yredim = round(ydim2*fac)
    (*pinfo).objdisp->resize, xytotal=[xredim, yredim]
    (*pinfo).objdisp->draw, frebin((*self.image)[x0:x1,y0:y1],xredim,yredim)
    svwin = !D.window
    wset,(*pinfo).gwin 
    xcen = (xcoo-x0+0.5)*fac - 0.5
    ycen = (ycoo-y0+0.5)*fac - 0.5
    tvcircle, fac*sky1, xcen, ycen, color=self.boxcolor, thick=2
    tvcircle, fac*sky2, xcen, ycen, color=self.boxcolor, thick=2
    IF svwin GE 0 THEN wset, svwin
    skyvec = [sky1<sky2,sky1>sky2]
    IF skyvec[1] LE skyvec[0] THEN skyvec[1] = skyvec[0] + 1
    widget_control, (*pinfo).iskyann, set_value=strn(skyvec[0],form='(F10.2)')
    widget_control, (*pinfo).oskyann, set_value=strn(skyvec[1],form='(F10.2)')

    badvec = [badmin<badmax,badmin>badmax]
    grorad = indgen(minrad>(skyvec[1]+1))+1
    skyvecx = skyvec
    aper, *self.image, xcoo, ycoo, mags, $
        errap, sky, skyerrx, gain, grorad, skyvecx, badvec, $
        /exact, /flux, /silent, setskyval=0

    wsky = where(grorad GE skyvec[0] AND grorad LE skyvec[1], nsky)
    npix = !DPI*grorad^2
    coeff = linfit(npix[wsky], mags[wsky])
    IF fskytrim EQ 'none' THEN BEGIN
        skyval = coeff[1]
    ENDIF ELSE BEGIN
        skyval = float(fskytrim)
    ENDELSE
    magscorr = mags - npix*skyval
    wset, (*pinfo).plot1
    nm = n_elements(mags)
    sb = (mags[1:nm-1]-mags[0:nm-2])/(npix[1:nm-1]-npix[0:nm-2])
    sb = [mags[0]/npix[0], sb]
    plot, grorad, sb>(max(sb)/1e5), xtitle='Outer Radius', $
        ytitle='Surface Brightness', $
        title=' ', /ynoz
    oplot, [skyvec[0],skyvec[0]], !y.crange, line=2
    oplot, [skyvec[1],skyvec[1]], !y.crange, line=2
    wset, (*pinfo).plot2
    plot, grorad, magscorr, xtitle='Radius', ytitle='Net', title=' '
    oplot, [skyvec[0],skyvec[0]], !y.crange, line=2
    oplot, [skyvec[1],skyvec[1]], !y.crange, line=2
    nn = max(where(grorad LE skyvec[0]))
    IF nn GE 0 THEN BEGIN
        objnet = magscorr[nn]
        skyvecx = skyvec
        aper, *self.image, xcoo, ycoo, mags2, $
            errap2, sky2, skyerr2, gain, grorad[nn], skyvecx, badvec, $
            /exact, /flux, /silent
        skyerr = skyerr2[0]
        objerr = errap2[0]
    ENDIF ELSE BEGIN
        objnet = -1
        objerr = -1
        skyerr = -1
    ENDELSE

    widget_control, (*pinfo).object_text, set_value=strn(objnet)
    widget_control, (*pinfo).sky_text, set_value=strn(skyval)
    widget_control, (*pinfo).objerr_text, set_value=strn(objerr)
    widget_control, (*pinfo).skyerr_text, set_value=strn(skyerr)

    log_count = self.log_count
    IF self.logging THEN BEGIN
        widget_control, self.maindisp, get_value=maingraph
        print, ' '
        print, '----------------------', $
               '-------------------------------' $
               + '--------- Window ' + strn(maingraph)
        print, 'APERTURE GROWTH PHOTOMETRY'
        print, 'X coordinate           = ',xcoo
        print, 'Y coordinate           = ',ycoo
        print, 'Inner sky radius       = ',skyvec[0]
        print, 'Outer sky radius       = ',skyvec[1]
        print, 'Bad pixel limit (low)  = ',badvec[0]
        print, 'Bad pixel limit (high) = ',badvec[1]
        print, 'Gain (PHPADU)          = ',gain
        print, 'Object net             = ',objnet
        print, 'Error in object net    = ',objerr
        print, 'Sky average            = ',skyval
        print, 'Error in sky average   = ',skyerr
    ENDIF
ENDIF
IF event.id EQ (*pinfo).quit_button THEN BEGIN
    destroy=1b
ENDIF

IF destroy THEN widget_control, event.top, /destroy

RETURN
END

PRO EXAMDISPLAY_APERGROWTH_EVENT, Event
widget_control, event.top, get_uvalue=pinfo
(*pinfo).this->apergrowth_event, event, pinfo
RETURN
END

PRO EXAMDISPLAY::APERGROWTH, Info
;
;  Popup for aperture photometry.
;

dispsize = 200

widget_control, self.maindisp, get_value=mw
wtitle = 'Aperture Growth Photometry (Window '+strn(mw)+')'
tlb = widget_base(/row, title=wtitle, group=(*info).maintlb, floating=0)
col1 = widget_base(tlb, /col)
col2 = widget_base(tlb, /col)
rowy = widget_base(col1, /row)
rowz = widget_base(col1, /row)
rowa = widget_base(col1, /row)
rowb = widget_base(col1, /row)
rowb1 = widget_base(col1, /row)
rowc = widget_base(col1, /row)
rowc1 = widget_base(col1, /row)
rowc2 = widget_base(col1, /row)
rowd = widget_base(col1, /row)

x_label = widget_label(rowy, value='X:')
x_text = widget_text(rowy, xsize=8, value=strn(self.xcursor), /edit)
y_label = widget_label(rowy, value='Y:')
y_text = widget_text(rowy, xsize=8, value=strn(self.ycursor), /edit)
curs_button = widget_button(rowy, value='READ CURSOR', /no_rel)
cntrd_button = widget_button(rowz, value='ADJUST CENTROID', /no_rel)
cntrd_label = widget_label(rowz, value='using box size = ')
cntrd_text = widget_text(rowz, xsize=8, value='7', /edit)

oskyann_label = widget_label(rowb, value='Sky Radii [px]:')
iskyann = widget_text(rowb, xsize=8, value='15', /edit)
oskyann = widget_text(rowb, xsize=8, value='20', /edit)
minrad_label = widget_label(rowb1, value='Outermost Radius (minimum) [px]:')
minrad_text = widget_text(rowb1, xsize=8, value='25', /edit)
fsky_label = widget_label(rowc, value='Force Sky Value: ')
fskyval = widget_text(rowc, xsize=15, value=' ',/edit)
gain_label = widget_label(rowc1, value='Gain [e- per ADU]:')
IF ptr_valid(self.header) THEN BEGIN
    par = strmid(*self.header, 0, 8)
    gainpar = where(strpos(par, 'GAIN') GE 0 $
                  OR strpos(par, 'PHPADU') GE 0, npar)
    gval = 1.0
    kk=npar
    WHILE kk GT 0 DO BEGIN
        kk = kk - 1
        useg = par[gainpar[kk]]
        gval2 = sxpar(*self.header, useg)
        IF gval2 GT 0 THEN BEGIN
            kk=-1
            gval = gval2
        ENDIF
    ENDWHILE
ENDIF ELSE gval = 1.0
gain = widget_text(rowc1, xsize=12, value=strn(gval), /edit)
badmin_label = widget_label(rowc2, value='Bad pixel limits:')
badmin = widget_text(rowc2, value='-32765', xsize=12, /edit)
badmax = widget_text(rowc2, value='32767', xsize=12, /edit)
objdisp = obj_new('IMGDISPLAY', rowd, xsize=dispsize, ysize=dispsize, $
                  /ps_dialog)
objdisp->psfile, '_examdisplay_aper.ps', 'APERTURE GROWTH PHOTOMETRY'

row3 = widget_base(col1, /row)
row4 = widget_base(col1, /row)
row5 = widget_base(col1, /row)
col6 = widget_base(col1, /col)

object_label = widget_label(row3, value='Object net:')
object_text = widget_text(row3, xsize=10, value='0', /edit)
objerr_label = widget_label(row3, value='Error:')
objerr_text = widget_text(row3, xsize=10, value='0')
sky_label = widget_label(row4, value='Sky:')
sky_text = widget_text(row4, xsize=10, value='0')
skyerr_label = widget_label(row4, value='Error:')
skyerr_text = widget_text(row4, xsize=10, value='0')
quit_button = widget_button(row5, value='DISMISS', /no_rel)

plot1_draw = widget_draw(col2, xsize=300, ysize=300)
plot2_draw = widget_draw(col2, xsize=300, ysize=300)

widget_control, tlb, /realize

widget_control, plot1_draw, get_value=plot1
widget_control, plot2_draw, get_value=plot2

objdisp->id, mainid
widget_control, mainid, get_value=gwin

popup_info = ptr_new({tlb:tlb, $
                this:(*info).this, $
                x_text:x_text, $
                y_text:y_text, $
                curs_button:curs_button, $
                cntrd_button:cntrd_button, $
                cntrd_text:cntrd_text, $
                oskyann:oskyann, $
                iskyann:iskyann, $
                minrad_text:minrad_text, $
                fskyval:fskyval, $
                gain:gain, $
                badmin:badmin, $
                badmax:badmax, $
                objdisp:objdisp, $
                dispsize:dispsize, $
                gwin:gwin, $
                object_text:object_text, $
                objerr_text:objerr_text, $
                sky_text:sky_text, $
                skyerr_text:skyerr_text, $
                plot1_draw:plot1_draw, $
                plot2_draw:plot2_draw, $
                plot1:plot1, $
                plot2:plot2, $
                quit_button:quit_button })

widget_control, tlb, set_uvalue=popup_info

xmanager, 'examdisplay_apergrowth', tlb, $
    event_handler='examdisplay_apergrowth_event', $
    cleanup='examdisplay_apergrowth_cleanup', /no_block

pseudo = {id:oskyann, top:tlb, handler:tlb}
widget_control, oskyann, send_event=pseudo

RETURN
END

PRO EXAMDISPLAY_APERPHOT_CLEANUP, Tlb
;
;  Deallocates info structure for r.o.i. photometry subwidget.
;
widget_control, tlb, get_uvalue=info
IF ptr_valid(info) THEN ptr_free, info
RETURN
END

PRO EXAMDISPLAY::APERPHOT_EVENT, Event, Pinfo
;
;  Event handler for aperture photometry.
;
destroy=0b

IF event.id EQ (*pinfo).curs_button THEN BEGIN
    widget_control, (*pinfo).x_text, $
        set_value=strn(self.xcursor,format='(F10.2)')
    widget_control, (*pinfo).y_text, $
        set_value=strn(self.ycursor,format='(F10.2)')
ENDIF
IF event.id EQ (*pinfo).cntrd_button $
    OR event.id EQ (*pinfo).cntrd_text THEN BEGIN
    text_number, (*pinfo).cntrd_text, cbox, format='(F10.2)'
    text_number, (*pinfo).x_text, xcoo, format='(F10.2)'
    text_number, (*pinfo).y_text, ycoo, format='(F10.2)'
    cntrd, *self.image, xcoo, ycoo, xcen, ycen, (cbox-1)/(0.637*2)
    IF xcen GT 0 AND ycen GE 0 THEN BEGIN
        xcoo=xcen
        ycoo=ycen
    ENDIF
    widget_control, (*pinfo).x_text, set_value=strn(xcoo,format='(F10.2)')
    widget_control, (*pinfo).y_text, set_value=strn(ycoo,format='(F10.2)')
ENDIF
IF  event.id EQ (*pinfo).cntrd_button $
    OR event.id EQ (*pinfo).cntrd_text $
    OR event.id EQ (*pinfo).x_text $
    OR event.id EQ (*pinfo).y_text $
    OR event.id EQ (*pinfo).curs_button $
    OR event.id EQ (*pinfo).objrad $
    OR event.id EQ (*pinfo).badmax $
    OR event.id EQ (*pinfo).badmin $
    OR event.id EQ (*pinfo).gain $
    OR event.id EQ (*pinfo).fskyval $
    OR event.id EQ (*pinfo).iskyann $
    OR event.id EQ (*pinfo).oskyann THEN BEGIN

    text_number, (*pinfo).iskyann, sky1, format='(F10.2)'
    text_number, (*pinfo).oskyann, sky2, format='(F10.2)'
    text_number, (*pinfo).objrad, objr, format='(F10.2)'
    text_number, (*pinfo).gain, gain, format='(F10.2)'
    text_number, (*pinfo).badmin, badmin, format='(G15.7)'
    text_number, (*pinfo).badmax, badmax, format='(G15.7)'
    text_number, (*pinfo).x_text, xcoo, format='(F10.2)'
    text_number, (*pinfo).y_text, ycoo, format='(F10.2)'

    force_sky = 0
    widget_control, (*pinfo).fskyval, get_value=fskyval
    fskytrim = strtrim(fskyval[0],2)
    If fskytrim NE '' THEN BEGIN
        text_number, (*pinfo).fskyval, fsky, format='(E20.7)'
        force_sky = 1
    ENDIF
    widget_control, (*pinfo).fskyval, get_value=fskyval
    fskytrim = strtrim(fskyval[0],2)
    IF fskytrim EQ '' THEN fskytrim = 'none'

    sz = size(*self.image)
    xdim = sz[1] & ydim = sz[2]

    half = round(1.1*max([sky1,sky2,objr]))

    x0 = (round(xcoo)-half)>0
    x1 = (round(xcoo)+half-1)<(xdim-1)
    y0 = (round(ycoo)-half)>0
    y1 = (round(ycoo)+half-1)<(ydim-1)
    xdim2 = x1-x0+1
    ydim2 = y1-y0+1
    fac = floor(min(float((*pinfo).dispsize/[xdim2,ydim2])))
    xredim = xdim2*fac
    yredim = ydim2*fac
    (*pinfo).objdisp->resize, xytotal=[xredim, yredim]
    (*pinfo).objdisp->draw, frebin((*self.image)[x0:x1,y0:y1],xredim,yredim)
    svwin = !D.window
    wset,(*pinfo).gwin 
    xcen = (xcoo-x0+0.5)*fac - 0.5
    ycen = (ycoo-y0+0.5)*fac - 0.5
    tvcircle, fac*objr, xcen, ycen, color=self.boxcolor, thick=2
    tvcircle, fac*sky1, xcen, ycen, color=self.boxcolor, thick=2
    tvcircle, fac*sky2, xcen, ycen, color=self.boxcolor, thick=2
    IF svwin GE 0 THEN wset, svwin
    skyvec = [sky1<sky2,sky1>sky2]
    badvec = [badmin<badmax,badmin>badmax]
    IF force_sky THEN BEGIN
        aper, *self.image, xcoo, ycoo, mags, $
            errap, sky, skyerr, gain, objr, skyvec, badvec, $
            /exact, /flux, /silent, setskyval=fsky
    ENDIF ELSE BEGIN
        aper, *self.image, xcoo, ycoo, mags, $
            errap, sky, skyerr, gain, objr, skyvec, badvec, $
            /exact, /flux, /silent
    ENDELSE

    widget_control, (*pinfo).object_text, set_value=strn(mags[0])
    widget_control, (*pinfo).sky_text, set_value=strn(sky[0])
    widget_control, (*pinfo).objerr_text, set_value=strn(errap[0])
    widget_control, (*pinfo).skyerr_text, set_value=strn(skyerr[0])

    log_count = self.log_count
    IF self.logging THEN BEGIN
        widget_control, self.maindisp, get_value=maingraph
        print, ' '
        print, '----------------------', $
               '-------------------------------' $
               + '--------- Window ' + strn(maingraph)
        print, 'APERTURE PHOTOMETRY'
        print, 'X coordinate           = ',xcoo
        print, 'Y coordinate           = ',ycoo
        print, 'Object radius          = ',objr
        print, 'Inner sky radius       = ',skyvec[0]
        print, 'Outer sky radius       = ',skyvec[1]
        print, 'Bad pixel limit (low)  = ',badvec[0]
        print, 'Bad pixel limit (high) = ',badvec[1]
        print, 'Gain (PHPADU)          = ',gain
        print, 'Object net             = ',mags[0]
        print, 'Error in object net    = ',errap[0]
        print, 'Sky average            = ',sky[0]
        print, 'Error in sky average   = ',skyerr[0]
    ENDIF
ENDIF
IF event.id EQ (*pinfo).quit_button THEN BEGIN
    destroy=1b
ENDIF

IF destroy THEN widget_control, event.top, /destroy

RETURN
END

PRO EXAMDISPLAY_APERPHOT_EVENT, Event
widget_control, event.top, get_uvalue=pinfo
(*pinfo).this->aperphot_event, event, pinfo
RETURN
END

PRO EXAMDISPLAY::APERPHOT, Info
;
;  Popup for aperture photometry.
;

dispsize = 200

widget_control, self.maindisp, get_value=mw
wtitle = 'Aperture Photometry (Window '+strn(mw)+')'
tlb = widget_base(/col, title=wtitle, group=(*info).maintlb, floating=0)
rowy = widget_base(tlb, /row)
rowz = widget_base(tlb, /row)
rowa = widget_base(tlb, /row)
rowb = widget_base(tlb, /row)
rowc = widget_base(tlb, /row)
rowc1 = widget_base(tlb, /row)
rowc2 = widget_base(tlb, /row)
rowd = widget_base(tlb, /row)

x_label = widget_label(rowy, value='X:')
x_text = widget_text(rowy, xsize=8, value=strn(self.xcursor), /edit)
y_label = widget_label(rowy, value='Y:')
y_text = widget_text(rowy, xsize=8, value=strn(self.ycursor), /edit)
curs_button = widget_button(rowy, value='READ CURSOR', /no_rel)
cntrd_button = widget_button(rowz, value='ADJUST CENTROID', /no_rel)
cntrd_label = widget_label(rowz, value='using box size = ')
cntrd_text = widget_text(rowz, xsize=8, value='7', /edit)

objrad_label = widget_label(rowa, value='Object Radius (px):')
objrad = widget_text(rowa, xsize=8, value='5', /edit)
oskyann_label = widget_label(rowb, value='Sky Radii (px):')
iskyann = widget_text(rowb, xsize=8, value='15', /edit)
oskyann = widget_text(rowb, xsize=8, value='20', /edit)
fsky_label = widget_label(rowc, value='Forced Sky Value: ')
fskyval = widget_text(rowc, xsize=15, value=' ',/edit)
gain_label = widget_label(rowc1, value='Gain (e- per ADU):')
IF ptr_valid(self.header) THEN BEGIN
    par = strmid(*self.header, 0, 8)
    gainpar = where(strpos(par, 'GAIN') GE 0 $
                  OR strpos(par, 'PHPADU') GE 0, npar)
    gval = 1.0
    kk=npar
    WHILE kk GT 0 DO BEGIN
        kk = kk - 1
        useg = par[gainpar[kk]]
        gval2 = sxpar(*self.header, useg)
        IF gval2 GT 0 THEN BEGIN
            kk=-1
            gval = gval2
        ENDIF
    ENDWHILE
ENDIF ELSE gval = 1.0
gain = widget_text(rowc1, xsize=12, value=strn(gval), /edit)
badmin_label = widget_label(rowc2, value='Bad pixel limits:')
badmin = widget_text(rowc2, value='-32765', xsize=12, /edit)
badmax = widget_text(rowc2, value='32767', xsize=12, /edit)
objdisp = obj_new('IMGDISPLAY', rowd, xsize=dispsize, ysize=dispsize, $
                  /ps_dialog)
objdisp->psfile, '_examdisplay_aper.ps', 'APERTURE PHOTOMETRY'

row3 = widget_base(tlb, /row)
row4 = widget_base(tlb, /row)
row5 = widget_base(tlb, /row)

object_label = widget_label(row3, value='Object net:')
object_text = widget_text(row3, xsize=10, value='0', /edit)
objerr_label = widget_label(row3, value='Error:')
objerr_text = widget_text(row3, xsize=10, value='0')
sky_label = widget_label(row4, value='Sky:')
sky_text = widget_text(row4, xsize=10, value='0')
skyerr_label = widget_label(row4, value='Error:')
skyerr_text = widget_text(row4, xsize=10, value='0')
quit_button = widget_button(row5, value='DISMISS', /no_rel)

widget_control, tlb, /realize

objdisp->id, mainid
widget_control, mainid, get_value=gwin

popup_info = ptr_new({tlb:tlb, $
                this:(*info).this, $
                x_text:x_text, $
                y_text:y_text, $
                curs_button:curs_button, $
                cntrd_button:cntrd_button, $
                cntrd_text:cntrd_text, $
                objrad:objrad, $
                oskyann:oskyann, $
                iskyann:iskyann, $
                fskyval:fskyval, $
                gain:gain, $
                badmin:badmin, $
                badmax:badmax, $
                objdisp:objdisp, $
                dispsize:dispsize, $
                gwin:gwin, $
                object_text:object_text, $
                objerr_text:objerr_text, $
                sky_text:sky_text, $
                skyerr_text:skyerr_text, $
                quit_button:quit_button })

widget_control, tlb, set_uvalue=popup_info

xmanager, 'examdisplay_aperphot', tlb, $
    event_handler='examdisplay_aperphot_event', $
    cleanup='examdisplay_aperphot_cleanup', /no_block

pseudo = {id:objrad, top:tlb, handler:tlb}
widget_control, objrad, send_event=pseudo

RETURN
END


PRO EXAMDISPLAY::WORLD_COO_UPDATE

IF self.wcs THEN BEGIN
    xyad, *self.header, self.xcursor, self.ycursor, $
          coo1, coo2
ENDIF ELSE BEGIN
    xc = self.xcursor + 1
    yc = self.ycursor + 1
    dx = xc - sxpar(*self.header, 'CRPIX1')
    dy = yc - sxpar(*self.header, 'CRPIX2')
    d1 = dx*sxpar(*self.header, 'CD1_1') + dy*sxpar(*self.header,'CD1_2')
    d2 = dy*sxpar(*self.header, 'CD2_1') + dy*sxpar(*self.header,'CD2_2')
    coo1 = d1 + sxpar(*self.header, 'CRVAL1')
    coo2 = d2 + sxpar(*self.header, 'CRVAL2')
ENDELSE

widget_control, self.x_world_text, set_value=strn(coo1,format='(F15.7)')
widget_control, self.y_world_text, set_value=strn(coo2,format='(F15.7)')

IF widget_info(self.x_world_60_text, /valid) THEN BEGIN
    ast = adstring(coo1,coo2, 3)
    sign = strpos(ast, '+')
    IF sign LT 0 THEN sign = strpos(ast,'-')
    scoo1 = strmid(ast,0,sign)
    scoo2 = strmid(ast,sign,strlen(ast)-strlen(scoo1))
    
    widget_control, self.x_world_60_text, set_value=scoo1
    widget_control, self.y_world_60_text, set_value=scoo2
ENDIF

RETURN
END

PRO EXAMDISPLAY_WORLD_COO_CLEANUP, Tlb
widget_control, tlb, get_uvalue=pinfo
IF ptr_valid(pinfo) THEN ptr_free, pinfo
RETURN
END


PRO EXAMDISPLAY::WORLD_COO_EVENT, Event, Pinfo
CASE event.id OF
    (*pinfo).log_button: BEGIN
        IF self.logging THEN BEGIN
            widget_control, self.maindisp, get_value=maingraph
            print, ' '
            print, '---------- (X,Y) = (', strn(self.xcursor), ',', $
                   strn(self.ycursor), ') ------------------------------' $
                   + '----- Window ' + strn(maingraph)
            print, 'Pixel value = ', $
                 (*self.image)[self.xcursor, self.ycursor]
            widget_control, self.x_world_text, get_value=tmp
            widget_control, self.x_world_type, get_value=tmp2
            print, tmp2[0], ' = ', tmp[0]
            widget_control, self.y_world_text, get_value=tmp
            widget_control, self.y_world_type, get_value=tmp3
            print, tmp3[0], ' = ', tmp[0]
            widget_control, self.x_world_60_text, get_value=tmp
            print, tmp2[0], ' = ', tmp[0]
            widget_control, self.y_world_60_text, get_value=tmp
            print, tmp3[0], ' = ', tmp[0]
        ENDIF ELSE BEGIN
            junk = dialog_message('Logging not on')
        ENDELSE
    END
    (*pinfo).quit_button: BEGIN
        self.x_world_type = -1L
        self.y_world_type = -1L
        self.x_world_text = -1L
        self.y_world_text = -1L
        self.x_world_60_text = -1L
        self.y_world_60_text = -1L
        widget_control, (*pinfo).tlb, /destroy
    END
ENDCASE
RETURN
END


PRO EXAMDISPLAY_WORLD_COO_EVENT, Event
widget_control, event.top, get_uvalue=pinfo
(*pinfo).this->world_coo_event, event, pinfo
RETURN
END

PRO EXAMDISPLAY::WORLD_COO, Info

IF 1b - ptr_valid(self.header) THEN RETURN

IF widget_info(self.x_world_text, /valid) THEN BEGIN
    widget_control, self.x_world_text, /show
    RETURN
ENDIF

widget_control, self.maindisp, get_value=mw
wtitle = 'World Coords (Window '+strn(mw)+')'
tlb = widget_base(/col, title=wtitle, group=(*info).maintlb, /floating)

row2 = widget_base(tlb, /row)
row3 = widget_base(tlb, /row)
row4 = widget_base(tlb, /row)
row5 = widget_base(tlb, /row)
row6 = widget_base(tlb, /row)
row7 = widget_base(tlb, /row)
row8 = widget_base(tlb, /row)
row9 = widget_base(tlb, /row)
row10 = widget_base(tlb, /row)
row11 = widget_base(tlb, /row)
row12 = widget_base(tlb, /row)
row99 = widget_base(tlb, /row)

;
;  What kind of coordinates?
ctype1 = strtrim(sxpar(*self.header, 'CTYPE1'),2)
ctype2 = strtrim(sxpar(*self.header, 'CTYPE2'),2)

t1 = ctype1
t2 = ctype2
t1a = strtrim(strupcase(gettok(t1,'-')),2)
t2a = strtrim(strupcase(gettok(t2,'-')),2)

equatorial = t1a EQ 'RA' AND t2a EQ 'DEC'
galactic = t1a EQ 'GLON' AND t2a EQ 'GLAT'
ecliptic = t1a EQ 'ELON' AND t2a EQ 'ELAT'
skycoords = equatorial OR galactic OR ecliptic

use_60 = equatorial

;
;  Get plate scales.
getrot, *self.header, rot, cdelt
IF skycoords THEN BEGIN
    IF cdelt[0] LT 1./60. THEN BEGIN
        cdelt[0] = cdelt[0] * 3600
        cunit1 = 'ARCSEC/PIXEL'
    ENDIF ELSE BEGIN
        cdelt[0] = cdelt[0] * 60
        cunit1 = 'ARCMIN/PIXEL'
    ENDELSE
    IF cdelt[1] LT 1./60. THEN BEGIN
        cdelt[1] = cdelt[1] * 3600
        cunit2 = 'ARCSEC/PIXEL'
    ENDIF ELSE BEGIN
        cdelt[1] = cdelt[1] * 60
        cunit2 = 'ARCMIN/PIXEL'
    ENDELSE
    equinox = sxpar(*self.header, 'EQUINOX')
    epoch = ''
    IF strtrim(equinox,2) EQ '0' THEN BEGIN
        equinox = ''
        epoch = sxpar(*self.header, 'EPOCH')
        IF strtrim(epoch,2) EQ '0' THEN epoch = ''
    ENDIF
ENDIF ELSE BEGIN
    cunit1 = 'PER PIXEL'
    cunit2 = 'PER PIXEL'
ENDELSE

scale1 = strn(abs(cdelt[0]),format='(G14.6)') + ' ' + cunit1
scale2 = strn(abs(cdelt[1]),format='(G14.6)') + ' ' + cunit2
t1 = ctype1
t2 = ctype2
t1a = strtrim(strupcase(gettok(t1,'-')),2)
t2a = strtrim(strupcase(gettok(t2,'-')),2)
equatorial = t1a EQ 'RA' AND t2a EQ 'DEC'
use_60 = equatorial

; xaxis_label = widget_label(row2, value='X Axis Type:  ')
; xaxis_text = widget_text(row2, xsize=12, value=ctype1)
; yaxis_label = widget_label(row3, value='Y Axis Type:  ')
; yaxis_text = widget_text(row3, xsize=12, value=ctype2)

xscale_label = widget_label(row4, value='X Scale:  ')
xscale_text = widget_text(row4, xsize=25, value=scale1)
yscale_label = widget_label(row5, value='Y Scale:  ')
yscale_text = widget_text(row5, xsize=25, value=scale2)

xvalue_label = widget_label(row6, value=ctype1 + ':  ')
xvalue_text = widget_text(row6, xsize=20, value='-999')
IF use_60 THEN $
    xvalue_60_text = widget_text(row7, xsize=20, value='-999')
yvalue_label = widget_label(row8, value=ctype2 + ':  ')
yvalue_text = widget_text(row8, xsize=20, value='-999')
IF use_60 THEN $
    yvalue_60_text = widget_text(row9, xsize=20, value='-999')

equinox = ''
epoch = ''
IF skycoords THEN BEGIN
    IF equatorial THEN lab = 'EQUATORIAL'
    IF galactic THEN lab = 'GALACTIC'
    IF ecliptic THEN lab = 'ECLIPTIC'
    arrow_label_frame = widget_base(row10,col=1)
    arrow_label1 = widget_label(arrow_label_frame, value='ORIENTATION IN')
    arrow_label2 = widget_label(arrow_label_frame, value=lab)
    arrow_label3 = widget_label(arrow_label_frame, value='SYSTEM')
    arrow_draw = widget_draw(row10, xsize=110, ysize=110)
ENDIF

IF equinox NE '' THEN BEGIN
    equinox_label = widget_label(row11, value='EQUINOX:  ')
    equinox_text = widget_text(row11, xsize=15, value=strn(equinox))
ENDIF ELSE IF epoch NE '' THEN BEGIN
    epoch_label = widget_label(row11, value='EPOCH:  ')
    epoch_text = widget_text(row11, xsize=15, value=strn(epoch))
ENDIF

log_button = widget_button(row12, value='LOG CURRENT VALUES', /no_rel)

quit_button = widget_button(row99, value='DISMISS', /no_rel)

widget_control, tlb, /realize

IF skycoords THEN BEGIN
    widget_control, arrow_draw, get_value=arrow_win
    svwin = !D.window
    wset, arrow_win
    arrows, *self.header, 55, 55, /notvertex
    IF svwin GE 0 THEN wset, svwin
ENDIF

popup_info = ptr_new({tlb:tlb, $
                this:(*info).this, $
                log_button:log_button, $
                quit_button:quit_button })

self.x_world_text = xvalue_text
self.y_world_text = yvalue_text
self.x_world_type = xvalue_label
self.y_world_type = yvalue_label
IF use_60 THEN BEGIN
    self.x_world_60_text = xvalue_60_text
    self.y_world_60_text = yvalue_60_text
ENDIF

widget_control, tlb, set_uvalue=popup_info

xmanager, 'examdisplay_world_coo', tlb, $
    event_handler='examdisplay_world_coo_event', $
    cleanup='examdisplay_world_coo_cleanup', /no_block
    
;; widget_control, (*info).tlb, /destroy

RETURN
END

PRO EXAMDISPLAY::LISTING, Info

IF 1b - ptr_valid(self.header) THEN RETURN

widget_control, self.maindisp, get_value=mw

IF self.logging THEN BEGIN
    print, ' '
    print, '----------------------', $
           '-------------------------------' $
           + '---------- Window ' + strn(mw)
    FOR i=0,n_elements(*self.header)-1 DO $
        print, strtrim((*self.header)[i])
ENDIF ELSE BEGIn
    title = 'Header Listing (Window '+strn(mw)+')'
    xdispstr, *self.header, title=title, $
        group=self.baseid, $
        font=*self.fixedfont
ENDELSE
;; widget_control, (*info).tlb, /destroy
RETURN
END

PRO EXAMDISPLAY::LOGGING, Info
self.logging = widget_info((*info).log_droplist, /droplist_select)
RETURN
END

PRO EXAMDISPLAY::HELP
helptext=[ $
'Examining the Displayed Image', $
' ', $
'Summary of Capabilities:', $
'   (1)   Pixel listing', $
'   (2)   Statistics of whole image', $
'   (3)   Statistics of region within zoom box', $
'   (4)   Various plots', $
'   (5)   Region-of-interest photometry', $
'   (6)   Aperture photometry', $
'   (7)   Aperture growth photometry', $
'   (8)   World coordinate display', $
'   (9)   Header display', $
'   (10)  Printing of outputs', $
'   (11)  Some history', $
' ',$
'Details:', $
' ',$
'   (1)  Pixel listing', $
' ',$
'        First position the zoom box where you want the listing', $
'        centered.  Then click on the "pixel listing" button.', $
'        An 11x11 area of the image will be listed.', $
' ',$
'   (2)  Statistics of image', $
' ',$
'        There are two choices, selected by clicking on the', $
'        following buttons:', $
' ',$
'        "ordinary statistics":  Mean, standard deviation, min, max, ',$
'                                median, and a little low-resolution ',$
'                                histogram will be displayed.', $
' ',$
'        "outlier statistics":  Statistics based on quantiles', $
'                               will be displayed.  The terminology', $
'                               of Tukey is used (hinges, fences,', $
'                               and so on) and is defined in the', $
'                               listing itself.', $
' ',$
'   (3)  Statistics of region within zoom box', $
'        There are two choices, selected by clicking on the', $
'        following buttons:', $
' ',$
'        "ordinary statistics":  see above ',$
'        "outlier statistics":  see above', $
' ',$
'   (4)  Various plots', $
' ',$
'        There are two choices, selected by clicking on the', $
'        following buttons:', $
' ',$
'        "row/column plot":  This is a cut centered on where the zoom', $
'                            box happens to be before you hit the button.', $
'                            A new widget pops up with sliders to adjust', $
'                            the plot limits and blanks in which plot', $
'                            limits may be typed directly (better than', $
'                            sliders for some situations, e.g., log plots).', $
'                            The new widget also has buttons to switch', $
'                            between row and column mode and a blank', $
'                            to fill in the width in pixels of the cut.', $
'                            There are also buttons to switch between ',$
'                            log and linear axis scaling, and buttons to', $
'                            increment or decrement the width of the cut.', $
'                            For widths greater than 1 pixel, a sum, not', $
'                            an average, is plotted.  A simple postscript', $
'                            output capability can be invoked by pushing', $
'                            the "Postscript" button at the bottom.', $
' ',$
'        "profile":          Instructions will appear.  They say just', $
'                            to click at either end of the desired profile.', $
'                            The plot appears in a new widget similar to', $
'                            one described under "row/column plot" above.', $
' ', $
'   (5)  Region-of-interest photometry', $
' ', $
'        Click on the button marked "r.o.i. photometry".  A popup appears', $
'        with buttons you click when you want to measure object and when', $
'        you want to measure sky.  The object and sky regions are actually', $
'        picked using a routine only slightly altered from CW_DEFROI that', $
'        comes with IDL.  This is a pretty straightforward r.o.i. defining', $
'        routine, and you can get to know any peculiarities just by trying', $
'        it out.  The results of the photometry are displayed in text ', $
'        fields within the subsidiary widget.  The sky and object ', $
'        region must be selected within the main image window.', $
' ', $
'        Photometry results can be stored in save files by pushing the', $
'        "write save file" button in the photometry pop-up.  Files are named', $
'        using a tag string specified via a text field in the photometry ', $
'        pop-up, combined with a counter incremented for each source ', $
'        (for uniqueness).  The contents of the save file are as follows: ', $
'               AVOBJNET    average net flux per pixel in object', $
'               LOG_COUNT   unique counter for object', $
'               OBJNET      net flux in entire object', $
'               OBJSKY      background flux in object aperture', $
'               OBJTOT      total flux in object aperture', $
'               OBJ_ROI     (POINTER) subscripts of object pixels', $
'               SKYAVG      average value of sky pixels', $
'               SKY_ROI     (POINTER) subscripts of sky pixels', $
' ', $
'   (6)  Aperture photometry', $
' ', $
'        Click on the button marked "aperture photometry".  A popup', $
'        appears that shows a small region around the cursor and allows', $
'        you to change photometry parameters.  If you move the cursor', $
'        on the main widget, you can do photometry at the new position', $
'        by pushing the READ CURSOR button.  The coordinates can be ', $
'        refined with the ADJUST CENTROID button.  IDLAstro library', $
'        routines do the work:  CNTRD for centroiding and APER for ', $
'        photometry.  The idea of including a small image display in ', $
'        this popup came from ATV by Barth et al.', $
' ', $
'   (7)  Aperture growth photometry', $
' ', $
'        Similar to plain aperture photometry except that sky is obtained', $
'        by fitting between two radii, plots are shown, and the object', $
'        aperture is always the same as the inner sky radius.  Error', $
'        estimates are obtained from APER for the appropriate sky and', $
'        object radii although the actual sky value comes from a different', $
'        algorithm.', $
' ', $
'   (8)  World coordinate display', $
' ', $
'        If you click on "world coordinates", you will get a little ', $
'        subsidiary widget that gives a continuous display of the cursor ',$
'        position in the world coordinate frame.  This is available ', $
'        only if a FITS header has been supplied.', $
' ', $
'   (9)  Header display', $
' ', $
'        Straightforward:  if a FITS header has been supplied, you can ', $
'        click on the "header listing" button and a simple file display', $
'        widget will come up showing the header text.', $
' ', $
'   (10) Printing of outputs', $
' ', $
'        This is controlled by a droplist you can use to toggle between', $
'        a state where image examination outputs appear in the usual IDL', $
'        command line window, and a state where a new text display widget', $
'        pops up to receive each result.  This widget allows simple text ', $
'        searches.', $
' ', $
'   (11) Some history', $
' ', $
'        There are several agendas incorporated into this program.  One ', $
'        was to incorporate routines from the Ultraviolet Imaging Telescope', $
'        (UIT) software library called MOUSSE into a widget.  One was to', $
'        write an image display that could be incorporated easily into', $
'        special-purpose analysis widgets while automatically including', $
'        a lot of general image examination tools.  Object-oriented IDL', $
'        coding is used (though not "object graphics" as such).' ]

xdispstr, helptext, title='Help on Image Examination', $
    group=(*self.popup_info).tlb, $
    font=*self.fixedfont

END


PRO EXAMDISPLAY::POPUP_EVENT, Event, Info
;  5 Apr 2000 - Dummy event for log_filename.  RSH
destroy = 0b
CASE event.id OF
    (*info).allstats_button:  BEGIN
        self->allstats, info
    END
    (*info).allrstats_button:  BEGIN
        self->allrstats, info
    END
    (*info).stats_button:  BEGIN
        self->stats, info
    END
    (*info).rstats_button:  BEGIN
        self->rstats, info
    END
    (*info).imlist_button:  BEGIN
        self->imlist, info
    END
    (*info).profile_button:  BEGIN
        self->profile, info
    END
    (*info).rowcol_button:  BEGIN
        self->rowcol, info
    END
    (*info).roiphot_button:  BEGIN
        self->roiphot, info
    END
    (*info).aperphot_button:  BEGIN
        self->aperphot, info
    END
    (*info).apergrowth_button:  BEGIN
        self->apergrowth, info
    END
    (*info).world_coo_button:  BEGIN
        self->world_coo, info
    END
    (*info).listing_button:  BEGIN
        self->listing, info
    END
    (*info).log_droplist:  BEGIN
        self->logging, info
    END
    (*info).help_button:  BEGIN
        self->help
    END
    (*info).quit_button:  BEGIN
        destroy = 1b
    END
ENDCASE

IF destroy THEN widget_control, event.top, /destroy
RETURN
END


PRO EXAMDISPLAY_POPUP_EVENT, Event
widget_control, event.top, get_uvalue=info
(*info).this->popup_event, event, info
RETURN
END


PRO EXAMDISPLAY_POPUP_CLEANUP, Tlb
widget_control, tlb, get_uvalue=info
IF ptr_valid(info) THEN ptr_free, info
RETURN
END

PRO EXAMDISPLAY::POPUP, Event

;  Changed where frame3 is added to misclabels array.  RSH, 8 Oct. 1999

IF ptr_valid(self.popup_info) THEN BEGIN
    tlb = (*self.popup_info).tlb
    iml = (*self.popup_info).imlist_button
    IF widget_info(tlb, /valid) $
        AND widget_info(iml, /valid) THEN BEGIN
        widget_control, tlb, /show
        RETURN
    ENDIF
ENDIF

widget_control, self.maindisp, get_value=mw
title = 'EXAMINE (Window ' + strn(mw) + ')'
tlb = widget_base(title=title, col=1, group_leader=event.top)

exam_buttons = widget_base(tlb, col=1)
top_row = widget_base(tlb, row=1)

frame4 = widget_base(tlb, col=1)
log_droplist = widget_droplist(frame4, $
                   value=['Print to Separate Window', 'Print in IDL Window'])
widget_control, log_droplist, set_droplist_select=self.logging

;log_fileblock = widget_base(frame4,row=1)
;log_filelab = widget_label(log_fileblock, value='Log Tag:  ')
;log_filename = widget_text(log_fileblock, xsize=10, ysize=1, $
                           ;value=*self.log_tag, /editable)

;IF self.logging THEN widget_control, log_filename, editable=0
                           
frame0 = widget_base(tlb,col=1,frame=2)
label0 = widget_label(frame0, value='WHOLE IMAGE:')
allstats_button = widget_button(frame0, value='ordinary statistics')
allrstats_button = widget_button(frame0, value='outlier statistics')

frame1 = widget_base(tlb,col=1,frame=2)
label1 = widget_label(frame1, value='CURRENT CURSOR OR BOX:')
imlist_button = widget_button(frame1, value='pixel listing')
rowcol_button = widget_button(frame1, value='row/column plot')
stats_button = widget_button(frame1, value='ordinary statistics')
rstats_button = widget_button(frame1, value='outlier statistics')

frame2 = widget_base(tlb,col=1,frame=2)
label2 = widget_label(frame2, value='INTERACTIVE:')
roiphot_button = widget_button(frame2, value='r.o.i. photometry')
aperphot_button = widget_button(frame2, value='aperture photometry')
apergrowth_button = widget_button(frame2, value='aperture growth photometry')
profile_button = widget_button(frame2, value='profile')

misc_labels = [frame4, frame1, frame2]

world_coo_button = -1L
listing_button = -1L
wcs = 0b
coo = 0b
IF ptr_valid(self.header) THEN BEGIN
    frame3 = widget_base(tlb,col=1,frame=2)
    label3 = widget_label(frame3, value='INFORMATION FROM HEADER:')
    listing_button = widget_button(frame3, value='header listing')
    extast, *self.header, aststruc
    szast = size(aststruc)
    ast_ok = szast[szast[0]+1] EQ 8
    IF ast_ok THEN BEGIN
        map_types=['DEF','AZP','TAN','SIN','STG', $
                   'ARC','ZPN','ZEA','AIR','CYP', $      
                   'CAR','MER','CEA','COP','COD', $
                   'COE','COO','BON','PCO','GLS', $                 
                   'PAR','AIT','MOL','CSC','QSC', $
                   'TSC']
        ctype = strtrim(sxpar(*self.header, 'CTYPE1'),2)
        IF strlen(ctype) LT 8 THEN ctype = strmid(ctype+'        ',0,8)
        mtype = strmid(ctype,5,3)
        junk = where(mtype EQ map_types, count)
        IF count EQ 1 THEN BEGIN
            wcs = 1b
            coo = 1b
        ENDIF ELSE BEGIN
            IF ctype NE '0       ' THEN BEGIN
                wcs = 0b
                coo = 1b
            ENDIF
        ENDELSE
        IF coo THEN BEGIN
            world_coo_button = widget_button(frame3, value='world coordinates')
        ENDIF
    ENDIF
    misc_labels = [misc_labels, frame3]
ENDIF

self.wcs = wcs

quit_button = widget_button(top_row, value='DISMISS')
help_button = widget_button(top_row, value='    Help    ')
misc_labels = [misc_labels, help_button]

results = widget_base(tlb,col=1)

widget_control, tlb, /realize

info = ptr_new({maintlb:event.top, $
               tlb:tlb, imlist_button:imlist_button, $
               profile_button:profile_button, $
               allstats_button:allstats_button, $
               stats_button:stats_button, $
               allrstats_button:allrstats_button, $
               rstats_button:rstats_button, $
               rowcol_button:rowcol_button, $
               roiphot_button:roiphot_button, $
               aperphot_button:aperphot_button, $
               apergrowth_button:apergrowth_button, $
               world_coo_button:world_coo_button, $
               listing_button:listing_button, $
               misc_labels:misc_labels, $
               x0:0L, y0:0L, $
               maingraph:-1L, $
               log_droplist:log_droplist, $
               quit_button:quit_button, $
               help_button:help_button, $
               instruc:-1L, $
               ;results:results, $
               this:self})

self.popup_info = info
               
widget_control, tlb, set_uvalue=info

xmanager, 'examdisplay_popup', tlb, $
    event_handler='examdisplay_popup_event', $
    cleanup='examdisplay_popup_cleanup', /no_block

RETURN
END

PRO EXAMDISPLAY::HOTKEYS_CLEANUP

IF ptr_valid(self.popup_info) THEN BEGIN
    tlb = (*(self.popup_info)).tlb
    IF widget_info(tlb, /valid) THEN $
        widget_control, (*self.popup_info).tlb, /destroy
ENDIF

IF ptr_valid(self.popup_info) THEN ptr_free, self.popup_info

RETURN
END


PRO EXAMDISPLAY::HOTKEYS_HELP
helptext=[ $
'Examining the Displayed Image', $
' ', $
'Summary of Capabilities:', $
'   (l)   Pixel listing', $
'   (S)   Ordinary statistics of whole image', $
'   (s)   Ordinary statistics of region within zoom box', $
'   (X)   Outlier statistics of whole image', $
'   (x)   Outlier statistics of region within zoom box', $
'   (c)   Row or column plot', $
'   (p)   Profile plot', $
'   (r)   Region-of-interest photometry', $
'   (a)   Aperture photometry', $
'   (A)   Aperture growth photometry', $
'   (w)   World coordinate display', $
'   (H)   Header display', $
' ', $
'For more complete help see HELP button on EXAMINE popup.' ]

xdispstr, helptext, title='Help on Examination Hotkeys', $
    group=(*self.popup_info).maintlb, $
    font=*self.fixedfont

END

PRO EXAMDISPLAY::HOTKEYS, Event
;  21 Oct 2000

self->hotkeys_cleanup

IF event.type EQ 0 THEN BEGIN
    let = string(event.ch)
ENDIF ELSE BEGIN
    let = ''
ENDELSE

widget_control, self.maindisp, get_value=maingraph

info = ptr_new({maintlb:event.top, $
               tlb:-1L, $
               x0:0L, y0:0L, $
               maingraph:maingraph, $
               instruc:-1L, $
               this:self})

self.popup_info = info

CASE let OF
    'S':  BEGIN
        self->allstats, info
    END
    'X':  BEGIN
        self->allrstats, info
    END
    's':  BEGIN
        self->stats, info
    END
    'x':  BEGIN
        self->rstats, info
    END
    'l':  BEGIN
        self->imlist, info
    END
    'p':  BEGIN
        self->profile, info
    END
    'c':  BEGIN
        self->rowcol, info
    END
    'r':  BEGIN
        self->roiphot, info
    END
    'a':  BEGIN
        self->aperphot, info
    END
    'A':  BEGIN
        self->apergrowth, info
    END
    'w':  BEGIN
        self->world_coo, info
    END
    'H':  BEGIN
        self->listing, info
    END
    'h':  BEGIN
        self->hotkeys_help
    END
    ELSE:  ; nothing
ENDCASE

RETURN
END


PRO EXAMDISPLAY::E, Event

;
;  Event handling for the examdisplay object.
;

IF widget_info(self.glabel, /valid) THEN BEGIN
    widget_control, self.maindisp, get_value=gwin
    widget_control, self.glabel, set_value=strn(gwin)
ENDIF

CASE event.id OF
    self.exam_button:  BEGIN
        self->popup, event
    END
    self.maintext1:  BEGIN
        self->hotkeys, event
    END
    ELSE:  BEGIN
        IF event.id EQ self.maindisp THEN BEGIN
            event_type = tag_names(event, /structure)
            IF event_type NE '' $
               AND widget_info(self.exam_button,/valid) THEN BEGIN
                IF event_type EQ 'WIDGET_TRACKING' THEN BEGIN
                    IF 1b - event.enter  THEN BEGIN
                        widget_control, self.maintext0, /input_focus
                    ENDIF
                ENDIF ELSE BEGIN
                    widget_control, self.maintext1, /input_focus
                ENDELSE
            ENDIF
        ENDIF
        self->imgdisplay::e, event
        IF obj_valid(self) THEN BEGIN
            IF widget_info(self.x_world_text, /valid) THEN BEGIN
                self->world_coo_update
            ENDIF
        ENDIF
    END
ENDCASE

RETURN
END

PRO EXAMDISPLAY::DRAW, Image, Header, _EXTRA=extra

IF n_elements(header) GT 0 THEN BEGIN
    IF ptr_valid(self.header) THEN ptr_free, self.header
    self.header = ptr_new(header)
ENDIF

self->imgdisplay::draw, image, _extra=extra
RETURN
END

PRO EXAMDISPLAY::CLEANUP

IF ptr_valid(self.header) THEN ptr_free, self.header
IF ptr_valid(self.log_tag) THEN ptr_free, self.log_tag

self->hotkeys_cleanup
self->imgdisplay::cleanup

RETURN
END


FUNCTION EXAMDISPLAY::INIT, Parent, $
    HEADER=header, NOEXAMINE=noexamine, LOG_tag=log_tag, _EXTRA=extra

;  Arguments:
;   Parent:    Widget ID of parent base widget
;  Keywords:
;   HEADER:    Image header
;   NOEXAMINE: If set omit examining capabilities.
;   LOG_TAG:   Never actually used; is here to avoid problems
;              with old calling programs.
;

ret = self->imgdisplay::init(Parent, _extra=extra)
    
;
;   self.button_frame is part of IMGDISPLAY
IF n_elements(header) GT 0 THEN self.header = ptr_new(header)

IF n_elements(log_tag) LE 0 THEN log_tag='examdisp'
self.log_tag = ptr_new(log_tag)

self.exam_button=-1L
self.maintext0=-1L
self.maintext1=-1L
self.logging = 1
nexam = keyword_set(noexamine)
IF 1b - nexam THEN BEGIN
    self.exam_button = widget_button(self.button_frame, value='EXAMINE')
    self.maintext0 = widget_text(self.button_frame, $
                                 scr_xsize=1,scr_ysize=1, $
                                 /editable)
    self.maintext1 = widget_text(self.button_frame, $
                                 scr_xsize=1,scr_ysize=1, $
                                 /all_events)
ENDIF

readout_exists = widget_info(self.xreadout, /valid)
IF readout_exists THEN BEGIN
    value_column = widget_info(self.xreadout, /parent)
    quantity_row = widget_info(value_column, /parent)
    label_column = widget_info(quantity_row, /child)
    junk = widget_label(label_column, value='Window:')
    self.glabel = widget_label(value_column, value='nnnnn')
ENDIF ELSE self.glabel = -1L

RETURN, ret
END

PRO EXAMDISPLAY__DEFINE

;  Just gets the object definition into the IDL session.

struct = {EXAMDISPLAY, exam_button:-1L, $
          maintext0:-1L, $
          maintext1:-1L, $
          header:ptr_new(), $
          popup_info:ptr_new(), $
          x_world_type:-1L, $
          y_world_type:-1L, $
          x_world_text:-1L, $
          y_world_text:-1L, $
          x_world_60_text:-1L, $
          y_world_60_text:-1L, $
          logging:1L, $
          log_count:0L, $
          log_tag:ptr_new(), $
          wcs:0b, $
          glabel:-1L, $
          inherits IMGDISPLAY}

RETURN
END
