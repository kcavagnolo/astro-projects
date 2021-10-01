; $Id: imgdisplay__define.pro,v 1.1.1.1 2007-02-03 17:10:49 cavagnolo Exp $
;+
;              Object Oriented Widget for Image Display and Hardcopy
;
; NAME:
;       IMGDISPLAY
;
; PURPOSE:
;       This file contains code for an object-oriented image display 
;       widget.
;
; EXPLANATION:
;       The programming is object-oriented, but the graphics are
;       in direct graphics mode, not object graphic.  This widget
;       lets the user adjust the image scaling and contrast and print
;       out hard copies.  
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE FOR INITIALIZATION:
;       new = obj_new('imgdisplay', parent, keywords=.....)
;
; ARGUMENT:
;       parent:          widget ID of parent base widget
;
; KEYWORD INPUTS:
;       xsize, ysize:    dimensions of total image area, pixels
;       x_scroll_size, y_scroll_size:  dimensions of scrolling
;                        viewport, if desired
;       uvalue:          arbitrary user value
;       kill_button:     include a button to destroy the widget cleanly
;       kill_dialog:     ask user "really?" before killing widget
;       fixedfont:       font for data entry fields
;       refresh:         make image refreshable; the widget
;                        will carry around its own pixmap backup(s)
;       subwindow:       X0,Y0, NX, NY specifying a section of the
;                        total window that will be used for image
;                        display (and be separately refreshable if
;       title:           title of generated hardcopy
;       nocontrols:      do not generate image display controls
;       scale_template:  another IMGDISPLAY from which to get
;                        image scaling parameters
;       bottom, ncolors: specify a section of the color table
;                        to be used, leaving entres 0...bottom-1
;                        for graphics colors
;       ps_dialog:       on generating hardcopy, prompt user for file
;                        name
;       nobuttons:       do not generate any control buttons at all
;       zoomwin:         generate a zoom window
;       panwin:          generate a pan window;  if two elements,
;                        then they are taken as x size and y size
;       readout:         generate a pixel readout
;       vertical:        put readout all the way at bottom
;
; OUTPUTS:
;       result = ID of immediate child of parent
;
; RESTRICTIONS:
;       This widget is self-contained and doesn't return
;       any events to the outside world.  It is a subordinate widget
;       and must be generated as a child of some other base.
;
; MODIFICATION HISTORY:
;       Converted to object-oriented code from cw_sig_display.pro,
;           3 Mar 99, RSH, RITSS
;       Several new capabilities.  23 April 1999 RSH
;       Spiffed up documentation, make ::V routine cleaner, fixed
;           bugs, added kill_dialog.  29 April 1999 RSH
;       Several small changes.  11 May 1999 RSH
;       Changed name of ::v to ::draw, as more conventional naming.
;           21 May 1999
;       Some cursor debugging.  24 May 1999 RSH
;       Cursor outputs not given if no image.  A pixmap memory leak
;           fixed.  14 July 1999  RSH
;       Fixed problem if zoom box is bigger in at least on dimension
;           than the main display.  23 July 1999 RSH
;       Size function now returns more info.  28 July 1999 RSH
;       Fixed bug in setting boxcolor.  2 Aug 1999 RSH
;       Made popup group leader of xloadct.  5 Aug 1999 RSH
;       Changed readouts to label widgets.  14 Sep 1999 RSH
;       Add NOOLDZOOM to ::DRAW so that a random zoom shadow
;           can be suppressed.  5 Oct 1999 RSH
;       Fixed bug trying to read from zoom window with no contents.
;           13 Oct 1999 RSH
;       INIT routine forces device to x.  21 Mar 2000 RSH
;       Redisplay button.  Scale pan window same as main window.
;           21 Mar 2000 RSH
;       Use tracking events to make sure mouse presses/releases don't
;           get mixed up.  Force pan window to have same scaling
;           as main window.  8 May 2000 RSH
;       Change label of xloadct button.  Wset main window before leaving
;           event handler.  17 May 2000 RSH
;       Subscripts surrounded by square brackets instead of
;           parentheses.  Thick=1 added to plots calls.  27 June 2000 RSH
;       Changed FIELDFONT to FIXEDFONT and use it in data listings.
;           Save viewport in info structure.  Emulated events assumed
;           anonymous.  Byte image instead of pixmap used to generate
;           zoom and pan displays to work better with 24 bit.  REDRAW
;           distinct from REFRESH added to support TrueColor visual.
;           TICKLE to force cursor move into main graphics window.
;           ZOOMDRAW modularized.  Sqrt and hist_equal
;           scaling added.  Togglable crosshair in zoom window.
;           XCOLORS (Fanning routine) instead of XLOADCT, to support
;           24 bits.  29 Aug 2000 RSH
;       Fixed bug in SIZE method if no zoom window.  RSH, 31 Aug 2000
;       Clamped cursor X and Y coords at image size.  RSH, 21 Sep 2000
;       Bug in boxcolor when not needed (no zoom or pan).  RSH, 3 Oct 2000
;       Button titles more compact.  SIZE returns box color.  RSH, 5 Oct 2000
;       Zoomlist fixed to work with repeater.  RSH, 6 Oct 2000
;       Zoom window cursor updated via zoom window events.  RSH, 10 Oct 2000
;       Support decomposed=1.  RSH, 10 Oct 2000
;       Grayimg ptr manipulation fixed for nocon, noscale.  RSH, 21 Oct 2000
;       Simplied uvalue search.  Climb hierarchy until an IMGDISPLAY
;          is found.  RSH, 21 Oct 2000
;       Button to toggle zoom window cursor.  RSH, 23 Oct 2000
;       Zoom cursor better in case of tiny images.  RSH, 30 Oct 2000
;       XDISPSTR substituted for XDISPLAYFILE.  RSH, 17 Nov 2000
;       Fixed subwindow bug I'd put in.  RSH, 17 Nov 2000
;       SETDISPLAY added.  Changed XCOLORS handling for TRUECOLOR
;          visual.  RSH, 17 May 2001
;       Unequal X and Y sizes of pan window allowed.  /NOSCALE option
;           for ::draw method.  RSH, 25 Sep 2001
;       Cleaner scrollable windows.  RSH, 26 Sep 2001
;       Gyration to make PostScript work with X visuals.  RSH, 26 Sep 2001
;       !DUMP was removed somewhere along the way.  RSH, 11 Dec 2001
;-

PRO IMGDISPLAY_E, Event

;  Event handler.  Can't be an object routine itself;  main job 
;  is to find out what object the widget is part of, then call the
;  real event handler for that instance.

tmp = event.id
iter = 0
imgd = 0
REPEAT BEGIN
    widget_control, tmp, get_uvalue=uval
    sz = size(uval)
    type = sz[sz[0]+1]
    IF type EQ 11 THEN imgd = obj_isa(uval, 'IMGDISPLAY')
    IF (type NE 11) OR (imgd LE 0) THEN tmp = widget_info(tmp,/parent)
    iter = iter + 1
ENDREP UNTIL (type EQ 11) AND (imgd GT 0)

uval->e, event

RETURN
END


PRO IMGDISPLAY::SIZE, Xsize, Ysize, X_scroll_size, Y_scroll_size, $
                      Pansize, Zoomsize, $
                      ZOOMFACTOR=zoomfactor, $
                      MAINDISP=maindisp, $
                      ZOOMDISP=zoomdisp, $
                      PANDISP=pandisp, $
                      CURZOOM=curzoom, $
                      CURSOR=cursor, $
                      BOXCOLOR=boxcolor

;  Returns size and other parameters to outer world.

xsize = self.xsize
ysize = self.ysize
x_scroll_size = self.x_scroll_size
y_scroll_size = self.y_scroll_size
pansize = self.pansize
maindisp = self.maindisp
zoomdisp = self.zoomdisp
pandisp = self.pandisp
zoomsize = self.zoomsize
boxcolor = self.boxcolor
IF zoomsize GT 0 THEN BEGIN
    zoomfactor = (*self.zoomfactors)[widget_info(self.zoomlist, $
                                         /droplist_sel)]
    curzoom = [self.xzstart, self.xzend, self.yzstart, self.yzend]
ENDIF
cursor = [self.xcursor, self.ycursor]

RETURN
END

PRO IMGDISPLAY::IMAGE, Imgptr

;  Returns pointer to image.

imgptr = self.image
RETURN
END


PRO IMGDISPLAY::E, Event

;  The real event handler for the object, called by the widget
;  event handler.
IF self.dump THEN print,'Entering imgdisplay::e'

imgex = ptr_valid(self.image)
boxcolor=self.boxcolor
widget_control, self.maindisp, get_value=maingraph
zoom_exists = widget_info(self.zoomdisp, /valid)
readout_exists = widget_info(self.xreadout, /valid)
pan_exists = widget_info(self.pandisp, /valid)
IF zoom_exists THEN widget_control, self.zoomdisp, get_value=zoomgraph
IF pan_exists THEN widget_control, self.pandisp, get_value=pangraph
scrollable = self.x_scroll_size LT self.xsize $
         OR  self.y_scroll_size LT self.ysize

delete_widget=0b
process_xy_event=0b
display_popup=0b
do_zoom_draw=0b
do_zoom_maint=0b
do_pan=0b
do_readout=0b
viewport_update=0b
;
;  The event handling is in two phases:  (1) Set flags requiring
;  certain actions and gather information needed.  (2)  Do the
;  work.  This makes it easier to delete the widget cleanly, among
;  other things.
;
CASE event.id OF
    ;
    ;  Change of zoom factor results in updating zoom display.
    self.zoomlist:  BEGIN
        ;  
        ;  For compatibility with repeater object, anything
        ;  that persistently changes widget state must be able
        ;  to come from outside.
        widget_control, self.zoomlist, set_droplist_sel=event.index
        do_zoom_maint=1b
        do_zoom_draw=1b
    END
    ;
    ;  Pushing left mouse button in pan window activates panning.
    ;  Releasing it deactivates panning.  If panning is active,
    ;  a cursor motion event results in updating current X and
    ;  Y coordinates.
    ;
    self.pandisp:  BEGIN
        evname = tag_names(event, /struct)
        CASE evname OF
            'WIDGET_TRACKING':  self.panactive = 0
            ELSE:  BEGIN
                IF self.panactive THEN BEGIN
                    IF event.x GE 0 AND event.x LE (self.pansize[0]-1) $
                        AND event.y GE 0 AND event.y LE (self.pansize[1]-1) THEN BEGIN
                        self.xcursor $
                            = (round(float(event.x)/self.pansize[0]*self.xsize)>0)<(self.xsize-1)
                        self.ycursor $
                            = (round(float(event.y)/self.pansize[1]*self.ysize)>0)<(self.ysize-1)
                        do_pan=1b
                        do_zoom_maint=1b
                        do_zoom_draw=1b
                        do_readout=1b
                    ENDIF
                    IF event.release EQ 1 THEN BEGIN
                        self.panactive = 0
                    ENDIF
                ENDIF ELSE BEGIN
                    IF event.press EQ 1 THEN BEGIN
                        self.panactive = 1
                    ENDIF ELSE BEGIN
                        self.panactive = 0
                    ENDELSE
                ENDELSE
            END
        ENDCASE
    END
    ;
    ;  Switch zoom cross-hair on and off.
    ;
    self.zoomxswitch:  BEGIN
        self.zoomxhair = 1b - self.zoomxhair
        self->zoomdraw
    END
    ;
    ;  Pushing left mouse button in zoom window activates the
    ;  update to current X and Y positions, which ultimately
    ;  results in update of pixel readout.  Releasing mouse button
    ;  stops X & Y update.
    ;
    self.zoomdisp:  BEGIN
        evname = tag_names(event, /struct)
        CASE evname OF
            'WIDGET_TRACKING':  BEGIN
                self.zoomactive = 0
            END
            ELSE:  BEGIN
            IF event.x GE 0 AND event.x LE (self.zoomsize-1) $
                AND event.y GE 0 AND event.y LE (self.zoomsize-1) THEN BEGIN
                zf = (*self.zoomfactors)[widget_info(self.zoomlist, $
                                             /droplist_sel)]
                self.xpos = (round((event.x+0.5)/zf-0.5+self.xzstart) $
                                   > self.xzstart) < self.xzend
                self.ypos = (round((event.y+0.5)/zf-0.5+self.yzstart) $
                                   > self.yzstart) < self.yzend
            ENDIF
            IF self.zoomactive THEN BEGIN
                self.xcursor = self.xpos
                self.ycursor = self.ypos
                do_readout=1b
                do_zoom_draw=1b
                IF event.type EQ 0 OR event.type EQ 1 THEN BEGIN
                    self.zoomactive = 0
                    IF self.xyactive THEN BEGIN
                        IF self.dump THEN print, 'XY branch 1'
                        self.xyactive = 0
                        process_xy_event=1b
                        xyhandler = *(self.xyhandler)
                    ENDIF
                ENDIF
            ENDIF ELSE BEGIN
                IF event.type EQ 0 OR event.type EQ 1 THEN BEGIN
                    self.zoomactive = 1
                    IF self.xyactive THEN BEGIN
                        IF self.dump THEN print, 'XY branch 2'
                        self.xyactive = 0
                        process_xy_event=1b
                        xyhandler = *(self.xyhandler)
                    ENDIF
                ENDIF ELSE BEGIN
                    self.zoomactive = 0
                ENDELSE
            ENDELSE
            END
        ENDCASE
    END
    ;
    ;  Display controls button activates popup to do image scaling,
    ;  color table manipulation, postscript output.
    ;
    self.dbuttonid: display_popup=1b
    ;
    ;  Kill button sets flag to delete widget further down in code.
    ;
    self.killid:  delete_widget=1b
    ;
    ;  Pushing left mouse button in main window activates the
    ;  update to current X and Y positions, which ultimately results
    ;  in update of pixel readout.  Releasing mouse button stops
    ;  X & Y update.  Sliding the scrollbars results in setting the
    ;  viewport_update flag, which ultimately tickles the pan 
    ;  window to move the scrolling box.
    ;
    self.maindisp:  BEGIN
        evname = tag_names(event, /struct)
        CASE evname OF
            'WIDGET_TRACKING':  self.mainactive = 0
            'XCOLORS_LOAD':  self->redraw
            ELSE:  BEGIN
            self.xpos = (event.x>0)<(self.xsize-1)
            self.ypos = (event.y>0)<(self.ysize-1)
            IF event.type EQ 3 THEN BEGIN
                viewport_update = 1b
            ENDIF ELSE BEGIN
                IF self.mainactive THEN BEGIN
                    self.xcursor = self.xpos
                    self.ycursor = self.ypos
                    do_zoom_maint=1b
                    do_zoom_draw=1b
                    do_readout=1b
                    IF event.type EQ 0 OR event.type EQ 1 THEN BEGIN
                        self.mainactive = 0
                        IF self.xyactive THEN BEGIN
                            IF self.dump THEN print, 'XY branch 3'
                            self.xyactive = 0
                            process_xy_event=1b
                            xyhandler = *(self.xyhandler)
                        ENDIF
                    ENDIF
                ENDIF ELSE BEGIN
                    IF event.type EQ 0 OR event.type EQ 1 THEN BEGIN
                        self.mainactive = 1
                        IF self.xyactive THEN BEGIN
                            IF self.dump THEN print, 'XY branch 4'
                            self.xyactive = 0
                            process_xy_event=1b
                            xyhandler = *(self.xyhandler)
                        ENDIF
                    ENDIF ELSE BEGIN
                        self.mainactive = 0
                    ENDELSE
                ENDELSE
            ENDELSE
            END
        ENDCASE
    END
    ELSE:
ENDCASE 

;
;  This is the "doing" part of the program, as opposed to the
;  "knowing" part above.
;
IF delete_widget THEN BEGIN
    ;
    ;  Delete the widget.  Note that the top widget is 
    ;  destroyed, i.e., the whole widget that this display
    ;  is part of.  Make sure all the widgets have cleanup
    ;  routines!
    ;
    ;  Ask user first, if it was set up that way.
    IF self.kill_dialog THEN BEGIN
        answer = dialog_message(['Really exit?'], /question, $
                                title='CONFIRM')
    ENDIF ELSE BEGIN
        answer = 'Yes'
    ENDELSE
    answer = strupcase(strtrim(answer,2)) ; defensive programming
    IF answer EQ 'YES' THEN widget_control, event.top, /destroy
ENDIF ELSE BEGIN     
    ;
    ;  Widget not deleted; go on to actually do stuff.
    ;
    IF imgex THEN BEGIN
        maindisp = self.maindisp
        rep = self.repeat_events
        ;
        ;  This is a hook left over from the non-object version of
        ;  this software.  It is probably a better idea in the world
        ;  of object orientation to use inheritance to add new
        ;  capabilities.  The xyhandler just processes the event
        ;  (whatever window it's in) before the rest of the actions
        ;  called for are taken as well.
        ;
        ;  The xyhandler must be written as a method of the class,
        ;  so that it has access to the data members.
        ;
        IF process_xy_event THEN BEGIN
            call_method, xyhandler, self, event
            IF rep THEN self.xyactive = 1
        ENDIF
        ;
        ;  This block handles motions of the scrollbars on the main
        ;  window.  The viewport motions are transmitted back to the
        ;  pan window so it can update its box cursor.  This is
        ;  done by generating the appropriate fake events and using
        ;  widget_control with send_event.
        ;
        IF viewport_update THEN BEGIN
            geo = widget_info(self.maindisp, /geometry)
            widget_control, self.maindisp, get_draw_view=newview
            IF ptr_valid(self.mainview) THEN ptr_free, self.mainview
            self.mainview = ptr_new(newview)
            ;  Generate new pan cursor
            x0n = round((newview[0]*self.pansize[0])/geo.draw_xsize)>0
            y0n = round((newview[1]*self.pansize[1])/geo.draw_ysize)>0
            x1n = round(((newview[0]+geo.xsize)*self.pansize[0]) $
                  /geo.draw_xsize)<(self.pansize[0]-1)
            y1n = round(((newview[1]+geo.ysize)*self.pansize[1]) $
                  /geo.draw_ysize)<(self.pansize[1]-1)
            xnew = round(0.5*(x0n+x1n))
            ynew = round(0.5*(y0n+y1n))
            event_a = {id:self.pandisp, top:self.imgtop, handler:0L, $
                       type:0, x:xnew, y:ynew, press:1b, $
                       release:0b, clicks:1}
            event_b = {id:self.pandisp, top:self.imgtop, handler:0L, $
                       type:1, x:xnew, y:ynew, press:0b, $
                       release:1b, clicks:1}
            IF widget_info(self.pandisp, /valid) THEN BEGIN
                widget_control, self.pandisp, send_event=event_a
                widget_control, self.pandisp, send_event=event_b
            ENDIF
        ENDIF
        ;  
        ;  This block handles pan window events.  What the user
        ;  sees is a little box outline moving around to represent
        ;  the portion of the whole image shown in the current
        ;  viewport of the main window.
        ;  
        IF scrollable AND do_pan THEN BEGIN
            svwin = !D.window
            geo = widget_info(self.maindisp, /geometry)
            old_pan_exists  = (self.xpstart GE 0) $
                          AND (self.xpend GE self.xpstart) $
                          AND (self.ypstart GE 0) $
                          AND (self.ypend GE self.ypstart)
            IF old_pan_exists THEN BEGIN
                ;  Restore pixels under old pan cursor
                wset, pangraph
                device, copy=[0, 0, $
                              self.xpend-self.xpstart+1, $
                              self.ypend-self.ypstart+1, $
                              self.xpstart, self.ypstart, $
                              self.pan_shad]
            ENDIF
            newview = [(self.xcursor-geo.xsize/2)>0, $
                       (self.ycursor-geo.ysize/2)>0] $
                      < [geo.draw_xsize-geo.xsize, $
                         geo.draw_ysize-geo.ysize]
            widget_control, self.maindisp, set_draw_view=newview
            ;  Generate new pan cursor
            x0n = round((newview[0]*self.pansize[0])/geo.draw_xsize)>0
            y0n = round((newview[1]*self.pansize[1])/geo.draw_ysize)>0
            x1n = round(((newview[0]+geo.xsize)*self.pansize[0]) $
                  /geo.draw_xsize)<(self.pansize[0]-1)
            y1n = round(((newview[1]+geo.ysize)*self.pansize[1]) $
                  /geo.draw_ysize)<(self.pansize[1]-1)
            ;  Save to shadow
            wset, self.pan_shad
            device, copy=[x0n, y0n, x1n-x0n+1, y1n-y0n+1, 0, 0, pangraph]
            ;  Make plot
            wset, pangraph
            plots, [x0n, x1n, x1n, x0n, x0n], $
                   [y0n, y0n, y1n, y1n, y0n], color=boxcolor, $
                   /device, thick=1
            IF svwin GE 0 THEN wset, svwin
            self.xpstart = x0n & self.ypstart = y0n
            self.xpend = x1n & self.ypend = y1n
        ENDIF
        ;
        ;  This block updates the zoom window.  It is updated in
        ;  response either to panning or to moving the cursor around
        ;  in the main window.  The main window shows a moving box
        ;  cursor that outlines the zoomed portion.
        ;
        IF do_zoom_maint AND zoom_exists THEN BEGIN
            ename = tag_names(event, /struct)
            IF (strmid(ename,0,7) EQ 'WIDGET_') OR ename EQ '' THEN BEGIN
                old_zoom_exists = (self.xzstart GE 0) $
                              AND (self.xzend GE self.xzstart) $
                              AND (self.yzstart GE 0) $
                              AND (self.yzend GE self.yzstart)
                IF old_zoom_exists THEN BEGIN
                    svwin = !D.window
                    wset, maingraph
                    device, copy=[0, 0, $
                                  self.xzend-self.xzstart+1, $
                                  self.yzend-self.yzstart+1, $
                                  self.xzstart, self.yzstart, $
                                  self.main_shad]
                    IF svwin GE 0 THEN wset, svwin
                ENDIF
                zf = (*self.zoomfactors)[widget_info(self.zoomlist, $
                                             /droplist_sel)]
                unzoom = floor(float(self.zoomsize)/zf)
                unzoom_half = unzoom/2
                xzstart = (self.xcursor - unzoom_half) > 0
                xzend = (self.xcursor + unzoom_half - 1) < (self.xsize-1)
                yzstart = (self.ycursor - unzoom_half) > 0
                yzend = (self.ycursor + unzoom_half - 1) < (self.ysize-1)
                IF self.xcursor GE self.xsize/2 THEN xzstart = xzend - unzoom + 1 $
                                                ELSE xzend = xzstart + unzoom - 1
                IF self.ycursor GE self.ysize/2 THEN yzstart = yzend - unzoom + 1 $
                                                ELSE yzend = yzstart + unzoom - 1
                xzstart = xzstart > 0
                xzend = xzend < (self.xsize-1)
                yzstart = yzstart > 0
                yzend = yzend < (self.ysize-1)
                self.xzstart=xzstart & self.xzend=xzend
                self.yzstart=yzstart & self.yzend=yzend
                unz_x = xzend-xzstart + 1
                unz_y = yzend-yzstart + 1
                svwin = !D.window
                ;  Box cursor:  Save to shadow
                wset, self.main_shad
                device, copy=[xzstart, yzstart, $
                        xzend-xzstart+1, yzend-yzstart+1, 0, 0, maingraph]
                ;  Make plot
                wset, maingraph
                IF 1b - process_xy_event THEN $
                    plots, [xzstart, xzend, xzend, xzstart, xzstart], $
                           [yzstart, yzstart, yzend, yzend, yzstart], $
                           color=boxcolor, /device, thick=1
                IF svwin GE 0 THEN wset, svwin
            ENDIF
        ENDIF
        IF do_zoom_draw AND zoom_exists THEN self->zoomdraw
        ;
        ;  This block updates the pixel readout.  This is very simple.
        ;  It just uses the current X and Y coordinates that have been
        ;  kept track of above, where keeping track includes correcting 
        ;  for all shrinkage/expansion factors.
        ;
        IF do_readout AND readout_exists AND ptr_valid(self.image) AND $
            self.xcursor GE 0 AND self.ycursor GE 0 THEN BEGIN
            widget_control, self.xreadout, set_value=strn(self.xcursor)
            widget_control, self.yreadout, set_value=strn(self.ycursor)
            sz = size(*self.image) & xsz = sz[1] & ysz = sz[2]
            selfx = (self.xcursor>0)<(xsz-1)
            selfy = (self.ycursor>0)<(ysz-1)
            curpix = (*self.image)[selfx,selfy]
            widget_control, self.preadout, set_value=strn(curpix)
        ENDIF
        IF display_popup THEN self->dpopup
        wset, maingraph
    ENDIF
ENDELSE 

EARLY_EXIT:
IF obj_valid(self) THEN IF self.dump THEN print,'Exiting IMGDISPLAY::e'

RETURN
END


PRO IMGDISPLAY::Tlb, Tlb
;
;  Return to the outside world the ID of top-level base widget 
;  containing the display.
;
next = self.baseid
WHILE next GT 0 DO BEGIN
   tlb = next
   next = widget_info(tlb, /parent)
ENDWHILE
RETURN
END


PRO IMGDISPLAY::ID, Draw_widget_id

;  Gets display subwidget id.
;
;  Input:
;        id = compound widget id
;  Output:
;        draw_widget_id = id of draw subwidget.  This isn't the
;                         same as the graphics window id, which
;                         is the value of the draw subwidget;
;                         the image itself is the value of the
;                         overall compound widget, not of the
;                         draw widget.

draw_widget_id = self.maindisp
RETURN
END

PRO IMGDISPLAY::BASEID, Imgdisplay_id

;  Gets imgdisplay base widget id.

imgdisplay_id = self.baseid
RETURN
END


PRO IMGDISPLAY::GRAB, Array, VERSION=version

;  Grab a pixmap, or the display if version=-1 (default).

IF n_elements(version) NE 1 THEN version=-1

IF version EQ -1 THEN BEGIN
    widget_control, self.maindisp, get_value=newwin
ENDIF ELSE BEGIN
    newwin = self.pixmap[version]
ENDELSE

savewin = !D.window
wset, newwin
device, bypass_translation=1
array = tvrd()
device, bypass_translation=0
IF savewin GE 0 THEN wset, savewin

RETURN
END




PRO IMGDISPLAY::RESIZE, XYTOTAL=xytotal, XYVIEWPORT=xyviewport, $
    SUBWINDOW=subwindow
;+
; NAME:
;       IMGDISPLAY::RESIZE
;
; PURPOSE:
;       This procedure resizes the main draw window.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       obj->RESIZE
;
; INPUT KEYWORD PARAMETERS:
;       XYTOTAL:      2-element vector giving the X and Y size of the
;                     total image area.
;       XYVIEWPORT:   2-element vector giving the X and Y of the
;                     portion actually seen and scrolled.
;       SUBWINDOW:    4-element vector giving portion of total display
;                     actually used to receive data.  Format is
;                     [X0, Y0, NX, NY].
;
; MODIFICATION HISTORY:
;       Written by:     RSH, RITSS, 6 Jan 98
;-

self->id, draw_wid

IF n_elements(xytotal) EQ 2 THEN BEGIN
    IF xytotal[0] GT 0 AND xytotal[1] GT 0 THEN BEGIN 
        widget_control, draw_wid, $
            draw_xsize=xytotal[0], draw_ysize=xytotal[1]
    ENDIF
    IF n_elements(subwindow) NE 4 THEN $
        self.subwindow=[0,0,xytotal]
    self.xsize = xytotal[0]
    self.ysize = xytotal[1]
    IF self.refresh THEN BEGIN
        savewin = !D.window
        FOR i=0, self.nversion-1 DO BEGIN
            wdelete, self.pixmap[i]
            window, /free, /pixmap, $
                xsize=self.subwindow[2], ysize=self.subwindow[3]
            self.pixmap[i] = !D.window
        ENDFOR
        IF savewin GE 0 THEN wset, savewin
    ENDIF
ENDIF

IF n_elements(xyviewport) EQ 2 THEN BEGIN
    IF xyviewport[0] GT 0 AND xyviewport[1] GT 0 THEN BEGIN 
        widget_control, draw_wid, $
            xsize=xyviewport[0], ysize=xyviewport[1]
    ENDIF
    self.x_scroll_size = xyviewport[0]
    self.y_scroll_size = xyviewport[1]
ENDIF

RETURN
END



PRO IMGDISPLAY::XY_SENSITIVE, Xyhandler, REPEAT_EVENTS=rep

;  Sets up a handler for XY cursor events.  RSH, RSTX, 11 Feb. 1998
;  This is left over from the non-object version of this software;
;  in the object-oriented paradigm, it is probably better to use
;  inheritance and define a new class.

;  Input arguments:
;     xyhandler = name of an IDL procedure to handle draw widget
;                 mouse clicks.  The calling sequence must be, e.g.,
;                 clickstuff, event
;                 if xyhandler = 'clickstuff'.  The handler is
;                 explicitly called in IMGDISPLAY::e above.
;
;  Input keyword:
;     /repeat_events:  This allows multiple events to be processed.
;                      The default is that only one event is processed,
;                      then you have to call this routine again to
;                      reinitialize for the next event if desired.
;                 

if self.dump then print,'Entering IMGDISPLAY::xy_sensitive'

;
IF ptr_valid(self.xyhandler) THEN ptr_free, self.xyhandler
self.xyactive = 1
self.xyhandler = ptr_new(xyhandler)
self.repeat_events = keyword_set(rep)

if self.dump then print,'Exiting IMGDISPLAY::xy_sensitive'

return
END




PRO IMGDISPLAY::PSFILE, Psfile, Title

;  Sets the name and title of a Postscript file that may or may
;  not be produced by clicking on the PS file button.
;  Moved data from uvalues to state struct.  RSH, 19 Feb 99

IF self.dump THEN print,'Entering IMGDISPLAY::psfile'
IF ptr_valid(self.psfilename) THEN ptr_free,self.psfilename
IF ptr_valid(self.psfiletitle) THEN ptr_free,self.psfiletitle
self.psfilename = ptr_new(psfile)
self.psfiletitle = ptr_new(title)
IF self.dump THEN print,'Exiting IMGDISPLAY::psfile'

RETURN
END

PRO IMGDISPLAY::REFRESH, SETBUF=setbuf, VERSION=version

;  Refreshes the display (copies pixmap to display).
;
;  /SETBUF does the reverse copy; display to pixmap instead
;  of pixmap to display.

IF self.dump THEN print,'Entering IMGDISPLAY::refresh'

IF n_elements(version) LT 1 THEN version=0

widget_control, self.maindisp, get_value=maingraph

IF keyword_set(setbuf) THEN BEGIN
    wset, self.pixmap[version]
    device, copy=[self.subwindow,0,0,maingraph]
    wset, maingraph
ENDIF ELSE BEGIN
    wset, maingraph
    device, copy=[0,0,self.subwindow[2],self.subwindow[3], $
            self.subwindow[0], self.subwindow[1], $
            self.pixmap[version]]
ENDELSE

IF self.dump THEN print,'Exiting IMGDISPLAY::refresh'

RETURN
END



PRO IMGDISPLAY_CLEANUP, Id

;
;  Widget cleanup routine.  Main job is to find out the associated
;  object and call the cleanup routine for that.

widget_control, id, get_uvalue=this
obj_destroy, this

RETURN
END

PRO IMGDISPLAY::CLEANUP

;  Object cleanup routine, called by widget cleanup routine.
;  26 May 1999 - Don't deallocate image supplied from somewhere else.
;  28 Aug 2000 - Free grayimg.

IF self.dump THEN print,'Entering IMGDISPLAY::cleanup'

IF self.refresh THEN BEGIN
    FOR i=0,self.nversion-1 DO wdelete, self.pixmap[i]
ENDIF

IF self.pan_shad GE 0 THEN wdelete, self.pan_shad
IF self.main_shad GE 0 THEN wdelete, self.main_shad

IF ptr_valid(self.psfilename) THEN ptr_free, self.psfilename
IF ptr_valid(self.psfiletitle) THEN ptr_free, self.psfiletitle
IF ptr_valid(self.xyhandler) THEN ptr_free, self.xyhandler
IF self.last_image_type NE 10 $
    AND ptr_valid(self.image) THEN ptr_free, self.image
IF ptr_valid(self.wedge_array) THEN $
    ptr_free, self.wedge_array
IF ptr_valid(self.zoomfactors) THEN $
    ptr_free, self.zoomfactors
IF ptr_valid(self.grayimg) THEN ptr_free, self.grayimg
IF ptr_valid(self.mainview) THEN ptr_free, self.mainview
IF ptr_valid(self.fixedfont) THEN ptr_free, self.fixedfont

IF self.dump THEN print,'Exiting IMGDISPLAY::cleanup'

RETURN
END


PRO IMGDISPLAY::GETSCALE, Scale, LENGTH=length
;+
; NAME:
;       IMGDISPLAY::GETSCALE
;
; PURPOSE:
;       This procedure returns an array the same length as the color
;       table, telling what data value corresponds to each element.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       obj->GETSCALE, Scale, LENGTH=length
;
; INPUT POSITIONAL PARAMETERS:
;       Id:       Widget ID of CW_SIG_DISPLAY compound widget.
;
; INPUT KEYWORD PARAMETERS:
;       LENGTH:   If you want the returned vector to have a different
;                 length from !D.table_size.
;
; OUTPUT POSITIONAL PARAMETERS:
;       Scale:    Floating point vector giving the data value for
;                 every color.
;
; MODIFICATION HISTORY:
;       Written by:     RSH, RITSS, 6 Jan 98
;       Fixed for bottom and ncolors.  RSH, SSAI, 26 Sep 2001
;-

min_sig = self.lowsigval
max_sig = self.hisigval
log = self.linlogval
sga = self.sigabsval

IF sga THEN BEGIN
    mindata = min_sig
    maxdata = max_sig
ENDIF ELSE BEGIN
    mindata = self.min_other
    maxdata = self.max_other
ENDELSE

IF n_elements(length) LT 1 THEN length = self.ncolors + self.bottom

data = findgen(1000)/999.*(maxdata-mindata)+mindata
IF log THEN bytedata = logscl(data, top=self.ncolors-1) + self.bottom $
       ELSE bytedata = bytscl(data, top=self.ncolors-1) + self.bottom

arg_inp = findgen(length)/(length-1)*(!D.table_size-1)
tabinv, float(bytedata), arg_inp, subs
scale = interpolate(data,subs)

RETURN
END


PRO IMGDISPLAY_V, Id, Image 

;  Stores value for imgdisplay widget; that is to say, displays an image.
;
;  Input arguments:
;      id = widget ID of overall compound widget (not the base
;           widget the next layer up)
;      image = image to be displayed
;

widget_control, id, get_uvalue=this
this->draw, image

RETURN
END

PRO IMGDISPLAY::TICKLE, _EXTRA=extra

;  Move cursor into main graphics window.  To force color table
;  loading in DirectGraphics.  RSH, 29 Aug 2000

widget_control, self.maindisp, get_value=maingraph
wset, maingraph
tvcrs,!D.x_size/2, !D.y_size/2

RETURN
END


PRO IMGDISPLAY::REDRAW, NOOLDZOOM=nooldzoom, _EXTRA=extra

;  Redraw existing image.  Needed for 24-bit mode.  Distinct from
;  refresh, which uses pixmaps.  RSH, 28 Aug 2000

IF self.dump THEN print,'Entering IMGDISPLAY::REDRAW'

IF 1b - ptr_valid(self.image) THEN GOTO, EARLY_EXIT

boxcolor = self.boxcolor

widget_control, self.maindisp, get_value=maingraph
;
;  Do we need to update a panning display?
;
use_pan = 0b
IF widget_info(self.pandisp, /valid) THEN BEGIN
    use_pan = 1b
    widget_control, self.pandisp, get_value=pangraph
    psize = self.pansize
ENDIF
old_pan_exists  = (self.xpstart GE 0) $
              AND (self.xpend GE self.xpstart) $
              AND (self.ypstart GE 0) $
              AND (self.ypend GE self.ypstart)

;
;  Do we need to update a zoom display?
;
use_zoom = 0b
IF widget_info(self.zoomdisp, /valid) THEN BEGIN
    use_zoom = 1b
    widget_control, self.zoomdisp, get_value=zoomgraph
    zf = (*self.zoomfactors)[widget_info(self.zoomlist, $
                                         /droplist_sel)]
    unzoom = floor(float(self.zoomsize)/zf)
ENDIF

IF keyword_set(nooldzoom) THEN BEGIN
    self.xzstart = -1
    self.xzend = -1
    self.yzstart = -1
    self.yzend = -1
ENDIF

old_zoom_exists  = (self.xzstart GE 0) $
              AND (self.xzend GE self.xzstart) $
              AND (self.yzstart GE 0) $
              AND (self.yzend GE self.yzstart)

;
;  How about the density wedge?
;
IF widget_info(self.xreadout, /valid) THEN BEGIN
    widget_control, self.wedge, get_value=wedgegraph
    svwin = !D.window
    wset, wedgegraph
    tv, *self.wedge_array
    IF svwin GE 0 THEN wset, svwin
ENDIF

widget_control, hourglass=1

;
;   In this block, the displaying is actually done.
;
wset, maingraph
tv, *self.grayimg
IF use_pan THEN BEGIN
    svwin = !D.window
    IF ptr_valid(self.grayimg) THEN BEGIN
        wset, pangraph
        tv, byte(round(frebin(*self.grayimg,psize[0],psize[1])))
        ;
        ;  Update panning cursor shadow.
        IF old_pan_exists THEN BEGIN
            svwin = !D.window
            ;  Get new pixels under old pan cursor
            wset, self.pan_shad
            device, copy=[self.xpstart, self.ypstart, $
                          self.xpend-self.xpstart+1, $
                          self.ypend-self.ypstart+1, $
                          0, 0, $
                          pangraph]
            IF svwin GE 0 THEN wset, svwin
        ENDIF
        plots, [self.xpstart, self.xpend, self.xpend, $
                self.xpstart, self.xpstart], $
               [self.ypstart, self.ypstart, self.ypend, $
                self.ypend, self.ypstart], color=boxcolor, $
               /device, thick=1
    ENDIF
    IF svwin GE 0 THEN wset, svwin
ENDIF

;
;   Display zoomed version of new image.
;
IF use_zoom THEN BEGIN
    IF old_zoom_exists THEN BEGIN
        ;  Get new pixels under old zoom cursor
        svwin = !D.window ; defensive programming
        wset, self.main_shad
        device, copy=[self.xzstart, self.yzstart, $
                      self.xzend-self.xzstart+1, $
                      self.yzend-self.yzstart+1, $
                      0, 0, $
                      maingraph]
        self->zoomdraw
        ;  Make plot
        wset, maingraph
        plots, [self.xzstart, self.xzend, self.xzend, self.xzstart, $
               self.xzstart], $
               [self.yzstart, self.yzstart, self.yzend, self.yzend, $
               self.yzstart], $
               color=boxcolor, /device, thick=1
        IF svwin GE 0 THEN wset, svwin
    ENDIF
ENDIF

;
;  If this IMGDISPLAY is set up to be refreshable (i.e., keep
;  track of its own backup pixmaps), then update the pixmaps.
;
IF self.refresh THEN BEGIN
    FOR i=0,self.nversion-1 DO BEGIN
        wset, self.pixmap[i]
        device, copy=[self.subwindow,0,0,maingraph]
    ENDFOR
    wset, maingraph
ENDIF
widget_control, hourglass=0


EARLY_EXIT:

IF self.dump THEN print,'Exiting IMGDISPLAY::REDRAW'

RETURN
END


PRO IMGDISPLAY::ZOOMDRAW
;
;  Display zoomed image
;  RSH, 29 Aug 2000
;  Revisions for small images.  RSH, 30 Oct 2000
;
IF widget_info(self.zoomdisp, /valid) THEN BEGIN

    unz_x = self.xzend-self.xzstart + 1
    unz_y = self.yzend-self.yzstart + 1

    boxcolor = self.boxcolor

    widget_control, self.zoomdisp, get_value=zoomgraph
    zf = (*self.zoomfactors)[widget_info(self.zoomlist, $
                                         /droplist_sel)]
    svwin = !D.window
    IF ptr_valid(self.grayimg) THEN BEGIN
        zoombytes = rebin((*(self.grayimg))[self.xzstart:self.xzend, $
            self.yzstart:self.yzend], unz_x*zf, unz_y*zf, /sample)
        wset, zoomgraph
        tv, zoombytes
    ENDIF
    IF self.zoomxhair THEN BEGIN
        xhair_unz = [self.xcursor - self.xzstart, $
                     self.ycursor - self.yzstart ]
        xhair = xhair_unz*zf + zf/2
        pct = round(0.4*self.zoomsize)
        x_xh_0 = (xhair[0] - pct)>0
        y_xh_0 = (xhair[1] - pct)>0
        x_xh_1 = (xhair[0] + pct)<(unz_x*zf-1)
        y_xh_1 = (xhair[1] + pct)<(unz_y*zf-1)
        plots, [xhair[0],xhair[0]], [y_xh_0,y_xh_1], $
               color=boxcolor,/dev,thick=1
        plots, [x_xh_0,x_xh_1], [xhair[1],xhair[1]], $
               color=boxcolor,/dev,thick=1
    ENDIF
    IF svwin GE 0 THEN wset, svwin
ENDIF
RETURN
END    

PRO IMGDISPLAY::SETDISPLAY, MINVAL=minval, MAXVAL=maxval, $
    LOGARITHMIC=logarithmic, LINEAR=linear, SQUAREROOT=squareroot, $
    HISTEQUAL=histequal, SIGMASCALING=sigmascaling, $
    FLUXSCALING=fluxscaling

;  Set image display scaling parameters.  RSH, 15 May 2001

self.linlogval = 0
self.sigabsval = 1
IF keyword_set(logarithmic) THEN self.linlogval = 1
IF keyword_set(squareroot) THEN self.linlogval = 2
IF keyword_set(histequal) THEN self.linlogval = 3
IF n_elements(minval) GT 0 THEN self.lowsigval = minval[0]
IF n_elements(maxval) GT 0 THEN self.hisigval = maxval[0]
IF n_elements(sigmascaling) GT 0 THEN self.sigabsval = 0

RETURN
END


PRO IMGDISPLAY::DRAW, Image, NOOLDZOOM=nooldzoom, NOSCALE=noscale

;  Display routine for imgdisplay object; called by the widget 
;  value-storing routine.
;
;  /NOOLDZOOM - Don't restore the zoom shadow array.
;  /NOSCALE   - Force TVing the image directly.

IF self.dump THEN print,'Entering IMGDISPLAY::DRAW'

boxcolor = self.boxcolor

widget_control, self.maindisp, get_value=maingraph

imagepar = n_params(0) GT 0
newimage = imagepar AND n_elements(image) GT 0

;
;  Do we need to update a panning display?
;
use_pan = 0b
IF widget_info(self.pandisp, /valid) THEN BEGIN
    use_pan = 1b
    widget_control, self.pandisp, get_value=pangraph
    psize = self.pansize
ENDIF
old_pan_exists  = (self.xpstart GE 0) $
              AND (self.xpend GE self.xpstart) $
              AND (self.ypstart GE 0) $
              AND (self.ypend GE self.ypstart)

;
;  Do we need to update a zoom display?
;
use_zoom = 0b
IF widget_info(self.zoomdisp, /valid) THEN BEGIN
    use_zoom = 1b
    widget_control, self.zoomdisp, get_value=zoomgraph
    zf = (*self.zoomfactors)[widget_info(self.zoomlist, $
                                         /droplist_sel)]
    unzoom = floor(float(self.zoomsize)/zf)
ENDIF

IF keyword_set(nooldzoom) THEN BEGIN
    self.xzstart = -1
    self.xzend = -1
    self.yzstart = -1
    self.yzend = -1
ENDIF

old_zoom_exists  = (self.xzstart GE 0) $
              AND (self.xzend GE self.xzstart) $
              AND (self.yzstart GE 0) $
              AND (self.yzend GE self.yzstart)

;
;  How about the density wedge?
;
IF widget_info(self.xreadout, /valid) THEN BEGIN
    widget_control, self.wedge, get_value=wedgegraph
    svwin = !D.window
    wset, wedgegraph
    tv, *self.wedge_array
    IF svwin GE 0 THEN wset, svwin
ENDIF

;
;  Are we copying our scale parameters from another IMGDISPLAY
;  object used as a template?
;
sct = self.scale_template
use_sct = obj_valid(sct)
nocon = keyword_set(self.nocontrols)

widget_control, hourglass=1

;
;  Update the current image.  If the new image is a pointer, we
;  just copy the pointer.  If it is an array, we move it to heap
;  storage.
;
;  Deallocate image, only if the previous display was from an array
;  rather than a pointer.  If the previous display was from a pointer, 
;  the storage should be managed elsewhere.
IF newimage THEN BEGIN
    ;
    ;  Is the image a pointer or an array?
    ;
    sz = size(image)
    type = sz[sz[0]+1]

    ;
    ;  Is image undefined?  If so, hey, no problem, exit.
    IF type EQ 0 THEN GOTO, EARLY_EXIT
    IF type EQ 10 THEN IF NOT ptr_valid(image) THEN GOTO, EARLY_EXIT


    IF self.last_image_type NE 10 THEN $
        IF ptr_valid(self.image) THEN ptr_free, self.image
    IF type EQ 10 THEN BEGIN   ; If the image argument is a pointer
        self.image = image
    ENDIF ELSE BEGIN           
        ;   In this block, the image arg isn't a pointer, so we
        ;   assume it's a good image array.
        self.image = ptr_new(image)
    ENDELSE
    self.last_image_type = type
ENDIF

;
;  In this block, we figure out if we need scaling parameters
;  then get them if we do need them.
;  
do_scale = 1b     ;  Usually we will scale the image
IF use_sct THEN BEGIN
    ;
    ;  Copy the scaling parameters from another
    ;  IMGDISPLAY object.
    min_sig = sct.lowsigval
    max_sig = sct.hisigval
    linlog = sct.linlogval
    sga = sct.sigabsval
    mno = sct.min_other
    mxo = sct.max_other
    IF sga THEN BEGIN
        mindisp = min_sig
        maxdisp = max_sig
    ENDIF ELSE BEGIN
        mindisp = mno
        maxdisp = mxo
    ENDELSE
ENDIF ELSE BEGIN
    IF nocon THEN BEGIN
        ;  If we have no display controls in existence, and
        ;  no template to be a source of scaling, we just tv 
        ;  the image directly (below).
        do_scale = 0b
    ENDIF ELSE BEGIN
        IF keyword_set(noscale) THEN BEGIN
            ;  Forced using keyword.
            do_scale = 0b
        ENDIF ELSE BEGIN
            ;  Here we are getting the scaling parameters from
            ;  this object itself.
            mindisp = self.lowsigval
            maxdisp = self.hisigval
            linlog = self.linlogval
            sga = self.sigabsval
        ENDELSE
    ENDELSE
ENDELSE

;
;   In this block, the displaying is actually done.
;
IF do_scale THEN BEGIN
    ;
    ;  Scaling
    self->display, maingraph, *self.image, mindisp, maxdisp, info=info, $
                 subwindow=self.subwindow, sctype=linlog, $
                 pixelunits=sga, ncolors=self.ncolors, $
                 min_other=mno, max_other=mxo, $
                 bottom=self.bottom,grayimg=gitmp
    IF ptr_valid(self.grayimg) THEN ptr_free, self.grayimg
    wset, maingraph
    xs = !D.x_size & ys = !D.y_size
    gi = bytarr(xs,ys)
    sw = self.subwindow
    sz = size(gitmp) & xdim = sz[1] & ydim = sz[2]
    xtlim = (sw[2]<(xs-1))<(xdim-1)
    ytlim = (sw[3]<(ys-1))<(ydim-1)
    gi[sw[0],sw[1]] = gitmp[0:xtlim,0:ytlim]
    gitmp = 0
    self.grayimg = ptr_new(gi, /no_copy)
    ;
    ;  We can scale either in image sigma or absolute pixel units.
    ;  Here we keep track of the limits in the "other" units for the
    ;  edification of the user.
    self.min_other = mno
    self.max_other = mxo
    self.lowsigval = mindisp
    self.hisigval = maxdisp
    self.linlogval = linlog
    self.sigabsval = sga
ENDIF ELSE BEGIN
    ;
    ;   No scaling
    IF ptr_valid(self.grayimg) THEN ptr_free, self.grayimg
    gi = byte(*self.image)
    self.grayimg = ptr_new(gi, /no_copy)
    wset, maingraph
    tv, *self.grayimg
ENDELSE
;
;   Display panning version.
;
IF use_pan THEN BEGIN
    svwin = !D.window
    IF ptr_valid(self.grayimg) THEN BEGIN
        wset, pangraph
        tv, byte(round(frebin(*self.grayimg,psize[0],psize[1])))
        ;
        ;  Update panning cursor shadow.
        IF old_pan_exists THEN BEGIN
            svwin = !D.window
            ;  Get new pixels under old pan cursor
            wset, self.pan_shad
            device, copy=[self.xpstart, self.ypstart, $
                          self.xpend-self.xpstart+1, $
                          self.ypend-self.ypstart+1, $
                          0, 0, $
                          pangraph]
            IF svwin GE 0 THEN wset, svwin
        ENDIF
        plots, [self.xpstart, self.xpend, self.xpend, $
                self.xpstart, self.xpstart], $
               [self.ypstart, self.ypstart, self.ypend, $
                self.ypend, self.ypstart], color=boxcolor, $
               /device, thick=1
    ENDIF
    IF svwin GE 0 THEN wset, svwin
ENDIF
;
;   Display zoomed version of new image.
;
IF use_zoom THEN BEGIN
    IF old_zoom_exists THEN BEGIN
        ;  Get new pixels under old zoom cursor
        svwin = !D.window ; defensive programming
        wset, self.main_shad
        device, copy=[self.xzstart, self.yzstart, $
                      self.xzend-self.xzstart+1, $
                      self.yzend-self.yzstart+1, $
                      0, 0, $
                      maingraph]
        self->zoomdraw
        ;  Make plot
        wset, maingraph
        plots, [self.xzstart, self.xzend, self.xzend, self.xzstart, $
               self.xzstart], $
               [self.yzstart, self.yzstart, self.yzend, self.yzend, $
               self.yzstart], $
               color=boxcolor, /device, thick=1
        IF svwin GE 0 THEN wset, svwin
    ENDIF
ENDIF

;
;  If this IMGDISPLAY is set up to be refreshable (i.e., keep
;  track of its own backup pixmaps), then update the pixmaps.
;
IF self.refresh THEN BEGIN
    FOR i=0,self.nversion-1 DO BEGIN
        wset, self.pixmap[i]
        device, copy=[self.subwindow,0,0,maingraph]
    ENDFOR
    wset, maingraph
ENDIF
widget_control, hourglass=0


EARLY_EXIT:
IF self.dump THEN print,'Exiting IMGDISPLAY::DRAW'

RETURN
END


PRO IMGDISPLAY::DISPLAY, Graph, Image, Min_sig, Max_sig, SCTYPE=sctype, $
                 INFO=info, TITLE=title, FILENAME=filename, $
                 GRAYIMG=grayimg, SUBWINDOW=subwindow, $
                 PIXELUNITS=pixelunits, $
                 MIN_OTHER=mno,MAX_OTHER=mxo,NCOLORS=ncolors, $
                 BOTTOM=bottom
;+
; NAME:
;       IMGDISPLAY::DISPLAY
;
; PURPOSE:
;       This procedure does the image scaling for the imgdisplay 
;       object/widget.
;
; CATEGORY:
;       Widgets, image processing
;
; CALLING SEQUENCE:
;       obj->DISPLAY, graph, Image, Min_sig, Max_sig
;
; INPUT POSITIONAL PARAMETERS:
;       Graph:        Graphics window ID for display (NOT the 
;                     draw widget ID)
;       Image:        Image to be displayed
;       IF /pixelunits is set:
;       Min_sig:      Minimum of stretch in image units.
;       Max_sig:      Maximum of stretch in image units.
;       IF pixelunit not set:
;       Min_sig:      Minimum of stretch in std devs above or below
;                     mean.  
;       Max_sig:      Maximum of stretch in std devs above or below
;                     mean.
;
; INPUT KEYWORD PARAMETERS:
;       SCTYPE:       One of 'LINEAR', 'LOG', 'SQRT', 'HIST'.
;                     Default='LINEAR'
;       PS:           Use big_ps to display image to Postscript.  
;                     Default = display to X window 
;       TITLE:        Title of Postscript display.  Default=
;                     'Display Dump'
;       FILENAME:     Filename of Postscript output.  Default=
;                     'sig_display.ps'
;       SUBWINDOW:    [xmin,ymin,xmax,ymax] defining region of display
;                     that is actually to be used.  Default=all.
;       PIXELUNITS:   Set this flag to work in data units rather than
;                     units of std dev.  Default=not set, units
;                     are std dev relative to mean
;       NCOLORS:      Number of byte values to scale to.
;       BOTTOM:       Lowest byte value to scale to.
;
; OUTPUT KEYWORD PARAMETERS:
;       GRAYIMG:      Scaled byte image actually displayed.
;       INFO:      Actual min and max of stretch in data units.
;       MIN_OTHER: Min of stretch in "other" units (sigmas if
;                  /pixelunits set, data units of pixelunits not set)
;       MAX_OTHER: Max of stretch in "other" units.
;
;
; MODIFICATION HISTORY:
;  Bare bones.  RSH, HSTX, 27 Oct. 1997
;  Postscript.  RSH, HSTX, 29 Oct. 1997
;  Zeroes display window before putting up image.  RSH, 3 Apr. 1998
;  3-sigma clip before taking final sigma.  RSH, RSTX, 4 May 1998
;  Redisplay with good notations.  RSH, 4 May 1998
;  Use 1024 random points rather than subimage for sigma.  RSH, 29 Dec 98
;  NCOLORS, BOTTOM.  RSH, 25 Feb 99
;-

if self.dump then print,'Entering imgdisplay::display'
IF n_elements(filename) LT 1 THEN filename='imgdisplay.ps'
IF n_elements(sctype) LT 1 THEN sctype=0
sz = size(image)
xdim = sz[1] & ydim = sz[2]
IF n_elements(subwindow) NE 4 THEN subwindow=[0,0,xdim,ydim]
IF n_elements(ncolors) LT 1 THEN ncolors=!D.table_size
IF n_elements(bottom) LT 1 THEN bottom=0 
nc = ncolors-bottom

nlm = n_elements(image)
irand = (round( (nlm-1)*randomu(seed, nlm<(512L*512L)) ) > 0) $
    < (nlm-1)
imtmp = image[irand]
moment_vec = moment(imtmp, maxmom=2, /nan)
av = moment_vec[0]
stdv = sqrt(moment_vec[1])
w3 = where((imtmp GT (av-3*stdv)) AND (imtmp LT (av+3*stdv)), c3)
IF c3 GT 0 THEN BEGIN
    moment_vec = moment((temporary(imtmp))[w3], maxmom=2, /nan)
    stdv = sqrt(moment_vec[1])
    av = moment_vec[0]
ENDIF ELSE BEGIN
    stdv = 0.5*max(temporary(imtmp))
ENDELSE
IF stdv EQ 0 THEN stdv = 1.0
IF keyword_set(pixelunits) THEN BEGIN
    xmin = min_sig
    xmax = max_sig
    mno = (xmin-av)/stdv
    mxo = (xmax-av)/stdv
ENDIF ELSE BEGIN
    xmin = av+min_sig*stdv
    xmax = av+max_sig*stdv
    mno = xmin
    mxo = xmax
ENDELSE
rev = 1
minpos = 0
IF n_elements(grayimg) LT 1 THEN BEGIN
    CASE sctype OF
        0:  BEGIN
        IF self.dump THEN print,'LINEAR SCALING'
        grayimg = bytscl(image,min=xmin,max=xmax,top=nc-1)+bottom
        END
        1:  BEGIN  
        IF self.dump THEN print,'LOG SCALING'
        grayimg = logscl(image,min=xmin,max=xmax,top=nc-1)+bottom
        END
        2:  BEGIN  
        IF self.dump THEN print,'SQRT SCALING'
        grayimg = sqrtscl(image,min=xmin,max=xmax,top=nc-1)+bottom
        END
        3:  BEGIN  
        IF self.dump THEN print,'HIST SCALING'
        grayimg = histscl(image,min=xmin,max=xmax,top=nc-1)+bottom
        END
    ENDCASE
ENDIF
IF self.dump THEN BEGIN
    print,'MIN(GRAYIMG)=',min(grayimg)
    print,'MAX(GRAYIMG)=',max(grayimg)
    print,'MEAN(GRAYIMG)=',avg(grayimg)
    print,'STDDEV(GRAYIMG)=',stddev(grayimg)
ENDIF
IF keyword_set(ps) THEN BEGIN
    IF keyword_set(pixelunits) THEN tag = ' data units' $
                               ELSE tag = ' sigma'
    scaletitle=strn(min_sig)+' to '+strn(max_sig)+tag
    scaletitle=scaletitle+' ('+sctype+')'
    IF NOT keyword_set(title) THEN title='Display Dump'
    big_ps, grayimg, immin=bottom,immax=ncolors-1, $
      title=title, rev=rev, scaletitle=scaletitle, $
      /nobar,file=filename
ENDIF ELSE BEGIN 
    wset, graph
    blank = bytarr(subwindow[2],subwindow[3])
    tv, blank, subwindow[0], subwindow[1]
    tv, grayimg, subwindow[0], subwindow[1]
ENDELSE 
info = [xmin,xmax]
if self.dump then print,sctype,av,stdv,xmin,xmax,minpos
if self.dump then print,'Exiting imgdisplay::display'
return
END

PRO IMGDISPLAY_DPOPUP_E, Event

;       This procedure is the event handler for image display
;       controls.

widget_control, event.top, get_uvalue=this
this->dpopup_e, event

RETURN
END

PRO IMGDISPLAY::DPOPUP_HELP
helptext=[ $
'Display Controls Popup Widget', $
'  ', $ 
'Summary of capabilities:  ', $
'   (1)  Change image display scaling', $
'   (2)  Change color table', $
'   (3)  Redisplay image', $
'   (4)  Generate PostScript output file', $
' ', $
'Details:', $
' ', $
'   (1)  Change image display scaling', $
' ', $
'        - Change between log, linear, square root, and histogram ', $
'          equalization intensity scaling using buttons at lower left;', $
'          this causes image to be redisplayed.', $
' ', $
'        - Change upper and lower scaling limits by typing them', $
'          into the boxes at the top center and hitting the', $
'          Return (Enter) key on the keyboard.', $
' ', $
'        - Scaling limits can be specified either as a number of sigma', $
'          about the mean value of the image, or directly in image units.', $
'          Which interpretation to give the limits is specified by the ', $
'          buttons underneath the limit boxes.  Note that the statistics', $
'          are computed from a random subset of image pixels and may vary', $
'          somewhat from invocation to invocation.', $
' ', $
'   (2)  Change color table.  Click on the Color/Gray button to invoke', $
'        the IDL program XCOLORS (from Coyote at www.dfanning.com).', $
'        For 8-bit color, update is continuous, whereas for 24-bit color, ', $
'        update is after the choice is made.', $
' ', $
'   (3)  Redisplay image.  Click on the Redisplay button; this works', $
'        whether or not you are changing display parameters.  The same', $
'        effect can be had by clicking in one of the scaling limit boxes', $
'        and hitting the Return (Enter) key.', $
' ', $
'   (4)  Generate PostScript output file.  Click on the PS File button;', $
'        you will be prompted for an output file name.' ]

xdispstr, helptext, title='Help with Display Controls', $
    group=self.dpopupid, font=*self.fixedfont
END

PRO IMGDISPLAY::DPOPUP_E, Event

;
;  The real event handler for the object, called by the
;  event handler for the widget.

IF self.dump THEN print,'Entering IMGDISPLAY::dpopup_e'


;  Routine is in two parts:  (1) figure out the situation,
;  gather data;  (2) do the required actions.

;
;  Do we have scaling controls?
yes_cont = 1b - keyword_set(self.nocontrols)
;
;  Do we want a dialog to choose the PostScript output
;  file name?
ps_dialog = self.ps_dialog

;
;  What sort of facilities have been chosen for the
;  overall IMGDISPLAY object?
zoom_exists = widget_info(self.zoomdisp, /valid)
readout_exists = widget_info(self.xreadout, /valid)
pan_exists = widget_info(self.pandisp, /valid)
widget_control, self.maindisp, get_value=maingraph
image = self.image  ; A pointer, in practice

;
;  If scaling controls exist, get their data.
IF yes_cont THEN BEGIN
    widget_control, self.lowsigid, get_value=min_sig
    widget_control, self.hisigid, get_value=max_sig
    widget_control, self.linlogid, get_value=log
    widget_control, self.sigabsid, get_value=sga
    self.lowsigval = min_sig
    self.hisigval = max_sig
    self.linlogval = log
    self.sigabsval = sga
ENDIF

;
;  Flags for actions to be taken in second part.
redisplay=0b       ; redisplay image
copy_to_ps=0b      ; make a PostScript version
destroy=0b         ; destroy the popup

CASE event.id OF
    self.redisplayid:  redisplay=1b   ; Nothing changed
    self.hisigid:  redisplay=1b    ; High limit changed
    self.lowsigid:  redisplay=1b   ; Low limit changed
    self.sigabsid:  BEGIN
        ;
        ;  Switch between scaling by sigmas and by
        ;  actual pixel data units.
        tmpmn = min_sig
        tmpmx = max_sig
        min_sig = self.min_other
        max_sig = self.max_other
        widget_control, self.lowsigid, set_value=min_sig
        widget_control, self.hisigid, set_value=max_sig
        self.min_other = temporary(tmpmn)
        self.max_other = temporary(tmpmx)
        other_tag = 'data units'
        IF sga THEN other_tag = 'sigma'
        widget_control, self.msgid, set_value = $
            +string(strtrim(string(self.min_other,format='(e10.3)'),2)) $
            + ' - ' +string(strtrim(string(self.max_other, $
            format='(e10.3)'),2)) + ' ' + other_tag       
    END
    ;
    ;  Switch between linear and log
    self.linlogid:  redisplay=1b   
    ;
    ;  Pop up the XCOLORS widget for color tables
    self.xloadctid:   BEGIN
        device, get_visual_name=vn
        vn = strupcase(vn)
        IF vn EQ 'GRAYSCALE' OR vn EQ 'PSEUDOCOLOR' THEN BEGIN
            xcolors,ncolors=self.ncolors,bottom=self.bottom, $
                group=self.dpopupid, /drag
        ENDIF ELSE BEGIN
            IF vn EQ 'DIRECTCOLOR' THEN BEGIN
                xcolors,ncolors=self.ncolors,bottom=self.bottom, $
                    group=self.dpopupid
            ENDIF ELSE BEGIN
                ;receiver = {XCOLORS_NOTIFYOBJ, self, method:'redraw'}
                ;xcolors,ncolors=self.ncolors,bottom=self.bottom, $
                    ;group=self.dpopupid, notifyobj=receiver
                xcolors,ncolors=self.ncolors,bottom=self.bottom, $
                    group=self.dpopupid, $
                    notifyid=[self.maindisp, self.imgtop]
            ENDELSE
        ENDELSE
    END
    ;
    ;  PostScript file desired
    self.psfileid:  BEGIN
        copy_to_ps=1b
    END 
    ;
    ;  Help text
    self.helpid:  BEGIN
        self->dpopup_help
    END
    ;
    ;  Prepare to go away
    self.quitid:  destroy=1b
ENDCASE

sz = size(image)
type = sz[sz[0]+1]
;
;  Type NE 0 means the image is defined (defensive 
;  programming).
IF redisplay AND (type NE 0) THEN BEGIN
    widget_control, hourglass=1
    IF self.dump THEN print, 'maingraph=',maingraph
    ;
    ;  Do the redisplay:
    self->draw
    ;
    ;  Inform the user on scaling results.
    other_tag = 'data units'
    IF self.sigabsval THEN other_tag = 'sigma'
    widget_control, self.msgid, set_value = $
        +string(strtrim(string(self.min_other, $
                        format='(e10.3)'),2)) $
        + ' - ' +string(strtrim(string(self.max_other, $
                                format='(e10.3)'),2)) $
        + ' ' + other_tag
    IF self.refresh THEN BEGIN
        FOR i=0,self.nversion-1 DO BEGIN
            wset, self.pixmap[i]
            device, copy=[self.subwindow,0,0,maingraph]
        ENDFOR
        wset, maingraph
    ENDIF
    widget_control, hourglass=0
ENDIF

;
;  PostScript output.
IF copy_to_ps AND (type NE 0) THEN BEGIN
    widget_control, hourglass=1
    ;
    ;  Set main graphics window.
    wset, maingraph
    ;
    ;  Do we have grayscale colors?
    tvlct, rtst, gtst, btst, /get
    wcolr = where(rtst ne gtst, colr)
    wcolg = where(gtst ne btst, colg)
    IF colr EQ 0 AND colg EQ 0 THEN colkey=0 ELSE colkey=1
    ;
    ;  Do we want a dialog to get the output
    ;  filename?
    IF ps_dialog THEN BEGIN
        IF ptr_valid(self.psfilename) THEN BEGIN
            prev = *self.psfilename
            ptr_free, self.psfilename
        ENDIF ELSE BEGIN
            prev = ''
        ENDELSE
        psfn = dialog_pickfile(file=prev, $
                   group=self.baseid, $
                   title='Specify Output PostScript File', $
                   filter='*.ps', /write)
        self.psfilename = ptr_new(psfn)        
    ENDIF ELSE BEGIN
        psfn = *self.psfilename
    ENDELSE
    ;
    ;  If the filename isn't a null string (CANCEL button
    ;  on dialog_pickfile), the go ahead... almost ...
    IF psfn NE '' THEN BEGIN
        ;
        ;  First, make sure one can write to the selected
        ;  directory (what a pain... a whole 'nother
        ;  "if" level).
        r = execute('openw,lun,psfn,/get_lun')
        IF r THEN BEGIN
            ;
            ;  NOW go ahead.
            free_lun, lun   ; close our fake file
            ;
            ;  There are two hard parts here:
            ;    -- including the pan and zoom windows on the
            ;       output, if they exist
            ;    -- making sure we don't accidentally create a
            ;       graphics window bigger than the screen (a no-no
            ;       in some systems, even for pixmaps)
            wsv = !D.window
            ;
            ;  Get main window geometry
            geo = widget_info(self.maindisp, /geometry)
            ;
            ;  Get main graphics window ID (not same thing as widget ID)
            widget_control, self.maindisp, get_value=win_for_ps
            ;
            ;  Create a pixmap the size of the scrolling area
            window, /free, /pixmap, xsize=geo.xsize, ysize=geo.ysize
            ;
            ;  If we really have a scrolling area, figure out where
            ;  in the image it is; if not, use [0,0]
            IF geo.xsize LT geo.draw_xsize $
              OR geo.ysize LT geo.draw_ysize THEN BEGIN
                widget_control, self.maindisp, get_draw_view=corner
            ENDIF ELSE BEGIN
                corner=[0,0]
            ENDELSE
            ;
            ;  Save pixmap number
            tmp_pixmap = !D.window
            ;
            ;  Copy the scrolling area from the main window into the
            ;  pixmap
            device, copy=[corner,geo.xsize,geo.ysize,0,0,win_for_ps]
            help,/struct,geo
            ;
            ;  Handle the blankety-blank visuals.
            device, get_visual_name=visual
            visual = strupcase(visual)
            ;
            ;  TVRD from the pixmap;  hang onto this byte image for
            ;  later
            IF visual EQ 'PSEUDOCOLOR' THEN BEGIN
                mainbytes = tvrd()
            ENDIF ELSE BEGIN
                mainbytes = tvrd(true=3)
            ENDELSE
            ;
            ;  Delete the pixmap
            wdelete, tmp_pixmap
            ;
            ;  Do we need to worry about a panning window?
            ;  If so, get byte image, get size.
            psz = 0L
            IF pan_exists THEN BEGIN
                widget_control, self.pandisp, $
                    get_value=pangraph
                window, /free, /pixmap, xsize=self.pansize[0],  $
                    ysize=self.pansize[1]
                tmp_pixmap = !D.window
                device, copy=[0,0,self.pansize,0,0,pangraph]
                IF visual EQ 'PSEUDOCOLOR' THEN BEGIN
                    panbytes = tvrd()
                ENDIF ELSE BEGIN
                    panbytes = tvrd(true=3)
                ENDELSE
                psz = self.pansize
                wdelete, tmp_pixmap
            ENDIF
            ;  
            ;  Do we need to worry about a zoom window?
            ;  If so, get byte image, get size.
            zsz = 0L
            IF zoom_exists THEN BEGIN
                widget_control, self.zoomdisp, $
                    get_value=zoomgraph
                window, /free, /pixmap, xsize=self.zoomsize,  $
                    ysize=self.zoomsize
                tmp_pixmap = !D.window
                device, copy=[0,0,self.zoomsize,self.zoomsize,0,0,zoomgraph]
                IF visual EQ 'PSEUDOCOLOR' THEN BEGIN
                    zoombytes = tvrd()
                ENDIF ELSE BEGIN
                    zoombytes = tvrd(true=3)
                ENDELSE
                zsz = self.zoomsize
                wdelete, tmp_pixmap
            ENDIF
            ;
            ;  Figure out size of array needed to tile the main,
            ;  pan, and zoom images together
            xout = ((psz[0] + zsz) > geo.xsize) + 20
            yout = (psz[1] > zsz) + geo.ysize + 50
            ;
            ;  Tile all the byte images we have (from 1 to 3)
            tvlct, rtab, gtab, btab, /get
            IF visual EQ 'PSEUDOCOLOR' THEN BEGIN
                allbytes = bytarr(xout,yout)
                IF pan_exists THEN allbytes[10,30] = panbytes
                IF zoom_exists THEN allbytes[psz[0]+20,30] = zoombytes
                allbytes[10,(psz[1]>zsz)+40] = mainbytes
                allbytes[10,10] = *(self.wedge_array)
            ENDIF ELSE BEGIN
                allbytes = bytarr(xout,yout,3)
                IF pan_exists THEN allbytes[10,30,0] = panbytes
                IF zoom_exists THEN allbytes[psz[0]+20,30,0] = zoombytes
                allbytes[10,(psz[1]>zsz)+40,0] = mainbytes
                allbytes[10,10,0] = rtab[*(self.wedge_array)]
                allbytes[10,10,1] = gtab[*(self.wedge_array)]
                allbytes[10,10,2] = btab[*(self.wedge_array)]
            ENDELSE
            tmp_device = !D.name

            ;
            ;  Get rid of tvlaser, which I cannot control.
            set_plot, 'ps'
            rat = (6.5/float(xout)) < (9.0/float(yout))
            xout_inches = rat*xout & yout_inches = rat*yout
            device, bits_per_pixel=8, filename=psfn, /color
            device, xoffset=1, yoffset=1, xsize=6.5, ysize=9, $
                /inches, /portrait
            IF visual EQ 'PSEUDOCOLOR' THEN BEGIN
                tvlct, rtab, gtab, btab
                tv, allbytes
            ENDIF ELSE BEGIN
                tvlct, bindgen(256), bindgen(256), bindgen(256)
                tv, allbytes, true=3
            ENDELSE
            device,/close
            ;
            ;tvlaser, file=psfn, /noprint, color=colkey, $
                ;title=*self.psfiletitle, bottom=self.bottom, $
                ;ncolors=self.ncolors
            set_plot, tmp_device
            tvlct, rtab, gtab, btab
            ;
            ;  Reset graphics window we had to start with, if valid
            ;  (defensive programming)
            IF wsv GE 0 THEN wset, wsv
            ;
            ;  Notify user in text area of popup
            widget_control, self.msgid, $
                set_value='Wrote ' + strtrim(*self.psfilename)
        ENDIF ELSE BEGIN
            ;
            ;  Bottom half of IF-block where we tried to open a file
            ;  in the selected directory.
            mess = ['Cannot plot to PostScript file', $
                    psfn, $
                    'Wrong directory?']
            junk = dialog_message(mess, /error)
        ENDELSE
    ENDIF

    wset, maingraph
    widget_control, hourglass=0
ENDIF

IF destroy THEN BEGIN
    widget_control, event.top, /destroy
ENDIF

IF self.dump THEN print,'Exiting IMGDISPLAY::dpopup_e'

RETURN
END

PRO IMGDISPLAY::DPOPUP

;       This procedure realizes the display controls popup.
;

IF self.dump THEN print,'Entering IMGDISPLAY::dpopup'

;
;  Only one copy per customer, please.  (Each IMGDISPLAY
;  object can have one copy going.)
IF widget_info(self.dpopupid,/valid_id) THEN BEGIN
    widget_control, self.dpopupid, /show
    IF self.dump THEN print,'Exiting IMGDISPLAY::dpopup'
    RETURN
ENDIF

;
;  Flag to tell whether we need controls as such:  image
;  scaling and color tables, apart from the PostScript button.
yes_cont = 1b-keyword_set(self.nocontrols)

;
;  Does zoom display exist?
yes_zoom = widget_info(self.zoomdisp, /valid)

;
;  Set up top level base and subpanels.  Note that we don't
;  make it modal.
widget_control, self.maindisp, get_value=mw
title = 'CONTROLS (Window '+strn(mw) + ')'
panel = widget_base(col=1,group_leader=self.baseid, title=title)
self.dpopupid=panel
panel_c1 = widget_base(panel,col=1)
panel_c1a = widget_base(panel_c1,col=1)
panel_c1b = widget_base(panel_c1a,col=1,/frame)

;
;  Scaling/display controls are desired:  upper and lower
;  limits, selection of units
IF yes_cont THEN BEGIN
    self.labelid = widget_label(panel_c1b, value='Display Limits')
    self.hisigid = cw_field(panel_c1b, title='Upper:  ', $
                       /float,/return_events, $
                       value=self.hisigval, $
                       fieldfont=*self.fixedfont)
    self.lowsigid = cw_field(panel_c1b, $
                             title='Lower:  ', $
                             /float,/return_events, $
                             value=self.lowsigval,fieldfont=*self.fixedfont)
    self.sigabsid = cw_bgroup(panel_c1b, $
                        ['sigma','data units'], /row, $
                        /exclusive, $
                        set_value=self.sigabsval, /no_release)
ENDIF

;
;  More subpanels.
panel_c23_all = widget_base(panel,col=1)
panel_c23_top = widget_base(panel_c23_all,row=1)
panel_c2 = widget_base(panel_c23_top,col=1,/frame)

;
;  Scaling/display control are desired:  linear/log selection
IF yes_cont THEN BEGIN
    self.linlogid = cw_bgroup(panel_c2, ['linear','log', 'sqrt', 'hist eq'], $
                       /col, label_top='Scaling Type', /exclusive, $
                       set_value=self.linlogval, /no_release)
ENDIF

;  Another subpanel
panel_c3 = widget_base(panel_c23_top,col=1,/frame)
label2a = widget_label(panel_c3,value='Controls')
self.redisplayid = widget_button(panel_c3, value='Redisplay')

;
;  Scaling/display controls are desired:  color table manipulation,
IF yes_cont THEN BEGIN
    self.xloadctid = widget_button(panel_c3, value='Color/Gray')
ENDIF

;
;  If you have the popup at all, you get the PostScript file
;  capability and the popup quit button.
self.psfileid = widget_button(panel_c3, value='PS File')
self.helpid = widget_button(panel_c3, value='Help')
self.msgid = widget_text(panel_c23_all, ysize=1,xsize=40,value=' ')
self.quitid = widget_button(panel_c3,value='DISMISS')

;
;  Text display.
IF yes_cont THEN BEGIN
    other_tag = 'data units'
    IF self.sigabsval THEN other_tag = 'sigma'
    msgstr = string(strtrim(string(self.min_other,format='(e10.3)'),2)) $
                + ' - ' +string(strtrim(string(self.max_other, $
                format='(e10.3)'),2)) + ' ' + other_tag       
    widget_control, self.msgid, set_value=msgstr
ENDIF

xmanager, 'IMGDISPLAY::dpopup', $
    self.dpopupid, event_handler='IMGDISPLAY_dpopup_e', /no_block
widget_control, self.dpopupid, /realize
widget_control, self.dpopupid, set_uvalue=self
IF self.dump THEN print,'Exiting IMGDISPLAY::dpopup'

RETURN
END




FUNCTION IMGDISPLAY::INIT, Parent, TITLE=title, $
                         XSIZE=xsize, YSIZE=ysize, $
                         X_SCROLL_SIZE=x_scroll_size, $
                         Y_SCROLL_SIZE=y_scroll_size, $
                         KILL_BUTTON=kill_button, $
                         FIXEDFONT=fixedfont,REFRESH=refresh, $
                         SUBWINDOW=subwindow,NOCONTROLS=nocontrols, $
                         SCALE_TEMPLATE=scale_template, $
                         NCOLORS=ncolors,BOTTOM=bottom, $
                         PS_DIALOG=ps_dialog,NOBUTTONS=nobuttons, $
                         ZOOMWIN=zoomwin,PANWIN=panwin, $
                         READOUT=readout,VERTICAL=vertical, $
                         BOXCOLOR=boxcolor,KILL_DIALOG=kill_dialog,  $
                         DUMP=dump

;  Make sure device is X-windows.  When this wasn't in, it got the
;  synchronization of moving boxes all messed up, if the device was
;  changed after the fact.

set_plot, 'x'

;  The important routine to generate the display window.
;  See comments at top of file.

yes_buttons = 1b-keyword_set(nobuttons)
yes_panwin = keyword_set(panwin)
yes_zoomwin = keyword_set(zoomwin)
yes_readout = keyword_set(readout)
yes_extras = yes_panwin OR yes_zoomwin OR yes_readout
yes_dump = keyword_set(dump)

IF yes_dump THEN print,'Entering imgdisplay::INIT'

;
;  Find top-level widget
;
above = parent
REPEAT BEGIN
    lastabove = above
    above = widget_info(above, /parent)
ENDREP UNTIL above EQ 0
top = lastabove

imgd_base = widget_base(parent, col=1, $
                      kill_notify='imgdisplay_cleanup', $
                      frame=2)
                      
;
;  The event-generating widgets have to be at the same level so that
;  the event handler can find the state structure.
;
imgd_buttons = widget_base(imgd_base, row=1)
IF keyword_set(title) THEN $
    title = widget_label(imgd_buttons, value=title, /align_left)
dbutton = -1L & kill = -1L

;  Do we want a button to create a popup for image scaling,
;  Postscript output, etc.?

kill_dialog = keyword_set(kill_dialog)
IF yes_buttons THEN BEGIN
    IF keyword_set(kill_button) THEN BEGIN 
        kill = widget_button(imgd_buttons,value='QUIT WIDGET')
    ENDIF 
    dbutton = widget_button(imgd_buttons, value='CONTROLS')
ENDIF

;  Do we want a zoom window?  If so, set up a droplist with
;  choices of zoom factor.

zoomlist = -1L
zoomfactors = [2,5,10,15,20]
IF yes_zoomwin THEN BEGIN
    zoomvalue = 'Zoom '+strtrim(string(zoomfactors,format='(I3)'),2)
    zoomlist = widget_droplist(imgd_buttons,value=zoomvalue)
    widget_control, zoomlist, set_droplist_select=1
ENDIF


;  Do we want to confine our display to part of the main window
;  (to leave room, e.g., for a plot)?

IF n_elements(subwindow) NE 4 THEN $
    subwindow=[0,0,xsize,ysize]

;  Create main window.

IF n_elements(x_scroll_size) LT 1 THEN x_scroll_size = -1L
IF n_elements(y_scroll_size) LT 1 THEN y_scroll_size = -1L
IF x_scroll_size GT 0 AND y_scroll_size GT 0 THEN BEGIN
    scrollable = x_scroll_size LT xsize OR  y_scroll_size LT ysize
ENDIF ELSE BEGIN
    scrollable = 0
ENDELSE
imgd_display = widget_base(imgd_base, row=1)
IF scrollable THEN BEGIN
    maindisp = widget_draw(imgd_display, x_scroll_size=x_scroll_size, $
                       y_scroll_size=y_scroll_size, $
                       xsize=xsize, ysize=ysize, /button_events, $
                       /motion_events, /tracking_events, /scroll)
ENDIF ELSE BEGIN
    maindisp = widget_draw(imgd_display,  $
                       xsize=xsize, ysize=ysize, /button_events, $
                       /motion_events, /tracking_events)
ENDELSE
IF scrollable THEN widget_control, maindisp, /draw_viewport_events

;  Do we want the widget to keep track of pixmaps for refreshing
;  the display?

nversion = 0
pixmap = lonarr(20)
IF keyword_set(refresh) THEN BEGIN
    nversion = round(refresh[0])
    IF nversion GT 20 THEN BEGIN
        message, 'Number of backup pixmaps truncated from '+ $
            strn(nversion) + ' to 20.', /info
        nversion = 20
    ENDIF
    ref=1b
    savewin = !D.window
    FOR i=0, nversion-1 DO BEGIN
        window, /free, /pixmap, xsize=subwindow[2], ysize=subwindow[3]
        pixmap[i] = !D.window
    ENDFOR
    IF savewin GE 0 THEN wset, savewin
ENDIF ELSE BEGIN
    ref=0b
    pixmap = -1L
ENDELSE

rep = 0b

nocon = keyword_set(nocontrols)
IF n_elements(scale_template) LT 1 THEN scale_template = obj_new()

IF n_elements(ncolors) LT 1 THEN ncolors=!D.table_size
IF n_elements(bottom) LT 1 THEN bottom=0
ps_dialog = keyword_set(ps_dialog)

pandisp = -1L
zoomdisp = -1L
zoomxswitch = -1L
pan_shad = -1L
main_shad = -1L

;  Set up "extras":  pan window, zoom window, pixel readout

IF yes_extras THEN BEGIN

    extras_frame = widget_base(imgd_base, row=1)

    IF yes_panwin THEN BEGIN
        IF (n_elements(panwin) GT 1) OR (panwin[0] GT 1) THEN  $
            pansize=panwin ELSE pansize=100
        IF n_elements(pansize) EQ 1 THEN pansize = [pansize, pansize]   
        pandisp = widget_draw(extras_frame, xsize=pansize[0], $
                      ysize=pansize[1], /button_events, /motion_events, $
                      /tracking_events)
        ;
        ;   Set up pixmap to hold cursor shadow - size is upper limit
        ;   - no need to be exact, just hold it under screen size
        ;   for CDE
        svwin = !D.window
        window, /free, /pixmap, xsize=pansize[0], ysize=pansize[1]
        pan_shad = !D.window & IF svwin GE 0 THEN wset, svwin
    ENDIF ELSE BEGIN
        pansize = -1
        pan_shad = -1
    ENDELSE
    IF yes_zoomwin THEN BEGIN
        IF zoomwin GT 1 THEN zoomsize=zoomwin ELSE zoomsize=100
        zoomdisp = widget_draw(extras_frame, xsize=zoomsize, $
                       ysize=zoomsize, /button_events, /motion_events, $
                       /tracking_events)
        ;
        ;  Cross-hair control in zoom window
        zoomxswitch = widget_button(extras_frame, /no_release, $
                      value='+')
        ;
        ;   Set up pixmap to hold cursor shadow
        svwin = !D.window
        xshad = zoomsize & yshad = zoomsize
        window, /free, /pixmap, xsize=xshad, ysize=yshad
        main_shad = !D.window & IF svwin GE 0 THEN wset, svwin
    ENDIF ELSE BEGIN
        zoomsize = -1
        main_shad = -1
    ENDELSE
    boxcolor = -1L
    IF yes_panwin OR yes_zoomwin THEN BEGIN
        IF n_elements(boxcolor) LE 0 THEN boxcolor=255L $
                                     ELSE boxcolor=long(boxcolor[0])
        device, get_decomposed=decomp
        IF decomp THEN boxcolor = boxcolor + 256*boxcolor + 256^2*boxcolor
    ENDIF
    IF yes_readout THEN BEGIN
        IF keyword_set(vertical) THEN thisbase = imgd_base $
                                 ELSE thisbase = extras_frame
        readout_frame = widget_base(thisbase, col=1, frame=2)
        wrow = widget_base(readout_frame, row=1)
        quantity_row = widget_base(readout_frame, row=1)
        label_column = widget_base(quantity_row, col=1)
        value_column = widget_base(quantity_row, col=1)
        xlab = widget_label(label_column, value='X: ')
        ylab = widget_label(label_column, value='Y: ')
        plab = widget_label(label_column, value='Pixel: ')
        xreadout = widget_label(value_column, value=strn(-999.9), xsize=70)
        yreadout = widget_label(value_column, value=strn(-999.9), xsize=70)
        preadout = widget_label(value_column, value=strn(-999.9), xsize=150)
        wedge_array = $
           byte(round(indgen(200)*float(ncolors-1)/199.0)+bottom) $
           # make_array(10, value=1b)
        wedge = widget_draw(wrow, xsize=200, ysize=10)
    ENDIF ELSE BEGIN
        xreadout = -1
        yreadout = -1
        preadout = -1
        wedge_array = -1
        wedge = -1
    ENDELSE
ENDIF ELSE BEGIN
    pansize = -1
    zoomsize = -1
    xreadout = -1
    yreadout = -1
    preadout = -1
    main_shad = -1
    pan_shad = -1
    wedge_array = -1
    wedge = -1
    boxcolor = -1
ENDELSE

IF n_elements(fixedfont) LT 1 THEN $
    fixedfont='-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1'

;  Fill in object data fields.

self.imgtop=top
self.baseid=imgd_base
self.button_frame=imgd_buttons
self.image=ptr_new()
self.last_image_type=-1L
self.xsize=xsize
self.ysize=ysize
self.x_scroll_size=x_scroll_size
self.y_scroll_size=y_scroll_size
self.dbuttonid=dbutton
self.dpopupid=-1L
self.redisplayid=-1L
self.quitid=-1L
self.labelid=-1L
self.lowsigid=-1L
self.hisigid=-1L
self.maindisp=maindisp
self.xloadctid=-1L
self.linlogid=-1L
self.sigabsid=-1L
self.lowsigval=-1.0
self.hisigval=3.0
self.linlogval=0L
self.sigabsval=0L
self.xcursor=-1L
self.ycursor=-1L
self.xpos=-1L
self.ypos=-1L
self.xyactive=0L
self.xyhandler=ptr_new()
self.killid=kill
self.psfileid=-1L
self.helpid=-1L
self.msgid=-1L
self.min_other=-32768.0
self.max_other=-32768.0
self.refresh=ref
self.nversion=nversion
self.pixmap=pixmap
self.subwindow=subwindow
self.repeat_events=rep
self.nocontrols=nocon
self.scale_template=scale_template
self.bottom=bottom
self.ncolors=ncolors
self.ps_dialog=ps_dialog
self.kill_dialog=kill_dialog
self.psfilename = ptr_new('imgdisplay.ps')
self.psfiletitle = ptr_new('IMGDISPLAY Image')
self.pandisp = pandisp
self.pansize = pansize
self.panactive = 0L
self.mainactive = 0L
self.zoomactive = 0L
self.zoomxhair = 1L
self.zoomdisp = zoomdisp
self.zoomxswitch = zoomxswitch
self.zoomsize = zoomsize
self.zoomlist = zoomlist
self.zoomfactors = ptr_new(zoomfactors)
self.xreadout = xreadout
self.yreadout = yreadout
self.preadout = preadout
self.main_shad = main_shad
self.pan_shad = pan_shad
self.boxcolor = boxcolor
self.wedge = wedge
self.wedge_array = ptr_new(wedge_array)
self.fixedfont = ptr_new(fixedfont)
self.dump = yes_dump

IF yes_dump THEN BEGIN
    print,'IMGDISPLAY Base = ',imgd_base
ENDIF

widget_control, imgd_base, set_uvalue=self
widget_control, imgd_base, event_pro='imgdisplay_e', $
  pro_set_value='imgdisplay_v'

IF yes_dump THEN print,'Exiting imgdisplay::INIT'
RETURN,1
END

PRO IMGDISPLAY__DEFINE

;  Defines the imgdisplay object and the state structure contained
;  in it.  Routine name formatted for IDL autodefinition.

resolve_routine, 'xcolors'  ; Because a contained object
                            ; definition may be needed

struct  = {IMGDISPLAY, imgtop:-1L, baseid:-1L, $
         button_frame:-1L, image:ptr_new(), last_image_type:-1L, $
         xsize:0L,ysize:0L,x_scroll_size:0L, y_scroll_size:0L, $
         maindisp:-1L, dbuttonid:-1L, $
         dpopupid:-1L, redisplayid:-1L, quitid:-1L, $
         labelid:-1L, lowsigid:-1L, hisigid:-1L, $
         xloadctid:-1L, linlogid:-1L, sigabsid:-1L, $
         lowsigval:-1.0, hisigval:3.0, $
         linlogval:0L, sigabsval:0L, $
         xcursor:-1L, ycursor:-1L, xpos:-1L, ypos:-1L, xyactive:0L, $
         xyhandler:ptr_new(), killid:-1L, psfileid:-1L, helpid:-1L, $
         psfilename:ptr_new(), psfiletitle:ptr_new(), $
         msgid:-1L, min_other:-32768.0, max_other:-32768.0, $
         grayimg:ptr_new(), refresh:0L, nversion:0L, pixmap:lonarr(20), $
         subwindow:lonarr(4), $
         repeat_events:0L, nocontrols:0L, $
         scale_template:obj_new(), $
         bottom:0L, ncolors:0L, ps_dialog:0L, kill_dialog:0L, $
         pandisp:-1L, pansize:[-1L,-1L], panactive:0L, mainactive:0L, $
         zoomactive:0L, zoomxhair:1L, $
         zoomxswitch:-1L,zoomdisp:-1L, zoomsize:-1L, zoomlist:-1L, $
         zoomfactors:ptr_new(), $
         xzstart:-1L, yzstart:-1L, xzend:-1L, yzend:-1L, $
         xpstart:-1L, ypstart:-1L, xpend:-1L, ypend:-1L, $
         xreadout:-1L, yreadout:-1L, preadout:-1L, $
         main_shad:-1L, pan_shad:-1L, boxcolor:255L, $
         mainview:ptr_new(), $
         wedge:-1L, wedge_array:ptr_new(), fixedfont:ptr_new(), $
         dump:0 }
         
RETURN
END
