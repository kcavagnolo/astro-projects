; $Id: gangexamine__define.pro,v 1.1.1.1 2007-02-03 17:10:49 cavagnolo Exp $
;+
;   NAME:
;                 GANGEXAMINE
;
;   PURPOSE:
;      Object-oriented image display widget for synchronized displays
;      permitting interactive examination of the image arrays.
;
;   EXPLANATION:
;      These routines set up an object-oriented widget for synchronized
;      (ganged) image displays.  The object orientation is in the programming
;      method, not the graphics, which are traditional IDL direct graphics.
;
;      For arguments, see the individual routines below, especially the
;      init method.  See also examdisplay__define.pro, and
;      repeater__define.pro, which set up the objects from which this 
;      one inherits directly.
;
;      Using the displays created by this object, the programmer can
;      replicate zooming and panning for two different images.
;      One display is the "sender" and the others are "receivers".
;      Double-clicking in a display window of a receiver turns it into
;      sender.
;
;   INHERITS:
;      examdisplay  - image display and readout widget
;                     (inherits in turn from imgdisplay)
;      repeater     - object to replicate and propagate events 
;                     from one widget to another
;   CATEGORY:
;      Widgets.
;
;   CALLING SEQUENCE FOR INITIALIZATION:
;      new = obj_new('gangexamine', parent, prev, first, keywords=.....)
;
;   ARGUMENTS:
;      Parent:    Widget ID of parent base widget
;      Prev:      Object reference for previous display in chain    
;      First:     Object reference for first display in chain
;
;   KEYWORDS:
;      SYNC:        If set, create a button to propagate display scaling
;                   from sender to other displays.
;      BLINKABLE:   If set, endow displays with blink capability.
;      _EXTRA:      The other keywords needed by EXAMDISPLAY, which see.
;
;
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, Raytheon ITSS, 23 April 1999
;      11 May 1999 - Added scale synchronizing button.  RSH
;      24 May 1999 - Changed call to ::v to call to ::draw.  RSH
;      27 May 1999 - Correct processing of double click
;                    if already sender.  RSH
;      14 Sep 1999 - Call examdisplay cleanup routine.  RSH
;      29 Aug 1999 - Propagate viewport updates.  Blinking.  RSH
;       3 Oct 2000 - Correct _EXTRA handling in INIT method.  
;                    Dummy values for non-existent pan, zoom.  RSH
;       5 Oct 2000 - More compact button format.  RSH
;      15 Nov 2000 - MULTI functions are button menu rather
;                    than droplist.  RSH
;       1 Dec 2000 - Fixed mistake in MULTI drop-menu.  RSH
;      17 May 2001 - Propagate XCOLORS events.  RSH
;      26 Sep 2001 - Compatible with non-square pan windows.  RSH
;-

PRO GANGEXAMINE_BLINK_C, Tlb
widget_control, tlb, get_uvalue=this
this->blink_c

RETURN
END

PRO GANGEXAMINE::BLINK_C
IF (*self.blink_info).zs GT 0 THEN wdelete, (*self.blink_info).zpx
IF (*self.blink_info).ps[0] GT 0 THEN wdelete, (*self.blink_info).ppx
wdelete, (*self.blink_info).mpx
RETURN
END

PRO GANGEXAMINE_BLINK_E, Event

widget_control, event.top, get_uvalue=this
this->blink_e, event

RETURN
END

PRO GANGEXAMINE::BLINK_E, Event

the_sender = (*self.blink_info).the_sender

text_number, (*self.blink_info).txt_delay, delay, format='(F10.2)'

IF event.id EQ (*self.blink_info).txt_delay THEN RETURN
IF event.id EQ (*self.blink_info).bg_sel THEN RETURN

IF widget_info((*self.blink_info).bg_sel, /valid) THEN BEGIN
    widget_control, (*self.blink_info).bg_sel, get_value=sel
ENDIF ELSE BEGIN
    sel = [1,1]
ENDELSE


nsel = n_elements(sel)
widget_control, (*self.blink_info).bg_control, get_value=control
dismiss = 0
cascade = 0

IF control EQ 2 THEN dismiss = 1
IF control EQ 0 THEN cascade = 1
starting = event.id EQ (*self.blink_info).bg_control AND cascade

IF starting THEN BEGIN
    wset, (*self.blink_info).mpx
    device, copy=[0,0,(*self.blink_info).xm, $
        (*self.blink_info).ym,0,0,(*self.blink_info).mg]
    IF (*self.blink_info).zs GT 0 THEN BEGIN
        wset, (*self.blink_info).zpx
        device, copy=[0,0,(*self.blink_info).zs, $
            (*self.blink_info).zs,0,0,(*self.blink_info).zg]
    ENDIF
    IF (*self.blink_info).ps[0] GT 0 THEN BEGIN
        wset, (*self.blink_info).ppx
        device, copy=[0,0,(*self.blink_info).ps[0], $
            (*self.blink_info).ps[1],0,0,(*self.blink_info).pg]
    ENDIF
    (*self.blink_info).going = 1
ENDIF

IF cascade AND (starting OR (event.id EQ event.top $
    AND tag_names(event, /struct) EQ 'WIDGET_TIMER')) THEN BEGIN

    IF widget_info((*self.blink_info).bg_chk, /valid) THEN $
        widget_control, (*self.blink_info).bg_chk, map=1

    this = ((*self.blink_info).current)
    this = this.prev
    it = ((*self.blink_info).current_i - 1) MOD nsel
    rep = 0L
    REPEAT BEGIN
        this = this.next
        it = (it + 1) MOD nsel
        rep = rep + 1
    ENDREP UNTIL sel[it] OR (rep GE nsel)

    IF sel[it] THEN BEGIN
        IF this EQ (*self.blink_info).the_sender THEN BEGIN
            wset, (*self.blink_info).mg
            device, copy=[0,0,(*self.blink_info).xm, $
                (*self.blink_info).ym,0,0,(*self.blink_info).mpx]
            IF (*self.blink_info).zs GT 0 THEN BEGIN
                wset, (*self.blink_info).zg
                device, copy=[0,0,(*self.blink_info).zs, $
                    (*self.blink_info).zs,0,0,(*self.blink_info).zpx]
            ENDIF 
            IF (*self.blink_info).ps[0] GT 0 THEN BEGIN
                wset, (*self.blink_info).pg
                device, copy=[0,0,(*self.blink_info).ps[0],  $
                    (*self.blink_info).ps[1],0,0,(*self.blink_info).ppx]
            ENDIF 
        ENDIF ELSE BEGIN
            widget_control, this.maindisp, get_value=g1
            wset, (*self.blink_info).mg
            device, copy=[0,0,(*self.blink_info).xm, $
                          (*self.blink_info).ym,0,0,g1]
            IF (*self.blink_info).zs GT 0 THEN BEGIN
                widget_control, this.zoomdisp, get_value=g1
                wset, (*self.blink_info).zg 
                device, copy=[0,0,(*self.blink_info).zs, $
                    (*self.blink_info).zs,0,0,g1]
            ENDIF 
            IF (*self.blink_info).ps[0] GT 0 THEN BEGIN
                widget_control, this.pandisp, get_value=g1
                wset, (*self.blink_info).pg 
                device, copy=[0,0,(*self.blink_info).ps[0], $ 
                    (*self.blink_info).ps[1],0,0,g1]
            ENDIF 
        ENDELSE
        IF widget_info((*self.blink_info).bg_chk,/valid) THEN BEGIN
            chk_button = replicate(0, nsel)
            chk_button[it] = 1
            widget_control, (*self.blink_info).bg_chk, $
                set_value=chk_button
        ENDIF
    ENDIF
    (*self.blink_info).current = this.next
    (*self.blink_info).current_i = (it + 1 ) MOD nsel
ENDIF

IF (dismiss OR (control EQ 1)) AND (*self.blink_info).going THEN BEGIN
    wset, (*self.blink_info).mg
    device, copy=[0,0,(*self.blink_info).xm, $
        (*self.blink_info).ym,0,0,(*self.blink_info).mpx]
    IF (*self.blink_info).zs GT 0 THEN BEGIN
        wset, (*self.blink_info).zg
        device, copy=[0,0,(*self.blink_info).zs, $
            (*self.blink_info).zs,0,0,(*self.blink_info).zpx]
    ENDIF 
    IF (*self.blink_info).ps[0] GT 0 THEN BEGIN
        wset, (*self.blink_info).pg
        device, copy=[0,0,(*self.blink_info).ps[0], $
            (*self.blink_info).ps[1],0,0,(*self.blink_info).ppx]
    ENDIF 
    widget_control, (*self.blink_info).top, /clear_events
    IF widget_info((*self.blink_info).bg_chk, /valid) THEN $
        widget_control, (*self.blink_info).bg_chk, map=0
    (*self.blink_info).going = 0
ENDIF

IF dismiss THEN BEGIN
    widget_control, (*self.blink_info).top, /destroy
ENDIF

IF cascade THEN BEGIN
    widget_control, (*self.blink_info).top, timer=delay
ENDIF

RETURN
END

PRO GANGEXAMINE::BLINK
;
;  Do blinking.  
;
IF widget_info(self.blink_control, /valid) THEN BEGIN
    widget_control, self.blink_control, /show
ENDIF ELSE BEGIN
    top = widget_base(col=1,group_leader=self.baseid,title='BLINK')
    row1 = widget_base(top, row=1)
    row2 = widget_base(top, row=1)
    row3 = widget_base(top, row=1)
    lab_delay = widget_label(row1, value='Delay (sec): ')
    txt_delay = widget_text(row1, value='1', /editable, xsize=7)
    ;
    ;  Count displays
    this = self
    count = 0L

    bg_labs = ''
    REPEAT BEGIN
        this = this.next
        IF this.sender THEN the_sender=this
        count = count + 1
        IF this NE self THEN BEGIN
        IF *this.label NE 'Untitled' THEN bg_labs = [bg_labs,*this.label] $
                                     ELSE bg_labs = [bg_labs,strn(count+1)]
        ENDIF
    ENDREP UNTIL this EQ self

    IF *this.label NE 'Untitled' THEN bg_labs[0] = *this.label $
                                 ELSE bg_labs[0] = 'Controller'
    ;
    ;  Selection
    
    bg_sel = cw_bgroup(row2, bg_labs, $
                   /col, label_top='Use Frames', /nonexclusive, $
                   set_value=replicate(1,count), /no_release)
    nlabs = n_elements(bg_labs)
    null_labs = replicate(' ',nlabs)
    bg_chk = cw_bgroup(row2, null_labs, $
                   /col, label_top='Current', /nonexclusive, $
                   set_value=replicate(0,count), /no_release, map=0)

    bg_control = cw_bgroup(row3, ['GO', 'STOP', 'DISMISS'], $
                       set_value=1, /row, /exclusive, /no_release)
    
    widget_control, top, /realize
    widget_control, top, set_uvalue=self
    self.blink_control = top
    xm = the_sender.xsize
    ym = the_sender.ysize
    zs = the_sender.zoomsize
    ps = the_sender.pansize
    widget_control, the_sender.maindisp, get_value=mg
    window, /free, /pixmap, xsize=xm, ysize=ym
    mpx = !D.window
    IF zs GT 0 THEN BEGIN
        widget_control, the_sender.zoomdisp, get_value=zg
        window, /free, /pixmap, xsize=zs, ysize=zs
        zpx = !D.window
    ENDIF ELSE BEGIN
        zg=-1
        zpx=-1
    ENDELSE
    IF ps[0] GT 0 THEN BEGIN
        widget_control, the_sender.pandisp, get_value=pg
        window, /free, /pixmap, xsize=ps[0], ysize=ps[1]
        ppx = !D.window
    ENDIF ELSE BEGIN
        pg=-1
        ppx=-1
    ENDELSE
    info = {top:top, txt_delay:txt_delay, current:the_sender, $
            current_i:0, bg_sel:bg_sel, bg_chk:bg_chk, bg_control:bg_control, $
            the_sender:the_sender, xm:xm, ym:ym, zs:zs, ps:ps, $
            mg:mg, zg:zg, pg:pg, mpx:mpx, zpx:zpx, ppx:ppx, going:0 }
    IF ptr_valid(self.blink_info) THEN ptr_free, self.blink_info
    self.blink_info = ptr_new(info, /no_copy)
    xmanager, 'GANGEXAMINE::blink', $
        top, event_handler='gangexamine_blink_e', $
        cleanup='gangexamine_blink_c', /no_block
ENDELSE
RETURN
END



PRO GANGEXAMINE::E, Event

;
;  Event handling for the GANGEXAMINE object.
;

;
;  What kind of event?  To do what?
;
tn = tag_names(event)
event_name = tag_names(event,/structure_name)
;
;  Find sender
this = self
WHILE 1 - this.sender DO this = this.next
the_sender = this

IF obj_valid(self) THEN BEGIN   ; defensive programming
    ;
    ;   Is it a draw event? 
    draw = event_name EQ 'WIDGET_DRAW'
    track = event_name EQ 'WIDGET_TRACKING'
    IF draw THEN type = event.type ELSE type = -99
    IF type EQ 1 AND self.eat_a_release THEN BEGIN
        ;
        ;   If we had a double-click, we need to catch the
        ;   next release, which will goof things up.
        self.eat_a_release = 0
        repeat_event = 0
    ENDIF ELSE BEGIN
        ;
        ;   "Specials" are for controls that are only in
        ;   GANGEXAMINE, not imgdisplay
        special = event.id EQ self.multi_menu
        ;
        ;   We don't rebroadcast viewport updates, because
        ;   in imgdisplay, they already generate duplicated
        ;   events (panning window motions).  We don't rebroadcast
        ;   doubleclicks because they have the meta-function of
        ;   changing the controlling display.
        repeat_event = 0
        double_click = 0
        IF (1 - track) THEN BEGIN
            IF draw THEN BEGIN
                double_click = event.clicks GE 2
                repeat_event = 1 - double_click
            ENDIF ELSE BEGIN
                repeat_event = 1
            ENDELSE
        ENDIF
        ;
        ;   If double click, make self the sender.
        IF double_click THEN BEGIN
            self->repeater::set_sender
            self.eat_a_release = 1
        ENDIF
        ;
        ;   Process specials.
        IF special THEN BEGIN
            action = event.value
            ;
            ;   Process sync button
            IF action EQ 'MULTI.Same Scale' THEN BEGIN
                ;
                ;  Get display parameters (data units)
                IF the_sender.sigabsval THEN BEGIN
                    lower_limit = the_sender.lowsigval
                    upper_limit = the_sender.hisigval
                ENDIF ELSE BEGIN
                    lower_limit = the_sender.min_other
                    upper_limit = the_sender.max_other
                ENDELSE
                scaling = the_sender.linlogval
                ;
                ;  Change all displays in ring to have those parameters
                this = the_sender.next
                WHILE 1 - this.sender DO BEGIN
                    this.sigabsval = 1
                    this.lowsigval = lower_limit
                    this.hisigval = upper_limit
                    this.linlogval = scaling
                    this->draw
                    this = this.next
                ENDWHILE
            ENDIF
            ;
            ;  Process blink button
            IF action EQ 'MULTI.Blink' THEN BEGIN
                self->blink
            ENDIF

        ENDIF
        ;
        ;   Cascade to the imgdisplay event processing.
        IF 1 - (special OR double_click OR track) THEN BEGIN
            self->examdisplay::e, event
        ENDIF
    ENDELSE
ENDIF

;
;  Viewport events are unique because you can't set the viewport
;  using an event.  XCOLORS events are unique because all TRUECOLOR
;  displays have to be changed no matter which is sender.
IF obj_valid(self) THEN BEGIN
    IF event.id EQ self.maindisp THEN BEGIN
        IF (event_name EQ 'WIDGET_DRAW' AND (1 - track)) THEN BEGIN
            IF event.type EQ 3 AND ptr_valid(the_sender.mainview) THEN BEGIN
                newview = *the_sender.mainview
                ;
                ;  Change all displays in ring to have those parameters
                this = the_sender.next
                WHILE 1 - this.sender DO BEGIN
                    widget_control, this.maindisp, set_draw_view=newview
                    this = this.next
                ENDWHILE
            ENDIF
        ENDIF
        IF event_name EQ 'XCOLORS_LOAD' THEN BEGIN
            this = self.next
            WHILE this NE self DO BEGIN
                this->redraw
                this = this.next
            ENDWHILE
            repeat_event = 0
        ENDIF
    ENDIF
ENDIF

;
;   Make sure we still exist.  If so, are we sender?
sender = 0
IF obj_valid(self) THEN sender = self.sender
;
;   Here's where the events get rebroadcast.  Only "real" events
;   (non-anonymous structures) get propagated.  This is defensive
;   programming to prevent loops.  The underlying repeater object
;   is structured so that only senders send, and they don't 
;   send to themselves, but why not some extra safety in case of
;   a booboo?
;
IF repeat_event AND sender AND event_name NE "" THEN BEGIN
    self->repeater::send, event
ENDIF

RETURN
END


FUNCTION GANGEXAMINE::INIT, Parent, Prev, First, SYNC=sync, $
                            BLINKABLE=blinkable, LABEL=label, $
                            _EXTRA=extra
;  Arguments:
;   Parent:    Widget ID of parent base widget
;   Prev:      Object reference for previous display in chain    
;   First:     Object reference for first display in chain
;
;  Keywords:
;   SYNC:        If set, create a button to propagate display scaling
;                from sender to other displays.
;   BLINKABLE:   If set, endow displays with blink capability.
;   LABEL:       A (short) ID for the display.
;   _EXTRA:      The other keywords needed by IMGDISPLAY, which see.
;

IF n_elements(label) GE 1 THEN self.label = ptr_new(label) $
                          ELSE self.label = ptr_new('Untitled')

;
;  Events from the widgets identified in these IMGDISPLAY tags will
;  be propagated along the display chain.
fields = ['zoomlist', 'pandisp', 'zoomdisp', 'killid', $
          'maindisp', 'zoomxswitch']

ret = self->repeater::init(parent, prev, first, fields)
IF self.sender THEN BEGIN
    ret = self->examdisplay::init(Parent, _extra=extra)
ENDIF ELSE BEGIN
    ;
    ;  No kill button on any display except the first, which
    ;  is the initial sender
    ret = self->examdisplay::init(Parent, kill_button=0, _extra=extra)
ENDELSE

IF keyword_set(blinkable) AND keyword_set(sync) THEN $
    menu_desc = ['1\MULTI','0\Blink','2\Same Scale']
IF keyword_set(blinkable) AND (NOT keyword_set(sync)) THEN $
    menu_desc = ['1\MULTI','2\Blink']
IF (NOT keyword_set(blinkable)) AND keyword_set(sync) THEN $
    menu_desc = ['1\MULTI','2\Same Scale']

IF keyword_set(blinkable) OR keyword_set(sync) THEN $
    self.multi_menu = cw_pdmenu(self.button_frame, menu_desc, $
                                /return_full_name)

RETURN, ret
END

PRO GANGEXAMINE::CLEANUP

;  Stop nicely when quitting from widget.

self->repeater::cleanup
self->examdisplay::cleanup

IF ptr_valid(self.blink_info) THEN BEGIN
    IF widget_info((*self.blink_info).top, /valid) THEN $
        widget_control, (*self.blink_info).top, /destroy
    ptr_free, self.blink_info
ENDIF
IF ptr_valid(self.label)      THEN ptr_free, self.label

RETURN
END


PRO GANGEXAMINE__DEFINE

;  Just gets the object definition into the IDL session.

struct = {GANGEXAMINE, multi_menu:-1L, $
          blink_control:-1L, blink_flag:1, blink_info:ptr_new(), $
          label:ptr_new(), eat_a_release:0b, $
          inherits EXAMDISPLAY, inherits REPEATER}

RETURN
END
