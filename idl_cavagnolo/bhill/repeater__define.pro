;+  
;  NAME:
;             REPEATER
;
;  PURPOSE:
;     In object-oriented programming using widgets, REPEATER is a 
;     base class to replicate events generated in one widget and 
;     send them to other widgets that have a similar structure. 
;
;  MOTTO:
;     Object-oriented programming is much easier to do
;     than to describe.
;
;  EXPLANATION:
;     This object is a base class intended to be inherited by
;     other more useful classes that need its functionality.
;
;     This object assumes a programming convention whereby the
;     INFO structure of a widget (see D. Fanning's book) is
;     actually the data portion of an IDL object.  Further, it
;     is assumed that the IDs of widgets whose events you might
;     want to rebroadcast are kept in data elements (structure 
;     tags) of the object.
;
;     The sending and receiving objects must be REPEATERs, by
;     inheritance.  Thus to use REPEATER, you have to define a
;     child class inheriting both from REPEATER and from the other
;     widget/object class you are really interested in.  An
;     example is GANGDISPLAY, which inherits from REPEATER and
;     IMGDISPLAY, and is used in the program CLEANTEST.
;
;  USAGE:
;     Assume you have defined a class called BUNCH_OF_WIDGETS,
;     inheriting from REPEATER, e.g.:
;
;        struct = {bunch_of_widgets, inherits my_fancy_display, $
;                  inherits repeater}
;     (This would be in bunch_of_widgets__define.pro.  You also 
;     have to set up an ::init method for bunch_of_widgets
;     that calles repeater::init properly.  Assume the first 
;     three arguments are the linkage arguments passed to 
;     repeater::init, which see below.)
;
;     Then where you're really using them, you do, e.g.:
;
;        base = widget_base(top_level_base, row=1)
;        bunch1 = obj_new('bunch_of_widgets', base)
;        bunch2 = obj_new('bunch_of_widgets', $
;                          base, bunch1, bunch1)
;        bunch3 = obj_new('bunch_of_widgets', $
;                          base, bunch2, bunch1)
;   
;     In this example, the args (apart from the class name) are
;     parent, prev, and first from repeater::init below.
;     
;     There isn't room here to teach object-oriented programming, but
;     it is hoped that this gives the flavor of using this particular
;     object.
;
;  MODIFICATION HISTORY:
;     Written by R. S. Hill, Raytheon ITSS, 23 April 1999
;     Square brackets around subscripts.  RSH, 1 Aug 2000
;     SET_SENDER procedure added.  RSH, 6 Oct 2000
;     SEND modified to avoid sending if only one object in chain.
;        RSH, 18 Oct 2000
;     INIT modified to fill in unspecified fields with SELF.  RSH, 1 Dec 2000
;-

PRO REPEATER::SEND, Event

;  Send out the replicated events.  These are anonymous,
;  so that the software can potentially tell them from
;  original events.

;  This block only executes if the current object is a sender.
;
IF self.sender THEN BEGIN
    ;
    ;  Fabricate the anonymous event structure.
    ;  
    ;  Get tag names of original event structure
    tn = tag_names(event)
    ntn = n_elements(tn)
    ;
    ;  Build string for execute function.
    exec_str = "event2 = {"
    FOR i=0,ntn-2 DO exec_str = exec_str + tn[i] $
        + ':event.' + tn[i] + ', '
    exec_str = exec_str + tn[ntn-1] + ':event.' + tn[ntn-1] + "}"
    r = execute(exec_str)
    ;
    ;  Now we have a fake event copied from the original.
    ;  
    ;  Next step:  find out which widget in the current object
    ;  sent the event.  THE WIDGET IDS ARE ASSUMED TO BE KEPT
    ;  IN THE OBJECT'S INFO STRUCTURE.
    nf = n_elements(*self.fields)  ; info structure tags containing
                                   ; the IDs of widgets whose events
                                   ; we wish to propagate
    ;
    ;  Linear search for relevant tag name;  assumed to be same tag
    ;  name in the info structure of every recipient and sender.
    i = 0L
    trial_id = -99L
    WHILE i LT nf AND trial_id NE event.id DO BEGIN 
        r = execute( $
            "trial_id = self." + strtrim((*self.fields)[i],2) $
            )
        i = i + 1
    ENDWHILE
    ;
    ;  This block is executed if we found the tag.
    IF trial_id EQ event.id THEN BEGIN
        current_field = (*self.fields)[i-1]
        ;
        ;  Broadcast recipients are in linked list.  The list
        ;  is traversed starting with the next object.  The
        ;  traversal stops when we've come full circle to the
        ;  sender (no echoey event loops wanted!).
        this = self.next
        WHILE obj_valid(this) AND this NE self DO BEGIN
            r = execute( $
                "target_id = this." + current_field $
                )
            event2.id = target_id
            ;event2.top = this.top
            event2.handler = 0
            ;
            ;  Send it!  This statement is our goal.
            widget_control, event2.id, send_event=event2
            this = this.next
        ENDWHILE
    ENDIF
ENDIF

RETURN
END

PRO REPEATER::SET_SENDER

;   Make self the sender

this = self.next
WHILE this NE self DO BEGIN
    this.sender = 0
    this = this.next
ENDWHILE
self.sender = 1

RETURN
END

FUNCTION REPEATER::INIT, Parent, Prev, First, Fields
;
;   Purpose:
;      Fills in the items in the data portion of the repeater.
;
;  Arguments:
;      Parent:   The parent widget
;      Prev:     Object reference.  The previous object in the
;                repeater chain.  May be a non-object dummy
;                value  or nothing at all, if this is the
;                first object in the chain.
;      First:    Object reference.  The first object in the
;                repeater chain.  May be a non-object dummy
;                value  or nothing at all, if *this* is the
;                first object in the chain
;      Fields:   List of fields that this object will use
;                to broadcast, if *it* is the sender.  Must be
;                data fields (structure tags) in the object
;                that inherits from repeater.

IF ptr_valid(self.fields) THEN ptr_free, self.fields
self.fields = ptr_new(fields)

;  Keep track of the top-level base.  Not now used, but
;  might be.
above = parent
REPEAT BEGIN
    lastabove = above
    above = widget_info(above, /parent)
ENDREP UNTIL above EQ 0
self.top = lastabove

;  Fill in linkage fields.  The first in the list starts
;  out being the sender, but the inheriting object can 
;  manipulate this.
IF obj_valid(prev) THEN BEGIN
    self.prev = prev
    prev.next = self
    self.sender = 0
ENDIF ELSE BEGIN
    self.prev = self
    self.sender = 1
ENDELSE

IF obj_valid(first) THEN BEGIN
    self.next = first
    first.prev = self
ENDIF ELSE BEGIN
    self.next = self
ENDELSE

RETURN, 1
END

PRO REPEATER::CLEANUP

;  Clean up pointers on deallocation.

IF ptr_valid(self.fields) THEN ptr_free, self.fields

RETURN
END



PRO REPEATER__DEFINE

;  Defines a repeater object to rebroadcast events to other widgets,
;  disguised as their own.

struct = {REPEATER, top:-1L, prev:obj_new(), next:obj_new(), $ 
    fields:ptr_new(), sender:0L}

END
