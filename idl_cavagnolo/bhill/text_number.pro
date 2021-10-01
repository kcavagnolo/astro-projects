PRO TEXT_NUMBER, Textwidget, Num_value, LONGTYPE=longtype, $
                 MINV=minv, MAXV=maxv, FORMAT=format, SET=set
;+
;  NAME:   TEXT_NUMBER
;
;  PURPOSE:
;    This procedure gets a string from a text widget, then converts
;    it to a number, then back to a string, and stores the final result
;    back into the text widget.  The reason for this is so the user
;    can see how the program parsed a number he entered.
;
;  INPUT ARGUMENTS:
;    Textwidget .......... Widget ID of text widget containing number.
;
;  INOUT/OUTPUT ARGUMENTS:
;    Num_value ........... Value of number in text widget.  Zero if
;                          numerical format not valid.  Used as input
;                          if /SET keyword used.
;
;  INPUT KEYWORDS:
;    /LONGTYPE ........... Setting flag causes num_value to be a long
;                          integer; otherwise, it's whatever
;                          strnumber returns.
;    MINV=, MAXV= ........ Num_value is forced into this range.  No
;                          defaults.
;    FORMAT= ............. IDL numerical format.
;    /SET ................ Setting this flag causes Num_value to be
;                          put into the widget.
;
;  MODIFICATION HISTORY:
;    Split off from CLEAN1.PRO.  RSH, RITSS, 5 Oct. 1999
;    Square bracket subscripts.  RSH, RITSS, 28 June 2000
;    FORMAT added.  RSH, RITSS, 29 Aug 2000
;    SET added.  RSH, RITSS, 20 Sep 2000
;-

;
;  Do we want to force a range?
use_min = n_elements(minv) GT 0
use_max = n_elements(maxv) GT 0

;
;  Get string value.
widget_control, textwidget, get_value=text_value

;
;  Convert and test validity.
IF strnumber(text_value[0], num_value) THEN BEGIN
    IF keyword_set(longtype) THEN num_value = round(num_value)
ENDIF ELSE BEGIN
    num_value = 0
ENDELSE

;
;  Force range if desired.
IF use_min THEN num_value = num_value > minv
IF use_max THEN num_value = num_value < maxv

;
;  Store formatted version back into text widget.
IF keyword_set(format) THEN BEGIN
    widget_control, textwidget, set_value=strn(num_value,format=format)
ENDIF ELSE BEGIN
    widget_control, textwidget, set_value=strn(num_value)
ENDELSE

RETURN
END
