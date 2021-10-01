;+-------------------------------------------------------------------
;  NAME:
; 	betterticklabels
;
;  PURPOSE:
; 	Tick labels are made a little cleaner. E.g., shorter exponentials,
;	no unnecessary trailing zeroes, accurate numbers when tick values
;	are small differences between large numbers, etc.
;
; CATEGORY:
;	Plotting, Graphics
;
;  EXAMPLE:
;	IDL> plot, y, ytickformat='betterticklabels'
;
;  EXAMPLE A - Make a dummy plot call so can determine Tick Max:
;	IDL> COMMON betterticklabels_common, yticklabels
;	IDL> plot, y, ytick_get=yticklabels, ytickname=replicate('      ',30)
;	IDL> AXIS, YAXIS=1, YRANGE=!y.crange, ytickformat='betterticklabels'
;	IDL> dum = TEMPORARY(yticklabels)	; so doesn't affect later calls
;
;  HISTORY:
;	03-Mar-2008 slight mod to handle floating point comparison
;       03-Aug-2006 fixed for values less than 2.e-6
;	15-Apr-04 be able to return xtick values and x tic labels (NOT DEBUGGED)
;	15-Oct-01 shift y axis labels if !p.font=0 (helps with some 3-D plots)
;	11-Jan-01 shift y-axis labels if on TEK and !p.font=0 
;	09-Oct-00 Written by Bill Davis
;--------------------------------------------------------------------
FUNCTION numberXticks	; NOT DEBUGGED
COMMON betterticklabels_common, yticklabels, xticklabels, xTickValues
   return, n_elements( xticklabels )
END

FUNCTION getXtickValues	; NOT DEBUGGED
COMMON betterticklabels_common, yticklabels, xticklabels, xTickValues
   IF N_ELEMENTS( xTickValues ) GT 0 THEN return, xTickValues  $
   ELSE RETURN, REPLICATE(' ', 6)
END


FUNCTION betterticklabels, axis, index, number
	   

COMMON betterticklabels_common, yticklabels, xticklabels, xTickValues
COMMON eot_common2, nDecimalPlaces, lastNumber


   ; was "number eq 0.0" -- may be needed in some cases
if ABS(number) LT 2.0e-6 then begin	; special case
  IF INDEX EQ 0 THEN BEGIN
     nDecimalPlaces = 0
     lastNumber = number
;;;  ENDIF
     if axis EQ 1 then begin
	   ; font=0 with Tektronix device misplaces the y-axis labels
	if !p.font eq 0 and !d.name EQ 'TEK' then RETURN, '0   '  
     endif
     if !p.font eq 0 and axis EQ 1 then RETURN, '0  ' 
     return, '0'
  ENDIF
endif

if number eq -1*!values.F_INFINITY then number = -1.0e30
if number eq !values.F_INFINITY then number = 1.0e30

;;print,'axis,index,number=', axis, index,number

NDECIMALPLACESNOW = 0	

IF axis eq 1 and N_ELEMENTS( yticklabels ) GT 0 THEN BEGIN	; plot was called once to set max
   if ABS(max(yticklabels)) LT .0001 then begin
;;;      tickStr = STRING( yticklabels(index), FORMAT='(G)' )
      tickStr = ExponentStr( axis, index, number)
   endif else if ABS(max(yticklabels)) LT .001 then begin
      tickStr = STRING( yticklabels(index), FORMAT='(f8.4)' )
   endif else if ABS(max(yticklabels)) LT .01 then begin
      tickStr = STRING( yticklabels(index), FORMAT='(f7.3)' )
   endif else if ABS(max(yticklabels)) LT .1 then begin
      tickStr = STRING( yticklabels(index), FORMAT='(f6.2)' )
   endif else if ABS(max(yticklabels)) LT 5 then begin
      tickStr = STRING( yticklabels(index), FORMAT='(f5.1)' )
   endif else if ABS(max(yticklabels)) GT 999999. then begin
         ; need scientific notation, if too big
      tickStr = ExponentStr( axis, index, number)
   endif else begin
      tickStr = STRING( LONG(yticklabels(index)), FORMAT='(I)' )
   endelse
ENDIF ELSE BEGIN

;;;if axis eq 1 then stop
   epsilon = 1.e-6
   IF INDEX EQ 0 THEN BEGIN	; first tick for this axis
      if number EQ 0.0 then begin
	 tickStr = STRING( FIX(number), FORMAT='(I)' )
	 nDecimalPlaces = 0
      endif else if ABS(number) LT 2.e-6 then begin
	 tickStr = '0.0'
	 nDecimalPlaces = 1
      endif else if ABS(number) LT .001 then begin
	 tickStr = STRING( number, FORMAT='(f8.4)' )
	 nDecimalPlaces = 4
      endif else if ABS(number) LT .01 then begin
	 tickStr = STRING( number, FORMAT='(f7.3)' )
	 nDecimalPlaces = 3
      endif else if ABS(number) LT 0.1 then begin
	 tickStr = STRING( number, FORMAT='(f6.2)' )
	 nDecimalPlaces = 2

      endif else if ABS(number) LT 5 and $
                    abs(number-FIX(number)) lt abs(number*epsilon) then begin
	 tickStr = STRING( number, FORMAT='(I)' )
	 nDecimalPlaces = 0

      endif else if ABS(number) LT 5 then begin
	 tickStr = STRING( number, FORMAT='(f5.1)' )
	 nDecimalPlaces = 1
;;;      endif else if ABS(number) GT 99999. then begin
      endif else if ABS(number) GT 999999. then begin
	 tickStr = ExponentStr( axis, index, number)
	 nDecimalPlaces = 5
      endif else begin
	 tickStr = STRING( number, FORMAT='(I)' )
	 nDecimalPlaces = 0
      endelse
;;;stop
      if STRPOS( tickStr, 'E') lt 0 and $
         STRPOS( STRUPCASE(number), 'E') LT 0 then begin
            ; increase # of decimal places until string matches number
         for ndec = nDecimalPlaces+1, 8 do begin
	    if ABS((FLOAT( tickStr ) - number)) GT ABS(number*epsilon) then begin
               tickStr = STRING( number, FORMAT='(G11.'+ STRTRIM(ndec,2) +')' )
                  ; trim trailing zeroes
               while strmid(tickstr,strlen(tickstr)-1,1) eq '0' do $
	            tickStr = strmid( tickStr, 0, strlen(tickstr)-1)
	       ptPos = strpos( tickStr, '.')
               nDecimalPlaces = strlen(tickstr)-ptpos-1
	    endif else goto, stringsMatch
	 endfor
      endif
stringsMatch:
      if STRPOS( STRUPCASE(number), 'E') GE 0 then BEGIN
            tickStr = ExponentStr( axis, index, number)
      ENDIF ELSE if ABS((FLOAT( tickStr ) - number)) GT ABS(number*epsilon) OR $
         nDecimalPlaces GE 5 then BEGIN
;;;         tickStr = STRING( number, FORMAT='(G9.3)' )
         if number lt .001 then begin
	    tickStr = ExponentStr( axis, index, number)
	    nDecimalPlaces = 5
	 endif
      endif

   ENDIF ELSE BEGIN	; subsequent ticks
         ; some cases needed higher tolerance for "close enough"
      if abs(number-lastnumber) gt .0001 then epsilon = 1.e-5
;;;stop  

      if ABS(number) GE 5 THEN thisN = 0 $
;;;      else if  ABS(number) GE .1 THEN thisN = 1 $
      else if  ABS(number) GE .0999999 THEN thisN = 1 $
      else if  ABS(number) GE .009999 THEN thisN = 2 $
      else if  ABS(number) GE .0009999 THEN thisN = 3 $
      else if  ABS(number) GE .00009999 THEN thisN = 4 $
      else if  number EQ 0.0 THEN thisN = 0 $
      else thisN = 5

      if ABS(number) LT 5 then begin
         if ABS(number-FIX(number)) lt ABS(number*epsilon) then thisN=0
      endif

	 ; sometimes very small numbers come in for zero
      if n_elements( lastnumber ) GT 0 then begin
	 if abs(number) lt 1.e-10 and abs(lastnumber) gt 1.e-6 then begin
	    thisN = 0
	 endif
      endif
      
      if ABS(number) GE 999999. THEN thisN = 5
      
      nDecimalPlaces = thisN > nDecimalPlaces
   
      if nDecimalPlaces EQ 5 then begin
;;;	 tickStr = STRING( number, FORMAT='(G9.3)' )
         tickStr = STRING( number, FORMAT='(G11.'+ STRTRIM(nDecimalPlaces,2) +')' )
         if ABS(number) lt .001 then begin
	    tickStr = ExponentStr( axis, index, number)
	 endif
      endif else if nDecimalPlaces EQ 4 then begin
	 tickStr = STRING( number, FORMAT='(f10.4)' )
      endif else if nDecimalPlaces EQ 3 then begin
	 tickStr = STRING( number, FORMAT='(f9.3)' )
      endif else if nDecimalPlaces EQ 2 then begin
	 tickStr = STRING( number, FORMAT='(f8.2)' )
      endif else if nDecimalPlaces EQ 1 then begin
	 tickStr = STRING( number, FORMAT='(f7.1)' )
      endif else begin
	 tickStr = STRING( number, FORMAT='(I)' )
      endelse
      
      if STRPOS( tickStr, 'x') lt 0 then begin
;;;	 if ABS((FLOAT( tickStr ) - number)) GT epsilon then begin
;;;	    print,'using g-format, tickstr=', tickstr
;;;            tickStr = STRING( number, FORMAT='(G9.3)' )
;;;	 endif
;;;stop
            ; increase # of decimal places until string matches number
         for ndec = nDecimalPlaces+1, 8 do begin
;;;print,ndec,tickstr
;;;stop
	    if ABS((FLOAT( tickStr ) - number)) GT ABS(number*epsilon) then begin
;;;	       print,'using g-format, tickstr=', tickstr
;;;               tickStr = STRING( number, FORMAT='(G9.'+ STRTRIM(ndec,2) +')' )
               tickStr = STRING( number, FORMAT='(G21.'+ STRTRIM(ndec,2) +')' )
                  ; trim trailing zeroes
               while strmid(tickstr,strlen(tickstr)-1,1) eq '0' do $
	            tickStr = strmid( tickStr, 0, strlen(tickstr)-1)
	       ptPos = strpos( tickStr, '.')
               nDecimalPlacesNow = strlen(tickstr)-ptpos-1
	    endif else goto, valOK
	 endfor
      endif

   ENDELSE
ENDELSE

valOK:
   ; sometimes very small numbers come in for zero
if n_elements( lastnumber ) GT 0 then begin
   if abs(number) lt 1.e-10 and abs(lastnumber) gt 1.e-6 then begin
      number=0.
      nDecimalPlacesNow = 0
      tickStr = '0'
   endif
endif

nDecimalPlaces = nDecimalPlacesNow > nDecimalPlaces

if STRPOS( STRUPCASE(tickStr), 'E') GE 0 then $	; must have been from G fmt
      tickStr = ExponentStr( axis, index, number)

lastNumber = number

if axis EQ 1 then begin
      ; font=0 with Tektronix device misplaces the y-axis labels
   if !p.font eq 0 and !d.name EQ 'TEK' then RETURN, STRTRIM( tickStr,2 )+'    '  
endif

if axis EQ 0 then begin
   IF INDEX EQ 0 THEN BEGIN	; first tick for this axis
      xticklabels = tickStr
      xTickValues = number
   ENDIF ELSE BEGIN
      if n_elements( xticklabels ) eq 0 then begin
	 xticklabels =  tickStr 
	 xTickValues = number 
      endif else begin
	 xticklabels = [ xticklabels, tickStr ]
	 xTickValues = [ xTickValues, number ]
      endelse
   ENDELSE
endif


if !p.font eq 0 and axis EQ 1 then RETURN, STRTRIM( tickStr,2 )+'  ' 
;;;if !p.font eq 0 and axis EQ 0 then RETURN, '    '+STRTRIM( tickStr,2 )

RETURN, STRTRIM( tickStr,2 )

END
