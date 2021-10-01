;-------------------------------------------------------------
;+
; NAME:
;       AARR
; PURPOSE:
;       Simulate an associative array (key / value pairs).
; CATEGORY:
; CALLING SEQUENCE:
;       val = aarr(arr, key)
; INPUTS:
;       arr = Input array.                   in
;             See notes below.
;       key = Key into array.                in
; KEYWORD PARAMETERS:
;       Keywords:
;         VALUE=val  If this then the corresponding key
;           is returned as the function value.
;         /ADD add a new key/value pair to the array.
;            Give both the key and value with this keyword:
;            arr2 = aarr(arr,key,value=val,/add)
;            Array arr must exist to start.
;            The updated array is returned or original if error.
;         /DROP drop the specified key/value pair from the array.
;            Give either the key or value with this keyword:
;            arr = aarr(arr,key,/drop) or
;            arr = aarr(arr,value=val,/drop)
;            The updated array is returned or original if error.
;         COUNT=cnt Number of elements in modified array (Only
;            returned for /DROP).  If all elements dropped a
;            null string is returned, check cnt.
; OUTPUTS:
;       val = Returned value for given key.  out
;         Reverse lookup, do not give key for this case.
; COMMON BLOCKS:
; NOTES:
;       Notes: The keys and values for a given array will all be the
;       same data type, probably text.  They must be in the array
;       as adjacent elements with key first.  Keys should be unique,
;       if not, the first found will be used.
; MODIFICATION HISTORY:
;       R. Sterner, 2006 Jul 06
;
; Copyright (C) 2006, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
	function aarr, arr, key, value=val, $
	  add=add, drop=drop, count=cnt, help=hlp
 
	if (n_params(0) lt 1) or keyword_set(hlp) then begin
	  print,' Simulate an associative array (key / value pairs).'
	  print,' val = aarr(arr, key)'
	  print,'   arr = Input array.                   in'
	  print,'         See notes below.'
	  print,'   key = Key into array.                in'
	  print,'   val = Returned value for given key.  out'
	  print,'     Reverse lookup, do not give key for this case.'
	  print,' Keywords:'
	  print,'   VALUE=val  If this then the corresponding key'
	  print,'     is returned as the function value.'
	  print,'   /ADD add a new key/value pair to the array.'
	  print,'      Give both the key and value with this keyword:'
	  print,'      arr2 = aarr(arr,key,value=val,/add)'
	  print,'      Array arr must exist to start.'
	  print,'      The updated array is returned or original if error.'
	  print,'   /DROP drop the specified key/value pair from the array.'
	  print,'      Give either the key or value with this keyword:'
	  print,'      arr = aarr(arr,key,/drop) or'
	  print,'      arr = aarr(arr,value=val,/drop)'
	  print,'      The updated array is returned or original if error.'
	  print,'   COUNT=cnt Number of elements in modified array (Only'
	  print,'      returned for /DROP).  If all elements dropped a'
	  print,'      null string is returned, check cnt.'
	  print,' Notes: The keys and values for a given array will all be the'
	  print,' same data type, probably text.  They must be in the array'
	  print,' as adjacent elements with key first.  Keys should be unique,'
	  print,' if not, the first found will be used.'
	  return, ''
	endif
 
	;-------------------------------------------------
	;  Add
	;-------------------------------------------------
	if keyword_set(add) then begin
	  if n_elements(key) eq 0 then begin
	    print,' Error in aarr: Must give a key for /ADD.'
	    return,arr				; No change.
	  endif
	  if n_elements(val) eq 0 then begin
	    print,' Error in aarr: Must give a value for /ADD.'
	    return,arr				; No change.
	  endif
	  return,[arr,key,val]
	endif
 
	;-------------------------------------------------
	;  Drop
	;-------------------------------------------------
	if keyword_set(drop) then begin
	  flag = bytarr(n_elements(arr)) + 1B	; Flag array, all 1s.
	  ;---  KEY  ---
	  if n_elements(key) ne 0 then begin	; Key was given.
	    w = where(arr eq key, cnt)		; Find it in array.
	    if cnt eq 0 then begin		; Not there, error.
	      print,' Error in aarr: Given key not found'
	      print,'   Key = ',key
	      return, arr			; No change.
	    endif
	    i = w(0)				; Key found in arr at index i.
	    flag([i,i+1]) = 0			; Clear flags for dropped pair.
	  endif
	  ;---  VAL  ---
	  if n_elements(val) ne 0 then begin
	    w = where(arr eq val, cnt)		; Find it in array.
	    if cnt eq 0 then begin		; Not there, error.
	      print,' Error in aarr: Given value not found'
	      print,'   Val = ',val
	      return, arr			; No change.
	    endif
	    i = w(0)				; Val found in arr at index i.
	    flag([i,i-1]) = 0			; Clear flags for dropped pair.
	  endif
	  in = where(flag, cnt)			; Find all uncleared flags.
	  if cnt eq 0 then return,''		; None, return null string.
	  return, arr(in)			; Returned array subset.
	endif
 
	;-------------------------------------------------
	;  Given Key, return Value
	;-------------------------------------------------
	if n_elements(key) ne 0 then begin	; Key was given.
	  w = where(arr eq key, cnt)		; Find it in array.
	  if cnt eq 0 then begin		; Not there, error.
	    print,' Error in aarr: Given key not found'
	    print,'   Key = ',key
	    return, arr				; No change.
	  endif
	  i = w(0)				; Key found in arr at index i.
	  return, arr(i+1)			; Return value.
	endif
 
	;-------------------------------------------------
	;  Given Value, return Key
	;-------------------------------------------------
	if n_elements(val) ne 0 then begin
	  w = where(arr eq val, cnt)		; Find it in array.
	  if cnt eq 0 then begin		; Not there, error.
	    print,' Error in aarr: Given value not found'
	    print,'   Val = ',val
	    return, arr				; No change.
	  endif
	  i = w(0)				; Val found in arr at index i.
	  return, arr(i-1)			; Return key.
	endif
 
	end
