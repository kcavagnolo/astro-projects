;-------------------------------------------------------------
;+
; NAME:
;       TAG_VALUE
; PURPOSE:
;       Return the value for a given structure tag.
; CATEGORY:
; CALLING SEQUENCE:
;       val = tag_value(ss, tag)
; INPUTS:
;       ss = given structure.     in
;       tag = given tag.          in
; KEYWORD PARAMETERS:
;       Keywords:
;         ERROR=err Error flag: 0=ok, else tag not found.
;           On error returned value is a null string.
; OUTPUTS:
;       val = returned value.     out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner, 2004 May 05
;
; Copyright (C) 2004, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
	function tag_value, ss, tag, error=err, help=hlp
 
	if (n_params(0) lt 2) or keyword_set(hlp) then begin
	  print,' Return the value for a given structure tag.'
	  print,' val = tag_value(ss, tag)'
	  print,'   ss = given structure.     in'
	  print,'   tag = given tag.          in'
	  print,'   val = returned value.     out'
	  print,' Keywords:'
	  print,'   ERROR=err Error flag: 0=ok, else tag not found.'
	  print,'     On error returned value is a null string.'
	  return,''
	endif
 
	w = where(tag_names(ss) eq strupcase(tag), cnt)
 
	if cnt eq 0 then begin
	  err = 1
	  return,''
	endif
 
	err = 0
	return, ss.(w(0))
 
	end
