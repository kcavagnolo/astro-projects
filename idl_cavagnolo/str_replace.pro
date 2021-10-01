function str_replace, source, insub, outsub 
;
;+
;   Name: str_replace
;
;   Purpose: replace all occurences of a substring with a replacement 
;	     if no replacement string is specified, a blank is inserted 
;
;   Input Parameters:
;      source - source string 
;      insub - target string for replace
;      outsub - replacement string
;
;   History: slf, 11/19/91
;            slf, 19-mar-93		; optimize case where insub and outsub
;					; are each 1 character in length
;
;-
if n_params() eq 2 then outsub=' '	;remove substrings is default
tsource=source				;dont clobber input

; if insub and outsub are both 1 character length, then then a byte replace 
; can be done - slf, 19-mar-1993
if strlen(insub) eq 1 and strlen(outsub) eq 1 then begin
   bsource=byte(tsource)
   binsub=byte(insub)
   boutsub=byte(outsub)
   winsub=where(bsource eq binsub(0),icount)
   if icount gt 0 then bsource(winsub)=boutsub(0)
   newstring=string(bsource)      
endif else begin
   ; slf, find uniq 1 character delimiter (makes str2arr phase much faster)
   delim_list=['%','@','&','+','$','^','#']
   di=-1
   repeat begin
      di=di+1
      arr_delim=delim_list(di)
      tdelim=where(tsource eq arr_delim,dcount)
   endrep until (dcount eq 0) or (di eq n_elements(delim_list)-1)
   if dcount ne 0 then arr_delim = '\\\\'	; last chance, hopefully uniq
   ssource=size(tsource)
   sarray=ssource(0) eq 1			;array operation
   if sarray then tsource=arr2str(tsource,delimit=arr_delim)
   split = str2arr(tsource, delimit=insub)	;make array via delimt insub
   newstring=arr2str(split,delimit=outsub)	;rebuild string via delimit out
   if sarray then newstring=str2arr(newstring,delimit=arr_delim)
endelse

return,newstring
end
