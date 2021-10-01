function str2arr,intext, delim,  array=array, delimit=delimit
;
;+
; NAME:
; 	STR2ARR	
; PURPOSE:
;	Convert delimited string into string array
;
; CALLING SEQUENCE:
;	STR2ARR, text [, delim=delim] 
; INPUTS:
;	TEXT - string, (for example,extracted from SXT generic file)
; OUTPUTS:
;	ARRAY - string array, number elements=number of delimeters+1
; COMMON BLOCKS;
;	NONE
;
; RESTRICTIONS:
; MODIFICATION HISTORY:
;	Version 0 - SLF, 5/9/91
;	slf - feature correction (occur is array, then prob.)
;	slf - 25-feb-92 - added positional delim parameter
;       slf -  2-feb-93 - changed recursion to loop for memory problems
;       slf - 19-mar-93 - optimize case where delimiter is 1 character (comma)
;       slf - 20-mar-93 - fixed a minor bug with major implications
;-
;
;
if n_params() eq 2 then delimit=delim		; slf, 25-feb-92
if not keyword_set(delimit) then delimit=','
delim_len=strlen(delimit)
if n_elements(array) eq 0 then array=''
text=intext(0)					; expect/make scaler
maxlen=strlen(text)

; slf, optimize case where delimiter is 1 character long
if delim_len eq 1 then begin
   bdelim=byte(delimit)
   btext=byte(text)
   wdelim=where(btext eq bdelim(0),dcount)
   if dcount gt 0 then begin
      wdelim=[0,wdelim+1]      
      sizewd=deriv_arr(wdelim)-1
      array=strarr(dcount+1)
      for i=0,dcount-1 do begin
         array(i)=strmid(text,wdelim(i),sizewd(i))
      endfor
      array(i)=strmid(text,wdelim(i),strlen(text)-wdelim(i) )      
   endif else array=text
endif else begin
   occur=strpos(text,delimit)
;
   while occur(0) ne -1 do begin
      substring=strmid(text,0,occur)
      array=[array,substring]
      text=strmid(text,occur+delim_len,maxlen)
      occur=strpos(text,delimit)
   endwhile
   array=[array,text]
   array=array(1:*)
endelse
;
return,array
end 
