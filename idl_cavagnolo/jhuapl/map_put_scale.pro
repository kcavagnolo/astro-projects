;-------------------------------------------------------------
;+
; NAME:
;       MAP_PUT_SCALE
; PURPOSE:
;       Embed map coordinates info in image.
; CATEGORY:
; CALLING SEQUENCE:
;       map_put_scale
; INPUTS:
; KEYWORD PARAMETERS:
;       Keywords:
;         /NOEMBED  do not embed scaling info in image.
;         INFO=info  Returned scaling info byte array.
;         ERROR=err  Error flag: 0=ok.
; OUTPUTS:
; COMMON BLOCKS:
;       map_set2_com
; NOTES:
;       Notes: Must use map_set2 to do the map command.
;         Works just like map_set but keeps track of info
;         needed to restore map projection at a later time
;         and packs it into 160 bytes.  map_put_scale embeds
;         this info in a map image using 160 pixels along the
;         bottom left of the image.  Must protect this info
;         from any processing done to the image.
; MODIFICATION HISTORY:
;       R. Sterner, 2002 Jan 18
;       R. Sterner, 2003 Sep 22 --- Added error flag.
;       R. Sterner, 2004 Jun 21 --- Supported Z buffer.
;
; Copyright (C) 2002, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
	pro map_put_scale, noembed=noembed, info=info, error=err, help=hlp
 
	common map_set2_com, pack
 
	if keyword_set(hlp) then begin
	  print,' Embed map coordinates info in image.'
	  print,' map_put_scale'
	  print,'   All args are keywords.'
	  print,' Keywords:'
	  print,'   /NOEMBED  do not embed scaling info in image.'
	  print,'   INFO=info  Returned scaling info byte array.'
	  print,'   ERROR=err  Error flag: 0=ok.'
	  print,' Notes: Must use map_set2 to do the map command.'
	  print,'   Works just like map_set but keeps track of info'
	  print,'   needed to restore map projection at a later time'
	  print,'   and packs it into 160 bytes.  map_put_scale embeds'
	  print,'   this info in a map image using 160 pixels along the'
	  print,'   bottom left of the image.  Must protect this info'
	  print,'   from any processing done to the image.'
	  return
	endif
 
	;------  Make sure a map_set2 command was done  -----------
	if n_elements(pack) eq 0 then begin
 	  print,' Error in map_put_scale: Map scaling not available.'
	  print,'   Must do a map_set2 command first.'
	  err = 1
	  return
	endif
	err = 0
 
	;----  Insert byte array with map scaling info in image  --------
	if not keyword_set(noembed) then begin
	  if !d.name ne 'Z' then ch=3 else ch=0
;	  tv,pack,0,0,3	; Put in lower left corner, in the blue channel.
	  tv,pack,0,0,ch	; Put in lower left corner, in the blue channel.
	endif
 
	info = pack	; Return scaling array.
 
	return
	end
