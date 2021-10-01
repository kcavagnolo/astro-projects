;+
;========================================================================
;;;
;;; $Id: escape_commas.pro,v 1.2 2008-10-15 19:35:51 cavagnolo Exp $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool replaces all comma in the string name with '\,' to keep
;;; cw_form happy.
;;;
;-

FUNCTION escape_commas, name

chars = byte( ' ' + name )
indexes = where( chars EQ (byte(','))[0], count )
for ii=count-1, 0, -1 do begin
     index = indexes(ii)
     chars = [chars(0:index-1), byte('\'), chars(index:*)] 
endfor

return, strtrim( string(chars), 2 )
end

