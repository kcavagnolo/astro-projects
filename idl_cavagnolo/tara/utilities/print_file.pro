;+
;========================================================================
;;;
;;; Routine to print & delete a file $Id: print_file.pro,v 1.2 2008-10-15 19:35:51 cavagnolo Exp $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;-
PRO print_file, filename

if (n_elements(filename) EQ 0) then filename = '~/idl.ps'

dest = getenv( 'LPDEST' )
if (dest NE '') then begin
  cmd = 'lpr -P ' + dest + ' ' + filename
endif else begin
  cmd = 'lpr ' + filename
endelse

print, cmd
spawn, cmd, result
print, result
file_delete, filename, /QUIET

return
end
