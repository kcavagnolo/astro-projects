
pro psplot,filename,DELETE=delete
;+
; NAME:
;	PSPLOT                        
; PURPOSE:
;       Routine to close and print a postscript plot file.
;       On VMS, will send to the postscript laser printer defined by the
;       logical PSLASER;  if this logical is not defined, then the default
;       is UIT$POSTSCRIPT  In Uinix, routine will check to see if a printer
;	is defined in the environment variable PSPRINTER.  If PSPRINTER is
;	not defined, then default is psuit.
; CALLING SEQUENCE:
;	PSPLOT,[filename]    
; OPTIONAL INPUT:
;	filename - scalar string giving the name of the postscript file to
;                  be printed.    If not supplied, then "idl.ps" is assumed.
;                  A file extension of "ps" is assumed if not supplied
; OPTIONAL KEYWORD INPUT:
;	DELETE - if present and non-zero, the file will be deleted upon
;		 printing.
; OUTPUTS:
;	none
; SIDE EFFECTS:
;	The current device output (presumably postscript) is closed.  Further 
;       graphics output is written to a new version of IDL.PS.   User is 
;       notified when output is complete
; HISTORY:
;	Written by Michael R. Greason, STX, May 1990.
;	Check for environment variable PSPRINTER  J. Offenberg July,1991
;       Added filename parameter  B. Smith, W. Landsman     August, 1991
;	Added DELETE keyword			  J. Offfenberg Feb 1993
;-
On_error,2                                  ;Return to caller

if N_elements(filename) NE 1 then file = 'idl.ps' else begin
   fdecomp,filename,disk,dir,name,ext
   if ext EQ '' then ext = 'ps'
   file = disk+ dir + name + '.'  + ext
endelse

if !D.NAME eq 'PS' then device, /close			; Close file.

psfile = findfile(file,count=nfound)
if nfound EQ 0 then message,'Unable to find postscript file ' + file
if !VERSION.OS NE "vms" then begin
       PSprinter = getenv('PSPRINTER')
 	if (PSprinter eq '') then begin
		PSprinter = 'psuit'
	   endIF
;Create print file command (UNIX).
        IF keyword_set(DELETE) THEN st = 'lpr -r -P'+PSprinter+' '+ file      $
	ELSE st = 'lpr -P'+PSprinter + ' ' + file
       print,'$ '+st
endif $
else begin		
    test = trnlog("PSLASER",pslaser)            ;VMS
    if not test then setlog,"pslaser","uit$postscript" 
    IF keyword_set(DELETE) THEN st='print/queue=pslaser/notify/delete '+file  $
    ELSE st = 'print/queue=pslaser/notify '+file
endelse
spawn,st
return
end

