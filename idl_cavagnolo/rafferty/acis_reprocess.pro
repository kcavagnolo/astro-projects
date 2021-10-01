pro acis_reprocess,chipid=chipid,turvy=turvy,newbpix=newbpix,ni=ni
;-----------------------------------------------------------------------
;
; Name: ACIS_REPROCESS
;
; Purpose: Takes data archive products and reprocesses them.  Should be 
;	   run from the parent directory of the observation.
;          
;          
; Inputs:  chipid - the id of the chip of interest (default is ccd 7)
;
; Keywords: none
;         
;         
; Comments: Uses Markevitch's method of blank-sky background.  
;           
;           
; Revision history:
;       written by D&L, 2002-12-13
;	updated to use any chip (DR), 2003-06-26
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 0) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'acis_reprocess [, chipid=chipid, /newbpix, /turvy]'
   return   
endif


;
; Set the default chip id = 7
;
if (n_elements(chipid) eq 0) then chipid=7
if (chipid gt 9) then begin
   print, 'The chip number is too large; it should range between 0 and 9.'
   return
endif
  
   
;
; Check for required data archive files
;
flt1file=findfile('secondary/*flt1*',count=num)
if (num eq 0) then begin
   print,'ERROR:'
   print,'FLT1 file not found in secondary directory!'
   return
endif else begin
   print,'Found FLT1 file:',flt1file
endelse

evt1file=findfile('secondary/acisf*evt1*',count=num)
if (num eq 0) then begin
   print,'ERROR:'
   print,'EVT1 file not found in secondary directory!'
   return
endif else begin
   print,'Found EVT1 file:',evt1file
endelse

evt2file=findfile('primary/acisf*evt2*',count=num)
if (num eq 0) then begin
   print,'ERROR:'
   print,'EVT2 file not found in primary directory!'
   return
endif else begin
   print,'Found EVT2 file:',evt2file
endelse

asol1file=findfile('primary/*asol1*',count=num)
if (num eq 0) then begin
   print,'ERROR:'
   print,'ASOL1 file not found in primary directory!'
   return
endif else begin
   print,'Found ASOL1 file(s):',asol1file
endelse


;
; Check for other required files
;
ciaofile=findfile('ciao_info.txt',count=num)
if (num eq 0) then begin
   print,'ERROR:'
   print,'CIAO_INFO.TXT file not found in observation directory!'
   return
endif else begin
   print,'Found CIAO_INFO.TXT file'
endelse
weightsfile=findfile('weights.txt',count=num)
if (num eq 0) then begin
   print,'ERROR:'
   print,'WEIGHTS.TXT file not found in observation directory!'
   return
endif else begin
   print,'Found WEIGHTS.TXT file'
endelse
;if keyword_set(turvy) then begin
;   newcoolfile=findfile('newcool',count=num)
;   if (num eq 0) then begin
;      print,'ERROR:'
;      print,'NEWCOOL file not found in observation directory!'
;      return
;   endif else begin
;      print,'Found NEWCOOL file'
;   endelse
;endif



;
; If all files are found, proceed
;
print,' '
print,'Found all necessary files.'
print,'Beginning reprocessing...'


;
; Unzip any gzipped files
;
print,' '
print,'Unzipping any gzipped files...'
cd,'primary'
cmdstring='gunzip *.gz'
spawn,cmdstring,result
cd,'..'
cd,'secondary'
cmdstring='gunzip *.gz'
spawn,cmdstring,result
cd,'..'
;cd,'supporting'
;cmdstring='gunzip *.gz'
;spawn,cmdstring,result
;cd,'..'
print,'...done'
print,' '


;
; Get general info and check aspect
;
print,'Calling obs_info.pro'
obs_info


;
; Reprocess the evt1 file
;
print,' '  
print,'Calling process_events.pro'
if keyword_set(newbpix) then process_events,/newbpix else process_events 
   

;
; Find correct background file and clean evt2 file
;
print,' '
print,'Calling acis_bg.pro'
if keyword_set(ni) then acis_bg,chipid,/ni else acis_bg,chipid


;
; Print status info to screen
;
print,' '
print,'ACIS_REPROCESS complete.'


;
; Return to IDL
;
return
end