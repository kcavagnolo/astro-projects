;+
; Project     : SOHO - CDS
;
; Name        : RM_FILE
;
; Purpose     : delete a file in OS independent way
;
; Category    : OS
;
; Explanation : uses openr,/del
;
; Syntax      : IDL> rm_file,file,err=err
;
; Inputs      : FILE = filename to delete
;
; Opt. Inputs : None
;
; Outputs     : None
;
; Opt. Outputs: None
;
; Keywords    : ERR = any errors
;
; Common      : None
;
; Restrictions: None
;
; Side effects: None
;
; History     : Version 1,  1-Jul-1996,  D.M. Zarro.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro rm_file,file,err=err

err=''
if datatype(file) ne 'STR' then begin
 err='Invalid input'
 return
endif

;-- expand wild chars

if strpos(file(0),'*') gt -1 then files=loc_file(file(0)) else files=file
count=n_elements(files)
ferr=strarr(count)
for i=0,count-1 do begin
 deleted=0
 on_ioerror,quit
 tmp=files(i)
 openr,unit,tmp,/get_lun,/del,error=error
 if error ne 0 then begin
  ferr(i)=tmp
 endif else begin
  close,unit
  free_lun,unit
  deleted=1
 endelse
 quit: on_ioerror,null
 if not deleted then ferr(i)=tmp
endfor

errors=where(ferr ne '',ecount)
if ecount gt 0 then err='Unable to remove file: '+arr2str(ferr(errors))

return & end