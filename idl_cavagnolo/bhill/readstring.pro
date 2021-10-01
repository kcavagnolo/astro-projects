PRO READSTRING,Filename,Array,MAXLINE=maxline
;+
; NAME:                 readstring
;       
; PURPOSE:  Read text file into a string array.
;
; CATEGORY: Text processing
;
; CALLING SEQUENCE: readstring,filename,array
;
; INPUT ARGUMENTS:  
;       filename - input file name
;
; OUTPUT ARGUMENTS:
;       array - string array with contents of file
;
; INPUT KEYWORD PARAMETER:
;       maxline - maximum number of lines allowed; default=1000
;
; MODIFICATION HISTORY:
;   30 Jun. 1997 - Written.  RSH/HSTX
;   28 Jun. 2000 - Square bracket subscripts.  RSH
;-
IF n_elements(maxline) LT 1 THEN maxline=1000
array = strarr(maxline)
openr,lun,filename,/get_lun
line = ''
i=0
WHILE NOT eof(lun) DO BEGIN
    readf,lun,line
    array[i] = line
    i = i + 1
ENDWHILE
i = i - 1
array = array[0:i]
free_lun,lun
RETURN
END
    

