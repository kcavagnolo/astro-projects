	FUNCTION Arr2Str, starray, delim, delimitor=delimitor, trim=trim
;	--------------------------------------------------------
;+						20-June-91
;	Name:
;		Arr2Str
;
;       Purpose: convert array to a delimited string (def delim is comma)
;
;	Input:
;		arr	input data array which is to be converted
;			to a simple string.
;	Input/keyword:
;		delimitor	default delimitor is ','
;		trim		If set, call STRTRIM when converting to string
;	Returned:
;		simple string using the delimitor to seperate the
;		elements of the orginal array.
;	History:
;		written by SLF
;		modified, slf, 11/19/91  - handle leading delimintor
;					   more gracfully
;		2-Dec-91 (MDM) - Fixed bug induced by 19-Nov fix
;				 it was not handling cases where input
;				 was an non-string array of more than one element
;		25-feb-91, slf - added positional delim parameter
;		 4-Jun-92, mdm - Changed to use N_ELEMENTS instead of KEYWORD_SET
;				 for checking "delimitor" so that null strings
;				 can be used with "STR_REPLACE" function
;               12-May-94, slf - remove strtrim and strcompress(???)
;		15-Jun-94, MDM - Added /TRIM optional keyword
;-
;	-------------------------------------------------------
;	ON_ERROR, 2	;force a return to caller on error

	if n_params() eq 2 then delimitor = delim
	IF (n_elements(delimitor) eq 0) then delimitor=','

        strings=string(starray)
	if (keyword_set(trim)) then strings = strtrim(strings,2)
	string=strings(0)
	for i=1,n_elements(starray)-1 do string= string + $
		delimitor + strings(i)
;	string=strcompress(string)
	return,string
	END

