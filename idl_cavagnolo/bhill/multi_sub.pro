PRO multi_sub,dimensions,list,multi_list,ARRAY=array
;+
; NAME:     multi_sub
;
; PURPOSE:
;     Convert array of one-dimensional subscripts (e.g., from where
;     function) to subscripts for individual dimensions.
; 
; CALLING SEQUENCE:
;     multi_sub,dimensions,list,multi_list
;
; INPUT ARGUMENTS:
;     dimensions        Dimensions of array for which subscripts are
;                       wanted, e.g., [512,1024] or [3,3,6].  Specify
;                       dummy value if the array is supplied directly
;                       via keyword input.
;
;     list              List of one-dimensional subscripts to be 
;                       converted.
;
; INPUT KEYWORD PARAMETER:
;     array             The array for which subscripts are wanted.  
;                       If this is supplied, the dimensions argument
;                       is ignored.
;
; OUTPUT ARGUMENTS:
;     multi_list        Gives the output subscripts.  Each row 
;                       corresponds to 1 dimension.
;
;                       M X N array, where N is the number of elements
;                       in the dimensions argument (or the number of
;                       dimensions of the array keyword value), and M
;                       is the number of elements in the list argument.
;                       E.g., if list contains 500 elements, and the
;                       dimensions are [1024,1024], then multi_list(*,0)
;                       has 500 elements and gives the X subscripts, and
;                       multi_list(*,1) has 500 elements and gives the
;                       Y subscripts.   
;           
;                       Returns -1 if list contains elements outside
;                       the implied or actual array bounds.
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:   none
;
; PROCEDURE:   straightforward
;
; MODIFICATION HISTORY:
;     12 Mar. 1997 - Written.  RSH, HSTX
;      2 Nov. 1999 - Fix for more than two dims.  RSH                   
;-

IF n_params(0) LT 3 THEN BEGIN
    print,'CALLING SEQUENCE:  multi_sub,dimensions,list,multi_list'
    print,'KEYWORD PARAMETERS:  array'
    retall
ENDIF

IF n_elements(array) GE 1 THEN BEGIN
    sz = size(array)
    dimensions = sz(1:sz(0))
ENDIF

ndim = n_elements(dimensions)
tot_elem = lonarr(ndim)
nelem = 1L
FOR i=0, ndim-2 DO BEGIN
    nelem = nelem*dimensions(i)
    tot_elem(i+1) = nelem
ENDFOR
nelem = nelem*dimensions(ndim-1)
tot_elem(0) = 1

IF max(list) GT (nelem-1) THEN BEGIN
    IF !debug THEN $
      print,'MULTI_SUB:  Invalid input.  Subscripts outside array bounds.'
    multi_list = -1
    return
ENDIF

nlst = n_elements(list)
multi_list = lonarr(nlst,ndim)
list2 = 0

FOR i = ndim-1, 0, -1 DO BEGIN
    this_list = floor((list-list2) / tot_elem(i))
    list2 = list2 + this_list*tot_elem(i)
    multi_list(0,i) = this_list
ENDFOR

return
END
