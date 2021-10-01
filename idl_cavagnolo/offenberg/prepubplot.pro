pro prepubplot, outarr, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9
;+
;	PREPUBPLOT
;PURPOSE:
;	To catenate a series of vectors (up to ten) into a single array,
;	in a format suitable for using with the routine PubPlot.
;
;	For instance, to combine 3 vectors, Y1, Y2 and Y3 into a single Y
;	vector to be used as the "Y" argument for PubPlot, execute
;	PREPUBPLOT, Y, Y1, Y2, Y3
;
;CALLING SEQUENCE:
;	PrePubPlot, array, v0, [v1, v2, v3, ...,v9]
;INPUTS:
;	v0-v9: Vector(s) (or 1-dimensional arrays) to be combined into
;	       a 2-D array.  
;OUTPUTS:
;	OUTARR:	Output array (you must supply a named variable.  The user
;		will be queried before OUTARR is overwritten, if necessary.)
;		OUTARR will be NxM where N is the length of largest vector
;		in v0...v9, and M is the number of valid vectors supplied.
;PROCEDURE:
;	Find the length of the largest vector.  Then create outarr, 
;	replicating the last element of each vector until it is the same
;	size as the the longest vector.  
;RESTRICTIONS:
;	Up to 10 vectors may be used.
;SIDE EFFECTS:
;	None
;HISTORY
;	Written by J. Offenberg, Hughes STX, 6 April 1994.
;-

sizeout = size(outarr)
IF (sizeout(0) ne 0) then BEGIN
	message,/inf,'WARNING: Output array will be overwritten:'
	help,outarr
	print,'To continue, hit "y" now. To quit, hit any other key'
	xx = get_kbrd(1)
	if (strupcase(xx) ne 'Y') then RETURN
endIF
sizev = [n_elements(v0),n_elements(v1),n_elements(v2),n_elements(v3),$
	 n_elements(v4),n_elements(v5),n_elements(v6),n_elements(v7),$
	 n_elements(v8),n_elements(v9)]

maxsiz = max(sizev)
outarr = replicate(v1(0),maxsiz>1)

FOR i=0, 9 do BEGIN
	IF (sizev(i) eq maxsiz) THEN BEGIN
	   command = string(i, f='("outarr = [[outarr],[v",I1,"]]")')
	endIF ELSE $
	IF (sizev(i) ne 0) THEN BEGIN
	   command = string(i, i, sizev(i)-1, maxsiz-sizev(i),$
	       f='("outarr = [[outarr],[v",I1,",replicate(v",I1,"(",I8,"),",I8,")]]")')
	endIF ELSE $
	BEGIN
	   command = ";"
	endELSE
    res = execute(command)

    if (not(res)) Then BEGIN 
	message,/inf,'ERROR: Execute command failed'
	stop
    endIF
endFOR

outarr = outarr(*,1:*)

end
