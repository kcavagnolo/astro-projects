pro dist_circle ,im, n, xcen ,ycen 
;+
; NAME:	
;	DIST_CIRCLE
; PURPOSE:	
;	Form a square array in which the value of each element is its
;	distance to a specified center.   Useful for circular aperture
;	photometry.
;
; CALLING SEQUENCE:
;	DIST_CIRCLE, IM, N, XCEN, YCEN
;
; INPUTS:
;	N = size of result, scalar.   Output array will have dimensions N x N
;	XCEN,YCEN = Scalars designating the X,Y pixel center.  These need
;		not be integers, and need not be located within the
;		output image
;
; OUTPUTS:
;	IM  - N by N floating array in which  the value of each pixel is 
;		equal to its distance to XCEN,YCEN
;
; EXAMPLE:
;	Total the flux in a circular aperture within 3' of a specified RA
;	and DEC on an 512 x 512 image IM, with a header H.
;
;	IDL> adxy, H, RA, DEC, x, y       ;Convert RA and DEC to X,Y
;	IDL> getrot, H, rot, cdelt        ;CDELT gives plate scale deg/pixel
;	IDL> cdelt = cdelt*3600.          ;Convert to arc sec/pixel
;	IDL> dist_circle, circle, 512, x, y  ;Create a distance circle image
;	IDL> circle = circle*abs(cdelt(0))   ;Distances now given in arcseconds
;	IDL> good = where(circle LT 180)  ;Within 3 arc minutes
;	IDL> print,total( IM(good) )      ;Total pixel values within 3'
;
; RESTRICTIONS:
;	The speed of DIST_CIRCLE decreases and the the demands on virtual
;	increase as the square of the output dimensions.   Users should
;	dimension the output array as small as possible, and re-use the
;	array rather than re-calling DIST_CIRCLE
;
; MODIFICATION HISTORY:
;	Adapted from DIST    W. Landsman            March 1991
;-
 On_error,2                ;Return to caller if an error occurs

 if N_params() LT 4  then begin
     print,'Calling Sequence - DIST_CIRCLE, IM, N, XCEN, YCEN'
     print,'IM - output image array'
     print,'N - size of the (square) output image array'
     print,'XCEN,YCEN - position from which to specify distances'
     return
 endif

 z = findgen(n)		  ;Make a row
 x_2 = (z - xcen) ^ 2	  ;X distances (squared)
 y_2 = (z - ycen) ^ 2     ;Y distances (squared)  
 im = fltarr( n, n, /NOZERO)  	  ;Make uninitialized output array
 for i = 0L,n-1 do begin	          ;Row loop
	im(0,i) = sqrt(x_2 + y_2(i))     ;Euclidian distance
 endfor

 return
 end

