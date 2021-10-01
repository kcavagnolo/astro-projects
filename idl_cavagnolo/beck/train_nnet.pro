;+
;*NAME:
;       TRAIN_NNET.PRO
;
;*PURPOSE:
;	Trains (computes) the values of the weights used by the neural-network
;	classifier.
;
;*CALLING SEQUENCE:
;	TRAIN_NNET, n_pat, n_in, n_hid, n_out, train_set, classes, bias_hid, $
;		w_hid, bias_out, w_out
;
;*INPUTS:
;	n_pat - number of training patterns (INT scalar).
;	n_in - number of input neurons (INT scalar).
;       n_hid - number of hidden neurons (INT scalar).
;       n_out - number of output neurons (INT scalar).
;	train_set - training data ( DBLARR[n_in,n_pat] ).
;	classes - classification of each training pattern ( INTARR[n_pat] ).
;
;*OUTPUTS:
;	bias_hid - bias weights on the hidden neurons ( DBLARR[n_hid] ).
;       w_hid - weights between input & hidden layers ( DBLARR[n_in,n_hid] ).
;       bias_out - bias weights on the output neurons ( DBLARR[n_out] ).
;       w_out - weights between hidden & output layers ( DBLARR[n_hid,n_out] ).
;
;*KEYWORD PARAMETERS:
;	outfile - set this keyword to write the computed weights to a FITS
;		file. 
;	alpha - learning rate, default=0.15.
;	mu - momemtum term, default=0.10.
;
;*EXAMPLE:
;	This example uses the neural network as a stellar spectral classifier.
;	It could be used to classify any type of data, if the data could
;	be input as a normalized vector.
;	--------------------------------------------------------------------
;	You have a set of 10 flux & wavelength calibrated spectra. If 
;	necessary, resample the spectra to the same dispersion (eg. nm/pixel). 
;	Extract the same wavelength region from all spectra. Normalize. Make 
;	sure all pixel values are between 0 and 1.0. Stack all spectra into 
;	a single 2-D array. This is the training set (see input variable 
;	"train_set" above). If each spectrum has 200 pixels, then the size of
;	train_set will be (200,10). n_pat = 10 and n_in = 200 also.
;
;	Create a integer vector ("classes", above) of 10 elements, each 
;	element is a number that designates the spectral type of the 
;	corresponding spectra in the training set, by subscript:
;
;		classes(0) <====> train_set(*,0)
;
;	It is help to generate a lookup table:
;
;		class		SP type
;		-----		-------
;		  0		  M0V
;		  1		  M1V
;		  2		  M1.5V
;		  3		  M2V
;		  4		  M3V
;		  5		  M4V
;		  6		  M5V
;
;	Example of classes vector:
;
;		IDL> classes = [0,1,2,2,3,4,4,5,6,6]
;
;	Note that in this case some spectral types have more than one example.
;	It is a good idea to have a many examples of each spectral type as
;	possible, this will allow the neural net to generalize better and be
;	able to ignore noise. 
;
;	CAUTION: Two examples of the same spectal type that very different
;	in appearance due to noise, poor calibraion, etc. may cause the
;	network not to converge to a solution. 
;
;	In this example the number of output neurons (n_out) is equal to 7.
;	Set n_hid to some number between n_in and n_out, in this example,
;	100 would be a good choice.
;
;	Ready to run:
;		IDL> train_nnet, 10, 200, 100, 7, train_set, classes, $
;			bias_hid, w_hid, bias_out, w_out
;
;*OPERATIONAL NOTES:
;
;	While program is running, it prints the training epoch (iteration)
;	and the total error of all training patterns across all output units
;	to standard output.
;
;*HISTORY:
;       Version 1.0     Terry Beck        
;	Advanced Computer Concepts, Inc.		21 Apr 1999
;-
;___________________________________________________________________________

pro train_nnet, n_pat, n_in, n_hid, n_out, train_set, classes, $
	bias_hid, w_hid, bias_out, w_out, outfile=outfile, $
	alpha=alpha, mu=mu
	
    if not(keyword_set(alpha)) then alpha=0.15
    if not(keyword_set(mu)) then mu=0.10
;
; initialize weights
;	
    bias_hid = double(randomu(seed,n_hid)) - 0.5
    w_hid = double(randomu(seed,n_in,n_hid)) - 0.5
    bias_out = double(randomu(seed,n_out)) - 0.5
    w_out = double(randomu(seed,n_hid,n_out)) - 0.5
;
; generate target array 
;    
    targ = fltarr(n_pat,n_out)
    n = indgen(n_pat)
    targ(n,classes(n)) = 1.0
;
; define other needed arrays
;
    delw = dblarr(n_hid,n_out)
    delw0 = dblarr(n_out)
    del_in = dblarr(n_hid)
    v0_old = dblarr(n_hid)
    v_old = dblarr(n_in,n_hid)
    w0_old = dblarr(n_out)
    w_old = dblarr(n_hid,n_out)
    
;.......n_pat > number of training patterns.  
;.......n_out > number of output units.  
;.......n_hid > number of hidden units.  
;.......n_in > number of input units.  
;.......x > input units.
;.......z > hidden units.
;.......y > output units.
;.......v > weights from input to hidden units.		(w_hid)
;.......v0 > bias on hidden units. 			(bias_hid)
;.......w > weights from hidden to output units.	(w_out)
;.......w0 > bias on output units. 			(bias_out)
;.......t > target vector.
	
    epoch = 0
    ct = 0
    last_sum = 10000
    flag = 0
    sum_error = 0

;
; feed foward phase
;
	print, "Training begins..."
	print 

train:	
	for pat = 0, n_pat-1 do begin
	
	    input = train_set(*,pat)
	    nnet, bias_hid, w_hid, bias_out, w_out, $
	    	input, z, y
;
; back propagation of error phase
;
	    k = indgen(n_out)
	    error = total(abs(targ(pat,k) - y(k)))
	    del = reform(targ(pat,k) - y(k))*y(k)*(1 - y(k))
	    
	    if (error gt 0.4) then flag = 1
	    sum_error = sum_error + error
;
	    delw0 = alpha*del
	    
	    for j = 0, n_hid-1 do begin
	        delw(j,k) = alpha*del(k)*z(j)
	    endfor
;
	    del_in = dblarr(n_hid)
	    
	    for j = 0, n_hid-1 do begin
		 del_in(j) =  total(del*w_out(j,k))
	    endfor
;
	    delz = dblarr(n_hid)
	    for j = 0,n_hid-1 do begin
		delz(j) = del_in(j)*z(j)*(1 - z(j))
	    endfor
;
	    delv0 = alpha*delz
	    delv = dblarr(n_in,n_hid)
	    
	    i = indgen(n_in)
	    for j = 0,n_hid-1 do begin
		delv(i,j) = alpha*delz(j)*input(i)
	    endfor
;
;.......weight adjustment phase............................
;
	    j = indgen(n_hid)
	    v0_new = bias_hid + delv0 + mu*(bias_hid - v0_old)
	    v0_old = bias_hid
	    bias_hid = v0_new
	    
	    for i = 0,n_in-1 do begin
	    	v_new = w_hid(i,j) + delv(i,j) + mu*(w_hid(i,j) - v_old(i,j))
	    	v_old(i,j) = w_hid(i,j)
	    	w_hid(i,j) = v_new
	    endfor
;
	    k = indgen(n_out)
	    w0_new = bias_out + delw0 + mu*(bias_out - w0_old)
	    w0_old = bias_out
	    bias_out = w0_new
	    
            for j = 0,n_hid-1 do begin
	    	w_new = w_out(j,k) + delw(j,k) + mu*(w_out(j,k) - w_old(j,k))
	    	w_old(j,k) = w_out(j,k)
	    	w_out(j,k) = w_new
	    endfor

	endfor
;	
;.......output phase.......................................
;
	epoch = epoch + 1
	ct = ct + 1
	if (ct eq 2) then begin
	    print, epoch, sum_error
 	    ct = 0
	endif
;
	if (flag eq 0) then begin
	    print, epoch, sum_error
	    print,'learning rate = ', alpha
	endif else begin
	    last_sum = sum_error
	    sum_error = 0
	    flag = 0
	    goto, train
	endelse
	
	if keyword_set(outfile)) then begin
	    s = size(outfile)
	    if s(0) ne 0 then outfile='weights.fits'
	    if (s(s(0)+1) ne 7) then outfile='weights.fits'
	    nnet_write_weights, bias_hid, w_hid, bias_out, w_out, $
	    	outfile=outfile
	endif
	
return
end
	

	
	
