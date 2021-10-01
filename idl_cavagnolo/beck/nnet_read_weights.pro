;+
;*NAME:
;	NNET_READ_WEIGHTS.PRO
;
;*PURPOSE:
;	Reads computed weights for a Neural Network from FITS file created
;	by NNET_WRITE_WEIGHTS.PRO
;
;*CATEGORY:
;	Misc.
;
;*CALLING SEQUENCE:
;	NNET_READ_WEIGHTS, Filename, Bias_hid, W_hid, Bias_out, W_out
;
;*INPUTS:
;	Filename - Name of FITS file to be read.
;
;*OUTPUTS:
;	Bias_hid - bias weights on the hidden neurons.
;       W_hid - weights between input & hidden layers.
;       Bias_out - bias weights on the output neurons.
;       W_out - weights between hidden & output layers.
;
;*KEYWORD PARAMETERS:
;	None.
;
;*NOTES:
;
;*EXAMPLES:
;	IDL> nnet_read_weights, 'wts.fits', bhid, whid, bout, wout
;
;*HISTORY:
;	Version 1.0	Terry Beck		
;	Advanced Computer Concepts, Inc.		21 Apr 1999
;-
;_________________________________________________________________________

pro nnet_read_weights, filename, bias_hid, w_hid, bias_out, w_out

if (n_params(0) ne 5) then begin
    print, 'CALLING SEQUENCE:'
    print, '    nnet_read_weights, filename, bias_hid, w_hid, bias_out, w_out'
    print
    retall
endif

tmp = mrdfits(filename,1)

bias_hid = tmp.bias_hid
w_hid = tmp.w_hid
bias_out = tmp.bias_out
w_out = tmp.w_out

return
end
