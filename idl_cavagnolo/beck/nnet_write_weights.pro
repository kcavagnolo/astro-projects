;+
;*NAME:
;	NNET_WRITE_WEIGHTS.PRO
;
;*PURPOSE:
;	Writes computed weights for a Neural Network to a FITS file.
;
;*CATEGORY:
;	Misc.
;
;*CALLING SEQUENCE:
;	NNET_WRITE_WEIGHTS, Bias_hid, B_hid, Bias_out, B_out
;
;*INPUTS:
;	Bias_hid - bias weights on the hidden neurons.
;       W_hid - weights between input & hidden layers.
;       Bias_out - bias weights on the output neurons.
;       W_out - weights between hidden & output layers.
;
;*OUTPUTS:
;	Program creates a FITS file.
;
;*KEYWORD PARAMETERS:
;	outfile -  name of output FITS file, default="weights.fits"
;
;*NOTES:
;
;*EXAMPLES:
;	IDL> nnet_write_weights, bhid, whid, bout, wout, outfile='wts.fits'
;
;*HISTORY:
;	Version 1.0	Terry Beck		
;	Advanced Computer Concepts, Inc.		21 Apr 1999
;-
;_________________________________________________________________________

pro nnet_write_weights, bias_hid, w_hid, bias_out, w_out, outfile=outfile

if (n_params(0) ne 4) then begin
    print, 'CALLING SEQUENCE:'
    print, '    nnet_write_weights, bias_hid, w_hid, bias_out, w_out'
    print
    print, 'KEYWORD(S):'
    print, '	outfile'
    retall
endif

if not(keyword_set(outfile)) then outfile='weights.fits'

tmp_struct = {bias_hid:bias_hid, w_hid:w_hid, bias_out:bias_out, w_out:w_out}

mwrfits,tmp_struct,outfile

return
end
