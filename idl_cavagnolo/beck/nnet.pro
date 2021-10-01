;+
;*NAME:
;	NNET.PRO
;
;*PURPOSE:
;	Neural network classifier. This is a standard 3-layer 
;	back-propagation net.
;
;*CALLING SEQUENCE:
;	NNET, bias_hid, w_hid, bias_out, w_out, input, h_output, $
;		output, first, second, third
;
;*INPUTS:
; 	bias_hid - Bias weights on the hidden neurons (DP vector [n_hid]).
;	w_hid - weights between input & hidden layers (DP array [n_in,n_hid]).
;	bias_out - Bias weights on the output neurons (DP vector [n_out]).
;	w_out - weights between hidden & output layers (DP array [n_hid,n_out]).
;	input - input values (DP vector [n_in]).
;	
;*OUTPUTS:
;	h_output - computed values of hidden layer neurons (DP vector [n_hid]).
;	output - computed values of output neurons (DP vector [n_out]).
;	first - index of output neuron with highest value.
;	second - index of second place neuron.
;	third - index of third place neuron (if there are more than two output
;		neurons).
;
;*KEYWORD PARAMETERS
;	None.
;
;*EXAMPLE:
;	IDL> nnet, bias_hid, w_hid, bias_out, w_out, input, hout, output
;
;	- input is the pattern you want to classify. Must be a normalized 
;	  float or DP vector or the same size as the training patterns used
;	  to train the weights.
;
;*HISTORY:
;	Version 1.0 	T. Beck		
;	Advanced Computer Concepts, Inc.		21 Apr 1999
;-
;___________________________________________________________________________

pro nnet, bias_hid, w_hid, bias_out, w_out, input, $
    h_output, output, first, second, third

; compute values of hidden layer neurons (z)

z_in = transpose(input#w_hid+bias_hid) 
z = 1.0d0/(1+exp(-z_in))

; compute values of output layer neurons (y)

y_in = transpose(z#w_out+bias_out)
output = 1.0d0/(1+exp(-y_in))
h_output = z

; find index of output unit w/max value, 2nd, and third choice

t = reverse(sort(output))

first = t(0)
second = t(1)
third = t(2)
    
return 
end
   
