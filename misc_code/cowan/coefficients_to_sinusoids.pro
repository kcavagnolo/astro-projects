FUNCTION coefficients_to_sinusoids, x, parameters
; Nick Cowan
; Sept 2009
; Please cite Cowan & Agol (2008) if you use this code
;
; INPUTS:
; x is assumed to run between 0 (eclipse), pi (transit) and 2pi
;
; J_0 dc offset
; A cosine coefficients
; B sine coefficients

n_params = N_ELEMENTS(parameters)

J_0 = parameters[0]
A = parameters[1:(n_params-1)/2]
B = parameters[(n_params-1)/2 + 1:n_params-1]

resolution = N_ELEMENTS(x)
J = REPLICATE(J_0, resolution)
       
FOR n=0, N_ELEMENTS(A)-1 DO J = J + A[n]*COS((n+1)*x) + B[n]*SIN((n+1)*x)

RETURN, J

END
