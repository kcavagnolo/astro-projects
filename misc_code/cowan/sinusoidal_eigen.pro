PRO sinusoidal_eigen, F_0, C, D, J_0, A, B, inverse
; Nick Cowan
; Sept 2009
; Please cite Cowan & Agol (2008) if you use this code
;
; F_0 mean of lightcurve
; C cosine coefficients of lightcurve
; D sine coefficients of lightcurve
;
; J_0 mean of longitudnal map
; A cosine coefficients of map
; B sine coefficients of map


; lightcurve --> map
IF(inverse EQ 0) THEN BEGIN
    long_C = [0,C]
    long_D = [0,D]

    n_modes = N_ELEMENTS(long_C)-1
    long_A = long_C
    long_B = long_D
    
    J_0 = 0.5*F_0
    
    long_A[1] = 2.0/!PI * long_C[1]
    long_B[1] = -2.0/!PI * long_D[1]
    
    FOR index=2, n_modes DO BEGIN
        IF((index MOD 2) EQ 0) THEN BEGIN
            long_A[index] = -(index^2-1)/2.0 * (-1)^(index/2) * long_C[index]
            long_B[index] = (index^2-1)/2.0 * (-1)^(index/2) * long_D[index]
        ENDIF ELSE BEGIN
            long_A[index] = 0D
            long_B[index] = 0D
        ENDELSE
    ENDFOR

    A = long_A[1:n_modes]
    B = long_B[1:n_modes]    
ENDIF


; map --> lightcurve
IF(inverse EQ 1) THEN BEGIN
    long_A = [0,A]
    long_B = [0,B]

    n_modes = N_ELEMENTS(long_A)-1
    long_C = long_A
    long_D = long_B
    
    F_0 = 2.0*J_0
    
    long_C[1] = !PI/2.0 * long_A[1]
    long_D[1] =  -!PI/2.0 * long_B[1]
    
    FOR index=2, n_modes DO BEGIN
        IF((index MOD 2) EQ 0) THEN BEGIN
            long_C[index] = -2.0/(index^2-1) * (-1)^(index/2) * long_A[index]
            long_D[index] = 2.0/(index^2-1) * (-1)^(index/2) * long_B[index]
        ENDIF ELSE BEGIN
            long_C[index] = 0D
            long_D[index] = 0D
        ENDELSE
    ENDFOR

    C = long_C[1:n_modes]
    D = long_D[1:n_modes]    
ENDIF

END
