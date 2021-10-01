FUNCTION thermal_map_to_lightcurve, phi_in, theta_in, R_p, T, xi, lambda, R_s, T_b
; Computes the orbital thermal variations of an exoplanet 
; VERSION 1.0
; Sept 2009
; Nick Cowan
;
; Please cite Cowan & Agol (2008) if you use this code
; Let me know if you find a bug (cowan@astro.washington.edu)
;
; ASSUMPTIONS
; edge-on orbit
; zero obliquity
; no limb-darkening
; each grid-point emits like a blackbody
; regular grid in phi and theta
;
; INPUTS
; phi_in[n_phi, n_theta] or phi_in[n_phi] longitude from the sub-stellar point (radians)
;                        positive phi is east; a hotspot east of
;                        the substellar point produces a peak somewhat
;                        before superior conjunction 
; theta_in[n_phi,n_theta] or theta_in[n_theta] latitude from equator (radians)
; R_p[n_phi, n_theta, n_t] or just R_p: planet-centric radius of grid points (cm)
; T[n_phi, n_theta, n_t] or T[n_phi, n_theta] temperature of emmiting surface (Kelvin)
; xi[n_t] observed phases (0 at superior conjunction; pi at inferior
;         conjunction).  If map is time-variable, the temperature maps
;         must be provided at the orbital phases observed
; lambda[n_lambda] wavelengths observed (in angstroms)
; R_s radius of star (cm)
; T_b[n_lambda] brightness temperature of star at observed wavelengths (Kelvin)
;
; OUTPUT
; F[n_t, n_lambda] flux from planet as function of phase and
;                  wavelength, in units of stellar flux


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make sure phi and theta arrays have proper dimensions
phi_size = SIZE(phi_in)
theta_size = SIZE(theta_in)
T_size = SIZE(T)
n_phi = T_size[1]
n_theta = T_size[2]

IF(phi_size[0] EQ 1) THEN BEGIN
    phi = MAKE_ARRAY(n_phi, n_theta, /DOUBLE)
    FOR theta_index=0, n_theta-1 DO phi[*,theta_index] = phi_in
ENDIF ELSE BEGIN
    phi = phi_in
ENDELSE

IF(theta_size[0] EQ 1) THEN BEGIN
    theta = MAKE_ARRAY(n_phi, n_theta, /DOUBLE)
    FOR phi_index=0, n_phi-1 DO theta[phi_index,*] = theta_in
ENDIF ELSE BEGIN
    theta = theta_in
ENDELSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; area of grid points (cm^2)
dA = (phi[1,0]-phi[0,0]) * (theta[0,1]-theta[0,0]) * COS(theta) * R_p^2

n_t = N_ELEMENTS(xi)
n_lambda = N_ELEMENTS(lambda)
F = MAKE_ARRAY(n_t, n_lambda, /DOUBLE)

; compute stellar flux
stellar_flux = MAKE_ARRAY(n_lambda, /DOUBLE)
FOR lambda_index=0, n_lambda-1 DO stellar_flux[lambda_index] = blackbody(lambda[lambda_index],T_b[lambda_index])
stellar_flux = stellar_flux * !PI * R_s^2

IF(T_size[0] EQ 3) THEN BEGIN
    ; time-variable temperature map
     FOR lambda_index=0, n_lambda-1 DO BEGIN        
        FOR t_index=0, n_t-1 DO BEGIN
            J = TOTAL(COS(theta) * blackbody(lambda[lambda_index],T[*,*,t_index]) * dA, 2)
            V = COS(phi + xi[t_index])
            V[WHERE(V LT 0)] = 0
            F[t_index, lambda_index] = TOTAL(J * V)/stellar_flux[lambda_index]
        ENDFOR        
    ENDFOR
   
ENDIF ELSE BEGIN
    ;steady-state temperature map
    FOR lambda_index=0, n_lambda-1 DO BEGIN        
        J = TOTAL(COS(theta) * blackbody(lambda[lambda_index],T) * dA, 2)
        FOR t_index=0, n_t-1 DO BEGIN
            V = COS(phi + xi[t_index])
            V[WHERE(V LT 0)] = 0
            F[t_index, lambda_index] = TOTAL(J * V)/stellar_flux[lambda_index]
        ENDFOR
    ENDFOR

ENDELSE

;STOP

RETURN, F

END
