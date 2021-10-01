FUNCTION d_proper_Mpc,z
;COMPUTE PROPER DISTANCE IN MPC GIVEN Z AND Q0 AND H0
common cosmology, Omega_m, H0, lambda

;in this function q0 defined to be Omega_m / 2.0

q0= Omega_m/2.0

IF (q0 ne 0)*(lambda eq 0.0) THEN BEGIN
    RETURN, 2.998E5*(q0*z+(q0-1.)*(sqrt(1.+2.*q0*z)-1.))/q0^2/H0/(1.+z)
ENDIF

IF (q0 eq 0) THEN BEGIN
    RETURN, 2.998E5*z*(1.+z/2.)/H0/(1.+z)
ENDIF

IF (lambda ne 0.0)*(2.*q0+lambda eq 1.0) THEN BEGIN
    nz = n_elements(z)

    IF nz gt 1 THEN BEGIN
        dp = fltarr(nz)

        FOR i = 0,nz-1 DO BEGIN
            zint = findgen(100)/99. * z(i)
            dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
            dp(i) = 2.998E5/H0 *int_tabulated(zint,dz_int)
        ENDFOR

        RETURN, dp

    ENDIF ELSE BEGIN
        zint = findgen(100)/99. * z
        dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
        dp = 2.998E5/H0 *int_tabulated(zint,dz_int)
        RETURN, dp
    ENDELSE
ENDIF
RETURN,'ERROR in proper distance, non-flat Lambda universe!'

END

;#####################
;#####################

FUNCTION dl, z
;COMPUTE LUMINOSITY DISTANCE GIVEN Z
common cosmology, Omega_m, H0, lambda
dl = (1+z)*d_proper_Mpc(z)
RETURN,dl
END

;#####################
;#####################

FUNCTION da,z
;COMPUTE ANGULAR DISTANCE IN MPC GIVEN Z
common cosmology, Omega_m, H0, lambda
da = d_proper_Mpc(z)/(1+z)
RETURN,da
END

;#####################
;#####################

FUNCTION dangarcmin,z
;COMPUTE ANGULAR DISTANCE IN MPC/ARCMIN GIVEN Z
common cosmology, q0, H0, lambda
dd = 2.908882E-4 * d_proper_Mpc(z)/(1+z)
RETURN,dd
END

;#####################
;#####################

FUNCTION r50C, T
; this procedure calculates r5000 in kpc/h70 using
; r5000(kpc/h70)= 226(kpc) (h70)^-1 (kT/1.0 kev)^(1/2)
common cosmology, Omega_m, H0, lambda
r5000 = 160. * sqrt(T) * (H0/70.)^(-1)
RETURN,r5000
END

;#####################
;#####################
; Main Program
;#####################
;#####################

pro r5000_z_tx

common cosmology, Omega_m, H0, lambda
pi = 3.14159265359

; take alternative assumption regarding geometry
Omega_m = 0.3
lambda = 0.7
H0 = 70.

openw,1,'values'
z = 0.01
WHILE (z LT 1.) DO BEGIN
    tx = 0.5
    WHILE tx LE 30.0 DO BEGIN
        r5000 = r50C(tx)
        r50pix = (r5000 * 60.) / (0.492 * (dangarcmin(z) * 1000.))
        IF (r50pix LE 1024.) THEN printf,1,tx,' ',z,' ',r50pix
        tx = tx + 0.5
    ENDWHILE
    printf,1,' '
    z = z + 0.01
ENDWHILE

close,1

END
