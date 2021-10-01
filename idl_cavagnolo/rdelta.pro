;+
; NAME:
;   RDELTA
;
; PURPOSE:
;   This procedure returns the radius (in Mpc) for input density 
;   contrast, redshift, and temperature. The formula used is taken
;   from Arnaud, Aghanim, and Neumann A&A, 2002, the beta
;   normalization is taken from Evrard, Metzler, and Navarro ApJ, 1996.
;
; CATEGORY:
;   Cosmology
;
; CALLING SEQUENCE:
;   result = rdelta(deltaC, z, tx, [H0= , k= , omega_m= , lambda0= , q0= , betat= , /SILENT])
;
; INPUTS:
;   deltaC: The density contrast for which one desires to have the radius
;           computed. IE, 500, 200, 180, 2500, ...
;   z: the redshift of the cluster or object
;   tx: the temperature (mainly X-Ray) of the cluser or object
;
; OPTIONAL KEYWORD INPUTS:
;    /SILENT - If set, the program will not display adopted cosmological
;        parameters at the terminal.
;    H0: Hubble parameter  in km/s/Mpc, default is 70
;
;        No more than two of the following four parameters should be
;        specified.   None of them need be specified -- the adopted defaults
;        are given.
;    k: curvature constant, normalized to the closure density.   Default is
;       0, indicating a flat universe
;    omega_m: Matter density, normalized to the closure density, default
;             is 0.3. Must be non-negative
;    lambda0: Cosmological constant, normalized to the closure density,
;             default is 0.7
;    q0: Deceleration parameter, numeric scalar = -R*(R'')/(R')^2, default
;        is -0.5
;    betat: The normalization of the virial relation as defined in EMN,
;           ApJ 1996
;
; OUTPUTS:
;   Density contrast radius in h_{70}**-1 kpc.
;
; PROCEDURE:
;   Bibtex reference for the paper motivating this pro:
;   @ARTICLE{2002A&A...389....1A,
;      author = {{Arnaud}, M. and {Aghanim}, N. and {Neumann}, D.~M.},
;       title = "{The X-ray surface brightness profiles of hot galaxy
;                 clusters up to vec z \~{} 0.8: Evidence for
;                 self-similarity and constraints on Omega_{0}}",
;     journal = {\aap},
;      eprint = {astro-ph/0110428},
;        year = 2002,
;       month = jul,
;      volume = 389,
;       pages = {1-18},
;         doi = {10.1051/0004-6361:20020378},
;      adsurl = {http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2002A%26A...389....1A&db_key=AST},
;     adsnote = {Provided by the Smithsonian/NASA Astrophysics Data System}
;   }
;
; EXAMPLE:
;   deltac = 180.
;   z = findgen(10)/10.
;   tx = replicate(5.2,10)
;   result = rdelta(2500,z,tx,H0=70)
;
; PROCEDURES CALLED:
;   COSMO_PARAM
;
; MODIFICATION HISTORY:
;   Written by: KWC, MSU-PA, Nov, 2006.
;-

FUNCTION rdelta, deltac, z, tx, h0=h0, k=k, omega_m=omega_m, lambda0=lambda0, q0=q0, silent=silent

ON_ERROR, 2
IF (n_params() LT 1) THEN BEGIN
   doc_library, 'rdelta'
   RETURN, -1
ENDIF

IF ((deltac LE 0.) OR (z LE 0.) OR (tx LE 0.)) THEN BEGIN
    print, 'ERROR - input parameters cannot be less than or equal to zero. You gave me:'
    print, 'Deltac = ',deltac
    print, 'z = ',z
    print, 'Tx = ',tx
    return, -1
ENDIF

; Return a full array of cosmo. density params
cosmo_param, omega_m, lambda0, k, q0

; Check for input cosmo variables
IF n_elements(H0) EQ 0 THEN H0 = 70.
IF n_elements(betat) EQ 0 THEN betat = 1.05
IF NOT keyword_set(silent) THEN $
  print,'RDELTA: H0:', H0, '; Omega_m:', omega_m, '; Lambda0:', lambda0, $
        '; q0: ',q0, '; k: ', k, f='(A,I3,A,f5.2,A,f5.2,A,f5.2,A,F5.2)' 

rd = z*0.
FOR i = 0, n_elements(z)-1 DO BEGIN
    IF z[i] LT 0. THEN rd[i] = 0. ELSE BEGIN
                                ; Calculate omega_z
        nume = omega_m*(1.+z[i])^3.
        deno = (omega_m*(1.+z[i])^(3.))+((1-omega_m-lambda0)*(1.+z[i])^(2.))+lambda0
        omegaz = nume/deno
        
        ; Calculate delta_z
        nume = deltac*omega_m
        deno = 18*!pi^2.*omegaz
        deltaz = nume/deno

        ; Calculate density radius
        rd[i] = 3.8*(sqrt(betat))*(1./sqrt(deltaz))*((1.+z[i])^(-3./2.))*(sqrt(tx[i]/10.))*((H0/50.)^(-1.))
    ENDELSE
ENDFOR

return, rd

END
