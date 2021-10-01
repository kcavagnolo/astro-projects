; I'm going to attempt to monte-carlo the sb profiles 
; in order to obtain an estimate of the 1-sigma errors
; on the density profile with deprojection (no systematics
; obviously). So this program is re-organized so that the
; deprojection happens many many times, but interpolation only
; happens once.
;****************************************************************

function fit_results, x, a
; given x and a output y
; a = [s0, rs, beta]

s0=a(0)
rs=a(1)
beta=a(2)

y = s0 + (x/rs)^beta
return,y
end

;****************************************************************

function fit_form, x, a

; function fit is s(r) = s0 + (r/rs)^beta
; a = [s0, rs, beta]
; returns [ s(r), ds/ds0, ds/drs, ds/dbeta]


s0=a(0) & rs=a(1) & beta=a(2)

sr = fit_results(x, a)

dsds0 = x/x
dsdrs = -beta * (x/rs)^(beta-1.0)*(x/rs^2.0)
dsdbeta = (x/rs)^beta * alog(x/rs)

vector = [ sr, dsds0, dsdrs, dsdbeta ]
return,vector
end

;****************************************************************

function d_proper_Mpc,z
;COMPUTE PROPER DISTANCE IN MPC GIVEN Z AND Q0 AND H0
common cosmology, Omega_m, H0, lambda

;in this function q0 defined to be Omega_m / 2.0

q0= Omega_m/2.0

if (q0 ne 0)*(lambda eq 0.0) then begin
    return, 2.998E5*(q0*z+(q0-1.)*(sqrt(1.+2.*q0*z)-1.))/q0^2/H0/(1.+z)
endif

if (q0 eq 0) then begin
    return, 2.998E5*z*(1.+z/2.)/H0/(1.+z)
endif

if (lambda ne 0.0)*(2.*q0+lambda eq 1.0) then begin
    nz = n_elements(z)

    if nz gt 1 then begin
        dp = fltarr(nz)

        for i = 0,nz-1 do begin
            zint = findgen(100)/99. * z(i)
            dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
            dp(i) = 2.998E5/H0 *int_tabulated(zint,dz_int)
        endfor

        return, dp

    endif else begin
        zint = findgen(100)/99. * z
        dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
        dp = 2.998E5/H0 *int_tabulated(zint,dz_int)
        return, dp
    endelse
endif
return,'ERROR in proper distance, non-flat Lambda universe!'

END

;****************************************************************

function dl, z
;COMPUTE LUMINOSITY DISTANCE GIVEN Z
common cosmology, Omega_m, H0, lambda
dl = (1+z)*d_proper_Mpc(z)
return,dl
END

;****************************************************************

function da,z
;COMPUTE ANGULAR DISTANCE IN MPC GIVEN Z
common cosmology, Omega_m, H0, lambda
da = d_proper_Mpc(z)/(1+z)
return,da
END

;****************************************************************

function dangarcmin,z
;COMPUTE ANGULAR DISTANCE IN MPC/ARCMIN GIVEN Z
common cosmology, q0, H0, lambda
dd = 2.908882E-4 * d_proper_Mpc(z)/(1+z)
return,dd
END

;****************************************************************

function vv, rii, rjj
if rii le rjj then return,0.0
pi = 3.14159265359
vvv = 4.*pi/3. * (rii^2 - rjj^2)^(1.5)
return,vvv
END

;****************************************************************

pro central_sb, dat1, dat2, nmc
; central_sb, '1tpr_nhfrozen_fefree.dat', 'reference.list', 1000

; restore the fit template and read some variables
restore,"xspecfittemplate_ext.sav"
fitfile = read_ascii(dat1, template = xspecfittemplate_ext)

restore,"reflist_template.sav"
ref = read_ascii(dat2, template = reflist_template)

; this program will deproj a possibly smoothed sbr profile
; convert it to nelec_interp using pre-measured annular spectra
; then calculate S entropy and fit that to 2 power laws
; right now it doesn't return anything, but that could change
; call it in IDL> sb_fits_mc, "blah.fits", "abell_blah", 5.5, 1000
; 1000 monte-carlo simulations

common cosmology, Omega_m, H0, lambda
pi = 3.14159265359

; take alternative assumption regarding geometry
Omega_m = 0.3
lambda = 0.7
H0 = 70.

cname = strcompress(ref.cluster,/remove_all)
cname = str_replace(cname,'ABELL','Abell')
cname = str_replace(cname,'CYGNUS_A','Cygnus A')
cname = str_replace(cname,'_',' ')
cname = str_replace(cname,'OPHIUCHUS','Ophiuchus')

; read in sb profiles, ASSUMES units CTS/PIXEL
; inner outer radii, units PIXEL (same as in sb profile)
; FITS file produced by dmextract command
; define the obsid
FOR i = 0, n_elements(ref.obsid)-1 DO BEGIN
    clustername = strcompress(ref.cluster[i], /remove_all)
    name = cname[i]
    obs = strcompress(ref.obsid[i], /remove_all)
    chip = strcompress(ref.chipid[i], /remove_all)
; change the chipid from eg: 'i0' to 'i0123'
    a = 'i'
    IF (STRCMP(chip,a,1) EQ 1 ) THEN chip = 'i0123'
    tkev = strcompress(ref.tx[i], /remove_all)
    z = strcompress(ref.z[i], /remove_all)

; define the filename for the radial profile
    filename = '../radprofs/10pixs/'+clustername+'_'+obs+'_'+chip+'_radprof_10pix.fits'
    print, "Opening ", filename

; assuming the table is in extension 1
; reads all the columns into a structure named fits
    fits = mrdfits(filename,1)
    sbr = fits.sur_bri
    sbr_err = fits.sur_bri_err
    texp = fits.exposure
    texp=texp[0]

; convert radii units from pixels to arcseconds
    pixtoarcsec = 0.492         ; ACIS scale is 0.492"/pixel
    sbr = sbr / (0.492)^2
    sbr_err = sbr_err / (0.492)^2 ; converted from per pixel to per square arcsecond

; the following step is only necessary if the 
; input surface brightness profile is not normalized by time
; retrieve the exposure time from the header
    sbr = sbr/texp
    sbr_err = sbr_err/texp

    sbr = sbr[0]/10^(-3.)
    sbr_err = sbr_err[0]/10^(-5.)

;    openw,1,'sbr'+strcompress(clustername,/remove_all)+'.dat'
    openw,1,'sbr'+strcompress(i,/remove_all)+'.dat'
    printf,1,cname[i],'   ',sbr,'   ',sbr_err
    close,1

ENDFOR

END
