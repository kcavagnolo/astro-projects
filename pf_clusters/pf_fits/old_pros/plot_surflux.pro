;***********************************************************************
;***********************************************************************

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

;***********************************************************************
;***********************************************************************

FUNCTION dl, z
;COMPUTE LUMINOSITY DISTANCE GIVEN Z
common cosmology, Omega_m, H0, lambda
dl = (1+z)*d_proper_Mpc(z)
RETURN,dl
END

;***********************************************************************
;***********************************************************************

FUNCTION da,z
;COMPUTE ANGULAR DISTANCE IN MPC GIVEN Z
common cosmology, Omega_m, H0, lambda
da = d_proper_Mpc(z)/(1+z)
RETURN,da
END

;***********************************************************************
;***********************************************************************

FUNCTION dangarcmin,z
;COMPUTE ANGULAR DISTANCE IN MPC/ARCMIN GIVEN Z
common cosmology, q0, H0, lambda
dd = 2.908882E-4 * d_proper_Mpc(z)/(1+z)
RETURN,dd
END

;***********************************************************************
;***********************************************************************

pro plot_surflux, dat1, pix

; this procedure makes a surface brightness profile
; for a specified cluster
; Call as:
; e.g.:
; IDL> plot_surflux,'noncf_reference.list','5'

common cosmology, Omega_m, H0, lambda
pi = 3.14159265359

; take alternative assumption regarding geometry
Omega_m = 0.3
lambda = 0.7
H0 = 70.

; read the reference list
restore,"reflist_template.sav"
ref = read_ascii(dat1, template = reflist_template)

FOR i=0,n_elements(ref.obsid)-1 DO BEGIN
    name=strcompress(ref.cluster[i],/remove_all)
    obs=strcompress(ref.obsid[i],/remove_all)
    id=strcompress(ref.chipid[i],/remove_all)
    z=ref.z[i]
    a = 'i'
    IF (STRCMP(id,a,1) EQ 1 ) THEN id = 'i0123'
    filename = '../radprofs/'+pix+'pixs/'+name+'_'+obs+'_'+id+'_radprof_'+pix+'pix_norm.fits'

; format the cluster names
    cname = strcompress(ref.cluster,/remove_all)
    cname = str_replace(cname,'ABELL','Abell')
    cname = str_replace(cname,'_',' ')
    cname = str_replace(cname,'CENTAURUS','Centaurus')
    cname = str_replace(cname,'COMA','Coma')
    name = cname[i]

; read columns into variables
; and then make into rows
print, "Opening ", filename
fits = mrdfits(filename,1)
sfl = fits.sur_flux
rin = transpose(fits.r(0,*))
rout = transpose(fits.r(1,*))
sfl_err = transpose(fits.sur_flux_err)

; convert the radii into arcseconds
pixtoarcsec = 0.492
rin = pixtoarcsec * rin
rout = pixtoarcsec * rout

;convert from pixels to arcsec^2
sfl = sfl / (0.492)^2
sfl_err = sfl_err / (0.492)^2

; calculate the mid-radius
; essential if using a log plot
rmid = rin + rout/ 2.

; calculate radii in kpc
rinkpc = dangarcmin(z)* rmid * 1000. / 60.

; the easiest way to handle a 2nd x-axis
; is to read the first and last entries
; of a calculated array, then store those values
; as the min and max for the 2nd x-axis' range
xmin1 = rmid[0]
xnum1 = n_elements(rmid)
xmax1 = rmid[xnum1-1]

xmin2 = rinkpc[0]
xnum2 = n_elements(rinkpc)
xmax2 = rinkpc[xnum2-1]

print, xmin1, xmax1, " limits in arcsec"
print, xmin2, xmax2, " limits in kpc"

;ymin = min(sfl)-0.1*min(sfl)
;IF (ymin < 10e-11) THEN ymin=10e-11
;ymax = max(sfl)+0.1*max(sfl)

; plot the profile
set_plot, 'PS'
device, filename = strcompress(obs,/remove_all)+'_sfl_'+pix+'pix.ps'
!fancy = 4
!psym = 10
!linetype = 0
plot, rmid, sfl, /XLOG, /YLOG, $
      xtitle = "Radius (arcseconds)", $
      ytitle = "Surface Flux (cts/cm!E2!N/arcsec!E2!N/s)", $
      xrange = [xmin1,xmax1], $
      yrange = [ymin,ymax], $
      xstyle = 9, $
      xcharsize = 0.8, $
      ycharsize = 0.8

; draw the legend
items = [name, obs]
legend, items, box=0, charsize=0.8, /bottom, /left

; overplot the errors
oploterr, rmid, sfl, sfl_err, 1

AXIS, /SAVE, /XLOG, $
      xstyle = 9, $
      xaxis = 1, $
      xrange = [xmin2,xmax2], $
      xcharsize = 0.8, $
      xtitle = 'Radius (kpc)'

device, /close
set_plot, "X"

ENDFOR

; make all these ps files into one ps file
SPAWN, 'ls *sfl*.ps > list'
SPAWN, 'cat list | perl pscat temp.ps'
SPAWN, 'psnup -4 temp.ps '+pix+'pix_sfl_profs.ps'
SPAWN, 'rm -f temp.ps list'

END
