; this procedure will plot a temperature profile
; and create a postscript for the profile

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

pro plot_txpr, prfile, reffile
; plot_txpr, '1tpr_nhfrozen_fefree.dat', 'reference_all.list'

; this procedure makes a temperature profile
; for a specified cluster

common cosmology, Omega_m, H0, lambda
pi = 3.14159265359

; take alternative assumption regarding geometry
Omega_m = 0.3
lambda = 0.7
H0 = 70.

; read the fit files
restore,"../../scripts/reflist_template.sav"
ref = read_ascii(reffile, template = reflist_template)
restore, '../../scripts/xspecfittemplate_ext.sav'
prfit = read_ascii(prfile,template=xspecfittemplate_ext)

rawname = strcompress(ref.cluster,/remove_all)
cname = str_replace(rawname,'ABELL','Abell')
cname = str_replace(cname,'_',' ')
cname = str_replace(cname,'HYDRA','Hydra')
cname = str_replace(cname,'CYGNUS','Cygnus')
cname = str_replace(cname,'OPHIUCHUS','Ophiuchus')

FOR i = 0, n_elements(ref.obsid)-1 DO BEGIN
    obs = strcompress(ref.obsid[i],/remove_all)
    name = cname[i]
    rname = rawname[i]
    chipid = ref.chip[i]
    z = ref.z[i]

    CATCH, error_status
    IF (error_status NE 0) THEN BEGIN
        GOTO, DONE_VAL
    ENDIF

    prtx     = prfit.tx[where(prfit.obsid EQ obs)]
    prtlo    = prfit.tlo[where(prfit.obsid EQ obs)]
    prterrlo = prtx-prtlo
    prthi    = prfit.thi[where(prfit.obsid EQ obs)]
    prterrhi = prthi-prtx

    rout = prfit.rout[where(prfit.obsid EQ obs)]
    rin  = prfit.rin[where(prfit.obsid EQ obs)]

; convert the radii into arcseconds
    rin = rin * 60
    rout = rout * 60

; calculate the mid-radius
; essential if using a log plot
    rmid = (rin + rout)/ 2.

; calculate radii in kpc
    rinkpc = dangarcmin(z)* rmid * 1000. / 60.

; the easiest way to handle a 2nd x-axis
; is to read the first and last entries
; of a calculated array, then store those values
; as the min and max for the 2nd x-axis' range
    min1 = 0.
    num1 = n_elements(rmid)
    max1 = rmid[num1-1] + 10.

    min2 = 0.
    num2 = n_elements(rinkpc)
    max2 = rinkpc[num2-1] + (dangarcmin(z)* 10. * 1000. / 60.)

    ymin = 0.
    IF (max(prthi) GT max(prthi)) THEN ythi=max(prthi) ELSE ythi=max(prthi)
    ymax = ythi+1.

;    print, min1, max1, " limits in arcsec"
;    print, min2, max2, " limits in kpc"

; define the labels for the plot axes
    ytx = textoidl('T_X [keV]')
    xtx = textoidl('R_{mid} [kpc]')

; plot the profile
    set_plot, 'PS'
    device, filename=obs+'_prtxprof.ps'
    !fancy = 4
    !p.font = 0
    !linetype = 0
    plot, rinkpc, prtx, $
      xtitle = xtx, $
      ytitle = ytx, $
      xrange = [min2,max2], $
      yrange = [ymin,ymax], $
      /xsty, $
      /ysty, $
      psym = -1, $
      symsize = 0.6, $
      charsize = 0.6

; draw the legend
    items = [name, obs]
    legend, items, box=0, charsize=0.8, /bottom, /left

; overplot the errors
    oploterror, rinkpc, prtx, prterrlo, psym=2, symsize=0.6, /lobar
    oploterror, rinkpc, prtx, prterrhi, psym=2, symsize=0.6, /hibar

;    AXIS, /SAVE, $
;      xstyle = 9, $
;      xaxis = 1, $
;      xrange = [min2,max2], $
;      xcharsize = 0.6, $
;      xtitle = 'R (kpc)'

DONE_VAL:
ENDFOR

device, /close
set_plot, 'x'

; make all these ps files into one ps file
SPAWN, 'ls *prtxprof*.ps > list'
SPAWN, 'cat list | perl ../../scripts/pscat.pl 1 all.ps'
SPAWN, 'rm -f temp list *prtxprof*.ps'
END
