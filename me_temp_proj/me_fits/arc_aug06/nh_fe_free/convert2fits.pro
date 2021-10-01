pro convert2fits, input, output, opt

; ie:
; convert2fits, '1tpr_nhfrozen_fefree.dat', '1tpr_nhfrozen_fefree.fits', 1

; restore the fit template and read some variables
IF (opt EQ 1) THEN BEGIN
    restore,"xspecfittemplate.sav"
    fit = read_ascii(input, template = xspecfittemplate)
    arr = {cluster:fit.cluster, $
           obsid:fit.obsid, $
           rin:fit.rin, $
           rout:fit.rout, $
           nh:fit.nh, $
           nlo:fit.nlo, $
           nhi:fit.nhi, $
           tx:fit.tx, $
           tlo:fit.tlo, $
           thi:fit.thi, $
           fe:fit.fe, $
           felo:fit.felo, $
           fehi:fit.fehi, $
           norm:fit.norm, $
           tx2:fit.tx2, $
           tlo2:fit.tlo2, $
           thi2:fit.thi2, $
           norm2:fit.norm2, $
           z:fit.z, $
           cr:fit.cr, $
           chisq:fit.chisq, $
           dof:fit.dof}
ENDIF ELSE IF (opt EQ 2) THEN BEGIN
    restore,"xspecfittemplate_ext.sav"
    fit = read_ascii(input, template = xspecfittemplate_ext)
    arr = {cluster:fit.cluster, $
           obsid:fit.obsid, $
           rin:fit.rin, $
           rout:fit.rout, $
           nh:fit.nh, $
           nlo:fit.nlo, $
           nhi:fit.nhi, $
           tx:fit.tx, $
           tlo:fit.tlo, $
           thi:fit.thi, $
           fe:fit.fe, $
           felo:fit.felo, $
           fehi:fit.fehi, $
           norm:fit.norm, $
           normlo:fit.normlo, $
           normhi:fit.normhi, $
           tx2:fit.tx2, $
           tlo2:fit.tlo2, $
           thi2:fit.thi2, $
           norm2:fit.norm2, $
           normlo2:fit.normlo2, $
           normhi2:fit.normhi2, $
           z:fit.z, $
           cr:fit.cr, $
           chisq:fit.chisq, $
           dof:fit.dof}
ENDIF ELSE IF (opt EQ 3) THEN BEGIN
    restore,"reflist_template.sav"
    fit = read_ascii(input, template = reflist_template)
    arr = {cluster:fit.cluster, $
           obsid:fit.obsid, $
           x:fit.ra, $
           y:fit.dec, $
           rmax:fit.rmax, $
           mincts:fit.mincts, $
           z:fit.z, $
           nh:fit.galnh, $
           tx:fit.tx, $
           fe:fit.fe, $
           lbol:fit.lbol, $
           chip:fit.chipid}
ENDIF

mwrfits,arr,output,/create

end
