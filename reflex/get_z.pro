PRO get_z, in1
;get_z, 'download.list'

;# Region file format: CIAO version 1.0
;circle(12:29:51.710,+08:01:18.24,0.41789')

outfile = 'crossref.list'

ref = mrdfits('reflex_catalog.fits',1,hdr)
refaname = ref.alt_name
refname = ref.name
refflux = ref.flux
reflx = ref.lx
refz = ref.redshift
refra = ref.ra
refdec = ref.dec
refl = ref.LII
refb = ref.BII

readcol, in1, format='A,I,A,F,F,F,F,F,F,F', $
         name, obsid, instr, expt, ra1, ra2, ra3, dec1, dec2, dec3

openw, 1, outfile
printf, 1, format='(A-20,A10,A20,A20,A10,A12,A12,A10,A10,A10)',$
        '#MyNames',$
        'AltName',$
        'RName',$
        'Obsid',$
        'Delta',$
        'Flux',$
        'Lx',$
        'z',$
        'ra',$
        'dec'
FOR i=0,n_elements(name)-1 DO BEGIN
    tempname = name[i]
    tempobs = obsid[i]
    tempra = (ra1[i]+(ra2[i]/60.)+(ra3[i]/3600.))*(360./24.)
    tempdec = dec1[i]+(dec2[i]/60.)+(dec3[i]/3600.)
    euler, tempra, tempdec, templ, tempb, select=1
    diff = sqrt((templ-refl)^2.+(tempb-refb)^2.)
    ord = where(diff EQ min(diff))
    IF refaname[ord] EQ "" THEN aname = 'none' ELSE aname = refaname[ord]
    printf, 1, format='(A-20,A10,A20,A20,F10.4,E12.2,E12.2,F10.4,F10.4,F10.4)',$
            tempname,$
            tempobs,$
            aname,$
            refname[ord],$
            diff[ord],$
            refflux[ord],$
            reflx[ord],$
            refz[ord],$
            refra[ord],$
            refdec[ord]
    filename = '/Volumes/GALACTUS/'+strcompress(tempobs,/remove_all)+'/primary/'+strcompress(tempobs,/remove_all)+'_'+'reflexsrc.reg'
    close,2
    openw,2,filename
    radec,refra[ord],refdec[ord],ihr,imin,xsec,ideg,imn,xsc
    printf,2,"circle(",num2str(ihr[0]),":",num2str(imin[0]),":",num2str(xsec[0]),",",$
                       num2str(ideg[0]),":",num2str(imn[0]),":",num2str(xsc[0]),",10')"
    close,2
ENDFOR
close, 1
END
