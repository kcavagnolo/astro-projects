pro disp_imgs, reffile

fsigno = '5d0'
signo = '30d0'

; open the reference file
restore, "~/research/redux/scripts/reflist_template.sav"
ref = read_ascii(reffile, template = reflist_template)

FOR i=0,n_elements(ref.obsid)-1 DO BEGIN
    obs = strcompress(ref.obsid[i],/remove_all)
    name = strcompress(ref.cluster[i],/remove_all)
    rootdir = '/Volumes/GALACTUS/'+obs+'/tempmap/'+signo+'/'
    cts     = rootdir+obs+'_cts.fits'
    flux    = rootdir+obs+'_'+fsigno+'_flux_abinned.fits'
    tmap    = rootdir+obs+'_'+signo+'_tempmap.fits'
    tlomap  = rootdir+obs+'_'+signo+'_tempmap_tlo.fits'
    thimap  = rootdir+obs+'_'+signo+'_tempmap_thi.fits'
    intmap  = rootdir+obs+'_'+signo+'_tempmap_interpol.fits'
    femap   = rootdir+obs+'_'+signo+'_tempmap_fe.fits'
    felomap = rootdir+obs+'_'+signo+'_tempmap_felo.fits'
    fehimap = rootdir+obs+'_'+signo+'_tempmap_fehi.fits'
    chimap  = rootdir+obs+'_'+signo+'_tempmap_chi.fits'
ENDFOR

atv, chimap, /block
atv, tmap, /block
atv, intmap, /block
atv, femap, /block

END
