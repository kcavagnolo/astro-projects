pro multi_tx, reffile

; multi_tx, 'reference_all.fits'

ref = mrdfits(reffile,1,hdr)
rawname = strcompress(ref.cluster,/remove_all)
cname = str_replace(rawname,'ABELL','Abell')
cname = str_replace(cname,'_',' ')
cname = str_replace(cname,'HYDRA','Hydra')
cname = str_replace(cname,'CYGNUS','Cygnus')
cname = str_replace(cname,'OPHIUCHUS','Ophiuchus')

set_plot, 'ps'
device, /encapsulated, filename='tx_prof.ps'
!p.multi = [0,3,3,0]
!x.margin = [10,2]
!y.margin = [5,2]

j = 0
FOR i = 0, n_elements(ref.obsid)-1 DO BEGIN
    obs = strcompress(ref.obsid[i],/remove_all)
    name = cname[i]
    rname = rawname[i]
    chipid = ref.chip[i]
    z = ref.z[i]

    prfilename = '1tpr_nhfrozen_fefree.fits'
    defilename = '1tde_nhfrozen_fetied.fits'
    title = name
    print, prfilename
    print, defilename
    print, title
    print, obs

    IF (j GT 9) THEN BEGIN
        ERASE
        j = 0
    ENDIF

    plot_tx, prfilename, defilename, z, title, obs
    j = j+1

ENDFOR

device, /close
set_plot, 'x'

END
