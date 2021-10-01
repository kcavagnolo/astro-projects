PRO mkfits, output

dat1 = 'fakset_nhfro_fefree_7-7.dat'
dat2 = 'fakset_nhfro_fefree_2-7.dat'
restore,"../scripts/xspectemp_rin_normerr_src.sav"
full = read_ascii(dat1, template = xspectemp_rin_normerr_src)
hard = read_ascii(dat2, template = xspectemp_rin_normerr_src)

FOR i = 0,n_elements(full.obsid)-1 DO BEGIN
    arr = {cluster:full.cluster[i], $
           obsid:full.obsid[i], $
           nh:full.nh[i], $
           nlo:full.nlo[i], $
           nhi:full.nhi[i], $
           z:full.z[i], $
           src:full.src[i], $
           tx2:full.rin[i], $
           xi:full.rout[i], $
           cr77:full.cr[i], $
           cr27:hard.cr[i], $
           tx77:full.tx[i], $
           tx77lo:full.tlo[i], $
           tx77hi:full.thi[i], $
           tx27:hard.tx[i], $
           tx27lo:hard.tlo[i], $
           tx27hi:hard.thi[i], $
           fe77:full.fe[i], $
           fe77lo:full.felo[i], $
           fe77hi:full.fehi[i], $
           fe27:hard.fe[i], $
           fe27lo:hard.felo[i], $
           fe27hi:hard.fehi[i], $
           norm77:full.norm[i], $
           norm77lo:full.normlo[i], $
           norm77hi:full.normhi[i], $
           norm27:hard.norm[i], $
           norm27lo:hard.normlo[i], $
           norm27hi:hard.normhi[i], $
           chi77:full.chisq[i], $
           chi27:hard.chisq[i], $
           dof77:full.dof[i], $
           dof27:hard.dof[i]}

    ;# push this array into a master array
    push, ext1arr, arr
ENDFOR

;# get a timestamp
get_date,date,/timetag

;# write the header for extension1
;# in general
sxaddpar, hdr1, 'COMMENT', '----------------------------------------'
sxaddpar, hdr1, 'COMMENT', 'Input spectrum params:'
sxaddpar, hdr1, 'COMMENT', 'NH = 3.0x10**20'
sxaddpar, hdr1, 'COMMENT', 'Tx = 5.0 keV'
sxaddpar, hdr1, 'COMMENT', 'Fe = 0.3 Solar'
sxaddpar, hdr1, 'COMMENT', 'redshift = 0.1'
sxaddpar, hdr1, 'COMMENT', 'counts = 30000'
sxaddpar, hdr1, 'COMMENT', '77 refers to [0.7-7.0]keV bandpass'
sxaddpar, hdr1, 'COMMENT', '27 refers to [2.0-7.0]keV bandpass'
sxaddpar, hdr1, 'COMMENT', 'If NLO and NHI equal zero, NH was frozen'
sxaddpar, hdr1, 'COMMENT', 'to the Galactic column density'
sxaddpar, hdr1, 'COMMENT', 'taken from Dickey and Lockman 1990'
sxaddpar, hdr1, 'COMMENT', '----------------------------------------'
sxaddpar, hdr1, 'EXTNAME', 'fake', 'Faked spectra'
sxaddpar, hdr1, 'MOD_DATE', date, 'File modification date.'
sxaddpar, hdr1, 'COMMENT', '----------------------------------------'
sxaddpar, hdr1, 'CLUSTER', 'CDA name of cluster'
sxaddpar, hdr1, 'OBSID', 'Observation Identification number'
sxaddpar, hdr1, 'EXPTIME', 'Nominal exposure time'
sxaddpar, hdr1, 'NH', 'Absorbing Hydr column density'
sxaddpar, hdr1, 'NLO', 'Lower bound of 90% confidence on NH'
sxaddpar, hdr1, 'NHI', 'Upper bound of 90% confidence on NH'
sxaddpar, hdr1, 'TXREF', 'Literature Tx'
sxaddpar, hdr1, 'Z', 'Redshift of cluster;# taken from NED'
sxaddpar, hdr1, 'SRC', 'Percent of total count rate which is source'
sxaddpar, hdr1, 'XI', '% of total cnt rate from TX2'
sxaddpar, hdr1, 'TX2', 'Tx of 2nd temp. component'
sxaddpar, hdr1, 'CR77', 'Count rate for given energy band'
sxaddpar, hdr1, 'TX77', 'Best-fit X-Ray temperature'
sxaddpar, hdr1, 'TX77LO', 'Lower bound of 90% confidence on tx'
sxaddpar, hdr1, 'TX77HI', 'Upper bound of 90% confidence on tx'
sxaddpar, hdr1, 'FE77', 'Best-fit X-Ray metallicity'
sxaddpar, hdr1, 'FE77LO', 'Lower bound of 90% confidence on metallicity'
sxaddpar, hdr1, 'FE77HI', 'Upper bound of 90% confidence on metallicity'
sxaddpar, hdr1, 'NORM77', 'Normalization factor'
sxaddpar, hdr1, 'NORM77LO', 'Lower bound of 90% confidence on normalization'
sxaddpar, hdr1, 'NORM77HI', 'Upper bound of 90% confidence on normalization'
sxaddpar, hdr1, 'CHI77', 'Reduced chi-square'
sxaddpar, hdr1, 'DOF77', 'Degrees of freedom'
sxaddpar, hdr1, 'CR27', 'Count rate for given energy band'
sxaddpar, hdr1, 'TX27', 'Best-fit X-Ray temperature'
sxaddpar, hdr1, 'TX27LO', 'Lower bound of 90% confidence on tx'
sxaddpar, hdr1, 'TX27HI', 'Upper bound of 90% confidence on tx'
sxaddpar, hdr1, 'FE27', 'Best-fit X-Ray metallicity'
sxaddpar, hdr1, 'FE27LO', 'Lower bound of 90% confidence on metallicity'
sxaddpar, hdr1, 'FE27HI', 'Upper bound of 90% confidence on metallicity'
sxaddpar, hdr1, 'NORM27', 'Normalization factor'
sxaddpar, hdr1, 'NORM27LO', 'Lower bound of 90% confidence on normalization'
sxaddpar, hdr1, 'NORM27HI', 'Upper bound of 90% confidence on normalization'
sxaddpar, hdr1, 'CHI27', 'Reduced chi-square'
sxaddpar, hdr1, 'DOF27', 'Degrees of freedom'

;# add to the FITS file
mwrfits, ext1arr, output, hdr1, /create

END
