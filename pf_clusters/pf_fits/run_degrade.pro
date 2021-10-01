PRO run_degrade, in1

datadir = '/mnt/DROBO'
degdir  = 'degrade_tx'
redir   = 'reprocessed'
txfile  = '~/research/pf_clusters/pf_fits/dat/degraded_tx.dat'
moca    = 5000
altzs   = ['0.1', '0.12', '0.14', '0.16', '0.18', $
           '0.2', '0.22', '0.24', '0.26', '0.28', $
           '0.3', '0.32', '0.34', '0.36', '0.38']
restore,"~/research/redux/scripts/reflist_template.sav"
restore,'~/research/redux/scripts/s_resultstemplate.sav'
ref = read_ascii(in1, template = reflist_template)

obsids = ref.obsid
zs = ref.z
txs = ref.tx
names = ref.cluster
FOR i=0,n_elements(obsids)-1 DO BEGIN
    multi = 'no'
    obsid = strcompress(obsids[i],/remove_all)
    name = strcompress(names[i],/remove_all)
    tx = strcompress(txs[i],/remove_all)
    z = strcompress(zs[i],/remove_all)
    ord = where(names EQ name)
    IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
        temp = obsids[ord]
        FOR j=1,n_elements(temp)-1 DO BEGIN
            obsid = obsid+'_'+strcompress(temp[j],/remove_all)
        ENDFOR
        multi = 'yes'
    ENDELSE

    ;# check for fit file
    file = '~/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
    check = findfile(file,count=count)
    IF (count NE 1) THEN GOTO,ERROR

    ;# print the fitting range to use
    data = read_ascii(file, template = s_resultstemplate)
    rmin = data.rmin[0]
    rmax = data.rmax[0]
    print, format='(A-25,F10.3,F10.3)','Rmin and Rmax:',rmin,rmax

    FOR j=0,n_elements(altzs)-1 DO BEGIN
        bin = altzs[j]+'deg'
                                
        ;# check for existing fit
        file = degdir+'/'+obsid+'_'+altzs[j]+'deg_results.log'
        check = findfile(file,count=count)
        IF (count EQ 1) THEN BEGIN
           print, '## STATUS: Fit for ',file,' already exists, skipping...'
           GOTO,SKIP
        ENDIF

        IF multi EQ 'no' THEN BEGIN
            sbrfile = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_sbprof_'+bin+'.fits'
            excor   = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_expcorr_'+bin+'.fits'
        ENDIF ELSE BEGIN
            sbrfile = datadir+'/merged/'+name+'/'+obsid+'_sbprof_'+bin+'.fits'
            excor   = datadir+'/merged/'+name+'/'+obsid+'_expcorr_'+bin+'.fits'
        ENDELSE
        check  = findfile(sbrfile,count=count)
        IF (count NE 1) THEN BEGIN
           print, '## ERROR: ',sbrfile,' does not exist, skipping...'
           GOTO, SKIP
        ENDIF
        check  = findfile(excor,count=count)
        IF (count NE 1) THEN excor = 'nihil'
    
        ;# run the main fitting routine
        degrade, $
          sbrfile, $            ;# surface brightness profile
          txfile, $             ;# temperature fits
          name, $               ;# name of cluster
          tx, $                 ;# cluster temp
          z, $                  ;# redshift
          moca, $               ;# number of monte carlo iterations
          obsid, $              ;# obsid working with
          excor, $              ;# use exposure correction
          rmin, $
          rmax

        ;# move output to unique files
        check = findfile(obsid+'_results.log',count=count)
        IF (count EQ 1) THEN BEGIN
           SPAWN, 'mv -f '+obsid+'_results.log '+degdir+'/'+obsid+'_'+altzs[j]+'deg_results.log'
           SPAWN, 'mv -f '+obsid+'_table.dat '+degdir+'/'+obsid+'_'+altzs[j]+'deg_table.dat'
           SPAWN, 'mv -f '+obsid+'_splot.ps '+degdir+'/'+obsid+'_'+altzs[j]+'deg_splot.ps'
        ENDIF
        SKIP:
    ENDFOR
ERROR:
ENDFOR
END
