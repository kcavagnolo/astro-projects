PRO run_kfit, in1

myhome  = GETENV('HOME')
usefake = 'no'
common fakepars, fakesbr
fakesbr = 'fakmarx/sims/z1_tx1_sbprof_10pix.fits'
redir   = 'reprocessed'
;txfile  = myhome+'/research/pf_clusters/pf_fits/dat/pf_temp_profs.dat'
;txfile  = myhome+'/research/pf_clusters/pf_fits/dat/degraded_tx.dat'
;txfile  = myhome+'/research/iras09/data/tx_2.5K/proj_1T_nhfro.dat'
txfile  = myhome+'/research/rbs_797/data/tx_2.5K/proj_1T_nhfro.dat'
moca    = 5000 

;# if using fake data
IF usefake EQ 'yes' THEN BEGIN
   head = headfits(fakesbr,ext=1)
   faketx = sxpar(head,'FAKTX')
   fakez = sxpar(head,'FAKZ')
   print, "## STATUS: Running kfitter w/ MOCK observation"
   kfitter, $
      fakesbr, $                ; surface brightness profile
      txfile, $                 ; temperature fits
      'fake', $                 ; name of cluster
      faketx, $                 ; cluster temp
      fakez, $                  ; redshift
      moca, $                   ; number of monte carlo iterations
      '0000', $                 ; obsid working with
      'nihil', $                ; use exposure correction
      'yes'                     ; use fake tx profile?
ENDIF ELSE BEGIN

   ;# load the template files
   readcol, in1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
            names, obsids, x, y, rmax, mincts, zs, nh, txs, fe, lbol, chip, $
            eobs, diff, robs, locs
   ;# start the looping
   FOR i=0,n_elements(obsids)-1 DO BEGIN
      multi = 'no'
      obsid = strcompress(obsids[i],/remove_all)
      name = strcompress(names[i],/remove_all)
      tx = strcompress(txs[i],/remove_all)
      z = strcompress(zs[i],/remove_all)
      datadir = strcompress(locs[i],/remove_all)
      ord = where(names EQ name)
      IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
         temp = obsids[ord]
         FOR j=1,n_elements(temp)-1 DO BEGIN
            obsid = obsid+'_'+strcompress(temp[j],/remove_all)
         ENDFOR
         multi = 'yes'
      ENDELSE
      bin = ''
      read,'Enter bin size: ',bin
      IF multi EQ 'no' THEN BEGIN
         sbrfile = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_sbprof_'+bin+'.fits'
         excor   = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_expcorr_'+bin+'.fits'
      ENDIF ELSE BEGIN
         sbrfile = datadir+'/merged/'+name+'/'+obsid+'_sbprof_'+bin+'.fits'
         excor   = datadir+'/merged/'+name+'/'+obsid+'_expcorr_'+bin+'.fits'
      ENDELSE
      check  = findfile(excor,count=count)
      IF (count NE 1) THEN BEGIN
         excor = 'nihil'
         print, "## WARNING: No exposure correction for this observation"
      ENDIF

      ;# run the commands
      kfitter, $
         sbrfile, $             ; surface brightness profile
         txfile, $              ; temperature fits
         name, $                ; name of cluster
         tx, $                  ; cluster temp
         z, $                   ; redshift
         moca, $                ; number of monte carlo iterations
         obsid, $               ; obsid working with
         excor, $               ; use exposure correction
         'no'                   ; use fake tx profile?
   ENDFOR
ENDELSE
END
