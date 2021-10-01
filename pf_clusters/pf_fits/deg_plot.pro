PRO deg_plot, infile

;#################################
;#################################
;# usage:                       ;#
;# IDL> plot_deg, 'my.list'     ;#
                                ;#
ktype = 'flat'                  ;# type of K to use, 'flat' or 'itpl'
model = 'nonzero'               ;# type of model to use, 'zero' or 'nonzero'
dir   = 'degrade_tx'            ;# directory holding data files
makeall = 'yes'                 ;# make a composite plot of splots?
csize = 1.0                     ;# size of plotted characters
psize = 1.0                     ;# size of plotted symbols
myhome  = GETENV('HOME')        ;#
                                ;#
;#                              ;#
;#                              ;#
;#################################
;#################################

set_plot, 'PS'
loadct, 13, /silent
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
readcol, infile, $
         FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', $
         comment='#', $
         clusters, obsids, x, y, rmax, mincts, z, nh, $
         tx, fe, lbol, chip, eobs, diff, robs, loc

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# list of files
OPENW, LIST, 'list', /get_lun

;# start looping
prevname = 'jdfdjf'
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obsid = strcompress(obsids[i],/remove_all)
   name  = strcompress(clusters[i],/remove_all)
   IF name EQ prevname THEN GOTO,SKIPOBS
   ord = where(clusters EQ name)
   IF n_elements(ord) NE 1 THEN BEGIN
      temp  = strcompress(obsids[ord],/remove_all)
      obsid = strjoin(temp,'_',/single)
   ENDIF

   ;# get nominal data
   file  = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file,count=count)
   IF (count LT 1) THEN BEGIN
      print, '# ERROR: No ',file,' found.'
      GOTO,ERROR
   ENDIF
   odata = read_ascii(file, template = s_resultstemplate)

   ;# make one ps splot file per cluster
   IF makeall EQ 'yes' THEN BEGIN
      files = FILE_SEARCH(dir+'/'+obsid+'_*deg_splot.ps',count=count)
      IF (count LT 1) THEN BEGIN
         print, '# ERROR: No ',dir+'/'+obsid+'_*deg_splot.ps files found.'
         GOTO,ERROR
      ENDIF
      void, keys
      FOR j=0,n_elements(files)-1 DO BEGIN
         key = split('deg_',files[j])
         key = split('_',key[0])
         get = n_elements(key)-1
         push, keys, key[get]
      ENDFOR
      ord = sort(keys)
      files = files[ord]
      file0 = myhome+'/research/pf_clusters/pf_fits/plots/splots/'+obsid+'_splot.ps'
      SPAWN, 'ps2ps '+file0+' temp.ps'
      files = ['temp.ps', files]
      OPENW, LIST2, 'list2', /get_lun
      printf, LIST2, transpose(files)
      FREE_LUN, LIST2
      SPAWN, 'cat list2 | pscat 1 '+obsid+'_allsplots.ps'
      SPAWN, 'rm -f list2 temp.ps'
   ENDIF

    ;# find all files
    files = FILE_SEARCH(dir+'/'+obsid+'_*deg_results.log',count=count)
    IF (count LT 1) THEN BEGIN
       print, '# ERROR: No ',dir+'/'+obsid+'_*deg_results.log files found.'
       GOTO,ERROR
    ENDIF
    void, altzs
    void, k0
    void, k0err
    FOR j=0,n_elements(files)-1 DO BEGIN

        ;# name file
        file = files[j]
        check = findfile(file,count=count)
        IF (count NE 1) THEN GOTO,ERROR

        ;# read alternate z
        altz = split('deg_',file)
        altz = split('_',altz[0])
        get = n_elements(altz)-1
        push, altzs, altz[get]

        ;# read in data
        data = read_ascii(file, template = s_resultstemplate)

        ;# get data out
        push, k0, data.k0[ind]
        push, k0err, data.k0err[ind]
    ENDFOR
    ok0 = odata.k0[ind]
    ok0err = odata.k0err[ind]
    push, altzs, z[i]
    push, k0, ok0
    push, k0err, ok0err
    dk0 = (k0-ok0)/ok0

    ;# plot stuff
    xmin = 0.75*min(altzs)
    xmax = 1.1*max(altzs)
    ymin = 0.75*min(k0-k0err)
    ymax = 1.1*max(k0+k0err)
    output = obsid+'_degrade.ps'
    printf, LIST, output
    device, filename = output, $
            /portrait, $
            /helvetica
    multiplot,[1,2]
    plotsym, 0, psize, /fill
    plot, altzs, k0, $
          title = name+' '+obsid, $
          ytitle = textoidl('K_0 [keV cm^2]'), $
          xran = [xmin,xmax], $
          yran = [ymin,ymax], $
          /xsty, /ysty, $
          psym = 8, $
          charsize = csize
    oploterror, altzs, k0, k0err, psym=8
    oplot,[-1,5],[ok0,ok0],linestyle=2
    multiplot
    plot, altzs, dk0, $
          xtitle = textoidl('Redshift'), $
          ytitle = textoidl('dK_0'), $
          xran = [xmin,xmax], $
          yran = [-4,4], $
          /xsty, /ysty, $
          psym = 8, $
          charsize = csize
    oplot,[-1,5],[0,0],linestyle=2
    oplot,[-1,5],[1,1],linestyle=1
    oplot,[-1,5],[-1,-1],linestyle=1
    oplot,[-1,5],[2,2],linestyle=1
    oplot,[-1,5],[-2,-2],linestyle=1
    oplot,[-1,5],[3,3],linestyle=1
    oplot,[-1,5],[-3,-3],linestyle=1
    device, /close

    ;# keep all the data
    print, format='(A-20,A10,F10.3,A5,F10.3)',$
           name,'mean dK0:',mean(dk0),'+/-',stddev(dk0)/sqrt(n_elements(dk0))
    push, allaltzs, altzs
    push, allk0, k0
    push, alldk0, dk0
    multiplot, /reset

ERROR:
SKIPOBS:
    prevname = name
ENDFOR

;# stats
fidz = (findgen(15)*0.02)+0.1
void, x
void, y
FOR j=0,n_elements(fidz)-1 DO BEGIN
   z = fidz[j]
   ord = where((allaltzs LT 1.01*z) * (allaltzs GT 0.99*z))
   a = alldk0[ord]
   push, y, mean(a)
   push, yerr, stddev(a)/sqrt(n_elements(a))
   push, x, z
   print, format='(A-15,F-6.4,A1,F10.3,A5,F10.3)',$
          'Mean dK0 for',z,':',mean(a),'+/-',stddev(a)/sqrt(n_elements(a))
ENDFOR
device, filename = 'temp.ps', $
        /portrait, $
        /helvetica
xmin = 0.75*min(x)
xmax = 1.1*max(x)
ymin = 0.75*min(y-yerr)
ymax = 1.1*max(y+yerr)
plotsym, 0, psize, /fill
plot, x, y, $
      xtitle = textoidl('Redshift'), $
      ytitle = textoidl('Mean dK_0'), $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      /xsty, /ysty, $
      psym = 8, $
      charsize = csize
oploterr, x, y, yerr
device, /close
printf, LIST, 'temp.ps'

;# plot stuff
set_plot, 'PS'
device, filename = 'alldegrade.ps', $
  /portrait, $
  /helvetica
xmin = 0.75*min(allaltzs)
xmax = 1.1*max(allaltzs)
ymin = 0.75*min(allk0)
ymax = 1.1*max(allk0)
plotsym, 0, psize, /fill
plot, allaltzs, allk0, $
      xtitle = textoidl('Redshift'), $
      ytitle = textoidl('K_0 [keV cm^2]'), $
      xran = [xmin,xmax], $
      yran = [0.0,ymax], $
      /xsty, /ysty, $
      psym = 8, $
      charsize = csize
device, /close
free_lun, LIST
SPAWN, 'cat list | pscat 1 degrade.ps'
SPAWN, 'rm -f list'
SPAWN, 'rm -f *_degrade.ps temp.ps'
END
