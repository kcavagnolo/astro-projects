PRO load_ltcrv, reffile

datadir = 'reprocessed'
psfile = 'full_lc.ps'

; open the reference file
readcol, reffile, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster,obsids,x,y,rmax,mincts,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robs,loc

FOR i = 0, n_elements(obsids)-1 DO BEGIN
    obs    = strcompress(obsids[i],/remove_all)
    cname  = strcompress(cluster[i],/remove_all)
    rootdir = loc[i]
    ltfile = rootdir+'/'+obs+'/'+datadir+'/'+obs+'_lc.fits'
    data = mrdfits(ltfile,1)
    dtime = data.time
    dctr = data.count_rate
    push, time, dtime
    push, ctr, dctr
    ord = where(dctr GT 0)
    push, meanctr, mean(dctr[ord])
    push, meant, mean(dtime[ord])
ENDFOR

; plot labels
header = "Cluster: "+cname
xtx = textoidl("Time (sec)")
ytx = textoidl("Count Rate (cts/sec)")

; make a hardcopy
set_plot, 'PS'
loadct, 13, /silent
device, filename=psfile, $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
ymin = 0.5
ymax = 1.5
xmin = min(time)
xmax = max(time)
plotsym, 0, 0.8, /fill
plot, time, ctr, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      psym = 8, $
      title  = header, $
      xtitle = xtx, $
      ytitle = ytx, $
      charsize = 1.0
oplot, meant, meanctr, psym=8, color=250
device, /close
END
