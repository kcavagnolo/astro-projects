PRO tc10k, dat1

tcbin = 0.12
myhome = GETENV('HOME')
restore,myhome+'/research/redux/scripts/tcooltemplate.sav'
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster,obsids,x,y,rmax,mincts,z,nh,tx,fe,lbol,chip,eobs,diff,robs,loc

;# begin loop through each cluster
FOR i = 0,n_elements(obsids)-1 DO BEGIN
    obsid = strcompress(obsids[i],/remove_all)
    name = strcompress(cluster[i],/remove_all)
    ord = where(cluster EQ name)
    IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
        temp = obsids[ord]
        FOR j=1,n_elements(temp)-1 DO BEGIN
            obsid = obsid+'_'+strcompress(temp[j],/remove_all)
        ENDFOR
    ENDELSE

    ;# check for file existance
    file = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_tcool.dat'
    check = findfile(file,count=count)
    IF (count NE 1) THEN GOTO,ERROR

    datacool = read_ascii(file, template = tcooltemplate)
    tc = datacool.tc52
    tcerr = datacool.tc52err
    r = (datacool.rin)*1000.
    r = r+0.1
    ord = where((tc EQ tc) AND (r LE 200))
    tc = tc[ord]
    tcerr = tcerr[ord]
    r = r[ord]
    rint = interpol(r,10000)
    tcint = interpol(tc,r,rint)
    ord = where((rint GT 9.9) AND (rint LT 10.1))
    n = ord[0]
;    push, alltc10, tcint[n]
    push, alltc10, tcint[0]
ERROR:
ENDFOR

;# make bins for cum prof
tx = ""
tmin = min(alltc10)
tmax = max(alltc10)
iter = tmin/2.
total = float(n_elements(alltc10))
print, '## STATUS: Working on cumulative profile...'
WHILE iter LE tmax DO BEGIN
   num = float(n_elements(where(alltc10 LE iter)))
   push, cumpr, num/total
   push, tx, iter
   iter = iter + 0.05
ENDWHILE
print, 'Done.'
push, tx, tmax
push, cumpr, 1.0
cxmin = 0.8*min(tx)
cxmax = 1.2*max(tx)
cymin = 0.8*min(cumpr)
cymax = 1.1
push, tx, cxmax
push, cumpr, 1.0

;# histogram of tcool dist.
;# setup the fancy plotting options
set_plot, 'PS'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
multiplot,[1,2]
xmin = 0.8*min(alltc10)
xmax = 1.2*max(alltc10)
device, filename='tc10.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
histoplot, alltc10, tcbin, $
  /log, $
  /xsty, xrange = [xmin,xmax], $
  ytitle = textoidl('Number of Clusters'), $
  charsize = 0.8
;items = [textoidl('t_{cool} at 10 kpc')]
;linearr = replicate(-99,n_elements(items))
;psyarr = replicate(-99,n_elements(items))
;legend, items, linestyle=linearr,psym=psyarr,/top,box=0,/right_legend,charsize=0.8

;# cumulative plot
xtex = textoidl('t_{cool} [Gyr]')
ytex = textoidl('Fractional number of clusters')
multiplot
plot, tx, cumpr, $
  linestyle = 0, $
  xrange = [xmin,xmax], $
  yrange = [cymin,cymax],$
  xtitle = xtex, $
  ytitle = ytex, $
  /xsty, /ysty, $
  /xlog, $
  charsize = 0.8, $
  psym = 10
device, /close
multiplot,/reset

END
