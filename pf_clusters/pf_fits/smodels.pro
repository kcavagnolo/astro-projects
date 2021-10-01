pro smodels, dat1
;# splots, 'reference.list'
  
;#################################
;#################################
alpha     = -0.178              ;# the alpha in k/t^alpha
plpts     = "no"                ;# plot the lines w/ points?
ktype     = "flat"              ;# type of K to use, "flat" or "itpl"
model     = "nonzero"           ;# type of model to use, "zero" or "nonzero"
output    = "smodels.eps"       ;# name of postscript
incleg    = "no"                ;# add legend to plot or make separate
outputkey = "key.ps"            ;# name of key postscript
;#################################
;#################################

;# set plot ranges
xmin = 0.25
xmax = 2000.
ymin = 2.
ymax = 3000.

;# restore the fit template and read some variables
restore,"~/research/redux/scripts/s_tabletemplate.sav"
restore,"~/research/redux/scripts/s_resultstemplate.sav"
restore,"~/research/pf_clusters/pf_fits/dat/purecool.sav"
pure = read_ascii("~/research/pf_clusters/pf_fits/dat/krprof_6_017.dat",template=template)
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         clusters,obsids,xs,ys,rmaxs,minctss,zs,nhs,$
         txs,fes,lbols,chips,eobss,diffs,robss,locs

prevname = 'lkjsdjfsdf'
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obs = strcompress(obsids[i],/remove_all)
   name = strcompress(clusters[i],/remove_all)
   IF name EQ prevname THEN GOTO,SKIP
   ord = where(clusters EQ name)
   IF n_elements(ord) NE 1 THEN BEGIN
      temp = strcompress(obsids[ord],/remove_all)
      obs  = strjoin(temp,'_',/single)
   ENDIF
   push, obsid, obs
   push, tx, txs[i]
   push, cluster, clusters[i]
   push, z, zs[i]
SKIP:
   prevname = name
ENDFOR

;# sort by tx                                                                                                         
s = sort(tx)
obs_or = obsid[s]
cluster = cluster[s]
tx_or = tx[s]
z_or = z[s]

;# format names
cluster = strcompress(cluster,/remove_all)
cluster = str_replace(cluster,'ABELL','Abell')
cluster = str_replace(cluster,'CENTAURUS','Centaurus')
cluster = str_replace(cluster,'HERCULES','Hercules')
cluster = str_replace(cluster,'HYDRA','Hercules')
cluster = str_replace(cluster,'OPHIUCHUS','Ophiuchus')
cluster = str_replace(cluster,'SERSIC','Sersic')
cluster = str_replace(cluster,'ZWICKY','Zwicky')
cluster = str_replace(cluster,'_0',' ')
cluster = str_replace(cluster,'_',' ')

;# define the color array
color = maken(20,250,n_elements(obsid))

;# get the cooling profile data
rpure = pure.r*1800.
kpure = pure.s*362.
tpure = 5.

;## get values specific to model
IF (ktype EQ "flat") THEN BEGIN
   IF (model EQ "nonzero") THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ "itpl") THEN BEGIN
   IF (model EQ "nonzero") THEN ind = 0 ELSE ind = 1
ENDIF

;# set up empty plot
set_plot, 'PS'
device, filename = output, $
        /color, $
        /encapsulated, $
        /helvetica, $
        bits=16
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!P.CHARTHICK = 1
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
dumx = [xmin,xmax]
dumy = [ymin,ymax]
csize = 1.0
lsize = csize * 0.40
thick = 6
xtitle = textoidl('R [kpc]')
ytitle = textoidl('K [keV cm^{2}]')

plot, dumx, dumy, /nodata, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      linestyle = 0, $
      xtitle = xtitle, $
      ytitle = ytitle, $
      /xsty, /ysty, $
      /ylog, /xlog, $
      charsize = csize

yresmax = 0.
yresmin = 0.
xresmax = 0.
xresmin = 0.
FOR i = 0,n_elements(obs_or)-1 DO BEGIN
    obs = strcompress(obs_or[i],/remove_all)
    kt = tx_or[i]
    red = z_or[i]

    file = 'tables/'+obs+'_table.dat'
    check = findfile(file,count=count)
    IF (count NE 1) THEN BEGIN
       print, '## ERROR: No ',file,' found.'
       GOTO,ERROR
    ENDIF
    obsdata = read_ascii(file, template = s_tabletemplate)
    obsr = obsdata.rin_mpc*1000.
;    obsr = (obsdata.rin_mpc+obsdata.rout_mpc)/2.*1000.
    IF (ktype EQ "flat") THEN obsk = obsdata.k_flat ELSE obsk = obsdata.k
    kerr = obsdata.k_err
    kerr = kerr[where(obsk EQ obsk)]
    obsr = obsr[where(obsk EQ obsk)]
    obsk = obsk[where(obsk EQ obsk)]

    ;# define filename and check for it
    file = 's_results/'+obs+'_results.log'
    check = findfile(file,count=count)
    IF (count NE 1) THEN GOTO,ERROR
    data = read_ascii(file, template = s_resultstemplate)

    ;# define value arrays
    chisq = data.chisq
    k0 = data.k0
    k0err = data.k0err
    k100 = data.k100
    plaw = data.plaw
    rmean = maken(1,1000,1000)
    r200 = rdelta(200,red,kt,/silent)*1000.
    chisq = chisq[ind]
    k0 = k0[ind]
    k0err = k0err[ind]
    k100 = k100[ind]
    plaw = plaw[ind]
    k = k0 + k100*(rmean/100)^plaw
    kr = k0 + k100*(obsr/100)^plaw
    tempk = kr-obsk
    num = n_elements(tempk)
    tempsig = abs(k0-obsk[num-1])/kerr[num-1]
    tempr = obsr/min(obsr)
    IF yresmax LT max(tempk) THEN yresmax = max(tempk)
    IF yresmin LT min(tempk) THEN yresmin = min(tempk)
    IF xresmax LT max(tempk) THEN xresmax = max(tempk)
    IF xresmin LT min(tempk) THEN xresmin = min(tempk)
    push, kresid, ptr_new(tempk)
    push, kresidr, ptr_new(tempr)
    push, allkresid, tempk[num-1]
    push, allsig, tempsig
    push, allchi, chisq
;    IF tempsig GT 10 THEN $
;    print,'>>>>>>>>>>> '+cluster[i]+'    '+obs+'   '+num2str(obsk[num-1])+'   '+num2str(k0)+'   '+num2str(kerr[num-1])+'   '+num2str(tempsig)
;    IF (abs(tempk[num-1])/k0 GT 0.50) THEN $
;       print, 'Bad fit? ',cluster[i],' -- ',obs,' -- ',tempk[num-1]
;    IF k0 LT 1. THEN print, '>>>>>>>>>>> '+cluster[i]
;    IF k0 LT 20. AND tempk[num-1] LT -40 THEN print, '>>>>>>>>>>> '+cluster[i]

    ;# store all values for later
    push, chisqall, chisq
    push, k0all, k0
    push, k100all, k100
    push, plawall, plaw
    push, cname, cluster[i]

    ;# get entropy profile for each cluster
    loadct,39,/silent
    plcolor = color[i]
    IF (n_elements(legcolor) EQ 0) THEN BEGIN
        legcolor = plcolor
    ENDIF ELSE BEGIN
        legcolor = [legcolor, plcolor]
    ENDELSE
    symbols = [0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8]
    plotsym, symbols[i],0.5,/fill
    r = rmean
    IF plpts EQ "yes" THEN oplot, r, k, psym=8, color=plcolor
    oplot, r, k, psym=0, color=plcolor, thick=1.2

ERROR:
    IF (count NE 1) THEN print, "## ERROR: ",file," does not exist, exiting."
ENDFOR

;# plot the pure cooling model
r = rpure
k = kpure*tpure
oplot, r, k, linestyle=2, psym=0

;# draw the legend for pure cooling model
items = ["Pure Cooling Model"]
linearr = [2]
psyarr = [0]
thiarr = [2.0]
legend, items, linestyle=linearr, psym=psyarr, thick=thiarr, charthick=1.0, charsize=0.8, /top, /left_legend
IF incleg EQ "yes" THEN legend, cname+textoidl(' (T_{X}= ')+strcompress(sigfig(tx_or,3),/remove_all)+'keV)', $
  psym=intarr(n_elements(cname))+8, /usersym, color=legcolor, charsize=0.8, /bottom,/right,/fill

device,/close

;# cut long legend into two pieces
FOR i=0,n_elements(cname)/2 DO BEGIN
    push,cname1,cname[i]
    push,txor1,tx_or[i]
    push,lcolor1,legcolor[i]
ENDFOR
FOR i=n_elements(cname)/2+1,n_elements(cname)-1  DO BEGIN
    push,cname2,cname[i]
    push,txor2,tx_or[i]
    push,lcolor2,legcolor[i]
ENDFOR

;# draw a separate key for the plot
set_plot, 'PS'
device, filename = outputkey, /encapsulated, /color
!fancy = 4
!linetype = 0
!p.font=0
legend, cname1+textoidl(' (T_{X}= ')+strcompress(sigfig(txor1,3),/remove_all)+'keV)', $
  psym=intarr(n_elements(cname1))+8, /usersym, color=lcolor1, charsize=0.9, /bottom,/left,/fill
legend, cname2+textoidl(' (T_{X}= ')+strcompress(sigfig(txor2,3),/remove_all)+'keV)', $
  psym=intarr(n_elements(cname2))+8, /usersym, color=lcolor2, charsize=0.9, /bottom,/right,/fill
device,/close
set_plot,"X"

;# calculate mean and std dev for each
k0mean = total(k0all)/n_elements(k0all)
k0dev = sqrt(total((k0mean-k0all)^2)/(n_elements(k0all)-1))
print, "<K0> = ",num2str(k0mean,4)," +/- ",num2str(k0dev,4)

k100mean = total(k100all)/n_elements(k100all)
k100dev = sqrt(total((k100mean-k100all)^2)/(n_elements(k100all)-1))
print, "<K100> = ",num2str(k100mean,4)," +/- ",num2str(k100dev,4)

plawmean = total(plawall)/n_elements(plawall)
plawdev = sqrt(total((plawmean-plawall)^2)/(n_elements(plawall)-1))
print, "<Plaw> = ",num2str(plawmean,4)," +/- ",num2str(plawdev,4)

;# Plot, Johnny!, PLOOOOTTTT!!!
set_plot, 'PS'
device, filename='kresid.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica, $
        bits = 16
plot, [xresmin,xresmax], [yresmin,yresmax], /nodata, $
      xrange = [xresmin,xresmax], $
      yrange = [yresmin,yresmax], $
      linestyle = 0, $
      xtitle = xtitle, $
      ytitle = textoidl('BF K - Obs K'), $
      /xlog, /ylog, $
      /xsty, /ysty, $
      charsize = csize
FOR j=0,n_elements(kresid)-1 DO BEGIN
;   pcolor = allcolor[j]
   x = *kresidr[j]
   y = *kresid[j]
   oplot, x, y, psym=1;, thick=2, color=pcolor
ENDFOR
device, /close

set_plot, 'PS'
device, filename='histkresid.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica, $
        bits = 16
dfhistoplot, allkresid, $
             POLYCOLOR=['gray', 'white'], $
             /FILLPOLYGON, $
             binsize=2., $
             xtitle=textoidl('Best-fit K_0 - Innermost K(r) [keV cm^2]'),$
             ytitle=textoidl('Histogram Density'), $
             datacolorname='black', $
             axiscolorname='black', $
             charsize=csize
device, /close

set_plot, 'PS'
device, filename='k0kresid.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica, $
        bits = 16
plotsym, 0, 0.8, /fill
plot, k0all, allkresid, $
      psym = 8, $
      /xlog, /xsty, /ysty, $
      xrange = [0.1,1.2*max(k0all)], $
      yrange = [-100.,1.2*max(allkresid)], $
      xtitle=textoidl('Best-fit K_0 [keV cm^2]'),$
      ytitle=textoidl('Best-fit K_0 - Innermost K(r) [keV cm^2]'),$
      charsize=csize
oplot, [1d-4,1d4], [0,0], linestyle=2, psym=0
device, /close

set_plot, 'PS'
device, filename='sigresid.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica, $
        bits = 16
plotsym, 0, 0.8, /fill
plot, k0all, allsig, $
      psym = 8, $
      /ylog, /xlog, /xsty, /ysty, $
      xrange = [0.8*min(k0all),1.2*max(allsig)], $
      yrange = [0.8*min(allsig),1.2*max(allsig)], $
      xtitle=textoidl('Best-fit K_0 [keV cm^2]'),$
      ytitle=textoidl('K_0 \sigma'),$
      charsize=csize
oplot, [1d-4,1d4], [1,1], linestyle=2, psym=0
device, /close

set_plot, 'PS'
device, filename='histchisq.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica, $
        bits = 16
dfhistoplot, allchi, $
             POLYCOLOR=['gray', 'white'], $
             /FILLPOLYGON, $
             binsize=0.25, $
             xtitle=textoidl('Reduced \chi^2'),$
             ytitle=textoidl('Histogram Density'), $
             datacolorname='black', $
             axiscolorname='black', $
             charsize=csize
device, /close

END
