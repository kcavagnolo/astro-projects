PRO splots, dat1
;# splots, 'reference.list'

;#################################
;#################################
plotpure = 'yes'                ;# plot pure cooling?
ksmooth  = 'no'                 ;# smooth K(r)?
mini     = 'yes'                 ;# make a small plot for proposals
txnorm   = 'no'                 ;# normalize k to k/t^alpha
rvnorm   = 'no'                 ;# normalize the radius using RV?
eznorm   = 'no'                 ;# normalize K by E(z)?
zfilter  = 'no'                 ;# make a cut in redshift space
zmin     = 0.4                  ;# redshift to cut at
zmax     = 100.                  ;# redshift to cut at
txfilter = 'no'                 ;# filter by tx?
txmin    = 8.0                 ;# tx to cut at
txmax    = 100.0                  ;# tx to cut at
plpts    = 'no'                 ;# plot the lines w/ points?
plerr    = 'no'                 ;# plot error bars?
ktype    = 'flat'               ;# type of K to use, 'flat' or 'itpl'
output   = 'allsplots.eps'         ;# name of postscript
incleg   = 'no'                 ;# add legend to plot or make separate
incbar   = 'yes'                ;# include a color bar for temperatures
outputkey = 'key.ps'            ;# name of key postscript
delta    = 200.                 ;# delta for virial radius
nsm      = 2.                   ;# smoothing length
alpha    = -0.64                ;# the alpha in k/t^alpha
ezpow    = 4./3.                ;# exponent of the scale factor E(z)
eztext   = '4/3'                ;# text for the scale factor E(z)
psize    = 0.4                  ;# point size
lthick   = 1                    ;# thickness of lines
csize    = 1.0                  ;# character size
om       = 0.3                  ;# omega matter
ol       = 0.7                  ;# omega lambda
;#################################
;#################################

IF rvnorm EQ "no" THEN BEGIN
   xmin = 0.25
   xmax = 2000.
ENDIF ELSE BEGIN
   xmin = 0.0001
   xmax = 1.0
ENDELSE

IF txnorm EQ "no" THEN BEGIN
   ymin = 1.
   ymax = 3000.
ENDIF ELSE BEGIN
   ymin = 0.9
   ymax = 3000.
ENDELSE

;# restore the fit template and read some variables
restore,"~/research/pf_clusters/pf_fits/dat/purecool.sav"
pure = read_ascii("~/research/pf_clusters/pf_fits/dat/krprof_6_017.dat",template=template)
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#',$
  clusters,obsids,xs,ys,rmaxs,minctss,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robss,locs

prevname = 'jdfdjf'
zf1 = 0
zf2 = 0
tf1 = 0
tf2 = 0
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obs = strcompress(obsids[i],/remove_all)
   name = strcompress(clusters[i],/remove_all)
   IF name EQ prevname THEN GOTO,SKIP
   ord = where(clusters EQ name)
   IF n_elements(ord) NE 1 THEN BEGIN
      temp = strcompress(obsids[ord],/remove_all)
      obs  = strjoin(temp,'_',/single)
   ENDIF
   zf1++
   IF (zfilter EQ 'yes') THEN BEGIN
      IF ((zs[i] LT zmin) OR (zs[i] GE zmax)) THEN GOTO,SKIP
   ENDIF
   zf2++
   tf1++
   IF (txfilter EQ 'yes') THEN BEGIN
      IF ((txs[i] LT txmin) OR (txs[i] GE txmax)) THEN GOTO,SKIP
   ENDIF
   tf2++
   push, obsid, obs
   push, tx, txs[i]
   push, cluster, clusters[i]
   push, z, zs[i]
SKIP:
   prevname = name
ENDFOR

;# print results of filtering
print, format='(A-35,I10)','## Number clusters before z-filter:',zf1
print, format='(A-35,I10)','## Number clusters after z-filter:',zf2
print, format='(A-35,I10)','## Number clusters before Tx-filter:',tf1
print, format='(A-35,I10)','## Number clusters after Tx-filter:',tf2

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

;# set up empty plot
set_plot, 'PS'
device, filename = output, $
        /color, $
        /encapsulated, $
        set_font='Times-Roman', $
        bits=16
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
dumx = [xmin,xmax]
dumy = [ymin,ymax]
lsize = csize * 0.40
IF rvnorm EQ "no" THEN $
   xtitle = textoidl('R [kpc]') ELSE $
      xtitle = textoidl('R/R_{'+num2str(delta,3)+'}')
IF txnorm EQ "no" THEN $
   ytitle = textoidl('K [keV cm^{2}]') ELSE $
      ytitle = textoidl('Scaled Entropy (1+z)^2 K\cdotT_{X}^{'+num2str(alpha,3)+'}')
;IF eznorm EQ "no" THEN $
;   ytitle = textoidl('K [keV cm^{2}]') ELSE $
;      ytitle = textoidl('K\cdotE(z)^{'+eztext+'} [keV cm^{2}]')

IF mini EQ "yes" THEN BEGIN
    plot, dumx, dumy, /nodata, $
          xrange = [xmin,xmax], $
          yrange = [ymin,ymax], $
          linestyle = 0, $
          xtitle = xtitle, $
          ytitle = ytitle, $
          /xsty, /ysty, $
          /ylog, /xlog, $
          YTickFormat='exponent', $
          POSITION = ASPECT(1.0), $
          charsize = csize
ENDIF ELSE BEGIN
    plot, dumx, dumy, /nodata, $
          xrange = [xmin,xmax], $
          yrange = [ymin,ymax], $
          linestyle = 0, $
          xtitle = xtitle, $
          ytitle = ytitle, $
          /xsty, /ysty, $
          /ylog, /xlog, $
          charsize = csize
ENDELSE

FOR i = 0, n_elements(obs_or)-1 DO BEGIN
    obs = strcompress(obs_or[i],/remove_all)
    kt = tx_or[i]
    red = z_or[i]
    file = 'tables/'+obs+'_table.dat'
    check = findfile(file,count=count)
    IF (count NE 1) THEN BEGIN
       print, '## ERROR: No ',file,' found.'
       GOTO,ERROR
    ENDIF

    ;# read the data
    restore,"~/research/redux/scripts/s_tabletemplate.sav"
    data = read_ascii(file, template = s_tabletemplate)
    push, cname, cluster[i]
    push, ltx, kt
    rmean = ((data.rin_mpc + data.rout_mpc)/2.)*1000.
    IF rvnorm EQ 'yes' THEN $
       rv = rdelta(delta, red, kt, /silent)*1000.
    IF eznorm EQ 'yes' THEN $
       ez = om*(1+red)^3.+(1-om-ol)*(1+red)^2.+ol
    IF (ktype EQ "flat") THEN k = data.k_flat ELSE k = data.k
    kerr = data.k_err

    ;# get rid of the "NaN" entries
    kerr = kerr[where(k EQ k)]
    rmean = rmean[where(k EQ k)]
    k = k[where(k EQ k)]

    ;# get entropy profile for each cluster
    loadct, 13, /silent
    plcolor = color[i]
    push, legcolor, plcolor
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
    symbols = [symbols,symbols,symbols,symbols,symbols,symbols]
    plotsym, symbols[i], psize, /fill
    IF ksmooth EQ "yes" THEN k = smooth(k,nsm)
    IF rvnorm EQ "no" THEN r=rmean ELSE r=rmean/rv
    IF txnorm EQ "yes" THEN BEGIN
        k    = k*(kt^alpha)*(1.0+red)^2.
        kerr = kerr*(kt^alpha)*(1.0+red)^2.
    ENDIF
    IF eznorm EQ 'yes' THEN BEGIN
       k    = k*ez^(ezpow)
       kerr = kerr*ez^(ezpow)
    ENDIF
    IF plpts EQ "yes" THEN oplot, r, k, psym=8, color=plcolor
    oplot, r, k, psym=0, color=plcolor, thick=lthick
    IF plerr EQ "yes" THEN BEGIN
        oploterror, r, k, kerr, psym=8, /nohat, color=plcolor, errcolor=plcolor
    ENDIF

ERROR:
 ENDFOR
print, format='(A-35,I10)','## Number of clusters plotted:',n_elements(cname)

;# plot the pure cooling model
IF rvnorm EQ "no"  THEN r=rpure ELSE r=rpure/1800
IF txnorm EQ "yes" THEN k=(kpure*tpure)*(tpure^alpha) ELSE k=kpure*tpure
IF eznorm EQ "yes" THEN k=(kpure*tpure)*ez^(ezpow) ELSE k=kpure*tpure
IF plotpure EQ "yes" THEN $
  oplot, r, k, linestyle=0, psym=0, thick=3

;# overplot mean profiles
k = 16.1+150.*(r/100.)^(1.20)
oplot, r, k, linestyle=2, psym=0, thick=3
k = 156.+107.*(r/100.)^(1.23)
oplot, r, k, linestyle=3, psym=0, thick=3

;# plot conduction curves
;rc = maken(0.0001,10000,1000)
;fc = 0.2
;kc = 10.*fc^(-1./3.)*(rc/4.)^(2./3.)
;oplot, rc, kc, thick=5

;# draw the legend for pure cooling model
;items = ["Pure Cooling Model","Mean K0 < 50","Mean K0 > 50"]
;linearr = [0,2,3]
;psyarr = [0,0,0]
;thiarr = [3,3,3]
;legend, items, linestyle=linearr, psym=psyarr, thick=thiarr, charthick=1.0, charsize=0.8, /top, /bottom,/right
;IF incleg EQ "yes" THEN $
;  legend, cname+textoidl(' (T_{X}= ')+strcompress(sigfig(tx_or,3),/remove_all)+'keV)', $
;  psym=intarr(n_elements(cname))+8, /usersym, color=legcolor, charsize=0.8, /bottom,/right,/fill,box=0

;# add legend for filters
;# add legend
items = ''
IF zfilter EQ 'yes' THEN BEGIN
   lzmin = num2str(zmin,2)+' <'
   lzmax = '< '+num2str(zmax,2)
   IF zmin LE 0. THEN lzmin=''
   IF zmax GT 10. THEN lzmax=''
   push, items, textoidl(lzmin+' z '+lzmax)
ENDIF
IF txfilter EQ 'yes' THEN BEGIN
   ltxmin = num2str(txmin,2)+' keV <'
   ltxmax = '< '+num2str(txmax,2)+' keV'
   IF txmin LE 0. THEN ltxmin=''
   IF txmax GT 10. THEN ltxmax=''
   push, items, textoidl(ltxmin+' T_{cluster} '+ltxmax)
ENDIF
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charthick=1.0, charsize=0.8, /bottom, /right, box=0
IF incleg EQ "yes" THEN $
   legend, cname+textoidl(' (T_{X}= ')+strcompress(sigfig(tx_or,3),/remove_all)+'keV)', $
           psym=intarr(n_elements(cname))+8, /usersym, color=legcolor, charsize=0.8, /bottom,/right,/fill,box=0

;# make a colorbar
loadct, 13, /silent
IF incbar EQ "yes" THEN BEGIN
   IF mini EQ 'yes' THEN BEGIN
      COLORBAR, NCOLORS = 250, bottom = 20, $
                POSITION = [0.15, 0.76, 0.85, 0.79], $
                range = [min(ltx),max(ltx)], format = '(F5.1)', $
                /vertical, /right, charsize = 0.75*csize, $
                divisions = 4
   ENDIF ELSE BEGIN
      COLORBAR, NCOLORS = 250, bottom = 20, $
                POSITION = [0.45, 0.13, 0.94, 0.17], $
                range = [min(ltx),max(ltx)], format = '(F5.1)', $
                /top, charsize = csize, $
                divisions = 4, title = textoidl('T_X [keV]')
   ENDELSE
ENDIF
device,/close

;# cut long legend into two pieces
IF incleg EQ "yes" THEN BEGIN
   FOR i=0,n_elements(cname)/2 DO BEGIN
      push,cname1,cname[i]
      push,txor1,ltx[i]
      push,lcolor1,legcolor[i]
   ENDFOR
   FOR i=n_elements(cname)/2+1,n_elements(cname)-1  DO BEGIN
      push,cname2,cname[i]
      push,txor2,ltx[i]
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
ENDIF
set_plot,"X"
END
