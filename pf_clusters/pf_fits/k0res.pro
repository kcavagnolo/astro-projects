PRO k0res, dat1

csize = 0.8
psize = 0.8
ktype = 'flat'
model = 'nonzero'
redir = 'reprocessed'
bin = '10pix'

loadct, 13
set_plot, 'PS'
device, $
  filename = 'k0res.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
   set_font='Times-Roman', $
  bits = 16
multiplot,[3,1]
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

restore,'~/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore,"~/research/redux/scripts/s_resultstemplate.sav"
restore,"~/research/redux/scripts/s_tabletemplate.sav"
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster,obsids,xs,ys,rmaxs,mincts,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robs,datadir
txfile  = read_ascii('dat/pf_temp_profs.dat',template = xspectemp_rin_normerr_src)
entfile = read_ascii('s_results/all_results.log', template = s_resultstemplate)

IF (ktype EQ "flat") THEN BEGIN
    IF (model EQ "nonzero") THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ "itpl") THEN BEGIN
    IF (model EQ "nonzero") THEN ind = 0 ELSE ind = 1
ENDIF

;# start the loop
FOR i = 0,n_elements(obsids)-1 DO BEGIN
    multi = 'no'
    obs  = strcompress(obsids[i],/remove_all)
    name = strcompress(cluster[i],/remove_all)
    ord  = where(cluster EQ name)
    IF n_elements(ord) NE 1 THEN BEGIN
        temp = obsids[ord]
        FOR j=1,n_elements(temp)-1 DO BEGIN
            obs = obs+'_'+strcompress(temp[j],/remove_all)
        ENDFOR
        multi = 'yes'
    ENDIF
    IF multi EQ 'no' THEN $
       sbr = datadir[i]+'/'+obs+'/'+redir+'/'+obs+'_sbprof_'+bin+'.fits' $
    ELSE $
       sbr = datadir[i]+'/merged/'+name+'/'+obs+'_sbprof_'+bin+'.fits'
;    check = findfile(sbr,count=count)
;    IF (count NE 1) THEN GOTO,ERROR
print,sbr

    ;# check for and open file
    file  = 'tables/'+obs+'_table.dat'
    check = findfile(file,count=count)
    IF (count NE 1) THEN GOTO,ERROR

    ;# get observed k values
    kr    = read_ascii(file, template = s_tabletemplate)
    krin   = kr.rin_mpc
    krout  = kr.rout_mpc
    krerr = reverse(kr.k_err)
    kr    = reverse(kr.k)
    num   = n_elements(kr)-1
    push, obsk, kr[num]
    push, obskerr, krerr[num]
    push, dkr, abs(kr[num-1]-kr[num])

    ;# get spec fit vales
    z    = txfile.z[where(txfile.cluster EQ name)]
    rin  = txfile.rin[where(txfile.cluster EQ name)]
    rout = txfile.rout[where(txfile.cluster EQ name)]
    z    = z[0]
    push, red, z
    push, outtx, txs[i]
    cosmology, z, result, /silent
    da   = result[4]
    push, zall, z
    push, rawdr, rout[0]-rin[0]
    push, dr, ((rout[0]*60.)-(rin[0]*60.))*da

    ;# sbr data
    fits = mrdfits(sbr,1,/silent)
    rmid = fits.rmid*0.492*da
    push, rsbr, rmid[1]-rmid[0]

    ;# get fitted k values
    k0    = entfile.k0[where(entfile.cluster EQ name)]
    k0err = entfile.k0err[where(entfile.cluster EQ name)]
    rmin  = entfile.rmin[where(entfile.cluster EQ name)]
    rmax  = entfile.rmax[where(entfile.cluster EQ name)]
    chi   = entfile.chisq[where(entfile.cluster EQ name)]
    chi1  = chi[2]
    chi2  = chi[3]
    rmin  = rmin[0]
    rmax  = rmax[0]
    k0 = k0[ind]
    k0err = k0err[ind]
    ord = where((krin GE rmin) * (krout LE rmax))
    IF ((k0-k0err LE 0.) OR (chi2 LE chi1)) THEN BEGIN
       k0 = k0+2.*k0err
       k0err = 1d-6
       push, uk, k0
       push, ub, n_elements(ord)
    ENDIF
    push, fitk, k0
    push, fitkerr, k0err
    push, allbins, n_elements(ord)
    push, allnames, name
ERROR:
ENDFOR

;# define the color array
color = maken(20,250,n_elements(outtx))
ord   = sort(outtx)
temp  = outtx[ord]
FOR j=0,n_elements(outtx)-1 DO BEGIN
    a = where(temp EQ outtx[j])
    a = floor(a[0])
    push, colors, color[a]
ENDFOR

y      = [ptr_new(fitk),ptr_new(fitk),ptr_new(fitk)]
yerr   = [ptr_new(fitkerr),ptr_new(fitkerr),ptr_new(fitkerr)]
x      = [ptr_new(red),ptr_new(dr),ptr_new(rsbr)]
dumerr = replicate(0,n_elements(fitk))
xerr   = [ptr_new(dumerr),ptr_new(dumerr),ptr_new(dumerr)]
xtex   = [textoidl('Redshift'), $
          textoidl('\DeltaR of Central T_X Bin [kpc]'), $
          textoidl('\DeltaR of Central SB Bin [kpc]')]
ytex   = [textoidl('K_{0} [keV cm^{2}]'),'','']
FOR j= 0,n_elements(y)-1 DO BEGIN
    py    = *y[j]
    pyerr = *yerr[j]
    px    = *x[j]
    pxerr = *xerr[j]
    ymin = 0.9*min(py)
    ymax = 1.1*max(py)
    xmin = 0.9*min(px)
    xmax = 1.1*max(px)
    IF j NE 0 THEN multiplot
    plotsym, 0, 0.8, /fill
    plot, px, py, $
          linestyle = 0, $
          xrange = [xmin,xmax], $
          yrange = [ymin,ymax],$
          xtitle = xtex[j], $
          ytitle = ytex[j], $
          /xsty, /ysty, $
          /xlog, /ylog, $
          charsize = csize, $
          psym = 8, symsize=0.01
    oploterror, px, py, pxerr, pyerr, psym=8, /lobar, symsize=psize
    oploterror, px, py, pxerr, pyerr, psym=8, /hibar, symsize=psize
    plots, px, py, psym=8, color=colors, symsize=0.8*psize
ENDFOR
device,/close
multiplot,/reset

set_plot, 'PS'
device, $
   filename = 'nbins_k0.eps', $
   /encapsulated, $
   /portrait, $
   set_font='Times-Roman'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
y = allbins
x = fitk
xerr = fitkerr
yerr = 0.*xerr
xmin = 0.70*min(x)
xmax = 1.25*max(x)
ymin = 0.50*min(y)
ymax = 1.10*max(y)
xtex = textoidl('K_0 [keV cm^2]')
ytex = textoidl('Number of bins fit in K(r)')
plotsym, 0, 0.5, /fill
plot, x, y, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      psym = 8, $
      position=ASPECT(1.0), $
      charsize = 1.0
oploterror, x, y, xerr, yerr, psym=8, /nohat
plotsym, 0, 0.5, /fill
oplot, uk, ub, psym=8
;plotsym, 8, 0.7, /fill
;oplot, uk, ub, psym=8
;plotsym, 6, 2.0, /fill
;oplot, uk, ub, psym=8
device,/close
set_plot, "X"

END
