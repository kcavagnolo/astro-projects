pro lxtx

; input files array
output = 'lxtx.ps'
psnum = '1'

lcut = "no"
lcuthi = 9d90
lcutlo = 0.1

tcut = "no"
tcuthi = 50
tcutlo = 4

cuttf = "no"
tfcuthi = 200.0
tfcutlo = 1.1

plot3d = "no"
push, pstitle, textoidl('R_{2500-CORE}')
push, files, 'r2500-50.fits'

FOR j = 0,n_elements(pstitle)-1 DO BEGIN
   data = mrdfits(files[j],1,/silent)
   names = data.cluster
   tf   = data.tx27/data.tx77
   tfhi = data.tx27/data.tx77*(sqrt(((data.tx27hi-data.tx27)/data.tx27)^2.+((data.tx77hi-data.tx77)/data.tx77)^2.))
   tflo = data.tx27/data.tx77*(sqrt(((data.tx27-data.tx27lo)/data.tx27)^2.+((data.tx77-data.tx77lo)/data.tx77)^2.))
   y = alog10(data.lbol)
   ylo = y-alog10(data.lbollo)
   yhi = alog10(data.lbolhi)-y
   x = alog10(data.tx77)
   xlo = x-alog10(data.tx77lo)
   xhi = alog10(data.tx77hi)-x

   FOR h=0,n_elements(y)-1 DO BEGIN
      IF ylo[h] GT yhi[h] THEN push, ayerr, ylo[h] ELSE push, ayerr, yhi[h]
      IF xlo[h] GT xhi[h] THEN push, axerr, xlo[h] ELSE push, axerr, xhi[h]
   ENDFOR

   ;# trim the data
   cuttex = ''
   IF lcut EQ "yes" THEN BEGIN
      ord = where((y LT lcuthi) AND (y GT lcutlo))
      x = x[ord]
      y = y[ord]
      xlo = xlo[ord]
      xhi = xhi[ord]
      ylo = ylo[ord]
      yhi = yhi[ord]
      tf = tf[ord]
      tflo = tflo[ord]
      tfhi = tfhi[ord]
      ayerr = ayerr[ord]
      axerr = axerr[ord]
      names = names[ord]
      cutlo = num2str(lcutlo,2)
      cuthi = num2str(lcuthi,2)
      IF lcuthi GT 1d10 THEN cuthi = '\infty'
      push, cuttex, textoidl('Lum-cut: '+cutlo+'--> '+cuthi)
   ENDIF
   IF tcut EQ "yes" THEN BEGIN
      ord = where((x LT tcuthi) AND (x GT tcutlo))
      x = x[ord]
      y = y[ord]
      xlo = xlo[ord]
      xhi = xhi[ord]
      ylo = ylo[ord]
      yhi = yhi[ord]
      tf = tf[ord]
      tflo = tflo[ord]
      tfhi = tfhi[ord]
      ayerr = ayerr[ord]
      axerr = axerr[ord]
      names = names[ord]
      cutlo = num2str(tcutlo,2)
      cuthi = num2str(tcuthi,2)
      push, cuttex, textoidl('T-cut: '+cutlo+'--> '+cuthi)
   ENDIF
   IF cuttf EQ "yes" THEN BEGIN
      ord = where((tf-tflo LT tfcuthi) AND (tf-tflo GE tfcutlo))
      x = x[ord]
      y = y[ord]
      xlo = xlo[ord]
      xhi = xhi[ord]
      ylo = ylo[ord]
      yhi = yhi[ord]
      tf = tf[ord]
      tflo = tflo[ord]
      tfhi = tfhi[ord]
      ayerr = ayerr[ord]
      axerr = axerr[ord]
      names = names[ord]
      cutlo = num2str(tfcutlo,2)
      cuthi = num2str(tfcuthi,2)
      push, cuttex, textoidl('Tf-cut: '+cutlo+'--> '+cuthi)
   ENDIF

   xtx = textoidl("T_X [keV]")
   ytx = textoidl("L_{bol} [10^{44} ergs s^{-1}]")
   
   plotsym, 0, 0.7, /fill
   set_plot, 'PS'
   device, filename = 'temp_'+strcompress(j,/remove_all)+'.ps', $
           /color, $
           /encapsulated, $
           /times, $
           bits=16
   !FANCY    = 4
   !LINETYPE = 0
   !P.FONT   = 0
   !X.THICK  = 3
   !Y.THICK  = 3
   !Z.THICK  = 3
   x = 10.^x
   y = 10.^y

   xmin = 0.8*min(x)
   xmax = 1.1*max(x)
   ymin = 0.8*min(y)
   ymax = 1.1*max(y)
   plot, x, y, $
         /xsty, /ysty, $
         /xlog, /ylog, $
         psym = 8, $
         title = pstitle[j], $
         xtitle = xtx, $
         ytitle = ytx, $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax], $
         charsize = 0.8
   oploterror, x, y, xlo, ylo, psym=8, /lobar
   oploterror, x, y, xhi, yhi, psym=8, /hibar

   ; fitting routines
   x = alog10(x)
   y = alog10(y)
   bc = bces(x, y, error=bcerr, xerror=axerr, yerror=ayerr)

   ; calculate scatter in T
   lpred = bc[1]*x+bc[0]
   lobsv = y
   tpred = (y-bc[0])/bc[1]
   tobsv = x
   lvar = (lobsv-lpred)^2.
   tvar = (tobsv-tpred)^2.
   lscatter = sqrt(total(lvar))/n_elements(y)
   tscatter = sqrt(total(tvar))/n_elements(x)

   ; output fit results
   print, '##########'
   print, 'BCES: ',bc
   print, 'BCES err: ',bcerr
   print, 'BCES scatter in L: ', lscatter
   print, 'BCES scatter in T: ', tscatter
   print, '##########'

   ; plotting in log space
;   xo = maken(-100,100,20)
;   yo = bc[1]*xo+bc[0]
;   oplot, xo, yo, linestyle=3

   ; plotting on log scale
   xo = maken(1d-4,1d3,20)
   yo = 10^bc[0]*xo^bc[1]
   oplot, xo, yo, linestyle=3

   ;# define some names and such for the legend
   eqn = textoidl('log L_{bol} = \alpha log T_X + \beta')
   power = strcompress(sigfig(bc[1],3),/remove_all)
   norm = strcompress(sigfig(bc[0],3),/remove_all)
   power = textoidl('\alpha: '+power+'\pm'+strcompress(sigfig(bcerr[1],2),/remove_all))
   norm = textoidl('\beta: '+norm+'\pm'+strcompress(sigfig(bcerr[0],2),/remove_all))
   lscatter = textoidl('L-scatter: '+strcompress(sigfig(lscatter,3),/remove_all))
   tscatter = textoidl('T-scatter: '+strcompress(sigfig(tscatter,3),/remove_all))
   items = [eqn, power, norm, lscatter, tscatter, cuttex]
   linearr = replicate(-99,n_elements(items))
   psyarr = replicate(-99,n_elements(items))
   legend, items, linestyle=linearr, psym=psyarr, charsize=0.8, spacing=space, /bottom, box=0, /right_legend
   device, /close

   IF plot3d EQ "yes" THEN BEGIN
      tfo = replicate(1.0,n_elements(xo))
      push, x, xo
      push, y, yo
      push, tf, tfo
      scatter_surface, x, y, tf, xtitle='Log T', ytitle='Log Lbol', ztitle='T_HFR' , /showLines
   ENDIF
ENDFOR

; make all these ps files into one ps file
;SPAWN, 'ls temp_*.ps > list'
;SPAWN, 'cat list | perl pscat.pl '+psnum+' '+output
;SPAWN, 'rm -f temp*.ps'
;SPAWN, 'rm -f list'

END
