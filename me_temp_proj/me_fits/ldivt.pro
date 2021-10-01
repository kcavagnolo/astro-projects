pro ldivt

; input files array
output = 'ldivt.ps'
pl = 3.
psnum = '1'

push, pstitle, textoidl('R_{2500}')
push, pstitle, textoidl('R_{2500-CORE}')
push, pstitle, textoidl('R_{5000}')
push, pstitle, textoidl('R_{5000-CORE}')

push, files, 'r2500.fits'
push, files, 'r2500-50.fits'
push, files, 'r5000.fits'
push, files, 'r5000-50.fits'

FOR j = 0,n_elements(pstitle)-1 DO BEGIN

   ;# get data
   data = mrdfits(files[j],1,/silent)
   x = data.z
   l = data.lbol
   llo = l-data.lbollo
   lhi = data.lbolhi-l
   t = data.tx77
   tlo = t-data.tx77lo
   thi = data.tx77hi-t
   y = l/t^pl
   ylo = y*(sqrt((pl*tlo/t)^2.+(llo/l)^2.))
   yhi = y*(sqrt((pl*thi/t)^2.+(lhi/l)^2.))
   xtx = textoidl("redshift")
   ppl = strcompress(sigfig(pl,2),/remove_all)
   ytx = textoidl("L_{[bol]}/T^{"+ppl+"}_{[0.7-7.0]} [10^{45} ergs sec^{-1} keV^{-"+ppl+"}]")

   ;# alt plot
   x = t
   y = l
   xtx = textoidl('T_{cluster} [keV]')
   ytx = textoidl("L_{bol} [10^{45} ergs s^{-1}]")
  
   ;# plot commands
   xmin = 0.8*min(x)
   xmax = 1.2*max(x)
   ymin = 0.8*min(y)
   ymax = 1.2*max(y)
   plotsym, 0, 0.5, /fill
   set_plot, 'PS'
;   device, filename = 'temp_'+strcompress(j,/remove_all)+'.ps', $
   device, filename = 'LT_'+files[j]+'.eps', $
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
   plot, x, y, $
         /xsty, /ysty, $
         /xlog, /ylog, $
         psym = 8, $
         title = pstitle[j], $
         xtitle = xtx, $
         ytitle = ytx, $
;         xrange = [xmin,xmax], $
;         yrange = [ymin,ymax], $
         xrange = [1,20], $
         yrange = [0.1,150], $
         charsize = 0.8
   oploterror, x, y, ylo, psym=8, /lobar
   oploterror, x, y, yhi, psym=8, /hibar
   device, /close
ENDFOR

; make all these ps files into one ps file
;SPAWN, 'ls temp_*.ps > list'
;SPAWN, 'cat list | perl pscat.pl '+psnum+' '+output
;SPAWN, 'rm -f temp*.ps'
;SPAWN, 'rm -f list'
;set_plot,'X'

END
