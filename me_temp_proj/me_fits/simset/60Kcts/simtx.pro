PRO simtx

!quiet=1
myhome = GETENV('HOME')
dat1 = 'fakset_nhfro_fefree_7-7.dat'
dat2 = 'fakset_nhfro_fefree_2-7.dat'
csize = 0.9
psize = 0.8
output = 'simtx.eps'
thbrcut = 1.1
fidtx2 = [0.5,0.75,1.0,$
          1.25,1.50,1.75,$
          2.0,2.25,2.50,2.75,3.0]
plcolor = maken(50,250,n_elements(fidtx2))

; restore the fit template and read some variables
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
full = read_ascii(dat1, template = xspectemp_rin_normerr_src)
hard = read_ascii(dat2, template = xspectemp_rin_normerr_src)

;# simd temps
tx77 = full.tx
lo77 = full.tx - full.tlo
hi77 = full.thi - full.tx
tx27 = hard.tx
lo27 = hard.tx - hard.tlo
hi27 = hard.thi - hard.tx
fe   = full.fe
etas = full.rout
tx2s = full.rin
crs  = full.cr
chisq= full.chisq
tf   = tx27/tx77
tfhi = (tx27/tx77)*(sqrt((hi27/tx27)^2.+(hi77/tx77)^2.))
tflo = (tx27/tx77)*(sqrt((lo27/tx27)^2.+(lo77/tx77)^2.))

;stop
;ord = where(tx2s EQ 3.0)
;plot,etas[ord],tf[ord], psym=2, xrange=[0,0.45], /xsty, yrange=[0.5,2.0], /ysty & oploterror,etas[ord],tf[ord],tfhi[ord],/hibar, psym=2 & oploterror,etas[ord],tf[ord],tflo[ord],/lobar, psym=2
;x=maken(-1,100,10) & y=replicate(1.0,n_elements(x))
;oplot,x,y

j = 0
FOR j=0,n_elements(fidtx2)-1 DO BEGIN
   tfid = fidtx2[j]
   ord = where(tx2s EQ tfid)
   IF ord[0] EQ -1 THEN GOTO,OOPS2
   myt2   = tx2s[ord]
   myeta  = etas[ord]
   mytf   = tf[ord]
   mytfhi = tfhi[ord]
   mytflo = tflo[ord]
   mychi  = chisq[ord]
   ord = where(mytf-mytflo GE thbrcut)
   IF ord[0] EQ -1 THEN GOTO, OOPS2
   myt2   = myt2[ord]
   myeta  = myeta[ord]
   mytf   = mytf[ord]
   mytfhi = mytfhi[ord]
   mytflo = mytflo[ord]
   mychi  = mychi[ord]
   ord = where(myeta EQ min(myeta))
   IF ord[0] EQ -1 THEN GOTO,OOPS2
   push, final_eta, myeta[ord]
   push, final_tx2, myt2[ord]
   push, final_tf, mytf[ord]
   push, final_tflo, mytflo[ord]
   push, final_tfhi, mytfhi[ord]
   push, final_color, plcolor[j]
OOPS2:
ENDFOR

final_fidtx = 5.0*(final_eta/final_eta)
loadct, 39
set_plot,'PS'
device, $
  filename = output, $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
xmin = 0.8*min(final_tx2)
xmax = 1.2*max(final_tx2)
ymin = 0.0
ymax = 0.45
plot, $
   final_tx2, final_eta, $
   /nodata, $
   linestyle = 0, $
   xrange = [xmin,xmax], $
   yrange = [ymin,ymax], $
   /xsty, /ysty, $
   symsize = psize, $
   charsize = csize, $
   xtitle = textoidl('T_{Cluster}-T_2 [keV]'), $
   ytitle = textoidl('\xi_{min} for T_{HBR} \geq 1.1 @ 90% confidence')
psyms = [0,3,4,5,8,$
         0,3,4,5,8,$
         0,3,4,5,8,$
         0,3,4,5,8]
FOR j=0,n_elements(fidtx2)-1 DO BEGIN
   tfid = fidtx2[j]
   ord = where(final_tx2 EQ tfid)
   IF ord[0] EQ -1 THEN GOTO,POP
   x = final_tx2[ord]
   y = final_eta[ord]
   colors = final_color[ord]
   plotsym, psyms[j], psize, /fill
   oplot, x, y, psym=8, color=0
   plotsym, psyms[j], psize*0.7, /fill
   oplot, x, y, psym=8, color=colors[0]
POP:
ENDFOR

;# draw a legend
items = [textoidl('Blue Circle: T_2 = 0.5 keV'), $
         textoidl('Cyan Star: T_2 = 0.75 keV'), $
         textoidl('Green Up Tri: T_2 = 1.0 keV'), $
         textoidl('Orange Down Tri: T_2 = 2.0 keV'), $
         textoidl('Red Square: T_2 = 3.0 keV')]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.8*csize, $
        /top, box=0, /right_legend
device, /close

; set-up device
set_plot,'PS'
device, filename = 'junk.ps'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
plotsym, 0, 0.5*psize, /fill
multiplot,[2,3]
FOR i=0,n_elements(fidtx2)-1 DO BEGIN
    ord = where(final_tx2 EQ fidtx2[i])
    IF ord[0] EQ -1 THEN GOTO,POP2
    x = fidtx2[i]/final_fidtx[ord]
    y = final_eta[ord]
    xmin = 0.8*min(x)
    xmax = 1.2*max(x)
    ymin = 0.00
    ymax = 0.45
    IF i NE 0 THEN multiplot
    plot, x, y, $
          psym = 8, $
          xrange=[0.,1.0], $
          yrange=[ymin,ymax], $
          /ysty, /xsty, $
          charsize = csize
    items = [textoidl('T_2 = '+num2str(fidtx2[i],3))]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=0.8*csize, $
            /top, box=0, /left_legend
POP2:
ENDFOR
device, /close

set_plot,'X'

END

