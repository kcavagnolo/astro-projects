PRO simtx

!quiet=1
myhome = GETENV('HOME')
dat1 = 'fak77.dat'
dat2 = 'fak27.dat'
fidfilef = myhome+'/research/me_temp_proj/me_fits/dat/c2fits_culled_r2500-50_7-7.dat'
fidfileh = myhome+'/research/me_temp_proj/me_fits/dat/c2fits_culled_r2500-50_2-7.dat'
csize = 0.9
psize = 0.8
output = 'simtx.eps'
fideta = [0.05, 0.1, 0.15, 0.2, 0.3, 0.4]
fidtx2 = [0.5, 0.75, 1.0, 2.0, 3.0]
thbrcut = 1.1
plcolor = maken(50,250,n_elements(fidtx2))

; restore the fit template and read some variables
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
full = read_ascii(dat1, template = xspectemp_rin_normerr_src)
hard = read_ascii(dat2, template = xspectemp_rin_normerr_src)
fidfull  = read_ascii(fidfilef, template = xspectemp_rin_normerr_src)
fidhard  = read_ascii(fidfileh, template = xspectemp_rin_normerr_src)

check = full.obsid/hard.obsid
uhoh = where(check NE 1)
IF uhoh NE -1 THEN BEGIN
   print,'## ERROR: OUT OF ORDER FILE: '+dat1+' '+dat2
   exit
ENDIF

;# fiducial temps
names = fidfull.cluster
obsids = fidfull.obsid
fid77 = fidfull.tx
fid27 = fidhard.tx
fidlo77 = fid77-fidfull.tlo
fidhi77 = fidfull.thi-fid77
fidlo27 = fid27-fidhard.tlo
fidhi27 = fidhard.thi-fid27
fidtf   = fid27/fid77
fidfe   = fidfull.fe
fidtfhi = (fid27/fid77)*(sqrt((fidhi27/fid27)^2.+(fidhi77/fid77)^2.))
fidtflo = (fid27/fid77)*(sqrt((fidlo27/fid27)^2.+(fidlo77/fid77)^2.))

;# simd temps
tfnames = full.cluster
tfobsids = full.obsid
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

FOR i=0,n_elements(obsids)-1 DO BEGIN

   ;# get data for this cluster
   tobsid = obsids[i]
   tname  = names[i]
   tfidtx = fid77[i]
   tfidfe = fidfe[i]
   tfidtf = fidtf[i]
   tfidtflo = fidtflo[i]
   tfidtfhi = fidtfhi[i]

   ;# get exposure time to fix Ken's dumb mistake
   file  = '/mnt/SINISTER/'+num2str(tobsid)+'/reprocessed/'+num2str(tobsid)+'_exclude.fits'
   check = findfile(file,count=count)
   IF (count NE 1) THEN GOTO,OOPS
   head = headfits(file,ext=1)
   texp = sxpar(head,'EXPOSURE')

   ;# get values associated with this cluster
   ord   = where(tfobsids EQ tobsid)
   IF ord[0] EQ -1 THEN GOTO,OOPS
   ttx77 = tx77[ord]
   ttf   = tf[ord]
   ttfhi = tfhi[ord]
   ttflo = tflo[ord]
   tfe   = fe[ord]
   teta  = etas[ord]
   ttx2  = tx2s[ord]
   tcr   = crs[ord]
   tcts  = tcr*texp
   tsnr  = sqrt(tcts)
   tchi  = chisq[ord]

   ;# for each t2, find minimum eta which
   ;# produces a thbr > 1.1 @ 1.6 sigma
   j = 0
   FOR j=0,n_elements(fidtx2)-1 DO BEGIN
      tfid = fidtx2[j]
      tmin = 0.99*tfid
      tmax = 1.01*tfid

      ;# get all specific t2s
      ord = where(ttx2 GT tmin AND $
                  ttx2 LT tmax); AND $
;                  tchi LT 1.5)
;                  ttf-ttflo LE 1.4)
      IF ord[0] EQ -1 THEN GOTO,OOPS2
      myt2   = ttx2[ord]
      myeta  = teta[ord]
      mytf   = ttf[ord]
      mytx   = ttx77[ord]
      mytfhi = ttfhi[ord]
      mytflo = ttflo[ord]
      myfe   = tfe[ord]
      mycr   = tcr[ord]
      mycts  = tcts[ord]
      mysnr  = tsnr[ord]
      mychi  = tchi[ord]

      ;# get only those etas for which tf > 1.1
      ord = where(mytf-mytflo GE thbrcut AND mycts GT 15000)
      IF ord[0] EQ -1 THEN BEGIN
          push, final_eta, -1.0
          push, final_tx2, tfid
          push, final_tf, -1.0
          push, final_tx, -1.0
          push, final_tflo, -1.0
          push, final_tfhi, -1.0
          push, final_fidtf, tfidtf
          push, final_fidfe, tfidfe
          push, final_fe, -1.0
          push, final_fidtx, tfidtx
          push, final_name, tname
          push, final_obsid, tobsid
          push, final_color, plcolor[j]
          push, final_cr, mycr[0]
          push, final_cts, mycts[0]
          push, final_snr, mysnr[0]
          GOTO, OOPS2
      ENDIF
      myt2   = myt2[ord]
      myeta  = myeta[ord]
      mytf   = mytf[ord]
      mytx   = mytx[ord]
      mytfhi = mytfhi[ord]
      mytflo = mytflo[ord]
      myfe   = myfe[ord]
      mycr   = mycr[ord]
      mycts  = mycts[ord]
      mysnr  = mysnr[ord]
      mychi  = mychi[ord]

      ;# select the smallest of those etas
      ord = where(myeta EQ min(myeta))
      IF ord[0] EQ -1 THEN GOTO,OOPS2

      ;# store all the values associated with that eta
      push, final_eta, myeta[ord]
      push, final_tx2, myt2[ord]
      push, final_tx, mytx[ord]
      push, final_tf, mytf[ord]
      push, final_tflo, mytflo[ord]
      push, final_tfhi, mytfhi[ord]
      push, final_fidtf, tfidtf
      push, final_fidfe, tfidfe
      push, final_fidtx, tfidtx
      push, final_fe, myfe[ord]
      push, final_cr, mycr[ord]
      push, final_cts, mycts[ord]
      push, final_snr, mysnr[ord]
      push, final_name, tname
      push, final_obsid, tobsid
      push, final_color, plcolor[j]
      OOPS2:
   ENDFOR
OOPS:
ENDFOR

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
xmin = 0.8*min(final_fidtx-final_tx2)
xmax = 1.2*max(final_fidtx-final_tx2)
ymin = 0.0
ymax = 0.45
plot, $
  final_fidtx, final_eta, $
  /nodata, $
  linestyle = 0, $
  xrange = [xmin,xmax], $
  yrange = [ymin,ymax], $
  /xsty, /ysty, $
  symsize = psize, $
  charsize = csize, $
  xtitle = textoidl('T_{Cluster}-T_2 [keV]'), $
  ytitle = textoidl('\xi_{min} for T_{HBR} \geq 1.1 @ 90% confidence')

psyms = [0,3,4,5,8]
FOR j=0,n_elements(fidtx2)-1 DO BEGIN
   tfid = fidtx2[j]
   tmin = 0.99*tfid
   tmax = 1.01*tfid
   ord = where(final_tx2 GT tmin AND final_tx2 LT tmax)
   x = final_fidtx[ord]-tfid
   y = final_eta[ord]
   a = where(final_cts[ord] GE 15000)
   x = x[a]
   y = y[a]
   colors = final_color[ord]
   plotsym, psyms[j], psize, /fill
   oplot, x, y, psym=8, color=0
   plotsym, psyms[j], psize*0.7, /fill
   oplot, x, y, psym=8, color=colors[0]
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
    ord = where(final_tx2 EQ fidtx2[i] AND final_eta GT 0)
    x = fidtx2[i]/final_fidtx[ord]
    y = final_eta[ord]
;z = final_obsid[ord]
    a = where(final_cts[ord] GE 0)
    x = x[a]
    y = y[a]
print, 'T2 ',fidtx2[i],' keV'
stop
;z = z[a]
;IF fidtx2[i] EQ 3.0 THEN BEGIN
;   dum = where(y LT 0.1 AND x LT 3 AND y GT 0)
;   print, z[dum]
;ENDIF
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
;          xtitle = textoidl('T_2/T_{Cluster}'), $
;          ytitle = textoidl('\xi_{min} for T_{HBR} \geq 1.1')
    items = [textoidl('T_2 = '+num2str(fidtx2[i],3))]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=0.8*csize, $
            /top, box=0, /left_legend
ENDFOR
device, /close

set_plot,'X'

END

