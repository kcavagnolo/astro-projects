PRO A

restore,'/home/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav'
t1 = read_ascii('junk',template=xspectemp_rin_normerr_src)
t2 = read_ascii('junk2',template=xspectemp_rin_normerr_src)

r = (t1.rin+t1.rout)/2.
r = r*60.
fe = t1.fe
felo = fe-t1.felo
fehi = t1.fehi-fe

set_plot, 'PS'
device, filename='fe.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
plotsym, 0, 1.0

plot, r, fe, $
      psym=8, $
      xrange=[1,300], $
      yrange=[0,0.8], $
      /xsty, /ysty, /xlog, $
      title='Zwicky 3146', $
      xtitle=textoidl('R_{mid} [arcsec]'), $
      ytitle = textoidl('Abundance [Z/Z'+sunsymbol()+']'), $
      position = ASPECT(0.3), $
      charsize=1.0

oploterror, r, fe, felo, /lobar, psym=8
oploterror, r, fe, fehi, /hibar, psym=8

r = (t2.rin+t2.rout)/2.
r = r*60.
fe = t2.fe
felo = fe-t2.felo
fehi = t2.fehi-fe
plotsym, 3, 0.8, /fill
oplot, r, fe, psym=8
oploterror, r, fe, felo, /lobar, psym=8
oploterror, r, fe, fehi, /hibar, psym=8
device, /close

END
