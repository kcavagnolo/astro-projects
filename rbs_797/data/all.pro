PRO ALL

readcol, 'all', FORMAT='(A,A,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)', comment='#', $
         name,obs,rin,rout,nh,nhlo,nhhi,tx,txlo,txhi,fe,felo,fehi,n,nlo,nhi,tx2,tx2lo,tx2hi,n2,n2lo,n2hi,z,zlo,zhi,cr,src,chisq,dof

ord = where(nhlo EQ 0.0)
nhlo[ord] = nh[ord]
nhhi[ord] = nh[ord]

ord = where(zlo EQ 0.0)
zlo[ord] = z[ord]
zhi[ord] = z[ord]

ord = where(rin EQ 0.0)
tx = tx[ord]
fe = fe[ord]
z = z[ord]
nh = nh[ord]
txlo = tx-txlo[ord]
txhi = txhi[ord]-tx
felo = fe-felo[ord]
fehi = fehi[ord]-fe
zlo = z-zlo[ord]
zhi = zhi[ord]-z
nhlo = nh-nhlo[ord]
nhhi = nhhi[ord]-nh

set_plot, 'PS'
device, filename='all.eps', $
        /encapsulated, $
        set_font='Times-Roman'
multiplot,[1,4]
plotsym, 0, 0.8, /fill
FOR i=0,n_elements(tx)-1 DO $
   IF txlo[i] GT txhi[i] THEN push, terr, txlo[i] ELSE push, terr, txhi[i]
print, 'Mean TX: ',wtd_mean(tx,(1./terr)), ' +/- ', stddev(tx)/sqrt(n_elements(tx))
plot, tx, $
      psym=8, $
      charsize=1.0
oploterror, tx, txlo, /lobar, psym=8
oploterror, tx, txhi, /hibar, psym=8
multiplot
FOR i=0,n_elements(fe)-1 DO $
   IF felo[i] GT fehi[i] THEN push, feerr, felo[i] ELSE push, feerr, fehi[i]
print, 'Mean FE: ',wtd_mean(fe,(1./feerr)), ' +/- ', stddev(fe)/sqrt(n_elements(fe))
plot, fe, $
      psym=8, $
      charsize=1.0
oploterror, fe, felo, /lobar, psym=8
oploterror, fe, fehi, /hibar, psym=8
multiplot
ord = where(z-zlo NE 0.0)
FOR i=0,n_elements(z)-1 DO $
   IF zlo[i] GT zhi[i] THEN push, zerr, zlo[i] ELSE push, zerr, zhi[i]
print, 'Mean Z: ',wtd_mean(z[ord],(1./zerr[ord])), ' +/- ', stddev(z[ord])/sqrt(n_elements(z[ord]))
plot, z, $
      psym=8, $
      charsize=1.0
oploterror, z, zlo, /lobar, psym=8
oploterror, z, zhi, /hibar, psym=8
multiplot
ord = where(nh-nhlo NE 0.0)
FOR i=0,n_elements(nh)-1 DO $
   IF nhlo[i] GT nhhi[i] THEN push, nherr, nhlo[i] ELSE push, nherr, nhhi[i]
print, 'Mean NH: ',wtd_mean(nh[ord],(1./nherr[ord])), ' +/- ', stddev(nh[ord])/sqrt(n_elements(nh[ord]))
plot, nh, $
      psym=8, $
      charsize=1.0
oploterror, nh, nhlo, /lobar, psym=8
oploterror, nh, nhhi, /hibar, psym=8

device, /close

END
