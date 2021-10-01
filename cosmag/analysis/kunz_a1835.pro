FUNCTION exponent, axis, index, number

  if number eq 0 then return, '0'  
  ex = String(number, format='(e8.0)')
  pt = StrPos(ex,'.')
  first = StrMid(ex,0,pt)
  sign = StrMid(ex,pt+2,1)
  thisExponent = StrMid(ex,pt+3)
  while StrMid(thisExponent,0,1) eq '0' do thisExponent = StrMid(thisExponent,1)
  if (Long(thisExponent) eq 0) then begin
     sign = ''
     thisExponent = '0'
  endif
  if (Long(first) eq 1) then begin
     if sign eq '-' then return, '10!U' + sign + thisExponent $
     else                return, '10!U' + thisExponent
  endif else begin
     if sign eq '-' then return, first + 'x10!U' + sign + thisExponent $
     else                return, first + 'x10!U' + thisExponent
  endelse
END


PRO kunz_a1835

; electron density
nem=[1.013051e-01,8.459975e-02,7.253090e-02,5.611583e-02,4.173251e-02, $
     3.347415e-02,2.727904e-02,2.577121e-02,2.115231e-02,1.808490e-02, $
     1.425932e-02,1.145005e-02,8.995093e-03,7.476746e-03,6.574554e-03, $
     5.492044e-03,4.334784e-03,3.633731e-03,2.939645e-03,2.261917e-03, $
     1.721952e-03,1.316291e-03,3.837178e-04]
; positive error bars
np =[3.885018e-03,2.732787e-03,1.800797e-03,1.295804e-03,1.210027e-03, $
     1.139675e-03,1.157072e-03,8.503261e-04,8.235903e-04,3.904686e-04, $
     3.416427e-04,2.591201e-04,3.524567e-04,1.812584e-04,1.239572e-04, $
     1.489112e-04,9.548168e-05,7.439211e-05,7.883378e-05,4.206105e-05, $
     2.050454e-05,1.856170e-05,5.283740e-06]
; negative error bars
nm =[3.855963e-03,2.742314e-03,1.736446e-03,1.302452e-03,1.232160e-03, $
     1.112957e-03,1.168092e-03,8.617833e-04,8.098673e-04,3.737143e-04, $
     3.560778e-04,2.492951e-04,2.439155e-04,1.850032e-04,1.802632e-04, $
     1.482904e-04,9.709612e-05,8.608619e-05,7.137054e-05,4.172175e-05, $
     3.562590e-05,2.053237e-05,5.464100e-06]

; TOTAL NUMBER DENSITY
n = nem/0.519

; ION NUMBER DENSITY
ni = 0.481*n

; MASSES
mp = 1.67262d-24
me = 9.1094d-28
mi = 1.24116424d0*mp

; temperature (keV)
t = [2.84830e+00,3.48600e+00,4.13090e+00,5.27510e+00,5.18100e+00, $
     5.94560e+00,5.17290e+00,8.38190e+00,9.15120e+00,8.09590e+00, $
     6.76090e+00,7.51270e+00,8.29030e+00,8.54550e+00,1.00260e+01, $
     8.04080e+00,8.92410e+00,8.50030e+00,1.19340e+01,7.96620e+00, $
     1.11550e+01,8.36600e+00,8.52710e+00]
; positive error bars
tp= [2.03840e-01,2.45410e-01,2.84280e-01,5.14560e-01,5.60550e-01, $
     1.17290e+00,7.71520e-01,2.11760e+00,2.93350e+00,1.14440e+00, $
     7.82560e-01,1.07890e+00,1.50870e+00,1.50980e+00,2.34780e+00, $
     1.51880e+00,1.40250e+00,1.74780e+00,2.56380e+00,1.01530e+00, $
     1.85190e+00,9.41650e-01,7.75140e-01]
; negative error bars
tm= [1.92460e-01,2.20360e-01,2.52120e-01,4.34520e-01,4.66790e-01, $
     8.52080e-01,6.45460e-01,1.48400e+00,1.82160e+00,9.11080e-01, $
     6.63070e-01,8.88690e-01,1.17570e+00,1.08300e+00,1.58620e+00, $
     1.07500e+00,1.07000e+00,1.24310e+00,1.91410e+00,8.14350e-01, $
     1.73210e+00,7.71190e-01,6.87430e-01]

; radius (kpc)
kpc = 3.0857e21
r = [1.16688e+01,2.14001e+01,2.91851e+01,3.69701e+01,4.47552e+01, $
     5.25402e+01,6.03252e+01,6.81102e+01,7.58953e+01,8.75728e+01, $
     1.03143e+02,1.20659e+02,1.40122e+02,1.61531e+02,1.84886e+02, $
     2.08241e+02,2.37435e+02,2.72467e+02,3.11392e+02,3.63941e+02, $
     4.32060e+02,5.27427e+02,9.43926e+02]
; positive error bars
rp= [5.83877e+00,3.89251e+00,3.89251e+00,3.89251e+00,3.89251e+00, $
     3.89251e+00,3.89251e+00,3.89251e+00,3.89251e+00,7.78503e+00, $
     7.78503e+00,9.73129e+00,9.73129e+00,1.16775e+01,1.16775e+01, $
     1.16775e+01,1.75163e+01,1.75163e+01,2.14088e+01,3.11401e+01, $
     3.69789e+01,5.83877e+01,3.58111e+02]
; negative error bars
rm= rp

!P.multi=0

; COMPUTE COOLING
xi = 0.75
lambda = 23.0 - alog(0.00966*ni^0.5/t^1.5)          ; Coulomb log for ions
dlnlp = (1.5/lambda)*(tp/t) - (0.5/lambda)*(np/nem) ; error bars on lambda
dlnlm = (1.5/lambda)*(tm/t) - (0.5/lambda)*(nm/nem)
cool = ni*nem*1d-22*(0.0086/t^1.7+0.058*t^0.5+0.063)         ; cooling function
dcooldt = ni*nem*1d-22*(0.0086*(-1.7)/t^2.7+0.058*0.5/t^0.5) ; errors on cool
dcooldn = 1.85356*nem*1d-22*(0.0086/t^1.7+0.058*t^0.5+0.063)
dlncoolp = (dcooldt/cool)*tp + (dcooldn/cool)*np
dlncoolm = (dcooldt/cool)*tm + (dcooldn/cool)*nm

; HEATING = COOLING DETERMINES B
povernu = 1176.6034*(t^2.5/lambda) ; ion pressure / ion-ion collision freq
bragfctr = 3075.0/1068.0 
b2 = (4.0*!pi/xi)*sqrt(povernu*cool*bragfctr)  ; magnetic field squared
b = sqrt(b2)                                   ; magnetic field
bp = b*(0.625*(tp/t)-0.25*dlnlp+0.25*dlncoolp) ; errors on b
bm = b*(0.625*(tm/t)-0.25*dlnlm+0.25*dlncoolm)

; EQUIPARTITION BETWEEN KE AND ME DETERMINES U
urms = b/sqrt(4.0*!pi*mi*ni)                   ; rms turbulent velocity
up = urms*((bp/b)-0.5*(np/nem))                ;errors on urms
um = urms*((bm/b)-0.5*(nm/nem))

; TURBULENT POWER = COOLING POWER DETERMINES L
L = b2*urms/(4.0*!pi*cool)                     ; effective turbulent outer scale
Lp = L*(3.0*(bp/b)-dlncoolp-0.5*(np/nem))      ;errors on L
Lm = L*(3.0*(bm/b)-dlncoolm-0.5*(nm/nem))

; DIFFUSION COEFFICIENT
dfc = urms*L                ; diffusion coefficient
dfcp = up*L+urms*Lp         ;errors on dfc
dfcm = um*L+urms*Lm

; ANALYTIC PROFILE

rfit = dindgen(301)
n0 = 0.115
r0 = 32.48
beta = 0.89
fctr = 1.0+(rfit/r0)^2
nfit = n0/fctr^beta
dlnndr = (-2.0/r0)*(rfit/r0)*beta/fctr
t0 = 9.55200
t1 = 7.38608
rct = 33.3464
delta = 0.618767
fctr = 1.0 + (rfit/rct)^2
tfit = t0 - t1/fctr^delta
dtdr = 2.0*delta*(t1/rct)*(rfit/rct)/fctr^(delta+1.0)
d2tdr2 = 2.0*delta*(t1/rct/rct)/fctr^(delta+1.0)*(1.0-2.0*(delta+1.0)*(rfit/rct)^2/fctr)
nifit = (0.481/0.519)*nfit
nnfit = nifit + nfit

; cooling for the analytic profile
lambda = 23.0 - alog(0.00966*nifit^0.5/tfit^1.5)
coolfit = nifit*nfit*1d-22*(0.0086/tfit^1.7+0.058*tfit^0.5+0.063)
povernu = 1176.6034*(tfit^2.5/lambda)
b2fit = (4.0*!pi/xi)*sqrt(povernu*coolfit*bragfctr)
bfit = sqrt(b2fit)
ufit = bfit/sqrt(4.0*!pi*mi*nifit)
Lfit = b2fit*ufit/(4.0*!pi*coolfit)
dfcfit = Lfit*ufit


; PLOT

set_plot,'ps'
!p.position=[0.2,0.2,0.95,0.95]
xmax = 250

device,filename='a1835_density.ps',xsize=7,ysize=5,/inches
plot,r,nem*1e2,xtit='!8r !6(kpc)',ytit='!8n!6!De!N (10!U-2!N cm!U-3!N)', $
  charsize=2,ticklen=0.03,xrang=[0,xmax],psym=3,yrang=[0,11],ystyle=1,xstyle=1
oploterror,r,nem*1e2,rp,np*1e2,psym=3,/hibar
oploterror,r,nem*1e2,rm,nm*1e2,psym=3,/lobar
xyouts,187,9.4,'!6A1835',charsize=2.25
xyouts,199,8.55,'!8obs.!6',charsize=2
device,/close

device,filename='a1835_temperature.ps',xsize=7,ysize=5,/inches
plot,r,t,xtit='!8r !6(kpc)',ytit='!8T !6(keV)',charsize=2,yrang=[2,12.5], $
  ticklen=0.03,psym=3,xrang=[0,xmax],ystyl=1,xstyl=1
oploterror,r,t,rp,tp,psym=3,/hibar
oploterror,r,t,rm,tm,psym=3,/lobar
xyouts,187,3.7,'!6A1835',charsize=2.25
xyouts,199,2.9,'!8obs.!6',charsize=2
device,/close

device,filename='a1835_lengthscale.ps',xsize=7,ysize=5,/inches
plot,r,L/kpc,/yl,xstyl=1,ystyl=1,xrang=[0,xmax],yrang=[1e-1,300], $
     xtit='!8r !6(kpc)',ytit='!8L !6(kpc)',charsize=2,ticklen=0.03,psym=3, $
     thick=2,ytickformat='exponent'
oplot,rfit,Lfit/kpc*(0.5/0.75)^(-1.5),linestyle=2
oplot,rfit,Lfit/kpc*(1.0/0.75)^(-1.5),linestyle=1
oploterror,r,L/kpc,rp,Lp/kpc,psym=3,/hibar
oploterror,r,L/kpc,rm,Lm/kpc,psym=3,/lobar
xyouts,187,0.365,'!6A1835',charsize=2.25
xyouts,187,0.2,'!8theory!6',charsize=2
device,/close

device,filename='a1835_bfield.ps',xsize=7,ysize=5,/inches
plot,r,b/1e-6,xtit='!8r !6(kpc)',ytit='!8B!6 (!7l!6G)',xstyl=1, $
  charsize=2,yrang=[5,22],ystyle=1,ticklen=0.03,psym=3,xrang=[0,xmax]
oplot,rfit,bfit/1e-6*(0.5/0.75)^(-0.5),linestyle=2
oplot,rfit,bfit/1e-6*(1.0/0.75)^(-0.5),linestyle=1
oploterror,r,b/1e-6,rp,bp/1e-6,psym=3,/hibar
oploterror,r,b/1e-6,rm,bm/1e-6,psym=3,/lobar
xyouts,187,19.52,'!6A1835',charsize=2.25
xyouts,187,18.2,'!8theory!6',charsize=2
device,/close

device,filename='a1835_diffusion.ps',xsize=7,ysize=5,/inches
plot,r,dfc,xtit='!8r !6(kpc)',ytit='!7j!D!6turb!N !6(cm!U2!N s!U-1!N)', $
  /yl,xstyl=1,ystyl=1,xrang=[0,xmax],yrang=[3e27,2e31],charsize=2, $
  ticklen=0.03,psym=3
oplot,rfit,dfcfit*(0.5/0.75)^(-2.0),linestyle=2
oplot,rfit,dfcfit*(1.0/0.75)^(-2.0),linestyle=1
oploterror,r,dfc,rp,dfcp,psym=3,/hibar
oploterror,r,dfc,rm,dfcm,psym=3,/lobar
xyouts,187,1.2e28,'!6A1835',charsize=2.25
xyouts,187,6.3e27,'!8theory!6',charsize=2
device,/close

device,filename='a1835_velocity.ps',xsize=7,ysize=5,/inches
plot,r,urms/1e5,xtit='!8r !6(kpc)',ytit='!8U!6!Drms!N (km s!U-1!N)',xstyl=1, $
  charsize=2,yrang=[70,300],ystyle=1,ticklen=0.03,psym=3,xrang=[0,xmax]
oplot,rfit,ufit/1e5*(0.5/0.75)^(-0.5),linestyle=2
oplot,rfit,ufit/1e5*(1.0/0.75)^(-0.5),linestyle=1
oploterror,r,urms/1e5,rp,up/1e5,psym=3,/hibar
oploterror,r,urms/1e5,rm,um/1e5,psym=3,/lobar
xyouts,187,108,'!6A1835',charsize=2.25
xyouts,187,90,'!8theory!6',charsize=2
device,/close

return
end



