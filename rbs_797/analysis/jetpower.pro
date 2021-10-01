PRO jetpower

; Plots behavior of jet power versus spin
; Includes both the "disk model" and the Blandford-Znajek.
; See Nemmen et al. 2006, MNRAS, submitted, "Models for jet power in
; elliptical galaxies: evidence for rapidly spinning black holes"
;
; Notation: 
;  - variables named like M (uppercase) which conflict with preexisting 
;    variables like m are represented as M_ .
;  - the dimensionless spin (a/M) is given by "a" (not "j", as in the paper)

; Fixed model parameters
;pcav = 6.05d45
pcav = 3.34d45
;m=5d9
;m=3.75d9
;mdot=0.008
;mdot=0.135
m=1.5d9
mdot=0.19
alpha=0.25

; Definition of constants
f_adaf=1
f=1.
c=2.99792458d10               ; speed of light
G_=.6673d-7                   ; gravitational constant
Msun=.199d34                  ; solar mass

; General definitions
M_=m*Msun
RS=2*G_*M_/c^2                  ; Schwarzschild radius

; Hawley et al. (1995), determination of beta from alpha.
; See Narayan et al. (1998) (Sgr A* paper).
; Assumes P_mag = B^2/8*Pi
c_mri=0.55 
betaAdaf=1.-alpha/c_mri

; calculation of gamma (adiabatic index) from Esin (1997)
gamma=(8.-3.*betaAdaf)/(6.-3.*betaAdaf)

; The quantities below come from Narayan & Yi (1995)
eps_=1./f_adaf*(5./3.-gamma)/(gamma-1.)   ; epsilon'
print, eps_
gg=sqrt(1.+18.*alpha^2/(5.+2.*eps_)^2)-1. ; g(alpha,epsilon')
c1=(5.+2.*eps_)/(3.*alpha^2)*gg
c3=2.*(5.+2.*eps_)/(9.*alpha^2)*gg
c2=sqrt(eps_*c3)

points=1000                                     ; will create an array with "points" elements
a_i=-1.                                         ; initial spin
a_f=1.                                          ; final spin
spin=findgen(points)/(points-1)*(a_f-a_i) + a_i ; black hole spin

; Fills array data
Pjet_bz=dindgen(points)
Pjet=dindgen(points)
Pjet_ges=dindgen(points)
  
for j=0,points-1 do begin       ; j -> spin
  a=spin[j]
  
  ; radius of the event horizon
  RH=G_/c^2*M_*(1+sqrt(1-a^2))
  
  ; ISCO (or marginally stable orbit)
  A1=1+(1-a^2)^(1./3.)*((1+a)^(1./3.)+(1-a)^(1./3.))
  A2=sqrt(3*a^2+A1^2)
  Risco=G_/c^2*M_*(3+A2-sqrt((3-A1)*(3+A1+2*A2)))
  R0_=1.0*Risco ; characteristic radius
  r0=R0_/RS
  
  ; angular velocity of metric
  omega=2*a*(G_/c^2*M_)^2/( (a*G_/c^2*M_)^2*(R0_+RS)+R0_^3  )*c
  
  ; Scale height (NY95)
  H=sqrt(2.5*c3)*R0_
  
  ; angular velocity of the disk relative to the metric
  Omega_disk=7.19d4*c2*1./m*r0^(-3./2.)
  
  ; magnetic field-enhancing shear in the Kerr metric
  g=1+omega/Omega_disk  
  
  ; azimuthal magnetic field (NY95)
  ; Radial magnetic field threading the hole. Assumed to be
  ; the same as the one threading the ISCO, including the
  ; field-enhancing shear due to the Kerr metric.
  B=6.55d8/sqrt(alpha)*sqrt(1-betaAdaf)/sqrt(c1)*c3^(1./4.)/sqrt(m)*sqrt(mdot)*r0^(-5./4.)
  B=g*B                         ; field-enhancing
  
  ; jet power from the BZ effect
  Pjet_bz[j]=1./32.*1./4.*B^2*RH^2*c*a^2
  
  ; jet power from the "disk" model
  Pjet[j]=B^2*H^2*R0_^2*(omega+Omega_disk)^2/(32.*c)

  ; jet power for Ges model
  ges_delta = 2.5
  ges_gamma = (0.002/(a-0.65)^2.)+(0.1/(a+0.95))+(0.002/(a-0.055)^2.)
  ges_beta = (-(3./2.)*a^3.)+(12.*a^2.)-(10.*a)+7.-ges_gamma
  ges_alpha = ges_delta*((3./2.)-a)
  Pjet_ges[j]=2d47*ges_alpha*ges_beta^2.*(B/1d5)^2.*(m/1d9)^2.*a^2.
endfor

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
set_plot, 'PS'
device, filename='nemspin.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Helvetica'
ytex = textoidl('log P_{jet} [erg s^{-1}]')
xtex = textoidl('j')
xmin = min(spin)
xmax = max(spin)
xmin = 0.7
ymax = 46
ord = where(Pjet gt 0)
ymin = min(alog10(Pjet[ord]))
ymin=45
plot, spin, alog10(Pjet), $
      /xsty, /ysty, $
      xtitle = xtex, $
      ytitle = ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.2, $
      position = aspect(1.0), $
      linestyle=0
oplot, spin, alog10(Pjet_bz), linestyle=3
oplot, spin, alog10(Pjet_ges), linestyle=2
oplot, [-100,100], replicate(alog10(pcav),2), linestyle=2
;items = [textoidl('\alpha = 0.2'), $
;         textoidl('\dot{m} = 0.02')]
;legend, items, psym=[0,0], box=0, charsize=1.0, /top, /left
device, /close

end
