;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;	rspsfp
;
;*PURPOSE:
;  Calculate point spread function (normalized surface brightness)
;    for Rosat PSPC
;
;*CALLING SEQUENCE:
;	rspsfp, offcen, energy, offang, psf, ierr=ierr, chatter=chatter
;
;*PARAMETERS:
;  OFFCEN  - Off axis angle of (center of) PSF (in arcmin)
;  ENERGY  - photon energy (keV)
;  OFFANG  - off axis angle = angle from PSF position (arcsec)
;  CHATTER - controls program feedback to user (default = 1)
;
;*OUTPUTS:
;  IERR  - contains codes for error conditions:
;          0 = no error, 1 = energy outside bounds 0.07-3 keV, 2 = ?, 3 = ?
;  PSF   - surface brightness of point spread function, normalized so that  
;          Integral 2*PI*r*dr*f   from 0 to infinity = 1   [1/arcsec2]
;
;*RESTRICTIONS:
;  Formulae diverge for energies above 2.0 keV. Program sets all energies
;    above 2 keV equal to 2.
;  Will not reproduce effects due to ghost images, important for energies 
;    below 0.15 keV.
;
;*NOTES:
;
;   The following is taken from the comments written by G. R. Hasinger:
;
;   The PSPC on-axis PSF is a combination of three, physically well 
;   understood terms:
;
;   1. a Gaussian for the intrinsic PSPC resolution due to the inherent
;      statistics of the primary electron generation. Theoretically
;      the Gaussian Sigma is propotrional to 1/SQRT(Energy)
;
;   2. an exponential function due to the finite penetration depth of
;      the X-rays in the counter gas combined with the 8.5 degree cone
;      angle. The PSPC is focussed for 1 keV; the 'chromatic aberration'
;      is largest for large energies
;
;   3. A Lorentz function for the mirror scattering; theoretically, the
;      behavior increases as the square of the energy if the grazing 
;      angle is constant, so diffraction forces the shape parameters to
;      vary as inverse energy.
;
;   Technically, these 3 components should be folded together, but their
;   angular domains are sufficiently separated that simply adding them
;   together is reasonably accurate.  Detailed comparison with calibration
;   data at monochromatic energies of 0.28, 0.93, 1.49, and 1.70keV provide
;   the parameter values.  No fit is possible below 0.15keV (channel 15) 
;   due to the PSPC electronics which give rise to additional 'ghost
;   images'. These events should be avoided as far as possible in PSF 
;   modelling.
;
;   The off-axis blur of the telescope, although highly structured and
;   asymmetric, can be modeled by a simple Gaussiane in its radially
;   integrated profile.  This Gaussian is added in quadrature to the
;   Gaussian of the detector.  Since the PSF is not convolved, but a 
;   simple addition of terms, the contribution of the exponential term
;   must be diminished while the Gaussian is "eating up" the exponential.
;   This is modelled as a Gaussian decay of the exponential term as a 
;   function of the off-axis angle.
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted 22 Jun 1992 from German Fortran code (G. Hasinger) for PSPC
;    modified 12 July 1993 (GAR) to update description in prologue and
;       to incorporate changes for calculating off-axis psf
;    modified 25 Aug 1993 (GAR) to reduce norm of scattering term (A3) by
;       30% (i.e., multiply by 0.70)
;    modified 10 Sep 1993 (GAR) to fix bug in defining Gaussian sigma and
;       to incorporate Jane's fix to fnor3 (for the scattering fraction)
;    modified 16 Sep 1993 (GAR) to change term in scattering norm from
;       0.059*0.7 to 0.075 (Gunther Hasinger's newest value)
;    modified 18 Nov 1993 (GAR) to correct documentation prologue
;-
;-------------------------------------------------------------------------------
pro rspsfp,offcen,energy,offang,psf,ierr=ierr,chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSPSFP, offcen (arcmin), energy (keV), offang (arcsec), PSF '+$
        '(1/arcsec^2),'
  print,'         IERR = IERR, chatter = chatter (1)'
  return
endif
if (n_elements(chatter) eq 0) then chatter = 1        ;default is 1
;
pi = 3.14159
efac = alog(10.)
ierr = 0
if ((energy le 0.07) or (energy ge 3.0)) then begin    ;outside bounds
  ierr = 1
  psf = 0.
  if (chatter eq 1) then print,' Energy outside bounds. Returning with ierr=1.'
  return
endif
;
if ((energy lt 0.15) and (chatter eq 1)) then print,$
   'Input energy < 0.15 keV. Watch out for ghost images.'
;
; Formulae diverge for energies above 2.0 keV. Set higher energies to 2 keV.
;
If (energy gt 2.0) then begin
  if (chatter eq 1) then print,$
     'Formulae diverge for E > 2.0 keV. Setting to 2.0.'
  energy = 2.0
endif
;
; Get energy-dependent parameters
;
; Exponential fraction; expression with exponent describes 
; diminishing of the exponential part for off-axis angles due to 
; increase of the Gaussian part. The exponential fraction has 
; been updated and care is taken for the artifact above 2 keV
;
; The old formula for the exponential fraction was:
;         A2 = 10**(-1.618 + 0.507*E + 0.148*E*E) 
;
a2 = -1.635 + 0.639*energy + 0.052*energy*energy       ;new PSF constants
a2 = exp(efac*a2)
a2 = a2*exp(-0.5*(offcen/12.0)*(offcen/12.0))          ;new factor
;
; Scattering fraction: A3 = 0.059*E**1.43
;
a3 = 0.075*(energy^1.43)     ;new scattering factor as of 16 Sep 1993
a2 = a2 < (1.0 - a3)         ;new statement to cure exponential artifact.
;
; Gaussian fraction (total integral under the PSF is 1)
;
a1 = 1.0 - A2 - A3
;
; Gaussian sigma: SIGMA = SQRT(108.7*E**(-0.888)+1.121*E**6)
; in new formula, change sigma to sigdet, and convolve with mirror sigma
;
sigdet2 = 108.7*energy^(-0.888) + 1.121*energy^6
;
; Gaussian sigma - Mirror (units = arc sec) -- new definition
;
sigmir2 = 0.219 * offcen^2.848
sigma = sqrt(sigdet2 + sigmir2)
;
; Exponential e-folding angle: RC = SQRT(50.61*E**(-1.472)+6.80*E**5.62)
; (units = arc sec)
;
rc = sqrt(50.61*energy^(-1.472) + 6.80*energy^5.62)
;
; Scattering Lorentzian break angles: BREAK1 = 39.95/E, BREAK2=861.9/E
;
break1 = 39.95/energy
break2 = 861.9/energy
;
; Scattering Lorentzian slope: ALPHA2 = 2.119+0.212*E
;
alpha2 = 2.119 + 0.212*energy
;
; Normalization by integrals 0-infinity
;
fnor1 = a1/(2.*pi*sigma*sigma)
fnor2 = a2/(2.*pi*rc*rc)
;
; This was the old code; we think it's wrong:
;       fnor3 = a3/(pi*( alog(break1*break1 + break2*break2) - $
;               alog(break1*break1) + 2./(alpha2-2.) ))
;
; And this is the code from Eric:
;
;        aux = 1.0 + bk2 * bk2 / bk1 / bk1
;        fn3 = a3 / ( pi * ( log( aux ) + 
;     +                   2.0 / ( aux * (alpha - 2.0 ) ) ) ) 
;
; and this is Jane's fix because we think that the code from Eric
; is wrong:
;
;        aux = 1.0 + bk2 * bk2 / bk1 / bk1
;        bk3 = bk2 * bk2 /bk1 / bk1
;        fn3 = a3 / ( pi * ( log( aux ) + 
;     +                   2.0 / bk3 / ( aux * (alpha - 2.0 ) ) ) ) 
;
aux = 1.0 + break2 * break2 / break1/ break1
break3 = break2 * break2 / break1/ break1
fnor3 = a3 /pi/( alog(aux) + 2.0 / break3 /aux/(alpha2-2.0) )
;
; calculate point spread function
;
arg1 = 0.5*(offang/sigma)*(offang/sigma) < 75.
arg2 = (offang/rc) < 75.
;
term3 = offang*0.
ind = where(offang le break2) 
if (ind(0) ge 0) then $
   term3(ind) = 1./(break1*break1 + offang(ind)*offang(ind) )
ind = where(offang gt break2)
if (ind(0) ge 0) then $
   term3(ind) = (offang(ind)/break2)^(-alpha2)/(break1*break1 + break2*break2)
;
psf = fnor1*exp(-arg1) + fnor2*exp(-arg2) + fnor3*term3
;
return
end     ;pro rspsfp
