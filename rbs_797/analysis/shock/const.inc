CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Useful constants mostly from CRC handbook and
C (http://hpiers.obspm.fr/eop-pc/models/constants.html)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C Speed of light
      real*8 c_light
      parameter (c_light=299792458.d0)
C Tropical year (exact)
      real*8 year_days,year_secs
      parameter (year_days=365.242190402d0,year_secs=year_days*86400.d0)
C Newton's gravitational constant (recommended and recent)
      real*8 G_newton,G_newton_quinn
      parameter (G_newton=6.673d-11,G_newton_quinn=6.67559d-11)
C Solar mass as GM
      real*8 GMsun,Msun
      parameter (GMsun=1.32712442076d20,Msun=GMsun/G_newton)
C AU and parsec
      include 'pi.inc'
      real*8 Astr_unit,parsec,kiloparsec,megaparsec
      parameter (Astr_unit=1.49597870d11)
      parameter (parsec=Astr_unit*(180.d0*3600.d0/pi))
      parameter (kiloparsec=1.d3*parsec,megaparsec=1.d6*parsec)
C Stefan Boltzmann constants (sigma) and a
      real*8 SB_sigma,SB_density
      parameter (SB_sigma=5.670400d-8,SB_density=4.d0*SB_sigma/c_light)
C Electron
      real*8 m_electron,q_electron
      parameter (m_electron=9.10938188d-31,q_electron=1.602176462e-19)
C Proton, hydorgen, helium
      real*8 m_proton,m_H,m_alpha,m_He
      parameter (m_proton=1.67262158d-27,m_H=m_proton+m_electron)
      parameter (m_alpha=6.64465598d-27,m_He=m_alpha+4.d0*m_electron)
C Energy conversion
      real*8 keV
      parameter (keV=1.d3*q_electron)
C Boltmann constant
      real*8 k_boltzmann
      parameter (k_boltzmann=1.3806503d-23)
C Mass fractions of helium and hydrogen and approximate mean mass per
C particle in fully ionized H, He gas
      real*8 f_He,f_H,mu,ntot_ne,nh_ne,rho_ne
      parameter (f_He=0.25d0,f_H=1.d0-f_He)
      parameter (mu=1.d0/(2.d0*f_H+3.d0*f_He*m_H/m_He))
      parameter (ntot_ne=(2.d0*f_H+3.d0*f_He*m_H/m_He)
     &     /(f_H+2.d0*f_He*m_H/m_He))
      parameter (nh_ne=f_H/(f_H+2.d0*f_He*m_H/m_He))
      parameter (rho_ne=m_H/(f_H+2.d0*f_He*m_H/m_He))
