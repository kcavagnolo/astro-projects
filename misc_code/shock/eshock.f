CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute shock age, energy etc from model and pre-shock data
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      implicit none
      include 'const.inc'
      real*8 rsmodel,rspixel,pixsize,kpcpersec,rskpc,rssi,rmodcore
      real*8 dunit,kt0,vunit,tunit,agemodel,age,eta,r0pixel,r0si,n0,rho0
      real*8 kick,eshock,mass,eaten
      print *,'Source scale (kpc/arcsec)?'
      read *,kpcpersec
      print *,'Pixel size (arcsec)?'
      read *,pixsize
      print *,'Shock radius (pixels)?'
      read *,rspixel
      rskpc=rspixel*pixsize*kpcpersec
      print *,'Shock radius (kpc):',rskpc
      rssi=rskpc*kiloparsec
      print *,'Shock radius (model units)?'
      read *,rsmodel
      dunit=rssi/rsmodel
      print *,'Model distance unit (kpc):',dunit/kiloparsec
      print *,'Preshock temperature (keV)?'
      read *,kt0
      vunit=sqrt(kt0*keV/(mu*m_H))
      print *,'Model velocity unit (km/sec):',vunit*1.d-3
      tunit=dunit/vunit
      print *,'Model time units (y):',tunit/year_secs
      print *,'Shock age (model units)?'
      read *,agemodel
      age=tunit*agemodel
      print *,'Shock age (y):',age/year_secs
      print *,'Radius at which to normalize preshock density (pixels)?'
      read *,r0pixel
      r0si=r0pixel*pixsize*kpcpersec*kiloparsec
      print *,'Preshock electron density at normalizing',
     &     ' radius (cm^{-3})?'
      read *,n0
      rho0=n0*1.d6*rho_ne
      print *,'eta (for rho ~ r**(-eta))?'
      read *,eta
      print *,'Radius of initial hot core of model (model units)?'
      read *,rmodcore
      mass=4.d0*pi*(r0si)**3*rho0
     &     *(rmodcore*rspixel/(rsmodel*r0pixel))**(3.d0-eta)/(3.d0-eta)
      print *,'Model kick parameter?'
      read *,kick
      eshock=1.5d0*((kt0*keV)/(mu*m_H))*(kick-1.d0)*mass
      print *,'Shock energy (J):',eshock
      print *,'Mean shock power (W):',eshock/age
      eaten=eshock/(0.1d0*c_light**2*Msun)
      print *,'Minimum mass swallowed (Msun):',eaten
      end
