C ______________________________	 Selects and Scales PM particles 
C		             December 1997         A. Klypin (aklypin@nmsu.edu) 
C		 Get a random fraction of particles from a parallelipiped:
C            Xmin < x < Xmax, Ymin < y < Ymax, Zmin < z < Zmax
C          Scales internal PM coordinates and velocities 
C          to  coordinates in Mpc/h and km/s
C          In PM the coordinates are in the range 1 - (NGRID+1)
C                     velocities are P = a_expansion*V_pec/(x_0H_0)
C                     where     x_0 = comoving cell_size=Box/Ngrid
C                                    H_0 = Hubble at z=0
C                   
C	     NROW = number of particles in 1D
C	     NGRID= number of cells        in 1D
       INCLUDE 'PMparameters.h'
       REAL     INPUT
       Character*50  FileASCII

C...................................................................
C			Read data and open files
      CALL RDTAPE
      write (*,*) ' RDTAPE is done'
      WRITE (*,'(A,$)') 'Enter File Name for ASCII data = '
      READ  (*,'(A)') FileASCII
      OPEN(17,FILE=FileASCII,STATUS='UNKNOWN')
       WRITE (17,100) HEADER,
     +                  AEXPN,AEXP0,AMPLT,ASTEP,ISTEP,PARTW,
     +                  EKIN,
     +                  NROWC,NGRID,NRECL,Om0,Oml0,hubble,
     +                  Ocurv
100   FORMAT(1X,'Header=>',A45,/
     +            1X,' A=',F8.3,' A0=',F8.3,' Ampl=',F8.3,' Step=',F8.3,/
     +            1X,' I =',I4,' WEIGHT=',F8.3,' Ekin=',E12.3,/
     +            1X,' Nrow=',I4,' Ngrid=',I4,' Nrecl=',I6,/
     +            1x,' Omega_0=',F7.3,' OmLam_0=',F7.4,' Hubble=',f7.3,/
     +            1x,' Omega_curvature=',F7.3)
      Box  =INPUT(' Enter box size in comoving Mpc/h =')
      Xmin =INPUT('  X_min (Mpc/h)=')
      Xmax =INPUT(' X_max (Mpc/h)=')
      Ymin =INPUT(' Y_min (Mpc/h)=')
      Ymax =INPUT(' Y_max (Mpc/h)=')
      Zmin =INPUT(' Z_min (Mpc/h)=')
      Zmax =INPUT(' Z_max (Mpc/h)=')
      Fraction =INPUT(' Enter fraction of particles=')
 
      BoxV    = Box*100.    ! Box size in km/s
      ScaleV = BoxV/AEXPN/NGRID  ! scale factor for Velocities
      ScaleC = Box/NGRID         ! scale factor for Coordinates
      Fraction =MAX(Min(Fraction,1.),0.)
      xxmax =-1.e+9
      xxmin = 1.e+9
      vmax  =-1.e+9
      vmin  = 1.e+9
      xrand=0.
      Icount =0
      Nseed =12071

      DO ifile =1,Nspecies+1                         ! Loop over all files
       DO IROW=1,NROW                           ! Loop over all pages
c                   write (*,*) ' Read page=',IROW,' file=',ifile
	      CALL GETROW(IROW,ifile)
	      DO IN=1,NPAGE                           !Loop over particles
	        X  =ScaleC* (XPAR(IN)-1.)         ! scale coordinates
	        Y  =ScaleC* (YPAR(IN)-1.)
	        Z  =ScaleC* (ZPAR(IN)-1.)
           If(Z.gt.Zmin .and. Z.le.Zmax)Then    ! take particles only 
           If(Y.gt.Ymin .and. Y.le.Ymax)Then   ! inside the box
           If(X.gt.Xmin .and. X.le.Xmax)Then
              If(Fraction.lt.0.999)xrand =RANDd(Nseed) ! random fraction
              IF(xrand.lt.Fraction)Then                !  get the particle
                 Vxs=ScaleV* VX(IN)                    ! scale velocities
                 Vys=ScaleV* VY(IN)
                 Vzs=ScaleV* VZ(IN)
                 Icount =Icount +1                          ! count particles
                 xxmax =MAX(xxmax,X,Y,Z)
                 xxmin =MIN(xxmin,X,Y,Z)
                 vmax =MAX(vmax,Vxs,Vys,Vzs)
                 vmin =MIN(vmin,Vxs,Vys,Vzs)
                 write (17,200) IN,X,Y,Z,Vxs,Vys,Vzs  ! write to a file
              EndIf
 200          Format(i8,3F9.4,3F9.2)
           EndIf
           EndIf
           EndIf
	      ENDDO
        ENDDO 
      ENDDO

      write (*,*) ' Scaled Coordinates were written to:',
     &            FileASCII
      write (*,*) ' Number of particles          =',Icount,
     &               ' Fraction=',Fraction
      write (*,*) ' Limits:', Xmin,Xmax
      write (*,*) '        ', Ymin,Ymax              
      write (*,*) '        ', Zmin,Zmax
      write (*,*) ' Min/Max of coordinates(Mpc/h)=',xxmin,xxmax
      write (*,*) ' Min/Max of velocities (km/s) =',vmin,vmax

      END


