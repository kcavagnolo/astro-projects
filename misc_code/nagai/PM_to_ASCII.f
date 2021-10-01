C ______________________________	 Convert PM format to ASCII 
C		             November 1997         A. Klypin (aklypin@nmsu.edu) 
C		                     
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
       Character  FileASCII*50
C...................................................................
C			Read data and open files
      WRITE (*,'(A,$)') 'Enter File Name for ASCII data = '
      READ  (*,'(A)') FileASCII
      OPEN(17,FILE=FileASCII,STATUS='UNKNOWN')
      CALL RDTAPE

      write (*,*) ' RDTAPE is done'
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
      BoxV =Box*100.    ! Box size in km/s
      ScaleV = BoxV/AEXPN/NGRID  ! scale factor for Velocities
      ScaleC = Box/NGRID         ! scale factor for Coordinates

      xmax =-1.e+9
      xmin = 1.e+9
      vmax =-1.e+9
      vmin = 1.e+9
      Icount =0
      DO ifile =1,Nspecies+1
C				       Loop over particles
       DO IROW=1,NROW
c                   write (*,*) ' Read page=',IROW,' file=',ifile
	      CALL GETROW(IROW,ifile)
	      DO IN=1,NPAGE
	        X  =ScaleC* (XPAR(IN)-1.)
	        Y  =ScaleC* (YPAR(IN)-1.)
	        Z  =ScaleC* (ZPAR(IN)-1.)
           Vxs=ScaleV* VX(IN)
           Vys=ScaleV* VY(IN)
           Vzs=ScaleV* VZ(IN)
           Icount =Icount +1
           xmax =MAX(xmax,X,Y,Z)
           xmin =MIN(xmin,X,Y,Z)
           vmax =MAX(vmax,Vxs,Vys,Vzs)
           vmin =MIN(vmin,Vxs,Vys,Vzs)
           write (17,200) IN,X,Y,Z,Vxs,Vys,Vzs
 200       Format(i8,3F8.3,3F9.2)
	      ENDDO
        ENDDO 
      ENDDO

      write (*,*) ' Scaled Coordinates were written to:',
     &            FileASCII
      write (*,*) ' Number of particles          =',Icount
      write (*,*) ' Min/Max of coordinates(Mpc/h)=',xmin,xmax
      write (*,*) ' Min/Max of velocities (km/s) =',vmin,vmax

      END


