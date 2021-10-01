C-------------------------------------------------
C        A.Klypin 25 Nov.1997 (NMSU)
C                            Bound-Density-Maxima code:
C                            Find  maxima of mass in spheres of radius Rsearch1 
C                            for particles in a periodic cube     of size     Box
C                           Coordinates: 0-NGRID,  Box =NGRID
C       All variables and constants are scaled to the Hubble constant of your
C       model. Only FINAL values are rescled to H=100km/s/Mpc
C       More detailed description of the algorithm and the code can be
C       found in file BDM_describe
C--------------------------------------------------   
C       Arrays Label and Lst provide quick access to neibhor particles
C       Label(i,j,k) = number of the first particle in
c                      the cell (i,j,k), if Label .ne. 0
C                    = 0 - empty cell
C       Cell         = size of a cell for Label
C       Lst(i)       = number of the next particle in the cell,
C                      where i-th was found.
C                      If Lst=0 - last point in the cell was found.
C     
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      Real             INPUT,Mhalo
      Real             Overdens(Nrad),Mass(Nrad),MnMass(Nrad)
      COMMON /OVER/ Ovlim,Ovcnt
      DIMENSION      Radsc(Nrad)

      Ovcnt   =INPUT('Enter Min. Center Overdensity for Halos   => ')
      Ovlim   =INPUT('Enter Overdensity Threshold for Halos     => ')
      Amassl  =INPUT('Enter Minimum halo mass in Msun/h         => ')      
      Rsearch1=INPUT('Enter comoving search radius(Mpc/h)       => ')
      Rsearch2=INPUT('Enter smaller radius(Mpc/h) of final halos=> ')
      Rminhalo=INPUT('Enter min.radius for halos(Mpc/h)         => ')
      Fract   =INPUT('Enter fraction of DM particles (1/4,1/2,1)=> ')
      Fract   =Min(Max(Fract,0.),1.0)                             
      Toohot  =INPUT('Enter rejection velocity limit (V/Vescape)=> ')
      dRdubl  =INPUT('Distance to check for Velocity duplicates => ')
      fVdubl  =INPUT('Define duplicates if  (v1-v2)/Vrms   <       ')     
      Box     =INPUT('Enter Comoving Box size(Mpc/h)            => ')
      write (*,*)
C     ------------------------------------------ Open files
      Open( 2,file='Catshort.DAT')   ! short list of halos
      Open( 3,file='ftn25.dat')   ! short list of halos
      Open(20,file='Catalog.DAT')   ! complete catalog with data for shells
C     ------------------------------------------ Read data
      CALL RDTAPE
            write (*,*) ' RDTAPE is done'
            write (*,*) ' Nnu=Number of Species=',Nspecies
      CALL ReadPnt(N,Fract)                     ! particles
         WRITE (2,100) HEADER,
     +               AEXPN,AEXP0,AMPLT,ASTEP,ISTEP,PARTW,
     +               EKIN,NROW,NGRID,Om0,Oml0,hubble
         WRITE (20,100) HEADER,
     +               AEXPN,AEXP0,AMPLT,ASTEP,ISTEP,PARTW,
     +               EKIN,NROW,NGRID,Om0,Oml0,hubble
100      FORMAT(1X,'Header=>',A45,/
     +         ' A=',F8.3,' A0=',F8.3,' Ampl=',F8.3,' Step=',F8.3,/
     +         ' I =',I4,' WEIGHT=',F8.3,' Ekin=',E12.3,
     +         ' Nrow=',I4,' Ngrid=',I4,/' Omega_0=',f6.3,
     +         ' Omega_L=',f6.3,' h=',f6.3)
C     ------------------------------------------ Constants ---------
           hsmall= hubble            ! Hubble/100.
           Box   = Box /hsmall       ! Scale to real Mpc          
           Omcold= Om0 
           Omhot = 0.
           Amassl= Amassl/hsmall     ! Scale to real masses
           Radius= Rsearch1/hsmall  ! Comoving Search radius in real Mpc
           Rminhalo=Rminhalo/hsmall
           dRdubl= dRdubl/hsmall
           Xscale= Box/Ngrid        ! Scale for comoving coordinates
           Vscale= 100.*hsmall*Xscale/AEXPN ! Scale for velocities
           Dscale= 2.746e+11*hsmall**2*(Box/NROW)**3 ! mass scale
C                            Dscale is a factor used to convert particles to mass 
C                                       =  mass of a particle for Omega=1 model
C                            Xscale is a factor to convert PM coordinates into Mpc
C                                       = Box / Ngrid = length of a PM cell in Mpc
C                           Vscale is a factor to convert PM pomenta to V (km/sec)
C                                       = the Hubble velocity at the distance of 1 PM cell
C                              the factor AEXPN is needed to go from momenta to pec.velocity
           write (*,*) ' Vscale=',Vscale, ' X=',Xscale,' Box(Mpc)=',Box
           Radius=Radius/Xscale    ! comoving radius in cell units
           Cell  =2.0              ! Cell-size for Linker List in grid units
           Box   =NGRID            ! all internal variables are in Grid units
           Riter =0.0001           ! Error for iteration of radius in cell units
           Rmax  =Radius           ! Treat maxima Identical  if dR<Rmax
           imin  =INT(-Radius/Cell)! Make recommendations. Code works 
           imax  =INT((Box+Radius)/Cell) ! if the limits are not optimal
                   If(imin.lt.Nm)Write(*,*)
     .               ' Wrong low limit:',Nm,' better be',imin
                   If(imax.gt.Nb)Write(*,*)
     .               ' Wrong upper limit:',Nb,' better be',imax
           write (*,*) 
     .        ' Box(cells)=',Box,' Cell=',Cell,' Rad(cells)=',Radius
         DtoNscale = Om0*Dscale/Fract ! scale dark matter particles
         ANlimit =Amassl/(DtoNscale)         ! and this
           R0    =0.004
           Rnrad =R0*((Nrad-1)/2.+1.)**1.5 
           Expect =10.*Ovlim*(NROW/float(NGRID))**3/Fract *
     &                   (4.1888* (R0/hsmall/Xscale)**3 )
           write (*,'(/" The first and the last shells have radii:",
     &                   2f8.4," Mpc/h, comoving")')  R0,Rnrad
           write (*,'( " Mass of a particle is                    ",
     &                   g11.3," M_sun/h")')          Om0*Dscale*hsmall

           write (*,'( " Halos will have more than                ",
     &                   g11.3," particles",/
     &                  "     inside search radius=                ",
     &                   F7.3," Mpc/h =", F7.2," cell units")')
     &                   ANlimit,Rsearch1,Rsearch1/hsmall/Xscale
           write (*,'( " Expected Nparticles in the first shell   ",
     &                   g11.3)') Expect
           write (*,'( "     assuming overdensity                 ",
     &                   g11.3)') Ovlim*10.     
           write (*,*) '   If you want to change shells, enter a factor'
           write (*,'(A,$)') '    or 1 to keep current radii : '
           read  (*,*) factor
           If(abs(factor-1.).ge.1.e-3.and.factor.gt.0.)Then
              R0    =0.004 * factor
              Rnrad =R0*((Nrad-1)/2.+1.)**1.5           
              write (*,*) '   The first and the last radii for halos'
              write (*,*) '   will be',R0,Rnrad,' Mpc/h, comoving'
           EndIf
           Do ir=1,Nrad                            ! Radii for halo shells
              Rad(ir) =R0/hsmall*((ir-1)/2.+1.)**1.5/Xscale ! comoving
              Rad2(ir)=Rad(ir)**2
           EndDo
           write (*,*) ' Radii(Mpc/h): 1st=',Rad(1)*Xscale*hsmall,
     +                  ' last=',Rad(Nrad)*Xscale*hsmall,' h=',hsmall
c------------------------------------------ Prepare Data
           RadOld =Radius
           Radius  =Rad(Nrad)            ! Make radius temporarily bigger
      CALL Points(Box,N,Ncp)            ! Make more points periodically
          write (*,*) ' Nparticles in periodic set=',Ncp,
     .              '( Maximim =',Np,')'
          write (2,*) ' Nparticles in periodic set=',Ncp,
     .              '( Maximum =',Np,')'
           Radius =RadOld                ! Restore radius
      Call List(Box,Ncp)                ! make linker List, Label
c                 CALL ReadMax(Ncentr,Xscale)                  ! read centers from a file
      CALL ReadCent(N,Ncentr)   ! read initial centers
          write (*,*) ' Nparticles=',N,' Ncenters=',Ncentr
C----------------------------- Find maxima
      CALL Pairs(Box,N,Ncentr,Riter)       ! find maxima 
      write (*,16)  Amassl*hsmall,ANlimit,Rminhalo*hsmall,
     &               Rsearch1,Rsearch2,Toohot
      write (2,16)  Amassl*hsmall,ANlimit,Rminhalo*hsmall,
     &               Rsearch1,Rsearch2,Toohot
      write (20,16) Amassl*hsmall,ANlimit,Rminhalo*hsmall,
     &               Rsearch1,Rsearch2,Toohot
      write (*,*) ' Om0=',Om0,' Dscale=',Dscale
 16   format(5x,'Small halos with Mass(M_sun/h) less than',
     .       g10.3,'(N_eff<',g11.3,') were removed',/
     .       5x,'Minimum Radius of Halo(Mpc/h)=',f7.4,/
     .       5x,'Comov.Search radius(Mpc/h)=',f7.4,
     .         ' Second radius(Mpc/h)=',f7.4,/
     .       5x,'Particles with V>',f5.1,'V_escape were removed')
      CALL SmallRem(ANlimit,Ncentr)  ! Remove small halos
      CALL CATALOG(Box,Ncentr)      ! Remove duplicates
C                                         Final touch: if Rsearch1 and Rsearch2 are different
C                                               then  improve position of halos by shrinking
C                                              the search Radius to Rsearch2 in three steps
      IF(ABS(Rsearch1-Rsearch2).gt.0.001*Rsearch1)Then
        Radius= (Rsearch1*0.667+Rsearch2*0.333)/hsmall/Xscale
        write (*,*) '    Iterate: Comoving Radius for halos(Mpc/h)=',
     &                   Radius*Xscale*hsmall
        CALL Pairs(Box,N,Ncentr,Riter) 

        Radius= (Rsearch1*0.333+Rsearch2*0.667)/hsmall/Xscale
        write (*,*) '    Iterate: Comoving Radius for halos(Mpc/h)=',
     &                   Radius*Xscale*hsmall
        CALL Pairs(Box,N,Ncentr,Riter) 

        Radius= Rsearch2/hsmall/Xscale
        write (*,*) '    Iterate: Comoving Radius for halos(Mpc/h)=',
     &                   Radius*Xscale*hsmall
        CALL Pairs(Box,N,Ncentr,Riter) 
        CALL CATALOG(Box,Ncentr)      !  Remove duplicates
      EndIf
       write (*,20) N,Ncentr,Box*Xscale*hsmall,Ovlim,Fract
       write (2,20) N,Ncentr,Box*Xscale*hsmall,Ovlim,Fract
       write (20,20)N,Ncentr,Box*Xscale*hsmall,Ovlim,Fract
20         Format(5x,'Statistics for ',i9,' points. Maxima=',i6,/
     .         5x,'Box(Mpc)=',f6.1,
     .            5x,'Overdensity Limit=',f6.1,
     .             ' Fraction of DM particles=',f5.3)
      CALL SPairs(Box,N,Ncentr)  ! Accumulate  statistics 
 
C-----------------------------      Auxiliary Settings
      nfalse =0
      Do ir=1,Nrad                  ! all scaled to hubble, not h^(-1)
         Radsc(ir) =Rad(ir) *Xscale ! scale radii to comoving Mpc
         MnMass(ir)=Om0*4.188*2.746e+11*hubble**2*Radsc(ir)**3 
c                                   mass at the mean density
      EndDo
      write (*,12) (Radsc(i)*hsmall,i=1,Nrad)
      write (2,12) (Radsc(i)*hsmall,i=1,Nrad)
      write (20,12) (Radsc(i)*hsmall,i=1,Nrad)
 12   Format(5x,'Radii of shells in comoving Mpc/h:',/3(4x,10f7.4/))
C---------------------------- Process Halos : Undind, Dublicates 
      CALL CLEAN(Ncentr,Xscale,Vscale,Dscale, 
     +            Fract,Radsc,MnMass,Toohot,Rminhalo)
      IF(fVdubl .gt.0. .and. dRdubl.gt.0.)Then    !  Remove VelDublicates
          dRdubl =dRdubl/Xscale                       ! scale to cell units
          fVdubl =fVdubl
         CALL RemVelDubl(Box,Ncentr,dRdubl,fVdubl,Xscale,Vscale) 
         write (*,25) Ncentr,dRdubl*Xscale*hsmall ,fVdubl
         write (2,25) Ncentr,dRdubl*Xscale*hsmall ,fVdubl
         write (20,25)Ncentr,dRdubl*Xscale*hsmall ,fVdubl
 25       Format(5x,'Number of centers=',i6,
     &                   2x,'Velocity Duplicates: dR(Mpc/h)<',f6.3,
     &                   2x,'dV/Vrms<',f6.3)
       ENDIF
C------------------------------- Print Results
      write ( 2,13)
      write (20,13)
 13   Format(6x,'Coordinates Mpc',9x,'Velocity km/s',7x,
     &   'Mass     Radius Vrms(3D) Vcirc Npart(<Rad)')
       write (20,14)
 14   Format('R(Mpc/h)',' N(<R)    M/Msun   Overdens   Vrms  Vcirc',
     &                  ' Dens(R)Msun/Mpc^3')
      Do i=1,Ncentr          ! Scale all to physical units
         Xc    =Xm(i)  *Xscale
         Yc    =Ym(i)  *Xscale
         Zc    =Zm(i)  *Xscale
         irad  =iRadius(i)
         Mhalo =Amc(i)
         Rhalo =Rmc(i)
         Vcirc =Vrm(i)
         Do ir =1,Nrad
            Summass     = Om0*Nbc(i,ir)/Fract
            Mass(ir)    = Dscale*Summass
            Overdens(ir)= Mass(ir)/MnMass(ir)
         EndDo
         If(irad.eq.0.or.Rhalo.le.Rminhalo
     .                .or.Mhalo.le.Amassl)nfalse=nfalse+1
         If((irad.ne.0.and.Rhalo.gt.Rminhalo)
     .                .and.Mhalo.gt.Amassl)Then
            irr =Max(irad,2)         ! don't take V from the first bin
            VXc =Wxc(i,irr)   *Vscale
            VYc =Wyc(i,irr)   *Vscale
            VZc =Wzc(i,irr)   *Vscale
            Nnp =INT(Mhalo/(Dscale*Om0))
             write (20,45) Xc*hsmall,Yc*hsmall,Zc*hsmall,
     &                      VXc,VYc,VZc,Mhalo*hsmall,
     &                      Rhalo*hsmall,Vrmc(i,irr),
     &                      Vcirc,Nnp
             write ( 2,45) Xc*hsmall,Yc*hsmall,Zc*hsmall,
     &                      VXc,VYc,VZc,Mhalo*hsmall,
     &                      Rhalo*hsmall,Vrmc(i,irr),
     &                      Vcirc,Nnp

            Do ir=1,Nrad
               If(ir.eq.1)Then
                  Vvc =Vrmc(i,ir)
                  Volume =4.188*Radsc(ir)**3
                  DenDm=Mass(ir)/Volume
                  If(Nbc(i,ir).le.2)Then ! too few particles
                      Radmean =Radsc(ir)/2.**0.3333
                  Else         ! take real mean radius
                      Radmean  =Rrc(i,ir)*Xscale/Nbc(i,ir)
                  EndIf
                  Vcr    =sqrt(Mass(1)/Radsc(1)/AEXPN)*6.58e-5
               Else              ! remove contribution from inner rad
                  Vvc =sqrt(MAX((Vrmc(i,ir)**2*Nbc(i,ir) -
     .              Vrmc(i,ir-1)**2*Nbc(i,ir-1))/
     .              max(Nbc(i,ir) -Nbc(i,ir-1),1),1.e-10))
                  Volume =4.188*(Radsc(ir)**3- Radsc(ir-1)**3) 
                  DenDm  =(Mass(ir)-Mass(ir-1)) /Volume
                  If(Nbc(i,ir) -Nbc(i,ir-1).le.2)Then ! too few particles
                     Radmean=((Radsc(ir)**3+Radsc(ir-1)**3)/2.)**0.333
                  Else         ! take real mean radius
                     Radmean=(Rrc(i,ir)-Rrc(i,ir-1))*Xscale/
     .                        (Nbc(i,ir)-Nbc(i,ir-1))
                  EndIf
                  Vcr    =sqrt(Mass(ir)/Radsc(ir)/AEXPN)*6.58e-5
                EndIf
               write (20,40) Radmean*hsmall,
     .                       Nbc(i,ir),Mass(ir)*hsmall,Overdens(ir),
     .                       Vvc,Vcr,DenDm/hsmall**2
            EndDo  ! end loop ir
45          Format(F7.3,2F8.3,3F8.1,g11.3,f7.3,2f7.1,I7,I7)
46          Format(F7.3,2F7.3,3F7.1,g10.3,f6.3,f7.1,4F7.2,F7.3)
40          Format(f7.4,I7,g11.3,g10.3,2f7.1,3g10.3)
41          Format(f7.4,I7,I6,g11.3,g10.3,2f7.1,f6.3,g10.3,4G10.3)
         Endif
      EndDo    !  end loop Ncentr
      write (*,50) Ncentr-nfalse,nfalse
c      write (2,50) Ncentr,nfalse
 50   Format('---------- Final Catalog has ',i7,' halos ------------',
     .      /'                    ',i7,' were too puffy or too small') 
      Stop
      End
C--------------------------------------------------------------
C                                           Make linker lists of particles in each cell
      SUBROUTINE List(Box,Ncp)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
             Do i=1,Ncp
                Lst(i)=-1
             EndDo

             Do k=Nm,Nb
             Do j=Nm,Nb
             Do i=Nm,Nb
                Label(i,j,k)=0
             EndDo
             EndDo
             EndDo  
      Do jp=1,Ncp
         i=INT(Xp(jp)/Cell)
         j=INT(Yp(jp)/Cell)
         k=INT(Zp(jp)/Cell)
         i=MIN(MAX(Nm,i),Nb)
         j=MIN(MAX(Nm,j),Nb)
         k=MIN(MAX(Nm,k),Nb)
         Lst(jp)      =Label(i,j,k)
         Label(i,j,k) =jp
      EndDo

      Return
      End
C--------------------------------------------------------------
C                                Find all neibhours for a center
C                                Xc,Yc,Zc - center; a0 -its weight
C
      SUBROUTINE Neib(Xnew,Ynew,Znew,Amnew)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
C                                       Initiate counters
        Xnew =0.
        Ynew =0.
        Znew =0.
  	     Amnew=0.
        Radius2=Radius**2
c                      limits for Label
         i1=INT((Xc-Radius)/Cell)
         j1=INT((Yc-Radius)/Cell)
         k1=INT((Zc-Radius)/Cell)
            i1=MIN(MAX(Nm,i1),Nb)
            j1=MIN(MAX(Nm,j1),Nb)
            k1=MIN(MAX(Nm,k1),Nb)
         i2=INT((Xc+Radius)/Cell)
         j2=INT((Yc+Radius)/Cell)
         k2=INT((Zc+Radius)/Cell)
            i2=MIN(MAX(Nm,i2),Nb)
            j2=MIN(MAX(Nm,j2),Nb)
            k2=MIN(MAX(Nm,k2),Nb)
C                                        Look for neibhours
            nn =0
         Do k=k1,k2
         Do j=j1,j2
         Do i=i1,i2
            jp =Label(i,j,k)
 10         If(jp.ne.0)Then
               dd =(Xc-Xp(jp))**2+(Yc-Yp(jp))**2+(Zc-Zp(jp))**2
               If(dd.lt.Radius2)Then
                  nn =nn +1
	             Xnew =Xnew +Xp(jp)
	             Ynew =Ynew +Yp(jp)
	             Znew =Znew +Zp(jp)
	             Amnew=Amnew+1.
               EndIf  
                  jp =Lst(jp)
               GoTo10
            EndIf
         EndDo
         EndDo
         EndDo
         	If(Amnew.GT.0.)Then
	            Xnew =Xnew /Amnew
	            Ynew =Ynew /Amnew
	            Znew =Znew /Amnew
            EndIf
      Return
      End
C--------------------------------------------------------------
C                                Find all neibhours for a center
C                                Xc,Yc,Zc - center; 
C                               
      SUBROUTINE SNeib
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'

         Radmax =Rad(Nrad)
         Do i=1,Nrad
              Nob1(i) =0
              Rob1(i) =0.
              Vx1(i)  =0.
              Vy1(i)  =0.
              Vz1(i)  =0.
              Vxx(i)  =0.
         EndDo
c                      limits for Label
         i1=INT((Xc-Radmax)/Cell)
         j1=INT((Yc-Radmax)/Cell)
         k1=INT((Zc-Radmax)/Cell)
            i1=MIN(MAX(Nm,i1),Nb)
            j1=MIN(MAX(Nm,j1),Nb)
            k1=MIN(MAX(Nm,k1),Nb)
         i2=INT((Xc+Radmax)/Cell)
         j2=INT((Yc+Radmax)/Cell)
         k2=INT((Zc+Radmax)/Cell)
            i2=MIN(MAX(Nm,i2),Nb)
            j2=MIN(MAX(Nm,j2),Nb)
            k2=MIN(MAX(Nm,k2),Nb)
C                                        Look for neibhours
         Do k=k1,k2
         Do j=j1,j2
         Do i=i1,i2
            jp =Label(i,j,k)   !  Dark matter
 10         If(jp.ne.0)Then
               dd =(Xc-Xp(jp))**2+(Yc-Yp(jp))**2+(Zc-Zp(jp))**2
               Do ir =1,Nrad
                  If(dd.lt.Rad2(ir))Then
                    Nob1(ir)= Nob1(ir)+ 1
                    Rob1(ir)= Rob1(ir)+ sqrt(dd)
                    Vxx(ir) = Vxx(ir) +VXp(jp)**2+VYp(jp)**2+VZp(jp)**2
                    Vx1(ir) = Vx1(ir) +VXp(jp)  
                    Vy1(ir) = Vy1(ir) +VYp(jp)
                    Vz1(ir) = Vz1(ir) +VZp(jp)
                  EndIf
               EndDo
               jp =Lst(jp)
               GoTo10
            EndIf
         EndDo
         EndDo
         EndDo
         Do ir=1,Nrad
            Vmn =0.
	         If(Nob1(ir).GT.0)Then
               Nobj    = Nob1(ir)
               Vx1(ir) = Vx1(ir)/Nobj
               Vy1(ir) = Vy1(ir)/Nobj
               Vz1(ir) = Vz1(ir)/Nobj
               Vmn     = Vx1(ir)**2+Vy1(ir)**2+Vz1(ir)**2
               Vxx(ir) = SQRT(ABS(Vxx(ir)/Nobj -Vmn))
	         EndIf
         EndDo
      Return
      End
C--------------------------------------------------------------
C                       Add points around the Box to take into account periodical conditions
C  It replicates points periodically and keeps those which are at  distance
C  less than Radius from the Box. 
C                        N     = number of points inside the Box
C                        Ncp = total number of points
      SUBROUTINE Points(Box,N,Ncp)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      Logical    Inside
         B1  =-Radius
         B2  =Box+Radius
         B3  =Box-Radius
         Ncp =N
      Do i=1,N               ! Add dark matter particles
         x0 =xp(i)
         y0 =yp(i)
         z0 =zp(i)
         Inside =            (x0.gt.Radius).and.(x0.lt.B3)
         Inside =Inside.and.((y0.gt.Radius).and.(y0.lt.B3))
         Inside =Inside.and.((z0.gt.Radius).and.(z0.lt.B3))
         If(.not.Inside)THEN         
         ux =VXp(i)
         uy =VYp(i)
         uz =VZp(i)
            Do k1 = -1,1
               zr =z0+k1*Box
               If(zr.GT.B1.AND.zr.LT.B2)Then
               kk =k1**2
               Do j1 = -1,1
                  yr =y0+j1*Box
                  If(yr.GT.B1.AND.yr.LT.B2)Then
                     jj = kk +j1**2
                     Do i1 = -1,1
                        xr =x0+i1*Box
                        ii =jj +i1**2
                        If((xr.GT.B1.AND.xr.LT.B2).AND.ii.ne.0)
     .                     Then
                             Ncp =Ncp +1
                             If(Ncp.le.Np)Then
                               xp(Ncp)  =xr
                               yp(Ncp)  =yr
                               zp(Ncp)  =zr
                               VXp(Ncp)  =ux
                               VYp(Ncp)  =uy
                               VZp(Ncp)  =uz
                             EndIf
                        EndIf
                     EndDo
                  EndIf
               EndDo
               EndIf
            EndDo
         EndIf
      EndDo
      If(Ncp.Gt.Np)Then
         write(*,*)' Too many points:',Ncp,' Maximum is =', Np
         STOP
      EndIf
      Return
      End
C-------------------------------------------------------------
C                     Find positions of 'Ncentr' spheres, which maximize number of particles
C                    inside 'radius Radius'. 
C                    Iterations are repeated until 1) mass stops increasing   OR
C                                                       2) the sphere stops moving: dR is less than Riter 
C                    Looks for all particles inside Radius for periodical set 
      SUBROUTINE Pairs(Box,N,Ncentr,Riter)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      INTEGER Lab(Nc)
      Do i=1,Ncentr
      	Lab(i) =1
         Amc(i) =0
         Rmc(i) =Radius
      EndDo
      Niter =0
10    Iter  =0
      Niter =Niter +1
      Do i=1,Ncentr
         Xc =Xm(i)
         Yc =Ym(i)
         Zc =Zm(i)
         a0 =Amc(i)
         in =Lab(i)
         If(in.gt.0)Then         ! If not converged, keep iterating
            Call Neib(Xnew,Ynew,Znew,Amnew) ! new center of mass
            Dr =MAX(ABS(Xnew-Xc),ABS(Ynew-Yc),ABS(Znew-Zc))
            IF(Dr.LT.Riter.or.Amnew.lt.Amc(i))
     .                  Lab(i)=-Niter     ! iterations converged
            Iter =Iter +1
	         IF(Xnew.lt.0.)Xnew =Xnew+Box
	         IF(Ynew.lt.0.)Ynew =Ynew+Box
	         IF(Znew.lt.0.)Znew =Znew+Box
	         IF(Xnew.gt.Box)Xnew =Xnew-Box
	         IF(Ynew.gt.Box)Ynew =Ynew-Box
	         IF(Znew.gt.Box)Znew =Znew-Box
            Xm(i)  = Xnew
            Ym(i)  = Ynew
            Zm(i)  = Znew
            Amc(i) = Amnew
         EndIf
      EndDo
      Write (*,*) ' Iteration=',Niter,' Centers left=',Iter
      If(Iter.GE.1 .and. Niter.LT.100)Goto10
      Return
      End
C-------------------------------- Remove dublicates in Velocity
C                   If distance is less than 'dRdubl' and 
C                   if difference in velocitiy is less than a fraction
C                   'fVdubl' of the velocity dispersion -->
C                   dublicates: keep only one (largest) halo
C
C  Condition of proximity in velocity depends on how close are halos
C  in space. Halos with velocity difference dv =sqrt(vx_1-vx_2)^2 +(y)+ (z)),
C  at distance d one from the other, and with internal velocity
C  dispersions Vrms_1 and Vrms_2 are considered too close ('dublicates)
C  if     dv < fVdubl*max(Vrms_1,Vrms_2)*sqrt(6-5(d/dRdubl)^2)

      SUBROUTINE RemVelDubl(Box,Ncentr,dRdubl,fVdubl,Xscale,Vscale)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      Logical    Lab(Nc)
      Real      Mhalo,Mhalo2

      dR2 = dRdubl**2
      fV2 = fVdubl**2
      nrem= 0
      Do i=1,Ncentr
         Lab(i) =.true.
      EndDo
      write (*,50)
 50   format(5X,'<========  Remove fake halos ========>',
     &     /17x,'difference in velocities dV is small as compared with',
     &     /17x,'internal rms velocity Vrms',
     &          ' and halos are close in space',/
     &     4x,'dR(Mpc)',' dV(km/s)',' Vrms(km/s)',
     &        ' N_part_halo_1',' N_part_halo_2',
     &        ' dVx(km/s) dVy(km/s) dVz(km/s)') 
      Do i=1,Ncentr-1
        Mhalo  =Amc(i)
        irad   =iRadius(i)
        Vrh2   =(Vrmc(i,irad)/Vscale)**2
        If(Mhalo.eq.0. .or. irad.eq.0)Lab(i) =.false.
       If(Lab(i))Then
       Do j=i+1,Ncentr
          dd =(Xm(j)-Xm(i))**2 +
     .        (Ym(j)-Ym(i))**2 +      
     .        (Zm(j)-Zm(i))**2     
          Mhalo2 =Amc(j)
          If(dd.lt.dR2.and.Mhalo2.gt.Mhalo/2.)Then    ! close distance pair
          If(Mhalo2.lt.Mhalo*2.)Then                         ! close mass range
             jrad=iRadius(j)
             dv =(Wxc(j,jrad)-Wxc(i,irad))**2 +
     .           (Wyc(j,jrad)-Wyc(i,irad))**2 +      
     .           (Wzc(j,jrad)-Wzc(i,irad))**2
             VV =MAX(Vrh2,(Vrmc(j,jrad)/Vscale)**2) ! max Vrms of the two
             If(dv.lt.(6.-5.*dd/dR2)*fV2*VV)Then   ! close in velocity
                nrem = nrem +1
 30             format(f10.4,f9.3,f11.2,2i14,3f10.3)                
                If(Mhalo.gt.Mhalo2)Then ! chose the biggest
                   Lab(j)=.false.               
                   write (*,30) sqrt(dd)*Xscale,sqrt(dv)*Vscale,
     +                            sqrt(VV)*Vscale,
     .                 Nbc(i,irad),Nbc(j,jrad),
     .               (Wxc(j,jrad)-Wxc(i,irad))*Vscale,
     +              (Wyc(j,jrad)-Wyc(i,irad))*Vscale,
     +              (Wzc(j,jrad)-Wzc(i,irad))*Vscale 
c        +              Xm(j)*Xscale,Ym(j)*Xscale,Zm(j)*Xscale
                Else
                   Lab(i)=.false.
                   write (*,30) sqrt(dd)*Xscale,sqrt(dv)*Vscale,
     +                            sqrt(VV)*Vscale,
     .                 Nbc(i,irad),Nbc(j,jrad),
     .               (Wxc(j,jrad)-Wxc(i,irad))*Vscale,
     +              (Wyc(j,jrad)-Wyc(i,irad))*Vscale,
     +              (Wzc(j,jrad)-Wzc(i,irad))*Vscale 
c        +              Xm(i)*Xscale,Ym(i)*Xscale,Zm(i)*Xscale
                EndIf
             EndIf
          EndIf
          EndIf
        EndDo
      EndIf
      EndDo
      m  =0
      meanmass =0
      Do i=1,Ncentr
         If(Lab(i))Then
            m = m +1
            Xm(m)      =Xm(i)
            Ym(m)      =Ym(i)
            Zm(m)      =Zm(i)
            Amc(m)     =Amc(i)   
            Rmc(m)     =Rmc(i)  
            Vrm(m)     =Vrm(i) 
            iRadius(m) =iRadius(i)
            Do ir=1,Nrad  !  Store Results 
               Wxc(m,ir)  =Wxc(i,ir) 
               Wyc(m,ir)  =Wyc(i,ir) 
               Wzc(m,ir)  =Wzc(i,ir) 
               Nbc(m,ir)  =Nbc(i,ir) 
               Rrc(m,ir)  =Rrc(i,ir)
               Vrmc(m,ir) =Vrmc(i,ir)
            EndDo
            meanmass =meanmass +Nbc(m,Nrad)
         EndIf
      EndDo
      xmeanmass = FLOAT(meanmass)/FLOAT(m)      
      write(*,10) m,Ncentr,xmeanmass
      write(20,10) m,Ncentr,xmeanmass
      write(*,20) dRdubl*Xscale*hubble,fVdubl
 10   format(5x,'Vel.Dublicates: New number of objects=',i6,' Old=',i6,
     .       ' mean N_particles=',g11.3)
 20   format('     Distance Difference=',f8.4,/
     .       '     Vrelative/Vrms <    ',f8.4)
      Ncentr =m
      Return
      End

C-------------------------------- Update Statistics of pairs 
C                   
      SUBROUTINE SPairs(Box,N,Ncentr)
C---------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'

         Do i=1,Ncentr
               Do ir=1,Nrad
                  Wxc(i,ir) = 0.
                  Wyc(i,ir) = 0.
                  Wzc(i,ir) = 0.
                  Nbc(i,ir) = 0
                  Rrc(i,ir) = 0.
                  Vrmc(i,ir)= 0.
               EndDo
         EndDo
      Do i=1,Ncentr
         Xc =Xm(i)
         Yc =Ym(i)
         Zc =Zm(i)
         Call SNeib
            Do ir=1,Nrad  !  Store Results 
               Wxc(i,ir) =Vx1(ir)
               Wyc(i,ir) =Vy1(ir)
               Wzc(i,ir) =Vz1(ir)
               Nbc(i,ir) =Nob1(ir)
               Rrc(i,ir) =Rob1(ir)
               Vrmc(i,ir)=Vxx(ir)
            EndDo
      EndDo
      Return
      End

C-------------------------------- Remove small halos
C           
C           
      SUBROUTINE SmallRem(ANlimit,Ncentr)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'

      New =0
      Do i=1,Ncentr
         If(Amc(i).GT.ANlimit)Then
          New =New +1
          Xm(New) =Xm(i)
          Ym(New) =Ym(i)
          Zm(New) =Zm(i)
          Amc(New)=Amc(i)
          Rmc(New)=Rmc(i)
         EndIf
      EndDo
      Ncentr =New
      Return
      End
C-------------------------------- Get Catalog of Halos
C                        Remove close centers, keep only the largest
C                        Emass - small difference in mass
      SUBROUTINE CATALOG(Box,Ncentr)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      Dimension    Lab(Nc)

      New =0
      Do i=1,Ncentr
        Lab(i) =1
      EndDo
      Do i=1,Ncentr
         If(Rmc(i).lt.1.e-5)Lab(i)=0
            Xc =Xm(i)
            Yc =Ym(i)
            Zc =Zm(i)
            a0 =Amc(i)
            Rh =Rmc(i)
            Do j=1,Ncentr
               If(i.ne.j)Then
                dd =((Xc-Xm(j))**2+(Yc-Ym(j))**2+(Zc-Zm(j))**2)
                IF(dd.lt.( (Rh+Rmc(j))/2. )**2 )Then
                      If(a0.GT.Amc(j))Then! take largest of maxima
                         Lab(j)=0
                      Else
                         If(a0.lt.Amc(j))Then
                            Lab(i)=0
                         Else   ! equal masses -> take max(i,j)
                            Lab(min(i,j)) =0
                         EndIf
                      EndIf
                 EndIf
              EndIf
            EndDo            
      EndDo
C                                 Remove maxima with Label =0
      Do i=1,Ncentr
        If(Lab(i).eq.1)Then
          New =New +1
          Xm(New) =Xm(i)
          Ym(New) =Ym(i)
          Zm(New) =Zm(i)
          Amc(New)=Amc(i)
          Rmc(New)=Rmc(i)
          Vrm(New)=Vrm(i)
          Wxc(New,1)=Wxc(i,1)
          Wyc(New,1)=Wyc(i,1)
          Wzc(New,1)=Wzc(i,1)
        EndIf
      EndDo
      Ncentr =New
      Return
      End
C---------------------------------------------------------
C                                                  Select particles
c                                                   as initial seeds of centers
      SUBROUTINE ReadCent(N,Ncentr)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      Real INPUT

      Ncentr =0
      Nline=INPUT('  Number of particles for initial seeds= ')
      jneib=INPUT('  Number of neibhours for a seed= ')
      If(Nline.eq.0)Return
         fr    = N/FLOAT(Nline)
         Nstep =MAX(INT(fr+1.),1)
         write (*,*) ' Every ',Nstep,' particle is used',
     &                ' as initial centre'
         Do i=1,N,Nstep
               Ncentr=Ncentr+1
               Amc(Ncentr) =1.
               Rmc(Ncentr) =1.
               Xm(Ncentr)  =Xp(i)
               Ym(Ncentr)  =Yp(i)
               Zm(Ncentr)  =Zp(i)
               j=Ncentr
         EndDo

      If(jneib.eq.0)goto 150               ! No more points

      Imax =MIN(INT(NGRID/Cell),Nb)
      write (*,*) ' Size of Grid centers=',Imax,Cell,NGRID
      Do k=0,Imax
      Do j=0,Imax
      Do i=0,Imax
         jp =Label(i,j,k)
        Do llcell =1,3   ! Choose particles from the cell
         Do jcount=1,jneib
            If(jp.ne.0)jp=Lst(jp) ! find if there are enough neib
         EndDo
         If(jp.ne.0)Then
            Ncentr=Ncentr+1
            If(Ncentr.le.Nc)Then
               Xm(Ncentr)  =Xp(jp)
               Ym(Ncentr)  =Yp(jp)
               Zm(Ncentr)  =Zp(jp)
            EndIf
         EndIf
        EndDo
      EndDo
      EndDo
      EndDo
150   write(*,*) ' Ncentr=',Ncentr,' Max=',Nc
      Return
      End
C---------------------------------------------------------
C                                        Read list of maxima
c                                     Xnew =X-1: coord. now are 0-NGRID       
      SUBROUTINE ReadMax(N,Xscale)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      Character*79 Line
      Real        INPUT

      Nline=INPUT(' N lines to skip in the List of Maxima= ')
      If(Nline.ne.0)Then
      Do i=1,Nline
         read(3,'(A)') Line
         If(i.le.7)write(*,*) Line
         If(i.le.7)write(2,*) Line
         If(i.le.7)write(20,*) Line
      EndDo
      EndIf
      N =0
      xxm =1000.
      xxn =-1000.
      Do i=1,Np 
         write (*,*) ' i=',i
         read (3,*,err=5,end=5) x0, y0, z0
            N=N+1
            Amc(N) =0.
            Rmc(N) =1.0
            Xm(N)  =x0
            Ym(N)  =y0
            Zm(N)  =z0
            xxm =min(xxm,Xm(N))
            xxm =min(xxm,Ym(N))
            xxm =min(xxm,Zm(N))
            xxn =max(xxn,Xm(N))
            xxn =max(xxn,Ym(N))
            xxn =max(xxn,Zm(N))
      EndDo
 5    write(*,*) ' Read Centers: X{Min,Max}=',xxm,xxn,' N=',N
     Return
      End
C---------------------------------------------------------
C                                        Read particles
      SUBROUTINE ReadPnt(N,Fract)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'

      N =0
      Iz=1
      Iy=1
      Ff=1.
      If(Fract.le.0.666)Then
         Iz=2
         Ff=0.5
         If(Fract.le.0.333)Then
            Iy=2
            Ff=0.25
         Endif
      EndIf
      Fract =Ff
C				       Loop over particles
      DO 10 IROW=1,NROW,Iz
         READ  (21,REC=IROW) RECDAT
	   DO   IN=1,NPAGE,Iy
            N   = N +1
            Xp(N)  =XPAR(IN)-1.
            Yp(N)  =YPAR(IN)-1.
            Zp(N)  =ZPAR(IN)-1.
            VXp(N)=VX(IN)
            VYp(N)=VY(IN)
            VZp(N)=VZ(IN)
         ENDDO
 10   CONTINUE
      Return
      End

C---------------------------------------------------
C                                Remove Unbound particles, find radius and mass
C                                No removal of unbound particles If:
C                                       Toohot =< 0 or Toohot =>5
C                             Vscale =100.*hsmall*Xscale/AEXPN 
C                              Xscale=Box/Ngrid 
C                              Dscale=2.75e+11*hsmall**2*(Box/NROW)**3 
      SUBROUTINE CLEAN(Ncentr,Xscale,Vscale,Dscale,
     +     Fract,Radsc,MnMass,Toohot,Rminhalo)  
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      dimension Mass(Nrad),MnMass(Nrad),Overdens(Nrad),Radsc(Nrad)
      Real          Mass,MnMass,Mhalo
      Do i=1,Ncentr          
         Xc =Xm(i)  *Xscale !          Scale all to physical units: comoving
         Yc =Ym(i)  *Xscale
         Zc =Zm(i)  *Xscale
         Do ir =1,Nrad
            Vrmc(i,ir)=Vrmc(i,ir) *Vscale   !  Scale to km/s
            Summass     = Om0*Nbc(i,ir)/Fract
            Mass(ir)    = Dscale*Summass
            Overdens(ir)= Mass(ir)/MnMass(ir)
         EndDo  
C                                     find size, mass, rad ... for a halo
         Call Decide(Mass,Radsc,Overdens,Nrad,irad,Mhalo,Rhalo,
     +                                       Vmaxrot,Rmaxrot )
         Vcirc=sqrt(Vmaxrot/AEXPN)*6.58e-5 ! =10^-5*sqrt(G 2e33/3.08e24) 
         If(Toohot.gt.0. .and. Toohot.lt.5.)Then
         Do iter =0,4                 !  Remove High Energy particles from halos
         If(irad .ge.1 .and. Rhalo.gt.Rminhalo)Then
           Vcc =Vcirc/Vscale          ! back to dimensionless units
           Vmaxrot=Vcc*Toohot*1.3**(4-iter) ! Increase the Rotatioanl Vel.
           Call RemEng(i,Vmaxrot,Rmaxrot)  ! Remove Unbound Particles
           Do ir =1,Nrad
             Vrmc(i,ir)=Vrmc(i,ir) *Vscale
             Summass     =Om0*Nbc(i,ir)/Fract
             Mass(ir)    = Dscale*Summass
             Overdens(ir)=Mass(ir)/MnMass(ir)
           EndDo  
          Call Decide(Mass,Radsc,Overdens,Nrad,irad,Mhalo,Rhalo,
     +                                       Vmaxrot,Rmaxrot)
          Vcirc=sqrt(Vmaxrot/AEXPN)*6.58e-5 !=10^-5*sqrt(2e33/3.08e24)
         EndIf
         EndDo       ! end iter energy
         EndIf
         Amc(i)     =Mhalo ! store results
         Rmc(i)     =Rhalo
         Vrm(i)     =Vcirc
         iRadius(i) =irad
c         write (*,*) '         ',Mhalo,Rhalo,Vcirc,irad
       Enddo   ! end i (Ncentr)
       Return
       End
C------------------------------------------------------------- 
C                                                             Find  halo radius and mass
      SUBROUTINE Decide(Mass,Rad,Overdens,Nrad,irad,Mhalo,Rhalo,
     +                                       Vmaxrot,Rmaxrot )
C--------------------------------------------------------------
      COMMON /OVER/ Ovlim,Ovcnt
      Real   Rad(Nrad),Overdens(Nrad),Mass(Nrad),Mhalo
      DATA  slope/0.999/
         Vmaxrot =0
         Rmaxrot =0. 
         Mhalo=0.
         Rhalo=0.   
      If(MAX(Overdens(1),Overdens(2)).LT.Ovcnt)Then
         irad =0         ! max overdensity is less then Ovcnt
         RETURN
      EndIf
      iov  =0
      Do ir=Nrad,1,-1
         If(Overdens(ir).gt.Ovlim .and. iov.eq.0)Then
            iov  =ir
         EndIf
      EndDo
      If(iov.eq.0)Then
         irad =0         ! max overdensity is less then Ovlim
         RETURN
      EndIf
      iovv =0
      Do ir=1,iov
         irleft =max(ir-1,1)
         irr1   =min(ir+1,Nrad)
         irr2   =min(ir+2,Nrad)
         Overright =(Overdens(irr1)+Overdens(irr2))/2.
         Overleft  =(Overdens(ir)+Overdens(irleft))/2.
         If(Overright.gt.0.97*Overleft)Then !stop:rising profile
            If(ir.eq.1)Then ! softer conditions for the first bin
               If(Overright.gt.1.2*Overleft)Then
                   iovv =ir
                   goto 20
               EndIf
            else 
                iovv =ir
               goto 20
            EndIf
         EndIf
      EndDo
 20   If(iovv.ne.0)iov =iovv
         irad =iov
 30      irleft  =max(irad-2,1)  ! check if overdens is steep
         irright =min(irad+1,Nrad)
         gradlf  =Overdens(irleft) * (Rad(irleft))**slope
         gradrt  =Overdens(irright)* (Rad(irright))**slope
         If(gradlf.lt.gradrt)Then  ! not a steep gradient
            irad =irad -1        ! reduce radius
            iovv =irad
            If(irad.eq.0)Then   ! zero radius -> quit
               RETURN
            Else
               goto 30             ! iterate
            EndIf
         EndIf
         If(irad.lt.Nrad)Then    ! interpolate to Overlim
            If(Ovlim.gt.Overdens(irad+1) .and.
     .         Overdens(irad).gt.Ovlim)Then       
              Mhalo=(Mass(irad)*(Ovlim-Overdens(irad+1))+
     .             Mass(irad+1)*(Overdens(irad)-Ovlim)) /
     .             (Overdens(irad)-Overdens(irad+1))
              Rhalo=(Rad(irad)*(Ovlim-Overdens(irad+1))+
     .             Rad(irad+1)*(Overdens(irad)-Ovlim)) /
     .             (Overdens(irad)-Overdens(irad+1))
            Else                 ! Overdens is > Overlim 
              Mhalo =Mass(irad)
              Rhalo =Rad(irad)
            EndIf
         Else                 ! Overdens is > Overlim 
            Mhalo =Mass(irad)
            Rhalo =Rad(irad)
         EndIf
         Do ir =1,irad  ! find max rotational velocity
            V2    = Mass(ir)/Rad(ir)
            If (V2.gt.Vmaxrot)Then
               Vmaxrot =V2               
               Rmaxrot =Rad(ir)
            EndIf
          EndDo
      Return
      End
C------------------------------------------------
C                                  Remove unbound particles for i-th halo
C                                  irad   = radius of the halo
C                                 Vmaxrot = max rotational Velocity
C                                 Rmaxrot = radius at max rot.velocity
      SUBROUTINE RemEng(i,Vmaxrot,Rmaxrot)
C-----------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      DIMENSION VXxc(Nrad), VYyc(Nrad), VZzc(Nrad)
         Xc  =Xm(i)  ! store center of mass and velocity      
         Yc  =Ym(i)     
         Zc  =Zm(i)     
         Do ir =1,Nrad ! store mean velocity
            VXxc(ir) =Wxc(i,ir) 
            VYyc(ir) =Wyc(i,ir) 
            VZzc(ir) =Wzc(i,ir) 
                                         ! escape velocity for NFW profile
            Vescape2(ir) = (2.15*Vmaxrot)**2 * 
     &           log(1.0+2.*Rad(ir)/Rmaxrot)/(Rad(ir)/Rmaxrot) 
               Wxc(i,ir) = 0.
               Wyc(i,ir) = 0.
               Wzc(i,ir) = 0.
               Nbc(i,ir) = 0
               Rrc(i,ir) = 0.
               Vrmc(i,ir)= 0.
            EndDo
            Call SrejNeib(VXxc,VYyc,VZzc)
            Do ir=1,Nrad  !  Store Results 
               Wxc(i,ir) =Vx1(ir)
               Wyc(i,ir) =Vy1(ir)
               Wzc(i,ir) =Vz1(ir)
               Nbc(i,ir) =Nob1(ir)
               Rrc(i,ir) =Rob1(ir)
               Vrmc(i,ir)=Vxx(ir)
            EndDo
      Return
      End
C--------------------------------------------------------------
C                                Accumilate statistics only
C                                if relative velocity is not large
C                                Xc,Yc,Zc - center; 
C                               
      SUBROUTINE SrejNeib(VXxc,VYyc,VZzc)
C--------------------------------------------------------------
      INCLUDE 'PMparameters.h'      
      INCLUDE 'PMlists.h'
      DIMENSION VXxc(Nrad), VYyc(Nrad), VZzc(Nrad)
         Radmax =Rad(Nrad)
         Do i=1,Nrad
              Nob1(i) =0
              Rob1(i) =0.
              Vx1(i)  =0.
              Vy1(i)  =0.
              Vz1(i)  =0.
              Vxx(i)  =0.
         EndDo
c                                           limits for Label
         i1=INT((Xc-Radmax)/Cell)
         j1=INT((Yc-Radmax)/Cell)
         k1=INT((Zc-Radmax)/Cell)
            i1=MIN(MAX(Nm,i1),Nb)
            j1=MIN(MAX(Nm,j1),Nb)
            k1=MIN(MAX(Nm,k1),Nb)
         i2=INT((Xc+Radmax)/Cell)
         j2=INT((Yc+Radmax)/Cell)
         k2=INT((Zc+Radmax)/Cell)
            i2=MIN(MAX(Nm,i2),Nb)
            j2=MIN(MAX(Nm,j2),Nb)
            k2=MIN(MAX(Nm,k2),Nb)
C                                        Look for neibhours
         Do k=k1,k2
         Do j=j1,j2
         Do i=i1,i2
           jp =Label(i,j,k)       !  Dark matter
 10        If(jp.ne.0)Then
             dd =(Xc-Xp(jp))**2+(Yc-Yp(jp))**2+(Zc-Zp(jp))**2
             If(dd.lt.Rad2(Nrad))Then
             iov =0
             Do ir =1,Nrad          ! find bin
                If(dd.le.Rad2(ir))Then
                   iov =ir
                   goto 30
                EndIf
             EndDo
 30          If(iov.eq. 0)Then   ! error
                write (*,*) ' Error SrejNeib: iov =',iov,' d=',sqrt(dd)
                Stop
             EndIf
             dv =  (VXxc(iov)-VXp(jp))**2+
     +             (VYyc(iov)-VYp(jp))**2+
     +             (VZzc(iov)-VZp(jp))**2
             If(dv .lt. Vescape2(iov))Then
              Do ir =iov,Nrad   ! Nrad
c                 If(dd.lt.Rad2(ir))Then
                   Nob1(ir)= Nob1(ir)+ 1
                   Rob1(ir)= Rob1(ir)+ sqrt(dd)
                   Vxx(ir) = Vxx(ir) +VXp(jp)**2+VYp(jp)**2+VZp(jp)**2
                   Vx1(ir) = Vx1(ir) +VXp(jp)  
                   Vy1(ir) = Vy1(ir) +VYp(jp)
                   Vz1(ir) = Vz1(ir) +VZp(jp)
c                 EndIf
              EndDo
             EndIf
             EndIf
           jp =Lst(jp)
           GoTo10
           EndIf
         EndDo
         EndDo
         EndDo
         Do ir=1,Nrad
            Vmn =0.
     	      If(Nob1(ir).GT.0)Then
               Nobj =Nob1(ir)
               Vx1(ir) = Vx1(ir)/Nobj
               Vy1(ir) = Vy1(ir)/Nobj
               Vz1(ir) = Vz1(ir)/Nobj
               Vmn     = Vx1(ir)**2+Vy1(ir)**2+Vz1(ir)**2
               Vxx(ir) = SQRT(ABS(Vxx(ir)/Nobj -Vmn))
	         EndIf
         EndDo
      Return
      End
