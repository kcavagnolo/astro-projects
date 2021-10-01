C       Nc - number of centers, Np - number of dm particles
C       Nrad - number of shells for halos
C
      PARAMETER (Nm  =-3, Nb=NGRID/2+4)  ! define size of linker mesh
      PARAMETER (Nc  =200000)            ! Max number of initial centers
      PARAMETER (Ncc  =Nc/4)    
      PARAMETER (Np  =INT(1.35*NROW**3)) ! Max Nparticles, make extra room 
      PARAMETER (Nrad=16)                ! Number of shells around a halo
      COMMON /CAT/ Xp(Np),Yp(Np),Zp(Np),VXp(Np),VYp(Np),VZp(Np)
      COMMON /MXM/ Xm(Nc),Ym(Nc),Zm(Nc),Amc(Nc),Rmc(Nc),Vrm(Nc),
     +             iRadius(Nc) 
      COMMON /GRD/ Label(Nm:Nb,Nm:Nb,Nm:Nb),Lst(Np)
      COMMON /MXC/ Wxc(Ncc,Nrad),Wyc(Ncc,Nrad),Wzc(Ncc,Nrad),
     .             Vrmc(Ncc,Nrad),Nbc(Ncc,Nrad),Rrc(Ncc,Nrad)
      COMMON /PNT/ Xc,Yc,Zc,a0,Radius,Rad(Nrad),Rad2(Nrad),Cell
      COMMON /STS/ Vx1(Nrad),Vy1(Nrad),Vz1(Nrad),Vxx(Nrad),Nob1(Nrad),
     .             Rob1(Nrad)
   	COMMON / ESCP/ Vescape2(Nrad)

