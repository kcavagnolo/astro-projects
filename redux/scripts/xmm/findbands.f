      program findbands
c     a program to determine the energy bands of a given dataset. 
c     The data will be in the form of a headed datatable, namely
c     something like filtersddmmyy.txt. The first part of the code
c     will determine the channels that each energy range corresponds
c     to. The second part of the code will be to read in an rmf file
c     to determine energies. 


c     The datafile contains data from the optimisation routine. This
c     consists of channels 21-240 with data from each filter present. 
c     Must find a way of getting rid of the header information. 

      parameter(N1=21,N2=240,N3=1024)
      integer i, flag1, flag2, flag3, flag4
      integer flag1b, flag2b, flag3b, flag4b
      real f0(N1:N2), f1(N1:N2), f2(N1:N2), f3(N1:N2), f4(N1:N2)
      real f1chst,f1chen,f2chst,f2chen,f3chst,f3chen,f4chst,f4chen
      real f1enst,f1enen,f2enst,f2enen,f3enst,f3enen,f4enst,f4enen
      real ch1(N3),ch2(N3),emin(N3),emax(N3)
      character(7) d

      open(unit=5, file="filters040505.txt", status="old")
      do i=N1-1,N2
       if (i.eq.N1-1) then
        read(5,'(a30,i1)') d
       else
        read(5,*) f0(i),f1(i),f2(i),f3(i),f4(i)
       endif
      enddo
      close(unit=5, status="keep")

      flag1=0
      flag2=0
      flag3=0
      flag4=0
      flag1b=0
      flag2b=0
      flag3b=0
      flag4b=0
      do i=N1,N2
        if ((f1(i).ne.0).and.(i.eq.N1+1)) then
           flag1=1
           f1chst=i-1
        endif
        if (flag1.eq.0) then
          if (f1(i).eq.1.) then
            flag1=1
            f1chst=i
          endif
        endif
        if ((flag1b.eq.0).and.(flag1.eq.1)) then
          if (f1(i).eq.0.) then
             flag1b=1
             f1chen=i-1
          else if (i.eq.N2) then
             f1chen=N2
          endif
        endif
        if ((f2(i).ne.0).and.(i.eq.N1+1)) then
           flag2=1
           f2chst=i-1
        endif
        if (flag2.eq.0) then
          if (f2(i).eq.1.) then
            flag2=1
            f2chst=i
          endif
        endif
        if ((flag2b.eq.0).and.(flag2.eq.1)) then
          if (f2(i).eq.0.) then
             flag2b=1
             f2chen=i-1
          else if (i.eq.N2) then
             f2chen=N2
          endif
        endif
        if ((f3(i).ne.0).and.(i.eq.N1+1)) then
           flag3=1
           f2chst=i-1
        endif
        if (flag3.eq.0) then
          if (f3(i).eq.1.) then
            flag3=1
            f3chst=i
          endif
        endif
        if ((flag3b.eq.0).and.(flag3.eq.1)) then
          if (f3(i).eq.0.) then
             flag3b=1
             f3chen=i-1
          else if (i.eq.N2) then
             f3chen=N2
          endif
        endif
        if ((f4(i).ne.0).and.(i.eq.N1+1)) then
           flag4=1
           f3chst=i-1
        endif
        if (flag4.eq.0) then
          if (f4(i).eq.1.) then
            flag4=1
            f4chst=i
          endif
        endif
        if ((flag4b.eq.0).and.(flag4.eq.1)) then
          if (f4(i).eq.0.) then
             flag4b=1
             f4chen=i-1
          else if (i.eq.N2) then
             f4chen=N2
          endif
        endif
      enddo

      write(*,*) 'Channel ranges are:'
      write(*,*) 'C1: ', f1chst,f1chen
      write(*,*) 'C2: ', f2chst,f2chen      
      write(*,*) 'C3: ', f3chst,f3chen
      write(*,*) 'C4: ', f4chst,f4chen
 
c     Stage 1 complete! Now to read in the rmf data. 

      open(unit=5, file="rmf.txt", status="old")
      read(*,*)
      read(*,*)
      read(*,*)
      read(*,*)
      do i=1,N3
         read(5,*) ch1(i),ch2(i), emin(i), emax(i) 
      enddo
      close(unit=5,status="keep")

      do i=1,N3
         if (ch1(i).eq.f1chst) then
            f1enst=emin(i)*1000
         endif
         if (ch1(i).eq.f1chen) then
            f1enen=emax(i)*1000
         endif
         if (ch1(i).eq.f2chst) then
            f2enst=emin(i)*1000
         endif
         if (ch1(i).eq.f2chen) then
            f2enen=emax(i)*1000
         endif         
         if (ch1(i).eq.f3chst) then
            f3enst=emin(i)*1000
         endif
         if (ch1(i).eq.f3chen) then
            f3enen=emax(i)*1000
         endif
         if (ch1(i).eq.f4chst) then
            f4enst=emin(i)*1000
         endif
         if (ch1(i).eq.f4chen) then
            f4enen=emax(i)*1000
         endif

      enddo

      write(*,*) 'Energy ranges are (ev): '
      write(*,*) 'E1: ', f1enst,f1enen
      write(*,*) 'E2: ', f2enst,f2enen
      write(*,*) 'E3: ', f3enst,f3enen
      write(*,*) 'E4: ', f4enst,f4enen

10    FORMAT(1X,F6.1,1X,F6.1)

      open (unit=5, file="ebandranges.txt", status="new")
         write(5,10) f1enst, f1enen
         write(5,10) f2enst, f2enen
         write(5,10) f3enst, f3enen
         write(5,10) f4enst, f4enen
      close (unit=5, status="keep")
     
      end
