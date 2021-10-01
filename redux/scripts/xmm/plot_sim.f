      program plot_sim
c     plotting some simulated spectra from the e-science work. 
c     going to float z for params 1 5 10 and keep everything else 2-2

      parameter(N=1024)

      real A(N), B(N), C(N), ch1(N), ch2(N)
      integer i

      write(*,*) 'here'
      open(unit=4, file='meksim1_1-1-1.txt', status='old')
      do i=1,N
         read(4,*) ch1(i), ch2(i), A(i)
      enddo   
      close(unit=4,status='keep')
      open(unit=4, file='meksim1_1-5-1.txt', status='old')
      do i=1,N
         read(4,*) ch1(i), ch2(i), B(i)
      enddo  
      open(unit=4, file='meksim1_1-10-1.txt', status='old')
      do i=1,N
         read(4,*) ch1(i), ch2(i), C(i)
      enddo  

c      call pgbegin(0,'test.ps/PS',1,1)
        call pgbegin(0,'?',1,1)
        call pgenv(21.,240.,0.,0.7,0,0)
        call pglabel('channel','rate','spectra varying Abund')
        do i=1,N
           call pgsci(2)
          call pgpt1(ch1(i),A(i),5)
           call pgsci(3)
          call pgpt1(ch1(i),B(i),6)
           call pgsci(4)
          call pgpt1(ch1(i),C(i),7)
        enddo

        call pgend      





      end



