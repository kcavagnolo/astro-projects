      program conv

c     A program to convert data between image and physical coordinates. 
c     The input file will be a set of image coordinates.

      parameter(N=132)
      real x(N),y(N)
      integer i, P(N)
      real PX1,PX2,grad,int,t1,t2,t3,t4

      IX1=1.0
      IX2=1024.
      PX1=20.5
      PX2=40940.5
      grad=(IX2-IX1)/(PX2-PX1)
      int=IX1-grad*PX1

      open(unit=4, file='temp.txt', status='old')
      do i=1,7
      read(4,*)
      enddo
      do i=1,N
      read(4,*) P(i),x(i),y(i)
      x(i)=(x(i)-int)/grad
      y(i)=(y(i)-int)/grad
c      write(*,*) P(i),x(i),y(i)
      enddo

      t1=x(1)
      t2=y(1)
      t3=(t1-int)/grad
      t4=(t2-int)/grad
c      write(*,*) t1,t2,t3,t4

      open(unit=4, file='sources.reg', status='new')
      write(4,*)'# Region file format: CIAO version 1.0'
      do i=1,N
      write(4,*)'circle(',x(i),',',y(i),',400.)'
c     radius set at 20'' 

      enddo
      





      end
