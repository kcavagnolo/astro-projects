      program qcomp
c     program to compare q values with different smoothing models
      
      parameter(N=7)
      real sm(N), q1min(N), q1max(N), q2min(N), q2max(N)
      real q3min(N), q3max(N)
      real x1(2),y1(2),x2(2),y2(2),x3(4),y3(4),x4(2),y4(2)
      integer i

      open(unit=4,status='old',file='qcompdata.txt')

      do i=1,N
      read(4,*) sm(i),q1min(i),q1max(i),q2min(i),q2max(i),
     & q3min(i),q3max(i)
      enddo

c     plotting the results
c      q1:


c      call pgbegin(0,'/xw',1,1)
      call pgbegin(0,'q1.ps/cps',1,1)
      call pgscf(3)
      call pgslw(3)
      call pgenv(0.,100.5,0.5,15.1,0,0)
      call pglabel('kernel size','q-values','q1')
      do i=1,N
         if (i.eq.1) then
            call pgsci(2)
            x3(1)=sm(i)
            x3(2)=105.
            x3(3)=105.
            x3(4)=sm(i)
            y3(1)=q1min(i)
            y3(2)=q1min(i)
            y3(3)=q1max(i)
            y3(4)=q1max(i)
            call pgsls(4)
            call pgsfs(3)
            call pgshs(50.,8.,0.0)
            call pgpoly(4,x3,y3)
            call pgsls(1)
            x4(1)=sm(i)
            x4(2)=sm(N)
            y4(1)=q1max(i)
            y4(2)=q1max(i)
            call pgline(2,x4,y4)
            x4(1)=sm(i)
            x4(2)=sm(N)
            y4(1)=q1min(i)
            y4(2)=q1min(i)
            call pgline(2,x4,y4)
         else
         x1(1)=sm(i)
         y1(1)=q1min(i)
         x2(1)=sm(i)
         y2(1)=q1max(i)
         call pgsci(4)
         call pgpt1(sm(i),q1min(i),5)
         call pgsci(3)
         call pgpt1(sm(i),q1max(i),6)
           if (i.ge.3) then
           call pgsci(4)
           call pgline(2,x1,y1)
           call pgsci(3)
           call pgline(2,x2,y2)
           endif
         x1(2)=x1(1)
         y1(2)=y1(1)
         x2(2)=x2(1)
         y2(2)=y2(1)
         endif
      enddo
      call pgend

c     q2:
c      call pgbegin(0,'/xw',1,1)
      call pgbegin(0,'q2.ps/cps',1,1)
      call pgscf(3)
      call pgslw(3)
      call pgenv(0.,100.5,0.0,8.,0,0)
      call pglabel('kernel size','q-values','q2')
      do i=1,N
         if (i.eq.1) then
            call pgsci(2)
            x3(1)=sm(i)
            x3(2)=105.
            x3(3)=105.
            x3(4)=sm(i)
            y3(1)=q2min(i)
            y3(2)=q2min(i)
            y3(3)=q2max(i)
            y3(4)=q2max(i)
            call pgsls(4)
            call pgsfs(3)
            call pgshs(50.,8.,0.0)
            call pgpoly(4,x3,y3)
            call pgsls(1)
            x4(1)=sm(i)
            x4(2)=sm(N)
            y4(1)=q2max(i)
            y4(2)=q2max(i)
            call pgline(2,x4,y4)
            x4(1)=sm(i)
            x4(2)=sm(N)
            y4(1)=q2min(i)
            y4(2)=q2min(i)
            call pgline(2,x4,y4)
         else
         x1(1)=sm(i)
         y1(1)=q2min(i)
         x2(1)=sm(i)
         y2(1)=q2max(i)
         call pgsci(4)
         call pgpt1(sm(i),q2min(i),5)
         call pgsci(3)
         call pgpt1(sm(i),q2max(i),6)
           if (i.ge.3) then
           call pgsci(4)
           call pgline(2,x1,y1)
           call pgsci(3)
           call pgline(2,x2,y2)
           endif
         x1(2)=x1(1)
         y1(2)=y1(1)
         x2(2)=x2(1)
         y2(2)=y2(1)
         endif
      enddo
      call pgend

c      call pgbegin(0,'/xw',1,1)
      call pgbegin(0,'q3.ps/cps',1,1)
      call pgscf(3)
      call pgslw(3)
      call pgenv(0.,100.5,0.0,15.,0,0)
      call pglabel('kernel size','q-values','q3')
      do i=1,N
         if (i.eq.1) then
            call pgsci(2)
            x3(1)=sm(i)
            x3(2)=105.
            x3(3)=105.
            x3(4)=sm(i)
            y3(1)=q3min(i)
            y3(2)=q3min(i)
            y3(3)=q3max(i)
            y3(4)=q3max(i)
            call pgsls(4)
            call pgsfs(3)
            call pgshs(50.,8.,0.0)
            call pgpoly(4,x3,y3)
            call pgsls(1)
            x4(1)=sm(i)
            x4(2)=sm(N)
            y4(1)=q3max(i)
            y4(2)=q3max(i)
            call pgline(2,x4,y4)
            x4(1)=sm(i)
            x4(2)=sm(N)
            y4(1)=q3min(i)
            y4(2)=q3min(i)
            call pgline(2,x4,y4)
         else
         x1(1)=sm(i)
         y1(1)=q3min(i)
         x2(1)=sm(i)
         y2(1)=q3max(i)
         call pgsci(4)
         call pgpt1(sm(i),q3min(i),5)
         call pgsci(3)
         call pgpt1(sm(i),q3max(i),6)
           if (i.ge.3) then
           call pgsci(4)
           call pgline(2,x1,y1)
           call pgsci(3)
           call pgline(2,x2,y2)
           endif
         x1(2)=x1(1)
         y1(2)=y1(1)
         x2(2)=x2(1)
         y2(2)=y2(1)
         endif
      enddo
      call pgend








      end
