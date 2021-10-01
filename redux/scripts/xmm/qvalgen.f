      program qvalgen
c     program to identify the q-values from the image generation. 
c     M is the size of each data block, N is the total numbe of 
c     non-blank rows. 

      parameter (N=8040,M=201)
      real a(N),unQ1(75:274,75:274),unQ2(75:274,75:274)
      real unQ3(75:274,75:274),unQ4(75:274,75:274)
      real Q1(75:274,75:274), Q2(75:274,75:274), Q3(75:274,75:274)
      real q1max,q1min,q2max,q2min,q3max,q3min
      integer i,j,k,t1,t2,t3,t4,t5

      open(unit=5, status='old', file='f1.txt')

      do i=1,N
      k=intgen(i,M)
        if (k.eq.1) then
          read(5,*) t1,t2,t3,t4,t5
          j=75
          else
          read(5,*) a(j),unQ1(t1,j),unQ1(t2,j),unQ1(t3,j)
     &    ,unQ1(t4,j),unQ1(t5,j)
          j=j+1
        endif
      enddo
      write(*,*) 'unQ1(274,274)=',unQ1(274,274)


      open(unit=5, status='old', file='f2.txt')

      do i=1,N
      k=intgen(i,M)
        if (k.eq.1) then
          read(5,*) t1,t2,t3,t4,t5
          j=75
          else
          read(5,*) a(j),unQ2(t1,j),unQ2(t2,j),unQ2(t3,j)
     &    ,unQ2(t4,j),unQ2(t5,j)
          j=j+1
        endif
      enddo
     
      write(*,*) 'unQ2(274,274)=',unQ2(274,274)

      open(unit=5, status='old', file='f3.txt')

      do i=1,N
      k=intgen(i,M)
        if (k.eq.1) then
          read(5,*) t1,t2,t3,t4,t5
          j=75
          else
          read(5,*) a(j),unQ3(t1,j),unQ3(t2,j),unQ3(t3,j)
     &    ,unQ3(t4,j),unQ3(t5,j)
          j=j+1
        endif
      enddo
     
      write(*,*) 'unQ3(274,274)=',unQ3(274,274)

      open(unit=5, status='old', file='f4.txt')

      do i=1,N
      k=intgen(i,M)
        if (k.eq.1) then
          read(5,*) t1,t2,t3,t4,t5
          j=75
          else
          read(5,*) a(j),unQ4(t1,j),unQ4(t2,j),unQ4(t3,j)
     &    ,unQ4(t4,j),unQ4(t5,j)
          j=j+1
        endif
      enddo
     
      write(*,*) 'unQ4(274,274)=',unQ4(274,274)

c     Normalising filters by dividing through by the fourth dataset. 

      do i=75,274
        do j=75,274
           Q1(i,j)=unQ1(i,j)/unQ4(i,j)
           Q2(i,j)=unQ2(i,j)/unQ4(i,j)
           Q3(i,j)=unQ3(i,j)/unQ4(i,j)
        enddo
      enddo  

      write(*,*) 'Q1(274,274)=',Q1(274,274)
      write(*,*) 'Q2(274,274)=',Q2(274,274)
      write(*,*) 'Q3(274,274)=',Q3(274,274)
c     Finding the min and max of each dataset. 

      q1min=Q1(75,75)
      q1max=Q1(75,75)
      q2min=Q2(75,75)
      q2max=Q2(75,75)
      q3min=Q3(75,75)
      q3max=Q3(75,75)

      do i=75,274
         do j=75,274
            if (Q1(i,j).gt.q1max) then
               q1max=Q1(i,j)
            endif
            if (Q1(i,j).lt.q1min) then
               q1min=Q1(i,j)
            endif
            if (Q2(i,j).gt.q2max) then
               q2max=Q2(i,j)
            endif
            if (Q2(i,j).lt.q2min) then
               q2min=Q2(i,j)
            endif
            if (Q3(i,j).gt.q3max) then
               q3max=Q3(i,j)
            endif
            if (Q3(i,j).lt.q3min) then
               q3min=Q3(i,j)
            endif
         enddo
      enddo


      write(*,*) 'q-ranges for f1 are:', q1min, 'to' ,q1max
      write(*,*) 'q-ranges for f2 are:', q2min, 'to' ,q2max
      write(*,*) 'q-ranges for f3 are:', q3min, 'to' ,q3max


      end


      function intgen(a,m)
      integer a,m
      real b,c,d
      b=(a+m-1)*1.0/m
      c=nint(b)
      d=b/c
      if (d.eq.1) then
      intgen=1
      else
      intgen=0
      endif
      end
