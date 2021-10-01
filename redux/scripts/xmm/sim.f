      program sim
      
      parameter (N=100,pi=3.141,M=3000)
      real x(N), y(N), z(N)
      integer i,j,k
      
      do i=1,N
         x(i)=i
         y(i)=sin(2*i*pi/N+pi/2)
         z(i)=sin(2*i*pi/N)
c      write(*,*) x(i), y(i), z(i)         
      enddo

c-------PLOT 1, TRY 1-------------
c	call pgbegin(0,'fig1.ps/PS',1,1)
	call pgbegin(0,'/NULL',1,1)
	call pgscf(2)
	call pgslw(3)

c       pgvport defines where you want to draw the first box.
c       (xleft, xright, yleft, yright) in REAL coordinates
	call pgvport(0.1,0.9,0.5,0.9)
c       pgwindow defines the range of the axes
c       (xleft, xright, yleft, yright)
	call pgwindow(0.,101.,-1.1,1.1)
c       pgbox has lots of arguments
c       (xopt,xtick,nxsub,yopt,ytick,nysub)
c       opt defines the plot options, tick defines the tick marks,
c       sub defines the number of subtick marks to divide the interval into. 
	call pgbox('CTMS',0.0,0,'BCNTS',0.5,5)
c       text labels, TLBR are top left bottom right. 
c	call pgmtxt('T',2.0,0.5,0.5,'x')
	call pgmtxt('L',2.0,0.5,0.5,'y')

	do i=1,N
c 	call pgpt1(x(i),y(i),-3)
c         do j=1,10000
c            do k=1,1000
c            enddo
c         enddo
	enddo


c-------PLOT2, TRY 2------------------------------------------------------

	call pgvport(0.1,0.9,0.1,0.5)
	call pgwindow(0.,101.1,-1.1,1.1)
	call pgbox('CBNTS',0.0,0,'BCNTS',0.5,5)

c	call pglabel('log L\\dFIR\\u (erg s\\u-1\\d)','LF slope',' ')
	call pgmtxt('B',2.0,0.5,0.5,'x')
	call pgmtxt('L',2.0,0.5,0.5,'z')
	do i=1,N
	call pgpt1(x(i),z(i),-3)
c         do j=1,10000
c            do k=1,1000
c            enddo
c         enddo
        enddo

c	call pgtext(x(i),y(i),'text')
	call pgend

c-------ALL PLOTS, TRY 2---------------
c       Try 2......
	call pgbegin(0,'/XW',1,1)
	call pgscf(2)
	call pgslw(3)

c       everything is going inside the do loop now...
	do i=1,N
	 call pgvport(0.1,0.9,0.5,0.9)
	 call pgwindow(0.,101.,-1.1,1.1)
	 call pgbox('CTMS',0.0,0,'BCNTS',0.5,5)
	 call pgmtxt('L',2.0,0.5,0.5,'y')
 	 call pgpt1(x(i),y(i),-3)
	 call pgvport(0.1,0.9,0.1,0.5)
	 call pgwindow(0.,101.1,-1.1,1.1)
	 call pgbox('CBNTS',0.0,0,'BCNTS',0.5,5)
	 call pgmtxt('B',2.0,0.5,0.5,'x')
	 call pgmtxt('L',2.0,0.5,0.5,'z')
	 call pgpt1(x(i),z(i),-3)
          do j=1,M
             do k=1,M
             enddo
          enddo
	enddo

	call pgend






      end
