c	program plotdata.f
c	plotting program to take in various parameters derived
c	from radial profiling and plot them.
c	columns in the data table (fov.txt) are currently
c
c	radius (physical)
c	Temp		fov
c	Temp_err+	fov
c	Temp_err-	fov
c	Abundance	fov
c	Abund_err+	fov
c	Abund_err-	fov
c	Reduced Chi Squared.

	parameter(N=5, Raderr=300, offset=50)
	real Rad(N), rup(N), rlo(N), Rad1(N), rup1(N), rlo1(N), Rad0(N)

	real Tfov(N), Tfovup(N), Tfovlo(N), Zfov(N), Zfovup(N), Zfovlo(N), C2fov(N)
	real TfovEup(N), TfovElo(N), ZfovEup(N), ZfovElo(N)

	real Texp(N), Texpup(N), Texplo(N), Zexp(N), Zexpup(N), Zexplo(N), C2exp(N)
	real TexpEup(N), TexpElo(N), ZexpEup(N), ZexpElo(N)

	real Tdbs(N), Tdbsup(N), Tdbslo(N), Zdbs(N), Zdbsup(N), Zdbslo(N), C2dbs(N)
	real TdbsEup(N), TdbsElo(N), ZdbsEup(N), ZdbsElo(N)


	integer i

5	FORMAT (F4.0,1X,F4.2,1X,F4.2,1X,F4.2,1X,F4.2,1X,F4.2,1X,F4.2,1X,F4.2)
	open(unit=4, file='fov.txt', status='old')
	do 6 i=1,N
	read(4,5) Rad(i), Tfov(i), Tfovup(i), Tfovlo(i), Zfov(i), Zfovup(i), Zfovlo(i), C2fov(i)
c	write(*,*)Rad(i), Tfov(i), Tfovup(i), Tfovlo(i), Zfov(i), Zfovup(i), Zfovlo(i), C2fov(i)
6	continue

	open(unit=4, file='exp.txt', status='old')
	do 7 i=1, N
	read(4,5) Rad(i), Texp(i), Texpup(i), Texplo(i), Zexp(i), Zexpup(i), Zexplo(i), C2exp(i)
c	write(*,*)Rad(i), Texp(i), Texpup(i), Texplo(i), Zexp(i), Zexpup(i), Zexplo(i), C2exp(i)
7	continue

	open(unit=4, file='dbs.txt', status='old')
	do 8 i=1, N
	read(4,5) Rad(i), Tdbs(i), Tdbsup(i), Tdbslo(i), Zdbs(i), Zdbsup(i), Zdbslo(i), C2dbs(i)
c	write(*,*) Rad(i), Tdbs(i), Tdbsup(i), Tdbslo(i), Zdbs(i), Zdbsup(i), Zdbslo(i), C2dbs(i)
8	continue

c	calculating the errors

	do 10 i=1,N
	rup(i)=Rad(i)+Raderr
	rlo(i)=Rad(i)-Raderr
	Rad1(i)=Rad(i)+offset
	Rad0(i)=Rad(i)-offset
	TfovEup(i)=Tfov(i)+Tfovup(i)
	TfovElo(i)=Tfov(i)-Tfovlo(i)
	ZfovEup(i)=Zfov(i)+Zfovup(i)
	ZfovElo(i)=Zfov(i)-Zfovlo(i)
	TexpEup(i)=Texp(i)+Texpup(i)
	TexpElo(i)=Texp(i)-Texplo(i)
	ZexpEup(i)=Zexp(i)+Zexpup(i)
	ZexpElo(i)=Zexp(i)-Zexplo(i)
	TdbsEup(i)=Tdbs(i)+Tdbsup(i)
	TdbsElo(i)=Tdbs(i)-Tdbslo(i)
	ZdbsEup(i)=Zdbs(i)+Zdbsup(i)
	ZdbsElo(i)=Zdbs(i)-Zdbslo(i)

c	write(*,*) ZfovEup(i)

10	continue

c	Plotting the data
c       1) Temp vs Radius



c        call pgbegin(0,'TR1141.ps/PS',1,1)
        call pgbegin(0,'/XW',1,1)
        call pgscf(3)
        call pgslw(3)
        call pgenv(0.,3000.,2.5,7.5,0,0)
        call pglabel('Radius (physical)','Temperature (keV)',
     &  ' 1141- 1216')
	call pgsch(2.0)
	call pgpt(N,Rad,Tfov,5)
        call pgerry(N,Rad,TfovElo,TfovEup,1.0)
	call pgpt(N,Rad1,Texp,6)
        call pgerry(N,Rad1,TexpElo,TexpEup,1.0)
	call pgpt(N,Rad0,Tdbs,7)
        call pgerry(N,Rad0,TdbsElo,TdbsEup,1.0)
	call pgsch(1.0)
        call pgtext(1500.,3.5,'square=exposure scaling')
        call pgtext(1500.,3.3,'cross=fov scaling')
	call pgtext(1500.,3.1,'triangle=double bg subtraction')
        call pgend

c        call pgbegin(0,'ZR1141.ps/PS',1,1)
        call pgbegin(0,'/XW',1,1)
        call pgscf(3)
        call pgslw(3)
        call pgenv(0.,3000.,0.25,0.8,0,0)
        call pglabel('Radius (physical)','Abundance (Solar)',
     &  ' 1141- 1216')
        call pgsch(2.0)
	call pgpt(N,Rad,Zfov,5)
        call pgerry(N,Rad,ZfovElo,ZfovEup,1.0)
	call pgpt(N,Rad1,Zexp,6)
        call pgerry(N,Rad1,ZexpElo,ZexpEup,1.0)
	call pgpt(N,Rad0,Zdbs,7)
        call pgerry(N,Rad0,ZdbsElo,ZdbsEup,1.0)
        call pgsch(1.0)
        call pgtext(1500.,0.7,'square=exposure scaling')
        call pgtext(1500.,0.68,'cross=fov scaling')
	call pgtext(1500.,0.66,'triangle=double bg subtraction')
        call pgend

c        call pgbegin(0,'CR1141.ps/PS',1,1)
        call pgbegin(0,'/XW',1,1)
        call pgscf(3)
        call pgslw(3)
        call pgenv(0.,3000.,0.9,2.0,0,0)
        call pglabel('Radius (physical)','Reduced Chi Squared',
     &  ' 1141- 1216')
        call pgsch(2.0)
	call pgpt(N,Rad,C2fov,5)
	call pgpt(N,Rad1,C2exp,6)
	call pgpt(N,Rad0,C2dbs,7)
        call pgsch(1.0)
        call pgtext(1500.,1.95,'square=exposure scaling')
        call pgtext(1500.,1.9,'cross=fov scaling')
	call pgtext(1500.,1.85,'triangle=double bg subtraction')
        call pgend


	end
