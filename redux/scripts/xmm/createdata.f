	program createdata
c	this program is generating a spectra in ascii format of photon/cm/cm/s/keV vs energy.
c	need to have text dumps already of the output files required ie the wrmf, warf and simulated spectra.
c	in this program they are called arf.txt, rmf.txt,data.txt
c	fdump "hcg62_spec_sou.warf+1" arf.txt "SPECRESP ENERG_LO ENERG_HI" - prhead=no
c	fdump "hcg62_spec_sou.wrmf+2" rmf.txt "CHANNEL E_MIN E_MAX" - prhead=no
c	fdump "meksim1_5-4-5.pha+1" data.txt "CHANNEL RATE" - prhead=no


	parameter(N=1024)
	parameter(energ_lo1_1=832.,energ_hi1_1=1022.,energ_lo1_2=1343.,energ_hi1_2=1577.,energ_lo1_3=1723.,energ_hi1_3=1956.)
	parameter(energ_lo2=300.,energ_hi2=438.)
	parameter(energ_lo3=672.,energ_hi3=832.)
	parameter(energ_lo4_1=1051.,energ_hi4_1=1343.,energ_lo4_2=1578.,energ_hi4_2=1723.,energ_lo4_3=1953.,energ_hi4_3=3431.)

	real column(N), channel(N), count_rate(N), S(N), E(N), incr(N),incr1, sum, temp, scale
	real column_rmf(N), channel_rmf(N), E_min_rmf(N), E_max_rmf(N), E_av_rmf(N)
	real column_arf(N), specresp(N), E_min_arf(N), E_max_arf(N), E_av_arf(N)
	real E1(N),CR1(N),S1(N),S1S(N)
	real E2(N),CR2(N),S2(N),S2S(N)
	real E3(N),CR3(N),S3(N),S3S(N)
	real E4(N),CR4(N),S4(N),S4S(N)
	integer i,j,J1,J2,J3,J4,k



c-------reading in spectral table----
	open(unit=5,file='data.txt',status='old')
	read(*,*)
	read(*,*)
	read(*,*)
	read(*,*)
	do 50 i=1,N
	read(*,*) column(i),channel(i),count_rate(i)
50	continue
	close(unit=5)

c-------reading in rmf table--------
	open(unit=5,file='rmf.txt',status='old')
	read(*,*)
	read(*,*)
	read(*,*)
	read(*,*)

	do 51 i=1,N
	read(*,*) column_rmf(i), channel_rmf(i), E_min_rmf(i), E_max_rmf(i)
	E_av_rmf(i)=(E_min_rmf(i)+E_max_rmf(i))/2
51	continue
	close(unit=5)

c-------reading in arf table---------
	open(unit=5,file='arf.txt',status='old')
	read(*,*)
	read(*,*)
	read(*,*)
	read(*,*)
	do 52 i=1,N
	read(*,*) column_arf(i), specresp(i), E_min_arf(i), E_max_arf(i)
	E_av_arf(i)=(E_min_arf(i)+E_max_arf(i))/2
52	continue
	close(unit=5)

c-------converting spectrum to x-axis units of keV using rmf-------
c-------actually already done. The values of E_av_rmf(i) match up to count_rate(i) already----

c-------converting y-axis to units of ph/cm/cm/s/keV---------------
c-------need to use arf as a lookup table---

	do 53 i=1,N
	S(i)=0.0
		do 54 k=1,N
		if (((E_av_rmf(i)).gt.(E_av_arf(k))).and.((E_av_rmf(i)).lt.(E_av_arf(k+1)))) then
			if ((E_av_rmf(i)-(E_av_arf(k))).lt.(E_av_arf(k+1)-(E_av_rmf(i)))) then
				S(i)=count_rate(i)/specresp(k)
				else
				S(i)=count_rate(i)/specresp(k+1)
			endif
		endif
54		continue
	E(i)=E_av_rmf(i)*1000
53	continue

c-------All done! (well regarding the spectra anyway). Only channels 22-275 are noticed. These are the ones
c-------used in the generation of the simulated spectra. Now we need to create S(E)dE for each filter.


c--------FILTER2--------

	j=0
	do 7 i=1,N
	if ((E(i).gt.energ_lo2).and.(E(i).lt.energ_hi2)) then
		j=j+1
		E2(j)=E(i)/1000
		CR2(j)=S(i)
	endif
7	continue

	J2=j
	sum=0.0
	do 55 j=1,J2-1
	incr(j)=E(j+1)-E(j)
	sum=sum+incr(j)
55	continue
	incr1=sum/(J2-1)

	temp=0.
	do 8 j=1,J2
c-------S2 is S(E)dE-----
	S2(j)=CR2(j)*incr1/(E2(j))
	temp=temp+S2(j)
8	continue

c-------S2S is S(E)dE scales such that sum_j S(E)dE=1
	scale=temp
c	write(*,*) 'scale=',scale
	temp=0.
	do 9 j=1,J2
	S2S(j)=S2(j)/scale
	temp=temp+S2S(j)
9	continue

11	format(F6.4,2X,F6.4)
12	format('# E (keV) S(E)*0.015')
13	format('#-------------------')

	open(unit=6,file='data2.dat',status='new')
	write(*,12)
	write(*,13)
	do 10 j=1,J2
	write(6,11) E2(j),S2S(j)
10	continue
	close(unit=6)


c-------FILTER3

	j=0
	do 27 i=1,N
	if ((E(i).gt.energ_lo3).and.(E(i).lt.energ_hi3)) then
		j=j+1
		E3(j)=E(i)/1000
		CR3(j)=S(i)
	endif
27	continue


	J3=j
	sum=0.0
	do 56 j=1,J3-1
	incr(j)=E(j+1)-E(j)
	sum=sum+incr(j)
56	continue
	incr1=sum/(J3-1)

	temp=0.
	do 28 j=1,J3
c-------S3 is S(E)dE-----
	S3(j)=CR3(j)*incr1/(E3(j))
	temp=temp+S3(j)
28	continue

	scale=temp
	temp=0.
	do 29 j=1,J3
	S3S(j)=S3(j)/scale
	temp=temp+S3S(j)
29	continue

	open(unit=26,file='data3.dat',status='new')
	write(26,12)
	write(26,13)
	do 20 j=1,J3
	write(26,11) E3(j),S3S(j)
20	continue
	close(unit=26)

c-------FILTER4

	j=0
	do 37 i=1,N
	if ((E(i).gt.energ_lo4_1).and.(E(i).lt.energ_hi4_1)) then
		j=j+1
		E4(j)=E(i)/1000
		CR4(j)=S(i)
	endif
	if ((E(i).gt.energ_lo4_2).and.(E(i).lt.energ_hi4_2)) then
		j=j+1
		E4(j)=E(i)/1000
		CR4(j)=S(i)
	endif
	if ((E(i).gt.energ_lo4_3).and.(E(i).lt.energ_hi4_3)) then
		j=j+1
		E4(j)=E(i)/1000
		CR4(j)=S(i)
	endif
37	continue

	J4=j
	sum=0.0
	do 57 j=1,J4-1
	incr(j)=E(j+1)-E(j)
	sum=sum+incr(j)
57	continue
	incr1=sum/(J4-1)

	temp=0.
	do 38 j=1,J4
c-------S4 is S(E)dE-----
	S4(j)=CR4(j)*incr1/(E4(j))
	temp=temp+S4(j)

38	continue

	scale=temp
	temp=0.
	do 39 j=1,J4
	S4S(j)=S4(j)/scale
	temp=temp+S4S(j)
39	continue

	open(unit=36,file='data4.dat',status='new')
	write(36,12)
	write(36,13)
	do 30 j=1,J4
	write(36,11) E4(j),S4S(j)
30	continue
	close(unit=36)

c-------FILTER1

	j=0
	do 47 i=1,N
	if ((E(i).gt.energ_lo1_1).and.(E(i).lt.energ_hi1_1)) then
		j=j+1
		E1(j)=E(i)/1000
		CR1(j)=S(i)
	endif
	if ((E(i).gt.energ_lo1_2).and.(E(i).lt.energ_hi1_2)) then
		j=j+1
		E1(j)=E(i)/1000
		CR1(j)=S(i)
	endif
	if ((E(i).gt.energ_lo1_3).and.(E(i).lt.energ_hi1_3)) then
		j=j+1
		E1(j)=E(i)/1000
		CR1(j)=S(i)
	endif
47	continue

	J1=j
	sum=0.0
	do 58 j=1,J1-1
	incr(j)=E(j+1)-E(j)
	sum=sum+incr(j)
58	continue
	incr1=sum/(J1-1)

	temp=0.
	do 48 j=1,J1
c-------S1 is S(E)dE-----
	S1(j)=CR1(j)*incr1/(E1(j))
	temp=temp+S1(j)

48	continue

	scale=temp
	temp=0.
	do 49 j=1,J1
	S1S(j)=S1(j)/scale
	temp=temp+S1S(j)
49	continue

	open(unit=46,file='data1.dat',status='new')
	write(46,12)
	write(46,13)
	do 40 j=1,J1
	write(46,11) E1(j),S1S(j)
40	continue
	close(unit=46)




	end
