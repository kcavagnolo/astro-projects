!*  Schedule for VLBA_SC   *!
!*  Experiment bc195    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   9.30  Release near March 23, 2010 *!
!* PI:       Kenneth Cavagnolo *!
!* Address:  OCA *!
!*           Blvd de Observatoire *!
!*           B.P. 4229, F-06304 Nice France *!
!*  *!
!* Phone:    +33492003062 (w) or +33687098367 (h) *!
!* EMAIL:    cavagnolo@oca.eu *!
!* Fax:      +33492003118 *!
!* Phone during observation: +33492003062 (w) or +33687098367 (h) *!
!* Observing mode: Continuum v6cm-512-16-2 *!
!* Notes:    Target              : IRAS 09104+4109 *!
!*           Phase calibrator    : J0923+4125 *!
!*           Coherence test      : J0926+4029 *!
!*           Amplitude calibrator: J0927+3902 (4C39.25) *!
!*  Start at 03h54m18s     Sun, 2010 Dec 26  Day of year  360   *!
program=bc195   
diskformat=mark5a
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 03h54m18s to 03h59m17s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
maxcaltime= 120
fe=(1,6cm),(3,6cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
logging=STANDARD
nchan=16
format=VLBA1:2
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,1),(4,2),(5,3),(6,4),(7,3),(8,4),(9,5),(10,6),(11,5)
baseband=(12,6),(13,7),(14,8),(15,7),(16,8)
ifchan=(1,A),(2,C),(3,A),(4,C),(5,A),(6,C),(7,A),(8,C),(9,A),(10,C),(11,A)
ifchan=(12,C),(13,A),(14,C),(15,A),(16,C)
sideband=(1,L),(2,L),(3,U),(4,U),(5,L),(6,L),(7,U),(8,U),(9,L),(10,L),(11,U)
sideband=(12,U),(13,L),(14,L),(15,U),(16,U)
bits=(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2),(9,2),(10,2),(11,2),(12,2)
bits=(13,2),(14,2),(15,2),(16,2)
period=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1),(11,1)
period=(12,1),(13,1),(14,1),(15,1),(16,1)
level=(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1),(9,-1),(10,-1)
level=(11,-1),(12,-1),(13,-1),(14,-1),(15,-1),(16,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,858.49),( 2,858.49),( 3,858.49),( 4,858.49),( 5,874.49),( 6,874.49)
bbsynth=( 7,874.49),( 8,874.49),( 9,890.49),(10,890.49),(11,890.49),(12,890.49)
bbsynth=(13,906.49),(14,906.49),(15,906.49),(16,906.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M),(9,8M),(10,8M)
bbfilter=(11,8M),(12,8M),(13,8M),(14,8M),(15,8M),(16,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S5),(4,S7),(5,S9),(6,S11),(7,S13),(8,S15)
pcalxbit2=(1,S2),(2,S4),(3,S6),(4,S8),(5,S10),(6,S12),(7,S14),(8,S16)
pcalxfreq1=(1,490),(2,510),(3,490),(4,510),(5,490),(6,510),(7,490),(8,510)
pcalxfreq2=(1,490),(2,510),(3,490),(4,510),(5,490),(6,510),(7,490),(8,510)
samplerate=16M
disk=off
  date = 2010Dec26
stop=03h54m18s   !NEXT!        
qual=  0
disk=on
stop=03h59m17s   !NEXT!

!* --- Scan from 03h59m27s to 04h00m37s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=03h59m27s   !NEXT!        
qual=  0
disk=on
stop=04h00m37s   !NEXT!

!* --- Scan from 04h00m37s to 04h04m11s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h04m11s   !NEXT!

!* --- Scan from 04h04m11s to 04h05m31s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h05m31s   !NEXT!

!* --- Scan from 04h05m31s to 04h09m05s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h09m05s   !NEXT!

!* --- Scan from 04h09m05s to 04h10m24s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h10m24s   !NEXT!

!* --- Scan from 04h10m24s to 04h13m59s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h13m59s   !NEXT!

!* --- Scan from 04h13m59s to 04h15m18s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h15m18s   !NEXT!

!* --- Scan from 04h15m18s to 04h18m53s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h18m53s   !NEXT!

!* --- Scan from 04h18m53s to 04h20m12s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h20m12s   !NEXT!

!* --- Scan from 04h20m12s to 04h23m46s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h23m46s   !NEXT!

!* --- Scan from 04h23m46s to 04h25m06s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h25m06s   !NEXT!

!* --- Scan from 04h25m06s to 04h28m40s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h28m40s   !NEXT!

!* --- Scan from 04h28m40s to 04h30m00s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h30m00s   !NEXT!

!* --- Scan from 04h30m00s to 04h33m34s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h33m34s   !NEXT!

!* --- Scan from 04h33m34s to 04h34m54s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h34m54s   !NEXT!

!* --- Scan from 04h34m54s to 04h38m28s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h38m28s   !NEXT!

!* --- Scan from 04h38m28s to 04h39m47s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h39m47s   !NEXT!

!* --- Scan from 04h39m47s to 04h43m22s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h43m22s   !NEXT!

!* --- Scan from 04h43m22s to 04h44m41s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h44m41s   !NEXT!

!* --- Scan from 04h44m41s to 04h48m15s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h48m15s   !NEXT!

!* --- Scan from 04h48m15s to 04h49m35s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h49m35s   !NEXT!

!* --- Scan from 04h49m35s to 04h53m09s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h53m09s   !NEXT!

!* --- Scan from 04h53m19s to 04h54m29s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=04h53m19s   !NEXT!        
qual=  0
disk=on
stop=04h54m29s   !NEXT!

!* --- Scan from 04h54m29s to 04h58m03s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=04h58m03s   !NEXT!

!* --- Scan from 04h58m03s to 04h59m23s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=04h59m23s   !NEXT!

!* --- Scan from 04h59m23s to 05h02m57s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h02m57s   !NEXT!

!* --- Scan from 05h02m57s to 05h04m16s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h04m16s   !NEXT!

!* --- Scan from 05h04m16s to 05h07m50s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h07m50s   !NEXT!

!* --- Scan from 05h07m50s to 05h09m10s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h09m10s   !NEXT!

!* --- Scan from 05h09m10s to 05h12m44s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h12m44s   !NEXT!

!* --- Scan from 05h12m44s to 05h14m03s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h14m03s   !NEXT!

!* --- Scan from 05h14m03s to 05h17m37s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h17m37s   !NEXT!

!* --- Scan from 05h17m37s to 05h18m57s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h18m57s   !NEXT!

!* --- Scan from 05h18m57s to 05h22m31s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h22m31s   !NEXT!

!* --- Scan from 05h22m31s to 05h23m50s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h23m50s   !NEXT!

!* --- Scan from 05h23m50s to 05h27m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h27m24s   !NEXT!

!* --- Scan from 05h27m24s to 05h28m44s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h28m44s   !NEXT!

!* --- Scan from 05h28m44s to 05h32m18s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h32m18s   !NEXT!

!* --- Scan from 05h32m18s to 05h33m37s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h33m37s   !NEXT!

!* --- Scan from 05h33m37s to 05h37m11s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h37m11s   !NEXT!

!* --- Scan from 05h37m11s to 05h38m30s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h38m30s   !NEXT!

!* --- Scan from 05h38m30s to 05h42m04s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h42m04s   !NEXT!

!* --- Scan from 05h42m04s to 05h43m23s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h43m23s   !NEXT!

!* --- Scan from 05h43m23s to 05h46m57s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h46m57s   !NEXT!

!* --- Scan from 05h47m07s to 05h48m17s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=05h47m07s   !NEXT!        
qual=  0
disk=on
stop=05h48m17s   !NEXT!

!* --- Scan from 05h48m17s to 05h51m51s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h51m51s   !NEXT!

!* --- Scan from 05h51m51s to 05h53m10s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h53m10s   !NEXT!

!* --- Scan from 05h53m10s to 05h56m44s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=05h56m44s   !NEXT!

!* --- Scan from 05h56m44s to 05h58m03s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=05h58m03s   !NEXT!

!* --- Scan from 05h58m03s to 06h01m37s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h01m37s   !NEXT!

!* --- Scan from 06h01m37s to 06h02m57s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h02m57s   !NEXT!

!* --- Scan from 06h02m57s to 06h06m31s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h06m31s   !NEXT!

!* --- Scan from 06h06m31s to 06h07m50s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h07m50s   !NEXT!

!* --- Scan from 06h07m50s to 06h11m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h11m24s   !NEXT!

!* --- Scan from 06h11m24s to 06h12m43s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h12m43s   !NEXT!

!* --- Scan from 06h12m43s to 06h16m17s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h16m17s   !NEXT!

!* --- Scan from 06h16m17s to 06h17m37s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h17m37s   !NEXT!

!* --- Scan from 06h17m37s to 06h21m11s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h21m11s   !NEXT!

!* --- Scan from 06h21m11s to 06h22m30s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h22m30s   !NEXT!

!* --- Scan from 06h22m30s to 06h26m04s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h26m04s   !NEXT!

!* --- Scan from 06h26m04s to 06h27m24s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h27m24s   !NEXT!

!* --- Scan from 06h27m24s to 06h30m58s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h30m58s   !NEXT!

!* --- Scan from 06h30m58s to 06h32m17s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h32m17s   !NEXT!

!* --- Scan from 06h32m17s to 06h35m51s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h35m51s   !NEXT!

!* --- Scan from 06h35m51s to 06h37m11s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h37m11s   !NEXT!

!* --- Scan from 06h37m11s to 06h40m45s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h40m45s   !NEXT!

!* --- Scan from 06h40m55s to 06h42m05s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=06h40m55s   !NEXT!        
qual=  0
disk=on
stop=06h42m05s   !NEXT!

!* --- Scan from 06h42m05s to 06h47m12s   Sun, 2010 Dec 26 --- *!
sname='J0926+4029'  ra=09h26m00.426850s  dec= 40d29'49.67273"  qual=  0  calib='V'
disk=on
stop=06h47m12s   !NEXT!

!* --- Scan from 06h47m12s to 06h48m30s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h48m30s   !NEXT!

!* --- Scan from 06h48m42s to 06h53m42s   Sun, 2010 Dec 26 --- *!
sname='4C39.25'  ra=09h27m03.013936s  dec= 39d02'20.85184"  qual=999  calib='V'
disk=off
stop=06h48m42s   !NEXT!        
qual=  0
disk=on
stop=06h53m42s   !NEXT!

!* --- Scan from 06h53m54s to 06h55m04s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=06h53m54s   !NEXT!        
qual=  0
disk=on
stop=06h55m04s   !NEXT!

!* --- Scan from 06h55m04s to 06h58m38s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=06h58m38s   !NEXT!

!* --- Scan from 06h58m38s to 06h59m58s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=06h59m58s   !NEXT!

!* --- Scan from 07h00m08s to 07h03m32s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h00m08s   !NEXT!        
qual=  0
disk=on
stop=07h03m32s   !NEXT!

!* --- Scan from 07h03m43s to 07h04m53s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h03m43s   !NEXT!        
qual=  0
disk=on
stop=07h04m53s   !NEXT!

!* --- Scan from 07h05m03s to 07h08m28s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h05m03s   !NEXT!        
qual=  0
disk=on
stop=07h08m28s   !NEXT!

!* --- Scan from 07h08m39s to 07h09m48s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h08m39s   !NEXT!        
qual=  0
disk=on
stop=07h09m48s   !NEXT!

!* --- Scan from 07h10m00s to 07h13m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h10m00s   !NEXT!        
qual=  0
disk=on
stop=07h13m24s   !NEXT!

!* --- Scan from 07h13m36s to 07h14m46s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h13m36s   !NEXT!        
qual=  0
disk=on
stop=07h14m46s   !NEXT!

!* --- Scan from 07h14m58s to 07h18m23s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h14m58s   !NEXT!        
qual=  0
disk=on
stop=07h18m23s   !NEXT!

!* --- Scan from 07h18m36s to 07h19m45s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h18m36s   !NEXT!        
qual=  0
disk=on
stop=07h19m45s   !NEXT!

!* --- Scan from 07h19m59s to 07h23m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h19m59s   !NEXT!        
qual=  0
disk=on
stop=07h23m24s   !NEXT!

!* --- Scan from 07h23m39s to 07h24m48s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h23m39s   !NEXT!        
qual=  0
disk=on
stop=07h24m48s   !NEXT!

!* --- Scan from 07h25m04s to 07h28m29s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h25m04s   !NEXT!        
qual=  0
disk=on
stop=07h28m29s   !NEXT!

!* --- Scan from 07h28m47s to 07h29m57s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h28m47s   !NEXT!        
qual=  0
disk=on
stop=07h29m57s   !NEXT!

!* --- Scan from 07h30m16s to 07h33m41s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h30m16s   !NEXT!        
qual=  0
disk=on
stop=07h33m41s   !NEXT!

!* --- Scan from 07h34m03s to 07h35m13s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h34m03s   !NEXT!        
qual=  0
disk=on
stop=07h35m13s   !NEXT!

!* --- Scan from 07h35m39s to 07h39m03s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h35m39s   !NEXT!        
qual=  0
disk=on
stop=07h39m03s   !NEXT!

!* --- Scan from 07h39m34s to 07h40m44s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h39m34s   !NEXT!        
qual=  0
disk=on
stop=07h40m44s   !NEXT!

!* --- Scan from 07h41m20s to 07h44m44s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h41m20s   !NEXT!        
qual=  0
disk=on
stop=07h44m44s   !NEXT!

!* --- Scan from 07h45m24s to 07h46m33s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h45m24s   !NEXT!        
qual=  0
disk=on
stop=07h46m33s   !NEXT!

!* --- Scan from 07h47m18s to 07h50m43s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h47m18s   !NEXT!        
qual=  0
disk=on
stop=07h50m43s   !NEXT!

!* --- Scan from 07h51m20s to 07h52m30s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h51m20s   !NEXT!        
qual=  0
disk=on
stop=07h52m30s   !NEXT!

!* --- Scan from 07h53m08s to 07h56m32s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h53m08s   !NEXT!        
qual=  0
disk=on
stop=07h56m32s   !NEXT!

!* --- Scan from 07h56m56s to 07h58m06s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=07h56m56s   !NEXT!        
qual=  0
disk=on
stop=07h58m06s   !NEXT!

!* --- Scan from 07h58m29s to 08h01m53s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=07h58m29s   !NEXT!        
qual=  0
disk=on
stop=08h01m53s   !NEXT!

!* --- Scan from 08h02m08s to 08h03m18s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h02m08s   !NEXT!        
qual=  0
disk=on
stop=08h03m18s   !NEXT!

!* --- Scan from 08h03m32s to 08h06m57s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h03m32s   !NEXT!        
qual=  0
disk=on
stop=08h06m57s   !NEXT!

!* --- Scan from 08h07m07s to 08h08m17s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h07m07s   !NEXT!        
qual=  0
disk=on
stop=08h08m17s   !NEXT!

!* --- Scan from 08h08m27s to 08h11m52s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h08m27s   !NEXT!        
qual=  0
disk=on
stop=08h11m52s   !NEXT!

!* --- Scan from 08h11m52s to 08h13m11s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=08h13m11s   !NEXT!

!* --- Scan from 08h13m11s to 08h16m46s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=08h16m46s   !NEXT!

!* --- Scan from 08h16m46s to 08h18m05s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=08h18m05s   !NEXT!

!* --- Scan from 08h18m05s to 08h21m40s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=08h21m40s   !NEXT!

!* --- Scan from 08h21m40s to 08h22m59s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=08h22m59s   !NEXT!

!* --- Scan from 08h22m59s to 08h26m33s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=08h26m33s   !NEXT!

!* --- Scan from 08h26m33s to 08h27m53s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=08h27m53s   !NEXT!

!* --- Scan from 08h28m03s to 08h31m27s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h28m03s   !NEXT!        
qual=  0
disk=on
stop=08h31m27s   !NEXT!

!* --- Scan from 08h31m38s to 08h32m47s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h31m38s   !NEXT!        
qual=  0
disk=on
stop=08h32m47s   !NEXT!

!* --- Scan from 08h32m58s to 08h36m23s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h32m58s   !NEXT!        
qual=  0
disk=on
stop=08h36m23s   !NEXT!

!* --- Scan from 08h36m34s to 08h37m43s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h36m34s   !NEXT!        
qual=  0
disk=on
stop=08h37m43s   !NEXT!

!* --- Scan from 08h37m55s to 08h41m19s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h37m55s   !NEXT!        
qual=  0
disk=on
stop=08h41m19s   !NEXT!

!* --- Scan from 08h41m32s to 08h42m42s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h41m32s   !NEXT!        
qual=  0
disk=on
stop=08h42m42s   !NEXT!

!* --- Scan from 08h42m55s to 08h46m19s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h42m55s   !NEXT!        
qual=  0
disk=on
stop=08h46m19s   !NEXT!

!* --- Scan from 08h46m34s to 08h47m43s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h46m34s   !NEXT!        
qual=  0
disk=on
stop=08h47m43s   !NEXT!

!* --- Scan from 08h47m59s to 08h51m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h47m59s   !NEXT!        
qual=  0
disk=on
stop=08h51m24s   !NEXT!

!* --- Scan from 08h51m42s to 08h52m52s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h51m42s   !NEXT!        
qual=  0
disk=on
stop=08h52m52s   !NEXT!

!* --- Scan from 08h53m13s to 08h56m38s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h53m13s   !NEXT!        
qual=  0
disk=on
stop=08h56m38s   !NEXT!

!* --- Scan from 08h57m07s to 08h58m17s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=08h57m07s   !NEXT!        
qual=  0
disk=on
stop=08h58m17s   !NEXT!

!* --- Scan from 08h58m56s to 09h02m20s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=08h58m56s   !NEXT!        
qual=  0
disk=on
stop=09h02m20s   !NEXT!

!* --- Scan from 09h03m22s to 09h04m31s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h03m22s   !NEXT!        
qual=  0
disk=on
stop=09h04m31s   !NEXT!

!* --- Scan from 09h05m54s to 09h09m18s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=09h05m54s   !NEXT!        
qual=  0
disk=on
stop=09h09m18s   !NEXT!

!* --- Scan from 09h10m19s to 09h11m29s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h10m19s   !NEXT!        
qual=  0
disk=on
stop=09h11m29s   !NEXT!

!* --- Scan from 09h12m20s to 09h15m45s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=09h12m20s   !NEXT!        
qual=  0
disk=on
stop=09h15m45s   !NEXT!

!* --- Scan from 09h15m56s to 09h17m06s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h15m56s   !NEXT!        
qual=  0
disk=on
stop=09h17m06s   !NEXT!

!* --- Scan from 09h17m06s to 09h20m40s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=09h20m40s   !NEXT!

!* --- Scan from 09h20m40s to 09h22m00s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=09h22m00s   !NEXT!

!* --- Scan from 09h22m00s to 09h25m34s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=09h25m34s   !NEXT!

!* --- Scan from 09h25m44s to 09h26m54s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h25m44s   !NEXT!        
qual=  0
disk=on
stop=09h26m54s   !NEXT!

!* --- Scan from 09h27m04s to 09h30m28s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=09h27m04s   !NEXT!        
qual=  0
disk=on
stop=09h30m28s   !NEXT!

!* --- Scan from 09h30m39s to 09h31m49s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h30m39s   !NEXT!        
qual=  0
disk=on
stop=09h31m49s   !NEXT!

!* --- Scan from 09h32m00s to 09h35m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=09h32m00s   !NEXT!        
qual=  0
disk=on
stop=09h35m24s   !NEXT!

!* --- Scan from 09h35m35s to 09h36m45s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h35m35s   !NEXT!        
qual=  0
disk=on
stop=09h36m45s   !NEXT!

!* --- Scan from 09h36m56s to 09h40m21s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=09h36m56s   !NEXT!        
qual=  0
disk=on
stop=09h40m21s   !NEXT!

!* --- Scan from 09h40m32s to 09h41m42s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h40m32s   !NEXT!        
qual=  0
disk=on
stop=09h41m42s   !NEXT!

!* --- Scan from 09h41m54s to 09h45m18s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=09h41m54s   !NEXT!        
qual=  0
disk=on
stop=09h45m18s   !NEXT!

!* --- Scan from 09h45m31s to 09h46m41s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h45m31s   !NEXT!        
qual=  0
disk=on
stop=09h46m41s   !NEXT!

!* --- Scan from 09h46m52s to 09h51m52s   Sun, 2010 Dec 26 --- *!
sname='J0926+4029'  ra=09h26m00.426850s  dec= 40d29'49.67273"  qual=999  calib='V'
disk=off
stop=09h46m52s   !NEXT!        
qual=  0
disk=on
stop=09h51m52s   !NEXT!

!* --- Scan from 09h52m03s to 09h53m13s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h52m03s   !NEXT!        
qual=  0
disk=on
stop=09h53m13s   !NEXT!

!* --- Scan from 09h53m33s to 09h58m32s   Sun, 2010 Dec 26 --- *!
sname='4C39.25'  ra=09h27m03.013936s  dec= 39d02'20.85184"  qual=999  calib='V'
disk=off
stop=09h53m33s   !NEXT!        
qual=  0
disk=on
stop=09h58m32s   !NEXT!

!* --- Scan from 09h58m52s to 10h00m02s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=09h58m52s   !NEXT!        
qual=  0
disk=on
stop=10h00m02s   !NEXT!

!* --- Scan from 10h00m20s to 10h03m45s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h00m20s   !NEXT!        
qual=  0
disk=on
stop=10h03m45s   !NEXT!

!* --- Scan from 10h04m03s to 10h05m13s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h04m03s   !NEXT!        
qual=  0
disk=on
stop=10h05m13s   !NEXT!

!* --- Scan from 10h05m33s to 10h08m57s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h05m33s   !NEXT!        
qual=  0
disk=on
stop=10h08m57s   !NEXT!

!* --- Scan from 10h09m16s to 10h10m26s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h09m16s   !NEXT!        
qual=  0
disk=on
stop=10h10m26s   !NEXT!

!* --- Scan from 10h10m45s to 10h14m10s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h10m45s   !NEXT!        
qual=  0
disk=on
stop=10h14m10s   !NEXT!

!* --- Scan from 10h14m28s to 10h15m38s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h14m28s   !NEXT!        
qual=  0
disk=on
stop=10h15m38s   !NEXT!

!* --- Scan from 10h15m57s to 10h19m21s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h15m57s   !NEXT!        
qual=  0
disk=on
stop=10h19m21s   !NEXT!

!* --- Scan from 10h19m38s to 10h20m48s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h19m38s   !NEXT!        
qual=  0
disk=on
stop=10h20m48s   !NEXT!

!* --- Scan from 10h21m06s to 10h24m30s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h21m06s   !NEXT!        
qual=  0
disk=on
stop=10h24m30s   !NEXT!

!* --- Scan from 10h24m46s to 10h25m56s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h24m46s   !NEXT!        
qual=  0
disk=on
stop=10h25m56s   !NEXT!

!* --- Scan from 10h26m12s to 10h29m36s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h26m12s   !NEXT!        
qual=  0
disk=on
stop=10h29m36s   !NEXT!

!* --- Scan from 10h29m51s to 10h31m01s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h29m51s   !NEXT!        
qual=  0
disk=on
stop=10h31m01s   !NEXT!

!* --- Scan from 10h31m16s to 10h34m41s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h31m16s   !NEXT!        
qual=  0
disk=on
stop=10h34m41s   !NEXT!

!* --- Scan from 10h34m55s to 10h36m05s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h34m55s   !NEXT!        
qual=  0
disk=on
stop=10h36m05s   !NEXT!

!* --- Scan from 10h36m20s to 10h39m44s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h36m20s   !NEXT!        
qual=  0
disk=on
stop=10h39m44s   !NEXT!

!* --- Scan from 10h40m00s to 10h41m10s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h40m00s   !NEXT!        
qual=  0
disk=on
stop=10h41m10s   !NEXT!

!* --- Scan from 10h41m28s to 10h44m52s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h41m28s   !NEXT!        
qual=  0
disk=on
stop=10h44m52s   !NEXT!

!* --- Scan from 10h45m11s to 10h46m21s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h45m11s   !NEXT!        
qual=  0
disk=on
stop=10h46m21s   !NEXT!

!* --- Scan from 10h46m43s to 10h50m07s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h46m43s   !NEXT!        
qual=  0
disk=on
stop=10h50m07s   !NEXT!

!* --- Scan from 10h50m29s to 10h51m39s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h50m29s   !NEXT!        
qual=  0
disk=on
stop=10h51m39s   !NEXT!

!* --- Scan from 10h52m03s to 10h55m28s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h52m03s   !NEXT!        
qual=  0
disk=on
stop=10h55m28s   !NEXT!

!* --- Scan from 10h55m51s to 10h57m01s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=10h55m51s   !NEXT!        
qual=  0
disk=on
stop=10h57m01s   !NEXT!

!* --- Scan from 10h57m25s to 11h00m50s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=10h57m25s   !NEXT!        
qual=  0
disk=on
stop=11h00m50s   !NEXT!

!* --- Scan from 11h01m12s to 11h02m22s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h01m12s   !NEXT!        
qual=  0
disk=on
stop=11h02m22s   !NEXT!

!* --- Scan from 11h02m45s to 11h06m09s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h02m45s   !NEXT!        
qual=  0
disk=on
stop=11h06m09s   !NEXT!

!* --- Scan from 11h06m29s to 11h07m39s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h06m29s   !NEXT!        
qual=  0
disk=on
stop=11h07m39s   !NEXT!

!* --- Scan from 11h07m59s to 11h11m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h07m59s   !NEXT!        
qual=  0
disk=on
stop=11h11m24s   !NEXT!

!* --- Scan from 11h11m41s to 11h12m51s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h11m41s   !NEXT!        
qual=  0
disk=on
stop=11h12m51s   !NEXT!

!* --- Scan from 11h13m09s to 11h16m33s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h13m09s   !NEXT!        
qual=  0
disk=on
stop=11h16m33s   !NEXT!

!* --- Scan from 11h16m49s to 11h17m58s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h16m49s   !NEXT!        
qual=  0
disk=on
stop=11h17m58s   !NEXT!

!* --- Scan from 11h18m14s to 11h21m38s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h18m14s   !NEXT!        
qual=  0
disk=on
stop=11h21m38s   !NEXT!

!* --- Scan from 11h21m52s to 11h23m02s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h21m52s   !NEXT!        
qual=  0
disk=on
stop=11h23m02s   !NEXT!

!* --- Scan from 11h23m15s to 11h26m40s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h23m15s   !NEXT!        
qual=  0
disk=on
stop=11h26m40s   !NEXT!

!* --- Scan from 11h26m52s to 11h28m02s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h26m52s   !NEXT!        
qual=  0
disk=on
stop=11h28m02s   !NEXT!

!* --- Scan from 11h28m14s to 11h31m39s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h28m14s   !NEXT!        
qual=  0
disk=on
stop=11h31m39s   !NEXT!

!* --- Scan from 11h31m50s to 11h33m00s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h31m50s   !NEXT!        
qual=  0
disk=on
stop=11h33m00s   !NEXT!

!* --- Scan from 11h33m11s to 11h36m36s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h33m11s   !NEXT!        
qual=  0
disk=on
stop=11h36m36s   !NEXT!

!* --- Scan from 11h36m46s to 11h37m56s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h36m46s   !NEXT!        
qual=  0
disk=on
stop=11h37m56s   !NEXT!

!* --- Scan from 11h38m07s to 11h41m31s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=999  calib='N'
disk=off
stop=11h38m07s   !NEXT!        
qual=  0
disk=on
stop=11h41m31s   !NEXT!

!* --- Scan from 11h41m41s to 11h42m51s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h41m41s   !NEXT!        
qual=  0
disk=on
stop=11h42m51s   !NEXT!

!* --- Scan from 11h42m51s to 11h46m25s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=11h46m25s   !NEXT!

!* --- Scan from 11h46m25s to 11h47m45s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=11h47m45s   !NEXT!

!* --- Scan from 11h47m45s to 11h51m19s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=11h51m19s   !NEXT!

!* --- Scan from 11h51m29s to 11h52m39s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=11h51m29s   !NEXT!        
qual=  0
disk=on
stop=11h52m39s   !NEXT!

!* --- Scan from 11h52m39s to 11h56m13s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=11h56m13s   !NEXT!

!* --- Scan from 11h56m13s to 11h57m33s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=11h57m33s   !NEXT!

!* --- Scan from 11h57m33s to 12h01m07s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h01m07s   !NEXT!

!* --- Scan from 12h01m07s to 12h02m26s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h02m26s   !NEXT!

!* --- Scan from 12h02m26s to 12h06m01s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h06m01s   !NEXT!

!* --- Scan from 12h06m01s to 12h07m20s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h07m20s   !NEXT!

!* --- Scan from 12h07m20s to 12h10m55s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h10m55s   !NEXT!

!* --- Scan from 12h10m55s to 12h12m14s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h12m14s   !NEXT!

!* --- Scan from 12h12m14s to 12h15m48s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h15m48s   !NEXT!

!* --- Scan from 12h15m48s to 12h17m08s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h17m08s   !NEXT!

!* --- Scan from 12h17m08s to 12h20m42s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h20m42s   !NEXT!

!* --- Scan from 12h20m42s to 12h22m02s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h22m02s   !NEXT!

!* --- Scan from 12h22m02s to 12h25m36s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h25m36s   !NEXT!

!* --- Scan from 12h25m36s to 12h26m56s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h26m56s   !NEXT!

!* --- Scan from 12h26m56s to 12h30m30s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h30m30s   !NEXT!

!* --- Scan from 12h30m30s to 12h31m49s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h31m49s   !NEXT!

!* --- Scan from 12h31m49s to 12h35m24s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h35m24s   !NEXT!

!* --- Scan from 12h35m24s to 12h36m43s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h36m43s   !NEXT!

!* --- Scan from 12h36m43s to 12h40m18s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h40m18s   !NEXT!

!* --- Scan from 12h40m18s to 12h41m37s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h41m37s   !NEXT!

!* --- Scan from 12h41m37s to 12h45m11s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=12h45m11s   !NEXT!

!* --- Scan from 12h45m21s to 12h46m31s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=12h45m21s   !NEXT!        
qual=  0
disk=on
stop=12h46m31s   !NEXT!

!* --- Scan from 12h46m31s to 12h51m38s   Sun, 2010 Dec 26 --- *!
sname='J0926+4029'  ra=09h26m00.426850s  dec= 40d29'49.67273"  qual=  0  calib='V'
disk=on
stop=12h51m38s   !NEXT!

!* --- Scan from 12h51m38s to 12h52m56s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=12h52m56s   !NEXT!

!* --- Scan from 12h53m07s to 12h58m06s   Sun, 2010 Dec 26 --- *!
sname='4C39.25'  ra=09h27m03.013936s  dec= 39d02'20.85184"  qual=999  calib='V'
disk=off
stop=12h53m07s   !NEXT!        
qual=  0
disk=on
stop=12h58m06s   !NEXT!

!* --- Scan from 12h58m16s to 12h59m26s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=12h58m16s   !NEXT!        
qual=  0
disk=on
stop=12h59m26s   !NEXT!

!* --- Scan from 12h59m26s to 13h03m00s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h03m00s   !NEXT!

!* --- Scan from 13h03m00s to 13h04m20s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h04m20s   !NEXT!

!* --- Scan from 13h04m20s to 13h07m54s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h07m54s   !NEXT!

!* --- Scan from 13h07m54s to 13h09m14s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h09m14s   !NEXT!

!* --- Scan from 13h09m14s to 13h12m48s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h12m48s   !NEXT!

!* --- Scan from 13h12m48s to 13h14m07s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h14m07s   !NEXT!

!* --- Scan from 13h14m07s to 13h17m42s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h17m42s   !NEXT!

!* --- Scan from 13h17m42s to 13h19m01s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h19m01s   !NEXT!

!* --- Scan from 13h19m01s to 13h22m36s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h22m36s   !NEXT!

!* --- Scan from 13h22m36s to 13h23m55s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h23m55s   !NEXT!

!* --- Scan from 13h23m55s to 13h27m29s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h27m29s   !NEXT!

!* --- Scan from 13h27m29s to 13h28m49s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h28m49s   !NEXT!

!* --- Scan from 13h28m49s to 13h32m23s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h32m23s   !NEXT!

!* --- Scan from 13h32m23s to 13h33m43s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h33m43s   !NEXT!

!* --- Scan from 13h33m43s to 13h37m17s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h37m17s   !NEXT!

!* --- Scan from 13h37m17s to 13h38m37s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h38m37s   !NEXT!

!* --- Scan from 13h38m37s to 13h42m11s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h42m11s   !NEXT!

!* --- Scan from 13h42m11s to 13h43m31s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h43m31s   !NEXT!

!* --- Scan from 13h43m31s to 13h47m05s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h47m05s   !NEXT!

!* --- Scan from 13h47m05s to 13h48m24s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h48m24s   !NEXT!

!* --- Scan from 13h48m24s to 13h51m59s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h51m59s   !NEXT!

!* --- Scan from 13h52m09s to 13h53m18s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=13h52m09s   !NEXT!        
qual=  0
disk=on
stop=13h53m18s   !NEXT!

!* --- Scan from 13h53m18s to 13h56m53s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=13h56m53s   !NEXT!

!* --- Scan from 13h56m53s to 13h58m12s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=13h58m12s   !NEXT!

!* --- Scan from 13h58m12s to 14h01m47s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=14h01m47s   !NEXT!

!* --- Scan from 14h01m47s to 14h03m06s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=14h03m06s   !NEXT!

!* --- Scan from 14h03m06s to 14h06m40s   Sun, 2010 Dec 26 --- *!
sname='IRASP09104'  ra=09h13m45.493000s  dec= 40d56'28.11400"  qual=  0  calib='N'
disk=on
stop=14h06m40s   !NEXT!

!* --- Scan from 14h06m40s to 14h08m00s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=  0  calib='V'
disk=on
stop=14h08m00s   !NEXT!

!* --- Scan from 14h11m34s to 14h12m54s   Sun, 2010 Dec 26 --- *!
sname='J0923+4125'  ra=09h23m31.304957s  dec= 41d25'27.43938"  qual=999  calib='V'
disk=off
stop=14h11m34s   !NEXT!        
qual=  0
disk=on
stop=14h12m54s   !NEXT!
disk=off
stop=14h12m59s   !NEXT!
     !QUIT! 
