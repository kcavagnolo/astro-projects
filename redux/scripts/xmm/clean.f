c       program clean
c       this program is a hacked version of snowden's software
c       for cleaning lightcurves. 


c        implicit none
c        integer i, status, luna, err, block, hdutype, nchan
c        integer tfields
c        character*20 lc_file1, extname, extn
c        character*8 tform(10), ttype(10), tunit(10)

        integer*4 bin, block, clobber, delt, err, hdutype, high,  
     +      i, ii, low, lun, luna, lunb, flag
     +      maxp, maxr, mod, n, nbins1, nbins2,  
     +      nchan, nn, np, pflg, smoo, status, tfields, varidat
        real*4 ap(10), chan(1000), d1, d2, dt, error1(500000), 
     +      error2(500000), fit(1000), hist(1000), hist1(1000), 
     +      hist_cnts(1000), th, 
     +      rate1(500000), rate2(500000), 
     +      mod_rate1(500000), mod_rate2(500000), 
     +      pmod_rate1(500000), pmod_rate2(500000), 
     +      ratelimm, ratelimp, ratelim, rhigh, rlow, sca, unc(1000), 
     +      x, y, ymax, ymaxf, ys
        real*8 tend(500000), tend1(500000), tend2(500000), 
     +      time1(500000), time2(500000), tstart(50000), 
     +      tstart1(500000), tstart2(500000),tmp 
        real rfttime(500000), rftrate(500000)
        real x1(2), y1(2), rfterr(500000), xmax
        real centre, sigerr, ratelimm2, ratelimp2m, hmax
        real xvals(500000), yvals(500000), zvals(500000)
        character*8 tform(10), ttype(10), tunit(10)
        character*20 extn, extname, lc_file1, lc_file2
        logical anyf, extend, simple

        data ap /0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
        data clobber /1/
        data d1, d2 /-1.0, 0.0/
        data delt /12.5/
        data err, status /0, 0/
        data lc_file1 /'rate.fits'/
        data extn /'RATE'/
        data status /0/
        data smoo /20/
        data mod /2/
        data pflg /1/
        data sca /3.0/

c       read in bin file
        call pgopen
        open(unit=4, file='bin.txt', status='old')
        read(4,*) bin

c
        call ftgiou(luna,status)
        if((status .ne. 0) .and. (err .eq. 0)) then
            err = 1
            print *, 'STATUS FTGIOU =', status
            print *, 'CLEAN_REL POSITION 1'
            stop
        endif        

c  Open and read in the first light curve
c
        call ftopen (luna,lc_file1,0,block,status)
        if((status .ne. 0) .and. (err .eq. 0)) then
            err = 1
            print *, 'STATUS FTOPEN =', status
            print *, 'CLEAN_REL POSITION 2'
            print *, 'Light curve does not exist'
            print *, lc_file1
            stop
        endif
        extname = 'dummy'
        do while (extn .ne. extname)
            call ftmrhd (luna,1,hdutype,status)
            if((status .ne. 0) .and. (err .eq. 0)) then
                err = 1
                print *, 'STATUS FTMRHD =', status
                print *, 'CLEAN_REL POSITION 3'
                print *, 'Light curve count rate ext does not exist'
                print *, lc_file1
                stop
            endif
c
c  Check to see if we have the rate extension
c
            call ftghbn (luna,10,nchan,tfields,ttype,tform,
     +          tunit,extname,varidat,status)
            if((status .ne. 0) .and. (err .eq. 0)) then
                err = 1
                print *, 'STATUS FTGHBN =', status
                print *, 'CLEAN_REL POSITION 4'
                print *, 'Could not read extension info'
                print *, lc_file1
            endif
        enddo
c
c  Read the column data
c
        call ftgcve(luna,1,1,1,nchan,0.0,rate1,anyf,status)
        call ftgcve(luna,2,1,1,nchan,0.0,error1,anyf,status)
        call ftgcvd(luna,3,1,1,nchan,0.0,time1,anyf,status)
        if((status .ne. 0) .and. (err .eq. 0)) then
            err = 1
            print *, 'STATUS FTGCVE =', status
            print *, 'CLEAN_REL POSITION 5'
            print *, 'Could not write the column information'
            print *, lc_file1
        endif

c******************** End of reading on the data ****************

c******************** Plotting rate curve ***********************

        xmax=time1(nchan)-time1(1)
c       rate curve of data. The x and y arrays are 
c       time1 and rate1. I have converted these into
c       better time coordinates and real values for plotting. 
c        goto 100
        call pgbegin(0,'/NULL',1,1)
c        call pgbegin(0,'t1.ps/PS',1,1)
        call pgenv(0.,xmax,0.0,1.0,0,0)
        call pglabel('Time','Rate',
     &  ' ')
        tmp=time1(1)
        do i=1,nchan
         rfttime(i)=time1(i)-tmp
         rftrate(i)=rate1(i)
         rfterr(i)=error1(i)
        call pgpt1(rfttime(i),rftrate(i),1)
        call pgerry(1,rfttime(i),rftrate(i)-rfterr(i),rftrate(i)
     +  +rfterr(i),1)
        x1(1)=rfttime(i-1)
        x1(2)=rfttime(i)
        y1(1)=rftrate(i-1)
        y1(2)=rftrate(i)
        call pgline(2,x1,y1)
        enddo
        call pgend
 100    continue ! i.e. don't plot the rate curve to the screen. 

c*********************** Binning Rate curve ***********************

        call binit(time1,rate1,bin,nchan,
     +          nbins1,pmod_rate1,tstart1,tend1)
        nbins1 = nbins1 - 1
        do while (tend1(nbins1) .le. tstart1(nbins1))
            nbins1 = nbins1 - 1
        enddo
        n = nbins1

c       investigate the binit subroutine. 
c       new rate curve of data. The x and y arrays are 
c       tstart1 and pmod_rate1. I have converted these into
c       better time coordinates and real values for plotting.
c       Note that the time bins have a lower and upper value now.  
c        goto 101
        call pgbegin(0,'/NULL',1,1)
c        call pgbegin(0,'t2.ps/PS',1,1)
        call pgenv(0.,xmax,0.0,1.0,0,0)
        call pglabel('Time','Rate',
     &  ' ')
        tmp=tstart1(1)
        do i=1,nchan
         rfttime(i)=tstart1(i)-tmp
         rftrate(i)=pmod_rate1(i)
        call pgpt1(rfttime(i),rftrate(i),1)
        x1(1)=rfttime(i-1)
        x1(2)=rfttime(i)
        y1(1)=rftrate(i-1)
        y1(2)=rftrate(i)
        if (rfttime(i).ge.0) then
        call pgline(2,x1,y1)
        endif
        enddo
        call pgend
c 101    continue       ! i.e. don't plot the rate curve to the screen. 



c************************* Smoothing rate curve *********************

c       Smooth the data
        call smooth(rate1,smoo,nchan,mod_rate1)

c       Investigating the smoothing of the data. 
c       It appears that this routine uses the raw rate data (rate1)
c       not the binned data (pmod_rate1)
c       The output is mod_rate1. Plotting 

c        goto 102
        call pgbegin(0,'/NULL',1,1)
c        call pgbegin(0,'t3.ps/PS',1,1)       
        call pgenv(0.,xmax,0.0,1.0,0,0)
        call pglabel('Time','Rate',
     &  ' ')
        tmp=time1(1)
        do i=1,nchan
         rfttime(i)=time1(i)-tmp
         rftrate(i)=mod_rate1(i)
        call pgpt1(rfttime(i),rftrate(i),1)
        x1(1)=rfttime(i-1)
        x1(2)=rfttime(i)
        y1(1)=rftrate(i-1)
        y1(2)=rftrate(i)
        call pgline(2,x1,y1)
        enddo
        call pgend
c 102    continue    ! i.e. don't plot the smoothed rate curve to the screen. 

c       This output is pretty heavily smoothed, 

c************************* Histogramming the data *************** 

c       Make the histogram, use the smoothed data
c
        do i=1,1000
            hist_cnts(i) = 0.0
        enddo
c
        do i=1,nchan
            nn = 50.0*mod_rate1(i) + 1
            if(nn .le. 1000) 
     +              hist_cnts(nn) = hist_cnts(nn) + 1.0
        enddo

c        goto 103
        call pgbegin(0,'/NULL',1,1)
c        call pgbegin(0,'t4.ps/PS',1,1)
        call pgenv(0.,1.,0.0,800.,0,0)
        call pglabel('Count rate','N',
     &  ' ')
        do i=1,1000
         rfttime(i)=hist_cnts(i)
        call pgpt1(i*1.0/50.,rfttime(i),5)
        enddo
        call pgend
c 103    continue   ! i.e. don't plot the histogram to the screen. 


c***************** Find the peak of the distribution **************

c       Find the peak of the distribution
        maxr = 0.0
        maxp = 0
        do i=10,990
            th = 0.0
            do ii =i-2,i+2
                th = th + hist_cnts(ii)
            enddo
            th = th/5.0
            fit(i) = th
            if (th .ge. maxr) then
                    maxr = th
                    maxp = i
c                endif
            endif
        enddo
        do i=10,990
            hist_cnts(i) = fit(i)
        enddo
c        write(*,*) maxr, maxp !=140,10? 
c        max is 140 because the first few and last few values
c        in the array are omitted

c  Copy the data over to the fit arrays
c
        print *, maxp
        delt = sqrt(float(maxp))*delt/5.0
        low = maxp - delt
        if(low .lt. 1) low = 1
        high = maxp + delt
        if(high .gt. 1000) high = 1000
        nn = 0
        do i=low,high
            nn = nn + 1
            chan(nn) = i
            hist(nn) = hist_cnts(i)
            unc(nn) = sqrt(hist_cnts(i))
            if(unc(nn) .lt. 1) unc(nn) = 1
        enddo
        rlow = low/50.0
        rhigh = high/50.0
        np = 3
        ap(1) = maxr                   ! max N
        ap(2) = 1.0/delt               ! max count rate
        ap(3) = maxp                   ! i value in array
        print *, 'Initial Guess: ', (ap(i),i=1,3), nn, np

c************************ Fit Gaussian **********************

        call gauss_fit(chan,hist,unc,nn,np,ap)
        print *, 'Final Values: ', (ap(i),i=1,3)

c  Write out a QDP plot file of the fit
c
        if(pflg .eq. 1) then
            call ftgiou(lun,status)
            if((status .ne. 0) .and. (err .eq. 0)) then
                err = 1
                print *, 'STATUS FTGIOU =', status
                print *, 'CLEAN_REL POSITION 10'
                stop
            endif
c
            if(clobber .eq. 1) then
                open(unit=lun,status='unknown',form='formatted',
     +                  file='hist.qdp')
            else
                open(unit=lun,status='new',form='formatted',
     +                  file='hist.qdp')
            endif
            write(lun,*) 'font roman'
            write(lun,*) 'cs 1.0'
            write(lun,*) 'skip sing'
            write(lun,*) 'gap errors'
            write(lun,*) 'col 1 on 1'
            write(lun,*) 'col 3 on 2'
            write(lun,*) 'col 1 on 3'
            write(lun,*) 'col 3 on 4'
            write(lun,*) 'col 1 on 5'
            write(lun,*) 'col 3 on 6'
            write(lun,*) 'col 2 on 7'
            write(lun,*) 'col 4 on 8'
            write(lun,*) 'line step 2'
            write(lun,*) 'read serr 2'
            write(lun,*) 'read serr 4'
            write(lun,*) 'read serr 5'
            write(lun,*) 'read serr 6'
            write(lun,*) 'read serr 7'
            write(lun,*) 'win 1'
            write(lun,*) 'yplot 1 2'
            write(lun,*) 'loc 0.03 0.72 0.97 1.0'
            write(lun,*) 'lab x Count Rate (counts/s)'
            write(lun,*) 'lab f'
            write(lun,*) 'la y N'
            write(lun,*) 'r x1 0 5.0'
c
            ymax = 0.0
            do i=1,1000
                y = ap(1)*exp(-ap(2)*(i - ap(3))*(i - ap(3)))
                if(hist_cnts(i) + 1.1*sqrt(hist_cnts(i)) .gt. ymax) 
     +                  ymax = hist_cnts(i) + 1.1*sqrt(hist_cnts(i))
            enddo
            ymax = 1.1*ymax
            ymaxf = ymax
            write(lun,*) 'r y1 0 ', ymax
            write(lun,*) 'la 1 p  2.5 ', 0.9*ymax,
     +              ' "Count Rate Histogram"'
            if(ap(3) .lt. 120.0) then
                write(lun,*) 'la 11 p  4.8 ', 0.9*ymax,
     +                  ' cs 0.8 just right "Fit Limits: Blue"'
                write(lun,*) 'la 12 p  4.8 ', 0.8*ymax,
     +                  ' cs 0.8 just right "Selection Limits: Red"'
            else
                write(lun,*) 'la 11 p  0.2 ', 0.9*ymax,
     +                  ' cs 0.8 just left "Fit Limits: Blue"'
                write(lun,*) 'la 12 p  0.2 ', 0.8*ymax,
     +                  ' cs 0.8 just left "Selection Limits: Red"'
            endif
c
c  Write out the control lines for the plot of the FOV light curve
c
            write(lun,*) 'win 2'
            write(lun,*) 'yplot 3 4'
            write(lun,*) 'loc 0.03 0.39 0.97 0.67'
            write(lun,*) 'la x Time (s)'
            write(lun,*) 'la y Count Rate (counts/s)'
            write(lun,*) 'r x2 0 ', tend1(nbins1)-tstart1(1)
            ymax = 0.0
            do i=1,nbins1
                dt = tend1(i) - tstart1(i)
                if(pmod_rate1(i) + 
     +                  sqrt(pmod_rate1(i)*dt)/dt .gt. ymax) then
                    ymax = pmod_rate1(i) + sqrt(pmod_rate1(i)*dt)/dt
                endif
            enddo
            write(lun,*) 'r y2 0.1 ', ymax
            write(lun,*) 'la 2 p ', 0.5*(tend1(nbins1)-tstart1(1)), 
     +              0.5*ymax,' "FOV Light Curve"'
            write(lun,*) 'log y'

            do i=1,1000
                x = 0.02*i - 0.01
                y = ap(1)*exp(-ap(2)*(i - ap(3))*(i - ap(3)))
                write(lun,2000) x, hist_cnts(i), sqrt(hist_cnts(i)), 
     +              y, d1, d2, d1, d2, d1, d2, d1, d2, d1, d1
 2000           format(1x,f12.3,3f10.3,10f5.1)
            enddo
        endif


c  Set up the good time intervals
c
        ratelimm = 0.02*(ap(3) - sca/sqrt(ap(2)))
        ratelimp = 0.02*(ap(3) + sca/sqrt(ap(2)))
        centre= 0.02*ap(3)
        sigerr=0.02/sqrt(ap(2))
c        write(*,*) centre, sigerr
c        write(*,*) ratelimm, ratelimp
        nn = 0
        do i=1,nbins1
            if((pmod_rate1(i) .gt. 0.0) .and. 
     +              (pmod_rate1(i) .le. ratelimp) .and. 
     +              (pmod_rate1(i) .ge. ratelimm)) then
                nn = nn + 1
                tstart(nn) = tstart1(i)
                tend(nn) = tend1(i)
            endif
        enddo

c******************** Plotting the rate histogram *****************
        xmax=0.0
        do i=1,1000
           if ( hist_cnts(i).gt.0.0 ) then
            x = 0.02*i - 0.01             
             if ( x.gt.xmax ) then
                xmax=x
             endif
           endif
        enddo
        xmax=1.1*xmax
c        call pgbegin(0,'/XW',1,1)
        call pgbegin(0,'histo.eps/CPS',1,1)
        call pgenv(0.,xmax,0.0,ymaxf,0,0)
        call pglabel('Count rate (counts/s)','Number',
     &  ' ')
        do i=1,1000
          x = 0.02*i - 0.01
          y = ap(1)*exp(-ap(2)*(i - ap(3))*(i - ap(3)))
          xvals(i)=x             ! count rate
          yvals(i)=hist_cnts(i)  ! number
          zvals(i)=y             ! gaussian data
          call pgsch(1.5)
          call pgpt1(xvals(i),yvals(i),5)
          call pgsch(1.)
          call pgsci(2)
          call pgpt1(xvals(i),zvals(i),1)
           x1(1)=xvals(i-1)
           x1(2)=xvals(i)
           y1(1)=zvals(i-1)
           y1(2)=zvals(i)
           call pgslw(4)
           call pgline(2,x1,y1)
          call pgsci(1)
        enddo
           call pgsci(3) ! 1.5 sigma lines
           x1(1)=ratelimm
           x1(2)=ratelimm
           y1(1)=0.0
           y1(2)=ymaxf
c          call pgline(2,x1,y1)        
           x1(1)=ratelimp
           x1(2)=ratelimp
           y1(1)=0.0
           y1(2)=ymaxf
c          call pgline(2,x1,y1) 
           call pgsci(5) ! 3 sigma lines
           x1(1)=centre+sca*sigerr
           x1(2)=centre+sca*sigerr
           y1(1)=0.0
           y1(2)=ymaxf
          call pgline(2,x1,y1)        
 
        call pgend

c       So x,y are the Gaussian data and hist_vals are the real data


c
c  If the intervals are close then run them together
c
        n = 1
        do i=2,nn
            if(abs(tstart(i) - tend(i-1)) .gt. 5.0) then
                tend(n) = tend(i-1)
                n = n + 1
                tstart(n) = tstart(i)
            endif
        enddo
        tend(n) = tend(nn)
c
c  Eliminate the short time intervals
c
        nn = 0
        do i=1,n
            if(tend(i)-tstart(i) .gt. 100.0) then
                nn = nn + 1
                tstart(nn) = tstart(i)
                tend(nn) = tend(i)
            endif
        enddo
c
c  Write out a count rate plot file
c
        if(pflg .eq. 1) then
            do i=1,nbins1
                x = (tstart1(i) + tend1(i))/2.0
                dt = tend1(i) - tstart1(i)
                y = pmod_rate1(i)
                ys = sqrt(pmod_rate1(i)*dt)/dt
                write(lun,2010) x-tstart1(1), d1, d2, d1, 
     +                  y, ys, d1, d2, d1, d2, d1, d2, d1, d1
 2010           format(1x,f12.3,3f5.1,2f10.3,8f5.1)
            enddo
            do i=1,nbins1
                x = (tstart1(i) + tend1(i))/2.0
                do ii=1,nn
                    if((x .ge. tstart(ii)) .and. 
     +                      (x .le. tend(ii))) then
                        dt = tend1(i) - tstart1(i)
                        y = pmod_rate1(i)
                        ys = sqrt(pmod_rate1(i)*dt)/dt
                        write(lun,2020) x-tstart1(1), d1, d2, d1, 
     +                          d1, d2, y, ys, d1, d2, d1, d2, d1, d1
 2020                   format(1x,f12.3,5f5.1,2f10.3,6f5.1)
                    endif
                enddo
            enddo

            write(lun,2050) ratelimm, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, d2, d1
            write(lun,2050) ratelimm, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, 1.05*ymaxf, d1
            write(lun,2050) ratelimp, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, 1.05*ymaxf, d1
            write(lun,2050) ratelimp, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, d2, d1
 2050       format(1x,f12.3,11f5.1,f10.3,f5.1)
            write(lun,2060) rlow, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, d1, d2
            write(lun,2060) rlow, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, d1, 1.05*ymaxf
            write(lun,2060) rhigh, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, d1, 1.05*ymaxf
            write(lun,2060) rhigh, d1, d2, d1, 
     +              d1, d2, d1, d2, d1, d2, d1, d2, d1, d2
            close(lun)
 2060       format(1x,f12.3,12f5.1,f10.3)
        endif



c******************** Plotting the cleaned lightcurve **************
c       tstart and tend define the different lightcurves based on 
c       whether the data has been cleaned or not. 

        call pgbegin(0,'/NULL',1,1)
c        call pgbegin(0,'t6.ps/CPS',1,1)
        call pgenv(0.,10000.,0.0,ymax,0,0)
        call pglabel('Time(s)','Count Rate (counts/s)',
     &  ' ')

        do i=1,nbins1
             x = (tstart1(i) + tend1(i))/2.0
             dt = tend1(i) - tstart1(i)
             y = pmod_rate1(i)
             ys = sqrt(pmod_rate1(i)*dt)/dt
             zvals(i)=y
        enddo

        do i=1,nbins1
           x = (tstart1(i) + tend1(i))/2.0
              do ii=1,nn
                    if((x .ge. tstart(ii)) .and. 
     +                      (x .le. tend(ii))) then
                      dt = tend1(i) - tstart1(i)
                      y = pmod_rate1(i)
                      ys = sqrt(pmod_rate1(i)*dt)/dt
                    endif
              enddo
          xvals(i)=x-tstart1(1)
          yvals(i)=y
        enddo

        do i=1,nbins1
          call pgpt1(xvals(i),yvals(i),5)
          call pgsci(2)
          call pgpt1(xvals(i),zvals(i),1)
          call pgsci(1)
        enddo
          
        call pgend

c       Not convinced about the last plot, but it doesnt matter. ]
c       All I have to do is output the upper and lower count rate
c       limits to a text file and use them in my program. 

c       Final thing to do is to output the min and max count rates
c       to a text file. 

        open(unit=4,status='new',form='formatted', file='minmax.txt')        
        write(4,*) ratelimm, ratelimp, centre, sigerr

         end




c********************************************************************
c********************************************************************
c*                                                                  *
c*                                                                  *
c*                            Subroutines                           *
c*                                                                  *
c*                                                                  *
c********************************************************************
c********************************************************************

        subroutine binit(time,rate,bin,nchan,n,bin_rate,tstart,tend)
c
        integer*4 j1, j2, n, nchan, bin
        real*4 rate(nchan), bin_rate(nchan), t
        real*8 tend(nchan), time(nchan), tstart(nchan)
c
        t = time(1)
        c = rate(1)
        i = 2
        n = 1
        tstart(n) = time(1) - 0.5
        nn = 1
        do while (i .lt. nchan)
            do while ((time(i) - t .lt. bin) .and. (i .le. nchan))
                c = c + rate(i)
                nn = nn + 1
                i = i + 1
            enddo
            if(nn .gt. 0) then
                bin_rate(n) = c/nn
                tend(n) = time(i-1) + 0.5
                n = n + 1
            endif
            tstart(n) = time(i) - 0.5
            c = 0.0
            nn = 0
            t = time(i)
        enddo
c
        return
        end


        subroutine smooth (rate,smoo,nchan,smoo_rate)
c
        implicit none
c
        integer*4 i, ih, j, j1, j2, nchan, smoo
        real*4 n, rate(nchan), smoo_rate(nchan), t
c
        ih = smoo/2
        do i=1,nchan
            j1 = i - ih
            if(j1 .lt. 1) j1 = 1
            j2 = i + ih
            if(j2 .gt. nchan) j2 = nchan
            t = 0.0
            n = 0.0
            do j=j1,j2
                t = t + rate(j)
                n = n + 1.0
            enddo
            smoo_rate(i) = t/n
        enddo
c
        return
        end

        subroutine gauss_fit(tempx,tempy,tempu,n,np,ap)
c
        implicit none
c
        integer*4 ftype, np, nppo, n
        real*4 a(10), alamda, almlim, ap(10), chisq, deltch, 
     +      i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, it, 
     +      ochisq, olamda, t, tempx(n), tempy(n), tempu(n)
        real*8 alpha(10,10), covar(10,10)
        integer*4 i, ierr, iflag, iup, iuplim, lista(10), maxitr
        character*1 fix(10)
        character*15 pshort(10)
        character*80 pname(10)
c
        common /modpar/ i0, i1, i2, i3, i4, i5, i6, i7, i8, i9
c
        equivalence(a(1),i0)
c
        data fix/10*' '/
        data ftype /2/
        data pshort /'NORM', 'WIDTH', 'OFFSET', ' ', ' ', ' ', ' ', 
     +      ' ', ' ', ' '/
        data pname /'NORM', 'WIDTH', 'OFFSET', ' ', ' ', ' ', ' ', 
     +      ' ', ' ', ' '/
c      
        if(np .ne. 3) then
            print *, 'Wrong number of parameters for a Gaussian fit'
            stop
        endif
c
        do i=1,np
            lista(i) = i
            fix(i) = '*'
            a(i) = ap(i)
        enddo
        nppo = np
c
c  *********************************************************************
c  *                                                                   *
c  *                       Main Fit Begins Here                        *
c  *                                                                   *
c  *********************************************************************
c
        maxitr = 100
        almlim = 1.E3
        iuplim = 10
c
c        call report(-1,' Initial Model Value Settings:',a,fix)
c
        alamda = -1.0
        ochisq = 0.0
        iup = 0
        i = 1
        iflag = 0
c
        do while ((iflag .eq. 0) .and. (i .lt. maxitr))
            i = i + 1
            olamda = alamda
            call mqm (ftype,tempx,tempy,tempu,n,a,10,lista,nppo,
     +          covar,alpha,10,chisq,alamda,ierr)
c
            deltch = abs(ochisq - chisq)             
c
c  Check for various termination conditions
c
            if((alamda .lt. olamda) .and. (deltch .lt. 0.1))
     +              iflag = 1
            if(iflag .eq. 0) then
                if(alamda .gt. olamda) iup = iup + 1
                if(alamda .lt. olamda) iup = 0
                if(iup .gt. iuplim) iflag = 1
                if(alamda .gt. almlim) iup = iup + 1
c
c                write(*,*)
c                write(*,*) 'ITER =',i,'  ALAMDA =',alamda,
c     +                  '  M.L.E. =',chisq
c                call report(-1,'Intermediate step..',a,fix)
            endif
        enddo
c
c        write(*,*)
c        call report(-1,' Final Model Values: (*=Floating)',a,fix)
        write(*,1010) ' Np =',np,' N =',n,' M.L.E. =',chisq
 1010   format(/,1x,a,i2,a,i4,a,g12.4,a,g14.7)
c
c  Finish off the fit to get the covarience matrix
c
        alamda = 0.0
        call mqm (ftype,tempx,tempy,tempu,n,a,10,lista,nppo,
     +          covar,alpha,10,chisq,alamda,ierr)
c
c  Write out final values and one sigma uncertainties
c
        print *, 'Final parameter values and the formal standard errors'
        print *
c
        do i=1,nppo
            ap(i) = a(i)
            write(6,2100) pshort(lista(i)),a(lista(i)),
     +              sngl(dsqrt(dabs(covar(lista(i),lista(i)))))
 2100       format(1x,a15,f13.5,' +/-',f11.5)
        enddo
c
        return
        end


        SUBROUTINE MQM (FTYPE,TEMPX,TEMPY,TEMPU,NDATA,A,MA,
     +      LISTA,MFIT,COVAR,ALPHA,NCA,CHISQ,ALAMDA,IERR)
C
CC  Maximum likelyhood fitting routine
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        SAVE
C
        INTEGER*4 FTYPE, IERR, IERR1, IHIT, J, K, KK, MA, MFIT
        INTEGER*4 LISTA(MA), NCA, NDATA
C
        REAL*4 A(MA), ALAMDA, ATRY(10), CHISQ, OCHISQ, 
     +      TEMPX(NDATA), TEMPY(NDATA), TEMPU(NDATA)
C
        REAL*8 ALPHA(NCA,NCA), COVAR(NCA,NCA), BETA(10), DA(10)
C
        IERR = 0
C
C  Do some initializations
C
        IF(ALAMDA .LT. 0.) THEN
            KK = MFIT + 1
C
C  Check to see if LISTA contains a proper permutation of the coeffs.
C
            J = 0
            DO WHILE ((J .LT. MA) .AND. (IERR .EQ. 0))
                J = J + 1
                IHIT = 0
                DO 11 K=1,MFIT
                    IF(LISTA(K) .EQ. J) IHIT = IHIT + 1
   11           ENDDO
                IF(IHIT .EQ. 0) THEN
                    LISTA(KK) = J
                    KK = KK + 1
                ELSE IF (IHIT .GT. 1) THEN
                    IERR = 3
                    PRINT *, 'MQM: Improper permutation ',
     +                  'in LISTA, 1'
                ENDIF
            ENDDO
C
            IF(KK .NE. (MA+1)) THEN
                 IERR = 3
                 PRINT *, 'MQM: Improper permutation in LISTA, 2'
            ENDIF
C
            IF(IERR .EQ. 0) THEN
                ALAMDA = 0.001
                CALL MRQ (FTYPE,TEMPX,TEMPY,TEMPU,NDATA,A,MA,
     +              LISTA,MFIT,ALPHA,BETA,NCA,CHISQ,IERR1)
                IF(IERR1 .EQ. 3) THEN
                    IERR = 3
                    PRINT *, 'MQM ==> Bad return from MRQ #1'
                ELSE
                    OCHISQ = CHISQ
                    DO 13 J=1,MA
                        ATRY(J) = A(J)
   13               ENDDO
                ENDIF
            ENDIF
        ENDIF
C
C  Alter linearized fitting matrix by augmenting diagonal elements
C
        IF(IERR .EQ. 0) THEN
            DO 15 J=1,MFIT
                DO 14 K=1,MFIT
                    COVAR(J,K) = ALPHA(J,K)
   14           ENDDO
                COVAR(J,J) = ALPHA(J,J)*(1.0D0 + DBLE(ALAMDA))
                DA(J) = BETA(J)
   15       ENDDO
C
C  Get the matrix solution
C
            CALL DGAUSJ(COVAR,MFIT,NCA,DA,1,1,IERR1)
            IF(IERR1 .EQ. 3) THEN
                IERR = 3
                PRINT *, 'MQM ==> Bad return from DGAUSJ'
            ELSE
C
C  If converged evaluate covarience matrix with ALAMDA = 0
C
                IF(ALAMDA .EQ. 0.) THEN
                    CALL DCVSRT(COVAR,NCA,MA,LISTA,MFIT)
                ELSE
C
C  Check to see if the trial succeeded
C
                    DO 16 J=1,MFIT
                        ATRY(LISTA(J)) = A(LISTA(J)) + SNGL(DA(J))
   16               ENDDO
C
                    CALL MRQ (FTYPE,TEMPX,TEMPY,TEMPU,NDATA,
     +                  ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,CHISQ,
     +                  IERR1)
                    IF(IERR1 .EQ. 3) THEN
                        IERR = 3
                        PRINT *,'MQM ==> Bad return ',
     +                          'from MRQ #2'
                    ELSE
                        IF(CHISQ .LT. OCHISQ) THEN
C
C  Success, accept the new solution
C
                            ALAMDA = 0.1*ALAMDA
                            OCHISQ = CHISQ
                            DO 18 J=1,MFIT
                                DO 17 K=1,MFIT
                                    ALPHA(J,K) = COVAR(J,K)
   17                           ENDDO
                                BETA(J) = DA(J)
                                A(LISTA(J)) = ATRY(LISTA(J))
   18                       ENDDO
                        ELSE
C
C  Failure, increase ALAMDA and return
C
                            ALAMDA = 10.*ALAMDA
                            CHISQ = OCHISQ
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
C
        RETURN
        END



         SUBROUTINE MRQ (FTYPE,TEMPX,TEMPY,TEMPU,NDATA,A,MA,
     +       LISTA,MFIT,ALPHA,BETA,NALP,CHISQ,IERR)
C
CC  Computes matrix elements for MQM
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
C       PARAMETER MMAX=10
C
        INTEGER*4 FTYPE, I, IERR, J, K, KK, MA, MFIT, NALP, NDATA
        INTEGER*4 LISTA(MFIT)
C
        REAL*4 A(MA), CHISQ, DY, DYDA(10), D2YDA2(10,10), 
     +          TEMPX(NDATA), TEMPY(NDATA), SIG2I, TEMPU(NDATA), 
     +          WT, YMOD
C
        REAL*8 ALPHA(NALP,NALP), BETA(MA)
C
        IERR = 0
C
C  Initialize (symmetric) ALPHA and BETA
C
        DO 12 J=1,MFIT
            DO 11 K=1,J
                ALPHA(J,K) = 0.
   11       ENDDO
            BETA(J) = 0.
   12   ENDDO
C
        CHISQ = 0.0
C
C  Summation loop over all data
C
        I = 0
        DO WHILE ((I .LT. NDATA) .AND. (IERR .EQ. 0))
            I = I + 1
            IF(FTYPE .EQ. 1) THEN
                CALL PFUNCS(TEMPX(I),A,YMOD,DYDA,
     +                  D2YDA2,LISTA,MFIT,MA)
            ELSEIF(FTYPE .EQ. 2) THEN
                CALL GFUNCS(TEMPX(I),A,YMOD,DYDA,
     +                  D2YDA2,LISTA,MFIT,MA)
            ELSE
                PRINT *, 'Illegal model type'
            ENDIF
            IF(YMOD .LT. -1.0D200) THEN
                PRINT *,'MRQ: Illegal model value =', YMOD
                PRINT *,'               TEMPX(I) =', TEMPX(I)
                PRINT *,'               TEMPY(I) =', TEMPY(I)
                PRINT *,'               TEMPU(I) =', TEMPU(I)
                PRINT *,'                      A =',
     +                  (A(KK),KK=1,MA)
                IERR = 3
            ELSE
                SIG2I = 1./(TEMPU(I)*TEMPU(I))
                DY = TEMPY(I) - YMOD
C
                DO 14 J=1,MFIT
                    WT = SIG2I*DYDA(LISTA(J))
                    DO 13 K=1,J
                        ALPHA(J,K) = ALPHA(J,K) + WT*DYDA(LISTA(K))
   13               ENDDO
                    BETA(J) = BETA(J) + DY*WT
   14           ENDDO
                CHISQ = CHISQ + DY*DY*SIG2I
c                print *, tempx(i), tempy(i), ymod, chisq
            ENDIF
        ENDDO
C
        if(ierr .eq. 0) then
C
C  Fill in the symmetric side
C
            do j=2,mfit
                do k=1,j-1
                    alpha(k,j) = alpha(j,k)
                enddo
            enddo
        endif
c
        return
        end

        SUBROUTINE DGAUSJ(A,N,NP,B,M,MP,IERR)
C
CC  Numerical Routines linear equation solution routine

C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
C       PARAMETER NMAX=200
C
        INTEGER*4 I, ICOL, IERR, IROW, J, K, L, LL
        INTEGER*4 M, MP, N, NP
        INTEGER*4 IPIV(200), INDXR(200), INDXC(200)
C
        REAL*8 A(NP,NP), B(NP,MP), BIG, DUM, PIVINV
C
        IERR = 0
        DO 11 J=1,N
            IPIV(J) = 0
   11   ENDDO
C
C  Start the main loop over the columns to be reduced
C
        I = 0
        DO WHILE ((I .LT. N) .AND. (IERR .EQ. 0))
            I = I + 1
            BIG = 0.D0
C
C  Start the outer loop of the search for the pivot element
C
            J = 0
            DO WHILE ((J .LT. N) .AND. (IERR .EQ. 0))
                J = J + 1
                IF(IPIV(J) .NE. 1) THEN
                    K = 0
                    DO WHILE ((K .LT. N) .AND. (IERR .EQ. 0))
                        K = K + 1
                        IF (IPIV(K) .EQ. 0) THEN
                            IF (ABS(A(J,K)) .GE. BIG) THEN
                                BIG = DABS(A(J,K))
                                IROW = J
                                ICOL = K
                            ENDIF
                        ELSE IF (IPIV(K) .GT. 1) THEN
                            IERR = 3
                            PRINT *, 'DGAUSJ, singular matrix, pos 1'
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO
            IF(IERR .EQ. 0) THEN
                IPIV(ICOL) = IPIV(ICOL) + 1
C
C  We now have the pivot element, so we interchange rows, if needed,
C  to put the pivot element on the diagonal.  The columns are not
C  physically interchanged, only relabeled: INDX(I), the column of the
C  Ith pivot element, is the Ith column that is reduced, while INDXR(I)
C  is the row in which that pivot element was origially located.  If
C  INDXR(I) =/ INDXC(I) there is an implied column interchange.  With
C  this form of bookkeeping, the solution B's will end up in the correct
C  order, and the inverse matrix will be scrambled by columns.
C
                IF (IROW .NE. ICOL) THEN
                    DO 14 L=1,N
                        DUM = A(IROW,L)
                        A(IROW,L) = A(ICOL,L)
                        A(ICOL,L) = DUM
   14               ENDDO
C
                    DO 15 L=1,M
                        DUM = B(IROW,L)
                        B(IROW,L) = B(ICOL,L)
                        B(ICOL,L) = DUM
   15               ENDDO
                ENDIF
C
C  We are now ready to divide the pivot row by the pivot element
C  located at IROW and ICOL
C
                INDXR(I) = IROW
                INDXC(I) = ICOL
                IF (A(ICOL,ICOL) .EQ. 0.) THEN
                    IERR = 3
                    PRINT *, 'DGAUSJ, singular matrix, pos 2'
                ELSE
                    PIVINV = 1.D0/A(ICOL,ICOL)
                    A(ICOL,ICOL) = 1.D0
                    DO 16 L=1,N
                        A(ICOL,L) = A(ICOL,L)*PIVINV
   16               ENDDO
C
                    DO 17 L=1,M
                        B(ICOL,L) = B(ICOL,L)*PIVINV
   17               ENDDO
C
C  Next we reduce the rows, except for the pivot row
C
                    DO 21 LL=1,N
                        IF(LL .NE. ICOL)THEN
                            DUM = A(LL,ICOL)
                            A(LL,ICOL) = 0.
                            DO 18 L=1,N
                                A(LL,L) = A(LL,L) - A(ICOL,L)*DUM
   18                       ENDDO
                            DO 19 L=1,M
                                B(LL,L) = B(LL,L) - B(ICOL,L)*DUM
   19                       ENDDO
                        ENDIF
   21               ENDDO
                ENDIF
            ENDIF
   22   ENDDO
C
C  This is the end of the main loop over the columns of the reduction.
C  Now unscramble the solution in view of the column interchanges.
C  We do this by interchanging pairs of columns in the order that the
C  permutation was built up.
C
        IF(IERR .EQ. 0) THEN
            DO 24 L=N,1,-1
                IF(INDXR(L).NE.INDXC(L))THEN
                    DO 23 K=1,N
                        DUM = A(K,INDXR(L))
                        A(K,INDXR(L)) = A(K,INDXC(L))
                        A(K,INDXC(L)) = DUM
   23               ENDDO
                ENDIF
   24       ENDDO
        ENDIF
C
        RETURN
        END

        SUBROUTINE DCVSRT(COVAR,NCVM,MA,LISTA,MFIT)
C
CC  Repacks the covarience matrix for MQMLMN

C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER*4 I, J, MA, MFIT, LISTA(MFIT), NCVM
C
        REAL*8 COVAR(NCVM,NCVM), SWAP
C
C  Zero all elements below the diagonal
C
        DO 12 J=1,MA-1
            DO 11 I=J+1,MA
                COVAR(I,J) = 0.
   11       ENDDO
   12   ENDDO
C
C  Repack off-diagonal elements of fit into correct locations
C  below the diagonal
C
        DO 14 I=1,MFIT-1
            DO 13 J=I+1,MFIT
                IF(LISTA(J) .GT. LISTA(I)) THEN
                    COVAR(LISTA(J),LISTA(I)) = COVAR(I,J)
                ELSE
                    COVAR(LISTA(I),LISTA(J)) = COVAR(I,J)
                ENDIF
   13       ENDDO
   14   ENDDO
C
C  Temporarily store origial elements in top row, and zero the diagonal
C
        SWAP = COVAR(1,1)
C
        DO 15 J=1,MA
            COVAR(1,J) = COVAR(J,J)
            COVAR(J,J) = 0.
   15   ENDDO
C
        COVAR(LISTA(1),LISTA(1)) = SWAP
C
C  Now sort the elements into proper order on diagonal
C
        DO 16 J=2,MFIT
            COVAR(LISTA(J),LISTA(J)) = COVAR(1,J)
   16   ENDDO
C
C  Finally, fill in above the diagonal by symmetry
C
        DO 18 J=2,MA
            DO 17 I=1,J-1
                COVAR(I,J) = COVAR(J,I)
   17       ENDDO
   18   ENDDO
C
        RETURN
        END


        SUBROUTINE GFUNCS (TEMPX,ALOC,YMODEL,DYDA,DY2DA2,
     +      LISTA,MFIT,MA)
C
CC  Calculates Gaussian function distribution and partial derivatives

C***********************************************************************
C
        IMPLICIT NONE
C
        SAVE
C
        INTEGER*4 I, II, MFIT, LISTA(MFIT), MA
C
        REAL*4 ALOC(MA), DY2DA2(10,10), DYDA(10), THETA, 
     +      TEMPX, YMODEL
C
        REAL*8 A(10), CHAN, G, YMOD
C
        DO I=1,10
            A(I) = DBLE(ALOC(I))
        ENDDO
        CHAN = DBLE(TEMPX)
C
C  Compute the model value
C
        G   = exp(-A(2)*(CHAN - A(3))*(CHAN - A(3)))
        YMOD = A(1)*G
        YMODEL = SNGL(YMOD)
C
C  Compute values on intervals to prepare for computation
C  of numerical derivatives.
C
        DYDA(1) = G
        DYDA(2) = -A(1)*(CHAN - A(3))*(CHAN - A(3))*G
        DYDA(3) = 2.0*A(1)*A(2)*(CHAN - A(3))*G
C
        DY2DA2(1,1) = 0.0
        DY2DA2(1,2) = -(CHAN - A(3))*(CHAN - A(3))*G
        DY2DA2(1,3) = 2.0*A(2)*(CHAN - A(3))*G
        DY2DA2(2,1) = -(CHAN - A(3))*(CHAN - A(3))*G
        DY2DA2(2,2) = A(1)*(CHAN - A(3))*(CHAN - A(3))*
     +          (CHAN - A(3))*(CHAN - A(3))*G
        DY2DA2(2,3) = 2.0*A(1)*(CHAN - A(3))*
     +          (1.0 -  A(2)*(CHAN - A(3))*(CHAN - A(3)))*G
        DY2DA2(3,1) = 2.0*A(2)*(CHAN - A(3))*G
        DY2DA2(3,2) = 2.0*A(1)*(CHAN - A(3))*
     +          (1.0 -  A(2)*(CHAN - A(3))*(CHAN - A(3)))*G
        DY2DA2(3,3) = 2.0*A(1)*A(2)*
     +          (2.0*A(2)*(CHAN - A(3))*(CHAN - A(3)) - 1.0)*G
C
        RETURN
        END


        SUBROUTINE PFUNCS (TEMPX,ALOC,YMODEL,DYDA,DY2DA2,
     +      LISTA,MFIT,MA)
C
CC  Calculates polynomial function distribution and partial derivatives
C
        IMPLICIT NONE
C
        SAVE
C
        INTEGER*4 I, II, MFIT, LISTA(MFIT), MA
C
        REAL*4 ALOC(MA), DY2DA2(10,10), DYDA(10), THETA, 
     +      TEMPX, YMODEL
C
        REAL*8 A(10), DTIME, YMOD
C
        DO I=1,10
            A(I) = DBLE(ALOC(I))
        ENDDO
        DTIME = DBLE(TEMPX)
C
C  Compute the model value
C
        YMOD = A(1) + A(2)*DTIME + A(3)*DTIME*DTIME + 
     +      A(4)*DTIME*DTIME*DTIME + 
     +      A(5)*DTIME*DTIME*DTIME*DTIME + 
     +      A(6)*DTIME*DTIME*DTIME*DTIME*DTIME + 
     +      A(7)*DTIME*DTIME*DTIME*DTIME*DTIME*DTIME + 
     +      A(8)*DTIME*DTIME*DTIME*DTIME*DTIME*DTIME*DTIME
        YMODEL = SNGL(YMOD)
C
C  Compute values on intervals to prepare for computation
C  of numerical derivatives.
C
        DYDA(1) = 1
        DYDA(2) = DTIME
        DYDA(3) = DTIME*DTIME
        DYDA(4) = DTIME*DTIME*DTIME
        DYDA(5) = DTIME*DTIME*DTIME*DTIME
        DYDA(6) = DTIME*DTIME*DTIME*DTIME*DTIME
        DYDA(7) = DTIME*DTIME*DTIME*DTIME*DTIME*DTIME
        DYDA(8) = DTIME*DTIME*DTIME*DTIME*DTIME*DTIME*DTIME
        DYDA(9) = 0.0
        DYDA(10) = 0.0
C
        DO I=1,10
            DO II=1,10
                DY2DA2(I,II) = 0
            ENDDO
        ENDDO
C
        RETURN
        END



