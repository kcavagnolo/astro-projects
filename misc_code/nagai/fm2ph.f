      implicit none
c Simulation arrays
      integer nsize, nee, npmax, nemax
      parameter ( npmax = 1024, nemax = 218 )
      real aexpn, zobs, dx, ebin(0:nemax), fluxmap(npmax,npmax,nemax)

c Filename
      character simname*200, arfname*200, rmfname*200, outname*200

c Spectral paramaters
      real nH
      real absorp(nemax), effarea(nemax)
      real sigism

c RMF
      integer nsmax,nchmax, ns,nch
      parameter (nsmax=nemax,nchmax=2048)
      real eb1(nsmax),eb2(nsmax),rsp(nchmax,nsmax)
      double precision rsp8(nchmax,nsmax)
      integer channel(nchmax)
      integer ngrpmax, grpmax
      parameter (ngrpmax=100)
      integer ngrp(nsmax), F_chan(nsmax,ngrpmax), N_chan(nsmax,ngrpmax)
      real lo_thresh, areascal
      integer flchan, chatter
      character*80 hduclas3,rmfversn
      character*80 telescop, instrume, detnam, filter, chantype

c Photon arrays
      integer nphotmax
      parameter (nphotmax=50 000 000) ! 50 mln photons
      real xphot(nphotmax), yphot(nphotmax)
      integer piphot(nphotmax)
      real nphottot
      integer nphot

c FITSIO stuff
      integer status, unitarf, unitrmf,colnum, nrows, blocksize, bitpix
      integer naxis, gcount, pcount, naxes(10)
      logical anyf
      character matext*80, colname*80, filename*200

      integer tfields
      character ttype(100)*30, tunit(100)*30, tform(100)*30
      

c Misc
      integer unit, newunit
      integer ie, i,j
      real e, rcore, rcut, xx, yy, rr
      double precision totflux, sum
      double precision fluxpix(npmax,npmax)
      double precision prob(nemax), refvec(2*nemax), mean
      double precision refvecrmf(4096)
      double precision ran28


c Random numbers
      integer ranref8, ipoidev8
      integer isim,nsim,ibin


c  ==================================================================

      call get_parameter_value_s ('sim',simname)
      call get_parameter_value_s ('arf',arfname)
      call get_parameter_value_s ('rmf',rmfname)
      call get_parameter_value_s ('out',outname)
      call get_parameter_value ('nH',nH,'e')
      call get_parameter_value ('nphot',nphottot,'e')
      call get_parameter_value ('rcore',rcore,'e')

      if (nphot.gt.nphotmax/1.1) then
        call exiterror ('too many photons')
      endif

      unit = newunit()
      open (unit, file = simname, form='unformatted',status='old')
      read(unit) aexpn, zobs
      read(unit) nsize, dx, nee
      read(unit) ( ebin(ie), ie = 0, nee )
      read(unit) fluxmap
      close (unit)

c     pixel size from [comoving kpc] to [physical kpc]
      dx = dx * aexpn  

      write(*,*) 'reading the flux map...'
      write(*,*) '   aexpn = ',aexpn
      write(*,*) '   zobs  = ',zobs
      write(*,*) '   nsize = ',nsize
      write(*,*) '   pixel size = ',dx*1000,' [kpc physical]'

*      call write_fits_image ('gov',fluxmap(1,1,100),npmax,npmax,'e','e')

c Prepare absorption array
      do ie=1,nee
        e = (ebin(ie-1)+ebin(ie))/2*1000.0
        absorp(ie) = sigism(e)*nH
        if (absorp(ie).lt.50.0) then
          absorp(ie)=exp(-absorp(ie))
        else
          absorp(ie)=0.0
        endif
      enddo

c A) Read RMF
      status = 0
      call ftgiou (unitrmf,status)
      filename = rmfname
      call ftopen (unitrmf,filename,0,blocksize,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      chatter = 0
      
      matext = 'SPECRESP MATRIX'
      call ftmnhd (unitrmf,-1,matext,0,status)
      if (status.ne.0) then
        status = 0
        matext='MATRIX'
        call ftmnhd (unitrmf,-1,matext,0,status)
      endif
      if (status.ne.0) 
     ~    call exit_fitsio ('SPECRESP MATRIX, '//filename,status)
      
      call rdrmf3(unitrmf, chatter,matext,
     &    telescop, instrume, detnam, filter, areascal,
     &    chantype, flchan, 
     &    nch, ns, eb1, eb2,
     &    grpmax,ngrp,F_chan, N_chan,
     &    rsp,lo_thresh,nchmax,nsmax,
     &    rmfversn,hduclas3,status)
      if (status.ne.0) call exit_fitsio ('MATRIX, '//filename,status)
      if (ns.ne.nee) call exiterror
     ~    ('number of energy channels in RMF does not match simulations')
      
      call ftclos (unitrmf,status)
      call ftfiou (unitrmf,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      do i=1,nch
        do j=1,ns
          rsp8(i,j)=rsp(i,j)
        enddo
      enddo

c Load ARF
      status = 0
      call ftgiou (unitarf,status)
      filename=arfname
      call ftopen(unitarf,filename,0,blocksize,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      
      matext = 'SPECRESP'
      call ftmnhd (unitarf,-1,matext,0,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      
      colname='SPECRESP'
      call ftgcno (unitarf,.false.,colname,colnum,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      
      call ftgnrw(unitarf,nrows,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      if (nrows.ne.nee) call exiterror
     ~    ('number of energy channels in ARF does not match simulations')
      
      call ftgcve(unitarf,colnum,1,1,nee,0.0,effarea,anyf,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      
      call ftclos(unitarf,status)
      call ftfiou(unitarf,status)
      if (status.ne.0) call exit_fitsio (filename,status)
      
c    Go over simulation map, multiply by absorption and by effective area
c    and compute total flux
      print*,'computing total observed flux...'
      if ( rcore .gt. 0. ) then
        write(*,*) '   excluding the inner ',rcore,'kpc'
      endif
      do i=1,npmax
        do j=1,npmax
          sum = 0 
          do ie=1,nee
            fluxmap(i,j,ie)=max(fluxmap(i,j,ie)*absorp(ie)*effarea(ie),0.0)
            sum = sum + fluxmap(i,j,ie)
          enddo
          fluxpix(i,j) = sum
          xx=(i-npmax/2+ran28(-100))*dx*1000.0 ! kpc
          yy=(j-npmax/2+ran28(-100))*dx*1000.0 ! kpc
          rr=(xx**2+yy**2)**0.5 ! kpc
          if ( rr .gt. rcore ) then
            totflux = totflux + sum
          endif
        enddo
      enddo

      write(*,*) '   totflux= ',totflux,' [10^60 cnts cm^2 s^-1]'

c    Go over map again, simulate photons
      print*,'simulating photons ... '
      nphot = 0
      do i=1,npmax
        do j=1,npmax

c         Do not generate photons for pixels with r<10kpc. 
c         This will significant cut down the total number of photons and reduce 
c         the file size, by more than an order of magnitude in most cases (Daisuke, 04/12/05).c
c         Test results:
c         Nphot            All        r>10kpc
c         ===============================================================
c         CL5csf1       37,094,823   2,945,954  (for 10^6 outside 100kpc)
c         CL10csf1      31,991,613   3,292,999  (for 10^5 outside 100kpc)
c 
c         Note that CL10csf1 has crazy amount of photons in the center compraed to others.
c
          xx=(i-npmax/2)*dx*1000.0 ! kpc
          yy=(j-npmax/2)*dx*1000.0 ! kpc
          rr=(xx**2+yy**2)**0.5 ! kpc

          rcut = 10.0
          IF ( rr .ge. rcut ) THEN

c       First see how many photons we want from this bin
          mean = fluxpix(i,j)/totflux*nphottot
          nsim = ipoidev8(mean) ! Poisson random number

          if (nsim.gt.0) then
            sum = fluxpix(i,j)
c         renormalize spectrum
            do ie=1,nee
              prob(ie)=fluxmap(i,j,ie)/sum
            enddo

c         make reference vector
            call refvec8(prob,nee,1,.false.,refvec,2*nee,status)
            if (status.ne.0) then
              print*,status
              pause
            endif

            do isim = 1,nsim
              nphot = nphot + 1
              if (nphot.gt.nphotmax) then
                call exiterror ('photon limit exceeded')
                write(*,*) ' nphot= ',nphot
                write(*,*) ' nphotmax= ',nphotmax
              endif

              ibin = ranref8 (refvec,2*nee)
*              piphot(nphot)=int((ebin(ibin-1)+ran28(-100)*(ebin(ibin)-ebin(ibin-1)))*1000/14.6)+1
c         ibin now corresponds to the nominal photon energy
c         we must now scatter it with the RMF
              call refvec8(rsp8(1,ibin),nch,1,.false.,refvecrmf,4096,status)
              piphot(nphot) = ranref8(refvecrmf,4096)
              xphot(nphot)=i
              yphot(nphot)=j
            enddo
          endif

          ENDIF  ! if ( rr .ge. rcut )
        enddo
      enddo

      write(*,*) '   nphot= ',nphot

c Dump photon list in the fits format
      do i=1,nphot
        xphot(i)=(xphot(i)-npmax/2+ran28(-100))*dx*1000.0 ! kpc
        yphot(i)=(yphot(i)-npmax/2+ran28(-100))*dx*1000.0 ! kpc
      enddo

      status = 0
      call unlink (outname)
      call ftgiou (unit,status)
      call ftinit(unit,outname,0,status)
      if (status.ne.0) call exit_fitsio (outname,status)
      bitpix=8
      naxis=0
      call ftphpr(unit,.true.,bitpix,naxis,naxes,pcount,gcount,.true.,status)
      call ftpdef(unit,bitpix,naxis,naxes,pcount,gcount,status)
     
c   create events extension
      tfields = 3
      ttype(1) = 'X'
      ttype(2) = 'Y'
      ttype(3) = 'PI'
      tform(1) = 'e'
      tform(2) = 'e'
      tform(3) = 'j'
      tunit(1) = 'kpc'
      tunit(2) = 'kpc'
      tunit(3) = 'PI channels'

      call FTIBIN (unit,nphot,tfields,ttype,tform,tunit,'EVENTS',0,status)
      
      call ftpkye (unit,'TLMIN1',-dx*1000.0*npmax/2,3,'min X',status)
      call ftpkye (unit,'TLMIN2',-dx*1000.0*npmax/2,3,'min Y',status)
      call ftpkye (unit,'TLMAX1',dx*1000.0*npmax/2,3,'max X',status)
      call ftpkye (unit,'TLMAX2',dx*1000.0*npmax/2,3,'max Y',status)

      call ftpcle (unit,1,1,1,nphot,xphot,status)
      call ftpcle (unit,2,1,1,nphot,yphot,status)
      call ftpclj (unit,3,1,1,nphot,piphot,status)

      call ftclos(unit,status)
      if (status.ne.0) call exit_fitsio (outname,status)
      call ftfiou (unit,status)

      call exit(0)

      end

      
