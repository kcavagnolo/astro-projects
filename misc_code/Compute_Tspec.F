c
c     --------------------------------------------------------------
      subroutine Output_Xray_3DTZ_Profiles_New ( 
     &                            xc , yc , zc , rcore, Tmin, ammin,
     &                            rmin , rmax , nrbin , ibintype ,  
     &                            dvir, dcrit, ifindcenter, 
     &                            lpointsout, l_force_center,
     &                            fprofname, rs_fname, hname )
c     --------------------------------------------------------------
c
c     Last updated: Feb 17, 2005 (Daisuke)
c
c     What's New?  
c       Mazzota weighting -> Alexey's weighting to properly account
c       for the contribution from cold clumps.
c
c     Changes are in w4.
c
c     input:  
c
c      integer ibintype : 0 - logarithmic, 1 - even (now only log)
c                         
c      real    dvir  : virial overdensity (with respect to mean)
c              dcrit : virial overdensity with respect to the critical
c      integer ifindcenter (see below) 
c      logical lpointsout  - if .true, output sample points in points.dat
c      logical l_force_center = T -> use input (xc,yc,zc) as center
c                               F -> determine the center using 
c                                 max. gas density (ifindcenter=0)   
c                                 max. DM density ( =1)
c                                 max. total density ( =2)
c                                 min. grav. potential (=3)
c     character*256 fprofname - full path and file name to output profile to 
c                               it must end with a space
c
c      include 'a_tree.h'
c      include 'a_control.h'      
      include 'a_hfind.h'
c
      real xc , yc , zc , rcore, rmin , rmax 
      real dvir, dcrit, rcore_phys
      integer nrbin, ibintype, ifindcenter 
      logical lpointsout, l_force_center 
      character*256 fprofname, rs_fname, hname
c
      character*256 fname
      real norm_rs
      double precision mp, mpc_2_cm, lambda, Lx, norm_xsbp
      real t1i,t2i,t3i,t4i,t5i
      real z1i,z2i,z3i,z4i,z5i
      real w1i,w2i,w3i,w4i,w5i
      real Tmin
c 
      real*8 a2b, b2a
      real a2t, anow, tnow
      integer m
      parameter ( npoints = 1000000 )
      integer iMarkCell(npoints)
      real xpn(npoints), ypn(npoints), zpn(npoints)
      real pdm(npoints), pd(npoints), pe(npoints)
      real pzIa(npoints), pzII(npoints)
      real ptemp(npoints), pent(npoints)
      common / POINTS / xpn, ypn, zpn, pdm, pd, pe, pzIa, pzII, 
     +     ptemp, pent
      parameter ( nmaxbins = 1000 )
      real rbin(0:nmaxbins)
      real t1bin(0:nmaxbins), t1ibin(0:nmaxbins)
      real t2bin(0:nmaxbins), t2ibin(0:nmaxbins)
      real t3bin(0:nmaxbins), t3ibin(0:nmaxbins)
      real t4bin(0:nmaxbins), t4ibin(0:nmaxbins)
      real t5bin(0:nmaxbins), t5ibin(0:nmaxbins)

      real z1bin(0:nmaxbins), z1ibin(0:nmaxbins)
      real z2bin(0:nmaxbins), z2ibin(0:nmaxbins)
      real z3bin(0:nmaxbins), z3ibin(0:nmaxbins)
      real z4bin(0:nmaxbins), z4ibin(0:nmaxbins)
      real z5bin(0:nmaxbins), z5ibin(0:nmaxbins)

      real w1bin(0:nmaxbins), w1ibin(0:nmaxbins)
      real w2bin(0:nmaxbins), w2ibin(0:nmaxbins)
      real w3bin(0:nmaxbins), w3ibin(0:nmaxbins)
      real w4bin(0:nmaxbins), w4ibin(0:nmaxbins)
      real w5bin(0:nmaxbins), w5ibin(0:nmaxbins)

      ! Alexey's weighting schemes
      double precision Tcont(0:nmaxbins), TcontC(0:nmaxbins)
      double precision Tline(0:nmaxbins), TlineC(0:nmaxbins)
      double precision Tmean(0:nmaxbins), TmeanC(0:nmaxbins)
      double precision wcont(0:nmaxbins), wcontC(0:nmaxbins)
      double precision wline(0:nmaxbins), wlineC(0:nmaxbins)
      double precision fluxcont(0:nmaxbins), fluxcontC(0:nmaxbins)
      double precision fluxline(0:nmaxbins), fluxlineC(0:nmaxbins)
      
      double precision emean, fcont, fline, TT, xx1, xx2, xx3
      double precision ww1, ww2
      integer it, nss
      parameter ( nss = 10000 )
      double precision Tcalmin, dlogT
      double precision tcal(nss), emean_cal(nss), fcont_cal(nss),
     ~     fline_cal(nss)
      save ncal, Tcalmin, dlogT, tcal, emean_cal, fcont_cal, fline_cal
      double precision TcontCi, wcontCi, fluxcontCi, TlineCi, wlineCi

      ! marking cells
      integer icON, icOFF, imark
      real ammin, ammax, rcut, rt

c
      real ndmp(0:nmaxbins)
      real tdum(8), entdum(8)
      integer nbin(nmaxbins)
      integer Level, icell, iPack(8)
      real vel(8), velx(8), vely(8), velz(8)
      double precision xx, yy, zz, exbin(0:nmaxbins)
      integer iIP(8,3)                     ! set of pointers to the leftmost
                                           ! cells - to be used as kernels
      data iIP   / 1 , 3 , 1 , 5 , 1 , 3 , 1 , 0 ,
     &             3 , 5 , 5 , 0 , 3 , 0 , 0 , 0 ,
     &             5 , 0 , 0 , 0 , 0 , 0 , 0 , 0  /
      integer nfpn
c
      real KEV_2_K
      parameter ( KEV_2_K = 1.160485e+7 )
c
      rlmin = log10(rmin)
      rlmax = log10(rmax)
      drl = (rlmax - rlmin) / float(nrbin) 
      nbins = int((rlmax-rlmin)/drl) + 1

      a5 = aexpn**5
      a3 = aexpn**3
      a2 = aexpn**2
      a2i = 1.0 / a2
      a5i = 1.0 / a5
      gamma1 = gamma - 1.0 
        
      zsim = 1.0/aexpn - 1.0
      fy = (1.0-0.5*Y_p)*(1.0-Y_p)
      mp = 1.6726d-24 
      mpc_2_cm = mpc * 100000.0
      norm_rs = 10.**(-23)

c
c...  Read in the Raymond-Smith cooling function from file
c
      irs = 1    ! 0 = RS_det(T,Z), 1 = RS_cgs(T,Z), 2 = RS(T,Z=0.3Msun),
      if ( irs .eq. 2 ) then
        call read_rs()
      else
        Ynow = 0.2496
        zsim = 1.0/aexpn - 1.0
        if ( zsim .le. 1.0e-20 ) zsim = 0.0
        ! Normalization for XSB [erg s^-1 cm^-2 arcmin^-2]
        fy = (1.0 - 0.5 * Ynow) * (1.0 - Ynow)
        if ( irs .eq. 0 ) call Read_RSdet_Table ( 0, rs_fname )
        if ( irs .eq. 1 ) call Read_RSdet_Table ( 1, rs_fname )
        write(*,*) 'Finished reading the RS spectra...'
      endif

c
c...  Precompute the Mekeal spectrum grids for the Alexey's weighting scheme
c
      Tcalmin = 0.25
      Tcalmin = 0.086  ! precompute Mekal spectra for T>0.086keV
      dlogT = 0.05*log(2.0d0)
      write(*,*) 'Precomuting Mekal spectra'
      call mk_Tw_cal (Tcalmin,dlogT,tcal,emean_cal,fcont_cal,
     ~     fline_cal,ncal)   
c      do i = 1, ncal
c         write(*,*) i, tcal(i), emean_cal(i), fcont_cal(i), fline_cal(i)
c      enddo
      write(*,*) 'Finished pre-computing the Mekal spectra'


c
c...  read halo catalog
c
      write(*,*) ' Read_Halo_Catalog...'
      call Read_Halo_Catalog( hname )
        
c
c...  mark all hydro cells associated with subclumps in the main cluster
c    
      write(*,*) 'Mark cells in subhalos...'
c      ammin = 1.0e12  ! [1/h Msun]
      ammax = 1.0e15  ! [1/h Msun]
      rfact = r0 * 1.e3 * aexpn / hubble ! code -> proper kpc
      rcut  = 0. / rfact ! [kpc physical] in code units
      rt    = 2.0        ! in units of [truncation radius]
      call Mark_Halo_Cells( ammin, ammax, rcut, rt )
      
      icON  = 0
      icOFF = 0
      do ic = 1, nctot
         if ( ind(ic) .eq. nil ) then
            icOFF = icOFF + 1
         else
            icON = icON + 1
         endif
      enddo
      write(*,*) ' # of marked cells = ',icON
      write(*,*) ' # of unmarked cells = ',icOFF
      
      
c
c...  Get ready for profiles!
c
      call Get_MaxLevelNow ()

      if (  .not. l_force_center ) then 
         call Find_Halo_Center ( xc, yc, zc, ifindcenter, 0 )
      endif

      m = 28548535
      xmax = -1.e6
      ymax = xmax
      zmax = xmax
      xmin = 1.e6
      ymin = xmin
      zmin = xmin
c
c.... select random samples in the volume
c
      do i = 1 , npoints
        rr    = 1.2 * rmax * RANDd ( m )
        theta = pi2 * RANDd ( m ) - pi
        phi   = pi2 * RANDd ( m )
        xpn(i) = rr * cos(theta) * cos(phi) + xc
        ypn(i) = rr * cos(theta) * sin(phi) + yc
        zpn(i) = rr * sin(theta) + zc
        if ( xpn(i) .lt. xmin ) xmin = xpn(i)
        if ( xpn(i) .gt. xmax ) xmax = xpn(i)
        if ( ypn(i) .lt. ymin ) ymin = ypn(i)
	if ( ypn(i) .gt. ymax ) ymax = ypn(i)
        if ( zpn(i) .lt. zmin ) zmin = zpn(i)
        if ( zpn(i) .gt. zmax ) zmax = zpn(i)
      enddo
c
      do i = 1 , npoints
        xx  = xpn(i)
        yy  = ypn(i)
        zz  = zpn(i)
        if ( xx .lt. 1.0 ) then 
          xx = xx + ng
        else 
          if ( xx .gt. narr ) then 
            xx = xx - ng
          endif
        endif
        if ( yy .lt. 1.0 ) then 
         yy = yy + ng
        else 
          if ( yy .gt. narr ) then 
            yy = yy - ng
          endif
        endif
        if ( zz .lt. 1.0 ) then 
          zz = zz + ng
        else 
          if ( zz .gt. narr ) then 
            zz = zz - ng
          endif
        endif
        
        idcell = iFindCell ( MaxLevel , xx , yy , zz )
        iMarkCell(i) = ind(idcell)

c....   gas density
        pd(i) = hvar(1,idcell)

c....   internal energy
        pe(i) = hvar(8,idcell)

c....   metallicity
        pzIa(i) = hvar(izIa,idcell)
        pzII(i) = hvar(izII,idcell)


      enddo
c
c.... construct profiles
c
      do i = 0 , nmaxbins
        nbin(i)  = 0
        rbin(i)  = 0.

        t1bin(i) = 0.
        t2bin(i) = 0.
        t3bin(i) = 0.
        t4bin(i) = 0.
        t5bin(i) = 0.
        
        z1bin(i) = 0.
        z2bin(i) = 0.
        z3bin(i) = 0.
        z4bin(i) = 0.
        z5bin(i) = 0.
        
        w1bin(i) = 0.
        w2bin(i) = 0.
        w3bin(i) = 0.
        w4bin(i) = 0.
        w5bin(i) = 0.
        
        t1ibin(i) = 0.
        t2ibin(i) = 0.
        t3ibin(i) = 0.
        t4ibin(i) = 0.
        t5ibin(i) = 0.

        z1ibin(i) = 0.
        z2ibin(i) = 0.
        z3ibin(i) = 0.
        z4ibin(i) = 0.
        z5ibin(i) = 0.

        w1ibin(i) = 0.
        w2ibin(i) = 0.
        w3ibin(i) = 0.
        w4ibin(i) = 0.
        w5ibin(i) = 0.

        Tcont(i) = 0.
        Tline(i) = 0.
        Tmean(i) = 0.
        wcont(i) = 0.
        wline(i) = 0.
        fluxcont(i) = 0.
        fluxline(i) = 0.
        
        TcontC(i) = 0.
        TlineC(i) = 0.
        TmeanC(i) = 0.
        wcontC(i) = 0.
        wlineC(i) = 0.
        fluxcontC(i) = 0.
        fluxlineC(i) = 0.

      enddo

c
c.... compute X-ray temperature and metallicity
c
      if ( lpointsout ) then 
        open ( 12 , file = 'points.dat' )
      endif
      do ii = 1 , npoints
        rp = sqrt((xpn(ii)-xc)**2+(ypn(ii)-yc)**2+(zpn(ii)-zc)**2)
        ibin = max ( int((log10(rp) - rlmin)/drl) + 1 , 0 )

        IF ( iMarkCell(ii) .ne. 0 ) THEN

c...      compute temperature and metallicity of the cell
          if ( pd(ii) .gt. 0. ) then
             Tcell = gamma1 * pe(ii) / pd(ii)
             Tcell = T_0 * Tcell * a2i
             Zcell = pzIa(ii) + pzII(ii)
             Zcell = Zcell / 0.01989 / pd(ii)
          else
             write(*,*) 'bad gas density in cell', ii,pd(ii)
          endif
         
c...      compute wight function
  
          if ( irs .eq. 2 ) then
             call lambda_rs( Tcell, lambda )   
          else 
             Tlog = log10(Tcell)
             Zlog = log10(Zcell)
             lambda = RS(Tlog,Zlog)
          endif
          
          w1 = pd(ii)
          w2 = pd(ii)**2
          w3 = pd(ii)**2 * Tcell**0.5
          w4 = pd(ii)**2 / Tcell**0.75
          if ( lambda .gt. 0. ) then
             w5 = pd(ii) * lambda
          else
             w5 = 0.
          endif
  
  
c...      Alexey's weighting scheme
          TT = Tcell / KEV_2_K
          if ( TT .lt. Tcalmin ) then
             it = 1
             ww2 = 0
             !ww1 = 1
             ww1 = 0
          else if ( TT .gt. tcal(ncal) ) then
             it = ncal-1
             !ww2 = 1
             ww2 = 0
             ww1 = 0
          else
             it = int((log(TT)-log(Tcalmin))/dlogT)+1
             ww2 = (log(TT)-(log(Tcalmin)+(it-1)*dlogT))/dlogT
             ww1 = 1-ww2
          endif
          
          fcont = ww1*fcont_cal(it)+ww2*fcont_cal(it+1)
          fline = ww1*fline_cal(it)+ww2*fline_cal(it+1)
          emean = ww1*emean_cal(it)+ww2*emean_cal(it+1)
  
          fcont = fcont * pd(ii)**2
          fline = fline * pd(ii)**2 * Zcell
  
  
          if ( Tcell .le. Tmin ) then
             w1 = 0.
             w2 = 0.
             w3 = 0.
             w4 = 0.
             w5 = 0.
             fcont = 0.
             fline = 0.
          endif
          
c          write(*,*) TT, it, ww1, ww2, fcont, fline, emean
  
          if ( ibin .le. nmaxbins ) then    
            nbin(ibin)   = nbin(ibin) + 1
 	    rbin(ibin)   = rbin(ibin) + rp
  
c...        sum up values  & convert code units to physical units
            t1bin(ibin) = t1bin(ibin) + Tcell * w1
            t2bin(ibin) = t2bin(ibin) + Tcell * w2
            t3bin(ibin) = t3bin(ibin) + Tcell * w3
            t4bin(ibin) = t4bin(ibin) + Tcell * w4
            t5bin(ibin) = t5bin(ibin) + Tcell * w5
            
            z1bin(ibin) = z1bin(ibin) + Zcell * w1
            z2bin(ibin) = z2bin(ibin) + Zcell * w2
            z3bin(ibin) = z3bin(ibin) + Zcell * w3
            z4bin(ibin) = z4bin(ibin) + Zcell * w4
            z5bin(ibin) = z5bin(ibin) + Zcell * w5
  
            w1bin(ibin) = w1bin(ibin) + w1
            w2bin(ibin) = w2bin(ibin) + w2
            w3bin(ibin) = w3bin(ibin) + w3
            w4bin(ibin) = w4bin(ibin) + w4
            w5bin(ibin) = w5bin(ibin) + w5
  
            ! Alexey's weighting
            Tcont(ibin) = Tcont(ibin) + TT*fcont/TT**0.875
            wcont(ibin) = wcont(ibin) + fcont/TT**0.875
            fluxcont(ibin) = fluxcont(ibin) + fcont
            
            Tline(ibin) = Tline(ibin) + emean*fline
            wline(ibin) = wline(ibin) + fline
  
            if ( lpointsout ) then 
              write(12,*) rp*r0, pd(ii), Tcell, exbin(ii)
            endif
          endif

        ENDIF ! if ( iMarkCell(ii) .ne. 0 )
  
      enddo
      if ( lpointsout ) then       
        close ( 12 )
      endif

c
c.... compute Tew(<r) for r>150kpc physical (excluding the central galaxy)
c

      call Get_MaxLevelNow ()
      DO Level = MinLevel , MaxLevelNow
         CellVolume = 1.0 * 2.0**(-3.0*Level)
         IF ( Level .eq. MinLevel ) THEN
            write(*,*) ' working on Level = ',Level
            do ic = 1 , ncell0
              if ( iOctCh(ic) .eq. nil ) then
                call Ps ( ic , Posx , Posy , Posz )
                call PeriodicBoundary( xc, yc, zc, Posx, Posy, Posz )
                rp = sqrt( (Posx-xc)**2 +
     &                     (Posy-yc)**2 +
     &                     (Posz-zc)**2 )
                ibin  = max ( int((log10(rp) - rlmin)/drl) + 1 , 0 )
                imark = ind(ic)

                if ( hvar(1,ic) .gt. 0. ) then
                   Tcell = gamma1 * hvar(8,ic) / hvar(1,ic)
                   Tcell = T_0 * Tcell * a2i
                   Zcell = hvar(izIa,ic) + hvar(izII,ic)
                   Zcell = Zcell / 0.01989 / hvar(1,ic)
                else
                   write(*,*) 'bad gas density in cell',ic,hvar(1,c)
                endif

                if ( irs .eq. 2 ) then
                   call lambda_rs( Tcell, lambda )   
                else 
                   Tlog = log10(Tcell)
                   Zlog = log10(Zcell)
                   lambda = RS(Tlog,Zlog)
                endif
                
                rhog  = hvar(1,ic)
                rhog2 = rhog**2
                w1 = rhog
                w2 = rhog2 
                w3 = rhog2 * Tcell**0.5  
                w4 = rhog2 / Tcell**0.75 
                if ( lambda .gt. 0. ) then
                   w5 = rhog2 * lambda
                else
                   w5 = 0.
                endif

                w1 = w1 * CellVolume
                w2 = w2 * CellVolume
                w3 = w3 * CellVolume
                w4 = w4 * CellVolume
                w5 = w5 * CellVolume

c...            Alexey's weighting scheme
                TT = Tcell / KEV_2_K
                if ( TT .lt. Tcalmin ) then
                   it = 1
                   ww2 = 0
                   !ww1 = 1
                   ww1 = 0
                else if ( TT .gt. tcal(ncal) ) then
                   it = ncal - 1
                   !ww2 = 1
                   ww2 = 0
                   ww1 = 0   
                else
                   it = int((log(TT)-log(Tcalmin))/dlogT)+1
                   ww2 = (log(TT)-(log(Tcalmin)+(it-1)*dlogT))/dlogT
                   ww1 = 1-ww2
                endif
                
                fcont = ww1*fcont_cal(it)+ww2*fcont_cal(it+1)
                fline = ww1*fline_cal(it)+ww2*fline_cal(it+1)
                emean = ww1*emean_cal(it)+ww2*emean_cal(it+1)
        
                fcont = fcont * rhog2 * CellVolume
                fline = fline * rhog2 * Zcell * CellVolume
                
                ! No weighting for gas with Tcell<Tmin or imark != 0
                if ( Tcell .le. Tmin .or. imark .ne. nil ) then
                   w1 = 0.
                   w2 = 0.
                   w3 = 0.
                   w4 = 0.
                   w5 = 0.
                   fcont = 0.
                   fline = 0.
                endif

                if ( ibin .le. nmaxbins .and. rp .gt. rcore ) then
                  t1ibin(ibin) = t1ibin(ibin) + Tcell * w1 
                  t2ibin(ibin) = t2ibin(ibin) + Tcell * w2 
                  t3ibin(ibin) = t3ibin(ibin) + Tcell * w3 
                  t4ibin(ibin) = t4ibin(ibin) + Tcell * w4 
                  t5ibin(ibin) = t5ibin(ibin) + Tcell * w5 
                                                           
                  z1ibin(ibin) = z1ibin(ibin) + Zcell * w1 
                  z2ibin(ibin) = z2ibin(ibin) + Zcell * w2 
                  z3ibin(ibin) = z3ibin(ibin) + Zcell * w3 
                  z4ibin(ibin) = z4ibin(ibin) + Zcell * w4 
                  z5ibin(ibin) = z5ibin(ibin) + Zcell * w5 
                  
                  w1ibin(ibin) = w1ibin(ibin) + w1 
                  w2ibin(ibin) = w2ibin(ibin) + w2 
                  w3ibin(ibin) = w3ibin(ibin) + w3 
                  w4ibin(ibin) = w4ibin(ibin) + w4 
                  w5ibin(ibin) = w5ibin(ibin) + w5

                  ! Alexey's weighting
                  TcontC(ibin) = TcontC(ibin) + TT*fcont/TT**0.875
                  wcontC(ibin) = wcontC(ibin) + fcont/TT**0.875
                  fluxcontC(ibin) = fluxcontC(ibin) + fcont
                  
                  TlineC(ibin) = TlineC(ibin) + emean*fline
                  wlineC(ibin) = wlineC(ibin) + fline
                endif

              endif
            enddo
         ELSE
            write(*,*) ' working on Level = ',Level
            call Select_Cells ( Level , nLevel )
            do ic1 = 1 , nLevel
              icell = iSelect(ic1)
              do ic2 = 0 , 7
                ic = icell + ic2
                if ( iOctCh(ic) .eq. nil ) then
                  call Ps ( ic , Posx , Posy , Posz )
                  call PeriodicBoundary( xc, yc, zc, Posx, Posy, Posz )
                  rp = sqrt( (Posx-xc)**2 +
     &                       (Posy-yc)**2 +
     &                       (Posz-zc)**2 )
                  ibin  = max ( int((log10(rp) - rlmin)/drl) + 1 , 0 )
                  imark = ind(ic)

                  if ( hvar(1,ic) .gt. 0. ) then
                     Tcell = gamma1 * hvar(8,ic) / hvar(1,ic)
                     Tcell = T_0 * Tcell * a2i
                     Zcell = hvar(izIa,ic) + hvar(izII,ic)
                     Zcell = Zcell / 0.01989 / hvar(1,ic)
                  else
                     write(*,*) 'bad gas density in cell',ic,hvar(1,c)
                  endif
                  
                  if ( irs .eq. 2 ) then
                     call lambda_rs( Tcell, lambda )   
                  else 
                     Tlog = log10(Tcell)
                     Zlog = log10(Zcell)
                     lambda = RS(Tlog,Zlog)
                  endif
                
                  rhog  = hvar(1,ic)
                  rhog2 = rhog**2
                  w1 = rhog
                  w2 = rhog2 
                  w3 = rhog2 * Tcell**0.5  
                  w4 = rhog2 / Tcell**0.75 
                  if ( lambda .gt. 0. ) then
                     w5 = rhog2 * lambda
                  else
                     w5 = 0.
                  endif
                  
                  w1 = w1 * CellVolume
                  w2 = w2 * CellVolume
                  w3 = w3 * CellVolume
                  w4 = w4 * CellVolume
                  w5 = w5 * CellVolume

c...              Alexey's weighting scheme
                  TT = Tcell / KEV_2_K
                  if ( TT .lt. Tcalmin ) then
                     it = 1
                     ww2 = 0
                     !ww1 = 1
                     ww1 = 0
                  else if ( TT .gt. tcal(ncal) ) then
                     it = ncal - 1
                     !ww2 = 1
                     ww2 = 0
                     ww1 = 0
                  else
                     it = int((log(TT)-log(Tcalmin))/dlogT)+1
                     ww2 = (log(TT)-(log(Tcalmin)+(it-1)*dlogT))/dlogT
                     ww1 = 1-ww2
                  endif
                  
                  fcont = ww1*fcont_cal(it)+ww2*fcont_cal(it+1)
                  fline = ww1*fline_cal(it)+ww2*fline_cal(it+1)
                  emean = ww1*emean_cal(it)+ww2*emean_cal(it+1)
          
                  fcont = fcont * rhog2 * CellVolume
                  fline = fline * rhog2 * Zcell * CellVolume

                  if ( Tcell .le. Tmin .or. imark .ne. nil ) then
                     w1 = 0.
                     w2 = 0.
                     w3 = 0.
                     w4 = 0.
                     w5 = 0.
                     fcont = 0.  ! already accounted above though..
                     fline = 0.
                  endif

                  if ( ibin .le. nmaxbins .and. rp .gt. rcore ) then
                    t1ibin(ibin) = t1ibin(ibin) + Tcell * w1 
                    t2ibin(ibin) = t2ibin(ibin) + Tcell * w2 
                    t3ibin(ibin) = t3ibin(ibin) + Tcell * w3 
                    t4ibin(ibin) = t4ibin(ibin) + Tcell * w4 
                    t5ibin(ibin) = t5ibin(ibin) + Tcell * w5 
                    
                    z1ibin(ibin) = z1ibin(ibin) + Zcell * w1 
                    z2ibin(ibin) = z2ibin(ibin) + Zcell * w2 
                    z3ibin(ibin) = z3ibin(ibin) + Zcell * w3 
                    z4ibin(ibin) = z4ibin(ibin) + Zcell * w4 
                    z5ibin(ibin) = z5ibin(ibin) + Zcell * w5 
                    
                    w1ibin(ibin) = w1ibin(ibin) + w1 
                    w2ibin(ibin) = w2ibin(ibin) + w2 
                    w3ibin(ibin) = w3ibin(ibin) + w3 
                    w4ibin(ibin) = w4ibin(ibin) + w4 
                    w5ibin(ibin) = w5ibin(ibin) + w5 

                    ! Alexey's weighting
                    TcontC(ibin) = TcontC(ibin) + TT*fcont/TT**0.875
                    wcontC(ibin) = wcontC(ibin) + fcont/TT**0.875
                    fluxcontC(ibin) = fluxcontC(ibin) + fcont
                  
                    TlineC(ibin) = TlineC(ibin) + emean*fline
                    wlineC(ibin) = wlineC(ibin) + fline
                  endif

                endif
              enddo
            enddo
         ENDIF
      ENDDO

c
c.... prepare profiles
c
      do i = 0 , nbins
	if ( nbin(i) .gt. 0 ) then
          rbin(i) = rbin(i) / nbin(i) * r0
          if ( w1bin(i) .gt. 0. ) t1bin(i) = t1bin(i) / w1bin(i)
          if ( w2bin(i) .gt. 0. ) t2bin(i) = t2bin(i) / w2bin(i) 
          if ( w3bin(i) .gt. 0. ) t3bin(i) = t3bin(i) / w3bin(i)
          if ( w4bin(i) .gt. 0. ) t4bin(i) = t4bin(i) / w4bin(i)
          if ( w5bin(i) .gt. 0. ) t5bin(i) = t5bin(i) / w5bin(i)
          
          if ( w1bin(i) .gt. 0. ) z1bin(i) = z1bin(i) / w1bin(i)
          if ( w2bin(i) .gt. 0. ) z2bin(i) = z2bin(i) / w2bin(i)
          if ( w3bin(i) .gt. 0. ) z3bin(i) = z3bin(i) / w3bin(i)
          if ( w4bin(i) .gt. 0. ) z4bin(i) = z4bin(i) / w4bin(i)
          if ( w5bin(i) .gt. 0. ) z5bin(i) = z5bin(i) / w5bin(i)

          ! Alexey's weighting scheme
          if ( wcont(i) .gt. 0. ) Tcont(i) = Tcont(i) / wcont(i)
          if ( wline(i) .gt. 0. ) then
            emean = Tline(i) / wline(i)
            call lin_interpolate8 (emean_cal,tcal,ncal,emean,xx1)
            Tline(i) = xx1
          endif
          fluxline(i) = wline(i)
          if ( fluxcont(i) .gt. 0. .or. fluxline(i) .gt. 0. ) then
            xx2 = fluxcont(i)/(fluxcont(i)+fluxline(i))
            fcont = exp(-(((xx2-1)**2)/0.15**2)**0.9)
            Tmean(i) = fcont*Tcont(i) + (1-fcont)*Tline(i)
            Tmean(i) = Tmean(i) * KEV_2_K
          endif
          
	endif
      enddo

c
c...  compute the cumulative profiles and output data to the file
c

      t1i = 0.0
      t2i = 0.0
      t3i = 0.0
      t4i = 0.0
      t5i = 0.0

      z1i = 0.0
      z2i = 0.0
      z3i = 0.0
      z4i = 0.0
      z5i = 0.0

      w1i = 0.0
      w2i = 0.0
      w3i = 0.0
      w4i = 0.0
      w5i = 0.0

      TcontCi = 0.0
      wcontCi = 0.0
      fluxcontCi = 0.0
      fluxlineCi = 0.0
      TlineCi = 0.0
      wlineCi = 0.0
          
      do i = 0 , nbins
        if ( i .eq. 0 ) then 
          rl = 0.0
        else
          rl = 10.**(rlmin + float(i-1)*drl)
        endif
        rr = 10.**(rlmin + float(i)*drl)
        volr = 4.18879 * (rr**3 - rl**3)        
	if ( volr .gt. 0. ) then
          t1i = t1i + t1ibin(i)  
          t2i = t2i + t2ibin(i)  
          t3i = t3i + t3ibin(i)  
          t4i = t4i + t4ibin(i)  
          t5i = t5i + t5ibin(i)  
                              
          z1i = z1i + z1ibin(i)  
          z2i = z2i + z2ibin(i)  
          z3i = z3i + z3ibin(i)  
          z4i = z4i + z4ibin(i)  
          z5i = z5i + z5ibin(i)  
                              
          w1i = w1i + w1ibin(i)  
          w2i = w2i + w2ibin(i)  
          w3i = w3i + w3ibin(i)  
          w4i = w4i + w4ibin(i)  
          w5i = w5i + w5ibin(i)  
          
          if ( w1i .gt. 0. ) t1ibin(i) = t1i / w1i
          if ( w2i .gt. 0. ) t2ibin(i) = t2i / w2i
          if ( w3i .gt. 0. ) t3ibin(i) = t3i / w3i
          if ( w4i .gt. 0. ) t4ibin(i) = t4i / w4i
          if ( w5i .gt. 0. ) t5ibin(i) = t5i / w5i

          if ( w1i .gt. 0. ) z1ibin(i) = z1i / w1i
          if ( w2i .gt. 0. ) z2ibin(i) = z2i / w2i
          if ( w3i .gt. 0. ) z3ibin(i) = z3i / w3i
          if ( w4i .gt. 0. ) z4ibin(i) = z4i / w4i
          if ( w5i .gt. 0. ) z5ibin(i) = z5i / w5i
c          write(*,*) i, z1ibin(i), z2ibin(i), z3ibin(i), z4ibin(i)


          ! Alexey's weighting scheme
          TcontCi = TcontCi + TcontC(i)
          wcontCi = wcontCi + wcontC(i)
          fluxcontCi = fluxcontCi + fluxcontC(i)
          TlineCi = TlineCi + TlineC(i)
          wlineCi = wlineCi + wlineC(i)
          
          if ( wcontCi .gt. 0. ) TcontC(i) = TcontCi / wcontCi
          if ( wlineCi .gt. 0. ) then
             emean = TlineCi / wlineCi
             call lin_interpolate8 (emean_cal,tcal,ncal,emean,xx1)
             TlineC(i) = xx1
          endif
          fluxcontC(i) = fluxcontCi
          fluxlineC(i) = wlineCi
          if ( fluxcontC(i) .gt. 0. .or. fluxlineC(i) .gt. 0. ) then
            xx2 = fluxcontC(i)/(fluxcontC(i)+fluxlineC(i))
            fcont = exp(-(((xx2-1)**2)/0.15**2)**0.9)
            TmeanC(i) = fcont*TcontC(i) + (1-fcont)*TlineC(i)
            TmeanC(i) = TmeanC(i) * KEV_2_K
          endif
        else
	  write(*,*) 'bad volr =',volr,i, rl, rr
        endif 
      enddo
c
      rcore_phys = rcore*(r0*aexpn/hubble)*1000.  
c
      nfpn = index ( fprofname , ' ' ) - 1
      open ( 12 , file = fprofname(1:nfpn) )
c
      write(12,10) xc , yc , zc  
      write(12,11) rmin , rmax , nrbin , ibintype
      write(12,12) ifindcenter, l_force_center
      write(12,13) rcore_phys, Tmin
      write(12,17)
      write(12,18)
      write(12,19)
      write(12,20)
 10   format ('# (xc,yc,zc) = ', 3(g13.7,1x))
 11   format ('# rmin = ',g12.6,' rmax =',g12.6,
     &        ' nrbin =',i5,' ibintype =',i1)
 12   format ('# ifindcenter =',i2,' l_force_center=',l6)
 13   format ('# Excluding cells : rcore =',g12.6,
     &        'kpc and Tmin = ',g12.5,'Kelvin')
 17   format ('# 5 columns each : T=int T W dV / int W dV ')
 18   format ('# <r> r_l r_m r_r  T(r)  Z(r)  T(<r)  Z(<r)  ' )
 19   format ('#   Weight function  = n,  n^2,  n^2*T^1/2,  Alexey,',  
     &     '  n^2Lambda(T,z) ' )
 20   format ('#  radius [1/h coming Mpc], Temperature[Kelvin]',  
     &        ' and Metallicity[Zsun]  ')
      do i = 0 , nbins 
        if ( i .eq. 0 ) then  
          rl = 0.0 
          rmid = 0.5*10.**rlmin 
        else
          rl = 10.**(rlmin + float(i-1)*drl)
          rmid = 10.**(rlmin + float(i)*drl - 0.5*drl) * r0
        endif
        rr = 10.**(rlmin + float(i)*drl)
        if ( nbin(i) .gt. 0 ) then
          write(12,21)
     &         rbin(i), rl*r0, rmid, rr*r0, 
     &         t1bin(i), t2bin(i), t3bin(i), Tmean(i), t5bin(i),
     &         z1bin(i), z2bin(i), z3bin(i), z4bin(i), z5bin(i),
     &         t1ibin(i), t2ibin(i), t3ibin(i), TmeanC(i), t5ibin(i),
     &         z1ibin(i), z2ibin(i), z3ibin(i), z4ibin(i), z5ibin(i),
     &         int(nbin(i))
 	endif
      enddo
 21   format(24(2x,g14.7),1x,i7)
 22   format(8(2x,g14.7))
      close ( 12 )
      write(*,*) 'done writing profiles. exiting...'
c
      return
      end
c
