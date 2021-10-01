C
C     =============================================================
C     Mekal spectra :
C     =============================================================
*      implicit none
*      real T, a, z
*      real ebin(0:10000), spec(10000), gov(10000), spec0(10000)
*      integer ne,i
*
*      T=10.0
*      a=0.3
*      z=0.5
*
*      call datafile ('mekal.test.2','1 2')
*      call linesfile (4,10000)
*      call readfile (ne,ebin(1),gov)
*      do i=1,ne
*        ebin(i-1)=ebin(i)-gov(i)
*      enddo
*      ebin(ne)=ebin(ne)+gov(ne)
*
*      call mekal (T, a, z, ne, ebin, spec)
*      call raym  (T, a, z, ne, ebin, spec0)
*      call mekal_set_abund ('aneb')
*      call mekal (T, a, z, ne, ebin, spec0)
*      do i=1,ne
*        print*,i,ebin(i),spec(i),spec0(i),spec(i)/spec0(i)
*      enddo      
*
*      end

      subroutine mekal (T,abund,z,ne,ebin,spec)
c
c Alexey Vikhlinin <alexey@head-cfa.harvard.edu>  Jul 11 2001
c
c Interpolates over the XSPEC pre-calculated MekaL model grid. The meaning
c of input and output parameters is identical to those in J.Hughes' Raymond
c -Smith code:
c
c
c Input:
c 
c real T -- temperature, keV;
c real abund -- relative heavy element abundance
c real z -- redshift;
c integer ne -- number of the spectrum bins, must be Ns.le.NSMAX;
c real ebin(0:ne) -- energy bin boundaries (note the dimension), receiver
c frame, keV
c
c Output:
c 
c real spec(ne) -- spectrum in units of photon*cm**3/s (into 4pi angle) in a
c given ebin interval (energy boundaries are in the receiver frame). Flux is
c given in emitter frame. It should be multiplied by EM=n_e*n_p*V  to get the
c luminosity.               NOTE THE DEFINITION OF EM~~~~~~~~~~~~
c
c The program requires CFITSIO library to read the XSPEC model file and 
c ReadPar library (included in the ZHTOOLS library) to specify the location
c of the model file [call get_cl_par ('mekal_model_file',mekal_model_file)]
c
c The model file should be specified by setting the mekal_model_file=filename
c parameter in the command line.
c
c If you prefer, the model file can be hardcoded by setting the
c -D'MEKALFILE="/path/to/mekal/model/file"' compiler option; in this case the
c ReadPar library and command line arguments will be not required.
c
c
c Entries mekal_set_abund and mekal_set_abund_table can be used to reset the
c abundance table. Usage
c
c  call mekal_set_abund ('allen')
c  call mekal_set_abund ('angers') [or 'xspec' or 'angr' - Anders E. &
c            Grevesse N. (1989,  Geochimica et Cosmochimica Acta 53, 197)]
c  call mekal_set_abund ('feld') [Feldman U. (1992, Physica Scripta 46, 202)]
c  call mekal_set_abund ('aneb') [Anders E. & Ebihara (1982, Geochimica et
c                                  Cosmochimica Acta 46, 2363)]
c
c  real atable(14) ! abundances of He, C, N, O, Ne, Na, Mg, Al, Si, S, 
c                  ! Ar, Ca, Fe, and Ni relative to hydrogen
c  call mekal_set_abund_table (atable)
c
c Fri Sep 17 02:04:58 2004: implement interpolation of log continuum over 1/T
c
      implicit none
      real T, abund, z
      integer ne
      real ebin(0:ne)
      real spec(ne)

      logical firstcall /.true./
      save firstcall
      integer nemax, ntmax, ne0, nt0
      parameter (nemax=3000, ntmax=50)
      real elow(nemax), ehigh(nemax),
     ~    mlspec(nemax,0:14,ntmax),t0(ntmax)
      real mlspec_lines(nemax,ntmax), mlspec_cont(nemax,ntmax)
      save ne0,nt0,t0,elow,ehigh,mlspec,mlspec_lines,mlspec_cont
      character*200 mekal_model_file
      logical defined

      real abund0(14) ! Solar abundances relative to Angers'
      data abund0 /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     ~     1.0, 1.0, 1.0, 1.0/
      real abund_angers(14)
      data abund_angers /
     ~    9.77e-2,              ! He
     ~    3.63e-4,              ! C
     ~    1.12e-4,              ! N
     ~    8.51e-4,              ! O
     ~    1.23e-4,              ! Ne
     ~    2.14e-6,              ! Na
     ~    3.80e-5,              ! Mg
     ~    2.95e-6,              ! Al
     ~    3.55e-5,              ! Si
     ~    1.62e-5,              ! S 
     ~    3.63e-6,              ! Ar
     ~    2.29e-6,              ! Ca
     ~    4.68e-5,              ! Fe
     ~    1.78e-6               ! Ni
     ~    /
      save abund0, abund_angers
      
      integer i,it1,it2,j,it,iemin,iemax,ie
      real wt1,wt2,wtc1,wtc2,sum,emin,emax
      real atable(14)

      if (firstcall) then
c Load the mekal model file
#ifdef MEKALFILE
        mekal_model_file = MEKALFILE
#else
c        call get_cl_par ('mekal_model_file',mekal_model_file)
c        if (.not.defined(mekal_model_file)) then
c          write (0,*) 'mekal_model_file=?'
c          call exit(1)
c        endif
#endif
        call loadmekalfile (mekal_model_file,elow,ehigh,mlspec,t0
     ~      ,nemax,ntmax,ne0,nt0)
        do i=1,ne0
          do it=1,nt0
            mlspec_cont(i,it) = mlspec(i,0,it)+abund0(1)*mlspec(i,1,it)
            mlspec_cont(i,it) = log(mlspec_cont(i,it))
            sum = 0
            do j=2,14
              sum = sum + abund0(j)*mlspec(i,j,it)
            enddo
            mlspec_lines(i,it) = sum
          enddo
        enddo
        firstcall = .false.
      endif


c Find weights for T-interpolation
      if (T.lt.t0(1)) then
        it1=1
        it2=2
c        write (0,*) 'warning: T<',t0(1),'
c     ~       , the minimum value for MekaL grid'
      else if (T.ge.t0(nt0)) then
        it1=nt0-1
        it2=nt0
c        write (0,*) 'warning: T>',t0(nt0),'
c     ~       , the maximum value for MekaL grid'
      else
        do i=1,nt0-1
          if (T.ge.t0(i).and.T.lt.t0(i+1)) then
            it1=i
            it2=i+1
          endif
        enddo
      endif
      wt1 = (t0(it2)-T)/(t0(it2)-t0(it1))
      wt2 = (T-t0(it1))/(t0(it2)-t0(it1))
      wtc1 = (1/T - 1/t0(it2))/(1/t0(it1)-1/t0(it2))
      wtc2 = (1/t0(it1) - 1/T)/(1/t0(it1)-1/t0(it2))

c Main interpolation
      iemin=1
      iemax=0
      do i=1,ne
        emin = ebin(i-1)*(1+z)
        emax = ebin(i)*(1+z)
        do ie=iemin,ne0
          if (emin.ge.elow(ie).and.emin.le.ehigh(ie)) then
            iemin = ie
            goto 101
          endif
        enddo
        ! we are here if the energy bin for emin is not found
        write (0,*) emin,' is outside the MekaL energy grid'
        call exit(1)
 101    continue

        do ie=iemin,ne0
          if (emax.ge.elow(ie).and.emax.le.ehigh(ie)) then
            iemax = ie
            goto 102
          endif
        enddo
        ! we are here if the energy bin for emax is not found
        write (0,*) emax,' is outside the MekaL energy grid'
        call exit(1)
 102    continue

        if (iemin.eq.iemax) then
c        emin and emax are within the same energy bin
          spec(i) = ((emax-emin)/(ehigh(iemin)-elow(iemin)))*
     ~        (
     ~   exp(wtc1*mlspec_cont(iemin,it1)+wtc2*mlspec_cont(iemin,it2)) +
     ~   abund*(wt1*mlspec_lines(iemin,it1)+wt2*mlspec_lines(iemin,it2))
     ~        )
        else
          sum = ((ehigh(iemin)-emin)/(ehigh(iemin)-elow(iemin)))*
     ~        (
     ~   exp(wtc1*mlspec_cont(iemin,it1)+wtc2*mlspec_cont(iemin,it2)) +
     ~   abund*(wt1*mlspec_lines(iemin,it1)+wt2*mlspec_lines(iemin,it2))
     ~        )
          do ie=iemin+1,iemax-1
            sum = sum +
     ~         exp(wtc1*mlspec_cont(ie,it1)+wtc2*mlspec_cont(ie,it2)) +
     ~         abund*(wt1*mlspec_lines(ie,it1)+wt2*mlspec_lines(ie,it2))
          enddo
          sum = sum + 
     ~        ((emax-elow(iemax))/(ehigh(iemax)-elow(iemax)))*
     ~        (
     ~   exp(wtc1*mlspec_cont(iemax,it1)+wtc2*mlspec_cont(iemax,it2)) +
     ~   abund*(wt1*mlspec_lines(iemax,it1)+wt2*mlspec_lines(iemax,it2))
     ~        )
          spec(i)=sum
        endif
      enddo

      do i=1,ne
        spec(i)=spec(i)*1e-14
      enddo

      ! Set flux equal to zero if T<t0(1)
      if (T.lt.t0(1)) then
         do i = 1, ne
            spec(i) = 0.0 
         enddo
      endif

      return

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      entry mekal_set_abund_table (atable)
      do i=1,14
        abund0(i)=atable(i)/abund_angers(i)
      enddo
      
      do i=1,ne0
        do it=1,nt0
          mlspec_cont(i,it) = mlspec(i,0,it)+abund0(1)*mlspec(i,1,it)
          sum = 0
          do j=2,14
            sum = sum + abund0(j)*mlspec(i,j,it)
          enddo
          mlspec_lines(i,it) = sum
        enddo
      enddo
      return

      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      subroutine mekal_set_abund (table)
      implicit none
      character*(*) table
      real atable(14)
      real abund_angers(14), abund_allen(14), 
     ~     abund_feld(14), abund_aneb(14)
      data abund_angers /
     ~    9.77e-2,              ! He
     ~    3.63e-4,              ! C
     ~    1.12e-4,              ! N
     ~    8.51e-4,              ! O
     ~    1.23e-4,              ! Ne
     ~    2.14e-6,              ! Na
     ~    3.80e-5,              ! Mg
     ~    2.95e-6,              ! Al
     ~    3.55e-5,              ! Si
     ~    1.62e-5,              ! S 
     ~    3.63e-6,              ! Ar
     ~    2.29e-6,              ! Ca
     ~    4.68e-5,              ! Fe
     ~    1.78e-6               ! Ni
     ~    /
      data abund_allen /
     ~    8.51e-02,             ! He
     ~    3.31e-04,             ! C
     ~    9.12e-05,             ! N
     ~    6.61e-04,             ! O
     ~    8.32e-05,             ! Ne
     ~    2.14e-6,              ! Na - taken from Angers table
     ~    2.63e-05,             ! Mg
     ~    2.95e-6,              ! Al - taken from Angers table
     ~    3.31e-05,             ! Si
     ~    1.58e-05,             ! S
     ~    6.31e-06,             ! Ar
     ~    2.00e-06,             ! Ca
     ~    3.98e-05,             ! Fe
     ~    2.00e-06              ! Ni
     ~    /
      data abund_feld /
     ~    9.77e-2,              ! He
     ~    3.98e-4,              ! C 
     ~    1.00e-4,              ! N 
     ~    8.51e-4,              ! O 
     ~    1.29e-4,              ! Ne
     ~    2.14e-6,              ! Na
     ~    3.80e-5,              ! Mg
     ~    2.95e-6,              ! Al
     ~    3.55e-5,              ! Si
     ~    1.62e-5,              ! S 
     ~    4.47e-6,              ! Ar
     ~    2.29e-6,              ! Ca
     ~    3.24e-5,              ! Fe
     ~    1.78e-6               ! Ni
     ~    /
      data abund_aneb /
     ~    8.01e-2,              ! He
     ~    4.45e-4,              ! C 
     ~    9.12e-5,              ! N 
     ~    7.39e-4,              ! O 
     ~    1.38e-4,              ! Ne
     ~    2.10e-6,              ! Na
     ~    3.95e-5,              ! Mg
     ~    3.12e-6,              ! Al
     ~    3.68e-5,              ! Si
     ~    1.89e-5,              ! S 
     ~    3.82e-6,              ! Ar
     ~    2.25e-6,              ! Ca
     ~    3.31e-5,              ! Fe
     ~    8.27e-5               ! Ni
     ~        /
      save abund_angers, abund_allen, abund_feld, abund_aneb
      integer i

      if (table.eq.'allen') then
        do i=1,14
          atable(i)=abund_allen(i)
        enddo
        call mekal_set_abund_table (atable)
        write(*,*) ' Abundance : Allen 1973 '
      else if (table.eq.'angers'.or.table.eq.'xspec'
     ~       .or.table.eq.'angr') then
        do i=1,14
          atable(i)=abund_angers(i)
        enddo
        call mekal_set_abund_table (atable)
        write(*,*) ' Abundance : Anders & Greavesse 1989 '
      elseif (table.eq.'feld') then
        do i=1,14
          atable(i)=abund_feld(i)
        enddo
        call mekal_set_abund_table (atable)
        write(*,*) ' Abundance : Feldman 1982 '
      elseif (table.eq.'aneb') then
        do i=1,14
          atable(i)=abund_aneb(i)
        enddo
        call mekal_set_abund_table (atable)
        write(*,*) ' Abundance : Anders & Ebihara 1982 '
      else
        write (0,*) 'warning: unknown abundance table: ',table
        write (0,*) 'warning: abundances not reset'
      endif
      write(*,*) ' '

      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      subroutine loadmekalfile (file, elow, ehigh, spec, t0, nemax, 
     ~     ntmax, ne, nt)
      implicit none
      character*(*) file
      integer unit, status, icol, coln(0:14)
      logical anyf
      integer ne, nt
      integer nemax, ntmax
      real elow(nemax), ehigh(nemax), spec(nemax,0:14,ntmax), t0(ntmax)
      integer i
      character*80 name
      character*80 errtext
      integer lnblnk

      status = 0
      call ftgiou (unit,status)
      call ftnopn (unit,file,0,status)

c Read energy extension
      call ftmnhd (unit,2,'ENERGIES',0,status)
      call ftgnrw (unit,ne,status)
      if (ne.gt.nemax) then
        write (0,*) 'increase nemax in loadmekalfile'
        call exit(1)
      endif

      call ftgcno (unit,.false.,'ENERG_LO',icol,status)
      call ftgcve (unit,icol,1,1,ne,0.0,elow,anyf,status)

      call ftgcno (unit,.false.,'ENERG_HI',icol,status)
      call ftgcve (unit,icol,1,1,ne,0.0,ehigh,anyf,status)
      

c Read spectra extension
      call ftmnhd (unit,2,'SPECTRA',0,status)
      call ftgnrw (unit,nt,status)
      if (nt.gt.ntmax) then
        write (0,*) 'increase ntmax in loadmekalfile'
        call exit(1)
      endif

      call ftgcno (unit,.false.,'PARAMVAL',icol,status)
      call ftgcve (unit,icol,1,1,nt,0.0,t0,anyf,status)

      call ftgcno (unit,.false.,'INTPSPEC',coln(0),status)
      do icol=1,14
        write (name,'(''ADDSP'',i3.3)') icol
        call ftgcno (unit,.false.,name,coln(icol),status)
      enddo

      do i=1,nt
        do icol=0,14
          call ftgcve (unit,coln(icol),i,1,ne,0.0,
     ~          spec(1,icol,i),anyf,status)
        enddo
      enddo
      
c
c The 0-th component seems to represent the continuum, while the 14 others - 
c the contribution of 14 elements:
c       1.........He abundance
c       2.........C     "
c       3.........N     "
c       4.........O     "
c       5.........Ne    "
c       6.........Na    "
c       7.........Mg    "
c       8.........Al    "
c       9.........Si    "
c      10.........S     "
c      11.........Ar    "
c      12.........Ca    "
c      13.........Fe    "
c      14.........Ni    "
c abundances seem to be relative to Solar, from Anders E. & Grevesse N. (1989,
c Geochimica et Cosmochimica Acta 53, 197)
c
c Spectrum is given by the number of photons in the given energy interval,
c with the normalization that of the XSPEC model:
c K      = 10**-14 / (4 pi (D_A*(1+z))**2) Int n_e n_H dV, where D_A is 
c          the angular size distance to the source (cm), n_e is the electron 
c          density (cm**-3), and n_H is the hydrogen density (cm**-3)

      call ftclos(unit,status)
      call ftfiou(unit,status)

      if (status.ne.0) then
        call ftgerr(status,errtext)
        write(0,'(a,a,a)') file(1:lnblnk(file)),': '
     ~      ,errtext(1:lnblnk(errtext))
        call exit(1)
      endif

      return
      end

c
c     -------------------------------------------------
      subroutine PreCompute_XSPEC ( ispec, zobs, ebin )
c     -------------------------------------------------
c     
c     read in previously stored RS emissivity lookup table
c     input: ftable - table filename (must end with a space)
c     
c     units : n_e * n_H * 1.0e-23 * RS_Spec [erg s^-1 cm^-3] 
c
c         ispec =0/1 (RS/Mekal)
c
      parameter ( NEMAX = 300, NTMAX = 200, NZMAX = 20 )
      integer NET, NZT, NTT, ispec, ie, it, iz
      real Zlmin, Zlmax, Tlmin, Tlmax, zobs
      real*8 RST(NTMAX,NZMAX),RSPEC(NEMAX,NTMAX,NZMAX)
      real SOLA(12)
      real ebin(0:NEMAX),spec(NEMAX)
      real e, eold, logstep, linstep
      common / RS01 / RST
      common / RS02 / Zlmin, Zlmax, dlZ, Tlmin, Tlmax, dlT
      common / RS03 / Xpri,Xsol,EeV1,EeV2,nrs,dene,SOLA,defactor2
      common / RS04 / NET, NZT, NTT, dE, RSPEC
      
c
c...  0.01-10.01keV with delta E=0.015keV for E<1keV
c                        0.015 log energy for E>1keV
c
      ne = 0 
      e = 0.1000
      logstep = 1.015
      linstep = 0.015
      
      do while ( e < 10.0 )  
         eold = e 
         if ( e*logstep < e + linstep ) then
            e = e + linstep
         else
            e = e * logstep
         endif
         ebin(ne) = eold
         ne = ne + 1
      enddo
      ebin(ne) = e

      ! fine energy resolution to resolve Fe line
      do i = ne, 188, -1
         ebin(i+3) = ebin(i)
      enddo
      ne = ne + 3
      ebin(188) = 6.688000
      ebin(189) = 6.698000
      ebin(190) = 6.699000

c      write(*,*) ' printing energy bins...'
c      do i = 1, ne
c         write(*,*) i, ebin(i)
c      enddo

c
c...  Set temperature and metallicity grids
c   

      NET = ne
      EeV1 = ebin(0)
      EeV2 = ebin(ne)
      dE  = linstep

      NZT = 17
      Zlmin = -3.0
      Zlmax = 1.0
      dlZ = (Zlmax-Zlmin)/(NZT-1)
      
      NTT = 141
      Tlmin = 6.0
      Tlmax = 8.8
      dlT = (Tlmax-Tlmin)/(NTT-1)
      
c
c...  Precompute the X-ray spectrum
c
      do iz = 0, NZT-1
        do it = 0, NTT-1 
          Zcell = 10.0**(zlmin+dlz*iz) 
          Tcell = 10.0**(tlmin+dlt*it)
c          write(*,*) i,j, Zcell, Tcell
          Tcell = Tcell / 1.160485e+7
          if ( Tcell .gt. 80. )  Tcell = 80.
          if ( ispec == 0 ) 
     &         call raym  (Tcell,Zcell,zobs,NET,ebin,spec)
          if ( ispec == 1 ) 
     &         call mekal (Tcell,Zcell,zobs,NET,ebin,spec)
          do ie = 1, NET
             RSPEC(ie,it,iz) = spec(ie)
          enddo
        enddo
      enddo

      write(*,10) NET, EeV1, EeV2, dE 
      write(*,11) NZT, Zlmin, Zlmax, dlZ
      write(*,12) NTT, Tlmin, Tlmax, dlT

 10   format('NET= ',i5,' EeV1 =',f8.3,' EeV2 = ',f8.3,' dE =',f8.3)
 11   format('NZT= ',i5,' Zlmin = ',f6.3,' Zlmax = ',f6.3,' dlZ =',f6.3)
 12   format('NTT= ',i5,' Tlmin = ',f6.3,' Zlmax = ',f6.3,' dlT =',f6.3)
      return
      end

c
c     -------------------------------------
      subroutine Read_Mekal_Spec ( ftable )
c     -------------------------------------
c     
c     read in previously stored RS emissivity lookup table
c     input: ftable - table filename (must end with a space)
c     
c     units : n_e * n_H * 1.0e-23 * RS_Spec [erg s^-1 cm^-3] 
c
      parameter ( NZMAX = 20 , NTMAX = 200, NEMAX = 300 )
      integer NET, NZT, NTT
      real Zlmin, Zlmax, Tlmin, Tlmax
      real Xpri, Xsol, EeV1, EeV2, dene
      real*8 defactor2
      integer nrs
      real*8 RST(NTMAX,NZMAX),RSPEC(NEMAX,NTMAX,NZMAX)
      real SOLA(12)
      common / RS01 / RST
      common / RS02 / Zlmin, Zlmax, dlZ, Tlmin, Tlmax, dlT
      common / RS03 / Xpri,Xsol,EeV1,EeV2,nrs,dene,SOLA,defactor2
      common / RS04 / NET, NZT, NTT, dE, RSPEC
      character*256 ftable 
      character*20 dummy
      integer lname 

      lname = index(ftable, ' ') - 1
      open ( 30 , file = ftable(1:lname) )
      do i = 1 , 13
        read(30,*) dummy
      enddo
      read(30,*) NET, EeV1, EeV2, dE
      read(30,*) NZT, Zlmin, Zlmax, dlZ
      read(30,*) NTT, Tlmin, Tlmax, dlT

      if ( NZT .gt. NZMAX ) then 
        write(*,*) '* error in Read_RS_Table: NZT > NZMAX:',NZT, NZMAX
        write(*,*) '  increase NZMAX and rerun'       
        stop
      endif
      if ( NTT .gt. NTMAX ) then 
        write(*,*) '* error in Read_RS_Table: NTT > NTMAX:',NTT, NTMAX
        write(*,*) '  increase NTMAX and rerun'       
        stop
      endif
      if ( NZT .le. 1 ) then 
        write(*,*) '* error in Read_RS_Table: NZT <= 1:',NZT
        stop
      endif
      if ( NTT .le. 1 ) then 
        write(*,*) '* error in Read_RS_Table: NTT <= 1:',NTT
        stop
      endif
      read(30,*) dummy
c      read(30,*) Xpri, Xsol, EeV1, EeV2, dene, nrs, defactor2
c      read(30,*) (SOLA(ie), ie=1,12)
      do it = 1 , NTT 
         do iz = 1 , NZT 
            read(30,*) (RSPEC(ii,it,iz),ii=1,NET)
         enddo
      enddo
      close ( 30 )

      write(*,9)  zobs
      write(*,10) NET, EeV1, EeV2, dE 
      write(*,11) NZT, Zlmin, Zlmax, dlZ
      write(*,12) NTT, Tlmin, Tlmax, dlT

 9    format('zobs= ',f6.3)
 10   format('NET= ',i5,' EeV1 =',f8.3,' EeV2 = ',f8.3,' dE =',f8.3)
 11   format('NZT= ',i5,' Zlmin = ',f6.3,' Zlmax = ',f6.3,' dlZ =',f6.3)
 12   format('NTT= ',i5,' Tlmin = ',f6.3,' Zlmax = ',f6.3,' dlT =',f6.3)
      return
      end
c
c     --------------------------------------------
      subroutine mk_Tw_cal (Tcalmin,dlogT,
     ~    tcal,emean_cal,fcont_cal,fline_cal,ncal)
c     --------------------------------------------
c
c     Pre-compute the Mekal X-ray spectrum for one solar metallicity.
c
      implicit none
      double precision Tcalmin,dlogT
      double precision tcal(*),emean_cal(*),fcont_cal(*),fline_cal(*)
      integer ncal
      

      real T,z,nH
      character*200 arffile, rmffile, filename
      character*80 arg, colname, matext, hduclas3,rmfversn
      character*80 telescop, instrume, detnam, filter, chantype
      integer nsmax,nchmax, ns,nch
c      parameter (nsmax=3072,nchmax=2048)
      parameter (nsmax=218,nchmax=1024)
      real eb1(nsmax),eb2(nsmax),rsp(nchmax,nsmax)
      real e_bin(0:nsmax), rsspec_cont(nsmax),rsspec_line(nsmax),
     ~    effarea(nsmax)
      real e_min(nchmax), e_max(nchmax)
      integer channel (nchmax)
      real respline(nchmax)
      integer ngrpmax, grpmax
      parameter (ngrpmax=100)
      integer ngrp(nsmax), F_chan(nsmax,ngrpmax), N_chan(nsmax,ngrpmax)
      real lo_thresh, areascal
      integer flchan, chatter
      double precision modelspec_justline(nsmax)
      double precision modelspec_line(nsmax), rsspec8line(nsmax)
      double precision modelspec_cont(nsmax), rsspec8cont(nsmax)
      double precision phmodelspec_line(nsmax), emodelspec_line(nsmax)
      double precision phmodelspec_cont(nsmax), emodelspec_cont(nsmax)
      integer cmin,cmax
      real emin,emax
      integer i,j
      real sigism,e
      double precision absorp
      character*80 plmodel, abtable ! Plasma and metal abundance models

      double precision fluxcont,fluxline, emean,weight

      integer unitrmf, unitarf, colnum, nrows, blocksize, status
      logical anyf
      
      logical defined

      double precision logT


      write (0,*) 'prepare T-weight calibration'
      call get_cl_par ('plasma_model',plmodel)
      if (.not.defined(plmodel)) then
        plmodel = 'mekal'
      endif
        
      call get_cl_par ('abundance_table',abtable)
      if (.not.defined(abtable)) then
        abtable = 'angers'
      endif

c      call get_parameter_value ('z',z,'e')
c      call get_parameter_value ('nHfit',nH,'e')
      call get_parameter_value_default ('z',z,0.0,'e')
      call get_parameter_value_default ('nHfit',nH,2.0e20,'e')

      call get_cl_par ('rmf',rmffile)
c      if (.not.defined(rmffile)) call exiterror ('rmf=?')
      if (.not.defined(rmffile)) then
          rmffile = '/home/daisuke/Xrays/Response/flux2phot-highres.rmf'
      endif
      call get_cl_par ('arf',arffile)
c      if (.not.defined(arffile)) call exiterror ('arf=?')
      if (.not.defined(arffile)) then
          arffile = '/home/daisuke/Xrays/Response/flux2phot-highres.arf'
      endif 
      
      call get_cl_par ('chanmin',arg)
      if (defined(arg)) then
        read (arg,*) cmin
      else
        call get_parameter_value_default ('emin',emin,0.7,'e')
        cmin = -1000
      endif

      call get_cl_par ('chanmax',arg)
      if (defined(arg)) then
        read (arg,*) cmax
      else
        call get_parameter_value_default ('emax',emax,2.0,'e')
        cmax = -1000
      endif
      
c A) Read RMF
      status=0
      call ftgiou (unitrmf,status)
      filename = rmffile
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

      call ftmnhd (unitrmf,-1,'EBOUNDS',0,status)
      if (status.ne.0) call exit_fitsio ('EBOUNDS, '//filename,status)

      call rdebd3(unitrmf,chatter,nchmax, 
     &    telescop,instrume,detnam,filter,areascal, 
     &    chantype, flchan,
     &    nch,channel,e_min,e_max,rmfversn,status)
      
      call ftclos (unitrmf,status)
      call ftfiou (unitrmf,status)
      if (status.ne.0) call exit_fitsio (filename,status)

c B) Read ARF
      if (arffile.eq.'none') then
        do i=1,ns
          effarea(i)=1
        enddo
      else
        call ftgiou (unitarf,status)
        filename=arffile
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
        if (nrows.ne.ns) call exiterror
     ~      ('Different number of energy channels in ARF and RMF')
        
        call ftgcve(unitarf,colnum,1,1,ns,0.0,effarea,anyf,status)
        if (status.ne.0) call exit_fitsio (filename,status)

        call ftclos(unitarf,status)
        call ftfiou(unitarf,status)
        if (status.ne.0) call exit_fitsio (filename,status)
      endif

      if (cmin.lt.0) then
        do i=1,nch
          if (e_max(i).lt.emin) then
            cmin = i+1
          endif
        enddo
        write (0,*) 'min chan = ',cmin
      endif

      if (cmax.lt.0) then
        cmax=nch
        do i=nch,1,-1
          if (e_min(i).gt.emax) then
            cmax = i
          endif
        enddo
        write (0,*) 'max chan = ',cmax
      endif


      e_bin(0)=eb1(1)
      do i=1,ns
        e_bin(i)=eb2(i)
      enddo

      call mekal_set_abund (abtable)


      T = Tcalmin
      logT = log(Tcalmin)
      ncal = 0
      do while ( T .lt. 22.0)
        ncal = ncal + 1
        T = exp(logT)
        call mekal (T, 0.0, z, ns, e_bin, rsspec_cont)
        call mekal (T, 3.0, z, ns, e_bin, rsspec_line)
        do i=1,ns
          rsspec8cont(i)=dble(rsspec_cont(i))
          rsspec8line(i)=dble(rsspec_line(i))
        enddo
      
        do i=1,ns
          e = 0.5*(eb1(i)+eb2(i))*1000.0
          absorp = sigism(e)*nH
          if (absorp.lt.50.0) then
            absorp=exp(-absorp)
          else
            absorp=0.0
          endif
          modelspec_cont(i) = absorp*rsspec8cont(i)*effarea(i)
          modelspec_line(i) = absorp*rsspec8line(i)*effarea(i)
          modelspec_justline(i) = (modelspec_line(i)-modelspec_cont(i))
          phmodelspec_cont(i)=rsspec8cont(i)
          phmodelspec_line(i)=rsspec8line(i)
          emodelspec_cont(i)=rsspec8cont(i)*e*1.60219e-12
          emodelspec_line(i)=rsspec8line(i)*e*1.60219e-12
        enddo
        
        do i = 1,nch
          respline(i)=0
        enddo
        do i=1,ns
          do j=1,nch
            respline(j)=respline(j)+modelspec_justline(i)*rsp(j,i)
          enddo
        enddo
        emean=0
        weight=0
        do i=1,nch
          emean = emean + respline(i)*0.5*(e_max(i)+e_min(i))
          weight = weight + respline(i)
        enddo
        emean = emean/weight

        fluxcont = 0
        fluxline = 0
        do i=1,ns
          do j=cmin,cmax
            fluxcont=fluxcont+modelspec_cont(i)*dble(rsp(j,i))
            fluxline=fluxline+modelspec_line(i)*dble(rsp(j,i))
          enddo
        enddo
        fluxline = (fluxline - fluxcont)/3.0 ! to convert to a=1

        fluxcont = fluxcont / 2e-13
        fluxline = fluxline / 2e-13

        tcal(ncal)=exp(logT)
        emean_cal(ncal)=emean
        fcont_cal(ncal)=fluxcont
        fline_cal(ncal)=fluxline

        logT = logT + dlogT
      enddo

      return
      end


*+RDRMF3
        subroutine rdrmf3(iunit, chatter,matext,
     &          telescop, instrume, detnam, filter, areascal,
     &          chantype, flchan, 
     &          ichan, ienerg, energ_lo, energ_hi,
     &          imaxgrp, ngrp, F_chan, N_chan,
     &          fmatrix, lo_thresh, maxchan,maxen,
     &          rmfversn,hduclas3,ierr)

	IMPLICIT NONE
	integer chatter, ierr, maxchan,maxen
	integer iunit, flchan
	integer ichan, ienerg, imaxgrp
	integer ngrp(maxen), F_chan(maxen,*)
	integer N_chan(maxen,*)
	real areascal, lo_thresh
	real energ_lo(maxen), energ_hi(maxen)
	real fmatrix(maxchan,maxen)
        character*(*) chantype
        character*(*) rmfversn
        character*(*) hduclas3
	character*(*) telescop, instrume, detnam, filter
        character*(*) matext

c --- DESCRIPTION -----------------------------------------------------
c
c Reads the RMF extension for an RMFVERSN = 1992a RMF file
c The file is assumed to conform to the HDUVERS2='1.*.*' family.
c Currently the OGIP formats supported are
c HDUVERS2 = '1.0.0'
c HDUVERS2 = '1.1.0'
c HDUVERS2 = '1.2.0'
c see OGIP/92-002a
c The HDU CLASS keywords have only been currently introduced thus DO NOT
c have to be present to use this reader.
c
c Assumes the FITS file is open.
c !!! NOTE !!! 
c     - The format definitions prior to HDUVERS2 = '1.2.0' did not require 
c       TLMIN/MAX keywords for the F_CHAN column. This leads to an incorrect
c       indexing for the fmatrix array for detectors whose 1st channel is 
c       numbered zero. This code will be able to correct for this bug for 
c       matrices written using formats prior to HDUVERS2 = '1.2.0' only if 
c       there happens to be a row in the BINTABLE which includes F_CHAN = 0.
c       This is often the case, but cannot be guarenteed.
c     - File is left open at end
c       ... close file using FTCLOS, or
c       ... read another extension
c
c Columns read are ...
c  ENERG_LO      : Low energy bound for row
c  ENERG_HI      : High energy bound for row
c  N_GRP         : Number of channel subsets for row
c  F_CHAN        : Firstchannel in each subset for row
c  N_CHAN        : Number of channels for each subset for row
c  MATRIX        : (non-zero) Matrix element for row
c Keywords read ...
c  TELESCOP      : Mission/Telescop name , if not present set to UNKNOWN
c  INSTRUME      : Instrument/Detector name, if not present set to UNKNOWN 
c  DETNAME       : Specific detector name, if not present set to NONE 
c  FILTER        : Filter in use, if not present set to NONE
c  TLMIN/MAX	 : (for F_CHAN col) for legal min & max for channel numbering
c  EFFAREA       : Effective area, if not present,set to 1
c  CHANTYPE      : Type of detector channel in use (PHA, PI)
c  LO_THRESH     : Threshold used to construct the MATRIX, if not present,
c                  set to 0
c  HDUCLAS3      : Values describing data, OGIP approved - REDIST,FULL,
c		   DETECTOR
c  RMFVERSN      : OGIP RMF version
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MATEXT          o : extension name SPECRESP/MATRIX
c  TELESCOP        o : String listing telescope/mission
c  INSTRUME        o : String listing instrument/detector
c  DETNAM          o : String listing specific detector name   
c  FILTER          o : String listing instrument filter in use
c  AREASCAL        o : Area scaling factor
c  CHANTYPE        o : Type of detector channel in use
c  FLCHAN          o : Lowest legal channel for this detector
c  RMFVERSN        o : RMF version
c  HDUCLAS3        o : Hduclas3 keyword value
c  ICHAN           o : No. channels in the full array
c  IENERG          o : No. energy bins
c  ENERG_LO        o : Array containing lower bound to each energy bin
c  ENERG_HI        o : Array containing upper bound to each energy bin
c  NGRP            o : Array containing no. channel subsets at each energy
c  IMAXGRP         o : Max no. grps in any given row
c  F_CHAN          o : Array containing 1st chan of each subset at each energy
c  N_CHAN          o : Array containing no. chans within each subset 
c                           at each energy
c  FMATRIX         o : Array containing the full matrix
c  LO_THRESH       o : The lower threshold used to construct the matrix
c  IERR            o : Error flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FTGCNO            :(FITSIO) returns table column number
c  subroutine FTGCV*            :(FITSIO) reads vector column
c  subroutine FTGKY*            :(FITSIO) reads individual kywds from header
c  subroutine WTERRM            :(CALLIB) writes error message to STDOUT
c  subroutine WTFERR            :(CALLIB) writes error msg (+fitsio) to STDOUT
c  subroutine WTFWRN            :(CALLIB) writes warning msg (+fitsio) to STDOUT
c  subroutine WTINFO            :(CALLIB) writes info message to STDOUT
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c PARENT Routine (wtrmf1.f) history
c 	Rehana Yusaf (1.0.0:93 Jul 26) WT_RMF1992A.F (CALLIB) used as basis
c 	Rehana Yusaf (1.0.1:93 Oct 27) added arguments for rmfversn and 
c					hduclas3, also the name has been 
c				     	changed from rd_rmf1992a. In addition 
c					if extname is not found 
c					HDUCLAS1='RESPONSE' is searched for.
c					and HDUCLAS2='RSP_MATRIX'
c 	Rehana Yusaf (1.0.2:93 Nov 10) HDUVERS2 is read to obtain rmfversn if 
c					HDUVERSN not present then RMFVERSN is 
c					read. Prev' only RMFVERSN read 
c 	Ian M George (1.1.0: 93 Nov 17) Took out searching for correct xtens
c					(this is now responsibilty of main)
c 	Rehana Yusaf (1.1.1:94 Jan 11) Remove mvalues array and read matrix
c					matrix values straight into fmatrix
c 	Rehana Yusaf (1.1.1: 94 Jun 24) Make the routine less verbose, that is 
c					only print warnings at chatter>30 
c PARENT Routine (wtrmf2.f) history
c 	Ian M George (1.0.0:95 Nov 22) Copied from wtrmf1.f (1.1.1), but 
c					chantype added as passed parameter
c 	Ian M George (1.0.1:95 Nov 29) Added wtinfo & friends
c THIS ROUTINE (wtrmf3.f) history
c Ian M George (1.0.0:96 Oct 04) Copied from wtrmf2.f (1.0.1), suuport for
c                             	HDUVERS2 = '1.2.0' added (and flchan added 
c                               as passed parameter
c
c Banashree M Seifert (1.1.0 Oct 10, 1996)
c       . wrtstr was char*80 and made *8
c
c Banashree M Seifert (1.2.0 Nov13, 1996)
c       . flchan needed to be initialised
c --------------------------------------------------------------------
	character*7 version
	parameter (version = '1.2.0')
*-
c Internals
        character*6 subname
        parameter (subname = 'rdrmf3')
	integer status
	integer i, k, siz_mat, siz_ngrp
	integer ie,j, ic, frow,felem,colnum,inull
	character*80 message
        character*8 wrtstr
        character*30 comm, hduvers2
	integer ivalues(10), ioff
	real enull
        logical anyflg, qflchan_prob
c Initialise
      ierr = 0
      status = 0

c User info, if requested
        message = 'using '//subname//' '//version
        call wtinfo(chatter,15,1,message)

c Get necessary keywords
c NAXIS2 ...
      status = 0
      call ftgkyj(iunit,'NAXIS2',ienerg,comm,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     &		'reading NAXIS2 keyword')
        ierr = 4
        goto 987
      ENDIF
      IF (ienerg.GT.maxen) THEN
        ierr = 4
	call wtferr(subname,version,status,
     & 		'Energy Array dimension is too small')
        goto 987
      ENDIF

c HDUCLAS3 ...
      hduclas3 = '  '
      status = 0
      call ftgkys(iunit,'HDUCLAS3',hduclas3,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUCLAS3 keyword')

c HDUVERS2 ...
      hduvers2 = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',hduvers2,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUVERS2 keyword')
c If HDUVERS2 < 1.2.0, then we have a potential problem regarding what the 
c first legal channel is for this detector. This will require a check later 
c so set a flag. 
	if((hduvers2.EQ.'1.0.0').or.(hduvers2.EQ.'1.1.0')) then
		qflchan_prob = .true.
	else
		qflchan_prob = .false.
	endif

c RMFVERSN ...
      rmfversn = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',rmfversn,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUVERS2 keyword')
      IF (rmfversn.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'RMFVERSN',rmfversn,comm,status) 
        call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading RMFVERSN keyword')
      ENDIF

c TELESCOP ...
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading TELESCOP keyword')
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF 	

c INSTRUME ...
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading INSTRUME keyword')
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF         

c FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading FILTER keyword')
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF         

c DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading DETNAM keyword')
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c EFFAREA ...
      status = 0
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading EFFAREA keyword')
      IF (status.NE.0) THEN
        areascal = 1
      ENDIF

c LO_THRESH ...
      status = 0
      call ftgkye(iunit,'LO_THRESH',lo_thresh,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading LO_THRESH keyword')
      IF (status.NE.0) THEN
        lo_thresh = 0
      ENDIF

c DETCHANS ...
      status = 0
      call ftgkyj(iunit,'DETCHANS',ichan,comm,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading DETCHANS keyword')
        ierr = 1
        goto 987
      ENDIF
      IF (ichan.GT.maxchan) THEN
        ierr = 4
	call wterrm(subname,version,
     & 		'Channel Array dimension is too small')
        goto 987
      ENDIF

c CHANTYPE
      status = 0
      call ftgkys(iunit,'CHANTYPE',chantype,comm,status)
      if(status.NE.0) then
        call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading CHANTYPE keyword from RMF')
	call wtinfo(chatter,1, 1,'setting CHANTYPE = UNKNOWN')
        chantype = 'UNKNOWN'
        status= 0
      endif


c Reasure user, if requested
	call wtinfo(chatter,20,1,'read all the keywords')
	call wtinfo(chatter,20,1,'reading the data')

c Get the data (and necessary extras)
c ENERG_LO ...
      frow = 1
      felem = 1
      status = 0
      call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding ENERG_LO column')
         ierr = 4
         goto 987
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_lo,
     &            anyflg,status)      
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading ENERG_LO column')
        ierr = 1
        goto 987
      ENDIF

c ENERG_HI ...
      status = 0
      call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding ENERG_LO column')
         ierr = 4
         goto 987
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_hi,
     &            anyflg,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading ENERG_HI column')
        ierr = 1
        goto 987
      ENDIF      


c NGRP ...
      status = 0
      call ftgcno(iunit,.false.,'N_GRP',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding N_GRP column')
         ierr = 4
         goto 987
      ENDIF
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,ienerg,inull,ngrp,
     &            anyflg,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading N_GRP column')
        ierr = 1
        goto 987
      ENDIF    

c F_CHAN ...
      status = 0
      call ftgcno(iunit,.false.,'F_CHAN',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding F_CHAN column')
         ierr = 4
         goto 987
      ENDIF
      do i=1,ienerg
        inull = 0
        call ftgcvj(iunit,colnum,i,felem,ngrp(i),inull,
     &            ivalues,anyflg,status)
        IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading F_CHAN column')
          ierr = 1
          goto 987
        ENDIF 
        do j=1,ngrp(i)
           F_chan(i,j) = ivalues(j)
        enddo
      enddo
c OK, if we're reading a dataset created with HDUVERS2 < 1.2.0, then we have 
c a potential problem regarding what the first legal channel is for this 
c detector. Try and check to see if F_CHAN is ever zero, and if so assume 
c that the first legal channel is also zero. THIS WILL OFTEN CATCH THE PROBLEM,
c BUT NOT ALWAYS - although there is little else we can do.
c If we're reading a dataset created with HDUVERS2 >= 1.2.0, then the TLMIN
c keyword should be present, so go read it.
	if(qflchan_prob) then
      	  do i=1,ienerg
	     if(ngrp(i).gt.0 .and. F_chan(i,1).eq.0) then
		flchan = 0
		goto 321
             endif
      	  enddo
          flchan=1
	else
          write(wrtstr, '(a,i1)') 'TLMIN', colnum
          call crmvblk(wrtstr)
      	  status = 0
      	  call ftgkyj(iunit,wrtstr,flchan,comm,status)
      	  if(status.NE.0) then
	    message = 'Problem reading '//wrtstr//' keyword'
            call wtfwrn(subname,version,chatter, 1, status, message)
	    message = 'assuming first legal channel in the detector is '//
     &		'numbered channel 1'
	    call wtinfo(chatter,1, 1,message)
            flchan = 1
            status= 0
          endif
	endif
321	continue

c N_CHAN ...
      status = 0
      call ftgcno(iunit,.false.,'N_CHAN',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding N_CHAN column')
         ierr = 4
         goto 987
      ENDIF
      do i=1,ienerg
        inull = 0
        call ftgcvj(iunit,colnum,i,felem,ngrp(i),inull,
     &              ivalues,anyflg,status)
        IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading N_CHAN column')
          ierr = 1
          goto 987
        ENDIF  
        do j=1,ngrp(i)
          N_chan(i,j) = ivalues(j)
        enddo
      enddo


c MATRIX ...
c initialise matrix array ...
      do i=1,ichan
        do j=1,ienerg
          fmatrix(i,j) = 0.0
        enddo
      enddo
      
c imaxgrp ...
      siz_ngrp = 0
      do i=1,ienerg
        siz_ngrp = MAX(siz_ngrp,ngrp(i))         
      enddo
      imaxgrp = siz_ngrp

c read matrix ...
      status = 0
      call ftgcno(iunit,.false.,'MATRIX',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding MATRIX column')
         ierr = 4
         goto 987
      ENDIF
      
c ... first worry about offsets
      if(flchan.EQ.0) then
        ioff = 1
      else
        ioff = 0
      endif
      do ie=1,ienerg
        siz_mat = 0
        do j=1,ngrp(ie)
          siz_mat = siz_mat + N_chan(ie,j)
        enddo
        k = 0
        do j=1,ngrp(ie)
          do ic=F_chan(ie,j),F_chan(ie,j)+N_chan(ie,j) - 1
            k=k+1
            enull = 0
            call ftgcve(iunit,colnum,ie,k,1,enull,
     &              fmatrix(ic+ioff,ie),anyflg,status)
            IF (status.NE.0) THEN
		call wtferr(subname,version,status,
     & 			'Problem reading MATRIX column')
              ierr = 1
              goto 987
            ENDIF
          enddo
        enddo
      enddo


987	if(ierr.NE.0) then
	  call wterrm(subname, version, ' Fatal - aborting')
	else
	  call wtinfo(chatter,20,1, 
     &		'successfully read RSP_MATRIX data')
	endif

      return
      end
c ----------------------------------------------------------------------
c     END OF RDRMF3
c ----------------------------------------------------------------------

  
*+RDEBD3
c     --------------------------------------------------------
	subroutine rdebd3(iunit,chatter,maxchan, 
     &		telescop,instrume,detnam,filter,areascal, 
     &		chantype, flchan,
     &		iebound,channel,e_min,e_max,rmfversn,ierr)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c  Reads the EBOUNDS extension for an RMFVERSN=1992a RMF file
c Assumes the extension conforms to HDUVERS2='1.*.*' family
c Currently the following formats are supported -
c HDUVERS2='1.0.0'
c HDUVERS2='1.1.0'
c see OGIP/92-002a
c The HDU CLASS keywords have only been currently introduced thus DO NOT
c have to be present to use this reader.  
c
c  Assumes the FITS is open.
c  !!! Note !!!! File is left open at the end
c      ... close file using FTCLOS, or
c      ... read another extension
c
c  Columns read are ...
c  CHANNEL     : Channel numbers
c  E_MIN       : Nominal lower energy bound
c  E_MAX       : Nominal upper energy bound
c
c  Keywords read ...
c  TELESCOP : Mission/Telescope name, NOTE: If not present set to UNKNOWN
c  INSTRUME : Instrument/Detector name NOTE: If not present set to UNKNOWN
c  DETNAM   : Specific detector name NOTE: If not present set to NONE
c  FILTER   : Filter in use, if not present set to NONE
c  TLMIN/MAX: (for CHANNEL col) for legal min & max for channel numbering
c  CHANTYPE : Type of detector channel in use (PHA, PI)
c  EFFAREA  : Areascaling factor, if not present set to 1
c  RMFVERSN : RMF version
c  
c --- VARIABLES -------------------------------------------------------
c                                                      
	IMPLICIT NONE
	integer chatter, ierr, maxchan
	integer iunit, flchan
	integer iebound, channel(maxchan)
	real areascal
	real e_min(maxchan), e_max(maxchan)
        character*(*) rmfversn, chantype
	character*(*) telescop, instrume, detnam, filter
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c Passed parameters
c
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (0 quite,10 normal,>20 silly)
c  TELESCOP      o   : String listing telescope/mission
c  INSTRUME      o   : String listing instrument/detector
c  DETNAM        o   : String listing specific detector name
c  FILTER        o   : String listing instrument filter in use
c  AREA          o   : Area scaling factor
c  CHANTYPE : Type of detector channel in use (PHA, PI)
c  FLCHAN          o : Lowest legal channel for this detector
c  IEBOUND       o   : No. channels in the full array
c  CHANNEL       o   : Channel array
c  E_MIN         o   : Array containing min nominal energy bound to each chan
c  E_MAX         o   : Array containing max nominal energy bound to each chan
c  RMFVERSN      o   : RMF version
c  IERR          o   : Error Flag, ierr = 0 okay
c                                  ierr = 1 error finding extension
c                                  ierr = 2 error finding Column number
c                                  ierr = 3 error reading data
c                                  ierr = 4 NAXIS2 not found
c				   ierr = 5 maxchan, array size NOT large enough
c
c --- CALLED ROUTINES --------------------------------------------------
c
c  subroutine FTMRHD     : (FITSIO) Move to extension
c  subroutine FTGKYS     : (FITSIO) Read FITS extension header keyword 
c  subroutine FTGCNO     : (FITSIO) Get column number
c  subroutine FTGCVx     : (FITSIO) Read data in x format
c  subroutine WT_FERRMSG : (CALLIB) Dumps FITSIO Error message etc
c
c Compilation & Linking 
c  link with FITSIO & CALLIB & FTOOLS
c
c Authors/Modification History: 
c PARENT Routine (rdebd1.f) history
c 	Rehana Yusaf (1.0.0:93 Jul 15)
c 	Rehana Yusaf (1.0.1:93 Oct 27) Rename from RD_EBD1992a, and
c				    additional argument passed, rmfversn
c 				    Also HDUCLASS is now used to find
c				    extension if not present then extname
c                                   searched for.
c 	Rehana Yusaf (1.0.2:93 Nov 10) Read HDUVERS2 keyword to get rmfversn
c 				    if HDUVERSN not present the read RMFVERSN
c				    previously RMFVERSN was read
c 	Ian M George (1.1.0:93 Nov 17) Took out extension searching stuff
c 	Rehana Yusaf (1.1.1:94 Jan 11) Additional argument, maxchan - array
c                                   dimension, compared to Naxis2 and error out
c                                   if too small
c 	Rehana Yusaf (1.1.2:94 Jun 24) Less verbose;Only printer warnings at
c                                   high chatter. Also add fcecho call
c                                   after channel error check
c THIS ROUTINE (wtebd3.f) history 
c Ian M George (1.0.0:96 Oct 04) copied from rdebd1 (v1.1.2), with flchan and
c				chantype added as passed parameters
c
c Banashree M Seifert (1.1.0, Oct 10, 1996)
c       . wrtstr was char*80 made *8 
c ----------------------------------------------------------------------
	character*7 version
	parameter (version = '1.1.0')
*- 
c Internals
        character*6 subname
        parameter (subname = 'rdebd3')
        character*30 comm
	character*80 message
        character*8 wrtstr
        integer status,  inull, felem, frow, colnum
        real enull
        logical anyflg
c Initialise     
      	ierr = 0
      	status = 0 


c User info, if requested
        message = 'using '//subname//' '//version
        call wtinfo(chatter,15,1,message)

c READING DATA 
c NAXIS2 ...
      status = 0
      call ftgkyj(iunit,'NAXIS2',iebound,comm,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          ' reading NAXIS2 keyword')
        ierr = 4
        goto 987
      ENDIF
      IF (iebound.GT.maxchan) THEN
        ierr = 5
        call wtferr(subname,version,status,
     &          'Channel Array dimension is too small')
        goto 987
      ENDIF

c RMFVERSN ...
      rmfversn = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',rmfversn,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading HDUVERS2 keyword')
      IF (rmfversn.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'RMFVERSN',rmfversn,comm,status)
        call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading RMFVERSN keyword')
      ENDIF

c TELESCOP ...
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     &          'Problem reading TELESCOP keyword')
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF

c INSTRUME ...
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     &          'Problem reading INSTRUME keyword')
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF

c FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading FILTER keyword')
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF

c DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading DETNAM keyword')
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c CHANTYPE ...
      status = 0
      call ftgkys(iunit,'CHANTYPE',chantype,comm,status)
      if(status.NE.0) then
        call wtfwrn(subname,version,chatter, 20, status,
     &          'Problem reading CHANTYPE keyword from EBOUNDS')
        call wtinfo(chatter,1, 1,'setting CHANTYPE = UNKNOWN')
        chantype = 'UNKNOWN'
        status= 0
      endif

c EFFAREA ...
      status = 0
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading EFFAREA keyword')
      IF (status.NE.0) THEN
        areascal = 1
      ENDIF
c Reasure user, if requested
        call wtinfo(chatter,20,1,'read all the keywords')
        call wtinfo(chatter,20,1,'reading the data')

c CHANNEL COLUMN NUMBER ...
      status = 0
      call ftgcno(iunit,.false.,'CHANNEL',colnum,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem finding CHANNEL column')
         ierr = 4
         goto 987
      ENDIF

c READ CHANNEL COLUMN ...
      status = 0
      frow = 1
      felem = 1
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,iebound,inull,channel,
     &            anyflg,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem reading CHANNEL column')
        ierr = 3
        goto 987
      ENDIF
	write(wrtstr, '(a,i1)') 'TLMIN', colnum
        call crmvblk(wrtstr)

        status = 0
        call ftgkyj(iunit,wrtstr(1:8),flchan,comm,status)
        if(status.NE.0) then
            message = 'Problem reading '//wrtstr//' keyword'
            call wtwarm(subname,version,chatter,20,message)
            message = 'assuming first legal channel in the detector '//
     &		'is numbered channel 1'
            call wtinfo(chatter,1, 1,message)
            flchan = 1
            status= 0
        endif

c E_MIN COLUMN NUMBER ...
      status = 0
      call ftgcno(iunit,.false.,'E_MIN',colnum,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem finding E_MIN column')
         ierr = 2
         goto 987
      ENDIF
    
c READ E_MIN ...
      status = 0 
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_min,
     &            anyflg,status) 
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem reading E_MIN column')
        ierr = 3
        goto 987
      ENDIF
      
c E_MAX COLUMN NUMBER ...

      status = 0
      call ftgcno(iunit,.false.,'E_MAX',colnum,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem finding E_MIN column')
         ierr = 2
         goto 987
      ENDIF

c READ E_MAX ...
      status = 0
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_max,
     &            anyflg,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem reading E_MIN column')
        ierr = 3
        goto 987
      ENDIF

987     if(ierr.NE.0) then
          call wterrm(subname, version, ' Fatal - aborting')
        else
          call wtinfo(chatter,20,1,
     &          'successfully read EBOUNDS data')
        endif

      return
      end
c --------------------------------------------------------------------
c     END OF RDEBD3
c --------------------------------------------------------------------   

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      
*+RDRMF3INFO
      subroutine rdrmf3_info(iunit, chatter,matext,
     &    telescop, instrume, detnam, filter, areascal,
     &    chantype, flchan, 
     &    ichan, ienerg, energ_lo, energ_hi,
     &    rmfversn,hduclas3,ierr)
c
c Reads the keyword, nuber of channels and the energy boundaries.
c
	IMPLICIT NONE
	integer chatter, ierr, maxchan,maxen
	integer iunit, flchan
	integer ichan, ienerg, imaxgrp
	real areascal, lo_thresh
	real energ_lo(*), energ_hi(*)
        character*(*) chantype
        character*(*) rmfversn
        character*(*) hduclas3
	character*(*) telescop, instrume, detnam, filter
        character*(*) matext

c --- DESCRIPTION -----------------------------------------------------
c
c Reads the RMF extension for an RMFVERSN = 1992a RMF file
c The file is assumed to conform to the HDUVERS2='1.*.*' family.
c Currently the OGIP formats supported are
c HDUVERS2 = '1.0.0'
c HDUVERS2 = '1.1.0'
c HDUVERS2 = '1.2.0'
c see OGIP/92-002a
c The HDU CLASS keywords have only been currently introduced thus DO NOT
c have to be present to use this reader.
c
c Assumes the FITS file is open.
c !!! NOTE !!! 
c     - The format definitions prior to HDUVERS2 = '1.2.0' did not require 
c       TLMIN/MAX keywords for the F_CHAN column. This leads to an incorrect
c       indexing for the fmatrix array for detectors whose 1st channel is 
c       numbered zero. This code will be able to correct for this bug for 
c       matrices written using formats prior to HDUVERS2 = '1.2.0' only if 
c       there happens to be a row in the BINTABLE which includes F_CHAN = 0.
c       This is often the case, but cannot be guarenteed.
c     - File is left open at end
c       ... close file using FTCLOS, or
c       ... read another extension
c
c Columns read are ...
c  ENERG_LO      : Low energy bound for row
c  ENERG_HI      : High energy bound for row
c  N_GRP         : Number of channel subsets for row
c  F_CHAN        : Firstchannel in each subset for row
c  N_CHAN        : Number of channels for each subset for row
c  MATRIX        : (non-zero) Matrix element for row
c Keywords read ...
c  TELESCOP      : Mission/Telescop name , if not present set to UNKNOWN
c  INSTRUME      : Instrument/Detector name, if not present set to UNKNOWN 
c  DETNAME       : Specific detector name, if not present set to NONE 
c  FILTER        : Filter in use, if not present set to NONE
c  TLMIN/MAX	 : (for F_CHAN col) for legal min & max for channel numbering
c  EFFAREA       : Effective area, if not present,set to 1
c  CHANTYPE      : Type of detector channel in use (PHA, PI)
c  LO_THRESH     : Threshold used to construct the MATRIX, if not present,
c                  set to 0
c  HDUCLAS3      : Values describing data, OGIP approved - REDIST,FULL,
c		   DETECTOR
c  RMFVERSN      : OGIP RMF version
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MATEXT          o : extension name SPECRESP/MATRIX
c  TELESCOP        o : String listing telescope/mission
c  INSTRUME        o : String listing instrument/detector
c  DETNAM          o : String listing specific detector name   
c  FILTER          o : String listing instrument filter in use
c  AREASCAL        o : Area scaling factor
c  CHANTYPE        o : Type of detector channel in use
c  FLCHAN          o : Lowest legal channel for this detector
c  RMFVERSN        o : RMF version
c  HDUCLAS3        o : Hduclas3 keyword value
c  ICHAN           o : No. channels in the full array
c  IENERG          o : No. energy bins
c  ENERG_LO        o : Array containing lower bound to each energy bin
c  ENERG_HI        o : Array containing upper bound to each energy bin
c  NGRP            o : Array containing no. channel subsets at each energy
c  IMAXGRP         o : Max no. grps in any given row
c  F_CHAN          o : Array containing 1st chan of each subset at each energy
c  N_CHAN          o : Array containing no. chans within each subset 
c                           at each energy
c  FMATRIX         o : Array containing the full matrix
c  LO_THRESH       o : The lower threshold used to construct the matrix
c  IERR            o : Error flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FTGCNO            :(FITSIO) returns table column number
c  subroutine FTGCV*            :(FITSIO) reads vector column
c  subroutine FTGKY*            :(FITSIO) reads individual kywds from header
c  subroutine WTERRM            :(CALLIB) writes error message to STDOUT
c  subroutine WTFERR            :(CALLIB) writes error msg (+fitsio) to STDOUT
c  subroutine WTFWRN            :(CALLIB) writes warning msg (+fitsio) to STDOUT
c  subroutine WTINFO            :(CALLIB) writes info message to STDOUT
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c PARENT Routine (wtrmf1.f) history
c 	Rehana Yusaf (1.0.0:93 Jul 26) WT_RMF1992A.F (CALLIB) used as basis
c 	Rehana Yusaf (1.0.1:93 Oct 27) added arguments for rmfversn and 
c					hduclas3, also the name has been 
c				     	changed from rd_rmf1992a. In addition 
c					if extname is not found 
c					HDUCLAS1='RESPONSE' is searched for.
c					and HDUCLAS2='RSP_MATRIX'
c 	Rehana Yusaf (1.0.2:93 Nov 10) HDUVERS2 is read to obtain rmfversn if 
c					HDUVERSN not present then RMFVERSN is 
c					read. Prev' only RMFVERSN read 
c 	Ian M George (1.1.0: 93 Nov 17) Took out searching for correct xtens
c					(this is now responsibilty of main)
c 	Rehana Yusaf (1.1.1:94 Jan 11) Remove mvalues array and read matrix
c					matrix values straight into fmatrix
c 	Rehana Yusaf (1.1.1: 94 Jun 24) Make the routine less verbose, that is 
c					only print warnings at chatter>30 
c PARENT Routine (wtrmf2.f) history
c 	Ian M George (1.0.0:95 Nov 22) Copied from wtrmf1.f (1.1.1), but 
c					chantype added as passed parameter
c 	Ian M George (1.0.1:95 Nov 29) Added wtinfo & friends
c THIS ROUTINE (wtrmf3.f) history
c Ian M George (1.0.0:96 Oct 04) Copied from wtrmf2.f (1.0.1), suuport for
c                             	HDUVERS2 = '1.2.0' added (and flchan added 
c                               as passed parameter
c
c Banashree M Seifert (1.1.0 Oct 10, 1996)
c       . wrtstr was char*80 and made *8
c
c Banashree M Seifert (1.2.0 Nov13, 1996)
c       . flchan needed to be initialised
c --------------------------------------------------------------------
	character*7 version
	parameter (version = '1.2.0')
*-
c Internals
        character*6 subname
        parameter (subname = 'rdrmf3')
	integer status
	integer i, k, siz_mat, siz_ngrp
	integer ie,j, ic, frow,felem,colnum,inull
	character*80 message
        character*8 wrtstr
        character*30 comm, hduvers2
	integer ivalues(10), ioff
	real enull
        logical anyflg, qflchan_prob
c Initialise
      ierr = 0
      status = 0

c User info, if requested
      message = 'using '//subname//' '//version
      call wtinfo(chatter,15,1,message)

c Get necessary keywords
c NAXIS2 ...
      status = 0
      call ftgkyj(iunit,'NAXIS2',ienerg,comm,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     &		'reading NAXIS2 keyword')
        ierr = 4
        goto 987
      ENDIF

c HDUCLAS3 ...
      hduclas3 = '  '
      status = 0
      call ftgkys(iunit,'HDUCLAS3',hduclas3,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUCLAS3 keyword')

c HDUVERS2 ...
      hduvers2 = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',hduvers2,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUVERS2 keyword')
c If HDUVERS2 < 1.2.0, then we have a potential problem regarding what the 
c first legal channel is for this detector. This will require a check later 
c so set a flag. 
	if((hduvers2.EQ.'1.0.0').or.(hduvers2.EQ.'1.1.0')) then
		qflchan_prob = .true.
	else
		qflchan_prob = .false.
	endif

c RMFVERSN ...
      rmfversn = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',rmfversn,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUVERS2 keyword')
      IF (rmfversn.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'RMFVERSN',rmfversn,comm,status) 
        call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading RMFVERSN keyword')
      ENDIF

c TELESCOP ...
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading TELESCOP keyword')
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF 	

c INSTRUME ...
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading INSTRUME keyword')
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF         

c FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading FILTER keyword')
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF         

c DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading DETNAM keyword')
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c EFFAREA ...
      status = 0
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading EFFAREA keyword')
      IF (status.NE.0) THEN
        areascal = 1
      ENDIF

c LO_THRESH ...
      status = 0
      call ftgkye(iunit,'LO_THRESH',lo_thresh,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading LO_THRESH keyword')
      IF (status.NE.0) THEN
        lo_thresh = 0
      ENDIF

c DETCHANS ...
      status = 0
      call ftgkyj(iunit,'DETCHANS',ichan,comm,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading DETCHANS keyword')
        ierr = 1
        goto 987
      ENDIF

c CHANTYPE
      status = 0
      call ftgkys(iunit,'CHANTYPE',chantype,comm,status)
      if(status.NE.0) then
        call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading CHANTYPE keyword from RMF')
	call wtinfo(chatter,1, 1,'setting CHANTYPE = UNKNOWN')
        chantype = 'UNKNOWN'
        status= 0
      endif


c Reasure user, if requested
	call wtinfo(chatter,20,1,'read all the keywords')
	call wtinfo(chatter,20,1,'reading the data')

c Get the data (and necessary extras)
c ENERG_LO ...
      frow = 1
      felem = 1
      status = 0
      call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding ENERG_LO column')
         ierr = 4
         goto 987
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_lo,
     &            anyflg,status)      
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading ENERG_LO column')
        ierr = 1
        goto 987
      ENDIF

c ENERG_HI ...
      status = 0
      call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding ENERG_LO column')
         ierr = 4
         goto 987
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_hi,
     &            anyflg,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     &      'Problem reading ENERG_HI column')
        ierr = 1
        goto 987
      ENDIF      



 987  if(ierr.NE.0) then
        print*,ierr
        call wterrm(subname, version, ' Fatal - aborting')
      else
        call wtinfo(chatter,20,1, 
     &      'successfully read RSP_MATRIX data')
      endif

      return
      end
c ----------------------------------------------------------------------
c     END OF RDRMF3
c ----------------------------------------------------------------------

*From:	47413::MCCAMMON     11-MAY-1992 16:33:40.35
*To:	NSSDC::STPMODELS
*CC:	
*Subj:	sigism.for
*
C----------------------------------------------------------------------
C
      FUNCTION SIGISM(ENERGY)  
C
C     Reference:
C      Robert Morrison and Dan McCammon
C      Ap.J., vol. 270,  p. 119 (1983).
C
C
C     Description: 
C
C     This function implements the approximation of Morrison and
C     McCammon (1983) to the interstellar photoelectric absorbtion
C     cross-section.  ENERGY is in eV and the resultant cross-section
C     is in cm**2/hydrogen atom.  Abundances of other elements relative to
C     hydrogen are appropriate for the interstellar medium in the solar
C     neighborhood (see reference for discussion).
C
C
C     Deficiencies:
C     Works only in the range of energy from 30 eV to 10,000 eV.
C       No bounds checking on energy range.
C
C     Bugs:
C     None known -- please report any problems to authors
C  
C     Authors:
C     Dan McCammon               (47413::MCCAMMON)
C     Dick Edgar                 (47413::EDGAR)
C
C     History:
C     25.1.85 - original (By Dick Edgar)
C     19.1.92  - modified format and output units (McCammon)
C
C     Parameters:
C     ENERGY - photon energy in eV
C
C     Type Definitions:
C      IMPLICIT NONE
C
C     Local variables:
      INTEGER I
C          (index for energy interval)
      REAL E
C          (photon energy in keV)

C     Import:
      REAL ENERGY
C          (energy in eV)

C     Export:
      REAL SIGISM
C          (effective cross section in cm**2/H atom)

C     Local constants:


      REAL EMAX(14)
C          (edge energies where polynomial changes -- in keV)
      REAL C0(14)
C          (zero-order polynomial coefficients for 14 energy intervals)
      REAL C1(14)
C          (1st-order polynomial coefficients)
      REAL C2(14)
C          (2nd-order polynomial coefficients)
C
      data emax/.100,.284,.400,.532,.707,.867,1.303,
     #    1.840,2.471,3.210,4.038,7.111,8.331,10.0/ 
      data c0/17.3,34.6,78.1,71.4,95.5,308.9,120.6,141.3,
     #    202.7,342.7,352.2,433.9,629.0,701.2/  
      data c1/608.1,267.9,18.8,66.8,145.8,-380.6,169.3, 
     #    146.8,104.7,18.7,18.7,-2.4,30.9,25.2/ 
      data c2/-2150.,-476.1,4.3,-51.4,-61.1,294.0,-47.7,
     #    -31.5,-17.0,0.0,0.0,0.75,0.0,0.0/ 
C


C     Start:
C
      E=energy/1.e3    
C          (convert to keV)
      do 100 i=1,14  
        if (E .lt. Emax(i)) goto 200
100   continue  
      i=14
200   sigism=(c0(i)+c1(i)*E+c2(i)*E*E)/E**3 * 1.E-24
      return
      end

C =========================================================================

      subroutine fxwrite (string, unit)
      implicit none
      character*(*) string
      integer unit
      integer lnblnk
      write (unit,'(a)') string(1:lnblnk(string))
      return
      end

      integer function clenact (string)
      implicit none
      character*(*) string
      clenact = len(string)
      return
      end

      subroutine crmvblk (string)
      implicit none
      character*(*) string

      call rmblanks(string)
      return
      end

      subroutine crmvlbk (string)
      implicit none
      character*(*) string

      call rmblanks(string)
      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
*+WTINFO
        subroutine wtinfo(chatter, wtchatter, level, string)

        IMPLICIT NONE
        integer level, chatter, wtchatter
        character*(*) string
c 
c Description:
c  Noddy routine which passes string and level onto the standard wtout 
c subroutine if chatter.GE.wtchatter
c
c Passed parameters
c  CHATTER       i   : (int) actual chatter flag
c  WTCHATTER     i   : (int) chatter flag at & or above which wtout called
c  LEVEL         i   : (int) importance level (see above)
c  STRING        i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  subroutine WTOUT      : (CALLIB) Standard roslib/callib string writer
c
c Compilation & Linking
c  link with CALLIB 
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c       character*7 version
c       parameter (version = '1.0.0')
*- 

        if(chatter.ge.wtchatter) then
                call wtout(level, string)
        endif

        return
        end
c -------------------------------------------------------------------

*+WTOUT
        subroutine wtout(level, string)

	IMPLICIT NONE
	integer level
        character*(*) string
c 
c Description:
c  Writes callib/roslib standard message string(s) to STDOUT, with the 
c format controled by the level parameter:
c 	level = 0 	high level message 
c       level = 1       moderate level message
c	level = 2 	low level message
c
c Passed parameters
c  LEVEL         i   : (int) importance level (see above)
c  STRING	 i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  function CLENACT      : (CALLIB) Returns actual length of string
c  subroutine CRMVLBK    : (CALLIB) Removes leading blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Ian M George     (1.1.0: 1996 Feb 06) lobbed in the islop stuff
c  Ian M George     (1.2.0: 1996 Sep 17) removed calls to crmvlbk to prevent
c                                        problems under Solaris 2.2
c	character*7 version
c	parameter (version = '1.2.0')
*- 
c Internals 
	integer istart, istop, strlen
	integer clenact, ifront, i, islop
	character*11 front, blank
	character*80 outstr

c Initialize
	blank = '           '
	strlen = 0	
	istart = 1
	istop = 0
	islop = 10

c Remove all leading blanks from i/p string & check-out size
c	call crmvlbk(string)
	strlen = clenact(string)

c Sort out the level
	if(level.eq.0) then
		front = ' '
		ifront = 1
	elseif(level.eq.1) then
		front = ' ... '
		ifront = 5
	elseif(level.eq.2) then
		front = ' ...... '
		ifront = 8
	else 
		front = ' ......... '
		ifront = 11
	endif

c Dump the first line
	istop = MIN(strlen,80 - ifront)
	if((istop.LT.strlen).and.
     &	   (string(istop+1:istop+1).NE.' ')) then
	   do i = istop,istop-islop,-1
	     if(string(i:i).EQ.' ') then
		istop = i
		goto 122
	     endif	
	   enddo
	endif
122	outstr = front(:ifront)//string(1:istop)
	call fcecho(outstr)

c Return if we've finished
123	if(istop.GE.strlen) then
		return
	else
125		istart = MAX(1,istop+1)
	        if((string(istart:istart).EQ.' ').and.
     &		  (istart+1.LT.strlen)) then
		  istop = istop + 1
		  goto 125
	        endif
		istop = MIN(strlen,istop + 80 - 9)
		if((istop.LT.strlen).and.
     &	   	  (string(istop+1:istop+1).NE.' ')) then
	   	  do i = istop,istop-islop,-1
	     	     if(string(i:i).EQ.' ') then
			istop = i
			goto 124
	     	     endif	
	   	  enddo
	        endif
124		outstr = blank(:ifront)//string(istart:istop)
		call fcecho(outstr)
		go to 123
	endif

	end
c -------------------------------------------------------------------
C******************************************************************************
C SUBROUTINE:
C      fcecho
C
C DESCRIPTION:
C      This subroutine provides a single point to send text to the
C      terminal. This routine should be modified when a new host
C      environment is used.
C
C AUTHOR/DATE:
C      Kent Blackburn  11/5/91
C
C MODIFICATION HISTORY:
C
C  11/28/94 EAG call fxwrite, which in turn calls umsput
C
C NOTES:
C      fcecho uses F77/VOS like calls for terminal I/O
C
C USAGE:
C      call fcecho(string)
C
C ARGUMENTS:
C      string - text string sent to terminal
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine umsput - put message
C
C******************************************************************************
      subroutine fcecho(string)

      character*(*) string
      integer dest,prio,irafsts

      dest = 1
      prio = 0
      irafsts = 0

c write to STDOUT and logfile
      call fxwrite (string, 6)
c      call logstr(string)
c      call umsput(string,dest,prio,irafsts)
      return
      end

*+WTERRM
        subroutine wterrm(subrout, version, string)

        IMPLICIT NONE
        character*(*) subrout, version, string
c 
c Description:
c  Writes callib/roslib standard error message string(s) to STDOUT.
c  Current format is approximately:
c       ' ERROR - '//subrout//version//string
c  but with a few extra bits of punctuation, plus some attempt to 
c  handle strings greater than 80 characters.
c
c Passed parameters
c  SUBROUT       i   : (char) Name of the subroutine from which wterr called
c  VERSION       i   : (char) Version of SUBROUT
c  STRING        i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  function CLENACT      : (CALLIB) Returns actual length of string
c  subroutine CRMVLBK    : (CALLIB) Removes leading blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Keith Arnaud     (1.1.0: 1996 Aug 21) added tsubrout, tversion, tstring
c                                        internal variables to prevent probs
c  					 under Solaris 2.2 when i/p string 
c    					 starts with leading spaces 
c       character*7 version
c       parameter (version = '1.1.0')
*- 
c Internals 
        integer sublen, verlen, strlen
        integer istart, istop, str1len
        integer clenact
        character*80 outstr
        character*255 tsubrout, tversion, tstring

C Copy the passed parameters into temporary strings to avoid any problems
C with modifying constant strings

        tsubrout = subrout(:MIN(len(tsubrout),len(subrout)))
        tversion = version(:MIN(len(tversion),len(version)))
        tstring  = string(:MIN(len(tstring),len(string)))

c Initialize
        sublen = 0
        verlen = 0
        strlen = 0      
        istart = 1
        istop = 0
        str1len = 0

c Remove all leading blanks from i/p strings
        call crmvlbk(tsubrout)
        call crmvlbk(tversion)
        call crmvlbk(tstring)

c Check out the size of each character string
        sublen = clenact(tsubrout)
        verlen = clenact(tversion)
        strlen = clenact(tstring)

c Work out the 1st bit of the first line (up to where string will begin)        
        outstr = ' ERROR - '//tsubrout(:sublen)//
     &          ' '//tversion(:verlen)//': '
        str1len = clenact(outstr) + 1

c Dump the first line
        istop = MIN(strlen,80 - str1len)
        outstr = outstr(:str1len)//tstring(1:istop)
        call fcecho(outstr)

c Return if we've finished
123     if(istop.GE.strlen) then
                return
        else
                istart = MAX(1,istop+1)
                istop = MIN(strlen,istop + 80 - 9)
                outstr = '         '//tstring(istart:istop)
                call fcecho(outstr)
                go to 123
        endif

        end
c -------------------------------------------------------------------



*+WTFERR
        subroutine wtferr(subrout, version, status, string)

	IMPLICIT NONE
        character*(*) subrout, version, string
	integer status
c 
c Description:
c  Writes callib/roslib standard error message string(s) to STDOUT, 
c  appending the FITSIO error message corresponding to the code 
c  passed down via status
c
c Passed parameters
c  SUBROUT       i   : (char) Name of the subroutine from which wterr called
c  VERSION       i   : (char) Version of SUBROUT
c  STATUS        i   : (int) FITSIO status flag
c  STRING	 i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  subroutine FTGERR     : (FITSIO) Gets FITSIO error string from status
c  subroutine FTVERS     : (FITSIO) Gets FITSIO version number
c  subroutine WTINFO     : (CALLIB) Writes callib/roslib info message
c  subroutine WTERRM     : (CALLIB) Writes callib/roslib error message
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c	character*7 version
c	parameter (version = '1.0.0')
*- 
c Internals 
	character*80 ftsmsg
	character*160 message
	character*6 cftsver	
	real ftsver

c Return if there's no error
	if(status.EQ.0) return

c Dump the user-defined error message
	call wterrm(subrout, version, string)
	
c Get & dump the FITSIO message & version
	call ftgerr(status, ftsmsg)
	call ftvers(ftsver)	
	write(cftsver,'(f6.3)') ftsver
	message = 'fitsio'//cftsver//' error message: '//ftsmsg
	call wtinfo(1,1,1,message)

	return
	end
c -------------------------------------------------------------------
*+WTFWRN
        subroutine wtfwrn(subrout, version, chatter, wtchatter, 
     &		status, string)

	IMPLICIT NONE
        character*(*) subrout, version, string
	integer status, chatter, wtchatter
c 
c Description:
c  Writes callib/roslib standard warning message string(s) to STDOUT, 
c  appending the FITSIO error message corresponding to the code 
c  passed down via status
c
c Passed parameters
c  SUBROUT       i   : (char) Name of the subroutine from which wterr called
c  VERSION       i   : (char) Version of SUBROUT
c  CHATTER       i   : (int) actual chatter flag
c  WTCHATTER     i   : (int) chatter level at or above which messages written
c  STATUS        i   : (int) FITSIO status flag
c  STRING	 i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  subroutine FTGERR     : (FITSIO) Gets FITSIO error string from status
c  subroutine FTVERS     : (FITSIO) Gets FITSIO version number
c  subroutine WTINFO     : (CALLIB) Writes callib/roslib info message
c  subroutine WTWARM     : (CALLIB) Writes callib/roslib warning message
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c	character*7 version
c	parameter (version = '1.0.0')
*- 
c Internals 
	character*80 ftsmsg
	character*160 message
	character*6 cftsver	
	real ftsver

c Return if there's no error
	if(status.EQ.0) return

c Return if running in silent mode
	if(chatter.LT.wtchatter) return

c Dump the user-defined error message
	call wtwarm(subrout, version, chatter, wtchatter, string)
	
c Get & dump the FITSIO message & version
	call ftgerr(status, ftsmsg)
	call ftvers(ftsver)	
	write(cftsver,'(f6.3)') ftsver
	message = 'fitsio'//cftsver//' error message: '//ftsmsg
	call wtinfo(chatter,wtchatter,1,message)

	return
	end
c -------------------------------------------------------------------
*+WTWARM
        subroutine wtwarm(subrout, version, chatter, wtchatter, string)

	IMPLICIT NONE
	integer chatter, wtchatter
        character*(*) subrout, version, string
c 
c Description:
c  Writes callib/roslib standard warning message string(s) to STDOUT.
c  Current format is approximately:
c  	' WARNING - '//subrout//version//string
c  but with a few extra bits of punctuation, plus some attempt to 
c  handle strings greater than 80 characters.
c
c Passed parameters
c  SUBROUT       i   : (char) Name of the subroutine from which wterr called
c  VERSION       i   : (char) Version of SUBROUT
c  CHATTER       i   : (int) chatter flag (nowt written if chatter = 0)
c  WTCHATTER     i   : (int) chatter flag at or above which string written
c  STRING	 i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  function CLENACT      : (CALLIB) Returns actual length of string
c  subroutine CRMVLBK    : (CALLIB) Removes leading blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original, based on wterrm (1.0.0)
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Ian M George     (1.1.0: 1996 Sep 17) removed calls to crmvlbk to prevent 
c					 problems under Solaris 2.2
c	character*7 version
c	parameter (version = '1.1.0')
*- 
c Internals 
	integer sublen, verlen, strlen
	integer istart, istop, str1len
	integer clenact
	character*80 outstr

c Initialize
	sublen = 0
	verlen = 0
	strlen = 0	
	istart = 1
	istop = 0
	str1len = 0

c Return if chatter flag is zero
	if(chatter.LT.wtchatter) return

c Remove all leading blanks from i/p strings
c	call crmvlbk(subrout)
c	call crmvlbk(version)
c	call crmvlbk(string)

c Check out the size of each character string
	sublen = clenact(subrout)
	verlen = clenact(version)
	strlen = clenact(string)

c Work out the 1st bit of the first line (up to where string will begin)	
	outstr = ' WARNING - '//subrout(:sublen)//
     &		' '//version(:verlen)//': '
	str1len = clenact(outstr) + 1

c Dump the first line
	istop = MIN(strlen,80 - str1len)
	outstr = outstr(:str1len)//string(1:istop)
	call fcecho(outstr)

c Return if we've finished
123	if(istop.GE.strlen) then
		return
	else
		istart = MAX(1,istop+1)
		istop = MIN(strlen,istop + 80 - 11)
		outstr = '           '//string(istart:istop)
		call fcecho(outstr)
		go to 123
	endif

	end
c -------------------------------------------------------------------
