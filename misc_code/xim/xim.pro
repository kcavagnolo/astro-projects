;;;;;;;;;;;;;;;;;;;;;;;;;;;
; XIM: WRAPPER SCRIPT ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro xim,d,t,vx,vy,vz,x,y,z,dir,redshift,$
        egrid,exptime,filename,native=native,$
        metallicity=metallicity,$
        fillingfactor=fillingfactor,nh=nh,cosmo=cosmo,$
        telescope=telescope,instrument=instrument,$
        x_pix=x_pix,y_pix=y_pix,ang_res=ang_res,$
        response=response,arf=arf,$
        spec_model=spec_model,spec_pars=spec_pars,$
        background=background,tbins=tbins,$
        splice=splice,split=split,projfile=projfile,$
        overwrite=overwrite,verbose=verbose,$
        vturb=vturb,center=center,rdcenter=rdcenter,$
        aimpoint=aimpoint,marx=marx,$
        gratings=gratings,roll=roll,pileup=pileup,$
        evts=evts,psf=psf

;
; Sebastian Heinz, 12/15/2008
;
; con_x initializes cosmology and calls relevant routines to calculate
; simulated X-ray spectrum
;
; CALLING SEQUENCE:
;
; pro xim,d,t,v,x,y,z,dir,redshift,$
;         egrid,exptime,filename,cosmo=cosmo,$
;         telescope=telescope,instrument=instrument,$
;         x_pix=x_pix,y_pix=y_pix,ang_res=ang_res,$
;         response=response,arf=arf,metallicity=metallicity,$
;         fillingfactor=fillingfactor,nh=nh,$
;         spec_model=spec_model,tbins=tbins,$
;         splice=splice,split=split,projfile=projfile,$
;         overwrite=overwrite,verbose=verbose,$
;         vturb=vturb,center=center,rdcenter=rdcenter,$
;         aimpoint=aimpoint,marx=marx,$
;         gratings=gratings,pileup=pileup
;
; input:  
;         d:    density array in cm^(-3) is defined as sqrt(n_H*n_e), i.e., the
;               square root of the product of electron and hydrogen density.
;
;         t:    temperature array in Kelvin (same dimensions as d).
;
;         vx:   x-component of the velocity in cm/s (same dimensions as d).
;
;         vy:   y-component of the velocity in cm/s (same dimensions as d).
;
;         vz:   z-component of the velocity in cm/s (same dimensions as d).
;
;         x:    x-coordinate of array (edge values) in cm
;               must have dimension d(*,0,0)+1, e.g., if d=[1,2,3],
;               x=[0.3,0.8,1.1,5.0]; must be monotonically increasing.
;
;         y:    y-coordinate of array (in cm), must have dimension of
;               d(0,*,0)+1.
;
;         z:    z-coordinate of array (in cm), must have dimension of
;               d(0,0,*)+1.
;
;         dir:  line-of-sight projection direction, either as scalar (x:1,
;               y:2, z:3) or 3D vector of line-of-sight direction.
;
;         redshift: proper redshift to source (for distance calculation and
;               proper cosmological red-shifting, angular size, and dimming),
;               using Hubble constant (70 km/s/Mpc is default, or as provided
;               by keyword cosmo).
;
;         egrid: spectral energy grid (edge values) for which spectrum should
;               be calculated for; in keV. Output spectrum will have dimensions
;               of egrid minus one.
;
;         exptime: exposure time in ksec (i.e., 10^3 sec)
;
;         filename: basename of file for output to be written in (with suffix
;               *.dat added) and specral grid file will be written with suffix
;               *_spec_model.dat added
;
; output:
;         all output will be written into three output files: a raw
;         image-spectral cube (not convolved with telescope response)
;         is written out in filename_spec_data.dat. The virtual
;         observation is written into filename.dat.  These are IDL
;         data files and can be loaded into IDL using the
;         restore,filename.dat command.  The third file is a FITS
;         events file of the virtual observation.
;
;         The first file contains the spectral cube (img), the image
;         sky coordinates imx and imy (in degrees) relative to center,
;         the file redshift, image coordinate axis size (n1, n2), the
;         energy grid of the raw spectral cube (logegrid), the output
;         energy grid (egrid), the three cosmological distance
;         measures (proper, angular, luminosity distance as pdist,
;         adist, ldist), and the coordinates of the reference point
;         (center, rdcenter).
;
;         The second file contains the spectral cube folded through
;         the telescope response with Poisson noise (simspec) and
;         without Poisson noise (xrayspec), the energy grid of the
;         array (egrid), the image coordinates in detector (imx,imy)
;         and sky coordiantes (ra,dec in degrees), the cosmological
;         parameters used, exposure time, and telescope information.
;         
;         The FITS events file filename.fits conforms to the OGIP
;         convention for binary events files and can be read into
;         tools like ds9.
;
; keywords:
;
;         native: If set, the output energy grid will be the native instrument
;                resolution (from the channel map in the response matrix),
;                using the upper and lower bound of egrid to bound the
;                channels for which the calculation will be performed
;
;         metallicity: metal abundance for elements with Z>2. format: array
;                (same dimensions as d) or scalar, in solar units (default is
;                unity)
;
;         fillingfactor: array (same dimensions as s) or scalar, indicating the
;                volume filling factor of the emitting material (default is
;                unity)
;
;         nh:    scalar absorbing hydrogen column density in units of 1e20
;                cm^-2 (default is zero).
;
;         cosmo: cosmological parameters (if not specified, concordance values
;                are assumed:
;                cosmo=[omega_lambda,omega_matter,omega_naught,H_naught],
;                default values are [0.7,0.3,1.0,70]
;
;         telescope: string containing the name of the telescope. Currently,
;                supported values are "IXO", "Chandra", and
;                "CUSTOM"
;
;         instrument: detector to be used. Currently supported values are
;                "XMS" for IXO and "ACIS-S" and "ACIS-I" for Chandra
;                (if the MARX keyword is set, "HRC-S" and "HRC-I" are also
;                available) "TES" and "WFI" for XEUS
;
;         x_pix: x pixel size (x-dir) in arcsec (default is 5.0" for IXO and
;                0.492" for Chandra)
;
;         y_pix: y pixel size (y-dir) in arcsec (default is 5.0" for IXO and
;                0.492" for Chandra)
;
;         ang_res: Angular resolution of telescope (default is 5" for IXO
;                and 0.5" for Chandra)
;
;         response: filename for user-provided response file (CAL/GEN/92-002 
;                format), default are the appropriate responses for the
;                respective instruments (using the cycle 10 responses for 
;                Chandra). The option "dummy" (default for "CUSTOM" telescope)
;                uses the identity matrix
;
;         arf:   optional filename for user provided ancilliary response file
;                (CAL/GEN/92-002 format), default are the appropriate arfs for
;                the respective instruments (using the cycle 10 responses for
;                Chandra).
;
;         spec_model: name of procedure to return spectral model grid;
;                default is the APEC code procedure apecgrid.  For
;                specifications of this procedure, see the README file
;
;         spec_pars: arbitrary variable or structure that can be passed to
;                spec_model via the common block "spec" for user-defined
;                spectral models.
;
;         background: name of procedure to return backgroun spectrum (sky and
;                instrumental background) for energy grid used in calculation
;
;         tbins: Number of logarithmic bins per decade in temperature
;                for spectral model. Default: 200 bins over 3 decades
;                in temperature. 
; 
;         splice: If data were split in prior step, set splice=1 and xim will
;                read in the previously split data files and combine them (the
;                information necessary is contained in the filename_info.dat
;                file). Default is splice=0
;
;         split: for large grids and many spectral bins, computation
;                can be split up by spectral range; set to
;                split=[proc,Procs], where proc is the processor
;                number and Procs the total number of
;                processor. Computation will proceed only through
;                spectral projection. To convolve the combined spectra
;                with the instrument response, run x_ray_sim with
;                keyword splice set to the total number of processors.
;
;         projfile: name of line-of-sight projection saved by previous
;                run to convolve with telescope response (avoids
;                re-running expesnsive spatial projection)
;
;         overwrite: set this keyword to overwrite the input variables with
;                rotated and/or rescaled arrays, rather than restoring the
;                original input variables upon completion (default is
;                overwrite=0)
;
;         verbose: if set, spit out running info on progress
;                (default); NOTE: set this to verbose=0 or verbose=1
;                if screen output is piped to disk file (for verbose >
;                1, performance will be seriously degraded).
;
;         vturb: turbulent Gaussian velocity broadening
;
;         center: reference coordinate point for calculating image
;                coordinates; default is lower-left corner of image;
;                two-element vector, units: cm
;
;         rdcenter: right-ascention (RA) and declination (DEC) of
;                reference point center; used for calculating sky
;                coordinates of image; two-element vector, units: degrees
;
;         aimpoint: Specifies the aimpoint of the telescope as a
;                vector (RA,DEC) in degrees. Default is [0,0]
;
;         marx:  only available in conjunction with telescope
;                "Chandra". Specifies that MARX ray tracing simulation should
;                be performed (default is marx=0, set to marx=1 to perform a
;                ximulation with MARX). Requires that MARX is set up
;                on the system and in the path.
;
;         gratings: only available in conjunction with MARX keyword. Specifies
;                whether and which gratings are supposed to be used. Supported
;                values are "HETG", "LETG", or "NONE"; default: "NONE".
;
;         roll:  only available in conjunction with MARX keyword. Specifies
;                roll angle in degrees (float scalar, default: 0)
;
;         pileup: only available in conjunction with MARX keyword. Specifies
;                that pileup effects should be taken into account in the MARX
;                simulation; default is pileup=1, to turn of, set pileup=0
;
;         evts:  if set, an events FITS file will be written (default is
;                evts=0); This can be very computationally expensive.
;
;         psf:   string containing the name of a procedure to provide
;                user-supplied energy dependent psf model
;

common spec,userparameters
common telescope,dpixx,dpixy,resolution
common cosmo,omegam,omegal,omegar,omega,h0,pdist,adist,ldist

; Splitting up computation?
if keyword_set(split) then begin
    if split(0) gt split(1) then begin
        print,'Split values incorrect'
        goto,finish
    endif
    split=split
endif else begin
    split=0
endelse

; Check keyword for spectral procedure
if keyword_set(spec_model) then spec_model=spec_model else $
  spec_model='apecgrid'

; Check keyword for spectral parameters
if keyword_set(spec_pars) then userparameters=spec_pars else $
  userparameters=create_struct('n',0)

; Keywords for referencing coordinates
if keyword_set(aimpoint) then $
    aimpoint=aimpoint else aimpoint=[0.,0.]
if keyword_set(center) then $
    center=center else center=[0.,0.]
if keyword_set(rdcenter) then $
    rdcenter=rdcenter else rdcenter=[0.,0.]
if keyword_set(roll) then roll=roll else roll=0.
if keyword_set(native) then native=native else native=0
if keyword_set(evts) then evts=evts else evts=0

if keyword_set(verbose) then verbose=verbose else verbose=0
if verbose gt 1 then begin
    print,'WARNING: Setting verbose > 1 can lead to poor performance'
    print,'         if stdout is written to log file'
endif

; Absorption:
if keyword_set(nh) then nh=nh else nh=0.e
if keyword_set(vturb) then vturb=vturb else vturb=0.0
if keyword_set(filename) then filename=filename else filename=''
filenamein=filename

; Preserving variables?
if keyword_set(overwrite) then $
  overwrite=overwrite else $
  overwrite=0

; Check if telescope is supported
if keyword_set(telescope) then begin
    telescope=telescope 
    if (telescope ne "Chandra" and $
        telescope ne "IXO" and $
        telescope ne "CUSTOM") then begin
        print,'Telescope '+telescope+' not supported in current setup'
        goto,finish
    endif
endif else begin
    telescope="IXO"
    instrument="XMS"
endelse

print,'Telescope = ',telescope

; Check for marx keyword
if telescope eq "Chandra" then begin
    if keyword_set(marx) then marx=marx else marx=0
endif else begin
    if keyword_set(marx) then begin
        if marx eq 1 then print,'MARX not supported for telescope '+telescope
    endif
    marx=0
endelse


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Telescope specific calculations ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
; Chandra ;
;;;;;;;;;;;

if telescope eq "Chandra" then begin

    if marx ne 0 then begin
        if keyword_set(gratings) then gratings=gratings else gratings="NONE"
        if keyword_set(pileup) then pileup=pileup else pileup=1
    endif
    dpixx=0.492
    dpixy=0.492
    resolution=0.5

    if keyword_set(psf) then psf=psf else psf="chandrapsf"
    if keyword_set(instrument) then instrument=instrument else $
      instrument="ACIS-S"

    case instrument of
        "ACIS-S" : begin
            respf='aciss_aimpt_cy11.rmf'
            ancrf='aciss_aimpt_cy11.arf'
            bgf="chandra_acis_s_bg"
        end
        "ACIS-I" : begin
            respf='acisi_aimpt_cy11.rmf'
            ancrf='acisi_aimpt_cy11.arf'
            bgf="chandra_acis_i_bg"
        end
    endcase

endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; if IXO, pick appropriate response file ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if telescope eq "IXO" then begin
; initialize telescope (default: IXO)

    ; telescope specific settings
    focal_length=2.0d3
    resolution=5.0
    if keyword_set(psf) then psf=psf else psf="ixopsf"

    ; instrument specific settings
    case instrument of
        "XMS" : begin
            dlem=egrid(0)*(10.e^(min(alog10(egrid(1:*)/egrid(0:*)))) - 1.e)
            pixpitch_x=250.d-4
            pixpitch_y=250.d-4
            if dlem lt 0.001 then $
              respf='ixo_ucal_0p2_081030.rsp'
            if dlem lt 0.010 then $
              respf='ixo_ucal_0p5_081030.rsp' else $
              respf='ixo_ucal_5p0_081030.rsp'
            ancrf=""
            if native eq 1 then respf='ixo_ucal_0p5_081030.rsp'
            bgf="ixoxmsbg"
        end
        "WFI" : begin
            pixpitch_x=100.d-4
            pixpitch_y=100.d-4
            respf='IXO_CDF_hard_wfi_default.rsp'
            ancrf=""
            bgf="ixowfibg"
        end
    endcase        
    dpixx=pixpitch_x/focal_length*3600.*180./!pi
    dpixy=pixpitch_y/focal_length*3600.*180./!pi
endif
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; if CUSTOM, pick appropriate response file ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if telescope eq "CUSTOM" then begin
    if keyword_set(x_pix) then dpixx=x_pix else dpixx=1.0
    if keyword_set(y_pix) then dpixy=y_pix else dpixy=1.0
    if keyword_set(ang_res) then $
      resolution=ang_res else resolution=1.0
    if keyword_set(psf) then psf=psf else psf="gausspsf"
    respf="dummy"
    ancrf=""
    bgf="dummybg"
endif

if keyword_set(background) then $
  background=background else background=bgf
if keyword_set(x_pix) then dpixx=x_pix else dpixx=0.492
if keyword_set(y_pix) then dpixy=y_pix else dpixy=0.492
if keyword_set(ang_res) then $
  resolution=ang_res else resolution=0.5
    
resppath='$XIMPATH/RESPONSES/'

; full grid

; read response for padding
oegrid=egrid
read_resp,resppath+respf,resppath+ancrf,egrid,oegrid,resp,channel,$
    verbose=verbose,native=native,chmin=chmin,chmax=chmax,ermin=ermin,ermax=ermax

; padd energy grid for instrument resolution
rnm=resp.(0)
respmax=resp.(3 + rnm*2)
respmin=resp.(5)
ttmin=total(triagonal(n_elements(respmin))*$
            (replicate(1.,n_elements(respmin))#respmin),2)
ttmin/=max(ttmin)
ttmax=total(triagonal(n_elements(respmax))*$
            (replicate(1.,n_elements(respmax))#respmax),2)
ttmax/=max(ttmax)
omin=max([0,min([n_elements(respmin),min(where(ttmin ge 0.99))])])
omax=max([0,min([n_elements(respmax),max(where(ttmax le 0.01))])])
delmin=(resp.(3))((resp.(4))(omin)) - (resp.(3))(min(resp.(4)))
delmax=(resp.(3))(max(resp.(2 + 2*rnm))) - (resp.(3))((resp.(2 + 2*rnm))(omax))

; native e-resolution? replace energy grid with instrument binning
if native ne 0 then egrid=oegrid

; set splice and projfile variables from keywords
if keyword_set(splice) then splice=splice else splice=0
if keyword_set(projfile) then projfile=projfile else projfile=" "

; Splicing files?
if splice ne 0 then begin
    overwrite=1
    goto,splicepoint
endif

; Spatial projection file provided?
if projfile ne " " then begin
    overwrite=1
    goto,projpoint
endif

; Check if ATOMDB is set up
help,/functions,out=heo
if ((where(strpos(heo,'CALC_SPECTRUM') ne -1))(0) eq -1) then begin
    print,'Please initiate the ATOMDB by running @init_atomdb_idl '+$
      'from '
    print,'       the command line before running x_ray_sim'
    goto,finish
endif    

; Check dimensionality of data arrays
if (max((size(d))(0:3)-(size(t))(0:3)) gt 0 or $
    max((size(d))(0:3)-(size(vx))(0:3)) gt 0 or $
    max((size(d))(0:3)-(size(vy))(0:3)) gt 0 or $
    max((size(d))(0:3)-(size(vz))(0:3)) gt 0) then begin
    print,'Error: Data arrays not of identical size'
    goto,finish
endif

if (size(d))(0) gt 3 then begin
    print,'Error: Data arrays have too many dimensions'
    goto,finish
endif

;rotate or transpose? Calculate line-of-sigh velocity
dir/=sqrt(total(dir^2))
if n_elements(dir) eq 3 then begin
    vlos=vx*d(0) + vy*d(1) + vz*d(2)
    if dir(0) ne 0. then begin
        if (dir(1) eq 0. and dir(2) eq 0.) then dir=1
    endif else begin
        if dir(1) eq 1. then begin
            if (dir(0) eq 0. and dir(2) eq 0.) then dir=2
        endif else begin
            if dir(2) eq 1. then if (dir(0) eq 0. and dir(1) eq 0.) then dir=3
        endelse
    endelse
endif

; make sure we have 3D arrays
if (size(d))(0) eq 1 then begin
    d=reform(d,(size(d))(1),1,1)
    vlos=reform(vlos,(size(d))(1),1,1)
    t=reform(t,(size(d))(1),1,1)
endif else begin
    if (size(d))(0) eq 2 then begin
        d=reform(d,(size(d))(1),(size(d))(2),1)
        vlos=reform(vlos,(size(d))(1),(size(d))(2),1)
        t=reform(t,(size(d))(1),(size(d))(2),1)
    endif
endelse

; store array sizes to maintain shape
n1=(size(d))(1)
n2=(size(d))(2)
n3=(size(d))(3)

; Check dimensionality of coordinate vectors
if (((size(d))(1) ne (size(x))(1)-1) or $
    ((size(d))(2) ne (size(y))(1)-1) or $
    ((size(d))(3) ne (size(z))(1)-1)) then begin
    print,'Error: Coordinate arrays x,y,z not of correct size.'
    print,'Set x,y,z to coordinate values of cell boundaries.'
    print,'Proper size of x is (size(d))(1)+1'
    print,'Proper size of y is (size(d))(2)+1'
    print,'Proper size of z is (size(d))(3)+1'
    goto,finish
endif

; Metallicity:
if keyword_set(metallicity) then begin
    metallicity=metallicity 
    if (n_elements(metallicity) gt 1) then $
      if (max((size(d))(0:3)-(size(metallicity))(0:3)) gt 0 or $
          max((size(d))(0:3)-(size(metallicity))(0:3)) gt 0) then begin
        print,'Error: Metallicity array not of correct size'
        goto,finish
    endif
endif else begin
    metallicity=1.0
endelse

; Filling factor:
if keyword_set(fillingfactor) then begin
  fillingfactor=fillingfactor
    if (n_elements(fillingfactor) gt 1) then $
      if (max((size(d))(0:3)-(size(fillingfactor))(0:3)) gt 0 or $
          max((size(d))(0:3)-(size(fillingfactor))(0:3)) gt 0) then begin
        print,'Error: Filling factor array not of correct size'
        goto,finish
    endif
endif else begin
  fillingfactor=1.0
endelse

; initialize cosmology
if keyword_set(cosmo) then cosmo=cosmo else cosmo=[0.7,0.3,1.0,70.0]
h0=cosmo(3)
omegam=cosmo(0)
omegal=cosmo(1)
omega=cosmo(2)
omegar=omega-omegam-omegal
calcdist,redshift

; Store original input data so as not to overwrite it.
if overwrite eq 0 then begin
    d1=reform(d,n1,n2,n3)
    t1=reform(t,n1,n2,n3)
    x1=x
    y1=y
    z1=z
    metallicity1=metallicity
    fillingfactor1=fillingfactor
    nh1=nh
endif

; now do rotation (if necessary)
if n_elements(dir) eq 3 then begin
    
; regularize grid to lowest resolution
    del=[(x(1:*)-x(0:*)),(y(1:*)-y(0:*)),(z(1:*)-z(0:*))]
    if (max(abs(del))-min(abs(del)))/min(abs(del)) gt 1.e-2 then begin
        if verbose gt 0 then print,'Stand by: Regridding to uniform resolution before rotation to LOS'
        maxdel=max(abs(del))
        cos=create_struct('x',x,'y',y,'z',z)
        for i=0,2 do begin
            co=cos.(i)
            nc=ceil((max(co) - min(co))/maxdel)
            cn=min(co) + findgen(nc)*maxdel/float(nc-1)
            regrid,d,co,d,cn,i,/average
            regrid,vlos,co,vlos,cn,i,/average
            regrid,t,co,t,cn,i,/average
            if nf gt 1 then begin
                regrid,fillingfactor,co,fillingfactor,cn,i,/average
            endif
            if nm gt 1 then begin
                regrid,metallicity,co,metallicity,cn,i,/average
            endif
            cos.(i)=co
        endfor
        x=cos.(0)
        y=cos.(1)
        z=cos.(2)
    endif

    nx=n_elements(x) & ny=n_elements(y) & nz=n_elements(z)
    nf=n_elements(fillingfactor)
    nm=n_elements(metallicity)

; determine rotation angles
    l1=sqrt(dir(0)^2 + dir(1)^2)
    if (dir(1) eq 0 and dir(1) le 0) then ang1=180. else $
      ang1=2.0*atan(dir(1)/(dir(0) + l1))*!radeg
    if l1 eq 0 then ang2=90. else ang2=atan(dir(2)/l1)*!radeg
; first rotation
    for i=0,nz-2 do begin
        d(*,*,i)=rot(d(*,*,i),ang1,missing=0)
        t(*,*,i)=rot(t(*,*,i),ang1,missing=0)
        vlos(*,*,i)=rot(vlos(*,*,i),ang1,missing=0)
        if nf gt 1 then $
          fillingfactor(*,*,i)=rot(fillingfactor(*,*,i),ang1,missing=0)
        if nm gt 1 then $
          metallicity(*,*,i)=rot(metaillicity(*,*,i),ang1,missing=0)
        if verbose ne 0 and i eq 0 then $
          print,format='($,"Rotating along LOS.... 000.00% complete")'
        if verbose ne 0 and i gt 0 then $
          print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
          float(i)/max([1.,float((nz-2) + (ny-2))])*100.
    endfor

; second rotation
    for i=0,ny-2 do begin
        if verbose ne 0 then $
          print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
          float(i+nz-2)/max([1.,float((nz-1) + (ny-1))])*100.
        d(*,i,*)=reform(rot(reform(d(*,i,*),nx-1,nz-1),$
                            ang2,missing=0),nx-1,1,nz-1)
        t(*,i,*)=reform(rot(reform(t(*,i,*),nx-1,nz-1),$
                            ang2,missing=0),nx-1,1,nz-1)
        vlos(*,i,*)=reform(rot(reform(vlos(*,i,*),nx-1,nz-1),$
                            ang2,missing=0),nx-1,1,nz-1)
        if nf gt 1 then $
          fillingfactor(*,i,*)=$
          reform(rot(reform(fillingfactor(*,i,*),nx,nz),$
                     ang2,missing=0),nx,1,nz)
        if nm gt 1 then $
          metallicity(*,i,*)=$
          reform(rot(reform(metallicity(*,i,*),nx,nz),$
                     ang2,missing=0),nx,1,nz)
    endfor
    if verbose ne 0 then $
      print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.0

; now we can project along coordinate axis.
    dir = 1

endif

if (dir ne 1 and dir ne 2 and dir ne 3) then begin
    print,'Error: direction vector does not have proper dimensions or values'
    goto,finish
endif

; transpose array to split along projection direction
case dir of
    1: begin
        tpm=[1,2,0]
        n=[n2,n3,n1]
        x2=y
        y2=z
        z2=x
    end
    2: begin
        tpm=[2,0,1]
        n=[n3,n1,n2]
        x2=z
        y2=x
        z2=y
    end
    3: begin
        tpm=[0,1,2]
        n=[n1,n2,n3]
        x2=x
        y2=y
        z2=z
    end
endcase

; store coordinates back into originals
x=x2
y=y2
z=z2

; transpose all relevant arrays
d=reform(transpose(d,tpm),n(0),n(1),n(2))
vlos=reform(transpose(vlos,tpm),n(0),n(1),n(2))
t=reform(transpose(t,tpm),n(0),n(1),n(2))

if n_elements(fillingfactor) gt 1 then $
  fillingfactor=reform(transpose(fillingfactor,tpm),n(0),n(1),n(2))
if n_elements(metallicity) gt 1 then $
  metallicity=reform(transpose(metallicity,tpm),n(0),n(1),n(2))

dir=3

; initialize energy grid variables for spectral calculations
; oversample energy grid by factor of 2 (so red-shift can be done on sub-bin
; level) and extend it to allow for Doppler shift
    maxv=max(vlos)+vturb
    minv=min(vlos)-vturb
    maxd=sqrt((3.e10 + maxv)/(3.e10 - maxv))
    mind=sqrt((3.e10 + minv)/(3.e10 - minv))
    
; initialize logarithmic energy grid for Doppler shift calculations
; use nyquist sampling (2x)
oversample=2.0
if native ne 0 and telescope eq "IXO" then oversample=1.0

; padding (stop at min and max energy of response)
emin=min([max([0.5*ermin,min(egrid-delmin)*mind]),ermax])
emax=min([max([ermin,max(egrid+delmax)*maxd]),2.0*ermax])
dlem=min(alog10(egrid(1:*)/egrid(0:*)))
enum=max([2,ceil(oversample*alog10(emax/emin)/dlem)])

; core energy grid
logegrid=emin*10.^(findgen(enum)/float(enum-1)*alog10(emax/emin))

; splitting array up for multiple processors?
if split(0) gt 0 then begin
    ; check for size
    if split(1) gt (size(d))(3) then begin
        print,'Grid size in split direction smaller than number of'
        print,'processors requested. Please reduce split(1) to a number'
        print,'smaller than or equal to '+$
          strtrim(string(format='(i4)',(size(d))(dir)),2)
        goto,finish
    endif 

    ; size of transposed array along split direction
    nz=(size(d))(3)
    nsplit=nz-1

    ; splitting indices
    nmin=max([0,min([nsplit,$
                     floor(float(split(0)-1)*$
                           float(nsplit+1)/float(split(1)))])])
    nmax=max([1,min([nsplit+1,$
                     floor(float(split(0))*$
                           float(nsplit+1)/float(split(1)))])])-1

    ; split the coordinate and data arrays
    z=reform(z(nmin:nmax+1),nmax-nmin+2)
    d=reform(d(*,*,nmin:nmax),n(0),n(1),nmax-nmin+1)
    vlos=reform(vlos(*,*,nmin:nmax),n(0),n(1),nmax-nmin+1)
    t=reform(t(*,*,nmin:nmax),n(0),n(1),nmax-nmin+1)
    if n_elements(metallicity) gt 1 then $
      metallicity=reform(metallicity(*,*,nmin:nmax),n(0),n(1),nmax-nmin+1)
    if n_elements(fillingfactor) gt 1 then $
      fillingfactor=reform(fillingfactor(*,*,nmin:nmax),n(0),n(1),nmax-nmin+1)

endif

; temperature range
tmin=min(t)+1.e-5
tmax=max([max(t),min(t)+2.e-5])

; suggestion: 200 bins over 3 orders of magnitude is a decent sampling
if keyword_set(tbins) then $
  tnum=max([2,tbins*(alog10(tmax)-alog10(tmin))]) else $
  tnum=max([1,floor(200.e*alog10(tmax/tmin)/alog10(1.e9/1.e6))])

; logarithmic temperature bins to cover parameter space
tgrid=tmin*10.e^(findgen(tnum+1)/float(tnum)*alog10(tmax/tmin))

; split? write separate files for sub-grids
if split(0) eq 1 then $
  save,egrid,dir,redshift,omegam,omegal,omegar,$
  omega,h0,pdist,adist,ldist,nh,split,$
  filename=filename+'_info.dat'

if split(0) gt 0 then filename+='_split_'+$
  strtrim(string(format='(i4.4)',split(0)),2)+'_'+$
  strtrim(string(format='(i4.4)',split(1)),2)
 
; initialize spectral model grid for temperature
call_procedure,spec_model,tgrid,logegrid*(1.0 + redshift),$
  sgrid,sgridzero,verbose=verbose

;run spectral projection
line_image,img,d,t,vlos,fillingfactor,metallicity,x,y,z,$
  redshift,sgrid,sgridzero,logegrid,egrid,tgrid,imx,imy,$
  filename=filename+'_spec_data.dat',verbose=verbose,$
  center=center,rdcenter=rdcenter

; stop here if splitting
if (split(0) ne 0) then goto,finish


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; done with splitting, continue only if not splitting ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; are we splicing back together?
splicepoint: if splice ne 0 then begin

   ; load in array
   if file_test(filename+'_info.dat') then begin
      restore,filename+'_info.dat' 
      splice=split(1)
   endif else begin
      print,'ERROR: No info file for split files exists.'
      print,'Please run xim with split=[1,1] to create this file'
      goto,finish
   endelse
   if verbose ne 0 then $
      print,format='($,"Splicing data......... 000.00% complete")'
   for i=0,splice-1 do $
      if (not file_test(filename+'_split_'+$
                        strtrim(string(format='(i4.4)',i+1),2)+'_'+$
                        strtrim(string(format='(i4.4)',splice),2)+$
                        '_spec_data.dat')) then begin
      print,' ' & print,'ERROR: some of the files to be spliced do not exist'
      goto,finish
      endif
   
   restore,filename+'_split_'+$
     strtrim(string(format='(i4.4)',1),2)+'_'+$
     strtrim(string(format='(i4.4)',splice),2)+$
     '_spec_data.dat'
   
   oimg=img
   
   nx=(size(img))(1)
   ny=(size(img))(2)
   nz=(size(img))(3)

   for i=2,splice do begin
       if verbose ne 0 then $
          print,format='($,A16,f6.2,"% complete")',$
          string(replicate(8b,16)),$
          float(i-1)/float(splice)*100.
         
       restore,filename+'_split_'+$
          strtrim(string(format='(i4.4)',i),2)+'_'+$
          strtrim(string(format='(i4.4)',splice),2)+$
          '_spec_data.dat'

       oimg+=img
   endfor

   img=oimg
   oimg=1.0

   if verbose ne 0 then $
      print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),$
      100.0
   egrid=float(egrid)

   for i=0,splice-1 do $
     spawn,'rm -f '+filename+'_split_'+$
     strtrim(string(format='(i4.4)',i+1),2)+'_'+$
     strtrim(string(format='(i4.4)',splice),2)+$
     '_spec_data.dat'
   
endif

; save spliced projected spectal cube for convolution with response
save,img,imx,imy,redshift,logegrid,egrid,$
   filename=filename+'_spec_data.dat'

; if keyword projfile is set, restore projected image
projpoint: if projfile ne " " then restore,projfile

    ; Perform response convolution
    ;
    ; MARX?
    if (telescope eq "Chandra" and marx ne 0) then begin
        marxxim,img,imx,imy,logegrid,rdcenter(0),rdcenter(1),filename,exptime,$
          gratings=gratings,detector=instrument,roll=roll,$
          pileup=pileup,verbose=verbose
    endif else begin
        ; Response provided by user? Override instrument defaults.
        if keyword_set(response) then respf=response
        if keyword_set(arf) then ancrf=arf
        
        ; output info
        if verbose eq 2 then begin
            print,'Resolution:  ',resolution
            print,'Pixel scale: ',dpixx,', ',dpixy
        endif

        ; spectral convolution and image re-gridding
        xray_proj,xrayspec,bgspec,img,imx,imy,xpix,ypix,ra,dec,nh,$
          logegrid,egrid,dpixx,dpixy,resolution,$
          telescope,resppath+respf,resppath+ancrf,channel,verbose=verbose,$
          filename=filename+'_x_ray_data.dat',$
          vturb=vturb,aimpoint=aimpoint,chmin=chmin,chmax=chmax,$
          native=native,psf=psf,background=background

        ; add Poisson noise
        poissonspec,xrayspec,bgspec,exptime,simspec,simbgspec,$
          simsourcespec,verbose=verbose

        ; save output
        save,simspec,simbgspec,simsourcespec,xrayspec,bgspec,egrid,channel,$
          imx,imy,xpix,ypix,ra,dec,cosmo,exptime,telescope,instrument,$
          redshift,exptime,rdcenter,aimpoint,roll,dpixx,dpixy,$
          chmin,chmax,respf,psf,ancrf,filename=filename+'.dat'

        ; make events file
        if evts ne 0 then make_evts,filename+'.dat',filename+'.fits',verbose

        ; make spectrum only if egrid is native
        if native ne 0 then $
          make_pi,channel,simspec,chmin,chmax,filename+'.pi',exptime,$
          telescope,instrument,respf,ancrf,native=native

    endelse

finish: print,'Done'
; preserving variable?
if overwrite eq 0 then begin
    d=d1
    t=t1
    x=x1
    y=y1
    z=z1
    metallicity=metallicity1
    fillingfactor=fillingfactor1
    nh=nh1
    filename=filenamein
endif

end
