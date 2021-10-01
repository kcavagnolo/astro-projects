pro add_to_catalog,system_name,redshift,catalog_file=catalog_file,spectra_dir=spectra_dir, $
                   caldb=caldb,Uprofile=Uprofile,Rprofile=Rprofile,Iprofile=Iprofile, $
                   psf=psf,gal_ra=gal_ra,gal_dec=gal_decx
;-----------------------------------------------------------------------
;
; Name: ADD_TO_CATALOG
;
; Purpose: Add cluster properties to a centralized catalog
;          
; Inputs:  system_name - Name of system (may include spaces)
;          redshift - redshift
;
; Optional: catalog_file - name of catalog fits file to use. If it does not
;			   exist, it will be created. 
;			   (Default =
;			   /home/rafferty/Catalog/catalog.fits)
;           caldb - caldb version (e.g., '3.2.0')
;	    spectra_dir - name of spectra directory. (Default = spectra)
;	    /cooling - add cooling rate to catalog
;	    /flux - add fluxes to catalog
;         
; Comments:            
;           
; Revision history:
;       written by DR, 2007-2-20
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 2) then begin
    print,string(7B),'CALLING SEQUENCE: ', $
      'add_to_catalog, system_name [, redshift, caldb=caldb, catalog_file=catalog_file, spectra_dir=spectra_dir,' + $
      'Uprofile=Uprofile, Rprofile=Rprofile, Iprofile=Iprofile]'
    return   
endif


;
; Set defaults
;
if (n_elements(catalog_file) eq 0) then catalog_file='/home/rafferty/Catalog/catalog.fits'
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'
if (n_elements(Uprofile) eq 0) then Uprofile='Uprofile.dat'
if (n_elements(Rprofile) eq 0) then Rprofile='Rprofile.dat'
if (n_elements(Iprofile) eq 0) then Iprofile='Iprofile.dat'
if (n_elements(psf) eq 0) then psf=0.0
if (n_elements(gal_ra) eq 0) then gal_ra=0.0
if (n_elements(gal_dec) eq 0) then gal_dec=0.0
num=100                         ; number of elements in each array


;
; Determine whether we are in X-ray or optical directory
;
test=findfile(Uprofile,count=numfiles)
if (numfiles eq 0) then xray=1 else xray=0


;
; First, do X-ray catalog
;
if (xray eq 1) then begin


;
; Check if source_mekal_flux.spectra, source_mkcflow_flux.spectra, and
; source_mkcflow exist
;
    mkfile=findfile(spectra_dir+'/source_mekal_flux.spectra',count=numfiles)
    if (numfiles eq 0) then mekal_flux=0 else mekal_flux=1
    mkcfile=findfile(spectra_dir+'/source_mkcflow_flux.spectra',count=numfiles)
    if (numfiles eq 0) then mkcflow_flux=0 else mkcflow_flux=1
    mkcffile=findfile(spectra_dir+'/source_mkcflow.spectra',count=numfiles)
    if (numfiles eq 0) then mkcflow_norm=0 else mkcflow_norm=1


;
; Read in data from "properties_deproj.dat"
;
    infile='./'+spectra_dir+'/properties_deproj.dat'
    readcol,infile,skipline=20,ax,akpc,kT,kTloerr,kThierr,n_e,n_eloerr,n_ehierr, $
      pres,presloerr,preshierr,s,sloerr,shierr,ab,abloerr,abhierr,ct,ctloerr,cthierr


;
; Read in data from "source_mkcflow.spectra"
;
    if (mkcflow_norm eq 1) then begin
        infitsfile='./'+spectra_dir+'/source_mkcflow.spectra'
        hd=headfits(infitsfile,exten=1)
        nfit=sxpar(hd,'NUM_REG','Parameter NUM_REG not found')
        ncool=sxpar(hd,'NUM_COOL','Parameter NUM_COOL not found')
        cnorm=dblarr(ncool)
        cnorm_lo=dblarr(ncool)
        cnorm_hi=dblarr(ncool)
        for i=0,ncool-1 do begin
            cnorm[i]=sxpar(hd,'CNORM'+strtrim(string(i+1),2))
            cnorm_lo[i]=sxpar(hd,'CN'+strtrim(string(i+1),2)+'ERRL')
            cnorm_hi[i]=sxpar(hd,'CN'+strtrim(string(i+1),2)+'ERRU')
        endfor


                                ;
                                ; Calculate total cooling rate
                                ;
        cnorm_tot=0.0
        cnorm_tot_lo=0.0
        cnorm_tot_hi=0.0
        for i=0,ncool-1 do begin
            cnorm_tot=cnorm_tot+cnorm[i]
            cnorm_tot_lo=cnorm_tot_lo+cnorm_lo[i]
            cnorm_tot_hi=cnorm_tot_hi+cnorm_hi[i]
        endfor
        cnorm_err_lo=cnorm_tot-cnorm_tot_lo
        cnorm_err_hi=cnorm_tot_hi-cnorm_tot
    endif


;
; Read in data from "source_mekal_flux.spectra" and "source_mkcflow_flux.spectra"
;
    if (mekal_flux eq 1) then begin 
        infitsfile='./'+spectra_dir+'/source_mekal_flux.spectra'
        hd=headfits(infitsfile,exten=1)
        Fcxray=sxpar(hd,'FCXRAY')
        Fcxray_lo=sxpar(hd,'FCXRAYL')
        Fcxray_hi=sxpar(hd,'FCXRAYU')
        Fcxray_lo_err=Fcxray-Fcxray_lo
        Fcxray_hi_err=Fcxray_hi-Fcxray
        Fcbol=sxpar(hd,'FCBOL') 
        Fcbol_lo=sxpar(hd,'FCBOLL')
        Fcbol_hi=sxpar(hd,'FCBOLU') 
        Fcbol_lo_err=Fcbol-Fcbol_lo
        Fcbol_hi_err=Fcbol_hi-Fcbol
        redshift=sxpar(hd,'REDSHIFT')
    endif

    if (mkcflow_flux eq 1) then begin
        infitsfile='./'+spectra_dir+'/source_mkcflow_flux.spectra'
        hd=headfits(infitsfile,exten=1)
        Fcxray_cooling=sxpar(hd,'FCXRAY')
        Fcxray_cooling_lo=sxpar(hd,'FCXRAYL')
        Fcxray_cooling_hi=sxpar(hd,'FCXRAYU')
        Fcxray_cooling_lo_err=Fcxray_cooling-Fcxray_cooling_lo
        Fcxray_cooling_hi_err=Fcxray_cooling_hi-Fcxray_cooling
        Fcbol_cooling=sxpar(hd,'FCBOL') 
        Fcbol_cooling_lo=sxpar(hd,'FCBOLL')
        Fcbol_cooling_hi=sxpar(hd,'FCBOLU') 
        Fcbol_cooling_lo_err=Fcbol_cooling-Fcbol_cooling_lo
        Fcbol_cooling_hi_err=Fcbol_cooling_hi-Fcbol_cooling
        redshift=sxpar(hd,'REDSHIFT')
    endif


;
; Read in header from evt2 file
;
    evt2file='./reprocessed/evt2_ccd_clean.fits.gz'
    hd=headfits(evt2file,exten=1)


;
; Get info out of evt1 header
;
    obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')
    ra=sxpar(hd,'RA_NOM')
    dec=sxpar(hd,'DEC_NOM')


;
; Read in annuli.txt
;
    central_file=findfile(spectra_dir+'/annuli_central.txt',count=numcentralfiles)
    infile='./'+spectra_dir+'/annuli.txt'
    readcol,infile,skipline=11,numline=1,delimiter=':',F='x,d',ellip
    readcol,infile,skipline=12,numline=1,delimiter=':',F='x,d',pa
    ex=ellip[0]
    pax=pa[0]
    readcol,infile,F='i,x,x,x,x,x,x,d',annulus_indx,counts
    if (numcentralfiles eq 1) then readcol,central_file,F='i,x,x,x,x,x,x,d',annulus_indx,counts_central
    reqx=sqrt(ax^2.0*(1.0-ex))


;
; Read in obs_info.txt if needed
;
    if (n_elements(caldb) eq 0) then begin
        infile='obs_info_'+strtrim(string(obsid),2)+'.txt'
        readcol,infile,skipline=35,F='x,a',numline=1,delimiter=':',caldb
    endif


;
; Read current working directory
;
    cd,current=xray_cwd


;
; Create structure:
; First, check if catalog file already exists; if so, check if entry
; exists; if so, update entry
;
    cat_file=findfile(catalog_file,count=numfiles)
    if (numfiles eq 1) then begin
        catalog_old=mrdfits(catalog_file,1,hd)
        indx=where(strtrim(catalog_old.System,2) eq strtrim(system_name,2))
        if (indx ne -1) then begin
            sys_props=catalog_old
            sys_props[indx].RA=ra
            sys_props[indx].DEC=dec
            sys_props[indx].ObsID=obsid
            sys_props[indx].Caldb=strtrim(caldb,2)
            if (mekal_flux eq 1) then begin
                sys_props[indx].Flux_bol=Fcbol
                sys_props[indx].Flux_bol_loerr=Fcbol_lo_err
                sys_props[indx].Flux_bol_hierr=Fcbol_hi_err 
            endif
            if (mkcflow_flux eq 1) then begin
                sys_props[indx].Flux_cool=Fcbol_cooling
                sys_props[indx].Flux_cool_loerr=Fcbol_cooling_lo_err
                sys_props[indx].Flux_cool_hierr=Fcbol_cooling_hi_err
            endif
            if (mkcflow_norm eq 1) then begin
                sys_props[indx].Cooling_rate=cnorm_tot
                sys_props[indx].Cooling_rate_loerr=cnorm_err_lo
                sys_props[indx].Cooling_rate_hierr=cnorm_err_hi
            endif
            sys_props[indx].eX=ex
            sys_props[indx].PAX=pax
            sys_props[indx].aX=dblarr(num)
            sys_props[indx].aX=ax
            sys_props[indx].reqX=dblarr(num)
            sys_props[indx].reqX=reqx
            sys_props[indx].kT=dblarr(num)
            sys_props[indx].kT=kt
            sys_props[indx].kT_loerr=dblarr(num)
            sys_props[indx].kT_loerr=ktloerr
            sys_props[indx].kT_hierr=dblarr(num)
            sys_props[indx].kT_hierr=kthierr
            sys_props[indx].n_e=dblarr(num)
            sys_props[indx].n_e=n_e
            sys_props[indx].n_e_loerr=dblarr(num)
            sys_props[indx].n_e_loerr=n_eloerr
            sys_props[indx].n_e_hierr=dblarr(num)
            sys_props[indx].n_e_hierr=n_ehierr
            sys_props[indx].pres=dblarr(num)
            sys_props[indx].pres=pres
            sys_props[indx].pres_loerr=dblarr(num)
            sys_props[indx].pres_loerr=presloerr
            sys_props[indx].pres_hierr=dblarr(num)
            sys_props[indx].pres_hierr=preshierr
            sys_props[indx].s=dblarr(num)
            sys_props[indx].s=s
            sys_props[indx].s_loerr=dblarr(num)
            sys_props[indx].s_loerr=sloerr
            sys_props[indx].s_hierr=dblarr(num)
            sys_props[indx].s_hierr=shierr
            sys_props[indx].ab=dblarr(num)
            sys_props[indx].ab=ab
            sys_props[indx].ab_loerr=dblarr(num)
            sys_props[indx].ab_loerr=abloerr
            sys_props[indx].ab_hierr=dblarr(num)
            sys_props[indx].ab_hierr=abhierr
            sys_props[indx].t_cool=dblarr(num)
            sys_props[indx].t_cool=ct
            sys_props[indx].t_cool_loerr=dblarr(num)
            sys_props[indx].t_cool_loerr=ctloerr
            sys_props[indx].t_cool_hierr=dblarr(num)
            sys_props[indx].t_cool_hierr=cthierr
            sys_props[indx].counts=dblarr(num)
            sys_props[indx].counts=counts
            if (numcentralfiles eq 1) then sys_props[indx].counts[0]=counts_central[0]
            sys_props[indx].Xray_path=xray_cwd
            mwrfits,sys_props,catalog_file,/create
            goto,skip_to_end
        endif 
    endif


;
; Otherwise, make a new entry
;
    num=100
    sys_props={System:' ',redshift:0d,RA:0d,DEC:0d,ObsID:0L,Caldb:' ',Flux_bol:0d,Flux_bol_loerr:0d,Flux_bol_hierr:0d,Flux_cool:0d,Flux_cool_loerr:0d,Flux_cool_hierr:0d, $
               Cooling_rate:0d,Cooling_rate_loerr:0d,Cooling_rate_hierr:0d,eX:0d,PAX:0d,aX:dblarr(num),reqX:dblarr(num),kT:dblarr(num),kT_loerr:dblarr(num), $
               kT_hierr:dblarr(num),n_e:dblarr(num),n_e_loerr:dblarr(num),n_e_hierr:dblarr(num),pres:dblarr(num),pres_loerr:dblarr(num), $
               pres_hierr:dblarr(num),s:dblarr(num),s_loerr:dblarr(num),s_hierr:dblarr(num),ab:dblarr(num),ab_loerr:dblarr(num),ab_hierr:dblarr(num), $
               t_cool:dblarr(num),t_cool_loerr:dblarr(num),t_cool_hierr:dblarr(num),counts:dblarr(num), $
               aU:dblarr(num),reqU:dblarr(num),SB_U_cnts:dblarr(num),SB_U_cnts_err:dblarr(num),SB_U_mag:dblarr(num),SB_U_mag_err:dblarr(num), $
               aR:dblarr(num),reqR:dblarr(num),SB_R_cnts:dblarr(num),SB_R_cnts_err:dblarr(num),SB_R_mag:dblarr(num),SB_R_mag_err:dblarr(num), $
               aI:dblarr(num),reqI:dblarr(num),SB_I_cnts:dblarr(num),SB_I_cnts_err:dblarr(num),SB_I_mag:dblarr(num),SB_I_mag_err:dblarr(num), $
               U_I_grad:0d,U_I_grad_err:0d,U_R_grad:0d,U_R_grad_err:0d,Grad_req_in:0d,Grad_req_out:0d, $
               Xray_path:' ',Optical_path:' ',psf:0d,Gal_RA:0d,Gal_DEC:0d,U_I_grad_zero:0d,U_R_grad_zero:0d, $
               SB_U_mag_syserr:dblarr(num),SB_R_mag_syserr:dblarr(num),SB_I_mag_syserr:dblarr(num), $
               SB_I_mag_staterr:dblarr(num),SB_R_mag_staterr:dblarr(num),SB_U_mag_staterr:dblarr(num), $
               SB_U_mag_compare1:dblarr(num),SB_U_mag_err_compare1:dblarr(num), $
               SB_R_mag_compare1:dblarr(num),SB_R_mag_err_compare1:dblarr(num), $
               SB_I_mag_compare1:dblarr(num),SB_I_mag_err_compare1:dblarr(num), $
               compare1:' ',reqU_compare1:dblarr(num),reqR_compare1:dblarr(num),reqI_compare1:dblarr(num), $
               ra_optical:0d,dec_optical:0d,k_apparent:0d,k_absolute:0d,A_K:0d,k_apparent_err:0d,k_absolute_err:0d, $
               P_cav_buoy:0d,P_cav_buoy_loerr:0d,P_cav_buoy_hierr:0d, $
               P_cav_cs:0d,P_cav_cs_loerr:0d,P_cav_cs_hierr:0d, $
               P_cav_refill:0d,P_cav_refill_loerr:0d,P_cav_refill_hierr:0d, $
               L_X_bol:0d,L_X_bol_loerr:0d,L_X_bol_hierr:0d, $
               L_cool:0d,L_cool_loerr:0d,L_cool_hierr:0d,cav_FOM:0d,cav_class:' '}
    sys_props.System=strtrim(system_name,2)
    read,redshift,prompt=' Enter the redshift: '
    sys_props.redshift=redshift
    sys_props.RA=ra
    sys_props.DEC=dec
    sys_props.ObsID=obsid
    sys_props.Caldb=strtrim(caldb,2)
    if (mekal_flux eq 1) then begin
        sys_props.Flux_bol=Fcbol
        sys_props.Flux_bol_loerr=Fcbol_lo_err
        sys_props.Flux_bol_hierr=Fcbol_hi_err 
    endif
    if (mkcflow_flux eq 1) then begin
        sys_props.Flux_cool=Fcbol_cooling
        sys_props.Flux_cool_loerr=Fcbol_cooling_lo_err
        sys_props.Flux_cool_hierr=Fcbol_cooling_hi_err
    endif
    if (mkcflow_norm eq 1) then begin
        sys_props.Cooling_rate=cnorm_tot
        sys_props.Cooling_rate_loerr=cnorm_err_lo
        sys_props.Cooling_rate_hierr=cnorm_err_hi
    endif
    sys_props.eX=ex
    sys_props.PAX=pax
    sys_props.aX=ax
    sys_props.reqX=reqx
    sys_props.kT=kt
    sys_props.kT_loerr=ktloerr
    sys_props.kT_hierr=kthierr
    sys_props.n_e=n_e
    sys_props.n_e_loerr=n_eloerr
    sys_props.n_e_hierr=n_ehierr
    sys_props.pres=pres
    sys_props.pres_loerr=presloerr
    sys_props.pres_hierr=preshierr
    sys_props.s=s
    sys_props.s_loerr=sloerr
    sys_props.s_hierr=shierr
    sys_props.ab=ab
    sys_props.ab_loerr=abloerr
    sys_props.ab_hierr=abhierr
    sys_props.t_cool=ct
    sys_props.t_cool_loerr=ctloerr
    sys_props.t_cool_hierr=cthierr
    sys_props.counts=counts
    if (numcentralfiles eq 1) then sys_props.counts[0]=counts_central[0]
    sys_props.Xray_path=xray_cwd
endif else begin


;
; Next, do optical catalog
;
;
; Check if Uprofile, Rprofile, and Iprofile exist
;
    Ufile=findfile(Uprofile,count=numfiles)
    if (numfiles eq 0) then Udata=0 else Udata=1
    Rfile=findfile(Rprofile,count=numfiles)
    if (numfiles eq 0) then Rdata=0 else Rdata=1
    Ifile=findfile(Iprofile,count=numfiles)
    if (numfiles eq 0) then Idata=0 else Idata=1


;
; Read in data from the files
;
    if (Udata eq 1) then begin
        readcol,Uprofile,skipline=12,f="d,d,d,d,d,d,d,d,d",sma_U,req_U,sb_U,sb_Uerr,sb_Uerr_tot,mag_U,mag_toterr_U,mag_lo_U,mag_hi_U
        mag_syserr_U=abs(mag_U-mag_lo_U)
        mag_staterr_U=mag_toterr_U
        mag_err_U=sqrt( (mag_staterr_U)^2.0 + (mag_syserr_U)^2.0 )
    endif
    if (Rdata eq 1) then begin
        readcol,Rprofile,skipline=12,f="d,d,d,d,d,d,d,d,d",sma_R,req_R,sb_R,sb_Rerr,sb_Rerr_tot,mag_R,mag_toterr_R,mag_lo_R,mag_hi_R   
        mag_syserr_R=abs(mag_R-mag_lo_R)
        mag_staterr_R=mag_toterr_R
        mag_err_R=sqrt( (mag_staterr_R)^2.0 + (mag_syserr_R)^2.0 )
    endif
    if (Idata eq 1) then begin
        readcol,Iprofile,skipline=12,f="d,d,d,d,d,d,d,d,d",sma_I,req_I,sb_I,sb_Ierr,sb_Ierr_tot,mag_I,mag_toterr_I,mag_lo_I,mag_hi_I  
        mag_syserr_I=abs(mag_I-mag_lo_I) 
        mag_staterr_I=mag_toterr_I
        mag_err_I=sqrt( (mag_staterr_I)^2.0 + (mag_syserr_I)^2.0 )
    endif
    
    
;
; Read current working directory
;
    cd,current=optical_cwd
    

;
; Create structure:
; First, check if catalog file already exists; if so, check if entry
; exists; if so, update entry
;
    num=100
    cat_file=findfile(catalog_file,count=numfiles)
    if (numfiles eq 1) then begin
        catalog_old=mrdfits(catalog_file,1,hd)
        indx=where(strtrim(catalog_old.System,2) eq strtrim(system_name,2))
        if (indx ne -1) then begin
            sys_props=catalog_old
            if (Udata eq 1) then begin     
                sys_props[indx].aU=dblarr(num)
                sys_props[indx].aU=sma_U
                sys_props[indx].reqU=dblarr(num)
                sys_props[indx].reqU=req_U
                sys_props[indx].SB_U_cnts=dblarr(num)
                sys_props[indx].SB_U_cnts=sb_U
                sys_props[indx].SB_U_cnts_err=dblarr(num)
                sys_props[indx].SB_U_cnts_err=sb_Uerr
                sys_props[indx].SB_U_mag=dblarr(num)
                sys_props[indx].SB_U_mag=mag_U
                sys_props[indx].SB_U_mag_err=dblarr(num)
                sys_props[indx].SB_U_mag_err=mag_err_U
                sys_props[indx].SB_U_mag_syserr=dblarr(num)
                sys_props[indx].SB_U_mag_syserr=mag_syserr_U
                sys_props[indx].SB_U_mag_staterr=dblarr(num)
                sys_props[indx].SB_U_mag_staterr=mag_staterr_U
            endif else begin
                sys_props[indx].aU=dblarr(num)
                sys_props[indx].reqU=dblarr(num)
                sys_props[indx].SB_U_cnts=dblarr(num)
                sys_props[indx].SB_U_cnts_err=dblarr(num)
                sys_props[indx].SB_U_mag=dblarr(num)
                sys_props[indx].SB_U_mag_err=dblarr(num)
                sys_props[indx].SB_U_mag_syserr=dblarr(num)
                sys_props[indx].SB_U_mag_staterr=dblarr(num)
            endelse
            if (Rdata eq 1) then begin     
                sys_props[indx].aR=dblarr(num)
                sys_props[indx].aR=sma_R
                sys_props[indx].reqR=dblarr(num)
                sys_props[indx].reqR=req_R
                sys_props[indx].SB_R_cnts=dblarr(num)
                sys_props[indx].SB_R_cnts=sb_R
                sys_props[indx].SB_R_cnts_err=dblarr(num)
                sys_props[indx].SB_R_cnts_err=sb_Rerr
                sys_props[indx].SB_R_mag=dblarr(num)
                sys_props[indx].SB_R_mag=mag_R
                sys_props[indx].SB_R_mag_err=dblarr(num)
                sys_props[indx].SB_R_mag_err=mag_err_R
                sys_props[indx].SB_R_mag_syserr=dblarr(num)
                sys_props[indx].SB_R_mag_syserr=mag_syserr_R
                sys_props[indx].SB_R_mag_staterr=dblarr(num)
                sys_props[indx].SB_R_mag_staterr=mag_staterr_R
            endif else begin
                sys_props[indx].aR=dblarr(num)
                sys_props[indx].reqR=dblarr(num)
                sys_props[indx].SB_R_cnts=dblarr(num)
                sys_props[indx].SB_R_cnts_err=dblarr(num)
                sys_props[indx].SB_R_mag=dblarr(num)
                sys_props[indx].SB_R_mag_err=dblarr(num)
                sys_props[indx].SB_R_mag_syserr=dblarr(num)
                sys_props[indx].SB_R_mag_staterr=dblarr(num)
            endelse
            if (Idata eq 1) then begin    
                sys_props[indx].aI=dblarr(num)
                sys_props[indx].aI=sma_I
                sys_props[indx].reqI=dblarr(num)
                sys_props[indx].reqI=req_I
                sys_props[indx].SB_I_cnts=dblarr(num)
                sys_props[indx].SB_I_cnts=sb_I
                sys_props[indx].SB_I_cnts_err=dblarr(num)
                sys_props[indx].SB_I_cnts_err=sb_Ierr
                sys_props[indx].SB_I_mag=dblarr(num)
                sys_props[indx].SB_I_mag=mag_I
                sys_props[indx].SB_I_mag_err=dblarr(num)
                sys_props[indx].SB_I_mag_err=mag_err_I
                sys_props[indx].SB_I_mag_syserr=dblarr(num)
                sys_props[indx].SB_I_mag_syserr=mag_syserr_I
                sys_props[indx].SB_I_mag_staterr=dblarr(num)
                sys_props[indx].SB_I_mag_staterr=mag_staterr_I
            endif else begin
                sys_props[indx].aI=dblarr(num)
                sys_props[indx].reqI=dblarr(num)
                sys_props[indx].SB_I_cnts=dblarr(num)
                sys_props[indx].SB_I_cnts_err=dblarr(num)
                sys_props[indx].SB_I_mag=dblarr(num)
                sys_props[indx].SB_I_mag_err=dblarr(num)
                sys_props[indx].SB_I_mag_syserr=dblarr(num)
                sys_props[indx].SB_I_mag_staterr=dblarr(num)
            endelse
            sys_props[indx].Optical_path=optical_cwd
            sys_props[indx].psf=psf
            sys_props[indx].Gal_RA=gal_ra
            sys_props[indx].Gal_DEC=gal_dec
            mwrfits,sys_props,catalog_file,/create
            goto,skip_to_end
        endif 
    endif


;
; Otherwise, make a new entry
;
    num=100
    sys_props={System:' ',redshift:0d,RA:0d,DEC:0d,ObsID:0L,Caldb:' ',Flux_bol:0d,Flux_bol_loerr:0d,Flux_bol_hierr:0d,Flux_cool:0d,Flux_cool_loerr:0d,Flux_cool_hierr:0d, $
               Cooling_rate:0d,Cooling_rate_loerr:0d,Cooling_rate_hierr:0d,eX:0d,PAX:0d,aX:dblarr(num),reqX:dblarr(num),kT:dblarr(num),kT_loerr:dblarr(num), $
               kT_hierr:dblarr(num),n_e:dblarr(num),n_e_loerr:dblarr(num),n_e_hierr:dblarr(num),pres:dblarr(num),pres_loerr:dblarr(num), $
               pres_hierr:dblarr(num),s:dblarr(num),s_loerr:dblarr(num),s_hierr:dblarr(num),ab:dblarr(num),ab_loerr:dblarr(num),ab_hierr:dblarr(num), $
               t_cool:dblarr(num),t_cool_loerr:dblarr(num),t_cool_hierr:dblarr(num),counts:dblarr(num), $
               aU:dblarr(num),reqU:dblarr(num),SB_U_cnts:dblarr(num),SB_U_cnts_err:dblarr(num),SB_U_mag:dblarr(num),SB_U_mag_err:dblarr(num), $
               aR:dblarr(num),reqR:dblarr(num),SB_R_cnts:dblarr(num),SB_R_cnts_err:dblarr(num),SB_R_mag:dblarr(num),SB_R_mag_err:dblarr(num), $
               aI:dblarr(num),reqI:dblarr(num),SB_I_cnts:dblarr(num),SB_I_cnts_err:dblarr(num),SB_I_mag:dblarr(num),SB_I_mag_err:dblarr(num), $
               U_I_grad:0d,U_I_grad_err:0d,U_R_grad:0d,U_R_grad_err:0d,Grad_req_in:0d,Grad_req_out:0d, $
               Xray_path:' ',Optical_path:' ',psf:0d,Gal_RA:0d,Gal_DEC:0d,U_I_grad_zero:0d,U_R_grad_zero:0d, $
               SB_U_mag_syserr:dblarr(num),SB_R_mag_syserr:dblarr(num),SB_I_mag_syserr:dblarr(num), $
               SB_I_mag_staterr:dblarr(num),SB_R_mag_staterr:dblarr(num),SB_U_mag_staterr:dblarr(num), $
               SB_U_mag_compare1:dblarr(num),SB_U_mag_err_compare1:dblarr(num), $
               SB_R_mag_compare1:dblarr(num),SB_R_mag_err_compare1:dblarr(num), $
               SB_I_mag_compare1:dblarr(num),SB_I_mag_err_compare1:dblarr(num), $
               compare1:' ',reqU_compare1:dblarr(num),reqR_compare1:dblarr(num),reqI_compare1:dblarr(num), $
               ra_optical:0d,dec_optical:0d,k_apparent:0d,k_absolute:0d,A_K:0d,k_apparent_err:0d,k_absolute_err:0d, $
               P_cav_buoy:0d,P_cav_buoy_loerr:0d,P_cav_buoy_hierr:0d, $
               P_cav_cs:0d,P_cav_cs_loerr:0d,P_cav_cs_hierr:0d, $
               P_cav_refill:0d,P_cav_refill_loerr:0d,P_cav_refill_hierr:0d, $
               L_X_bol:0d,L_X_bol_loerr:0d,L_X_bol_hierr:0d, $
               L_cool:0d,L_cool_loerr:0d,L_cool_hierr:0d,cav_FOM:0d,cav_class:' '}
    sys_props.System=strtrim(system_name,2)
    read,redshift,prompt=' Enter the redshift: '
    sys_props.redshift=redshift
    if (Udata eq 1) then begin       
        sys_props.aU=sma_U
        sys_props.reqU=req_U
        sys_props.SB_U_cnts=sb_U
        sys_props.SB_U_cnts_err=sb_Uerr
        sys_props.SB_U_mag=mag_U
        sys_props.SB_U_mag_err=mag_err_U
        sys_props.SB_U_mag_syserr=mag_syserr_U
        sys_props.SB_U_mag_staterr=mag_staterr_U
    endif
    if (Rdata eq 1) then begin       
        sys_props.aR=sma_R
        sys_props.reqR=req_R
        sys_props.SB_R_cnts=sb_R
        sys_props.SB_R_cnts_err=sb_Rerr
        sys_props.SB_R_mag=mag_R
        sys_props.SB_R_mag_err=mag_err_R
        sys_props.SB_R_mag_syserr=mag_syserr_R
        sys_props.SB_R_mag_staterr=mag_staterr_R
    endif
    if (Idata eq 1) then begin       
        sys_props.aI=sma_I
        sys_props.reqI=req_I
        sys_props.SB_I_cnts=sb_I
        sys_props.SB_I_cnts_err=sb_Ierr
        sys_props.SB_I_mag=mag_I
        sys_props.SB_I_mag_err=mag_err_I
        sys_props.SB_I_mag_syserr=mag_syserr_I
        sys_props.SB_I_mag_staterr=mag_staterr_I
    endif
    sys_props.Optical_path=optical_cwd
    sys_props.psf=psf
    sys_props.Gal_RA=gal_ra
    sys_props.Gal_DEC=gal_dec
endelse


;
; Make the header
;



;
; Update catalog fits file
;
; Check if catalog file already exists
;
cat_file=findfile(catalog_file,count=numfiles)
if (numfiles eq 1) then begin
    catalog_old=mrdfits(catalog_file,1,hd)
    catalog_new=struct_append(catalog_old,sys_props)
    mwrfits,catalog_new,catalog_file,/create
endif else begin
    mwrfits,sys_props,catalog_file
endelse


;
; Print status info to screen
;
skip_to_end:
print,' '
print,'ADD_TO_CATALOG complete.'
print,catalog_file+' has been updated.'


;
; Return to IDL
;
return
end
