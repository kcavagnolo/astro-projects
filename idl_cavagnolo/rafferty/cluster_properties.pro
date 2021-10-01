pro cluster_properties,deprojected=deprojected, $
    projected=projected,cooling=cooling,spectra_dir=spectra_dir, $
    deproj_old_pipe=deproj_old_pipe,no_update=no_update,old_pipe=old_pipe,flux=flux
;-----------------------------------------------------------------------
;
; Name: CLUSTER_PROPERTIES
;
; Purpose: Calculate the following cluster parameters and store the values
;	   in one or more data files:  
;	     central cooling time, central temperature, temperature decrement*, 
;	     central entropy, central density, central pressure, cluster X-Ray 
;	     luminosity*, cluster bolometric luminosity*, cooling rate
;	     (*not yet implemented)
;          
; Inputs:  none (defaults to /DEPROJECTED)
;
; Optional: /DEPROJECTED - if set, deprojected values are used.
;			   output file: "cluster_properties_deproj.dat"
;	    /PROJECTED - if set, projected values are used.
;			 output file: "cluster_properties_proj.dat"
;	    /COOLING - if set, cooling values are used.
;		       output file: "cluster_properties_cooling.dat"
;	    spectra_dir - name of the spectra directory (default is 'spectra')    
;	    /DEPROJ_OLD_PIPE - if set, deprojected data from the old pipeline 
;			       will be used
;	    /NO_UPDATE - if set, the obs info file will not be updated, 
;			 data file(s) will be updated as normal
;	    /FLUX - if set, the fluxes will be written (must have used /FLUX 
;		    or /COOL_FLUXES keyword with FIT_SPECTRA)
;         
; Comments: PROJECTED code not yet implemented
;           
;           
; Revision history:
;       written by DR, 2005-1-27
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 0) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'cluster_properties [, /PROJECTED, /DEPROJECTED, /COOLING, /NO_UPDATE, /OLD_PIPE, spectra_dir=spectra_dir]'
   return   
endif
if (keyword_set(projected) or keyword_set(deprojected) or keyword_set(cooling) or keyword_set(deproj_old_pipe)) then begin
endif else begin
   deprojected=1
endelse
if (keyword_set(deprojected) and keyword_set(deproj_old_pipe)) then begin
   print,'ERROR: Cannot use /DEPROJECTED keyword with /DEPROJ_OLD_PIPE'
   return
endif


;
; Set default spectra directory
;
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'


;
; Check if source_mekal_flux.spectra and source_mkcflow_flux.spectra exist
;
mkfile=findfile('spectra/source_mekal_flux.spectra',count=num)
if (num eq 0) then mekal_flux=0 else mekal_flux=1
mkcfile=findfile('spectra/source_mkcflow_flux.spectra',count=num)
if (num eq 0) then mkcflow_flux=0 else mkcflow_flux=1


;
; If /DEPROJECTED is set, use deprojected values
;
if keyword_set(deprojected) then begin


  ;
  ; Read in data from "properties_deproj.dat"
  ;
  infile='./'+spectra_dir+'/properties_deproj.dat'
  if keyword_set(old_pipe) then begin
     readcol,infile,skipline=20,rarcsec,rkpc,kT,kTloerr,kThierr,n_e,n_eloerr,n_ehierr,pres,presloerr,preshierr,s,sloerr,shierr,ab,abloerr,abhierr,ct
     s=size(rarcsec)
     l=s[0]
     ctloerr=dblarr(l)
     cthierr=dblarr(l)
     for i=0,l-1 do begin
         ctloerr[i]=0
         cthierr[i]=0
     endfor
  endif else begin
     readcol,infile,skipline=20,rarcsec,rkpc,kT,kTloerr,kThierr,n_e,n_eloerr,n_ehierr,pres,presloerr,preshierr,s,sloerr,shierr,ab,abloerr,abhierr,ct,ctloerr,cthierr
  endelse
   
   
  ;
  ; Calculate central radius limit from average radius
  ;
  r_cent_limit_kpc=rkpc[0]*2.0
  r_cent_limit=rarcsec[0]*2.0
   

  ;
  ; Find central cooling time, temperature, density, entropy, and pressure
  ;
  ct_cent=ct[0]
  ct_cent_lo=ctloerr[0]
  ct_cent_hi=cthierr[0]
  kT_cent=kT[0]
  kT_cent_lo=kTloerr[0]
  kT_cent_hi=kThierr[0]
  n_e_cent=n_e[0]
  n_e_cent_lo=n_eloerr[0]
  n_e_cent_hi=n_ehierr[0]
  s_cent=s[0]
  s_cent_lo=sloerr[0]
  s_cent_hi=shierr[0]
  pres_cent=pres[0]
  pres_cent_lo=presloerr[0]
  pres_cent_hi=preshierr[0]
  
  
  ;
  ; Find temperature decrement
  ;
  goto,skip_temp_decr
  kT_truncated_indices=where(kT lt 20.0)
  kT_truncated=kT[kT_truncated_indices]
  kT_truncatedloerr=kTloerr[kT_truncated_indices]
  kT_truncatedhierr=kThierr[kT_truncated_indices]
  kTmax=max(kT_truncated,kTmax_index)
  kTmaxloerr=kT_truncatedloerr[kTmax_index]
  kTmaxhierr=kT_truncatedhierr[kTmax_index]
  kT_decr=kTmax-kT_cent
  kT_decrloerr=sqrt(kTmaxloerr^2.0+kT_centhierr^2.0)
  kT_decrhierr=sqrt(kTmaxhierr^2.0+kT_centloerr^2.0)
  rkpc_truncated=rkpc[kT_truncated_indices]
  rkpc_kTmax=rkpc_truncated[kTmax_index]
  kT_grad=kT_decr/rkpc_kTmax
  kT_gradloerr=kT_decrloerr/rkpc_kTmax
  kT_gradhierr=kT_decrhierr/rkpc_kTmax
  skip_temp_decr:
  
  ;
  ; Read fluxes from fits file
  ;
  goto,skip_flux
  infitsfile='./'+spectra_dir+'/source_mekal.spectra'
  hd=headfits(infitsfile,exten=1)
  Fxray=sxpar(hd,'FXRAY')
  Fxray_lo=sxpar(hd,'FXRAYL')
  Fxray_hi=sxpar(hd,'FXRAYU')
  Fxray_lo_err=Fxray-Fxray_lo
  Fxray_hi_err=Fxray_hi-Fxray
  Fbol=sxpar(hd,'FBOL')
  Fbol_lo=sxpar(hd,'FBOLL')
  Fbol_hi=sxpar(hd,'FBOLU')
  Fbol_lo_err=Fbol-Fbol_lo
  Fbol_hi_err=Fbol_hi-Fbol
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
  skip_flux:
endif


;
; If /PROJECTED is set, use projected values
;
if keyword_set(projected) then begin

endif


;
; If /DEPROJ_OLD_PIPE is set, use old pipeline values
;
if keyword_set(deproj_old_pipe) then begin

  ;
  ; Look for "properties_deproj"
  ;
  prop_file='./'+spectra_dir+'/properties_deproj.dat'
  propfile=findfile(prop_file,count=num)
  if (num eq 0) then begin


     ;
     ; Read in data from "nHvaried_deproj.dat"
     ;
     infile='./'+spectra_dir+'/nHvaried_deproj.dat'
     readcol,infile,kT_old,kTlo_old,kThi_old,ab_old,ablo_old,abhi_old,norm_old,normlo_old,normhi_old
     infile='./'+spectra_dir+'/deproj_radii.dat'
     readcol,infile,rpix_old
     s=size(kT_old)
     ndata=s[1]
     rin=dblarr(ndata)
     rout=dblarr(ndata)
     rin[0]=0.0
     rout[0]=rpix_old[0]
     for i=1,ndata-1 do begin
        rin[i]=rpix_old[i-1]
        rout[i]=rpix_old[i]
     endfor
     scale=0.4919		 ; ACIS scale [arcsec/pixel]
     ravg_old=(rin+rout)*scale/2    ; central radius of each annulus
  
  
     ;
     ; Get redshift, lumdist, and angscale
     ;
     print,'Please enter values for the following parameters:'
     print,' '
     read,redshift,prompt='                  Redshift: '
     read,lumdist,prompt=' Luminosity distance [Mpc]: '
     read,angscale,prompt='Angular scale [kpc/arcsec]: '
     print,' '
       
   
     ; 
     ; Calculate densities
     ;
     normloerr_old=norm_old-normlo_old
     normhierr_old=normhi_old-norm_old
     kTloerr_old=kT_old-kTlo_old
     kThierr_old=kThi_old-kT_old
     abloerr_old=ab_old-ablo_old
     abhierr_old=abhi_old-ab_old
     pixel_conversion=angscale*0.4919*1000*3.085678
     vol=4.0*3.141592654/3.0*(rout^3.0-rin^3.0)*(pixel_conversion)^3.0 ;[10^54 cm^3]
     n_e_old=sqrt(1.2*norm_old*4*3.141592654*((lumdist*3.085678E6)^2.0/(1.0+redshift)^2.0)*1E14/vol/1E18)  ; density 
     n_eloerr_old=sqrt((0.5*normloerr_old/norm_old)^2.0)*n_e_old  ; add errors in quadrature and find low error of n_e_old
     n_ehierr_old=sqrt((0.5*normhierr_old/norm_old)^2.0)*n_e_old  ; add errors in quadrature and find high error of n_e_old


     ;
     ; Calculate pressures
     ;
     pres_old=2.0*n_e_old*kT_old*1.60219E-9
     presloerr_old=sqrt((n_eloerr_old/n_e_old)^2.0+(kTloerr_old/kT_old)^2.0)*pres_old
     preshierr_old=sqrt((n_ehierr_old/n_e_old)^2.0+(kThierr_old/kT_old)^2.0)*pres_old
  
  
     ;
     ; Calculate entropies
     ;
     s_old=kT_old/(n_e_old)^(0.66667)
     sloerr_old=sqrt((kTloerr_old/kT_old)^2.0+(0.66667*n_eloerr_old/n_e_old)^2.0)*s_old
     shierr_old=sqrt((kThierr_old/kT_old)^2.0+(0.66667*n_ehierr_old/n_e_old)^2.0)*s_old
  
   
     ;
     ; Calculate cooling times
     ;
     cfn_file=findfile('cfn.dat',count=num)
     if (num eq 0) then begin
        make_cfn_file
     endif
     print,' '
     print,'Please enter the following values into the NEWCOOL program.'
     print,'You can select all of them together with the mouse and paste'
     print,'with the middle button.  NEWCOOL will then calculate cooling'
     print,'times for each line automatically.'
     print,' '
     for i=0,ndata-1 do begin
        print,strtrim(string(kT_old[i]),2)+' '+strtrim(string(n_e_old[i]),2)+' '+strtrim(string(ab_old[i]),2)
     endfor
     print,'0 0 0'
     print,' '
     spawn,'newcool'
     print,' '
     ct_old=dblarr(ndata)
     for i=0,ndata-1 do begin
        read,temp,prompt='Please enter the cooling time for region '+strtrim(string(i+1),2)+': '
        ct_old[i]=temp
     endfor


     ;
     ; Write data to "properties_deproj.dat"
     ;
     get_lun,unit
     outfile='./'+spectra_dir+'/properties_deproj.dat'
     openw,unit,outfile
     printf,unit,'# Cluster properties calculated from the deprojected data.'
     printf,unit,'#'
     printf,unit,'# Columns:'
     printf,unit,'# (1) Average radius of annulus [arcsec]'
     printf,unit,'# (2) Average radius of annulus [kpc]'
     printf,unit,'# (3) kT [keV]'
     printf,unit,'# (4) kT low error [keV]'
     printf,unit,'# (5) kT high error [keV]'
     printf,unit,'# (6) n_e [cm^-3]'
     printf,unit,'# (7) n_e low error [cm^-3]'
     printf,unit,'# (8) n_e high error [cm^-3]'
     printf,unit,'# (9) Pressure [ergs cm^-3]'
     printf,unit,'# (10) Pressure low error [ergs cm^-3]'
     printf,unit,'# (11) Pressure high error [ergs cm^-3]'
     printf,unit,'# (12) Entropy [keV cm^2]'
     printf,unit,'# (13) Entropy low error [keV cm^2]'
     printf,unit,'# (14) Entropy high error [keV cm^2]'
     printf,unit,'# (15) Abundance [solar]'
     printf,unit,'# (16) Abundance low error [solar]'
     printf,unit,'# (17) Abundance high error [solar]'
     printf,unit,'# (18) Cooling time [yr]'
     printf,unit,' '
     fmt='$(f9.3,3x,f9.3,3(3x,f9.4),6(3x,e13.5),3(3x,f9.3),3(3x,f9.7),3x,e13.5)'
     for i=0,ndata-1 do begin
        printf,unit,fmt,ravg_old[i],ravg_old[i]*angscale,kT_old[i],kTloerr_old[i],kThierr_old[i],n_e_old[i],n_eloerr_old[i],n_ehierr_old[i],pres_old[i],presloerr_old[i],preshierr_old[i],s_old[i],sloerr_old[i],shierr_old[i],ab_old[i],abloerr_old[i],abhierr_old[i],ct_old[i]
     endfor
     printf,unit,' '
     close,unit
     free_lun,unit
     rkpc_old=ravg_old*angscale
     rarcsec_old=ravg_old
  endif else begin
     infile='./'+spectra_dir+'/properties_deproj.dat'
     readcol,infile,skipline=21,rarcsec_old,rkpc_old,kT_old,kTloerr_old,kThierr_old,n_e_old,n_eloerr_old,n_ehierr_old,pres_old,presloerr_old,preshierr_old,s_old,sloerr_old,shierr_old,ab_old,abloerr_old,abhierr_old,ct_old
  endelse  


  ;
  ; Find central cooling time, temperature, density, entropy, and pressure
  ;
  r_cent_limit_kpc=rkpc_old[0]*2.0
  r_cent_limit=rarcsec_old[0]*2.0
  ct_cent=ct_old[0]
  ct_cent_lo=0
  ct_cent_hi=0
  kT_cent=kT_old[0]
  kT_cent_lo=kTloerr_old[0]
  kT_cent_hi=kThierr_old[0]
  n_e_cent=n_e_old[0]
  n_e_cent_lo=n_eloerr_old[0]
  n_e_cent_hi=n_ehierr_old[0]
  s_cent=s_old[0]
  s_cent_lo=sloerr_old[0]
  s_cent_hi=shierr_old[0]
  pres_cent=pres_old[0]
  pres_cent_lo=presloerr_old[0]
  pres_cent_hi=preshierr_old[0]
  
  
  ;
  ; Find temperature decrement
  ;
;  kTmax_old=max(kT_old,kTmax_index_old)
;  kT_decr_old=kTmax_old-kT_cent_old
;  rkpc_kTmax_old=rkpc_old[kTmax_index_old]
;  kT_grad_old=kT_decr_old/rkpc_kTmax_old
  
endif


;
; If /COOLING is set, use cooling values
;
if keyword_set(cooling) then begin


  ;
  ; Read in data from "source_mkcflow.spectra"
  ;
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
; If /FLUX is set, get flux values
;
if keyword_set(flux) then begin


  ;
  ; Read in data from "source_mekal_flux.spectra", "source_mkcflow_flux.spectra", or "source_mekal.spectra"
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

endif


;
; Update log file
;
if keyword_set(no_update) then goto,skip_logfile


;
; Read in header from evt2 file
;
evt2file='./reprocessed/evt2_ccd_clean.fits'
hd=headfits(evt2file,exten=1)


;
; Get OBS_ID out of evt1 header
;
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')


   ;
   ; Update obs_info file
   ;
   get_lun,unit
   outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'
   openw,unit,outfile,/append
   append_text='S'
   if keyword_set(deprojected) then append_text=append_text+', /DEPROJECTED'
   if keyword_set(projected) then append_text=append_text+', /PROJECTED'
   if keyword_set(cooling) then append_text=append_text+', /COOLING'
   if keyword_set(flux) then append_text=append_text+', /FLUX'
   if keyword_set(deproj_old_pipe) then append_text=append_text+', /DEPROJ_OLD_PIPE'
   printf,unit,'Output of CLUSTER_PROPERTIE'+append_text+", spectra_dir='"+spectra_dir+"'"
   printf,unit,!stime
   printf,unit,' '
   if keyword_set(deprojected) then begin
      printf,unit,'Deprojected Values (1 sigma errors)'
      printf,unit,'-----------------------------------'
      printf,unit,'Radius of central region [arcsec]   : '+strtrim(string(r_cent_limit),2)
      printf,unit,'Radius of central region [kpc]      : '+strtrim(string(r_cent_limit_kpc),2)
      printf,unit,'Central cooling time [yr]           : '+strtrim(string(ct_cent),2)+' (-'+strtrim(string(ct_cent_lo),2)+', +'+strtrim(string(ct_cent_hi),2)+')'
      printf,unit,'Central kT [keV]                    : '+strtrim(string(kT_cent),2)+' (-'+strtrim(string(kT_cent_lo),2)+', +'+strtrim(string(kT_cent_hi),2)+')'
      printf,unit,'Central density [cm^-3]             : '+strtrim(string(n_e_cent),2)+' (-'+strtrim(string(n_e_cent_lo),2)+', +'+strtrim(string(n_e_cent_hi),2)+')'
      printf,unit,'Central entropy [keV cm^2]          : '+strtrim(string(s_cent),2)+' (-'+strtrim(string(s_cent_lo),2)+', +'+strtrim(string(s_cent_hi),2)+')'
      printf,unit,'Central pressure [keV cm^-3]        : '+strtrim(string(pres_cent),2)+' (-'+strtrim(string(pres_cent_lo),2)+', +'+strtrim(string(pres_cent_hi),2)+')'
;      printf,unit,'Temperature drop (max-central) [keV] : '+strtrim(string(kT_decr),2)
;      printf,unit,'Corresponding temp grad [keV/kpc]    : '+strtrim(string(kT_grad),2)
;      printf,unit,' '
;      printf,unit,'X-ray luminosity (0.5 - 7.0 keV) [erg/s]        : '+strtrim(string(Fxray),2)+' (-'+strtrim(string(Fxray_lo_err),2)+', +'+strtrim(string(Fxray_hi_err),2)+')'
;      printf,unit,'X-ray luminosity corrected for nH [erg/s]       : '+strtrim(string(Fcxray),2)+' (-'+strtrim(string(Fcxray_lo_err),2)+', +'+strtrim(string(Fcxray_hi_err),2)+')'
;      printf,unit,'Bolometric luminosity (0.001 - 100 keV) [erg/s] : '+strtrim(string(Fbol),2)+' (-'+strtrim(string(Fbol_lo_err),2)+', +'+strtrim(string(Fbol_hi_err),2)+')'
;      printf,unit,'Bolometric luminosity corrected for nH [erg/s]  : '+strtrim(string(Fcbol),2)+' (-'+strtrim(string(Fcbol_lo_err),2)+', +'+strtrim(string(Fcbol_hi_err),2)+')'
      printf,unit,' '
   endif
   if keyword_set(projected) then begin
   endif
   if keyword_set(deproj_old_pipe) then begin
      printf,unit,'Deprojected Values from Old Pipeline (1 sigma errors)'
      printf,unit,'-----------------------------------------------------'
      printf,unit,'Central cooling time [yr]           : '+strtrim(string(ct_cent),2)
      printf,unit,'Central kT [keV]                    : '+strtrim(string(kT_cent),2)+' (-'+strtrim(string(kT_cent_lo),2)+', +'+strtrim(string(kT_cent_hi),2)+')'
      printf,unit,'Central density [cm^-3]             : '+strtrim(string(n_e_cent),2)+' (-'+strtrim(string(n_e_cent_lo),2)+', +'+strtrim(string(n_e_cent_hi),2)+')'
      printf,unit,'Central entropy [keV cm^2]          : '+strtrim(string(s_cent),2)+' (-'+strtrim(string(s_cent_lo),2)+', +'+strtrim(string(s_cent_hi),2)+')'
      printf,unit,'Central pressure [keV cm^-3]        : '+strtrim(string(pres_cent),2)+' (-'+strtrim(string(pres_cent_lo),2)+', +'+strtrim(string(pres_cent_hi),2)+')'
;      printf,unit,'Temperature drop (max-central) [keV] : '+strtrim(string(kT_decr_old),2)
;      printf,unit,'Corresponding temp grad [keV/kpc]    : '+strtrim(string(kT_grad_old),2)
      printf,unit,' '
   endif
   if keyword_set(cooling) then begin
      printf,unit,'Cooling Values (1 sigma errors)'
      printf,unit,'-------------------------------'
      printf,unit,'Cooling rate [solar masses/yr]              : '+strtrim(string(cnorm_tot),2)+' (-'+strtrim(string(cnorm_err_lo),2)+', +'+strtrim(string(cnorm_err_hi),2)+')'
;      if keyword_set(flux) then printf,unit,'Cooling flux (0.5 - 7.0 keV) [erg/s/cm^2]   : '+strtrim(string(Fcxray_cooling),2)+' (-'+strtrim(string(Fcxray_cooling_lo),2)+', +'+strtrim(string(Fcxray_cooling_hi),2)+')'
;      if keyword_set(flux) then printf,unit,'Cooling flux (0.001 - 100 keV) [erg/s/cm^2] : '+strtrim(string(Fcbol_cooling),2)+' (-'+strtrim(string(Fcbol_cooling_lo),2)+', +'+strtrim(string(Fcbol_cooling_hi),2)+')'
      printf,unit,' '
   endif
   if keyword_set(flux) then begin
      dist=lumdist(redshift,/silent)
      factor=4.0*3.1415*dist^2.0*(3.085678)^2.0*1E6	; 10^42 cm^2
      if (mekal_flux eq 1) then begin
         printf,unit,'X-ray Fluxes inside Cooling region (1 sigma errors)'
         printf,unit,'---------------------------------------------------'
;      printf,unit,'X-ray flux (0.5 - 7.0 keV) [erg/s/cm^2]       : '+strtrim(string(Fxray),2)+' (-'+strtrim(string(Fxray_lo_err),2)+', +'+strtrim(string(Fxray_hi_err),2)+')'
         printf,unit,'0.5 - 7.0 keV flux corrected for nH [erg/s/cm^2]   : '+strtrim(string(Fcxray),2)+' (-'+strtrim(string(Fcxray_lo_err),2)+', +'+strtrim(string(Fcxray_hi_err),2)+')'
;      printf,unit,'Bolometric flux (0.001 - 100 keV) [erg/s/cm^2] : '+strtrim(string(Fbol),2)+' (-'+strtrim(string(Fbol_lo_err),2)+', +'+strtrim(string(Fbol_hi_err),2)+')'
         printf,unit,'0.001 - 100 keV flux corrected for nH [erg/s/cm^2] : '+strtrim(string(Fcbol),2)+' (-'+strtrim(string(Fcbol_lo_err),2)+', +'+strtrim(string(Fcbol_hi_err),2)+')'
         printf,unit,'0.001 - 100 keV Luminosity [10^42 erg/s]           : '+strtrim(string(Fcbol*factor),2)+' (-'+strtrim(string(Fcbol_lo_err*factor),2)+', +'+strtrim(string(Fcbol_hi_err*factor),2)+')'
         printf,unit,' '
      endif
      if (mkcflow_flux eq 1) then begin
         printf,unit,'Cooling Fluxes inside Cooling region (1 sigma errors)'
         printf,unit,'---------------------------------------------------'
;      printf,unit,'X-ray flux (0.5 - 7.0 keV) [erg/s/cm^2]       : '+strtrim(string(Fxray_cooling),2)+' (-'+strtrim(string(Fxray_cooling_lo_err),2)+', +'+strtrim(string(Fxray_cooling_hi_err),2)+')'
         printf,unit,'0.5 - 7.0 keV flux corrected for nH [erg/s/cm^2]   : '+strtrim(string(Fcxray_cooling),2)+' (-'+strtrim(string(Fcxray_cooling_lo_err),2)+', +'+strtrim(string(Fcxray_cooling_hi_err),2)+')'
;      printf,unit,'Bolometric flux (0.001 - 100 keV) [erg/s/cm^2] : '+strtrim(string(Fbol_cooling),2)+' (-'+strtrim(string(Fbol_cooling_lo_err),2)+', +'+strtrim(string(Fbol_cooling_hi_err),2)+')'
         printf,unit,'0.001 - 100 keV flux corrected for nH [erg/s/cm^2] : '+strtrim(string(Fcbol_cooling),2)+' (-'+strtrim(string(Fcbol_cooling_lo_err),2)+', +'+strtrim(string(Fcbol_cooling_hi_err),2)+')'
         printf,unit,'0.001 - 100 keV Luminosity [10^42 erg/s]           : '+strtrim(string(Fcbol_cooling*factor),2)+' (-'+strtrim(string(Fcbol_cooling_lo_err*factor),2)+', +'+strtrim(string(Fcbol_cooling_hi_err*factor),2)+')'
         printf,unit,' '
      endif
   endif   
   printf,unit,'------------------------------------------'
   printf,unit,' '
   close,unit
   free_lun,unit
skip_logfile:


;
; Write data files
;
if (keyword_set(deprojected) or keyword_set(deproj_old_pipe)) then begin
   get_lun,unit
   outfile='cluster_properties_deproj.dat'
   openw,unit,outfile
   printf,unit,'# Cluster Properties
   printf,unit,'#'
   printf,unit,'# (1) Outer radius of central region [arcsec]'
   printf,unit,'# (2) Outer radius of central region [kpc]'
   printf,unit,'# (3) Central cooling time [yr]'
   printf,unit,'# (4) Central cooling time low error [yr]'
   printf,unit,'# (5) Central cooling time high error [yr]'
   printf,unit,'# (6) Central kT [keV]'
   printf,unit,'# (7) Central kT low error [keV]'
   printf,unit,'# (8) Central kT high error [keV]'
   printf,unit,'# (9) Central n_e [cm^-3]'
   printf,unit,'# (10) Central n_e low error [cm^-3]'
   printf,unit,'# (11) Central n_e high error [cm^-3]'
   printf,unit,'# (12) Central entropy [keV cm^2]'
   printf,unit,'# (13) Central entropy low error [keV cm^2]'
   printf,unit,'# (14) Central entropy high error [keV cm^2]'
   printf,unit,'# (15) Central pressure [keV cm^-3]'
   printf,unit,'# (16) Central pressure low error [keV cm^-3]'
   printf,unit,'# (17) Central pressure high error [keV cm^-3]'
   printf,unit,' '
   fmt='$(f9.3,3x,f9.3,3(3x,e13.5),6(3x,f9.4),3(3x,f9.4),3(3x,e13.5))'
   printf,unit,fmt,r_cent_limit,r_cent_limit_kpc,ct_cent,ct_cent_lo,ct_cent_hi,kT_cent,kT_cent_lo,kT_cent_hi, $
     n_e_cent,n_e_cent_lo,n_e_cent_hi,s_cent,s_cent_lo,s_cent_hi,pres_cent,pres_cent_lo,pres_cent_hi
   close,unit
   free_lun,unit 
endif
if keyword_set(projected) then begin
   outfile='cluster_properties_proj.dat'
endif
if keyword_set(cooling) then begin
   outfile='cluster_properties_cooling.dat'
endif


;
; Print status info to screen
;
print,' '
print,'CLUSTER_PROPERTIES complete.'
print,'Please see '+outfile+' for details.'

   
;
; Return to IDL
;
return
end