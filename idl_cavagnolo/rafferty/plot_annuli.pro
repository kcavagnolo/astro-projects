pro plot_annuli,deprojected=deprojected,projected=projected,old_pipe=old_pipe,central=central,spectra_dir=spectra_dir
;-----------------------------------------------------------------------
;
; Name: PLOT_ANNULI
;
; Purpose: Plot deprojected temperature, density, pressure, entropy,
;	   abundance, and surface brightness vs. radius for the annuli.  
;          Default behavior is to plot deprojected values
;          
; Inputs:  none
;
; Keywords: /DEPROJECTED - if set, only deprojected values are plotted
;	    /PROJECTED - if set, only projected values are plotted       
;         
; Comments: 
;           
;           
; Revision history:
;	fixed projected code (DR), 2006-6-26
;       written by D&L, 2002-12-10
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 0) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'plot_annuli [, /PROJECTED, /DEPROJECTED]'
   return   
endif
if (keyword_set(projected) or keyword_set(deprojected) or keyword_set(central)) then begin
endif else begin
   deprojected=1
endelse
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'


;
; If /DEPROJECTED is set, call radial_plots_deproj.pro
;
if keyword_set(deprojected) then begin
   print,'Please enter values for the following parameters:'
   print,' '
   read,redshift,prompt='                  Redshift: '
   ldist=lumdist(redshift,/silent)
   angscale=1.0/zang(1.0,redshift,/silent)
   print,' '
   print,'For H0=70, Omega_Lambda=0.7, Omega_M=0.3:'
   print,'   Luminosity distance = ',strtrim(string(ldist),2),' [Mpc]'
   print,'         Angular scale = ',strtrim(string(angscale),2),' [kpc/arcsec]'
   print,' '
   print,'Now making radial plots of deprojected fits...'
   if keyword_set(central) then begin
      if keyword_set(old_pipe) then begin
         radial_plots_deproj,redshift,ldist,angscale,/old_pipe,/central,spectra_dir=spectra_dir
      endif else begin
         radial_plots_deproj,redshift,ldist,angscale,/central,spectra_dir=spectra_dir
      endelse
      print,'...done'
   endif else begin
      if keyword_set(old_pipe) then begin
         radial_plots_deproj,redshift,ldist,angscale,/old_pipe,spectra_dir=spectra_dir
      endif else begin
         radial_plots_deproj,redshift,ldist,angscale,spectra_dir=spectra_dir
      endelse
      print,'...done'
   endelse
endif

;
; If /PROJECTED is set, make projected plots
;
if keyword_set(projected) then begin
   ;
   ; Read in annular properties
   ;
   readcol, './'+spectra_dir+'/radii.dat',r1,r2
   spec_file='./'+spectra_dir+'/source_1T.spectra'
   hd=headfits(spec_file,exten=1)
   nrad=sxpar(hd,'NUM_REG','Parameter NUM_REG not found')
     
  
   ;
   ; Find the surface brightness and count rate in each annulus
   ; (used later for deconvolution)
   ;
   get_lun,unit
   openr,unit,'ciao_info.txt'
   ciao_root=' '
   readf,unit,ciao_root
   cmdroot='source '+ciao_root+'/bin/ciao.csh ; '
   close,unit
   free_lun,unit


   hdbg=headfits('./background/bg.fits',exten=1)
   bkgn=sxpar(hdbg,'BKGNORM','Parameter BKGNORM not found')
   bkgnorm=strtrim(string(bkgn),2)

   cmdstring='punlearn dmextract'
   spawn,cmdroot+cmdstring,result

   regfile=findfile('regions/annuli_*.reg',count=num)
   if (num gt 1) then regfile=regfile[0]
   print,' '
   answer=' '
   print,'Using region file '+regfile+'.'
   read,answer,prompt='If OK, enter "y" otherwise enter filename: '
   if (answer ne 'y') then regfile=answer
   cmdstring='dmextract infile="./reprocessed/evt2_ccd_clean_no_ptsrc.fits[bin sky=@'+regfile+']" bkg="./background/bg.fits[bin sky=@'+regfile+']" outfile=./'+spectra_dir+'/rprofile.fits bkgnorm="'+bkgnorm+'"'
   spawn,cmdroot+cmdstring,result
   
   print,' '
   print,'Finding surface brightness and count rate in the annuli...'
   cmdstring='dmlist "./'+spectra_dir+'/rprofile.fits[cols area]" data,clean'
   spawn,cmdroot+cmdstring,result
   ;print,result
   area_annulus=strtrim(result(8:nrad+7),2)
   ;print,area_annulus
   ;cmdstring='dmlist "./'+spectra_dir+'/rprofile.fits[cols sur_bri_err]" data,clean'
   ;spawn,cmdroot+cmdstring,result
   ;sur_bri_err=strtrim(result(6:nrad+5),2)

   cmdstring='dmlist "./'+spectra_dir+'/rprofile.fits[cols net_rate]" data,clean'
   spawn,cmdroot+cmdstring,result
   net_rate=strtrim(result(8:nrad+7),2)

   cmdstring='dmlist "./'+spectra_dir+'/rprofile.fits[cols err_rate]" data,clean'
   spawn,cmdroot+cmdstring,result
   err_rate=strtrim(result(8:nrad+7),2)
   print,'...done.'


   ;
   ; Write the surface brightness and count rate to xsurf.dat
   ; for use later with deconv.f (ignore the outer annulus)
   ;
   ; Requires:
   ; rm = mean radius (arcsec)
   ; r1 = inner radius (arcsec)
   ; r2 = outer radius (arcsec)
   ; flux = count rate in annulus, in ct/sec (0.1 - 10.0 keV)
   ; xsurf = surf brightness, in ct/sec/arcmin^2 (0.1 - 10.0 keV)
   ; xerror = error in xsurf
   ; rml = log10 of rm
   ; erp = positive error bar in rml
   ; erm = negative error bar in rml
   ; xsurfl = log10 of xsurf
   ; exp = positive error bar in xsurfl
   ; exm = minus error bar in xsurfl
   ;
   arcsec=0.4919				; [arcsec/pixel]
   arcmin=arcsec/60				; [arcmin/pixel]
   outfile='./'+spectra_dir+'/xsurf.dat'
   get_lun,unit
   openw,unit,outfile
   fmt4='$(f8.3,a5,f8.3,a5,f8.3,a5,e10.3,a5,e10.3,a5,e10.3,a5,e10.3,a5,e10.3,a5,e10.3,a5,e10.3,a5,e10.3,a5,e10.3)'
   for i=0,nrad-2 do begin
      rin=r1[i]*arcsec							; [arcsec]
      rout=r2[i]*arcsec							; [arcsec]
      rmid=(rin+rout)/2							; [arcsec]
      rwidth=(rout-rin)/2						; [arcsec]
      flux=net_rate(i)							; [cts/sec]
      xsurf= double(net_rate(i))/double(area_annulus(i))/arcmin^2	; [cts/sec/arcmin^2]
      xerror=double(err_rate(i))/double(area_annulus(i))/arcmin^2	; [cts/sec/arcmin^2]
      rml=alog10(rmid)
      erp=alog10(rwidth)
      erm=erp
      xsurfl=alog10(xsurf)
      expl=alog10(xerror/xsurf+1)
      exml=-alog10(1-xerror/xsurf)
   
      printf,unit,fmt4,rmid,'     ',rin,'     ',rout,'     ',flux,'     ', $
         		    xsurf,'     ',xerror,'     ',rml,'     ',erp,'     ', $
         		    erm,'     ',xsurfl,'     ',expl,'     ',exml
   endfor
   close,unit
   free_lun,unit


   ;
   ; Change to the spectra directory
   ;
   cd,spectra_dir
   

   ;
   ; Call the deconvolution program deconv with these inputs
   ;
   ; file xsurf.dat, giving the radii and flux of annuli
   ; dmpc = angular diamter distance (Mpc)
   ; z = redshift
   ; xlamb = x-ray emissivity coeffient, such that
   ; 	the xray emissivity emiss (ergs/cm^3/sec)
   ;	in the band (0.1 -- 2.4 keV)*(1+z)
   ;	is emiss = xlamb*xne**2
   ;	where xne is the electron density
   ;
   print,'Now running deconv...'
   print,'Enter the angular diameter distance [Mpc]'
   print,'Hit ENTER.'
   print,'Enter the redshift.'
   print,'Hit ENTER.'
   print,'Finaly, enter 1 and hit ENTER.'
   cmdstring='deconv'
   spawn,cmdstring
   print,'...done'


   ;
   ; Change back to the obs directory
   ;
   cd,'..'
   print,'Please enter values for the following parameters:'
   print,' '
   read,redshift,prompt='                  Redshift: '
   ldist=lumdist(redshift,/silent)
   angscale=1.0/zang(1.0,redshift,/silent)
   print,' '
   print,'For H0=70, Lambda0=0.7, LamdbdaM=0.3:'
   print,'   Luminosity distance = ',strtrim(string(ldist),2),' [Mpc]'
   print,'         Angular scale = ',strtrim(string(angscale),2),' [kpc/arcsec]'
   print,' '
   print,'Now making radial plots of projected fits...'
   radial_plots_proj,angscale,spectra_dir=spectra_dir
   print,'...done'
endif


;
; Update log file
;
;
; Read in header from evt2_ccd_clean file
;
hd=headfits('./reprocessed/evt2_ccd_clean.fits',exten=1)


;
; Get OBS_ID out of evt2 header
;
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')


;
; Update obs_info file
;
get_lun,unit
outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'
openw,unit,outfile,/append
append_text='I'
if keyword_set(deprojected) then append_text=append_text+', /DEPROJECTED'
if keyword_set(projected) then append_text=append_text+', /PROJECTED'
printf,unit,'Output of PLOT_ANNUL'+append_text
printf,unit,!stime
printf,unit,' '
if keyword_set(deprojected) then begin
printf,unit,'Cosmology: H0=70 [km/s/Mpc], Lambda0=0.7, LambdaM=0.3'
printf,unit,'Redshift = '+strtrim(string(redshift),2)
printf,unit,'Luminosity Distance = '+strtrim(string(ldist),2)+' [Mpc]'
printf,unit,'Angular Scale = '+strtrim(string(angscale),2)+' [kpc/arcsec]'
printf,unit,' '
endif
if keyword_set(deprojected) then begin
   printf,unit,'Plots of kT, rho, P, S (deprojected)  : radial_plots_deproj_1.ps'
   printf,unit,'Plots of Ab, SB (deprojected)         : radial_plots_deproj_2.ps'
   printf,unit,'Plots of cooling times (deprojected)  : cooling_time_deproj.ps'
endif
if keyword_set(projected) then begin
   printf,unit,'Plots of kT, rho, P, S (projected)    : radial_plots_proj_1.ps'
   printf,unit,'Plots of nH, Ab, chi, SB (projected)  : radial_plots_proj_2.ps'
;   printf,unit,'Plots of cooling times (projected)    : cooling_time_proj.ps'
endif
printf,unit,' '
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit


;
; Print status info to screen
;
print,' '
print,'PLOT_ANNULI complete.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end
