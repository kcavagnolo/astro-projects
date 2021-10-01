pro fit_spectra,deprojected=deprojected,projected=projected, $
                cooling=cooling,spectra_dir=spectra_dir, $
                central=central,old_pipe=old_pipe,flux=flux, $
                cool_fluxes=cool_fluxes
;-----------------------------------------------------------------------
;
; Name: FIT_SPECTRA
;
; Purpose: Creates scripts and fits the spectra extracted by
;	   EXTRACT_ANNULI with a single temperature model. Default
;	   behavior is to fit deprojected model.
;          
;          
; Inputs:  none
;
; Optional inputs: spectra_dir - name of the spectra directory (default is 'spectra')
;
; Keywords: /DEPROJECTED - if set, only deprojected model is fit (projct*
;			   wabs*mekal; requires XSPEC >= 11.3.1)
;	    /PROJECTED - if set, only projected model is fit (wabs*mekal)
;	    /COOLING - if set, only cooling model is fit (projct*wabs*
;		       [mekal+mkcflow]; requires XSPEC >= 11.3.1)
;	    /CENTRAL - if set, deprojected model with new central regions is fit
;	    /OLD_PIPE - if set, old pipeline naming conventions followed
;	    /FLUX - if set, fluxes are calculated
;	    /COOL_FLUXES - if set, the cooling and X-ray fluxes inside the cooling radius 
;		           are calculated (must run EXTRACT_COOLING first)
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by DR, 2004-6-29
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 0) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'fit_spectra [, /PROJECTED, /DEPROJECTED, /COOLING, /FLUX, /XRAY_FLUX, /COOL_FLUX, /CENTRAL, /OLD_PIPE, spectra_dir=spectra_dir]'
   return   
endif
if (keyword_set(cooling) or keyword_set(projected) or $
    keyword_set(deprojected) or keyword_set(central) or $
    keyword_set(cool_fluxes) ) then begin
endif else begin
   deprojected=1
endelse


;
; Set default spectra directory
;
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'


;
; Define command root for HEASOFT
;
get_lun,unit
openr,unit,'ciao_info.txt'
ciao_root=' '
lheasoft_root=' '
readf,unit,ciao_root
readf,unit,lheasoft_root
cmdroot='source '+lheasoft_root+' ; '
close,unit
free_lun,unit


;
; Prompt for initial parameter guesses
;
print,' '
print,' '
print,'Please enter values for the following parameters:'
print,' '
read,nreg,prompt='       Number of regions to fit: '
if ( keyword_set(cooling) or keyword_set(cool_fluxes) ) then read,ncool,prompt='          Cooling region number: '
read,nH_gal,prompt='      Galactic nH [10^22 cm^-2]: '
read,kT_guess,prompt='         kT initial guess [keV]: '
if ( keyword_set(cooling) or keyword_set(cool_fluxes) ) then read,kT_low,prompt='     Cooling model low kT [keV]: '
read,ab_guess,prompt='         Abundance inital guess: '
read,redshift,prompt='                       Redshift: '
read,lo_energy,prompt='        Low energy cutoff [keV]: '
read,hi_energy,prompt='       High energy cutoff [keV]: '
read,bin,prompt='                 Counts per bin: '
print,' '
inner_bound_answer=' '
read,inner_bound_answer,prompt='Did you remove a central point source? (y/n) '
answer_check1:
if ( (inner_bound_answer ne 'y') and (inner_bound_answer ne 'n') ) then begin
   read,inner_bound_answer,prompt='Please type "y" or "n": '
   goto,answer_check1
endif
binning=fix(bin)
if (inner_bound_answer eq 'y') then begin
   read,inner_a,prompt='Enter semi-major axis of inner boundary [pixels]: '
   read,inner_b,prompt='Enter semi-minor axis of inner boundary [pixels]: '
   read,inner_pa,prompt='Enter position angle of inner boundary [degrees]: '
endif else begin
   inner_a=0.0
   inner_b=0.0
   inner_pa=0.0
endelse


;
; Print summary to screen for verification
;
verification:
fmt0='$(a30,i7,a14)'
fmt1='$(a30,f7.3,a14)'
print,' '
print,' '
print,'         Summary of parameter values:'
print,'-------------------------------------------------'
print,fmt0,'1) Number of regions to fit = ',nreg
print,fmt1,'2)              Galactic nH = ',nH_gal,   ' [10^22 cm^-2]' 
print,fmt1,'3)                       kT = ',kT_guess, ' [keV]        '
print,fmt1,'4)                Abundance = ',ab_guess, ' [solar]      '
print,fmt1,'5)                 Redshift = ',redshift
print,fmt1,'6)               Low energy = ',lo_energy,' [keV]        '
print,fmt1,'7)              High energy = ',hi_energy,' [keV]        '
print,fmt0,'8)                  Binning = ',binning,  ' [counts]     '
print,fmt1,'9)    Inner semi-major axis = ',inner_a,  ' [pixels]     '
print,fmt1,'10)   Inner semi-minor axis = ',inner_b,  ' [pixels]     '
print,fmt1,'11)    Inner position angle = ',inner_pa, ' [degrees]    '
if (keyword_set(cooling) or keyword_set(cool_fluxes)) then print,fmt0,'12)   Cooling region number = ',ncool
if (keyword_set(cooling) or keyword_set(cool_fluxes)) then print,fmt1,'13)    Cooling model low kT = ',kT_low, ' [keV]        '
print,'-------------------------------------------------'
print,' '
answer=' '
read,answer,prompt='Are these values correct? (y/n) '
answer_check2:
if ( (answer ne 'y') and (answer ne 'n') ) then begin
   read,answer,prompt='Please type "y" or "n": '
   goto,answer_check2
endif


;
; If incorrect, ask for correction
;
if (answer eq 'n') then begin
   print,' '
   if (keyword_set(cooling) or keyword_set(cool_fluxes)) then begin
      read,par_to_correct,prompt='Enter number of parameter to correct (1-13): '
   endif else begin
      read,par_to_correct,prompt='Enter number of parameter to correct (1-11): '
   endelse
   par_check:
   if (keyword_set(cooling) or keyword_set(cool_fluxes)) then begin
      if ( (par_to_correct lt 1) or (par_to_correct gt 13) ) then begin
         read,par_to_correct,prompt='Please enter a number between 1 and 13: '
         goto,par_check
      endif
   endif else begin
      if ( (par_to_correct lt 1) or (par_to_correct gt 11) ) then begin
         read,par_to_correct,prompt='Please enter a number between 1 and 11: '
         goto,par_check
      endif     
   endelse
   if (par_to_correct eq 1) then read,nreg,prompt='       Number of regions to fit: '
   if (par_to_correct eq 2) then read,nH_gal,prompt='      Galactic nH [10^22 cm^-2]: '
   if (par_to_correct eq 3) then read,kT_guess,prompt='         kT initial guess [keV]: '
   if (par_to_correct eq 4) then read,ab_guess,prompt='         Abundance inital guess: '
   if (par_to_correct eq 5) then read,redshift,prompt='                       Redshift: '
   if (par_to_correct eq 6) then read,lo_energy,prompt='        Low energy cutoff [keV]: '
   if (par_to_correct eq 7) then read,hi_energy,prompt='       High energy cutoff [keV]: '
   if (par_to_correct eq 8) then read,binning,prompt='Counts per bin (0 = no binning): '
   if (par_to_correct eq 9) then read,inner_a,prompt='Enter semi-major axis of inner boundary [pixels]: '
   if (par_to_correct eq 10) then read,inner_b,prompt='Enter semi-minor axis of inner boundary [pixels]: '
   if (par_to_correct eq 11) then read,inner_pa,prompt='Enter position angle of inner boundary [degrees]: '
   if (par_to_correct eq 12) then read,ncool,prompt='    Outer cooling region number: '
   if (par_to_correct eq 13) then read,kT_low,prompt='     Cooling model low kT [keV]: '
   goto,verification
endif


;
; If correct, group photons
;
print,' '
if (binning eq 1) then begin
   print,'Binning set to 1: no binning will be done.'
   print,' '
endif else begin
   print,'Binning counts ('+strtrim(string(binning),2)+' counts per bin) with GRPPHA...'
   print,' '
   cd,spectra_dir
   if keyword_set(central) then begin
      for i=1,2 do begin
         n=fix(i)
         g_file='central_reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
         group_file=findfile(g_file,count=num)
         if (num eq 0) then begin
            print,'Now grouping photons for central_reg'+strtrim(string(n),2)+' ...'
            pi_file='central_reg'+strtrim(string(n),2)+'_sou.pi'
            out_file='central_reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
            cmdstring='grppha '+pi_file+' '+out_file+' comm="group min '+strtrim(string(binning),2)+'" tempc="exit"'
            spawn,cmdroot+cmdstring,result
         endif else begin
            print,'WARNING: central_reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi already exists.'
            print,'No grouping performed for this region.'
         endelse
      endfor
   endif 
   if (keyword_set(cooling) or keyword_set(cool_fluxes)) then begin
      for i=1,ncool-1 do begin
         n=fix(i)
         g_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
         group_file=findfile(g_file,count=num)
         if (num eq 0) then begin
            print,'Now grouping photons for reg'+strtrim(string(n),2)+' ...'
            pi_file='reg'+strtrim(string(n),2)+'_sou.pi'
            out_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
            cmdstring='grppha '+pi_file+' '+out_file+' comm="group min '+strtrim(string(binning),2)+'" tempc="exit"'
            spawn,cmdroot+cmdstring,result
         endif else begin
            print,'WARNING: reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi already exists.'
            print,'No grouping performed for this region.'
         endelse
      endfor
      if (ncool lt nreg) then begin
         for i=ncool,ncool+1 do begin
            n=fix(i)
            g_file='cooling_reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
            group_file=findfile(g_file,count=num)
            if (num eq 0) then begin
               print,'Now grouping photons for cooling_reg'+strtrim(string(n),2)+' ...'
               pi_file='cooling_reg'+strtrim(string(n),2)+'_sou.pi'
               out_file='cooling_reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
               cmdstring='grppha '+pi_file+' '+out_file+' comm="group min '+strtrim(string(binning),2)+'" tempc="exit"'
               spawn,cmdroot+cmdstring,result
            endif else begin
               print,'WARNING: cooling_reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi already exists.'
               print,'No grouping performed for this region.'
            endelse
         endfor
      endif else begin
         n=fix(nreg)
         g_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
         group_file=findfile(g_file,count=num)
         if (num eq 0) then begin
            print,'Now grouping photons for reg'+strtrim(string(n),2)+' ...'
            pi_file='reg'+strtrim(string(n),2)+'_sou.pi'
            out_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
            cmdstring='grppha '+pi_file+' '+out_file+' comm="group min '+strtrim(string(binning),2)+'" tempc="exit"'
            spawn,cmdroot+cmdstring,result
         endif else begin
            print,'WARNING: reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi already exists.'
            print,'No grouping performed for this region.'
         endelse
      endelse
      if (nreg ge (ncool+2)) then begin
         for i=ncool+2,nreg do begin
            n=fix(i)
            g_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
            group_file=findfile(g_file,count=num)
            if (num eq 0) then begin
               print,'Now grouping photons for reg'+strtrim(string(n),2)+' ...'
               pi_file='reg'+strtrim(string(n),2)+'_sou.pi'
               out_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
               cmdstring='grppha '+pi_file+' '+out_file+' comm="group min '+strtrim(string(binning),2)+'" tempc="exit"'
               spawn,cmdroot+cmdstring,result
            endif else begin
               print,'WARNING: reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi already exists.'
               print,'No grouping performed for this region.'
            endelse
         endfor
      endif
   endif 
   if (keyword_set(projected) or keyword_set(deprojected)) then begin
      for i=1,nreg do begin
         n=fix(i)
         g_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
         group_file=findfile(g_file,count=num)
         if (num eq 0) then begin
            print,'Now grouping photons for reg'+strtrim(string(n),2)+' ...'
            pi_file='reg'+strtrim(string(n),2)+'_sou.pi'
            out_file='reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
            cmdstring='grppha '+pi_file+' '+out_file+' comm="group min '+strtrim(string(binning),2)+'" tempc="exit"'
            spawn,cmdroot+cmdstring,result
         endif else begin
            print,'WARNING: reg'+strtrim(string(n),2)+'_sou_g'+strtrim(string(binning),2)+'.pi already exists.'
            print,'No grouping performed for this region.'
         endelse
      endfor
   endif
   cd,'..'
   print,'...done.'
endelse
cd,spectra_dir


;
; If /DEPROJECTED keyword set, generate XSPEC script
;
if keyword_set(deprojected) then begin

   print,' '
   print,'Now generating XSPEC script for deprojection...'
   if keyword_set(flux) then begin 
      make_projct_script,nreg,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,inner_a,inner_b,inner_pa
   endif else begin
      make_projct_script,nreg,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,inner_a,inner_b,inner_pa,/noflux
   endelse


   ;
   ; Run XSPEC
   ;
   cmdstring='xspec - projct_wabs_mekal.xcm'
   spawn,cmdroot+cmdstring

endif


;
; If /CENTRAL keyword set, generate XSPEC script
;
if keyword_set(central) then begin

   print,' '
   print,'Now generating XSPEC script for deprojection...'
   if keyword_set(old_pipe) then begin
      make_projct_script,nreg,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,/central,/old_pipe,inner_a,inner_b,inner_pa
   endif else begin
      make_projct_script,nreg,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,/central,inner_a,inner_b,inner_pa
   endelse
   

   ;
   ; Run XSPEC
   ;
   cmdstring='xspec - projct_wabs_mekal.xcm'
   spawn,cmdroot+cmdstring

endif


;
; If /PROJECTED keyword set, generate XSPEC script
;
if keyword_set(projected) then begin

   print,' '
   print,'Now generating XSPEC script...' 
   if keyword_set(flux) then begin 
      make_mekal_script,nreg,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,/flux
   endif else begin
      make_mekal_script,nreg,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning
   endelse

   ;
   ; Run XSPEC
   ;
   cmdstring='xspec - wabs_mekal.xcm'
   spawn,cmdroot+cmdstring


endif


;
; If /COOLING keyword set, generate XSPEC script
;
if keyword_set(cooling) then begin

   print,' '
   print,'Now generating XSPEC script...'
   if keyword_set(flux) then begin 
      make_mkcflow_script,nreg,ncool,nH_gal,kT_low,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,inner_a,inner_b,inner_pa,/flux
   endif else begin
      make_mkcflow_script,nreg,ncool,nH_gal,kT_low,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,inner_a,inner_b,inner_pa
   endelse
   

   ;
   ; Run XSPEC
   ;
   cmdstring='xspec - projct_wabs_mekal_mkcflow.xcm'
   spawn,cmdroot+cmdstring

endif


;
; If /COOL_FLUXES keyword set, generate XSPEC script
;
if keyword_set(cool_fluxes) then begin

   print,' '
   print,'Now generating XSPEC scripts...'
   make_xray_flux_script,nreg,ncool,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,inner_a,inner_b,inner_pa
;   make_cooling_flux_script_noerr,nreg,ncool,nH_gal,kT_low,kT_guess,ab_guess,redshift,lo_energy,hi_energy,binning,inner_a,inner_b,inner_pa


   ;
   ; Run XSPEC twice
   ;
   cmdstring='xspec - projct_wabs_mekal_flux.xcm'
   spawn,cmdroot+cmdstring
   
;   cmdstring='xspec - projct_mkcflow_flux.xcm'
;   spawn,cmdroot+cmdstring

endif


;
; Change back to OBS directory and
; read in header from evt2_ccd_clean file
;
cd,'..'
hd=headfits('./reprocessed/evt2_ccd_clean.fits',exten=1)


;
; Get OBS_ID out of evt2 header
;
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')


;
; Update info file
;
get_lun,unit
outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'
openw,unit,outfile,/append
append_text='A'
if keyword_set(deprojected) then append_text=append_text+', /DEPROJECTED'
if keyword_set(projected) then append_text=append_text+', /PROJECTED'
if keyword_set(cooling) then append_text=append_text+', /COOLING'
if keyword_set(central) then append_text=append_text+', /CENTRAL'
if keyword_set(cool_fluxes) then append_text=append_text+', /COOL_FLUXES'
printf,unit,'Output of FIT_SPECTR'+append_text+", spectra_dir='"+spectra_dir+"'"
printf,unit,!stime
printf,unit,' '
if (keyword_set(deprojected) or keyword_set(central)) then begin
   printf,unit,'MEKAL XSPEC script              : ./'+spectra_dir+'/projct_wabs_mekal.xcm'
   printf,unit,'MEKAL XSPEC output log          : ./'+spectra_dir+'/xspec_mekal.log'
   printf,unit,'Plot of PROJCT*WABS*MEKAL fit   : ./'+spectra_dir+'/plot_mekal.ps'
   printf,unit,'PROJCT*WABS*MEKAL model spectra : ./'+spectra_dir+'/source_mekal.spectra'
   printf,unit,' '
endif
if keyword_set(projected) then begin
   printf,unit,'MEKAL XSPEC script              : ./'+spectra_dir+'/wabs_mekal.sl'
   printf,unit,'MEKAL XSPEC output log          : ./'+spectra_dir+'/xspec_1T.log'
   printf,unit,'Plots of WABS*MEKAL fit         : ./'+spectra_dir+'/plot_1T.ps'
   printf,unit,'WABS*MEKAL model spectra        : ./'+spectra_dir+'/source_1T.spectra'
   printf,unit,' '
endif
if keyword_set(cooling) then begin
   printf,unit,'MKCFLOW XSPEC script                      : ./'+spectra_dir+'/projct_wabs_mekal_mkcflow.xcm'
   printf,unit,'MKCFLOW XSPEC output log                  : ./'+spectra_dir+'/xspec_mkcflow.log'
   printf,unit,'Plot of PROJCT*WABS*(MEKAL+MKCLFOW) fit   : ./'+spectra_dir+'/pgplot_mkcflow.ps'
   printf,unit,'PROJCT*WABS*(MEKAL+MKCLFOW) model spectra : ./'+spectra_dir+'/source_mkcflow.spectra'
   printf,unit,' '
endif
if keyword_set(cool_fluxes) then begin
   printf,unit,'X-ray flux XSPEC script                : ./'+spectra_dir+'/projct_wabs_mekal_flux.xcm'
   printf,unit,'X-ray flux XSPEC output log            : ./'+spectra_dir+'/xspec_mekal_flux.log'
   printf,unit,'PROJCT*WABS*(MEKAL) flux model spectra : ./'+spectra_dir+'/source_mekal_flux.spectra'
   printf,unit,'Cooling flux XSPEC script              : ./'+spectra_dir+'/projct_mkcflow_flux.xcm'
   printf,unit,'Cooling flux XSPEC output log          : ./'+spectra_dir+'/xspec_mkcflow_flux.log'
   printf,unit,'PROJCT*WABS*(MEKAL+MKCLFOW) spectra    : ./'+spectra_dir+'/source_mkcflow_flux.spectra'
   printf,unit,' '
endif
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit


;
; Print status info to screen
;
print,' '
print,'FIT_SPECTRA complete.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end
