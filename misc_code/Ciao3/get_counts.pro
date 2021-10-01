pro get_counts,evt_file,region,bkg_file=bkg_file,bkg_region=bkg_region,$
		rootstr=rootstr,ciao_path=ciao_path,energy=energy,clobber=clobber,$
		expmap=expmap,bkg_expmap=bkg_expmap,image=image
;-----------------------------------------------------------------------
;
; Name: GET_COUNTS
;
; Purpose: Finds the number of counts in a region of the source file
;          
;                  
; Inputs:  evt_file - source file
;	   region - region of interest
;
; Optional Inputs:
;	   bkg_file - background file 
;	   bkg_region - region for background area of source file (if
;			no background file is specified)
;	   expmap - exposure map for the source file
;	   bkg_expmap - exposure map for the bkg_file
;	   rootstr - root sting for output file names 
;          ciao_path - path to ciao initialization script 
;	   energy - energy range to filter the source file in the
;		    format 'loenergy:hienergy' in eV (e.g. '500:7000')
;	   /COBBLER - if set, overwrite existing files
;	   /IMAGE - set if the source file is an image and not an event
;		    list
;         
; Comments: 
;           
;           
; Revision history:
;       written by D&L, 2004-3-3 
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 7)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'get_counts, evt_file, region_file [, bkg_file=bkg_file, bkg_region=bkg_region,', $
   		 'rootstr=rootstr, ciao_path=ciao_path, energy=energy, /clobber,', $
   		 'expmap=expmap, bkg_expmap=bkg_expmap, /IMAGE]'
   return   
endif


;
; Define defaults 
;
if (n_elements(ciao_path) eq 0) then begin
   cmdroot='source /export/home/Rafferty/Applications/ciao_3.3/bin/ciao.csh ; '
endif else begin
   cmdroot='source '+ciao_path+' ; '
endelse

pos=strpos(region,'.reg')   
region_root=strmid(region,0,pos) 
if (n_elements(rootstr) eq 0) then begin
   rootstr=region_root
endif 

if (n_elements(energy) eq 0) then energy='500:7000'


;
; Define the file names
;
if keyword_set(image) then infile=evt_file+'[bin sky=region('+region+')]' else infile=evt_file+'[energy='+energy+'][bin sky=region('+region+')]'
data_file=rootstr+'.dat'
txtfile=rootstr+'.txt'
fitsfile=rootstr+'.fits'


;
; Check that output file name is valid
;
if keyword_set(clobber) then begin
   dat_file=findfile(data_file,count=num)
   if (num gt 0) then begin
      rm_file,dat_file
   endif
endif else begin
   dat_file=findfile(data_file,count=num)
   if (num gt 0) then begin
      print,'ERROR:'
      print,'   '+data_file+' already exists!'
      print,'   Please choose a different root string, rename the file,'
      print,'   or use the /CLOBBER keyword.'
      return
   endif
   txt_file=findfile(txtfile,count=num)
   if (num gt 0) then begin
      print,'ERROR:'
      print,'   '+txtfile+' already exists!'
      print,'   Please choose a different root string, rename the file,'
      print,'   or use the /CLOBBER keyword.'      
      return
   endif
   fits_file=findfile(fitsfile,count=num)
   if (num gt 0) then begin
      print,'ERROR:'
      print,'   '+fitsfile+' already exists!'
      print,'   Please choose a different root string, rename the file,'
      print,'   or use the /CLOBBER keyword.'      
      return
   endif
endelse


;
; Find the info for the region 
;
if (n_elements(bkg_file) eq 0) then begin
   if (n_elements(bkg_region) eq 0) then begin
      cmdstring='dmextract infile="'+infile+'" bkg=NONE outfile='+rootstr+'.fits'
      if (n_elements(expmap) ne 0) then cmdstring=cmdstring+' exp='+expmap
      if keyword_set(clobber) then cmdstring=cmdstring+' clobber=yes'
      spawn,cmdroot+cmdstring,result

      cmdstring='dmlist "'+rootstr+'.fits[cols counts,err_counts,count_rate,area,exposure,net_rate,err_rate,sur_bri,sur_bri_err]" data,clean outfile='+rootstr+'.dat'
      spawn,cmdroot+cmdstring,result
         
      readcol,data_file,counts,err_counts,count_rate,area,exposure,net_rate,err_rate,sur_bri,sur_bri_err
   
      ;
      ; Print the results to the screen and to a file
      ;
      outfile=rootstr+'.txt'
      get_lun,unit
      openw,unit,outfile
      printf,unit,' '
      printf,unit,'Info for the '+region_root+' region of '+evt_file
      printf,unit,'for energies between '+energy+' eV.'
      if (n_elements(expmap) ne 0) then begin
         printf,unit,'Counts and rates are corrected for exposure'
         printf,unit,'given by '+expmap+'.'
      endif
      printf,unit,'(no background file or region specified)'
      printf,unit,' '
      printf,unit,'   Source properties '
      printf,unit,'----------------------------------------------'
      printf,unit,'Counts inside region: '+strtrim(string(counts),2)+' +/- '+strtrim(string(err_counts),2)
      printf,unit,'Area of region      : '+strtrim(string(area),2)+' [pixel^2]'
      printf,unit,'Exposure time       : '+strtrim(string(exposure),2)+' [s]'
      printf,unit,'Count rate          : '+strtrim(string(net_rate),2)+' +/- '+strtrim(string(err_rate),2)+' [count/s]'
      printf,unit,'Count rate per pixel: '+strtrim(string(net_rate/area),2)+' +/- '+strtrim(string(err_rate/area),2)+' [count/s/pixel]'
      printf,unit,'Surface brightness  : '+strtrim(string(sur_bri),2)+' +/- '+strtrim(string(sur_bri_err),2)+' [count/pixel^2]'
      printf,unit,'----------------------------------------------'
      printf,unit,' '
      close,unit
      free_lun,unit   

      print,' '
      print,'Info for the '+region_root+' region of '+evt_file
      print,'for energies between '+energy+' eV.'
      if (n_elements(expmap) ne 0) then begin
         print,'Counts and rates are corrected for exposure'
         print,'given by '+expmap+'.'
      endif
      print,'(no background file or region specified)'
      print,' '
      print,'   Source properties '
      print,'----------------------------------------------'
      print,'Counts inside region: '+strtrim(string(counts),2)+' +/- '+strtrim(string(err_counts),2)
      print,'Area of region      : '+strtrim(string(area),2)+' [pixel^2]'
      print,'Exposure time       : '+strtrim(string(exposure),2)+' [s]'
      print,'Count rate          : '+strtrim(string(net_rate),2)+' +/- '+strtrim(string(err_rate),2)+' [count/s]'
      print,'Count rate per pixel: '+strtrim(string(net_rate/area),2)+' +/- '+strtrim(string(err_rate/area),2)+' [count/s/pixel]'
      print,'Surface brightness  : '+strtrim(string(sur_bri),2)+' +/- '+strtrim(string(sur_bri_err),2)+' [count/pixel^2]'
      print,'----------------------------------------------'
      print,' '
   
   endif else begin
      
      bkgfilter=evt_file+'[energy='+energy+'][bin sky=region('+bkg_region+')]'
      
      cmdstring='dmextract infile="'+infile+'" bkg="'+bkgfilter+'" outfile='+rootstr+'.fits'
      if (n_elements(expmap) ne 0) then cmdstring=cmdstring+' exp='+expmap+' bkgexp='+expmap
      if keyword_set(clobber) then cmdstring=cmdstring+' clobber=yes'
      spawn,cmdroot+cmdstring,result

      cmdstring='dmlist "'+rootstr+'.fits[cols counts,err_counts,count_rate,area,exposure,bg_counts,bg_err,bg_rate,bg_area,bg_exposure,net_rate,err_rate,sur_bri,sur_bri_err]" data,clean outfile='+rootstr+'.dat clobber=yes'
      if keyword_set(clobber) then cmdstring=cmdstring+' clobber=yes'
      spawn,cmdroot+cmdstring,result      
         
      readcol,data_file,counts,err_counts,count_rate,area,exposure,bg_counts,bg_err,bg_rate,bg_area,bg_exposure,net_rate,err_rate,sur_bri,sur_bri_err

      ;
      ; Print the results to the screen and to a file
      ;
      outfile=rootstr+'.txt'
      get_lun,unit
      openw,unit,outfile
      printf,unit,' '
      printf,unit,'Info for the '+region_root+' region of '+evt_file
      printf,unit,'for energies between '+energy+' eV.'
      if (n_elements(expmap) ne 0) then begin
         printf,unit,'Counts and rates are corrected for exposure'
         printf,unit,'given by '+expmap+'.'
      endif
      printf,unit,'(with '+bkgfilter+' as background)'
      printf,unit,' '
      printf,unit,'   Source properties '
      printf,unit,'----------------------------------------------'
      printf,unit,'Counts inside region: '+strtrim(string(counts),2)+' +/- '+strtrim(string(err_counts),2)
      printf,unit,'Area of region      : '+strtrim(string(area),2)+' [pixel^2]'
      printf,unit,'Exposure time       : '+strtrim(string(exposure),2)+' [s]'
      printf,unit,'Count rate          : '+strtrim(string(count_rate),2)+' [count/s]'
      printf,unit,'Count rate per pixel: '+strtrim(string(count_rate/area),2)+' [count/s/pixel]'
      printf,unit,'----------------------------------------------'
      printf,unit,' '
      printf,unit,'   Background properties '
      printf,unit,'----------------------------------------------'
      printf,unit,'Counts inside region: '+strtrim(string(bg_counts),2)+' +/- '+strtrim(string(bg_err),2)
      printf,unit,'Area of region      : '+strtrim(string(bg_area),2)+' [pixel^2]'
      printf,unit,'Exposure time       : '+strtrim(string(bg_exposure),2)+' [s]'
      printf,unit,'Count rate          : '+strtrim(string(bg_rate),2)+' [count/s]'
      printf,unit,'Count rate per pixel: '+strtrim(string(bg_rate/bg_area),2)+' [count/s/pixel]'
      printf,unit,'----------------------------------------------'
      printf,unit,' '
      printf,unit,'   Background-subtracted properties '
      printf,unit,'----------------------------------------------'
      printf,unit,'Net count rate      : '+strtrim(string(net_rate),2)+' +/- '+strtrim(string(err_rate),2)+' [count/s]'
      printf,unit,'Net rate per pixel  : '+strtrim(string(net_rate/area),2)+' +/- '+strtrim(string(err_rate/area),2)+' [count/s/pixel]'
      printf,unit,'Surface brightness  : '+strtrim(string(sur_bri),2)+' +/- '+strtrim(string(sur_bri_err),2)+' [count/pixel^2]'
      printf,unit,'----------------------------------------------'
      printf,unit,' '
      close,unit
      free_lun,unit   
      
      print,' '
      print,' '
      print,' '
      print,'Info for the '+region_root+' region of '+evt_file
      print,'for energies between '+energy+' eV.'
      if (n_elements(expmap) ne 0) then begin
         printf,'Counts and rates are corrected for exposure'
         printf,'given by '+expmap+'.'
      endif
      print,'(with '+bkgfilter+' as background)'
      print,' '
      print,'   Source properties '
      print,'----------------------------------------------'
      print,'Counts inside region: '+strtrim(string(counts),2)+' +/- '+strtrim(string(err_counts),2)
      print,'Area of region      : '+strtrim(string(area),2)+' [pixel^2]'
      print,'Exposure time       : '+strtrim(string(exposure),2)+' [s]'
      print,'Count rate          : '+strtrim(string(count_rate),2)+' [count/s]'
      print,'Count rate per pixel: '+strtrim(string(count_rate/area),2)+' [count/s/pixel]'
      print,'----------------------------------------------'
      print,' '
      print,'   Background properties '
      print,'----------------------------------------------'
      print,'Counts inside region: '+strtrim(string(bg_counts),2)+' +/- '+strtrim(string(bg_err),2)
      print,'Area of region      : '+strtrim(string(bg_area),2)+' [pixel^2]'
      print,'Exposure time       : '+strtrim(string(bg_exposure),2)+' [s]'
      print,'Count rate          : '+strtrim(string(bg_rate),2)+' [count/s]'
      print,'Count rate per pixel: '+strtrim(string(bg_rate/bg_area),2)+' [count/s/pixel]'
      print,'----------------------------------------------'
      print,' '
      print,'   Background-subtracted properties '
      print,'----------------------------------------------'
      print,'Net count rate      : '+strtrim(string(net_rate),2)+' +/- '+strtrim(string(err_rate),2)+' [count/s]'
      print,'Net rate per pixel  : '+strtrim(string(net_rate/area),2)+' +/- '+strtrim(string(err_rate/area),2)+' [count/s/pixel]'
      print,'Surface brightness  : '+strtrim(string(sur_bri),2)+' +/- '+strtrim(string(sur_bri_err),2)+' [count/pixel^2]'
      print,'----------------------------------------------'
      print,' '
   endelse

      
   
endif else begin

   bgfile=bkg_file+'[energy='+energy+'][bin sky=region('+region+')]'
   hdbg=headfits(bkg_file,exten=1)
   bkgn=sxpar(hdbg,'BKGNORM',count=bkgn_cnt)
   if (bkgn_cnt eq 0) then bkgn='1.0'
   bkgnorm=strtrim(string(bkgn),2)
   
   cmdstring='dmextract infile="'+infile+'" bkg="'+bgfile+'" outfile='+rootstr+'.fits bkgnorm='+bkgnorm
   if (n_elements(expmap) ne 0) then cmdstring=cmdstring+' exp='+expmap
   if (n_elements(bkg_expmap) ne 0) then cmdstring=cmdstring+' bkgexp='+bkg_expmap
   if keyword_set(clobber) then cmdstring=cmdstring+' clobber=yes'
   spawn,cmdroot+cmdstring,result

   cmdstring='dmlist "'+rootstr+'.fits[cols counts,err_counts,count_rate,area,exposure,bg_counts,bg_err,bg_rate,bg_area,bg_exposure,net_rate,err_rate,sur_bri,sur_bri_err]" data,clean outfile='+rootstr+'.dat'
   if keyword_set(clobber) then cmdstring=cmdstring+' clobber=yes'
   spawn,cmdroot+cmdstring,result   
      
   readcol,data_file,counts,err_counts,count_rate,area,exposure,bg_counts,bg_err,bg_rate,bg_area,bg_exposure,net_rate,err_rate,sur_bri,sur_bri_err

   ;
   ; Print the results to the screen and to a file
   ;
   outfile=rootstr+'.txt'
   get_lun,unit
   openw,unit,outfile
   printf,unit,' '
   printf,unit,'Info for the '+region_root+' region of '+evt_file
   printf,unit,'for energies between '+energy+' eV.'
   if (n_elements(expmap) ne 0) then begin
      printf,unit,'Counts and rates are corrected for exposure'
      if (n_elements(bkg_expmap) eq 0) then printf,unit,'given by '+expmap+'.' else printf,unit,'given by '+expmap+' and '+bkg_expmap+'.'
   endif
   printf,unit,'(with '+bkg_file+' as background)'
   printf,unit,' '
   printf,unit,'   Source properties '
   printf,unit,'----------------------------------------------'
   printf,unit,'Counts inside region: '+strtrim(string(counts),2)+' +/- '+strtrim(string(err_counts),2)
   printf,unit,'Area of region      : '+strtrim(string(area),2)+' [pixel^2]'
   printf,unit,'Exposure time       : '+strtrim(string(exposure),2)+' [s]'
   printf,unit,'Count rate	    : '+strtrim(string(count_rate),2)+' [count/s]'
   printf,unit,'Count rate per pixel: '+strtrim(string(count_rate/area),2)+' [count/s/pixel]'
   printf,unit,'----------------------------------------------'
   printf,unit,' '
   printf,unit,'   Background properties (BKGNORM = '+bkgnorm+')'
   printf,unit,'----------------------------------------------'
   printf,unit,'Counts inside region: '+strtrim(string(bg_counts),2)+' +/- '+strtrim(string(bg_err),2)
   printf,unit,'Area of region      : '+strtrim(string(bg_area),2)+' [pixel^2]'
   printf,unit,'Exposure time       : '+strtrim(string(bg_exposure/double(bkgnorm)),2)+' [s]'
   printf,unit,'Count rate	    : '+strtrim(string(bg_rate*double(bkgnorm)),2)+' [count/s]'
   printf,unit,'Count rate per pixel: '+strtrim(string(bg_rate*double(bkgnorm)/bg_area),2)+' [count/s/pixel]'
   printf,unit,'----------------------------------------------'
   printf,unit,' '
   printf,unit,'   Background-subtracted properties '
   printf,unit,'----------------------------------------------'
   printf,unit,'Net count rate      : '+strtrim(string(net_rate),2)+' +/- '+strtrim(string(err_rate),2)+' [count/s]'
   printf,unit,'Net rate per pixel  : '+strtrim(string(net_rate/area),2)+' +/- '+strtrim(string(err_rate/area),2)+' [count/s/pixel]'
   printf,unit,'Surface brightness  : '+strtrim(string(sur_bri),2)+' +/- '+strtrim(string(sur_bri_err),2)+' [count/pixel^2]'
   printf,unit,'----------------------------------------------'
   printf,unit,' '
   close,unit
   free_lun,unit   
      
   print,' '
   print,' '
   print,' '
   print,'Info for the '+region_root+' region of '+evt_file
   print,'for energies between '+energy+' eV.'
   if (n_elements(expmap) ne 0) then begin
      print,'Counts and rates are corrected for exposure'
      if (n_elements(bkg_expmap) eq 0) then print,'given by '+expmap+'.' else print,'given by '+expmap+' and '+bkg_expmap+'.'
   endif
   print,'(with '+bkg_file+' as background)'
   print,' '
   print,'   Source properties '
   print,'----------------------------------------------'
   print,'Counts inside region: '+strtrim(string(counts),2)+' +/- '+strtrim(string(err_counts),2)
   print,'Area of region      : '+strtrim(string(area),2)+' [pixel^2]'
   print,'Exposure time       : '+strtrim(string(exposure),2)+' [s]'
   print,'Count rate          : '+strtrim(string(count_rate),2)+' [count/s]'
   print,'Count rate per pixel: '+strtrim(string(count_rate/area),2)+' [count/s/pixel]'
   print,'----------------------------------------------'
   print,' '
   print,'   Background properties (BKGNORM = '+bkgnorm+')'
   print,'----------------------------------------------'
   print,'Counts inside region: '+strtrim(string(bg_counts),2)+' +/- '+strtrim(string(bg_err),2)
   print,'Area of region      : '+strtrim(string(bg_area),2)+' [pixel^2]'
   print,'Exposure time       : '+strtrim(string(bg_exposure/double(bkgnorm)),2)+' [s]'
   print,'Count rate          : '+strtrim(string(bg_rate*double(bkgnorm)),2)+' [count/s]'
   print,'Count rate per pixel: '+strtrim(string(bg_rate*double(bkgnorm)/bg_area),2)+' [count/s/pixel]'
   print,'----------------------------------------------'
   print,' '
   print,'   Background-subtracted properties '
   print,'----------------------------------------------'
   print,'Net count rate      : '+strtrim(string(net_rate),2)+' +/- '+strtrim(string(err_rate),2)+' [count/s]'
   print,'Net rate per pixel  : '+strtrim(string(net_rate/area),2)+' +/- '+strtrim(string(err_rate/area),2)+' [count/s/pixel]'
   print,'Surface brightness  : '+strtrim(string(sur_bri),2)+' +/- '+strtrim(string(sur_bri_err),2)+' [count/pixel^2]'
   print,'----------------------------------------------'
   print,' '
endelse

return
end