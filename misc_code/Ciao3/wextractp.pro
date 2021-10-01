pro wextractp,region,evt2_file,bg_file,root,ciao_path,QE=qe,STATS=stats,LOG=log,wide=wide
;-----------------------------------------------------------------------
;
; Name: WEXTRACTP
;
; Purpose: Extracts the spectra in the region specified for spectral
;	   fitting and makes appropriate weighted rmf and arf files
;                  
; Inputs:  region - region file in CIAO format
;	   evt2_file - events file
;	   bg_file - blank-sky background file
;	   root - root string for the created files
;
; Optional inputs:  ciao_path - path the to the ciao setup script
;
; Keywords:  /QE - if set, the QE correction will be applied
; 	     /STATS - if set, the statistics of the extracted regions
; 		      will be found (counts, etc.)
;	     /LOG - if set, the summary with be written to a log file
;                
; Comments: 
;                     
; Revision history:
;       written by DR, 2003-03-20
; 	modified for the pipeline (DR) -- no longer makes RMFs, 2003-04-03
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 4) or (np gt 8)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'wextractp, region, evt2_file, bg_file, root [, ciao_path, /QE, /STATS, /LOG]'
   return   
endif


;
; Define default root directory for CIAO
;
if (n_elements(ciao_path) eq 0) then begin
   cmdroot='source /iraf/ciao_3.1/bin/ciao.csh ; '
endif else begin
   cmdroot='source '+ciao_path+' ; '
endelse


;
; Define the file names
;
pi_file=root+'.pi'
bg_pi_file=root+'_bg.pi'
warf_file=root+'.warf'
fef_weights=root+'.wgt'
wrmf_file=root+'.wrmf'


;
; Create PHA and WMAP files with dmextract
;
print,'Running dmextract for '+root+'...'
cmdstring='punlearn dmextract'
spawn,cmdroot+cmdstring,result

cmdstring='pset dmextract opt=pha1'
spawn,cmdroot+cmdstring,result

cmdstring='pset dmextract wmap="[energy=500:2000][bin det=8]"'
spawn,cmdroot+cmdstring,result

evt2_filter='"'+evt2_file+'[sky=region('+region+')][bin pi=1:1024:1]"'

cmdstring='dmextract infile='+evt2_filter+' outfile='+pi_file
spawn,cmdroot+cmdstring,result

bg_filter='"'+bg_file+'[sky=region('+region+')][bin pi=1:1024:1]"'

cmdstring='dmextract infile='+bg_filter+' outfile='+bg_pi_file
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Create a weighted ARF
;
print,'Now creating a weighted ARF for '+root+'...'
cmdstring='punlearn mkwarf'
spawn,cmdroot+cmdstring,result

pi_filter='"'+pi_file+'[WMAP]"'

if keyword_set(wide) then begin
   cmdstring='mkwarf infile='+pi_filter+' outfile='+warf_file+' weightfile='+fef_weights+' spectrumfile=NONE egridspec=0.3:11.0:0.01'
   spawn,cmdroot+cmdstring,result
endif else begin
   cmdstring='mkwarf infile='+pi_filter+' outfile='+warf_file+' weightfile='+fef_weights+' spectrumfile=NONE egridspec=0.5:8.0:0.01'
   spawn,cmdroot+cmdstring,result
endelse
print,'...done.'


;
; Create a weighted RMF
;
;print,'Now creating a weighted RMF for '+root+'...'
;cmdstring='punlearn mkrmf'
;spawn,cmdroot+cmdstring,result

;cmdstring='pset mkrmf weights='+fef_weights
;spawn,cmdroot+cmdstring,result

;cmdstring='mkrmf infile=CALDB outfile='+wrmf_file+' axis1="energy=0:1" axis2="pi=1:1024:1" logfile=NONE'
;spawn,cmdroot+cmdstring,result
;print,'...done.'


;
; Update the header of the extracted pi file
;
print,'Updating the header of '+pi_file+'...'
cmdstring='punlearn dmhedit'
spawn,cmdroot+cmdstring,result

cmdstring='dmhedit infile='+pi_file+' filelist= operation=add key=RESPFILE value='+wrmf_file
spawn,cmdroot+cmdstring,result

cmdstring='punlearn dmhedit'
spawn,cmdroot+cmdstring,result

cmdstring='dmhedit infile='+pi_file+' filelist= operation=add key=ANCRFILE value='+warf_file
spawn,cmdroot+cmdstring,result

cmdstring='punlearn dmhedit'
spawn,cmdroot+cmdstring,result

cmdstring='dmhedit infile='+pi_file+' filelist= operation=add key=BACKFILE value='+bg_pi_file
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Add the exposure keyword to the ARF file (required for ISIS)
;
print,'Adding EXPOSURE keyword to ARF...'
hd=headfits(pi_file,exten=1)
exposure=sxpar(hd,'EXPOSURE')
expstring=strtrim(string(exposure),2)

cmdstring='punlearn dmhedit'
spawn,cmdroot+cmdstring,result

cmdstring='dmhedit infile='+warf_file+' filelist= operation=add key=EXPOSURE value='+expstring
spawn,cmdroot+cmdstring,result
print,'...done.


;
; Correct the ARF for QE if specified
;
if keyword_set(qe) then begin
   print,'Applying QE correction to ARF of '+root+'...'
   
   ;
   ; Find the GTI file
   ;
   gtifile=findfile('./*gti*',count=num)
   if (num eq 0) then begin
      print,'ERROR:'
      print,'    GTI file not found in current directory!'
      return
   endif else begin
      print,'    Found GTI file:',gtifile
   endelse
   
   
   ;
   ; Find the qe_cont file
   ;
   qefile=findfile('./qe_cor*',count=num)
   if (num eq 0) then begin
      print,'ERROR:'
      print,'    QE_CONT file not found in current directory!'
      return
   endif else begin
      print,'    Found QE_CONT file:',qefile
   endelse
   
   
   ;
   ; Apply QE correction
   ;
   cmdstring='corrarf -gtifile '+gtifile+' -arf '+warf_file+' -qe_cont qe_cor.H20.C10.O2.N1'
   spawn,cmdstring,qeresult
   print,'...done.'
endif


;
; Get the statistics if specified
;
if keyword_set(stats) then begin
   print,'Finding the statistics for '+root+'...'
   cmdstring='punlearn dmextract'
   spawn,cmdroot+cmdstring,result

   infile_flt='"'+evt2_file+'[bin sky=region('+region+')]"'

   cmdstring='dmextract infile='+infile_flt+' outfile=counts.fits clobber=yes'
   spawn,cmdroot+cmdstring,result

   bginfile_flt='"'+bg_file+'[bin sky=region('+region+')]"'

   cmdstring='dmextract infile='+bginfile_flt+' outfile=bgcounts.fits clobber=yes'
   spawn,cmdroot+cmdstring,result

   cmdstring='dmlist "counts.fits[cols counts,err_counts,area]" data'
   spawn,cmdroot+cmdstring,cnts_result

   cmdstring='dmlist "bgcounts.fits[cols counts,err_counts,area]" data'
   spawn,cmdroot+cmdstring,bgcnts_result
   print,'...done'
endif


;
; Clean up the files
;


;
; Print status info to log file if specified
;
if keyword_set(log) then begin
   outfile=root+'_info.txt'
   get_lun,unit
   openw,unit,outfile
   printf,unit,' '
   printf,unit,'         Summary of WEXTRACT'
   printf,unit,'         -------------------'
   printf,unit,' '
   printf,unit,'  Extracted region              : '+pi_file
   printf,unit,'  Background region             : '+bg_pi_file
   printf,unit,'  Weighted RMF                  : '+wrmf_file
   if keyword_set(qe) then begin
      printf,unit,'  Weighted ARF (QE corrected)   : '+warf_file
      printf,unit,' '
      printf,unit,'  Output of corrarf:'
      printf,unit,'  ------------------'
      printf,unit,qeresult
   endif else begin
      printf,unit,'  Weighted ARF (QE uncorrected) : '+warf_file
   endelse
   printf,unit,' '
   if keyword_set(stats) then begin
      printf,unit,'  Statistics of extracted region:'
      printf,unit,'  -------------------------------'
      printf,unit,cnts_result(9:12)
      printf,unit,' '
      printf,unit,'  Statistics of background region:'
      printf,unit,'  --------------------------------'
      printf,unit,bgcnts_result(9:12)
      printf,unit,' '
   endif
   close,unit
   free_lun,unit
endif else begin


   ;
   ; Otherwise print to the screen
   ;
   print,' '
   print,' '
   print,'         -------------------'
   print,'          WEXTRACT complete.'
   print,'         -------------------'
   print,' '
   print,'  Extracted region              : '+pi_file
   print,'  Background region             : '+bg_pi_file
   print,'  Weighted RMF                  : '+wrmf_file
   if keyword_set(qe) then begin
      print,'  Weighted ARF (QE corrected)   : '+warf_file
      printf,unit,' '
      printf,unit,'  Output of corrarf:'
      printf,unit,'  ------------------'
      printf,unit,qeresult
   endif else begin
      print,'  Weighted ARF (QE uncorrected) : '+warf_file
   endelse
   print,' '
   if keyword_set(stats) then begin
      print,'  Statistics of extracted region:'
      print,'  -------------------------------'
      print,cnts_result(9:12)
      print,' '
      print,'  Statistics of background region:'
      print,'  --------------------------------'
      print,bgcnts_result(9:12)
      print,' '
   endif
endelse


;
; Return to IDL
;
return
end


