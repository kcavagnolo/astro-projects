pro wextract,region,evt2_file,bg_file,root, $
             ciao_path=ciao_path,mask_file=mask_file, $
             STATS=stats,LOG=log,CLOBBER=clobber
;-----------------------------------------------------------------------
;
; Name: WEXTRACT
;
; Purpose: Extracts the spectra in the region specified for spectral
;	   fitting and makes appropriate weighted rmf and arf files
;                  
; Inputs:  region - region file in CIAO format
;	   evt2_file - events file
;	   bg_file - blank-sky background file
;	   root - root string for the created files
; 	   
; Optional inputs:  ciao_path - path to the ciao setup script
;		    mask_file - MSK1 file (see secondary directory)
;
; Keywords:  /STATS - if set, the statistics of the extracted regions
; 		      will be found (counts, etc.)
;	     /LOG - if set, the summary with be written to a log file
;	     /CLOBBER - if set, wextract will over-write any existing 
;			files with the same root name
;	                     
; Comments: 
;                     
; Revision history:
;       written by DAR, 2003-03-20
;	updated to Ciao 3.1, 2004-08-16 (DR)
;	updated to Ciao 3.2 and mkacisrmf, 2005-2-3 (DR)
;	changed errors to "gaussian", 2005-6-2 (DR)
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 4) or (np gt 5)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'wextract, region, evt2_file, bg_file, root [, ciao_path=ciao_path, mask_file=mask_file, /STATS, /LOG, /CLOBBER]'
   return   
endif


;
; Define default root directory for CIAO
;
if (n_elements(ciao_path) eq 0) then begin
   cmdroot='source /export/home/Rafferty/Applications/ciao_3.3/bin/ciao.csh ; '
endif else begin
   cmdroot='source '+ciao_path+'/bin/ciao.csh ; '
endelse


;
; Define the file names (follow acisspec conventions)
;
pi_file=root+'_sou.pi'
bg_pi_file=root+'_bgd.pi'
warf_file=root+'_sou.warf'
fef_weights=root+'.wgt'
wrmf_file=root+'_sou.wrmf'


;
; Check if /CLOBBER is set
;
if keyword_set(clobber) then endtext=' clobber=yes' else endtext=' '


;
; Create PHA and WMAP files with dmextract
;
print,'Running dmextract for '+root+'...'
cmdstring='punlearn dmextract'
spawn,cmdroot+cmdstring,result

cmdstring='pset dmextract opt=pha1'
spawn,cmdroot+cmdstring,result

cmdstring='pset dmextract error=gaussian'	; use gaussian errors instead of Poisson errors (more appropriate for few counts)
spawn,cmdroot+cmdstring,result

cmdstring='pset dmextract bkgerror=gaussian'	; use gaussian errors instead of Poisson errors (more appropriate for few counts)
spawn,cmdroot+cmdstring,result

cmdstring='pset dmextract wmap="[energy=500:2000][bin det=8]"'
spawn,cmdroot+cmdstring,result

evt2_filter='"'+evt2_file+'[sky=region('+region+')][bin pi=1:1024:1]"'

cmdstring='dmextract infile='+evt2_filter+' outfile='+pi_file
spawn,cmdroot+cmdstring+endtext,result

bg_filter='"'+bg_file+'[sky=region('+region+')][bin pi=1:1024:1]"'

cmdstring='dmextract infile='+bg_filter+' outfile='+bg_pi_file
spawn,cmdroot+cmdstring+endtext,result
print,'...done.'


;
; Create a weighted RMF
;
print,'Now creating a weighted RMF for '+root+'...'
cmdstring='punlearn mkacisrmf'
spawn,cmdroot+cmdstring,result

cmdstring='mkacisrmf infile=CALDB outfile='+wrmf_file+' wmap="'+pi_file+'[WMAP]"  energy=0.4:8.0:0.01 channel=1:1024:1 chantype=PI ccd_id=7 chipx=512 chipy=512 gain=CALDB'
spawn,cmdroot+cmdstring+endtext,result	; NOTE: mkacisrmf ignores ccd_id, chipx, and chipy values when wmap is supplied
print,'...done.'


;
; Create a weighted ARF
;
print,'Now creating a weighted ARF for '+root+'...'
cmdstring='punlearn mkwarf'
spawn,cmdroot+cmdstring,result

pi_filter='"'+pi_file+'[WMAP]"'

if (n_elements(mask_file) eq 0) then begin
   cmdstring='mkwarf infile='+pi_filter+' outfile='+warf_file+' weightfile='+fef_weights+' spectrumfile=NONE egridspec="grid('+wrmf_file+'[cols ENERG_LO,ENERG_HI])"'
   spawn,cmdroot+cmdstring+endtext,result
endif else begin
;   cmdstring='pset mkwarf mskfile='+mask_file
;   spawn,cmdroot+cmdstring,result
   cmdstring='mkwarf infile='+pi_filter+' outfile='+warf_file+' weightfile='+fef_weights+' spectrumfile=NONE egridspec="grid('+wrmf_file+'[cols ENERG_LO,ENERG_HI])"'
   spawn,cmdroot+cmdstring+endtext,result
endelse
print,'...done.'


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
   printf,unit,'  Weighted ARF                  : '+warf_file
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
endif


;
; Lastly, print to the screen
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
print,'  Weighted ARF                  : '+warf_file
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


;
; Return to IDL
;
return
end


