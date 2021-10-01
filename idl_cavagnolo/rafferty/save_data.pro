pro save_data,save_dir=save_dir,spectra_dir=spectra_dir
;-----------------------------------------------------------------------
;
; Name: SAVE_DATA
;
; Purpose: Saves the important data for the object:
;		- All plots and logs
;		- Results of model fits
;		- Region files
;		- Cleaned image
;		- EVT2 file, BG file, and GTI file
;          
;          
; Inputs: save_dir - name of the save directory, default is 'save' 
;	  spectra_dir - name of spectra directory, default is 'spectra'
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by D&L, 2002-11-01
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 2) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'save_data [, save_dir=save_dir, spectra_dir=spectra_dir]'
   return   
endif


;
; Define defaults
;
if (n_elements(save_dir) eq 0) then save_dir='save'
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'


;
; Create the save_dir
;
cmdstring='mkdir '+save_dir
spawn,cmdstring
cmdstring='mkdir '+save_dir+'/images'
spawn,cmdstring
cmdstring='mkdir '+save_dir+'/regions'
spawn,cmdstring
cmdstring='mkdir '+save_dir+'/plots'
spawn,cmdstring
cmdstring='mkdir '+save_dir+'/logs'
spawn,cmdstring
cmdstring='mkdir '+save_dir+'/fits'
spawn,cmdstring


;
; Move the files to save_dir
;
cmdstring='mv *.ps '+save_dir+'/plots/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/*.ps '+save_dir+'/plots/.'
spawn,cmdstring
cmdstring='mv obs_info_*.txt '+save_dir+'/.'
spawn,cmdstring
cmdstring='mv images/ccd_img_clean.fits '+save_dir+'/images/.'
spawn,cmdstring
cmdstring='mv regions/*.reg '+save_dir+'/regions/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/*.reg '+save_dir+'/regions/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/*.txt '+save_dir+'/logs/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/*.log '+save_dir+'/logs/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/*.dat '+save_dir+'/fits/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/source.spectra '+save_dir+'/fits/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/bg.fits '+save_dir+'/fits/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/evt2_ccd_clean_*.fits '+save_dir+'/fits/.'
spawn,cmdstring
cmdstring='mv '+spectra_dir+'/evt2_ccd_bg.gti '+save_dir+'/fits/.'
spawn,cmdstring


;
; Remove remaining files
;
cmdroot='unalias rm ; '
cmdstring='rm *.*'
spawn,cmdroot+cmdstring
cmdstring='rm -r background'
spawn,cmdroot+cmdstring
cmdstring='rm -r images'
spawn,cmdroot+cmdstring
cmdstring='rm -r primary'
spawn,cmdroot+cmdstring
cmdstring='rm -r regions'
spawn,cmdroot+cmdstring
cmdstring='rm -r reprocessed'
spawn,cmdroot+cmdstring
cmdstring='rm -r secondary'
spawn,cmdroot+cmdstring
cmdstring='rm -r '+spectra_dir
spawn,cmdroot+cmdstring


;
; Print status info to screen
;
print,'SAVE_DATA complete.'


;
; Return to IDL
;
return
end
