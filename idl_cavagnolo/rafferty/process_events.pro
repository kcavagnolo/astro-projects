pro process_events,newbpix=newbpix
;-----------------------------------------------------------------------
;
; Name: PROCESS_EVENTS
;
; Purpose: Reads in evt1 FITS file and makes a new evt2 FITS file,
;          applying a new gain map, randomizing the PHA, and applying
;	   pixel randomization 
;          
;          
; Inputs:  /newbpix - make a new bad pixel file; Skip as default, as it takes 
;		      ~8-10 hours to complete
;         
;         
; Comments: Based on "Analysis Guide: ACIS data preperation - CIAO 2.3"
;           
;           
; Revision history:
;       written by D&L, 2002-11-06
;	applied CTI correction, 2003-3-18 (DR)
;	updated to Ciao 3.0, 2004-1-28 (DR)
;	updated to Ciao 3.2, 2005-2-2 (DR)
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 0) or (np gt 0)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'process_events'
   return   
endif


;
; Define root directory for CIAO
;
get_lun,unit
openr,unit,'ciao_info.txt'
ciao_root=' '
readf,unit,ciao_root
cmdroot='source '+ciao_root+'/bin/ciao.csh ; '
close,unit
free_lun,unit


;
; Find evt1 file, asol1 file, flt1 file, and bpix1 file
;
evt1file=findfile('secondary/*evt1.fits')
asol1file=findfile('primary/*asol1.fits',count=asolnum)
flt1file=findfile('secondary/*flt1.fits')
evt2file=findfile('primary/acisf*evt2.fits')
bpixfile=findfile('primary/acisf*bpix1.fits',count=num)
if (num eq 2) then new_bpix=1 else new_bpix=0


;
; Check if more than one asol1 file is present, and if so,
; create a stack
;
if (asolnum gt 1) then begin
   get_lun,unit
   outfile='asol1.list'
   openw,unit,outfile
   for i=0,asolnum-1 do printf,unit,asol1file[i]
   printf,unit,' '
   close,unit
   free_lun,unit
endif


;
; Make a new directory called 'reprocessed'
;
cmdstring='mkdir reprocessed'
spawn,cmdstring,result


;
; Read in header from evt2 file and get read and data modes
;
hd=headfits(evt2file(0),exten=1)
readmode=sxpar(hd,'READMODE','Parameter READMODE not found')
datamode=sxpar(hd,'DATAMODE','Parameter DATAMODE not found')


;
; Read in header of evt1 file and get focal plane temperature
;
hd2=headfits(evt1file(0),exten=1)
fptemp=sxpar(hd2,'FP_TEMP','Parameter FP_TEMP not found')


;
; Check modes and temperature
;
if (strtrim(readmode,2) eq 'TIMED') then begin
   if (strtrim(datamode,2) eq 'FAINT') then eventdef='stdlev1'
   if (strtrim(datamode,2) eq 'VFAINT') then eventdef='stdlev1'
   if (strtrim(datamode,2) eq 'GRADED') then eventdef='grdlev1'
endif

if (fix(fptemp) eq 153) then fptempc=-120 else fptempc=0


;
; Make a new bpix file if required files are present
;
if (new_bpix eq 1) then goto,skip_newbpix
if keyword_set(newbpix) then begin 
   msk1file=findfile('secondary/*msk1*',count=num)
   print,' '
   if (num eq 0) then begin
      print,'MSK1 file not found in secondary directory!'
      print,'Skipping creation of new bpix file.'
      new_bpix=0
      goto,skip_newbpix
   endif else begin
      print,'Found MSK1 file:',msk1file
   endelse
   bias0file=findfile('secondary/*bias0*',count=biasnum)
   if (biasnum eq 0) then begin
      print,'BIAS0 files not found in secondary directory!'
      print,'Skipping creation of new bpix file.'
      new_bpix=0 
      goto,skip_newbpix
   endif else begin
      print,'Found BIAS0 file(s):',bias0file
      if (biasnum gt 1) then begin
         get_lun,unit
         outfile='bias0.list'
         openw,unit,outfile
         for i=0,biasnum-1 do printf,unit,bias0file[i]
         printf,unit,' '
         close,unit
         free_lun,unit
      endif
   endelse
   pbk0file=findfile('secondary/*pbk0*',count=num)
   if (num eq 0) then begin
      print,'PBK0 file not found in secondary directory!'
      print,'Skipping creation of new bpix file.'
      new_bpix=0
      goto,skip_newbpix
   endif else begin
      print,'Found PBK0 file:',pbk0file
   endelse


   ;
   ; If all files were found, remove acis_detect_afterglow correction
   ;
   print,'Found all files necessary to create a new bad pixel file.'
   print,' '
   print,'Removing acis_detect_afterglow correction from evt1 file...'
   cmdstring='punlearn dmtcalc'
   spawn,cmdroot+cmdstring,result

   cmdstring='dmtcalc infile='+evt1file+' outfile=./secondary/acis_reset_evt1.fits expression="status=status,status=X15F,status=X14F,status=X13F,status=X12F"'
   spawn,cmdroot+cmdstring,result
   print,'...done.'


   ;
   ; Now create new bpix file
   ;
   ; Check that STARTBEP keyword matches
   ;
;   evt1_hd=headfits(evt1file(0),exten=1)
;   startbep_evt1=sxpar(evt1_hd,'STARTBEP','Parameter STARTBEP not found')

;   print,'Checking STARTBEP keywords...'
;   for i=0,biasnum-1 do begin
;      bias_hd=headfits(evt1file(0),exten=1)
;      startbep_bias=sxpar(bias_hd,'STARTBEP','Parameter STARTBEP not found')
;      if (startbeg_bias ne startbep_evt1) then begin
;         print,'STARTBEP keywords do not match for evt1 file and '+biasfile[i]
;         return
;      endif
;   endfor
;   print,'...OK.'


   ;
   ; If all keywords match, run acis_run_hotpix
   ;
   cmdstring='punlearn acis_run_hotpix'
   spawn,cmdroot+cmdstring,result

   print,'Making a new bad pixel file...'
   if (biasnum eq 1) then begin
      cmdstring='acis_run_hotpix infile=./secondary/acis_reset_evt1.fits outfile=./primary/new_bpix1.fits badpixfile='+bpixfile+' biasfile='+bias0file+' maskfile='+msk1file+' pbkfile='+pbk0file
      spawn,cmdroot+cmdstring,result
   endif else begin
      cmdstring='acis_run_hotpix infile=./secondary/acis_reset_evt1.fits outfile=./primary/new_bpix1.fits badpixfile='+bpixfile+' biasfile=@bias0.list maskfile='+msk1file+' pbkfile='+pbk0file
      spawn,cmdroot+cmdstring,result
   endelse
   new_bpix=1
   print,'...done.'
endif else begin
   new_bpix=0
endelse


;
; Reset 
;
skip_newbpix:
cd,'primary'

cmdstring='punlearn ardlib'
spawn,cmdroot+cmdstring,result

ardlibfile=findfile('acis_set_ardlib',count=num)
if (num eq 0) then begin
   cmdstring='cp '+ciao_root+'/contrib/bin/acis_set_ardlib .'
   spawn,cmdstring,result
endif 

if (new_bpix eq 1) then bpixfile='new_bpix1.fits' else bpixfile=findfile('acisf*bpix1.fits')

cmdstring='./acis_set_ardlib '+bpixfile
spawn,cmdroot+cmdstring,result

cd,'..'


;
; Run acis_process_events in CIAO
;
print,' '
print,'Running acis_process_events to make new evt1 file...'
print,'A warning about event islands may be ignored (see'
print,'http://cxc.harvard.edu/ciao/faq/ape_badpix.html).

cmdstring='punlearn acis_process_events'
spawn,cmdroot+cmdstring,result

if (new_bpix eq 1) then bpixfile='./primary/new_bpix1.fits' else bpixfile=findfile('primary/acisf*bpix1.fits')
cmdstring='pset acis_process_events badpixfile='+bpixfile
spawn,cmdroot+cmdstring,result

if (asolnum eq 1) then begin
   cmdstring='pset acis_process_events acaofffile='+asol1file
   spawn,cmdroot+cmdstring,result
endif else begin
   cmdstring='pset acis_process_events acaofffile="@asol1.list"'
   spawn,cmdroot+cmdstring,result
endelse

cmdstring='pset acis_process_events eventdef=")'+eventdef+'"'
spawn,cmdroot+cmdstring,result

cmdstring='pset acis_process_events apply_tgain=yes' 		
spawn,cmdroot+cmdstring,result

;if ( (chipid eq 4) or (chipid eq 5) or (chipid eq 7) or (chipid eq 8) or (chipid eq 9) ) then no_cti=1 else no_cti=0
no_cti=0
if ( (fptempc eq -120) and (no_cti eq 0) ) then begin
   cmdstring='pset acis_process_events apply_cti=yes'
   spawn,cmdroot+cmdstring,result
endif else begin
   cmdstring='pset acis_process_events apply_cti=no'
   spawn,cmdroot+cmdstring,result
endelse

if (fptempc ne -120) then begin		; do not apply CTI and time-dependent gain unless FP=-120
   cmdstring='pset acis_process_events apply_cti=no'
   spawn,cmdroot+cmdstring,result
   cmdstring='pset acis_process_events apply_tgain=no' 		
   spawn,cmdroot+cmdstring,result
endif

if (strtrim(datamode,2) eq 'VFAINT') then begin
  cmdstring='pset acis_process_events check_vf_pha=yes'
  spawn,cmdroot+cmdstring,result
endif else begin
  cmdstring='pset acis_process_events check_vf_pha=no'
  spawn,cmdroot+cmdstring,result
endelse

if (new_bpix eq 1) then evt1file='./secondary/acis_reset_evt1.fits'
if (asolnum eq 1) then begin
   cmdstring='acis_process_events infile='+evt1file+' outfile='+'./reprocessed/acis_new_evt1.fits acaofffile='+asol1file
   spawn,cmdroot+cmdstring,result
endif else begin
   cmdstring='acis_process_events infile='+evt1file+' outfile='+'./reprocessed/acis_new_evt1.fits acaofffile="@asol1.list"'
   spawn,cmdroot+cmdstring,result   
endelse
print,'...done.'


;
;  Filter for bad grades
;
print,'Filtering new evt1 file for bad grades...'
cmdstring='punlearn dmcopy'
spawn,cmdroot+cmdstring,result
;cmdstring='dmcopy "./reprocessed/acis_new_evt1.fits[EVENTS][grade=0,2,3,4,6,status=0]" ./reprocessed/acis_flt_evt1.fits'
; Don't filter by status=0, as suggested by Markevitch in his blank-sky background cookbook
; to better match blank-sky filtering.  Instead, filter out VF mode background, problems with CTI algorithm convergence.
; afterglow, streaks, and all others except neighboring pixels (status bit 5) 
cmdstring='dmcopy "./reprocessed/acis_new_evt1.fits[EVENTS][grade=0,2,3,4,6,status=xxxxxxxx0xx000000000000000x00000]" ./reprocessed/acis_flt_evt1.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Filter on Good Time Interval (GTI)
;
print,'Filtering evt1 file on GTI...'
cmdstring='punlearn dmcopy'
spawn,cmdroot+cmdstring,result
cmdstring='dmcopy "./reprocessed/acis_flt_evt1.fits[EVENTS][@'+flt1file+'][cols -phas]" ./reprocessed/acis_evt2.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Run destreak
;
print,'Running destreak on ccd_id=8...'
cmdstring='punlearn destreak'
spawn,cmdroot+cmdstring,result
cmdstring='destreak infile=./reprocessed/acis_evt2.fits outfile=./reprocessed/acis_dstrk_evt2.fits ccd_id=8'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Read in header from new evt1 file
;
hd=headfits('./reprocessed/acis_flt_evt1.fits',exten=1)


;
; Get GAINFILE, CALDBVER, and OBS_ID out of evt1 header
;
gainfile=sxpar(hd,'GAINFILE','Parameter GAINFILE not found')
;caldbver=sxpar(hd,'CALDBVER','Parameter CALDBVER not found') ;NOTE: the CALDBVER header
							      ;is not updated by acis_process_events
							      ;so finding the caldbver this way
							      ;does not work! Use instead:
readcol,ciao_root+'/CALDB/docs/chandra/caldb_version/caldb_version.txt',format='a',version
scaldb=size(version)
nlines=scaldb[1]
caldbver=version[nlines-1]
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')


;
; Update obs_info file
;
get_lun,unit
outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'
openw,unit,outfile,/append
printf,unit,'Output of PROCESS_EVENTS'
printf,unit,!stime
printf,unit,' '
printf,unit,'Reprocessed evt2 file       : ./reprocessed/acis_dstrk_evt2.fits'
printf,unit,'CALDB version of new file   : ',caldbver
printf,unit,' '
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit


;
; Print status info to screen
;
print,' '
print,'PROCESS_EVENTS complete. New evt2 file created.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end