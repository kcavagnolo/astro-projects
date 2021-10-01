pro acis_bg,chipid,turvy=turvy,ni=ni
;-----------------------------------------------------------------------
;
; Name: ACIS_BG
;
; Purpose: Finds correct background file and cleans evt2 file as required
;          
;          
;          
; Inputs:  chipid - the id of the chip of interest
;         
;         
; Comments: Based on "Acis Background Subtraction (Blank-Sky Files)" thread
;           for CIAO 2.3
;           
;           
; Revision history:
;       written by D&L, 2002-11-07 
;	added background normilization code (DR), 2003-3-30
;	added code to get the total counts on S3 chip (DR), 2003-4-10
;	updated to use any chip (DR), 2003-06-26
;       updated for Ciao 3 (DR), 2004-2-27
;	updated for noninteractive (ni) execution (DR), 2006-4-20
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 1)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'acis_bg, chipid'
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
; Make new directories called 'backgound', 'regions', and 'images'
;
if not keyword_set(ni) then begin
   cmdstring='mkdir background'
   spawn,cmdstring,result
   cmdstring='mkdir images'
   spawn,cmdstring,result
   cmdstring='mkdir regions'
   spawn,cmdstring,result
endif


;
; Run dmcopy in CIAO to restrict events to the specified ccd and 0.1<E<12 keV
;
print,'Running dmcopy to restrict events to specified ccd and 0.1<E<12 keV...'
cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=100:12000,ccd_id='+strtrim(string(chipid),2)+']" ./reprocessed/evt2_ccd.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/evt2_ccd.fits[bin sky=8]" ./reprocessed/img8_ccd.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Look up correct backgound file
;
if not keyword_set(ni) then begin
   print,'Finding correct background file...'
   cmdstring='punlearn acis_bkgrnd_lookup'
   spawn,cmdroot+cmdstring,result

   cmdstring='acis_bkgrnd_lookup ./reprocessed/evt2_ccd.fits'
   spawn,cmdroot+cmdstring,result

   cmdstring='pget acis_bkgrnd_lookup outfile'
   spawn,cmdroot+cmdstring,result

   res_size=size(result,/n_elements)
   if (res_size eq 5) then bgfile=strtrim(result(4),2)
   if (res_size eq 6) then bgfile=strtrim(result(5),2)
   if ( (res_size ne 5) and (res_size ne 6) ) then begin
      print,' '
      print,' Below is the output of acis_bkgrnd_lookup.'
      print,' Enter the number of the line that gives the path'
      print,' to the background file.'
      print,' '
      for i=0,res_size-1 do begin
         print,strtrim(string(i+1),1)+') '+result(i)
      endfor
      print,' '
      read,answer,prompt='Enter line number (1-'+strtrim(string(res_size),1)+'): '
      bgfile=strtrim(result(answer-1),2)
   endif

   print,' '
   print,'...done.  Using the following file:'
   print,bgfile

   print,'Running dmcopy to restrict background file to specified ccd...'
   bgfilter=bgfile+'[ccd_id='+strtrim(string(chipid),2)+']'
   cmdstring='dmcopy "'+bgfilter+'" ./background/bgevt2_ccd.fits'
   spawn,cmdroot+cmdstring,result
   print,'...done.'
endif


;
; Remove bright sources
;
; First, check if region files are already present
;
cluster_regfile=findfile('regions/cluster.reg',count=num1)
ccd_regfile=findfile('regions/ccd.reg',count=num2)
if ( (num1 eq 1) and (num2 eq 1) ) then goto,skip_regions



;
; Next, start ds9 for cluster region selection (save as ds9.reg in CIAO format)
;
print,'Opening ds9...'
print,' '
print,'Please create a region centered on the cluster that'
print,'encompases the bright emmision and save it in Ciao'
print,'format as regions/cluster.reg.'
print,' '
print,'Then, delete this region and create a new region'
print,'that encompases the entire ccd of interest (the box region'
print,'works well -- hold down shift and grab a corner to'
print,'rotate) and save it in Ciao format '
print,'as regions/ccd.reg.'
print,' '
print,'Then, exit ds9.'
cmdstring=ciao_root+'/ots/saord/ds9 ./reprocessed/img8_ccd.fits'	; use ds9 from CIAO installation
;cmdstring='/iraf/extern/ds9/ds9 ./reprocessed/img8_ccd.fits'
spawn,cmdstring,result


;
; Make sure the region files were saved correctly
;
ntries=0
chk_reg:
cluster_regfile=findfile('regions/cluster.reg',count=num)
if (num eq 0) then begin
   print,' '
   print,'  --ERROR--'
   print,'Cluster region file not found in regions directory!'
   print,'Please save the region in CIAO format as'
   print,'regions/cluster.reg.  Restarting ds9...'
   cmdstring=ciao_root+'/ots/saord/ds9 ./reprocessed/img8_ccd.fits'
;   cmdstring='/iraf/extern/ds9/ds9 ./reprocessed/img8_ccd.fits'
   spawn,cmdstring,result
   ntries=ntries+1
   if (ntries eq 2) then return
   goto,chk_reg
endif 
ntries=0
ccd_regfile=findfile('regions/ccd.reg',count=num)
if (num eq 0) then begin
   print,' '
   print,'  --ERROR--'
   print,'CCD region file not found in regions directory!'
   print,'Please save the region in CIAO format as'
   print,'regions/ccd.reg.  Restarting ds9...'
   cmdstring=ciao_root+'/ots/saord/ds9 ./reprocessed/img8_ccd.fits'
;   cmdstring='/iraf/extern/ds9/ds9 ./reprocessed/img8_ccd.fits'
   spawn,cmdstring,result
   ntries=ntries+1
   if (ntries eq 2) then return
   goto,chk_reg
endif 

skip_regions:
;
; Second, run wavdetect on evt2_ccd.fits
;
; Reduce image size
;
if not keyword_set(ni) then begin
   print,'Running wavdetect to find point sources...'
   cmdstring='dmcopy "./reprocessed/evt2_ccd.fits[sky=region(./regions/ccd.reg)][bin x=::1,y=::1]" ./background/ccd_img.fits'
   spawn,cmdroot+cmdstring,result


   ;
   ; Run wavdetect
   ;
   cmdstring='punlearn wavdetect'
   spawn,cmdroot+cmdstring,result

   cmdstring='pset wavdetect regfile=./regions/ccd_src.reg'
   spawn,cmdroot+cmdstring,result

   cmdstring='wavdetect infile=./background/ccd_img.fits outfile=./background/ccd_src.fits scellfile=./background/ccd_scell.fits imagefile=./background/ccd_imgfile.fits defnbkgfile=./background/ccd_nbgd.fits'
   spawn,cmdroot+cmdstring,result
   print,'...done.'
endif


;
; Now remove sources
;
print,'Removing bright sources...'
cmdstring='dmcopy "./reprocessed/evt2_ccd.fits[exclude sky=region(./background/ccd_src.fits)]" ./background/evt2_ccd_no_ptsrc.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./background/evt2_ccd_no_ptsrc.fits[exclude sky=region(./regions/cluster.reg)]" ./background/evt2_ccd_no_src.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Create a lightcurve, filtered for 2.5<E<7.0 keV
;
if not keyword_set(ni) then begin
   print,'Creating lightcurve...'
   if (chipid eq 7) then begin	; filter front-illuminated and back-illuminated chips differently
      cmdstring='dmcopy "./background/evt2_ccd_no_src.fits[energy=2500:7000]" ./background/evt2_ccd_bg.fits'
      spawn,cmdroot+cmdstring,result

      cmdstring='punlearn dmextract'
      spawn,cmdroot+cmdstring,result

      cmdstring='dmextract infile="./background/evt2_ccd_bg.fits[bin time=::1037.12]" outfile=./background/evt2_ccd_bg.lc opt=ltc1'
      spawn,cmdroot+cmdstring,result
   endif else begin
      cmdstring='dmcopy "./background/evt2_ccd_no_src.fits[energy=300:12000]" ./background/evt2_ccd_bg.fits'
      spawn,cmdroot+cmdstring,result

      cmdstring='punlearn dmextract'
      spawn,cmdroot+cmdstring,result

      cmdstring='dmextract infile="./background/evt2_ccd_bg.fits[bin time=::259.28]" outfile=./background/evt2_ccd_bg.lc opt=ltc1'
      spawn,cmdroot+cmdstring,result
   endelse
   print,'...done.'


   ;
   ; Filter the lightcurve for flares
   ;
   print,' '
   print,'Now running CHIPS to filter the lightcurve.  If there are no'
   print,'problems, type "exit" to continue.  '
   print,' '
   print,'If there is a strong flare'
   print,'present, estimate the mean count rate and set it using'
   print,'the command "lc->mean = 0.7" (for mean=0.7 cnts/s). Then, run'
   print,'lc_clean again as follows: '
   print,'  chips> lc_clean("./background/evt2_ccd_bg.lc")'
   print,'  chips> print postfile ./background/flt_lghtcrv.eps'
   print,'  chips> exit'
   print,' '

   get_lun,unit
   outfile='lc_clean_wrap.sl'
   openw,unit,outfile
   printf,unit,'% S-lang wrapper for lc_clean.sl'
   printf,unit,' '
   printf,unit,'()=evalfile( "'+ciao_root+'/contrib/share/slsh/local-packages/lc_clean.sl" );'
   printf,unit,'lc->outfile="./background/evt2_ccd_bg.gti";'
   printf,unit,'lc->verbose=1;'
   printf,unit,'lc_clean("./background/evt2_ccd_bg.lc");'
   printf,unit,'() = chips_eval( "print postfile ./background/flt_lghtcrv.eps" );'
   ;printf,unit,'() = chips_eval( "exit" );'
   printf,unit,' '
   close,unit
   free_lun,unit

   cmdstring='chips --slscript lc_clean_wrap.sl'
   spawn,cmdroot+cmdstring,result
   print,'...done.'
endif


;
; Filter on GTI
;
print,'Filtering on GTI'
cmdstring='dmcopy "./reprocessed/evt2_ccd.fits[@./background/evt2_ccd_bg.gti]" ./reprocessed/evt2_ccd_clean.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Create color maps with filtering based on Mike's "extract_colormaps" tool
;
print,'Creating color maps...'
cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=290:540,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_0.29_0.54.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=540:1000,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_0.54_1.0.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=1000:1570,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_1.0_1.57.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=1570:2000,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_1.57_2.0.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=2000:3500,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_2.0_3.5.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=3500:5000,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_3.5_5.0.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=5000:7000,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_5.0_7.0.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./reprocessed/acis_dstrk_evt2.fits[energy=7000:10000,ccd_id='+strtrim(string(chipid),2)+'][bin x=::4,y=::4]" ./images/ccd_img_7.0_10.0.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Remove point sources from cleaned evt2 file
;
; Start ds9 to check point sources
;
if keyword_set(ni) then goto,skip_pt_src
print,'Now starting ds9...'
print,'If you want to remove all regions found by WAVEDETECT,'
print,'simply exit ds9 without making any changes.  If, however,'
print,'you want to make changes, please delete regions '
print,'that you do not want to be removed during spectral fitting.'
print,'Any remaining regions will be removed before fitting.  '
print,'Save the new region file as:'
print,'"regions/ccd_src_alt.reg" in Ciao format. It may be'
print,'helpful to look at the colormaps or the color image'
print,'in the "images" directory in identifying central point sources.'
print,' '
print,'Lastly, exit ds9.'
cmdstring=ciao_root+'/ots/saord/ds9 ./background/ccd_img.fits -regionfile ./regions/ccd_src.reg'
;cmdstring='/iraf/extern/ds9/ds9 ./background/ccd_img.fits -regionfile ./regions/ccd_src.reg'
spawn,cmdstring,result
print,'...done.'

skip_pt_src:
;
; Use ccd_src_alt.reg if present, otherwise use ccd_src.reg
;
src_regfile=findfile('regions/ccd_src_alt.reg',count=alt_reg)
if (alt_reg eq 0) then begin
   print,'New region file (ccd_src_alt.reg) NOT found:'
   print,'removing all point sources in ccd_src.reg'
   print,'from cleaned evt2 file...'
   cmdstring='dmcopy "./reprocessed/evt2_ccd_clean.fits[exclude sky=region(./regions/ccd_src.reg)]" ./reprocessed/evt2_ccd_clean_no_ptsrc.fits'
   spawn,cmdroot+cmdstring,result
   cmdstring='dmcopy "./reprocessed/evt2_ccd_clean_no_ptsrc.fits[exclude sky=region(./regions/cluster.reg)]" ./reprocessed/evt2_ccd_clean_no_src.fits'
   spawn,cmdroot+cmdstring,result
   print,'...done.' 
endif else begin
   print,'New region file (ccd_src_alt.reg) found:'
   print,'removing point sources remaining in ccd_src_alt.reg '
   print,'from cleaned evt2 file...'
   cmdstring='dmcopy "./reprocessed/evt2_ccd_clean.fits[exclude sky=region(./regions/ccd_src_alt.reg)]" ./reprocessed/evt2_ccd_clean_no_ptsrc.fits'
   spawn,cmdroot+cmdstring,result
   cmdstring='dmcopy "./reprocessed/evt2_ccd_clean_no_ptsrc.fits[exclude sky=region(./regions/cluster.reg)]" ./reprocessed/evt2_ccd_clean_no_src.fits'
   spawn,cmdroot+cmdstring,result
   print,'...done.'
endelse


;
; Make a new image of the chip
;
print,'Making an image of the cleaned file...'
cmdstring='dmcopy "./reprocessed/evt2_ccd_clean_no_ptsrc.fits[sky=region(./regions/ccd.reg)][bin x=::1,y=::1]" ./images/ccd_img_clean.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Filter the background file for VF status bit (status bit 23)
; We can just filter by status=0, since there are no other nonzero bits
; (If the observation is not in VF mode, this bit will be 0)
;
cmdstring='dmcopy "./background/bgevt2_ccd.fits[EVENTS][status=0]" ./background/bgevt2_ccd_filt.fits'
spawn,cmdroot+cmdstring,result
filt_bgfile=findfile('./background/bgevt2_ccd_filt.fits',count=fbgnum)
if (fbgnum eq 0) then spawn,'cp ./background/bgevt2_ccd.fits ./background/bgevt2_ccd_filt.fits'


;
; Find asol1 file
;
asol1file=findfile('primary/*asol1.fits',count=asolnum)


;
; Run reproject_events to tailor the background file to the dataset
;
print,'Running reproject_events...'
cmdstring='punlearn reproject_events'
spawn,cmdroot+cmdstring,result

if (asolnum eq 1) then begin
   cmdstring='pset reproject_events aspect='+asol1file
   spawn,cmdroot+cmdstring,result
endif else begin
   cmdstring='pset reproject_events aspect="@asol1.list"'
   spawn,cmdroot+cmdstring,result
endelse

cmdstring='pset reproject_events random=0'
spawn,cmdroot+cmdstring,result

cmdstring='reproject_events infile="./background/bgevt2_ccd_filt.fits[cols -time]" outfile=./background/bgevt2_ccd_reproj.fits match=./reprocessed/evt2_ccd_clean.fits'
spawn,cmdroot+cmdstring,result

cmdstring='punlearn dmtcalc'
spawn,cmdroot+cmdstring,result

cmdstring='dmtcalc infile=./background/bgevt2_ccd_reproj.fits outfile=./background/bg.fits expr="TIME=1.0"'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Create an evt2 file for the energy band 10-12 keV
; for use when scaling the background.  Use the evt2
; file with all sources (including cluster) removed.
;
print,'Running dmcopy to restrict source events to 10<E<12 keV...'
cmdstring='dmcopy "./reprocessed/evt2_ccd_clean_no_src.fits[energy=10000:12000]" ./background/evt2_10-12_kev.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Create a background file for the energy band 10-12 keV
; First make a background file excluding the same regions
; as in the source file.
;
print,'Running dmcopy to restrict background events to 10<E<12 keV...'
if (alt_reg eq 1) then begin
   cmdstring='dmcopy "./background/bg.fits[exclude sky=region(./regions/ccd_src_alt.reg)]" ./background/bg_no_ptsrc.fits'
   spawn,cmdroot+cmdstring,result
endif else begin
   cmdstring='dmcopy "./background/bg.fits[exclude sky=region(./regions/ccd_src.reg)]" ./background/bg_no_ptsrc.fits'
   spawn,cmdroot+cmdstring,result
endelse

cmdstring='dmcopy "./background/bg_no_ptsrc.fits[exclude sky=region(./regions/cluster.reg)]" ./background/bg_no_src.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmcopy "./background/bg_no_src.fits[energy=10000:12000]" ./background/bg_10-12_kev.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Find the count rate in the 10-12 keV band in the source file
; (with all sources removed) and background file (with the same
; regions removed)
;
print,'Finding count rate in the source and background'
print,'in the 10-12 keV band...'
cmdstring='punlearn dmextract'
spawn,cmdroot+cmdstring,result

cmdstring='dmextract infile="./background/evt2_10-12_kev.fits[bin sky=@regions/ccd.reg]" bkg=NONE outfile=./background/cnts_10-12.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmlist "./background/cnts_10-12.fits[cols net_rate]" data,clean outfile=./background/source_cnts.dat'
spawn,cmdroot+cmdstring,result

get_lun,unit
openr,unit,'./background/source_cnts.dat'
source_cntrate=' '
junk=' '
readf,unit,junk
readf,unit,source_cntrate
close,unit
free_lun,unit

cmdstring='punlearn dmextract'
spawn,cmdroot+cmdstring,result

cmdstring='dmextract infile="./background/bg_10-12_kev.fits[bin sky=@regions/ccd.reg]" bkg=NONE outfile=./background/bg_cnts_10-12.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmlist "./background/bg_cnts_10-12.fits[cols net_rate]" data,clean outfile=./background/bg_cnts.dat'
spawn,cmdroot+cmdstring,result

get_lun,unit
openr,unit,'./background/bg_cnts.dat'
bg_cntrate=' '
junk=' '
readf,unit,junk
readf,unit,bg_cntrate
close,unit
free_lun,unit

print,'...done.'
print,' '
print,'Background count rate (10-12 keV): ',bg_cntrate
print,'Source count rate (10-12 keV): ',source_cntrate


;
; Find the ratio of count rates
;
norm_ratio=double(bg_cntrate)/double(source_cntrate)
print,'Normilization ratio (background/source): ',norm_ratio
print,' '


;
; Read in header from background file
;
hdbg=headfits('./background/bg.fits',exten=1)


;
; Get EXPOSURE out of background header
;
bg_exp=sxpar(hdbg,'EXPOSURE','Parameter EXPOSURE not found')


;
; Update the background header with the EXPOSURE and BKGNORM keywords
;
print,'Normalizing the background...'
bgexposure=strtrim(string(bg_exp*norm_ratio),2)

cmdstring='dmhedit ./background/bg.fits filelist=none operation=add key=EXPOSURE value='+bgexposure+' datatype=float'
spawn,cmdroot+cmdstring,result
cmdstring='dmhedit ./background/bg.fits filelist=none operation=add key=BKGNORM value='+strtrim(string(1.0/norm_ratio),2)+' datatype=float'
spawn,cmdroot+cmdstring,result
;cmdstring='dmhedit ./background/bg.fits filelist=none operation=add key=BACKSCAL value='+strtrim(string(norm_ratio),2)+' datatype=float'
;spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Find the total counts on the chip (excluding the point sources)
;
print,'Finding the total counts on the chip...'
cmdstring='dmextract infile="./reprocessed/evt2_ccd_clean_no_ptsrc.fits[bin sky=@regions/ccd.reg]" bkg=NONE outfile=./background/tot_cnts.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmlist "./background/tot_cnts.fits[cols counts]" data,clean outfile=./background/tot_cnts.dat'
spawn,cmdroot+cmdstring,result

get_lun,unit
openr,unit,'./background/tot_cnts.dat'
totcnts=' '
junk=' '
readf,unit,junk
readf,unit,totcnts
close,unit
free_lun,unit

print,'...done.'


;
; Find the counts in the cluster region (excluding the point sources)
;
print,'Finding the counts in the cluster region...'
cmdstring='dmextract infile="./reprocessed/evt2_ccd_clean_no_ptsrc.fits[bin sky=@regions/cluster.reg]" bkg=NONE outfile=./background/cluster_cnts.fits'
spawn,cmdroot+cmdstring,result

cmdstring='dmlist "./background/cluster_cnts.fits[cols counts]" data,clean outfile=./background/cluster_cnts.dat'
spawn,cmdroot+cmdstring,result

get_lun,unit
openr,unit,'./background/cluster_cnts.dat'
clustercnts=' '
junk=' '
readf,unit,junk
readf,unit,clustercnts
close,unit
free_lun,unit

print,'...done.'


;
; Read in header from new evt2_ccd_clean_no_ptsrc file
;
hd=headfits('./reprocessed/evt2_ccd_clean_no_ptsrc.fits',exten=1)


;
; Get OBS_ID, EXPOSURE out of evt2 header
;
exposure=sxpar(hd,'EXPOSURE','Parameter EXPOSURE not found')
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')


;
; Update obs_info file
;
get_lun,unit
outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'
openw,unit,outfile,/append
printf,unit,'Output of ACIS_BG,'+strtrim(string(chipid),2)
printf,unit,!stime
printf,unit,' '
printf,unit,'Cleaned evt2 file (ccd only): ./reprocessed/evt2_ccd_clean.fits'
printf,unit,'Cleaned evt2 file (no ptsrc): ./reprocessed/evt2_ccd_clean_no_ptsrc.fits'
printf,unit,'Cleaned image of ccd ( " )  : ./images/ccd_img_clean.fits'
printf,unit,'Colormaps                   : ./images/ccd_img_lokeV_hikeV.fits'
printf,unit,' '
printf,unit,'New exposure time [s]       : '+strtrim(string(exposure),2)
printf,unit,'Total counts in ccd.reg     : '+strtrim(string(totcnts),2)
printf,unit,'Total counts in cluster.reg : '+strtrim(string(clustercnts),2)
printf,unit,' '
;printf,unit,'Blank-Sky file                           : '+bgfile
printf,unit,'New background file                      : ./background/bg.fits'
printf,unit,'Background count rate (10-12 keV) [cts/s]: '+strtrim(string(bg_cntrate),2)
printf,unit,'Source count rate (10-12 keV) [cts/s]    : '+strtrim(string(source_cntrate),2)
printf,unit,'Normalization ratio (background/source)  : '+strtrim(string(norm_ratio),2)
printf,unit,'Old background exposure time [s]         : '+strtrim(string(bg_exp),2)
printf,unit,'New background exposure time [s]         : '+strtrim(string(bgexposure),2)
printf,unit,' '
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit


;
; Print status info to screen
;
print,' '
print,'ACIS_BG complete.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end