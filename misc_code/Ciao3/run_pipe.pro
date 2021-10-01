pro run_pipe
;-----------------------------------------------------------------------
;
; Name: RUN_PIPE
;
;
; Purpose: Sets up and runs the pipeline noninteractively through 
;	   spectral extraction (does not fit the spectra).         
;          
;          
; Inputs:  chipid - the id of the chip of interest
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by DAR, 2006-03-03 
;	
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 0) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'setup_pipe'
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
cmdstring='mkdir regions'
spawn,cmdstring,result
cmdstring='mkdir background'
spawn,cmdstring,result
cmdstring='mkdir images'
spawn,cmdstring,result
cmdstring='mkdir reprocessed'
spawn,cmdstring,result


;
; Unzip any gzipped files
;
print,' '
print,'Unzipping any gzipped files...'
cd,'primary'
cmdstring='gunzip *.gz'
spawn,cmdstring,result
cd,'..'
cd,'secondary'
cmdstring='gunzip *.gz'
spawn,cmdstring,result
cd,'..'
;cd,'supporting'
;cmdstring='gunzip *.gz'
;spawn,cmdstring,result
;cd,'..'
print,'...done'
print,' '


;
; Read in info from evt2 file
;
evt2file=findfile('primary/acisf*evt2.fits')
hd2=headfits(evt2file(0),exten=1)
obsid=sxpar(hd2,'OBS_ID','Parameter OBS_ID not found')
object=sxpar(hd2,'OBJECT','Parameter OBJECT not found')
dateobs=sxpar(hd2,'DATE-OBS','Parameter DATE-OBS not found')
exposure=sxpar(hd2,'EXPOSURE','Parameter EXPOSURE not found')


;
; Ask for chip number
;
print,' '
print,' '
print,' Now starting setup for ObsID '+strtrim(string(obsid),2)+' ('+strtrim(string(object),2)+').'
print,' '
answer=' '
print,' '
print,' Now opening the event 2 file in ds9. '
print,' '
print," If you don't know the chip ID, use the BIN menu to bin out"
print,' so that the entire focal plane is visible. This will allow you'
print,' to identify the ID of the chip of interest (see README for chip'
print,' layout). '
print,' '
print,' Next, create a region centered on the cluster that'
print,' encompases the bright emmision and save it in Ciao'
print,' format as "regions/cluster.reg".'
print,' '
print,' Then, delete this region and create a new region'
print,' that encompases the entire ccd of interest (the box region'
print,' works well -- hold down shift and grab a corner to'
print,' rotate) and save it in Ciao format as "regions/ccd.reg".'
print,' '  
print," Exit ds9 when you're done."
print,' '
cmdstring=ciao_root+'/ots/saord/ds9 '+evt2file
spawn,cmdstring,result
print,' '

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
   cmdstring=ciao_root+'/ots/saord/ds9 '+evt2file
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
   cmdstring=ciao_root+'/ots/saord/ds9 '+evt2file
   spawn,cmdstring,result 
   ntries=ntries+1
   if (ntries eq 2) then return
   goto,chk_reg
endif 


read,chipid,prompt=' Enter the chip number you wish to analyze: (0-9) '
answer_check:
if ( (chipid lt 0) or (chipid gt 9) ) then begin
   read,chipid,prompt='Please enter a number between 0-9: '
   goto,answer_check
endif
chipid=fix(chipid)


;
; Run dmcopy in CIAO to restrict events to the specified ccd
;
print,' '
print,'Running dmcopy to restrict events to specified ccd and 0.1<E<12 keV...'
cmdstring='dmcopy "'+evt2file+'[energy=100:12000,ccd_id='+strtrim(string(chipid),2)+']" ./reprocessed/evt2_ccd.fits'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Look up correct backgound file
;
print,' '
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


;
; Second, run wavdetect on evt2_ccd.fits
;
; Reduce image size
;
print,' '
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


;
; Start ds9 to check point sources
;
print,'Now starting ds9...'
print,'If you want to remove all regions found by WAVEDETECT,'
print,'simply exit ds9 without making any changes.  If, however,'
print,'you want to make changes, please delete regions '
print,'that you do not want to be removed during spectral fitting.'
print,'Any remaining regions will be removed before fitting.  '
print,'Save the new region file as "regions/ccd_src_alt.reg" in Ciao format.'
print,' '
print,'Lastly, exit ds9.'
cmdstring=ciao_root+'/ots/saord/ds9 ./background/ccd_img.fits -regionfile ./regions/ccd_src.reg'
spawn,cmdstring,result
print,'...done.'


;
; Find the counts in the cluster region (excluding the point sources)
;
print,'Finding the counts in the cluster region...'
cmdstring='dmextract infile="./reprocessed/evt2_ccd.fits[bin sky=@regions/cluster.reg]" bkg=NONE outfile=./background/cluster_cnts.fits'
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

print,' '
print,' There are a total of '+strtrim(string(clustercnts),2)+' counts in "cluster.reg".'
print,' '
print,' Now starting ds9 to determine annular properties.'
print,' Determine the center, ellipticity, position angle, and'
print,' minimum number of counts per annulus that you desire.'
print," When you're done, exit ds9."
cmdstring=ciao_root+'/ots/saord/ds9 ./background/ccd_img.fits'
spawn,cmdstring,result


center=dblarr(2)
print,' '
read,center,prompt=' Enter the center for the annuli in physical coordinates as x,y: '
xc=center[0]
yc=center[1]
read,e,prompt=' Enter the ellipticity for the annuli (1-b/a): '
read,posang,prompt=' Enter the position angle for the annuli (degrees ccw from +x-axis): '
read,mincnts,prompt=' Enter the minimum number of counts for each annulus: '
read,centercnts,prompt=' Enter the number of counts for the central region: '
read,numregs,prompt=' Enter the number of regions to extract (0 to decide later): '


;
; Remove temporary files: ./background/cluster_cnts.fits
;			  ./background/cluster_cnts.dat
;			  ./background/evt2_ccd_no_src.fits
;			  ./background/evt2_ccd_no_ptsrc.fits
;			  ./reprocessed/evt2_ccd.fits
;
rm_file,'./background/cluster_cnts.fits'
rm_file,'./background/cluster_cnts.dat'
rm_file,'./background/evt2_ccd_no_src.fits'
rm_file,'./background/evt2_ccd_no_ptsrc.fits'
rm_file,'./reprocessed/evt2_ccd.fits'


;
; Run the pipeline
;
acis_reprocess,chipid=chipid,/newbpix,/ni
if (numregs eq 0) then extract_annuli,xc,yc,mincnts,/mkacisrmf,chipid=chipid,ellip=e,pa=posang,cencnts=centercnts else extract_annuli,xc,yc,mincnts,/mkacisrmf,chipid=chipid,ellip=e,pa=posang,cencnts=centercnts,n_to_fit=numregs


;
; Return to IDL
;
return
end