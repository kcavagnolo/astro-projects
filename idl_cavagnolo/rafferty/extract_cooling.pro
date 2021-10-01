pro extract_cooling,chipid=chipid,spectra_dir=spectra_dir,mkacisrmf=mkacisrmf,ignore_last=ignore_last
;-----------------------------------------------------------------------
;
; Name: EXTRACT_COOLING
;
; Purpose: Extracts the two spectra for fitting with the mkcflow model
;          around the cooling radius such that the cooling radius falls
;	   at the edge of a region and not in the middle.
;          
;          
; Optional Inputs:  spectra_dir - the name of the spectra directory
;                   chipid - the id of the chip of interest (default is 7)
;		    
; Keywords: /MKACISRMF - if set, use mkacisrmf to make the weighted rmfs
;         
;         
; Comments: Should be run AFTER extract_annuli, as outputs of extract_
;	    annuli are required
;           
;           
; Revision history:
;       written by DR, 2004-7-21
;	
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 2) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'extract_cooling [, chipid=chipid, spectra_dir=spectra_dir]'
   return   
endif


;
; Set the defaults
;
if (n_elements(chipid) eq 0) then chipid=7
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'


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
; Reset 
;
cd,'primary'

bpixfile=findfile('new_bpix1.fits',count=num)
if (num eq 0) then begin
   bpixfile=findfile('*bpix1.fits',count=num)
   if (num eq 0) then begin
      print,'ERROR:'
      print,'BPIX1 file not found in primary directory!'
      return
   endif
endif
print,'Found BPIX1 file:',bpixfile


cmdstring='punlearn ardlib'
spawn,cmdroot+cmdstring,result

ardlibfile=findfile('acis_set_ardlib',count=num)
if (num eq 0) then begin
   cmdstring='cp '+ciao_root+'/contrib/bin/acis_set_ardlib .'
   spawn,cmdstring,result
endif 

cmdstring='acis_set_ardlib '+bpixfile
spawn,cmdroot+cmdstring,result

cd,'..'


;
; Read in annuli properties from annuli.txt
;
fmt = 'A,F'
readcol,'./'+spectra_dir+'/annuli.txt',skipline=5,f=fmt,junk,annuli_cent,delimiter=':',numline=2,/silent
readcol,'./'+spectra_dir+'/annuli.txt',skipline=11,f=fmt,junk,annuli_prop,delimiter=':',numline=2,/silent
xc=annuli_cent[0]
yc=annuli_cent[1]
ellip=annuli_prop[0]
pa=annuli_prop[1]
print,' '
print,'Using x centroid [pixels] : ',xc
print,'Using y centroid [pixels] : ',yc
print,'Using ellip [1 - b/a]     : ',ellip
print,'Using position angle [rad]: ',pa


;
; Read in average radii in arcsec and 
; cooling times in years from properties_deproj.dat
;
readcol,'./'+spectra_dir+'/properties_deproj.dat',skipline=23,ravg,ravg_kpc,j3,j4,j5,j6,j7,j8,j9,j10,j11,j12,j13,j14,j15,j16,j17,tcool,/silent


;
; Read in sma from radii.dat
;
readcol,'./'+spectra_dir+'/radii.dat',r1,r2,/silent


;
; Find the cooling radius by interpolation or extrapolation
;
s=size(ravg)
nreg=s[1]
if keyword_set(ignore_last) then begin
   tcool=tcool[0:nreg-2]
   ravg=ravg[0:nreg-2]
   ravg_kpc=ravg_kpc[0:nreg-2]
endif
rcool=cspline(tcool,ravg,7.7E9)
rcool_kpc=cspline(tcool,ravg_kpc,7.7E9)
rcool_pix=rcool/0.4919
print,' '
print,'Interpolated cooling radius = '+strtrim(string(rcool),2)+' [arcsec]'
print,'                            = '+strtrim(string(rcool_pix),2)+' [pixels]'
print,'                            = '+strtrim(string(rcool_kpc),2)+' [kpc]'


;
; Find the annulus that contains the cooling radius
;
ncool=0
outer=0
for i=1,nreg do begin
   if ( (rcool_pix gt r1[i-1]) and (rcool_pix lt r2[i-1]) ) then ncool=i
endfor
print,' '
print,'Cooling radius is located in Annulus '+strtrim(string(ncool),2)+' ('+strtrim(string(r1[ncool-1]),2)+' - '+strtrim(string(r2[ncool-1]),2)+' pixels).'
print,' '
if (ravg[ncool-1]/0.4919 le rcool_pix) then reg_to_expand=ncool else reg_to_expand=ncool-1
if (reg_to_expand eq nreg) then begin
   print,'Outer region will be changed to ('+strtrim(string(r1[reg_to_expand-1]),2)+' - '+strtrim(string(r2[reg_to_expand-1]),2)+' pixels).'
   outer=1
   goto,jump1
endif
if (reg_to_expand eq 0) then reg_to_expand=1	;make sure reg_to_expand is not zero
print,'Annulus '+strtrim(string(reg_to_expand),2)+' will be changed to ('+strtrim(string(r1[reg_to_expand-1]),2)+' - '+strtrim(string(rcool_pix),2)+' pixels), and 
print,'Annulus '+strtrim(string(reg_to_expand+1),2)+' will be changed to ('+strtrim(string(rcool_pix),2)+' - '+strtrim(string(r2[reg_to_expand]),2)+' pixels).'
print,'Cooling region number (required for FIT_SPECTRA): '+strtrim(string(reg_to_expand),2)


;
; Make the new regions
;
jump1:
if outer then begin
   r_inner_new=r1[reg_to_expand-1]
   r_outer_new=r2[reg_to_expand-1]
endif else begin
   r_inner_new=dblarr(2)
   r_outer_new=dblarr(2)
   r_inner_new[0]=r1[reg_to_expand-1]
   r_inner_new[1]=rcool_pix
   r_outer_new[0]=rcool_pix
   r_outer_new[1]=r2[reg_to_expand]
endelse


;
; Write region files in CIAO format for the new annuli
;
if outer then begin
 if (ellip eq 0.0) then begin
   outfile='./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand),2)+'.reg'
   xcstring=strtrim(string(xc),2)
   ycstring=strtrim(string(yc),2)
   r1string=strtrim(string(r_inner_new),2)
   r2string=strtrim(string(r_outer_new),2)
   get_lun,unit
   openw,unit,outfile
   printf,unit,'# Region file format: CIAO version 3.0'
   printf,unit,'annulus(',xcstring,',',ycstring,',',r1string,',',r2string,')'
   close,unit
   free_lun,unit
 endif else begin
   pastring=strtrim(string(pa/(2.0*!PI)*360.0),2)
   outfile='./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand),2)+'.reg'
   xcstring=strtrim(string(xc),2)
   ycstring=strtrim(string(yc),2)
   a1string=strtrim(string(r_inner_new),2)
   a2string=strtrim(string(r_outer_new),2)
   b1string=strtrim(string(r_inner_new*(1-ellip)),2)
   b2string=strtrim(string(r_outer_new*(1-ellip)),2)
   get_lun,unit
   openw,unit,outfile
   printf,unit,'# Region file format: CIAO version 3.1'
   printf,unit,'+ellipse(',xcstring,',',ycstring,',',a2string,',',b2string,',',pastring,')'
   printf,unit,'-ellipse(',xcstring,',',ycstring,',',a1string,',',b1string,',',pastring,')'
   close,unit
   free_lun,unit
 endelse
endif else begin
 if (ellip eq 0.0) then begin
   for i=0,1 do begin
      outfile='./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand+i),2)+'.reg'
      xcstring=strtrim(string(xc),2)
      ycstring=strtrim(string(yc),2)
      r1string=strtrim(string(r_inner_new(i)),2)
      r2string=strtrim(string(r_outer_new(i)),2)
      get_lun,unit
      openw,unit,outfile
      printf,unit,'# Region file format: CIAO version 3.0'
      printf,unit,'annulus(',xcstring,',',ycstring,',',r1string,',',r2string,')'
      close,unit
      free_lun,unit
   endfor
 endif else begin
   pastring=strtrim(string(pa/(2.0*!PI)*360.0),2)
   for i=0,1 do begin
      outfile='./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand+i),2)+'.reg'
      xcstring=strtrim(string(xc),2)
      ycstring=strtrim(string(yc),2)
      a1string=strtrim(string(r_inner_new(i)),2)
      a2string=strtrim(string(r_outer_new(i)),2)
      b1string=strtrim(string(r_inner_new(i)*(1-ellip)),2)
      b2string=strtrim(string(r_outer_new(i)*(1-ellip)),2)
      get_lun,unit
      openw,unit,outfile
      printf,unit,'# Region file format: CIAO version 3.1'
      printf,unit,'+ellipse(',xcstring,',',ycstring,',',a2string,',',b2string,',',pastring,')'
      printf,unit,'-ellipse(',xcstring,',',ycstring,',',a1string,',',b1string,',',pastring,')'
      close,unit
      free_lun,unit
   endfor
 endelse
endelse


;
; Create pha, wrmf, and warf files
;
; First copy evt2, bg, qe_corr, and GTI files to spectra directory
; (if they're not there already)
;
search_path=spectra_dir+'/evt2_ccd_clean_no_ptsrc.fits'
evt2file=findfile(search_path,count=num)
if (num eq 0) then begin
   cmdstring='cp reprocessed/evt2_ccd_clean_no_ptsrc.fits '+spectra_dir+'/.'
   spawn,cmdstring
endif
search_path=spectra_dir+'/bg.fits'
bgfile=findfile(search_path,count=num)
if (num eq 0) then begin
   cmdstring='cp background/bg.fits '+spectra_dir+'/.'
   spawn,cmdstring
endif
search_path=spectra_dir+'/evt2_ccd_bg.gti'
gtifile=findfile(search_path,count=num)
if (num eq 0) then begin
   cmdstring='cp background/evt2_ccd_bg.gti '+spectra_dir+'/.'
   spawn,cmdstring
endif


;
; If \MKACISRMF is set, use it instead of acisspec_mod
;
if keyword_set(mkacisrmf) then begin
   maskfile=findfile('./secondary/*msk1*',count=num)
   if (num eq 0) then begin
      cd,spectra_dir
      if outer then begin
         print,'Now running WEXTRACT on outer region'
         print,'-------------------------------------------'
         root='cooling_reg'+strtrim(string(reg_to_expand-1),2)
         reg_file='cooling_reg'+strtrim(string(reg_to_expand-1),2)+'.reg'
         wextract,reg_file,'evt2_ccd_clean_no_ptsrc.fits','bg.fits',root,ciao_path=ciao_root
      endif else begin
         for i=1,2 do begin
            print,'Now running WEXTRACT on region '+strtrim(string(i),2)+' of 2'
            print,'-------------------------------------------'
            root='cooling_reg'+strtrim(string(reg_to_expand+i-1),2)
            reg_file='cooling_reg'+strtrim(string(reg_to_expand+i-1),2)+'.reg'
            wextract,reg_file,'evt2_ccd_clean_no_ptsrc.fits','bg.fits',root,ciao_path=ciao_root
         endfor
      endelse
   endif else begin
      cd,spectra_dir
      if outer then begin
         print,'Now running WEXTRACT on outer region'
         print,'-------------------------------------------'
         root='cooling_reg'+strtrim(string(reg_to_expand-1),2)
         reg_file='cooling_reg'+strtrim(string(reg_to_expand-1),2)+'.reg'
         wextract,reg_file,'evt2_ccd_clean_no_ptsrc.fits','bg.fits',root,ciao_path=ciao_root,mask_file=maskfile
      endif else begin
         for i=1,2 do begin
            print,'Now running WEXTRACT on region '+strtrim(string(i),2)+' of 2'
            print,'-------------------------------------------'
            root='cooling_reg'+strtrim(string(reg_to_expand+i-1),2)
            reg_file='cooling_reg'+strtrim(string(reg_to_expand+i-1),2)+'.reg'
            wextract,reg_file,'evt2_ccd_clean_no_ptsrc.fits','bg.fits',root,ciao_path=ciao_root,mask_file=maskfile
         endfor
      endelse
   endelse

endif else begin

   ;
   ; Call ACISSPEC_MOD script to create pha, wrmf, and warf files   
   ;
   cd,spectra_dir
   if outer then begin
      print,' '
      print,'Now running ACISSPEC_MOD on outer region.'
      print,'-------------------------------------------'
      root='cooling_reg'+strtrim(string(reg_to_expand-1),2)
      reg_file='cooling_reg'+strtrim(string(reg_to_expand-1),2)+'.reg'
   
      cmdstring='acisspec_mod soufile1="evt2_ccd_clean_no_ptsrc.fits[sky=region('+reg_file+')]" bgfile1="bg.fits[sky=region('+reg_file+')]" root='+root
      spawn,cmdroot+cmdstring,result
   endif else begin
      for i=1,2 do begin
         print,' '
         print,'Now running ACISSPEC_MOD on region '+strtrim(string(i),2)+' of 2'
         print,'-------------------------------------------'
         root='cooling_reg'+strtrim(string(reg_to_expand+i-1),2)
         reg_file='cooling_reg'+strtrim(string(reg_to_expand+i-1),2)+'.reg'
     
         cmdstring='acisspec_mod soufile1="evt2_ccd_clean_no_ptsrc.fits[sky=region('+reg_file+')]" bgfile1="bg.fits[sky=region('+reg_file+')]" root='+root
         spawn,cmdroot+cmdstring,result
      endfor
   endelse
endelse
print,' '
print,'Extraction complete.'
print,' '
cd,'..'


;
; Add keywords required by PROJCT to header of each
; pha file:  XFLT001 = semi-major axis in pixels for outer boundary
;	     XFLT002 = semi-minor axis
;	     XFLT003 = position angle
;
print,' '
answer=' '
print,'During the extract_annuli stage, did you calculate'
read,answer,prompt='angles of partial annulus segments (y/n)? '
answer_check:
if ( (answer ne 'y') and (answer ne 'n') ) then begin
   read,answer,prompt='Please type "y" or "n": '
   goto,answer_check
endif

if (answer eq 'n') then begin
   if (ellip eq 0.0) then begin
      for i=1,2 do begin
        istring=strtrim(string(reg_to_expand+i-1),2)
        phafile='./'+spectra_dir+'/cooling_reg'+istring+'_sou.pi'
        rstring=strtrim(string(r_outer_new(i-1)),2)
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+rstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+rstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value=0.0 datatype=float'
        spawn,cmdroot+cmdstring,result
      endfor
   endif else begin
      for i=1,2 do begin
        istring=strtrim(string(reg_to_expand+i-1),2)
        phafile='./'+spectra_dir+'/cooling_reg'+istring+'_sou.pi'
        astring=strtrim(string(r_outer_new(i-1)),2)
        bstring=strtrim(string(r_outer_new(i-1)*(1.0-ellip)),2)
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+astring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+bstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value='+pastring+' datatype=float'
        spawn,cmdroot+cmdstring,result
      endfor
   endelse
endif else begin
   if (ellip eq 0.0) then begin
      for i=1,2 do begin
        istring=strtrim(string(reg_to_expand+i-1),2)
        phafile='./'+spectra_dir+'/reg'+istring+'_sou.pi'
        hd=headfits(phafile,exten=1)
        xflt0004=sxpar(hd,'XFLT0004','Parameter XFLT0004 not found')
        xflt0005=sxpar(hd,'XFLT0005','Parameter XFLT0005 not found')
;        xflt0006=sxpar(hd,'XFLT0006','Parameter XFLT0006 not found')
;        xflt0007=sxpar(hd,'XFLT0007','Parameter XFLT0007 not found')
        xflt4string=strtrim(string(xflt0004),2)
        xflt5string=strtrim(string(xflt0005),2)
;        xflt6string=strtrim(string(xflt0006),2)
;        xflt7string=strtrim(string(xflt0007),2)
        phafile='./'+spectra_dir+'/cooling_reg'+istring+'_sou.pi'
        rstring=strtrim(string(r_outer_new(i-1)),2)
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+rstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+rstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value=0.0 datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0004 value='+xflt4string+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0005 value='+xflt5string+' datatype=float'
        spawn,cmdroot+cmdstring,result
;        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0006 value='+xflt6string+' datatype=float'
;        spawn,cmdroot+cmdstring,result
;        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0007 value='+xflt7string+' datatype=float'
;        spawn,cmdroot+cmdstring,result
      endfor
   endif else begin
      for i=1,2 do begin
        istring=strtrim(string(reg_to_expand+i-1),2)
        phafile='./'+spectra_dir+'/reg'+istring+'_sou.pi'
        hd=headfits(phafile,exten=1)
        xflt0004=sxpar(hd,'XFLT0004','Parameter XFLT0004 not found')
        xflt0005=sxpar(hd,'XFLT0005','Parameter XFLT0005 not found')
;        xflt0006=sxpar(hd,'XFLT0006','Parameter XFLT0006 not found')
;        xflt0007=sxpar(hd,'XFLT0007','Parameter XFLT0007 not found')
        xflt4string=strtrim(string(xflt0004),2)
        xflt5string=strtrim(string(xflt0005),2)
;        xflt6string=strtrim(string(xflt0006),2)
;        xflt7string=strtrim(string(xflt0007),2)
        phafile='./'+spectra_dir+'/cooling_reg'+istring+'_sou.pi'
        astring=strtrim(string(r_outer_new(i-1)),2)
        bstring=strtrim(string(r_outer_new(i-1)*(1.0-ellip)),2)
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+astring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+bstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value='+pastring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0004 value='+xflt4string+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0005 value='+xflt5string+' datatype=float'
        spawn,cmdroot+cmdstring,result
;        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0006 value='+xflt6string+' datatype=float'
;        spawn,cmdroot+cmdstring,result
;        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0007 value='+xflt7string+' datatype=float'
;        spawn,cmdroot+cmdstring,result
      endfor
   endelse
endelse


;
; Add normalized background exposure time to background files
;
hdbg=headfits('./background/bg.fits',exten=1)
bg_exp=sxpar(hdbg,'EXPOSURE','Parameter EXPOSURE not found')
bgexposure=strtrim(string(bg_exp),2)
for i=1,2 do begin
   istring=strtrim(string(reg_to_expand+i-1),2)
   bgfile='./'+spectra_dir+'/cooling_reg'+istring+'_bgd.pi'
   cmdstring='dmhedit '+bgfile+' filelist=none operation=add key=EXPOSURE value='+bgexposure+' datatype=float'
   spawn,cmdroot+cmdstring,result
endfor


;
; Read in header from evt2_ccd_clean file
;
hd=headfits('./reprocessed/evt2_ccd_clean.fits',exten=1)


;
;
; Get OBS_ID out of header
;
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')


;
; Update the obs_info file
;
get_lun,unit
outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'
openw,unit,outfile,/append
printf,unit,' '
printf,unit,'Output of EXTRACT_COOLING, chipid='+strtrim(string(chipid),2)+", spectra_dir='"+spectra_dir+"'"
printf,unit,!stime
printf,unit,' '
printf,unit,'Cooling region number               : '+strtrim(string(reg_to_expand),2)
printf,unit,'Cooling radius (at 7.7 Gyr) [arcsec]: '+strtrim(string(rcool),2)
printf,unit,'Cooling radius (at 7.7 Gyr) [kpc]   : '+strtrim(string(rcool_kpc),2)
printf,unit,'Spectral files created              : ./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand),2)+'_sou.pi'
printf,unit,'                                      ./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand),2)+'_bgd.pi'
printf,unit,'                                      ./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand+1),2)+'_sou.pi'
printf,unit,'                                      ./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand+1),2)+'_bgd.pi'
printf,unit,'Region files for new annuli         : ./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand),2)+'.reg'
printf,unit,'                                      ./'+spectra_dir+'/cooling_reg'+strtrim(string(reg_to_expand+1),2)+'.reg'
printf,unit,' '
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit


;
; Print status info to screen
;
print,' '
print,'EXTRACT_COOLING complete.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end
