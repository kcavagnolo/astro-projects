pro sb_profile,evt2file,regfile,ccdfile,psroot=psroot
;-----------------------------------------------------------------------
;
; Name: SB_PROFILE
;
; Purpose: Generates a surface brightness profile given a set of annuli
;          from ds9
;          
;          
; Inputs:  evt2file - input evt2 file name
;	   regfile - input region file name made using the annuli region in ds9
;	   ccdfile - ccd region file name 
;	   psroot - output postscript root name (e.g. "sb_plot")
;         
;         
; Comments:  To be run in 'SB' directory
;           
;           
; Revision history:
;       written by D&L, 2003-12-25
;	
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 3)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'sb_profile, evt2file, regfile [, psfile]'
   return   
endif
if (n_elements(psroot) eq 0) then psroot='sb'
psfile=psroot+'.ps'
epsfile=psroot+'.eps'


;
; Define root directory for CIAO
;
get_lun,unit
openr,unit,'../ciao_info.txt'
ciao_root=' '
readf,unit,ciao_root
cmdroot='source '+ciao_root+'/bin/ciao.csh ; '
close,unit
free_lun,unit


;
; Read in region information
;
fmt='a'
readcol,regfile,skipline=1,f=fmt,delimiter=' ',region,/silent


;
; Read in regions to be masked
;
fmt='a'
readcol,ccdfile,skipline=1,f=fmt,delimiter=' ',ccd_region,/silent


;
; Create new region files
;
s=size(region)
nreg=s[1]
s=size(mask_region)
nmask=s[1]
goto,jump1
for i=0,nreg-1 do begin
   reg_file='sb_reg'+strtrim(string(i+1),2)+'.reg'
   get_lun,unit
   openw,unit,reg_file
   printf,unit,'# Region file format: CIAO version 3.1'
   printf,unit,region[i]+'*'+ccd_region
   printf,unit,' '
   close,unit
   free_lun,unit
endfor


;
; Run dmextract using above regions
;
for i=0,nreg-1 do begin
   in_file='"'+evt2file+'[bin sky=region(sb_reg'+strtrim(string(i+1),2)+'.reg)]"'
   out_file='sb_reg'+strtrim(string(i+1),2)+'.fits'
   cmdstring='dmextract infile='+in_file+' outfile='+out_file+' clobber=yes'
   spawn,cmdroot+cmdstring,result
   cmdstring='dmtcalc infile='+out_file+' outfile=mid_'+out_file+' expression="rmid=0.5*(R[0]+R[1])"'
   spawn,cmdroot+cmdstring,result
endfor


;
; Read in the surface brightness for each annulus
;
rmid=fltarr(nreg)
net_counts=fltarr(nreg)
area=fltarr(nreg)
sur_bri=fltarr(nreg)
sur_bri_err=fltarr(nreg)
for i=0,nreg-1 do begin
   sb_file='mid_sb_reg'+strtrim(string(i+1),2)+'.fits[cols rmid,net_counts,area,sur_bri,sur_bri_err]'
   dat_file='mid_sb_reg'+strtrim(string(i+1),2)+'.dat'
   cmdstring='dmlist "'+sb_file+'" data,clean outfile='+dat_file
   spawn,cmdroot+cmdstring,result
   readcol,dat_file,rm,ncnts,ar,sbri,sbri_err,/silent
   rmid[i]=rm
   net_counts[i]=ncnts
   area[i]=ar
   sur_bri[i]=sbri
   sur_bri_err[i]=sbri_err
endfor


;
; Write the output to a file
;
get_lun,unit
outfile='sur_bri.dat'
openw,unit,outfile
printf,unit,'Surface Brightness Profile'
printf,unit,' '
printf,unit,'Column 1:  Radius (pixels)'
printf,unit,'Column 2:  Net counts (counts)'
printf,unit,'Column 3:  Area (pixels^2)'
printf,unit,'Column 4:  Surface brightness (counts/pixel^2)'
printf,unit,'Column 5:  Error (counts/pixel^2)'
printf,unit,' '
for i=0,nreg-1 do begin
   printf,unit,strtrim(string(rmid(i)),2)+'   '+strtrim(string(net_counts(i)),2)+'   '+strtrim(string(area(i)),2)+'   '+strtrim(string(sur_bri(i)),2)+'   '+strtrim(string(sur_bri_err(i)),2)
endfor
printf,unit,' '
close,unit
free_lun,unit

jump1:
;
; Make a plot 
;
readcol,'sur_bri.dat',r,nc,ar,sb,sberr,/silent
hd=headfits(evt2file(0),exten=1)
exposure=sxpar(hd,'EXPOSURE','Parameter EXPOSURE not found')

!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file=psfile
plotsym,0,0.6,/fill
ploterror,r*0.4919,sb/(0.4919)^2.0/exposure,sberr/(0.4919)^2.0/exposure,psym=8,hatlength=111,/xlog,/ylog,$
     title='Surface Brightness Profile',$
     xtitle='Radius (arsec)',ytitle='SB (counts s!E-1!N arcsec!E-2!N)'
device, /close      


;
; Make an encapsulated plot
;
!p.font=0
set_plot,'ps'
!x.style=1
device,/encapsul,xsize=7,ysize=7,set_font='Times-Roman',file=epsfile,landscape=0
!p.multi=[0,1,1]
;device,set_font='Times-Roman',file=epsfile,/encapsulate,xsize=11,ysize=10
plotsym,0,0.6,/fill
ploterror,r*0.4919,sb/(0.4919)^2.0/exposure,sberr/(0.4919)^2.0/exposure,psym=8,hatlength=111,/xlog,/ylog,$
;     title='Surface Brightness Profile',$
          position=[0.1,0.1,0.95,0.95],$
          xrange=[1,200],$
     xtitle='Radius (arsec)',ytitle='SB (cts s!E-1!N arcsec!E-2!N)'
xyouts,30,4E-4,'Surface'
xyouts,30,2.3E-4,'Brightness'
device, /close   


;
; Print status info to screen
;
print,' '
print,'SB_PROFILE complete.'
print,'Please see sur_bri.dat for details and '+psfile+' for the plot.'


;
; Return to IDL
;
return
end
