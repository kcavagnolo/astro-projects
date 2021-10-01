pro radial_plots_proj,angscale,spectra_dir=spectra_dir
;------------------------------------------------------------------------------
; Name:  RADIAL_PLOTS_PROJ
;
; Purpose:  Plot projected temperature, density, pressure, entropy, abundance, 
;	    and surface brightness vs. radius for the annuli.
;
;
; Inputs:  z - redshift
;	   lumdist -luminosity distance [Mpc]
;	   angscale - angular scale [kpc/arcsec]
;
;
; Comments:
;
;
; Revision history:
;       written by D&L, 2003-11-12
;------------------------------------------------------------------------------   
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'


;
; Read data from the output of fit_spectra.pro
;
readcol, './'+spectra_dir+'/sb.dat',skipline=14,r1,r2,sb,sberr,area
readcol, './'+spectra_dir+'/radii.dat',rin,rout
readcol, './'+spectra_dir+'/emiss.dat',rm,r1e,r2e,emiss,emissp,emissm,xne,rml,erp,erm,xnel,enp,enm
readcol, './'+spectra_dir+'/xsurf.dat',rmx,r1x,r2x,ctrate,xsurf,xerror,rmlx,erpx,ermx,xsurfl,expx,exm

spec_file='./'+spectra_dir+'/source_1T.spectra'
hd=headfits(spec_file,exten=1)
nHgal=sxpar(hd,'NH0','Parameter NH0 not found')
nfit=sxpar(hd,'NUM_REG','Parameter NUM_REG not found')
KT=dblarr(nfit)
KT_lo=dblarr(nfit)
KT_hi=dblarr(nfit)
ab=dblarr(nfit)
ab_lo=dblarr(nfit)
ab_hi=dblarr(nfit)
norm=dblarr(nfit)
norm_lo=dblarr(nfit)
norm_hi=dblarr(nfit)
for i=0,nfit-1 do begin
   KT[i]=sxpar(hd,'KT'+strtrim(string(i+1),2))
   KT_lo[i]=sxpar(hd,'KT'+strtrim(string(i+1),2)+'ERRL')
   KT_hi[i]=sxpar(hd,'KT'+strtrim(string(i+1),2)+'ERRU','Parameter KT'+strtrim(string(i+1),2)+'ERRU not found')
   ab[i]=sxpar(hd,'AB'+strtrim(string(i+1),2),'Parameter AB'+strtrim(string(i+1),2)+' not found')
   ab_lo[i]=sxpar(hd,'AB'+strtrim(string(i+1),2)+'ERRL','Parameter AB'+strtrim(string(i+1),2)+'ERRL not found')
   ab_hi[i]=sxpar(hd,'AB'+strtrim(string(i+1),2)+'ERRU','Parameter AB'+strtrim(string(i+1),2)+'ERRU not found')
   norm[i]=sxpar(hd,'NORM'+strtrim(string(i+1),2),'Parameter NORM'+strtrim(string(i+1),2)+' not found')
   norm_lo[i]=sxpar(hd,'N'+strtrim(string(i+1),2)+'ERRL','Parameter NORM'+strtrim(string(i+1),2)+'ERRL not found')
   norm_hi[i]=sxpar(hd,'N'+strtrim(string(i+1),2)+'ERRU','Parameter NORM'+strtrim(string(i+1),2)+'ERRU not found')
endfor


;
; Set plot 1 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='radial_plots_proj_1.ps',/color,landscape=1
!p.multi=[0,2,2]


;
; Calculate the average radius and width for each annulus
;
scale=0.4919		    ; ACIS-S scale [arcsec/pixel]
ravg=(rin+rout)*scale/2    ; central radius of each annulus
rwidth=(rout-rin)*scale/2  ; width of each annulus
rmax=max(rout)
!x.style=1
!x.range=[1,rmax*scale+20]

KTv=KT
KTvlo=KT_lo
KTvhi=KT_hi
Abv=ab			; Set values to nH varied case to make it easy
Abvlo=ab_lo
Abvhi=ab_hi
normv=norm
normvlo=norm_lo
normvhi=norm_hi

KTvloerr=KTv-KTvlo
KTvhierr=KTvhi-KTv
Abvloerr=Abv-Abvlo  ; Values for nH varied
Abvhierr=Abvhi-Abv
normvloerr=normv-normvlo
normvhierr=normvhi-normv

;KTgloerr=KTg-KTglo
;KTghierr=KTghi-KTg  ; Values for nH fixed to galactic
;Abgloerr=Abg-Abglo
;Abghierr=Abghi-Abg
;normgloerr=normg-normglo
;normghierr=normghi-normg
zeroy=intarr(1,nfit)


;
; Plot temperature vs. radius
;
plotsym,3,0.6,/fill
ploterror,ravg,KTv,rwidth,zeroy,psym=8,hatlength=111,/xlog,$
     yrange=[min(KTv)-0.5,max(KTv)+0.5],$
     title='Temperature Profile',$
     xtitle='Radius (arcsec)',ytitle='kT (keV)' 
oploterror,ravg,KTv,rwidth,KTvloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,KTv,rwidth,KTvhierr,psym=8,hatlength=180,/HIBAR
;legend,'nH varied',psym=88,position=[1.1,min(KTv)+2.0],charsize=0.6,box=0

;plotsym,0,1
;oploterror,ravg,KTg,rwidth,KTgloerr,psym=8,hatlength=100,/LOBAR
;oploterror,ravg,KTg,rwidth,KTghierr,psym=8,hatlength=100,/HIBAR
;legend,'nH galactic',psym=88,position=[1.1,min(KTv)+1.4],charsize=0.6,box=0


;
; Calculate the density from the emissivity
;
; First find the average of the normilization from MEKAL:
;
normvtot=0.0
dnormvlotot=0.0
dnormvhitot=0.0
vcnt=0
for i=0,nfit-1 do begin
  if (normvhi(i) lt 1.0) then begin
    normvtot=normvtot+normv(i)
    ;
    ; Add the errors in quadrature
    ;
    dnormvlotot=dnormvlotot+(normv(i)-normvlo(i))^2.0
    dnormvhitot=dnormvhitot+(normvhi(i)-normv(i))^2.0
    vcnt=vcnt+1
  endif
endfor

normvavg=normvtot/vcnt		    ; average value of norm for nH galactic

dnormvloavg=sqrt(dnormvlotot)/vcnt  ; low uncertainty in normvavg
dnormvhiavg=sqrt(dnormvhitot)/vcnt  ; high uncertainty in normvavg


;
; Then calculate the density (n_e=sqrt(emiss/ctrate*norm*1.21E14 [cm^-3])
; and add the fractional uncertainties in quadrature
;
n_ev=sqrt(emiss/ctrate*normvavg*1.21E14)  ; density for nH galactic
n_evloerr=sqrt((0.5*emissm/emiss)^2.0+(0.5*dnormvloavg/normvavg)^2.0)*n_ev  ; add errors in quadrature and find low error of n_eg
n_evhierr=sqrt((0.5*emissp/emiss)^2.0+(0.5*dnormvhiavg/normvavg)^2.0)*n_ev  ; add errors in quadrature and find high error of n_eg


;
; Make the plot of n_e vs. radius
;
plotsym,0,1
ploterror,ravg,n_ev,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(n_ev),max(n_ev)],$
     title='Density Profile',$
     xtitle='Radius (arcsec)',ytitle='n!De!N (cm!E-3!N)' 
oploterror,ravg,n_ev,rwidth,n_evloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,n_ev,rwidth,n_evhierr,psym=8,hatlength=180,/HIBAR
;plotsym,0,0.6,/fill
;oploterror,ravg,n_eg,rwidth,n_egloerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,n_eg,rwidth,n_eghierr,psym=8,hatlength=110,/HIBAR


;
; Calculate the pressure from the ideal gas law (P=2.0*n_ekT [erg/cm^3])
;
;presg=2.0*n_eg*KTg*1.60219E-9		; includes conversion to from keV to ergs
;presgloerr=sqrt((n_egloerr/n_eg)^2.0+(KTgloerr/KTg)^2.0)*presg
;presghierr=sqrt((n_eghierr/n_eg)^2.0+(KTghierr/KTg)^2.0)*presg

presv=2.0*n_ev*KTv*1.60219E-9
presvloerr=sqrt((n_evloerr/n_ev)^2.0+(KTvloerr/KTv)^2.0)*presv
presvhierr=sqrt((n_evhierr/n_ev)^2.0+(KTvhierr/KTv)^2.0)*presv


;
; Make the plot of pressure vs. raduis
;
plotsym,0,1
ploterror,ravg,presv,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(presv),max(presv)],$
     title='Pressure Profile',$
     xtitle='Radius (arcsec)',ytitle='P (erg cm!E-3!N)' 
oploterror,ravg,presv,rwidth,presvloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,presv,rwidth,presvhierr,psym=8,hatlength=180,/HIBAR
;plotsym,0,0.6,/fill
;oploterror,ravg,presg,rwidth,presgloerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,presg,rwidth,presghierr,psym=8,hatlength=110,/HIBAR


;
; Calculate the entropy as S=T/(n_e)^2/3 [kev cm^2]
;
;sg=KTg/(n_eg)^(0.66667)
;sgloerr=sqrt((KTgloerr/KTg)^2.0+(0.66667*n_egloerr/n_eg)^2.0)*sg
;sghierr=sqrt((KTghierr/KTg)^2.0+(0.66667*n_eghierr/n_eg)^2.0)*sg

sv=KTv/(n_ev)^(0.66667)
svloerr=sqrt((KTvloerr/KTv)^2.0+(0.66667*n_evloerr/n_ev)^2.0)*sv
svhierr=sqrt((KTvhierr/KTv)^2.0+(0.66667*n_evhierr/n_ev)^2.0)*sv


;
; Make the plot of entropy vs. radius
;
plotsym,0,1
ploterror,ravg,sv,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(sv),max(sv)],$
     title='Entropy Profile',$
     xtitle='Radius (arcsec)',ytitle='S (kev cm!E2!N)' 
oploterror,ravg,sv,rwidth,svloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,sv,rwidth,svhierr,psym=8,hatlength=180,/HIBAR
;plotsym,0,0.6,/fill
;oploterror,ravg,sf,rwidth,sfloerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,sf,rwidth,sfhierr,psym=8,hatlength=110,/HIBAR
device, /close

skip_den:
;
; Set plot 2 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='radial_plots_proj_2.ps',/color,landscape=1
!p.multi=[0,2,2]


;
; Plot abundance vs. radius
;
plotsym,3,0.6,/fill
ploterror,ravg,Abv,rwidth,zeroy,psym=8,hatlength=111,/xlog,$
     yrange=[min(Abv)-0.3,max(Abv)+0.3],$
     title='Abundance Profile',$
     xtitle='Radius (arcsec)',ytitle='Z (relative to solar)'
oploterror,ravg,Abv,rwidth,Abvloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,Abv,rwidth,Abvhierr,psym=8,hatlength=180,/HIBAR

;plotsym,0,1
;oploterror,ravg,Abg,rwidth,Abgloerr,psym=8,hatlength=100,/LOBAR
;oploterror,ravg,Abg,rwidth,Abghierr,psym=8,hatlength=100,/HIBAR


;
; Plot SB vs. radius
;
ravgsb=(r1+r2)*scale/2.0
sb=sb/scale^2				; convert to cnts/s/cm^2/arcsec^2
sb_elements=where(sb gt 0.0, count)
sb_nonzero=sb[sb_elements]
plotsym,0,0.4
ploterror,ravgsb,sb_nonzero,sberr,psym=8,hatlength=100,/xlog,/ylog,$
     yrange=[min(sb_nonzero),max(sb)*1.1],$
     title='Surface Brightness Profile',$
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N arcsec!E-2!N)'

device, /close

goto,skip_cool
;
; Find cooling time from the NEWCOOL program
;
cfn_file=findfile('cfn.dat',count=num)
if (num eq 0) then begin
   make_cfn_file
endif
print,' '
print,'Please enter the following values into the NEWCOOL program.'
print,'You can select all of them together with the mouse and paste'
print,'with the middle button.  NEWCOOL will then calculate cooling'
print,'times for each line automatically.'
print,' '
for i=0,nfit-1 do begin
   print,strtrim(string(KTv[i]),2)+' '+strtrim(string(n_ev[i]),2)+' '+strtrim(string(Abv[i]),2)
endfor
print,'0 0 0'
print,' '
spawn,'newcool'
print,' '
y_v=dblarr(nfit)
for i=0,nfit-1 do begin
   read,temp,prompt='Please enter the cooling time for region '+strtrim(string(i+1),2)+': '
   y_v[i]=temp
endfor


;
; Make a separate plot of the cooling time for use in 
; finding the cooling radius

!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='cooling_time_deproj.ps',/color,landscape=1

plotsym,0,1
ploterror,ravg,y_v,rwidth,zeroy,psym=8,hatlength=100,/xlog,/ylog,$
     yrange=[min(y_v),max(y_v)],$
     title='Cooling Time',$
     xtitle='Radius (arcsec)',ytitle='t!Dcool!N (yrs)'
;oploterror,ravg,y_v,rwidth,y_vloerr,psym=8,hatlength=180,/LOBAR
;oploterror,ravg,y_v,rwidth,y_vhierr,psym=8,hatlength=180,/HIBAR

;plotsym,0,0.6,/fill
;oploterror,ravg,y_f,rwidth,y_floerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,y_f,rwidth,y_fhierr,psym=8,hatlength=110,/HIBAR

oplot,[1,rmax*scale+20.0],[1.4E10,1.4E10],line=2,psym=-3

device, /close

skip_cool:
;
; Save all values to a file
;
get_lun,unit
outfile='./'+spectra_dir+'/properties_proj.dat'
openw,unit,outfile
printf,unit,'# Cluster properties calculated from the projected data.'
printf,unit,'#'
printf,unit,'# Columns:'
printf,unit,'# (1) Inner radius of annulus [arcsec]'
printf,unit,'# (2) Outer radius of annulus [arcsec]'
printf,unit,'# (3) Average radius of annulus [arcsec]'
printf,unit,'# (4) Average radius of annulus [kpc]'
printf,unit,'# (5) kT [keV]'
printf,unit,'# (6) kT low error [keV]'
printf,unit,'# (7) kT high error [keV]'
printf,unit,'# (8) n_e [cm^-3]'
printf,unit,'# (9) n_e low error [cm^-3]'
printf,unit,'# (10) n_e high error [cm^-3]'
printf,unit,'# (11) Pressure [ergs cm^-3]'
printf,unit,'# (12) Pressure low error [ergs cm^-3]'
printf,unit,'# (13) Pressure high error [ergs cm^-3]'
printf,unit,'# (14) Entropy [keV cm^2]'
printf,unit,'# (15) Entropy low error [keV cm^2]'
printf,unit,'# (16) Entropy high error [keV cm^2]'
printf,unit,'# (17) Abundance [solar]'
printf,unit,'# (18) Abundance low error [solar]'
printf,unit,'# (19) Abundance high error [solar]'
;printf,unit,'# (20) Cooling time [yr]'
printf,unit,' '
fmt='$(2(f9.3,3x),f9.3,3x,f9.3,3(3x,f9.4),6(3x,e13.5),3(3x,f9.3),3(3x,f9.7),3x,e13.5)'
for i=0,nfit-3 do begin
   printf,unit,fmt,rin[i]*scale,rout[i]*scale,ravg[i],ravg[i]*angscale,KTv[i],KTvloerr[i],KTvhierr[i],n_ev[i],n_evloerr[i],n_evhierr[i],presv[i],presvloerr[i],presvhierr[i],sv[i],svloerr[i],svhierr[i],Abv[i],Abvloerr[i],Abvhierr[i]
endfor
printf,unit,' '
close,unit
free_lun,unit



;
; Return to IDL
;
return
end
