pro radial_plots
;------------------------------------------------------------------------------
; Name:  RADIAL_PLOTS
;
; Purpose:  Plot temperature, abundance, nH and surface brightness vs. radius 
;           for the annuli.
;
;
; Inputs:  none
;
;
; Comments:
;
;
; Revision history:
;       written by D&L, 2002-12-05
;	added cooling radius plot (DR), 2003-4-16
;       added code to ignore norms with excessive high error (DR), 2003-6-25
;------------------------------------------------------------------------------   
;
; Read data from the outputs of extract_annuli.pro, isis_fit.sl, and deconv.f
;
readcol, './spectra/radii.dat',rin,rout
readcol, './spectra/nHvaried.dat',KTv,KTvlo,KTvhi,Abv,Abvlo,Abvhi,nH,nHlo,nHhi,normv,normvlo,normvhi,redchiv
readcol, './spectra/nHfixed.dat',KTf,KTflo,KTfhi,Abf,Abflo,Abfhi,nHav,normf,normflo,normfhi,redchif
readcol, './spectra/nHgal.dat',KTg,KTglo,KTghi,Abg,Abglo,Abghi,nHg,normg,normglo,normghi,redchig
readcol, './spectra/sb.dat',skipline=14,r1,r2,sb,sberr,area
readcol, './spectra/emiss.dat',rm,r1e,r2e,emiss,emissp,emissm,xne,rml,erp,erm,xnel,enp,enm
readcol, './spectra/xsurf.dat',rmx,r1x,r2x,ctrate,xsurf,xerror,rmlx,erpx,ermx,xsurfl,expx,exm



!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='radial_plots_1.ps',/color,landscape=1
!p.multi=[0,2,2]


;
; Calculate the average radius and width for each annulus
;
scale=0.4919		    ; ACIS-S scale [arcsec/pixel]
ravg=(rin+rout)*scale/2    ; central radius of each annulus
rwidth=(rout-rin)*scale/2  ; width of each annulus
nreg=size(rin,/n_elements) ; number of regions extracted
nfit=size(KTv,/n_elements)  ; number of regions fitted
rmax=max(rout)
!x.style=1
!x.range=[1,rmax*scale+20]



KTvloerr=KTv-KTvlo
KTvhierr=KTvhi-KTv
Abvloerr=Abv-Abvlo  ; Values for nH varied
Abvhierr=Abvhi-Abv
nHloerr=nH-nHlo
nHhierr=nHhi-nH

KTfloerr=KTf-KTflo
KTfhierr=KTfhi-KTf  ; Values for nH fixed to the average
Abfloerr=Abf-Abflo
Abfhierr=Abfhi-Abf

KTgloerr=KTg-KTglo
KTghierr=KTghi-KTg  ; Values for nH fixed to galactic
Abgloerr=Abg-Abglo
Abghierr=Abghi-Abg

zeroy=intarr(1,nreg)


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
legend,'nH varied',psym=88,position=[1.1,min(KTv)+2.0],charsize=0.6,box=0

plotsym,0,0.6,/fill
oploterror,ravg,KTf,rwidth,KTfloerr,psym=8,hatlength=140,/LOBAR
oploterror,ravg,KTf,rwidth,KTfhierr,psym=8,hatlength=140,/HIBAR
legend,'nH average',psym=88,position=[1.1,min(KTv)+1.7],charsize=0.6,box=0

plotsym,0,1
oploterror,ravg,KTg,rwidth,KTgloerr,psym=8,hatlength=100,/LOBAR
oploterror,ravg,KTg,rwidth,KTghierr,psym=8,hatlength=100,/HIBAR
legend,'nH galactic',psym=88,position=[1.1,min(KTv)+1.4],charsize=0.6,box=0


;
; Calculate the density from the emissivity
;
; First find the average of the normilization from MEKAL:
;
normgtot=0.0
normftot=0.0
dnormglotot=0.0
dnormflotot=0.0
dnormghitot=0.0
dnormfhitot=0.0
gcnt=0
fcnt=0
for i=0,nfit-1 do begin
  if (normghi(i) lt 1.0) then begin
    normgtot=normgtot+normg(i)
    ;
    ; Add the errors in quadrature
    ;
    dnormglotot=dnormglotot+(normg(i)-normglo(i))^2.0
    dnormghitot=dnormghitot+(normghi(i)-normg(i))^2.0
    gcnt=gcnt+1
  endif

  if (normfhi(i) lt 1.0) then begin
    normftot=normftot+normf(i)
    dnormflotot=dnormflotot+(normf(i)-normflo(i))^2.0
    dnormfhitot=dnormfhitot+(normfhi(i)-normf(i))^2.0
    fcnt=fcnt+1
  endif
  
endfor

normgavg=normgtot/gcnt		    ; average value of norm for nH galactic
normfavg=normftot/fcnt		    ; average value of norm for nH average

dnormgloavg=sqrt(dnormglotot)/gcnt  ; low uncertainty in normgavg
dnormghiavg=sqrt(dnormghitot)/gcnt  ; high uncertainty in normgavg
dnormfloavg=sqrt(dnormflotot)/fcnt  ; low uncertainty in normfavg
dnormfhiavg=sqrt(dnormfhitot)/fcnt  ; high uncertainty in normfavg


;
; Then calculate the density (n_e=sqrt(emiss/ctrate*norm*1.21E14 [cm^-3])
; and add the fractional uncertainties in quadrature
;
n_eg=sqrt(emiss/ctrate*normgavg*1.21E14)  ; density for nH galactic
n_egloerr=sqrt((0.5*emissm/emiss)^2.0+(0.5*dnormgloavg/normgavg)^2.0)*n_eg  ; add errors in quadrature and find low error of n_eg
n_eghierr=sqrt((0.5*emissp/emiss)^2.0+(0.5*dnormghiavg/normgavg)^2.0)*n_eg  ; add errors in quadrature and find high error of n_eg

n_ef=sqrt(emiss/ctrate*normfavg*1.21E14)  ; density for nH fixed
n_efloerr=sqrt((0.5*emissm/emiss)^2.0+(0.5*dnormfloavg/normfavg)^2.0)*n_ef  ; add errors in quadrature and find low error of n_ef
n_efhierr=sqrt((0.5*emissp/emiss)^2.0+(0.5*dnormfhiavg/normfavg)^2.0)*n_ef  ; add errors in quadrature and find high error of n_ef


;
; Make the plot of n_e vs. radius
;
plotsym,0,1
ploterror,ravg,n_eg,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(n_eg),max(n_eg)],$
     title='Density Profile',$
     xtitle='Radius (arcsec)',ytitle='n!De!N (cm!E-3!N)' 
oploterror,ravg,n_eg,rwidth,n_egloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,n_eg,rwidth,n_eghierr,psym=8,hatlength=180,/HIBAR
plotsym,0,0.6,/fill
oploterror,ravg,n_ef,rwidth,n_efloerr,psym=8,hatlength=110,/LOBAR
oploterror,ravg,n_ef,rwidth,n_efhierr,psym=8,hatlength=110,/HIBAR


;
; Calculate the pressure from the ideal gas law (P=n_ekT [erg/cm^3])
;
presg=n_eg*KTg*1.60219E-9		; includes conversion to from keV to ergs
presgloerr=sqrt((n_egloerr/n_eg)^2.0+(KTgloerr/KTg)^2.0)*presg
presghierr=sqrt((n_eghierr/n_eg)^2.0+(KTghierr/KTg)^2.0)*presg

presf=n_ef*KTf*1.60219E-9
presfloerr=sqrt((n_efloerr/n_ef)^2.0+(KTfloerr/KTf)^2.0)*presf
presfhierr=sqrt((n_efhierr/n_ef)^2.0+(KTfhierr/KTf)^2.0)*presf


;
; Make the plot of pressure vs. raduis
;
plotsym,0,1
ploterror,ravg,presg,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(presg),max(presg)],$
     title='Pressure Profile',$
     xtitle='Radius (arcsec)',ytitle='P (erg cm!E-3!N)' 
oploterror,ravg,presg,rwidth,presgloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,presg,rwidth,presghierr,psym=8,hatlength=180,/HIBAR
plotsym,0,0.6,/fill
oploterror,ravg,presf,rwidth,presfloerr,psym=8,hatlength=110,/LOBAR
oploterror,ravg,presf,rwidth,presfhierr,psym=8,hatlength=110,/HIBAR


;
; Calculate the entropy as S=T/(n_e)^2/3 [kev cm^2]
;
sg=KTg/(n_eg)^(0.66667)
sgloerr=sqrt((KTgloerr/KTg)^2.0+(0.66667*n_egloerr/n_eg)^2.0)*sg
sghierr=sqrt((KTghierr/KTg)^2.0+(0.66667*n_eghierr/n_eg)^2.0)*sg

sf=KTf/(n_ef)^(0.66667)
sfloerr=sqrt((KTfloerr/KTf)^2.0+(0.66667*n_efloerr/n_ef)^2.0)*sf
sfhierr=sqrt((KTfhierr/KTf)^2.0+(0.66667*n_efhierr/n_ef)^2.0)*sf


;
; Make the plot of entropy vs. radius
;
plotsym,0,1
ploterror,ravg,sg,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(sg),max(sg)],$
     title='Entropy Profile',$
     xtitle='Radius (arcsec)',ytitle='S (kev cm!E2!N)' 
oploterror,ravg,sg,rwidth,sgloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,sg,rwidth,sghierr,psym=8,hatlength=180,/HIBAR
plotsym,0,0.6,/fill
oploterror,ravg,sf,rwidth,sfloerr,psym=8,hatlength=110,/LOBAR
oploterror,ravg,sf,rwidth,sfhierr,psym=8,hatlength=110,/HIBAR

device, /close



!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='radial_plots_2.ps',/color,landscape=1
!p.multi=[0,2,2]
ravg=(rin+rout)*scale/2
rwidth=(rout-rin)*scale/2
zeroy=intarr(1,nreg)


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

plotsym,0,0.6,/fill   
oploterror,ravg,Abf,rwidth,Abfloerr,psym=8,hatlength=140,/LOBAR
oploterror,ravg,Abf,rwidth,Abfhierr,psym=8,hatlength=140,/HIBAR

plotsym,0,1
oploterror,ravg,Abg,rwidth,Abgloerr,psym=8,hatlength=100,/LOBAR
oploterror,ravg,Abg,rwidth,Abghierr,psym=8,hatlength=100,/HIBAR


;
; Plot nH vs. radius
;
plotsym,0,1
ploterror,ravg,nH,rwidth,zeroy,psym=8,hatlength=180,/xlog,$
     yrange=[min(nH)-0.03,max(nH)+0.03],$
     title='Absorption Profile',$
      xtitle='Radius (arcsec)',ytitle='N!BH!N (10!E22!N cm!E-2!N)'

oploterror,ravg,nH,rwidth,nHloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,nH,rwidth,nHhierr,psym=8,hatlength=180,/HIBAR

oplot,[1,rmax*scale+20.0],[nHav(0),nHav(0)],line=2,psym=-3
oplot,[1,rmax*scale+20.0],[nHg(0),nHg(0)],line=1,psym=-3


;
; Plot reduced chi-squared vs. radius
;
plotsym,3,0.6,/fill
ploterror,ravg,redchiv,rwidth,zeroy,psym=8,hatlength=180,/xlog,$
     yrange=[0,max(redchiv)+0.5],$
     title='Reduced !9c!X!E2!N',$
      xtitle='Radius (arcsec)',ytitle='Reduced !9c!X!E2!N'

plotsym,0,0.6,/fill   
oploterror,ravg,redchif,rwidth,zeroy,psym=8,hatlength=140

plotsym,0,1   
oploterror,ravg,redchig,rwidth,zeroy,psym=8,hatlength=100


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
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N cm!E-2!N arcsec!E-2!N)'

device, /close


;
; Make a separate plot of the cooling time for use in 
; finding the cooling radius

!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='cooling_radius.ps',/color,landscape=1


;
; Calculate cooling time=2.9E10*sqrt(kT)/n_e vs. radius
;
y_g=2.9E10*sqrt(KTg)/n_eg/1000.0
y_gloerr=sqrt((0.5*KTgloerr/KTg)^2.0+(n_egloerr/n_eg)^2.0)*y_g
y_ghierr=sqrt((0.5*KTghierr/KTg)^2.0+(n_eghierr/n_eg)^2.0)*y_g

y_f=2.9E10*sqrt(KTf)/n_ef/1000.0
y_floerr=sqrt((0.5*KTfloerr/KTf)^2.0+(n_efloerr/n_ef)^2.0)*y_f
y_fhierr=sqrt((0.5*KTfhierr/KTf)^2.0+(n_efhierr/n_ef)^2.0)*y_f

plotsym,0,1
ploterror,ravg,y_g,rwidth,zeroy,psym=8,hatlength=100,/xlog,/ylog,$
     yrange=[min(y_g),max(y_g)],$
     title='Cooling Time',$
     xtitle='Radius (arcsec)',ytitle='t!Dcool!N (yrs)'
oploterror,ravg,y_g,rwidth,y_gloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,y_g,rwidth,y_ghierr,psym=8,hatlength=180,/HIBAR

plotsym,0,0.6,/fill
oploterror,ravg,y_f,rwidth,y_floerr,psym=8,hatlength=110,/LOBAR
oploterror,ravg,y_f,rwidth,y_fhierr,psym=8,hatlength=110,/HIBAR

oplot,[1,rmax*scale+20.0],[1.4E10,1.4E10],line=2,psym=-3

device, /close


;
; Return to IDL
;
return
end
