pro pub_plots,projected=projected,spectra_dir=spectra_dir
;------------------------------------------------------------------------------
; Name:  RADIAL_PLOTS_DEPROJ
;
; Purpose:  Plot deprojected temperature, density, pressure, entropy, abundance, 
;	    surface brightness, and cooling time vs. radius for the annuli. 
;
;
; Inputs:  none
;
; Keywords: /projected - if set, overplot the projected temperature and abundance
;
;
; Comments:
;
;
; Revision history:
;       written by DAR, 2005-10-25
;------------------------------------------------------------------------------   
;
; Read data from the output of radial_plots
;
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'
readcol, './'+spectra_dir+'/sb.dat',skipline=16,r1,r2,sb_ec,sb_ecerr,sb,sberr,area

readcol, './'+spectra_dir+'/properties_deproj.dat',skipline=24,ravg,ravgkpc,KT,KTloerr,KThierr,$
	n_e,n_eloerr,n_ehierr,pres,presloerr,preshierr,s,sloerr,shierr,Ab,Abloerr,Abhierr,y,yloerr,yhierr

scale=0.4919		    ; ACIS-S scale [arcsec/pixel]
sr=size(ravg)
rsize=sr[1]
rwidth=dblarr(rsize)
rwidth[0]=ravg[0]
for i=1,rsize-1 do begin
   rwidth[i]=ravg[i]-(ravg[i-1]+rwidth[i-1])
endfor
zeroy=intarr(1,rsize)
rmax=300

spec_file='./'+spectra_dir+'/source_1T.spectra'
hd=headfits(spec_file,exten=1)
nfit=sxpar(hd,'NUM_REG','Parameter NUM_REG not found')
KTp=dblarr(nfit)
KTp_lo=dblarr(nfit)
KTp_hi=dblarr(nfit)
abp=dblarr(nfit)
abp_lo=dblarr(nfit)
abp_hi=dblarr(nfit)
for i=0,nfit-1 do begin
   KTp[i]=sxpar(hd,'KT'+strtrim(string(i+1),2))
   KTp_lo[i]=sxpar(hd,'KT'+strtrim(string(i+1),2)+'ERRL')
   KTp_hi[i]=sxpar(hd,'KT'+strtrim(string(i+1),2)+'ERRU','Parameter KT'+strtrim(string(i+1),2)+'ERRU not found')
      abp[i]=sxpar(hd,'AB'+strtrim(string(i+1),2),'Parameter AB'+strtrim(string(i+1),2)+' not found')
   abp_lo[i]=sxpar(hd,'AB'+strtrim(string(i+1),2)+'ERRL','Parameter AB'+strtrim(string(i+1),2)+'ERRL not found')
   abp_hi[i]=sxpar(hd,'AB'+strtrim(string(i+1),2)+'ERRU','Parameter AB'+strtrim(string(i+1),2)+'ERRU not found')
endfor
KTploerr=KTp-KTp_lo
KTphierr=KTp_hi-KTp
abploerr=abp-abp_lo
abphierr=abp_hi-abp


;
; Set plot 1 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='radial_plots.eps',landscape=0,/encapsulate,xsize=20,ysize=40
;!p.multi=[0,2,4]
!x.style=1
!x.range=[1,rmax]

;
; Plot temperature vs. radius
;
plotsym,0,0.5
ploterror,ravg,KT,rwidth,zeroy,psym=8,hatlength=80,/xlog,position=[0.1,0.7,0.5,0.9],$
     yrange=[min(KT)-0.5,max(KT)+1.5],$
     xtickname=[' ',' ',' ',' ',' '],$
;     title='Temperature Profile',$
;     xtitle='Radius (arcsec)',
     ytitle='kT (keV)' 
oploterror,ravg,KT,rwidth,KTloerr,psym=8,hatlength=80,/LOBAR
oploterror,ravg,KT,rwidth,KThierr,psym=8,hatlength=80,/HIBAR

if keyword_set(projected) then begin
;   legend,'Deprojected',psym=88,position=[rmax/10.0,min(KT)+2.2],charsize=0.4,box=0
   plotsym,4,0.5
   oploterror,ravg,KTp,rwidth,KTploerr,psym=8,hatlength=80,/LOBAR
   oploterror,ravg,KTp,rwidth,KTphierr,psym=8,hatlength=80,/HIBAR
;   legend,'Projected',psym=88,position=[rmax/10.0,min(KT)+1.4],charsize=0.4,box=0
endif


;
; Make the plot of n_e vs. radius
;
plotsym,0,0.5
ploterror,ravg,n_e,rwidth,zeroy,psym=8,hatlength=80,/xlog,/ylog,position=[0.5,0.7,0.9,0.9],/noerase,$
     yrange=[min(n_e),max(n_e)],$
     xtickname=[' ',' ',' ',' ',' '],$
     ytickname=[' ',' ',' ',' ',' '],$
;     title='Density Profile',$
;     xtitle='Radius (arcsec)',
     ytitle='n!De!N (cm!E-3!N)' 
oploterror,ravg,n_e,rwidth,n_eloerr,psym=8,hatlength=80,/LOBAR
oploterror,ravg,n_e,rwidth,n_ehierr,psym=8,hatlength=80,/HIBAR


;
; Make the plot of pressure vs. raduis
;
plotsym,0,0.5
ploterror,ravg,pres,rwidth,zeroy,psym=8,hatlength=80,/xlog,/ylog,position=[0.1,0.5,0.5,0.7],/noerase,$
     yrange=[min(pres),max(pres)],$
     xtickname=[' ',' ',' ',' ',' '],$
;     title='Pressure Profile',$
;     xtitle='Radius (arcsec)',
     ytitle='P (erg cm!E-3!N)' 
oploterror,ravg,pres,rwidth,presloerr,psym=8,hatlength=80,/LOBAR
oploterror,ravg,pres,rwidth,preshierr,psym=8,hatlength=80,/HIBAR


;
; Make the plot of entropy vs. radius
;
plotsym,0,0.5
ploterror,ravg,s,rwidth,zeroy,psym=8,hatlength=80,/xlog,/ylog,position=[0.5,0.5,0.9,0.7],/noerase,$
     yrange=[min(s),max(s)],$
     xtickname=[' ',' ',' ',' ',' '],$
;     title='Entropy Profile',$
;     xtitle='Radius (arcsec)',
     ytitle='S (kev cm!E2!N)' 
oploterror,ravg,s,rwidth,sloerr,psym=8,hatlength=80,/LOBAR
oploterror,ravg,s,rwidth,shierr,psym=8,hatlength=80,/HIBAR


;
; Plot abundance vs. radius
;
plotsym,0,0.5
ploterror,ravg,Ab,rwidth,zeroy,psym=8,hatlength=80,/xlog,position=[0.1,0.3,0.5,0.5],/noerase,$
     yrange=[min(Ab)-0.3,max(Ab)+0.3],$
     xtickname=[' ',' ',' ',' ',' '],$
;     title='Abundance Profile',$
;     xtitle='Radius (arcsec)',
     ytitle='Z (relative to solar)'
oploterror,ravg,Ab,rwidth,Abloerr,psym=8,hatlength=80,/LOBAR
oploterror,ravg,Ab,rwidth,Abhierr,psym=8,hatlength=80,/HIBAR

if keyword_set(projected) then begin
   plotsym,4,0.5
   oploterror,ravg,abp,rwidth,abploerr,psym=8,hatlength=80,/LOBAR
   oploterror,ravg,abp,rwidth,abphierr,psym=8,hatlength=80,/HIBAR
endif


;
; Plot SB vs. radius
; In units of [cnts/sec/cm^2/arcsec^2]
; (useful since chip effective area drops near the edges)
;
ravgsb=(r1+r2)*scale/2.0
sb_ec=sb_ec/scale^2.0				; convert to cnts/s/arcsec^2
plotsym,0,0.5
ploterror,ravgsb,sb_ec,sb_ecerr,psym=8,hatlength=80,/xlog,/ylog,position=[0.5,0.3,0.9,0.5],/noerase,$
     yrange=[5E-9,max(sb_ec)*1.5],$
;     title='Surface Brightness Profile',$
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N cm!E-2!N arcsec!E-2!N)'


;
; Make a plot of the cooling time 
;
plotsym,0,0.5
ploterror,ravg,y,rwidth,zeroy,psym=8,hatlength=80,/xlog,/ylog,position=[0.1,0.1,0.5,0.3],/noerase,$
     yrange=[min(y),max(y)],$
;     title='Cooling Time',$
     xtitle='Radius (arcsec)',ytitle='t!Dcool!N (yrs)'
oploterror,ravg,y,rwidth,yloerr,psym=8,hatlength=80,/LOBAR
oploterror,ravg,y,rwidth,yhierr,psym=8,hatlength=80,/HIBAR

oplot,[1,rmax],[7.7E9,7.7E9],line=2,psym=-3

device, /close


;
; Return to IDL
;
return
end
