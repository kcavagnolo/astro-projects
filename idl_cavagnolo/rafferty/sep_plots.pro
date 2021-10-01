pro sep_plots,projected=projected,spectra_dir=spectra_dir,proj_spectra_dir=proj_spectra_dir,rmax=rmax,r2max=r2max,spectra2_dir=spectra2_dir
;------------------------------------------------------------------------------
; Name:  SEP_PLOTS
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
sun=sunsymbol()
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'
if (n_elements(spectra2_dir) eq 0) then spectra2_dir='spectra'
readcol,spectra_dir+'/sb.dat',skipline=16,r1,r2,sb_ec,sb_ecerr,sb,sberr,area
readcol,spectra_dir+'/properties_deproj.dat',skipline=24,ravg,ravgkpc,KT,KTloerr,KThierr,$
	n_e,n_eloerr,n_ehierr,pres,presloerr,preshierr,s,sloerr,shierr,Ab,Abloerr,Abhierr,y,yloerr,yhierr
readcol,spectra2_dir+'/properties_deproj.dat',skipline=24,ravg2,ravgkpc2,KT2,KTloerr2,KThierr2,$
	n_e2,n_eloerr2,n_ehierr2,pres2,presloerr2,preshierr2,s2,sloerr2,shierr2,Ab2,Abloerr2,Abhierr2,y2,yloerr2,yhierr2	

scale=0.4919		    ; ACIS-S scale [arcsec/pixel]
sr=size(ravg)
rsize=sr[1]
rwidth=dblarr(rsize)
rwidth[0]=ravg[0]
for i=1,rsize-1 do begin
   rwidth[i]=ravg[i]-(ravg[i-1]+rwidth[i-1])
endfor
zeroy=intarr(1,rsize)
if (spectra2_dir ne spectra_dir) then begin
   sr2=size(ravg2)
   rsize2=sr2[1]
   rwidth2=dblarr(rsize2)
   rwidth2[0]=ravg2[0]
   for i=1,rsize2-1 do begin
      rwidth2[i]=ravg2[i]-(ravg2[i-1]+rwidth2[i-1])
   endfor
   zeroy2=intarr(1,rsize2)
endif else begin
   rwidth2=rwidth
   ravg2=ravg
endelse

if (n_elements(rmax) eq 0) then rmax=300
if (n_elements(r2max) eq 0) then r2max=rmax

if (n_elements(proj_spectra_dir) eq 0) then begin
   proj_spectra_dir=spectra_dir
   ravg_proj=ravg
   rwidth_proj=rwidth
endif else begin
   readcol,proj_spectra_dir+'/radii.dat',rin,rout
   scale=0.4919	      	    ; ACIS-S scale [arcsec/pixel]
   ravg_proj=(rin+rout)*scale/2    ; central radius of each annulus
   rwidth_proj=(rout-rin)*scale/2  ; width of each annulus
endelse

spec_file='./'+proj_spectra_dir+'/source_1T.spectra'
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

!p.font=0
set_plot,'ps'
pos_plot=[0.1,0.1,0.9,0.9]
ht=120

;
; Plot temperature vs. radius
;
device,set_font='Times-Roman',file='kT.eps',landscape=0,/encapsulate,xsize=10,ysize=10
!x.style=1
!x.range=[1,rmax]
!y.style=0
plotsym,0,1
ploterror,ravg,KT,rwidth,zeroy,psym=8,hatlength=ht,/xlog,position=pos_plot,$
     yrange=[min(KT)-0.5,max(KT)+1.5],$
;     xtickname=[' ',' ',' ',' ',' '],$
;     title='Temperature Profile',$
     xtitle='Radius (arcsec)',$
     ytitle='kT (keV)' 
oploterror,ravg,KT,rwidth,KTloerr,psym=8,hatlength=ht,/LOBAR
oploterror,ravg,KT,rwidth,KThierr,psym=8,hatlength=ht,/HIBAR

if keyword_set(projected) then begin
   legend,'Deprojected',psym=88,position=[1.1,max(KT)+2.1],charsize=0.8,box=0
   plotsym,4,1
   oploterror,ravg_proj,KTp,rwidth_proj,KTploerr,psym=8,hatlength=ht,/LOBAR
   oploterror,ravg_proj,KTp,rwidth_proj,KTphierr,psym=8,hatlength=ht,/HIBAR
   legend,'Projected',psym=88,position=[1.1,max(KT)+1.4],charsize=0.8,box=0
endif
device, /close


;
; Make the plot of n_e vs. radius
;
device,set_font='Times-Roman',file='ne.eps',landscape=0,/encapsulate,xsize=10,ysize=10
!x.style=1
!x.range=[1,r2max]
!y.style=1
plotsym,0,1
ploterror,ravg,n_e,rwidth,zeroy,psym=8,hatlength=ht,/xlog,/ylog,position=pos_plot,$
     yrange=[0.5*min(n_e),2*max(n_e)],$
;     xtickname=[' ',' ',' ',' ',' '],$
;     ytickname=[' ',' ',' ',' ',' '],$
;     title='Density Profile',$
     xtitle='Radius (arcsec)',$
     ytitle='n!De!N (cm!E-3!N)' 
oploterror,ravg,n_e,rwidth,n_eloerr,psym=8,hatlength=ht,/LOBAR
oploterror,ravg,n_e,rwidth,n_ehierr,psym=8,hatlength=ht,/HIBAR
device, /close


;
; Make the plot of pressure vs. radius
;
device,set_font='Times-Roman',file='p.eps',landscape=0,/encapsulate,xsize=10,ysize=10
!x.style=1
!x.range=[1,r2max]
plotsym,0,1
ploterror,ravg,pres,rwidth,zeroy,psym=8,hatlength=ht,/xlog,/ylog,position=pos_plot,$
     yrange=[0.5*min(pres),2*max(pres)],$
;     xtickname=[' ',' ',' ',' ',' '],$
;     title='Pressure Profile',$
     xtitle='Radius (arcsec)',$
     ytitle='P (erg cm!E-3!N)' 
oploterror,ravg,pres,rwidth,presloerr,psym=8,hatlength=ht,/LOBAR
oploterror,ravg,pres,rwidth,preshierr,psym=8,hatlength=ht,/HIBAR
device, /close


;
; Make the plot of entropy vs. radius
;
device,set_font='Times-Roman',file='S.eps',landscape=0,/encapsulate,xsize=10,ysize=10
!x.style=1
!x.range=[1,rmax]
!y.style=0
plotsym,0,1
ploterror,ravg,s,rwidth,zeroy,psym=8,hatlength=ht,/xlog,/ylog,position=pos_plot,$
     yrange=[min(s),max(s)],$
;     xtickname=[' ',' ',' ',' ',' '],$
;     title='Entropy Profile',$
     xtitle='Radius (arcsec)',$
     ytitle='S (kev cm!E2!N)' 
oploterror,ravg,s,rwidth,sloerr,psym=8,hatlength=ht,/LOBAR
oploterror,ravg,s,rwidth,shierr,psym=8,hatlength=ht,/HIBAR
device, /close


;
; Plot abundance vs. radius -- use spectra2_dir if available
;
device,set_font='Times-Roman',file='Z.eps',landscape=0,/encapsulate,xsize=10,ysize=10
!x.style=1
!x.range=[1,rmax]
plotsym,0,1
ploterror,ravg2,Ab2,rwidth2,zeroy2,psym=8,hatlength=ht,/xlog,position=pos_plot,$
     yrange=[min(Ab2)-0.3,max(Ab2)+0.3],$
;     xtickname=[' ',' ',' ',' ',' '],$
;     title='Abundance Profile',$
     xtitle='Radius (arcsec)',$
     ytitle='Z / Z!X'+sun
oploterror,ravg2,Ab2,rwidth2,Abloerr2,psym=8,hatlength=ht,/LOBAR
oploterror,ravg2,Ab2,rwidth2,Abhierr2,psym=8,hatlength=ht,/HIBAR

;if keyword_set(projected) then begin
;   legend,'Deprojected',psym=88,position=[1.1,max(Ab)+0.4],charsize=0.8,box=0
;   plotsym,4,1
;   oploterror,ravg_proj,abp,rwidth_proj,abploerr,psym=8,hatlength=ht,/LOBAR
;   oploterror,ravg_proj,abp,rwidth_proj,abphierr,psym=8,hatlength=ht,/HIBAR
;   legend,'Projected',psym=88,position=[1.1,max(Ab)+0.25],charsize=0.8,box=0
;endif
device, /close


;
; Plot SB vs. radius
; In units of [cnts/sec/cm^2/arcsec^2]
; (useful since chip effective area drops near the edges)
;
device,set_font='Times-Roman',file='sb.eps',landscape=0,/encapsulate,xsize=10,ysize=10
!x.style=1
!x.range=[1,r2max]
!y.style=1
ravgsb=(r1+r2)*scale/2.0
sb_ec=sb_ec/scale^2.0				; convert to cnts/s/arcsec^2
plotsym,0,1
ploterror,ravgsb,sb_ec,sb_ecerr,psym=8,hatlength=ht,/xlog,/ylog,position=pos_plot,$
     yrange=[5E-9,max(sb_ec)*2],$
;     title='Surface Brightness Profile',$
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N cm!E-2!N arcsec!E-2!N)'
device, /close


;
; Make a plot of the cooling time 
;
device,set_font='Times-Roman',file='t_cool.eps',landscape=0,/encapsulate,xsize=10,ysize=10
!x.style=1
!x.range=[1,rmax]
!y.style=0
plotsym,0,1
ploterror,ravg,y,rwidth,zeroy,psym=8,hatlength=ht,/xlog,/ylog,position=pos_plot,$
     yrange=[min(y),max(y)],$
;     title='Cooling Time',$
     xtitle='Radius (arcsec)',ytitle='Cooling time (yr)'
oploterror,ravg,y,rwidth,yloerr,psym=8,hatlength=ht,/LOBAR
oploterror,ravg,y,rwidth,yhierr,psym=8,hatlength=ht,/HIBAR

oplot,[1,rmax],[7.7E9,7.7E9],line=2,psym=-3

device, /close


;
; Create a tar file of all the plots
;
cmdstring='tar cvf plots.tar sb.eps ne.eps kT.eps p.eps t_cool.eps S.eps Z.eps'
spawn,cmdstring


;
; Return to IDL
;
return
end
