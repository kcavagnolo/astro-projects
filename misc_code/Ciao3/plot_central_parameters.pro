pro plot_central_parameters,data_file
;------------------------------------------------------------------------------
; Name:  PLOT_PARAMETERS
;
; Purpose:  Plot deprojected parameters for the sample
;
;
; Inputs:  data_file - data file output by MAKE_PARAMETER_TABLE
;
;
; Comments:
;
;
; Revision history:
;       written by DAR, 2004-8-27
;------------------------------------------------------------------------------   
;
; Read data from the output of make_parameter_table
;
fmt='a,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d'
readcol,/debug,data_file,rkpc,ct,ctloerr,cthierr,kT,kTloerr,kThierr,n_e,n_eloerr,n_ehierr,s,sloerr,shierr,pres,presloerr,preshierr,U_b_color
;readcol,
sz=size(ct)
nplot=sz[1]
zerox=intarr(1,nplot)


;
; Define error in the colors
;
U_b_err=dblarr(1,nplot)
for i=0,nplot-1 do begin
   U_b_err[i]=sqrt(2.0*(0.02^2.0))	; error in McNamara O'Connell 1992 in fits to standard stars and my error in reading from the plots
endfor
;U_b_err[1]=0.06	; error in A2124 due to transformation from D4000 to (U-b)Nuc
U_b_err[11]=0.06		; error in Hydra A
U_b_err[4]=0.06		; error in A478
;U_b_err[4]=0.06	; error in A644


;
; Set plot 1 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='central_value_plots.ps',/color,landscape=1
!p.multi=[0,2,2]
!x.style=1
!y.style=1
!psym=8


;
; (1) Plot cooling time vs. color
;
plotsym,0,1
ploterror,ct/1.0E9,U_b_color,zerox,U_b_err,$
   xtitle='Central cooling time (Gyr)',$
   ytitle='(U-b)!DNuc!N',$
   xrange=[0.1,0.7],$
   yrange=[1.1,-0.1]
oploterror,ct/1.0E9,U_b_color,ctloerr/1.0E9,U_b_err,/lobar
oploterror,ct/1.0E9,U_b_color,cthierr/1.0E9,U_b_err,/hibar


;
; (2) Plot n_e vs. color
;
plotsym,0,1
ploterror,n_e,U_b_color,zerox,U_b_err,$
   xtitle='n!De!N (cm!E-3!N)',$
   ytitle='(U-b)!DNuc!N',$
   xrange=[0,.2],$
   yrange=[1.1,-0.1]
oploterror,n_e,U_b_color,n_eloerr,U_b_err,/lobar
oploterror,n_e,U_b_color,n_ehierr,U_b_err,/hibar


;
; (3) Plot S vs. color
;
plotsym,0,1
ploterror,s,U_b_color,zerox,U_b_err,$
   xtitle='S (keV cm!E2!N)',$
   ytitle='(U-b)!DNuc!N',$
   xrange=[0,25],$
   yrange=[1.1,-0.1]
oploterror,s,U_b_color,sloerr,U_b_err,/lobar
oploterror,s,U_b_color,shierr,U_b_err,/hibar


;
; (4) Plot P vs. color
;
plotsym,0,1
ploterror,pres,U_b_color,zerox,U_b_err,$
   xtitle='P (erg cm!E-3!N)',$
   ytitle='(U-b)!DNuc!N',$
   xrange=[1E-10,2E-9],$
   yrange=[1.1,-0.1]
oploterror,pres,U_b_color,presloerr,U_b_err,/lobar
oploterror,pres,U_b_color,preshierr,U_b_err,/hibar

device, /close

;
; Plot 2 (nice plot of color vs ct) 
;
!p.font=0
set_plot,'ps'
device,/encapsul,xsize=10,ysize=10,set_font='Times-Roman',file='ct_vs_color.eps',/color,landscape=0
!p.multi=[0,0,1]
!x.style=1
!y.style=1
!psym=8
plotsym,0,1
ploterror,ct/1.0E9,U_b_color-0.77,zerox,U_b_err,$
   position=[0.1,0.1,0.95,0.95],$
   xtitle='Central cooling time (10!E9!N yr)',$
   ytitle='!9D!X(U-B)!DNuc!N (mag)',$
   xrange=[0.1,2.1],$
   yrange=[0.3,-0.9]
oploterror,ct/1.0E9,U_b_color-0.77,ctloerr/1.0E9,U_b_err,/lobar
oploterror,ct/1.0E9,U_b_color-0.77,cthierr/1.0E9,U_b_err,/hibar
device, /close

;
; Return to IDL
;
return
end
