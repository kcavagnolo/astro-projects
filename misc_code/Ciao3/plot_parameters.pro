pro plot_parameters,data_file
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
fmt='a,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d'
readcol,data_file,f=fmt,skipline=27,system_name,rkpc,ct,ctloerr,cthierr,kT,kTloerr,kThierr,n_e,n_eloerr,n_ehierr,s,sloerr,shierr,pres,presloerr,preshierr,U_b_color,L_Halpha,mdot,mdotloerr,mdothierr
s=size(ct)
nplot=s[1]
zerox=intarr(1,nplot)


;
; Define error in the colors
;
U_b_err=dblarr(1,nplot)
for i=0,nplot-1 do begin
   U_b_err[i]=sqrt(2.0*(0.02^2.0))	; error in McNamara O'Connell 1992 in fits to standard stars and my error in reading from the plots
endfor
;U_b_err[1]=0.06	; error in A2124 due to transformation from D4000 to (U-b)Nuc
U_b_err[9]=0.06		; error in Hydra A
U_b_err[4]=0.06		; error in A478
;U_b_err[4]=0.06	; error in A644


;
; Normalize colors by BCG template mean
;
U_b_color=U_b_color-0.77


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
   xtitle='Central cooling time (10!E9!N yr)',$
   ytitle='!9D!X(U-b)!DNuc!N (mag)',$
   xrange=[0.1,1.7],$
   yrange=[0.3,-0.9]
oploterror,ct/1.0E9,U_b_color,ctloerr/1.0E9,U_b_err,/lobar
oploterror,ct/1.0E9,U_b_color,cthierr/1.0E9,U_b_err,/hibar


;
; (2) Plot cooling time vs. L_H-alpha
;
nonzero_L=where(l_halpha gt 0.0)
l_halpha_nonzero=l_halpha(nonzero_L)
plotsym,0,1
ploterror,ct(nonzero_L)/1.0E9,l_halpha_nonzero,zerox,zerox,/ylog,$
   xtitle='Central cooling time (10!E9!N yr)',$
   ytitle='L!DH!9a!X!N (10!E40!N erg s!E-1!N)',$
   xrange=[0.1,1.7],$
   yrange=[0.1,200]
oploterror,ct(nonzero_L)/1.0E9,l_halpha_nonzero,ctloerr(nonzero_L)/1.0E9,zerox,/lobar
oploterror,ct(nonzero_L)/1.0E9,l_halpha_nonzero,cthierr(nonzero_L)/1.0E9,zerox,/hibar
plotsym,1,3
ct_upper_limit=dblarr(1)
l_h_upper_limit=dblarr(1)
ct_upper_limit[0]=1.91670e+08/1.0E9
l_h_upper_limit[0]=0.39
oplot,ct_upper_limit,l_h_upper_limit


;
; (3) Plot cooling time vs. M-dot
nonzero_mdot=where(mdot gt 0.0)
plotsym,0,1
ploterror,ct(nonzero_mdot)/1.0E9,mdot(nonzero_mdot),zerox,zerox,/ylog,$
   xtitle='Central cooling time (10!E9!N yr)',$
   ytitle='Cooling Rate (solar masses yr!E-1!N)',$
   xrange=[0.1,0.7],$
   yrange=[0.1,200]
oploterror,ct(nonzero_mdot)/1.0E9,mdot(nonzero_mdot),ctloerr(nonzero_mdot)/1.0E9,mdotloerr(nonzero_mdot),/lobar
oploterror,ct(nonzero_mdot)/1.0E9,mdot(nonzero_mdot),cthierr(nonzero_mdot)/1.0E9,mdothierr(nonzero_mdot),/hibar
plotsym,1,3
ct_upper_limit=dblarr(1)
m_upper_limit=dblarr(1)
ct_upper_limit[0]=5.45935e+08/1.0E9
m_upper_limit[0]=2
oplot,ct_upper_limit,m_upper_limit


;
; (4) Plot L_H-alpha vs. M-dot
readcol,'l_cool_H_alpha.dat',f='a,d,d,d,d',name,mdot,mdotloerr,mdothierr,l_halpha
s=size(mdot)
nplot=s[1]
zerox=intarr(1,nplot)
plotsym,0,1
ploterror,mdot,l_halpha,zerox,zerox,/xlog,/ylog,$
   ytitle='L!DH!9a!X!N (10!E40!N erg s!E-1!N)',$
   xtitle='Cooling Rate (solar masses yr!E-1!N)',$
   xrange=[0.1,500],$
   yrange=[0.1,500]
oploterror,mdot,l_halpha,mdotloerr,zerox,/lobar
oploterror,mdot,l_halpha,mdothierr,zerox,/hibar
plotsym,6,3
m_upper_limit=dblarr(2)
l_h_upper_limit=dblarr(2)
m_upper_limit[0]=2
m_upper_limit[1]=8
l_h_upper_limit[0]=1.6
l_h_upper_limit[1]=333
oplot,m_upper_limit,l_h_upper_limit



goto,skip_kt
;
; (4) Plot L_H-alpha vs. M-dot
nonzero_both=where( (mdot gt 0.0) and (l_halpha gt 0.0) )
plotsym,0,1
ploterror,mdot(nonzero_both),l_halpha(nonzero_both),zerox,zerox,/xlog,/ylog,$
   ytitle='L!DH!9a!X!N (10!E40!N erg s!E-1!N)',$
   xtitle='Cooling Rate (solar masses yr!E-1!N)',$
   xrange=[0.1,200],$
   yrange=[0.1,200]
oploterror,mdot(nonzero_both),l_halpha(nonzero_both),mdotloerr(nonzero_both),zerox,/lobar
oploterror,mdot(nonzero_both),l_halpha(nonzero_both),mdothierr(nonzero_both),zerox,/hibar



;
; (3) Plot kT decrement vs. color
;
plotsym,0,1
ploterror,kTdecr,U_b_color,zerox,U_b_err,$
   xtitle='!9D!XkT (keV)',$
   ytitle='(U-b)!DNuc!N',$
   xrange=[1.0,8.0],$
   yrange=[1.1,-0.1]
oploterror,kTdecr,U_b_color,kTdecrloerr,U_b_err,/lobar
oploterror,kTdecr,U_b_color,kTdecrhierr,U_b_err,/hibar


;
; (4) Plot kT gradient vs. color
;
plotsym,0,1
ploterror,dkTdr,U_b_color,zerox,U_b_err,$
   xtitle='!9D!XkT/!9D!Xr (keV/kpc)',$
   ytitle='(U-b)!DNuc!N',$
   xrange=[0,0.15],$
   yrange=[1.1,-0.1]
oploterror,dkTdr,U_b_color,dkTdrloerr,U_b_err,/lobar
oploterror,dkTdr,U_b_color,dkTdrhierr,U_b_err,/hibar

skip_kt:
device, /close



;
; Return to IDL
;
return
end
