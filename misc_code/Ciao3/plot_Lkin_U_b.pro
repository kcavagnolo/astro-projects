pro plot_Lkin_U_b
;------------------------------------------------------------------------------
; Name:  PLOT_LKIN_U_B
;
; Purpose:  Plot L_kin versus (U-b) color for the sample
;
;
; Inputs:  
;
;
; Comments:
;
;
; Revision history:
;       written by DAR, 2004-8-27
;------------------------------------------------------------------------------   
;
; Read L_kin data
;
readcol, 'Lkin_A_churazov_g.txt',KAgch,KAgchlo,KAgchhi
readcol, 'Lkin_A_churazov_f.txt',KAfch,KAfchlo,KAfchhi
readcol, 'Lkin_G_churazov_p.txt',KGpch,KGpchlo,KGpchhi
readcol, 'Lkin_G_churazov_g.txt',KGgch,KGgchlo,KGgchhi
readcol, 'Lkin_G_churazov_f.txt',KGfch,KGfchlo,KGfchhi
readcol, 'Lkin_A_s_g.txt',KAgs,KAgslo,KAgshi
readcol, 'Lkin_A_s_f.txt',KAfs,KAfslo,KAfshi
readcol, 'Lkin_G_s_g.txt',KGgs,KGgslo,KGgshi
readcol, 'Lkin_G_s_f.txt',KGfs,KGfslo,KGfshi
readcol, 'Lkin_G_s_p.txt',KGps,KGpslo,KGpshi
readcol, 'Lkin_A_ref_g.txt',KAgrf,KAgrflo,KAgrfhi
readcol, 'Lkin_A_ref_f.txt',KAfrf,KAfrflo,KAfrfhi
readcol, 'Lkin_G_ref_g.txt',KGgrf,KGgrflo,KGgrfhi
readcol, 'Lkin_G_ref_f.txt',KGfrf,KGfrflo,KGfrfhi
readcol, 'Lkin_G_ref_p.txt',KGprf,KGprflo,KGprfhi


;
; Read (U-b) data
;
readcol,'U_b_Ag.txt',U_b_Ag,U_b_Aglo,U_b_Aghi
readcol,'U_b_Af.txt',U_b_Af,U_b_Aflo,U_b_Afhi
readcol,'U_b_Gg.txt',U_b_Gg,U_b_Gglo,U_b_Gghi
readcol,'U_b_Gf.txt',U_b_Gf,U_b_Gflo,U_b_Gfhi
readcol,'U_b_Gp.txt',U_b_Gp,U_b_Gplo,U_b_Gphi


;
; Set plot 1 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='Lkin_vs_U_b.ps',/color,landscape=1
device,/encapsul,xsize=10,ysize=10,set_font='Times-Roman',file='plot3.eps',landscape=0
!p.multi=[0,1,1]
ploterror,U_b_Ag,KAgs,zeroyAgs,psym=3,hatlength=120,/xlog,/ylog,/nohat,$
     position=[0.1,0.1,0.95,0.95],$
     xtickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
     ytickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
     yrange=[0.005,10000],$
     xrange=[0.005,10000],$
;     xrange=[40,60],$
     ytitle='L!Bmech!N (10!U42!N erg/s)',$
     xtitle='L!BX!N-L!Bspec!N (< r!Bcool!N) (10!U42!N erg/s)'

oploterror,U_b_Ag,KAgs,KAgslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Ag,KAgs,KAgshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_Gg,KAgs,KAgslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Gg,KAgs,KAgshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_Af,KAfs,KAfslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Af,KAfs,KAfshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_Gf,KGfs,KGfslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Gf,KGfs,KGfshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_p,KGps,KGpslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_p,KGps,KGpshi,psym=3,errcolor=0,hatlength=120,/HIBAR

oploterror,U_b_Ag,KAgrf,KAgrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Ag,KAgrf,KAgrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_Gg,KAgrf,KAgrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Gg,KAgrf,KAgrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_Af,KAfrf,KAfrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Af,KAfrf,KAfrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_Gf,KGfrf,KGfrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Gf,KGfrf,KGfrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_p,KGprf,KGprflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_p,KGprf,KGprfhi,psym=3,errcolor=0,hatlength=200,/HIBAR	


plotsym,0,0.9,/fill
oploterror,U_b_Ag,KAgch,U_b_Aglo,KAgchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,U_b_Ag,KAgch,U_b_Aghi,KAgchhi,psym=8,errcolor=0,hatlength=300,/HIBAR
plotsym,0,0.9
oploterror,U_b_Gg,KGgch,U_b_Gglo,KGgchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,U_b_Gg,KGgch,U_b_Gghi,KGgchhi,psym=8,errcolor=0,hatlength=300,/HIBAR

plotsym,4,1.2,/fill
oploterror,U_b_Af,KAfch,U_b_Aflo,KAfchlo,psym=8,errcolor=0,hatlength=260,/LOBAR	
oploterror,U_b_Af,KAfch,U_b_Afhi,KAfchhi,psym=8,errcolor=0,hatlength=260,/HIBAR
plotsym,4,1.2
oploterror,U_b_Gf,KGfch,U_b_Gflo,KGfchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,U_b_Gf,KGfch,U_b_Gfhi,KGfchhi,psym=8,errcolor=0,hatlength=300,/HIBAR

plotsym,8,0.9
oploterror,U_b_p,KGpch,U_b_plo,KGpchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,U_b_p,Kgpch,U_b_phi,KGpchhi,psym=8,errcolor=0,hatlength=300,/HIBAR	

device, /close



;
; Return to IDL
;
return
end
