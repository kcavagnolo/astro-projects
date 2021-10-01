pro plot_lumin_color
;------------------------------------------------------------------------------
; Name:  PLOT_LKIN_U_B
;
; Purpose:  Plot L_kin and L_H-alpha versus (U-b) color for the sample
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
; Read Lx and Lspec data
;
readcol, 'Lcoolx_deproj_g_A.txt',LcAg,LcAglo,LcAghi,LxAg,LxAglo,LxAghi
readcol, 'Lcoolx_deproj_g_G.txt',LcGg,LcGglo,LcGghi,LxGg,LxGglo,LxGghi
readcol, 'Lcoolx_deproj_f_A.txt',LcAf,LcAflo,LcAfhi,LxAf,LxAflo,LxAfhi
readcol, 'Lcoolx_deproj_f_G.txt',LcGf,LcGflo,LcGfhi,LxGf,LxGflo,LxGfhi
readcol, 'Lcoolx_deproj_p_G.txt',Lcp,Lcplo,Lcphi,Lxp,Lxplo,Lxphi


;
; Read (U-b) data
;
readcol,'U_b_Ag.txt',U_b_Ag,U_b_Aglo,U_b_Aghi
readcol,'U_b_Af.txt',U_b_Af,U_b_Aflo,U_b_Afhi
readcol,'U_b_Gg.txt',U_b_Gg,U_b_Gglo,U_b_Gghi
readcol,'U_b_Gf.txt',U_b_Gf,U_b_Gflo,U_b_Gfhi
readcol,'U_b_Gp.txt',U_b_Gp,U_b_Gplo,U_b_Gphi

;
; Read L_H-alpha data
;
readcol,'L_halpha_Ag.txt',L_halpha_Ag
readcol,'L_halpha_Af.txt',L_halpha_Af
readcol,'L_halpha_Gg.txt',L_halpha_Gg
readcol,'L_halpha_Gf.txt',L_halpha_Gf
readcol,'L_halpha_Gp.txt',L_halpha_Gp



;
; Normalize to the "template BCGs" of MO92 <(U-b)_Nuc> = 0.77
;
U_b_Ag=U_b_Ag-0.77
U_b_Af=U_b_Af-0.77
U_b_Gg=U_b_Gg-0.77
U_b_Gf=U_b_Gf-0.77
U_b_Gp=U_b_Gp-0.77

sAgs=size(U_b_Ag)
zeroyAgs=intarr(1,sAgs[1])


;
; Set plot 1 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='L_vs_color.ps',/color,landscape=1
;device,/encapsul,xsize=10,ysize=10,set_font='Times-Roman',file='plot3.eps',landscape=0
!p.multi=[0,2,2]
!y.style=1
!x.style=1
ploterror,U_b_Ag,KAgs,zeroyAgs,psym=3,hatlength=120,/ylog,/nohat,$
;    position=[0.1,0.1,0.95,0.95],$
;     xtickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
;     ytickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
     yrange=[0.1,1000],$
     xrange=[0.23,-0.74],$
;     xrange=[40,60],$
     ytitle='L!Bmech!N (10!U42!N erg/s)',$
     xtitle='!9D!X(U-B)!DNuc!N (mag)'

oploterror,U_b_Ag,KAgs,KAgslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Ag,KAgs,KAgshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_Gg,KAgs,KAgslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Gg,KAgs,KAgshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_Af,KAfs,KAfslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Af,KAfs,KAfshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_Gf,KGfs,KGfslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Gf,KGfs,KGfshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,U_b_Gp,KGps,KGpslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,U_b_Gp,KGps,KGpshi,psym=3,errcolor=0,hatlength=120,/HIBAR

oploterror,U_b_Ag,KAgrf,KAgrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Ag,KAgrf,KAgrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_Gg,KAgrf,KAgrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Gg,KAgrf,KAgrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_Af,KAfrf,KAfrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Af,KAfrf,KAfrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_Gf,KGfrf,KGfrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Gf,KGfrf,KGfrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,U_b_Gp,KGprf,KGprflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,U_b_Gp,KGprf,KGprfhi,psym=3,errcolor=0,hatlength=200,/HIBAR	


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
oploterror,U_b_Gp,KGpch,U_b_Gplo,KGpchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,U_b_Gp,Kgpch,U_b_Gphi,KGpchhi,psym=8,errcolor=0,hatlength=300,/HIBAR	



ploterror,LxAg,U_b_Ag,zeroyAgs,psym=3,hatlength=120,/xlog,/nohat,$
;     position=[0.1,0.1,0.95,0.95],$
;     xtickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
;     ytickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
     yrange=[0.3,-0.9],$
     xrange=[5,1750],$
;     xrange=[40,60],$
     ytitle='!9D!X(U-B)!DNuc!N (mag)',$
     xtitle='L!BX!N-L!Bspec!N (< r!Bcool!N) (10!U42!N erg/s)'
     LxcAglo=sqrt(LxAglo^2+LcAghi^2)
     LxcAghi=sqrt(LxAghi^2+LcAglo^2)
     LxcGglo=sqrt(LxGglo^2+LcGghi^2)
     LxcGghi=sqrt(LxGghi^2+LcGglo^2)
     LxcAflo=sqrt(LxAflo^2+LcAfhi^2)
     LxcAfhi=sqrt(LxAfhi^2+LcAflo^2)
     LxcGflo=sqrt(LxGflo^2+LcGfhi^2)
     LxcGfhi=sqrt(LxGfhi^2+LcGflo^2)
     Lxcplo=sqrt(Lxplo^2+Lcphi^2)
     Lxcphi=sqrt(Lxphi^2+Lcplo^2)
oploterror,LxAg-LcAg,U_b_Ag,U_b_Aglo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LxAg-LcAg,U_b_Ag,U_b_Aghi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,LxGg-LcGg,U_b_Gg,U_b_Gglo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LxGg-LcGg,U_b_Gg,U_b_Gghi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,LxAf-LcAf,U_b_Af,U_b_Aflo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LxAf-LcAf,U_b_Af,U_b_Afhi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,LxGf-LcGf,U_b_Gf,U_b_Gflo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LxGf-LcGf,U_b_Gf,U_b_Gfhi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,Lxp-Lcp,U_b_Gp,U_b_Gplo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,Lxp-Lcp,U_b_Gp,U_b_Gphi,psym=3,errcolor=0,hatlength=120,/HIBAR
;oploterror,Lxcon-Lccon,Kcon,Kconhi,psym=3,errcolor=0,hatlength=120,errstyle=0,/hibar,/nohat

plotsym,0,1
oploterror,LxAg-LcAg,U_b_Ag,LxcAglo,U_b_Aglo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,LxAg-LcAg,U_b_Ag,LxcAghi,U_b_Aghi,psym=8,errcolor=0,hatlength=300,/HIBAR

oploterror,LxGg-LcGg,U_b_Gg,LxcGglo,U_b_Gglo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,LxGg-LcGg,U_b_Gg,LxcGghi,U_b_Gghi,psym=8,errcolor=0,hatlength=300,/HIBAR


oploterror,LxAf-LcAf,U_b_Af,LxcAflo,U_b_Aflo,psym=8,errcolor=0,hatlength=260,/LOBAR	
oploterror,LxAf-LcAf,U_b_Af,LxcAfhi,U_b_Afhi,psym=8,errcolor=0,hatlength=260,/HIBAR

oploterror,LxGf-LcGf,U_b_Gf,LxcGflo,U_b_Gflo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,LxGf-LcGf,U_b_Gf,LxcGfhi,U_b_Gfhi,psym=8,errcolor=0,hatlength=300,/HIBAR


oploterror,Lxp-Lcp,U_b_Gp,Lxcplo,U_b_Gplo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,Lxp-Lcp,U_b_Gp,Lxcphi,U_b_Gphi,psym=8,errcolor=0,hatlength=300,/HIBAR	


ploterror,L_halpha_Ag/100,KAgs,zeroyAgs,psym=3,hatlength=120,/xlog,/ylog,/nohat,$
;    position=[0.1,0.1,0.95,0.95],$
;     xtickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
;     ytickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
     yrange=[0.1,1000],$
     xrange=[0.001,10],$
;     xrange=[40,60],$
     ytitle='L!Bmech!N (10!U42!N erg/s)',$
     xtitle='L!DH!9a!X!N (10!U42!N erg/s)'

oploterror,L_halpha_Ag/100,KAgs,KAgslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,L_halpha_Ag/100,KAgs,KAgshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,L_halpha_Gg/100,KAgs,KAgslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,L_halpha_Gg/100,KAgs,KAgshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,L_halpha_Af/100,KAfs,KAfslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,L_halpha_Af/100,KAfs,KAfshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,L_halpha_Gf/100,KGfs,KGfslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,L_halpha_Gf/100,KGfs,KGfshi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,L_halpha_Gp/100,KGps,KGpslo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,L_halpha_Gp/100,KGps,KGpshi,psym=3,errcolor=0,hatlength=120,/HIBAR

oploterror,L_halpha_Ag/100,KAgrf,KAgrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,L_halpha_Ag/100,KAgrf,KAgrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,L_halpha_Gg/100,KAgrf,KAgrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,L_halpha_Gg/100,KAgrf,KAgrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,L_halpha_Af/100,KAfrf,KAfrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,L_halpha_Af/100,KAfrf,KAfrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,L_halpha_Gf/100,KGfrf,KGfrflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,L_halpha_Gf/100,KGfrf,KGfrfhi,psym=3,errcolor=0,hatlength=200,/HIBAR
oploterror,L_halpha_Gp/100,KGprf,KGprflo,psym=3,errcolor=0,hatlength=200,/LOBAR	
oploterror,L_halpha_Gp/100,KGprf,KGprfhi,psym=3,errcolor=0,hatlength=200,/HIBAR	


plotsym,0,0.9,/fill
oploterror,L_halpha_Ag/100,KAgch,KAgchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,L_halpha_Ag/100,KAgch,KAgchhi,psym=8,errcolor=0,hatlength=300,/HIBAR
plotsym,0,0.9
oploterror,L_halpha_Gg/100,KGgch,KGgchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,L_halpha_Gg/100,KGgch,KGgchhi,psym=8,errcolor=0,hatlength=300,/HIBAR

plotsym,4,1.2,/fill
oploterror,L_halpha_Af/100,KAfch,KAfchlo,psym=8,errcolor=0,hatlength=260,/LOBAR	
oploterror,L_halpha_Af/100,KAfch,KAfchhi,psym=8,errcolor=0,hatlength=260,/HIBAR
plotsym,4,1.2
oploterror,L_halpha_Gf/100,KGfch,KGfchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,L_halpha_Gf/100,KGfch,KGfchhi,psym=8,errcolor=0,hatlength=300,/HIBAR

plotsym,8,0.9
oploterror,L_halpha_Gp/100,KGpch,KGpchlo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,L_halpha_Gp/100,Kgpch,KGpchhi,psym=8,errcolor=0,hatlength=300,/HIBAR	


ploterror,LcAg,U_b_Ag,zeroyAgs,psym=3,hatlength=120,/xlog,/nohat,$
;     position=[0.1,0.1,0.95,0.95],$
;     xtickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
;     ytickname=['10!U-2!N','10!U-1!N','10!U0!N','10!U1!N','10!U2!N','10!U3!N',' '],$
     yrange=[0.3,-0.9],$
     xrange=[0.1,300],$
;     xrange=[40,60],$
     ytitle='!9D!X(U-B)!DNuc!N (mag)',$
     xtitle='L!Bspec!N (< r!Bcool!N) (10!U42!N erg/s)'
     LxcAglo=sqrt(LxAglo^2+LcAghi^2)
     LxcAghi=sqrt(LxAghi^2+LcAglo^2)
     LxcGglo=sqrt(LxGglo^2+LcGghi^2)
     LxcGghi=sqrt(LxGghi^2+LcGglo^2)
     LxcAflo=sqrt(LxAflo^2+LcAfhi^2)
     LxcAfhi=sqrt(LxAfhi^2+LcAflo^2)
     LxcGflo=sqrt(LxGflo^2+LcGfhi^2)
     LxcGfhi=sqrt(LxGfhi^2+LcGflo^2)
     Lxcplo=sqrt(Lxplo^2+Lcphi^2)
     Lxcphi=sqrt(Lxphi^2+Lcplo^2)
oploterror,LcAg,U_b_Ag,U_b_Aglo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LcAg,U_b_Ag,U_b_Aghi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,LcGg,U_b_Gg,U_b_Gglo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LcGg,U_b_Gg,U_b_Gghi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,LcAf,U_b_Af,U_b_Aflo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LcAf,U_b_Af,U_b_Afhi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,LcGf,U_b_Gf,U_b_Gflo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,LcGf,U_b_Gf,U_b_Gfhi,psym=3,errcolor=0,hatlength=120,/HIBAR
oploterror,Lcp,U_b_Gp,U_b_Gplo,psym=3,errcolor=0,hatlength=120,/LOBAR	
oploterror,Lcp,U_b_Gp,U_b_Gphi,psym=3,errcolor=0,hatlength=120,/HIBAR
;oploterror,Lxcon-Lccon,Kcon,Kconhi,psym=3,errcolor=0,hatlength=120,errstyle=0,/hibar,/nohat

plotsym,0,1
oploterror,LcAg,U_b_Ag,LcAglo,U_b_Aglo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,LcAg,U_b_Ag,LcAghi,U_b_Aghi,psym=8,errcolor=0,hatlength=300,/HIBAR

oploterror,LcGg,U_b_Gg,LcGglo,U_b_Gglo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,LcGg,U_b_Gg,LcGghi,U_b_Gghi,psym=8,errcolor=0,hatlength=300,/HIBAR


oploterror,LcAf,U_b_Af,LcAflo,U_b_Aflo,psym=8,errcolor=0,hatlength=260,/LOBAR	
oploterror,LcAf,U_b_Af,LcAfhi,U_b_Afhi,psym=8,errcolor=0,hatlength=260,/HIBAR

oploterror,LcGf,U_b_Gf,LcGflo,U_b_Gflo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,LcGf,U_b_Gf,LcGfhi,U_b_Gfhi,psym=8,errcolor=0,hatlength=300,/HIBAR


oploterror,Lcp,U_b_Gp,Lcplo,U_b_Gplo,psym=8,errcolor=0,hatlength=300,/LOBAR	
oploterror,Lcp,U_b_Gp,Lcphi,U_b_Gphi,psym=8,errcolor=0,hatlength=300,/HIBAR	



device, /close



;
; Return to IDL
;
return
end
