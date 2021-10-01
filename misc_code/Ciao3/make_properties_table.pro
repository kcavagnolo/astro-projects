pro make_properties_table,deprojected=deprojected,projected=projected, $
	                 cooling=cooling,outfile_root=outfile_root,outfile_dir=outfile_dir
;-----------------------------------------------------------------------
;
; Name: MAKE_PROPERTIES_TABLE.
;
; Purpose: Reads cluster properties for each cluster on the list 
;          
; Inputs: none (defaults to /DEPROJECTED)
;
; Optional inputs: outfile_dir - name of output directory
;				(default is "/home/rafferty/System_props/')
;		   outfile_root - name of output file root
;				(default is "system_props_deproj")
;		 
; Keywords: /DEPROJECTED - reads deprojected properties from
;			   "cluster_properties_deproj.dat" 
;	    /PROJECTED - reads deprojected properties from
;			 "cluster_properties_proj.dat" 
;	    /COOLING - reads deprojected properties from
;		       "cluster_properties_cooling.dat" 
;
; Comments: /PROJECTED and /COOLING keywords not yet implemented
;           
; Revision history:
;       written by DAR, 2004-8-7
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 0) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'make_properties_table [, /PROJECTED, /DEPROJECTED, /COOLING, outfile_root=outfile_root]'
   return   
endif
if (keyword_set(projected) or keyword_set(deprojected) or keyword_set(cooling)) then begin
endif else begin
   deprojected=1
endelse


;
; Set Defaults
;
if (n_elements(outfile_root) eq 0) then outfile_root='system_props_deproj'
if (n_elements(outfile_dir) eq 0) then outfile_dir='/home/rafferty/System_props/'
table_file=outfile_dir+outfile_root+'.tex'
data_file=outfile_dir+outfile_root+'.dat'


;
; Define list of systems
;
n_sys=100
system_dir=strarr(n_sys)
system_name=strarr(n_sys)
system_mdot=dblarr(n_sys)
system_mdotloerr=dblarr(n_sys)
system_mdothierr=dblarr(n_sys)
system_L_Halpha=dblarr(n_sys)
system_color_U_b=dblarr(n_sys)

for i=0,n_sys-1 do begin
  system_dir[i]='none'
endfor

system_dir[0]='/export/home/Rafferty/Data/A85'
system_dir[1]='/export/home/Rafferty/Data/A2124'
system_dir[2]='/export/home/Rafferty/Data/MS0735/Chandra/4197'
system_dir[3]='/export/home/Rafferty/Data/A2597/Obs922'
system_dir[4]='/export/home/Rafferty/Data/HCG62/Obs921'
system_dir[5]='/export/home/Rafferty/Data/2A0335+096/Obs919'
system_dir[6]='/export/home/Rafferty/Data/A133'
system_dir[7]='/export/home/Rafferty/Data/MKW3S'
system_dir[8]='/export/home/Rafferty/Data/Centaurus'
system_dir[9]='/export/home/Rafferty/Data/A478'
system_dir[10]='/export/home/Rafferty/Data/A262'
system_dir[11]='/export/home/Rafferty/Data/A1795'
system_dir[12]='/export/home/Rafferty/Data/A2199'
;system_dir[13]='/export/home/Rafferty/Data/Perseus'
;system_dir[14]='/export/home/Rafferty/Data/PKS0745'
system_dir[15]='/export/home/Rafferty/Data/A496'
;system_dir[16]='/export/home/Rafferty/Data/M87'
system_dir[17]='/export/home/Rafferty/Data/A2029'
;system_dir[18]='/export/home/Rafferty/Data/A2052'
system_dir[19]='/export/home/Rafferty/Data/A2626'
system_dir[20]='/export/home/Rafferty/Data/MKW4'
system_dir[21]='/export/home/Rafferty/Data/A644'
system_dir[22]='/export/home/Rafferty/Data/HydraA'
system_dir[23]='/export/home/Rafferty/Data/A1991'
system_dir[24]='/export/home/Rafferty/Data/A4059'
system_dir[25]='/export/home/Rafferty/Data/A1689/Chandra'
system_dir[26]='/export/home/Rafferty/Data/Coma/Chandra'
system_dir[27]='/export/home/Rafferty/Data/A754/Chandra'
system_dir[28]='/export/home/Rafferty/Data/A773/Chandra/5006'
;system_dir[29]='/export/home/Rafferty/Data/RBS797'
system_dir[30]='/export/home/Rafferty/Data/Zw2701/Chandra'
system_dir[31]='/export/home/Rafferty/Data/A907/Chandra/3185'
system_dir[32]='/export/home/Rafferty/Data/Zw3146/Chandra/909'
system_dir[33]='/export/home/Rafferty/Data/A963/Chandra'
system_dir[34]='/export/home/Rafferty/Data/A1060/Chandra/2220'
system_dir[35]='/export/home/Rafferty/Data/A1068/Chandra/1652'
system_dir[36]='/export/home/Rafferty/Data/A1204/Chandra'
system_dir[37]='/export/home/Rafferty/Data/A1650/Chandra'
system_dir[38]='/export/home/Rafferty/Data/Zw1358/Chandra/516'
system_dir[39]='/export/home/Rafferty/Data/A1835/Chandra'
system_dir[40]='/export/home/Rafferty/Data/MS1455/Chandra/4192'
system_dir[41]='/export/home/Rafferty/Data/A2065/Chandra'
system_dir[42]='/export/home/Rafferty/Data/A2218/Chandra'
system_dir[43]='/export/home/Rafferty/Data/A2244/Chandra'

system_name[0]='A85'
system_name[1]='A2124'
system_name[2]='MS07'
system_name[3]='A2597'
system_name[4]='HCG62'
system_name[5]='2A0335'
system_name[6]='A133'
system_name[7]='MKW3S'
system_name[8]='Centaurus'
system_name[9]='A478'
system_name[10]='A262'
system_name[11]='A1795'
system_name[12]='A2199'
system_name[13]='Perseus'
system_name[14]='PKS0745'
system_name[15]='A496'
system_name[16]='M87'
system_name[17]='A2029'
system_name[18]='A2052'
system_name[19]='A2626'
system_name[20]='MKW4'
system_name[21]='A644'
system_name[22]='HydraA'
system_name[23]='A1991'
system_name[24]='A4059'
system_name[25]='A1689'
system_name[26]='Coma'
system_name[27]='A754'
system_name[28]='A773'
system_name[29]='RBS797'
system_name[30]='Zw2701'
system_name[31]='A907'
system_name[32]='Zw3146'
system_name[33]='A963'
system_name[34]='A1060'
system_name[35]='A1068'
system_name[36]='A1204'
system_name[37]='A1650'
system_name[38]='Zw1358'
system_name[39]='A1835'
system_name[40]='MS1455'
system_name[41]='A2065'
system_name[42]='A2218'
system_name[43]='A2244'


; M_dot in solar masses yr^-1
system_mdot[0]=-1
system_mdot[1]=-1
system_mdot[2]=-1
system_mdot[3]=59
system_mdotloerr[3]=40
system_mdothierr[3]=40
system_mdot[4]=1.1
system_mdotloerr[4]=0.2
system_mdothierr[4]=0.3
system_mdot[5]=120
system_mdotloerr[5]=30
system_mdothierr[5]=30
system_mdot[6]=25
system_mdotloerr[6]=6
system_mdothierr[6]=6
system_mdot[7]=2
system_mdot[8]=10.2
system_mdotloerr[8]=1.2
system_mdothierr[8]=1.5
system_mdot[9]=150
system_mdotloerr[9]=68
system_mdothierr[9]=60
system_mdot[10]=1.5
system_mdotloerr[10]=0.4
system_mdothierr[10]=0.7
system_mdot[11]=18
system_mdotloerr[11]=10
system_mdothierr[11]=12
system_mdot[12]=2.0
system_mdotloerr[12]=1.9
system_mdothierr[12]=7.0
system_mdot[13]=54
system_mdotloerr[13]=18
system_mdothierr[13]=48
system_mdot[14]=-1
system_mdot[15]=-1
system_mdot[16]=1.8
system_mdotloerr[16]=0.6
system_mdothierr[16]=1.2
system_mdot[17]=-1
system_mdot[18]=12
system_mdotloerr[18]=3
system_mdothierr[18]=4
system_mdot[19]=-1
system_mdot[20]=-1
system_mdot[21]=-1
system_mdot[22]=14
system_mdotloerr[22]=7
system_mdothierr[22]=9
system_mdot[23]=-1
system_mdot[24]=6.7
system_mdotloerr[24]=4.1
system_mdothierr[24]=8.5
system_mdot[25]=-1
system_mdot[26]=-1
system_mdot[27]=-1
system_mdot[28]=-1
system_mdot[29]=-1
system_mdot[30]=-1
system_mdot[31]=-1
system_mdot[32]=-1
system_mdot[33]=-1
system_mdot[34]=-1
system_mdot[35]=-1
system_mdot[36]=-1
system_mdot[37]=-1
system_mdot[38]=-1
system_mdot[39]=-1
system_mdot[40]=-1
system_mdot[41]=-1
system_mdot[42]=-1
system_mdot[43]=-1


; L_Halpa in 10^40 erg s^-1
system_L_Halpha[0]=0.92
system_L_Halpha[1]=-1
system_L_Halpha[2]=100
system_L_Halpha[3]=147
system_L_Halpha[4]=-1
system_L_Halpha[5]=57
system_L_Halpha[6]=0.39
system_L_Halpha[7]=1.6
system_L_Halpha[8]=-1
system_L_Halpha[9]=10.8
system_L_Halpha[10]=1.14
system_L_Halpha[11]=50.5
system_L_Halpha[12]=1.26
system_L_Halpha[13]=241
system_L_Halpha[14]=149
system_L_Halpha[15]=1.7
system_L_Halpha[16]=0.55
system_L_Halpha[17]=0.39
system_L_Halpha[18]=1.95
system_L_Halpha[19]=1.6
system_L_Halpha[20]=-1
system_L_Halpha[21]=0.32
system_L_Halpha[22]=8.3
system_L_Halpha[23]=1.5
system_L_Halpha[24]=-1
system_L_Halpha[25]=-1
system_L_Halpha[26]=-1
system_L_Halpha[27]=-1
system_L_Halpha[28]=-1
system_L_Halpha[29]=-1
system_L_Halpha[30]=-1
system_L_Halpha[31]=-1
system_L_Halpha[32]=-1
system_L_Halpha[33]=-1
system_L_Halpha[34]=-1
system_L_Halpha[35]=-1
system_L_Halpha[36]=-1
system_L_Halpha[37]=-1
system_L_Halpha[38]=-1
system_L_Halpha[39]=-1
system_L_Halpha[40]=-1
system_L_Halpha[41]=-1
system_L_Halpha[42]=-1
system_L_Halpha[43]=-1




;goto,skip_to_avg_all
; McNamara & O'Connell or average of Cardiel and Johnstone from D4000
system_color_U_b[0]=0.63
system_color_U_b[1]=0.75
system_color_U_b[2]=-1
system_color_U_b[3]=0.18
system_color_U_b[4]=-1
system_color_U_b[5]=0.72
system_color_U_b[6]=-1
system_color_U_b[7]=0.73
system_color_U_b[8]=-1
system_color_U_b[9]=0.47
system_color_U_b[10]=0.96
system_color_U_b[11]=0.32
system_color_U_b[12]=0.68
system_color_U_b[13]=0.0
system_color_U_b[14]=0.1
system_color_U_b[15]=0.75
system_color_U_b[16]=0.56
system_color_U_b[17]=0.75
system_color_U_b[18]=0.58
system_color_U_b[19]=0.76
system_color_U_b[20]=0.75
system_color_U_b[21]=0.81
system_color_U_b[22]=0.11
system_color_U_b[23]=0.66
system_color_U_b[24]=-1
system_color_U_b[25]=-1
system_color_U_b[26]=-1
system_color_U_b[27]=-1
system_color_U_b[28]=-1


skip_to_avg_all:
; Average of all available data from D4000
system_color_U_b[0]=0.67
system_color_U_b[1]=0.75
system_color_U_b[2]=-1
system_color_U_b[3]=0.24
system_color_U_b[4]=-1
system_color_U_b[5]=0.70
system_color_U_b[6]=-1
system_color_U_b[7]=0.73
system_color_U_b[8]=-1
system_color_U_b[9]=0.47
system_color_U_b[10]=0.93
system_color_U_b[11]=0.30
system_color_U_b[12]=0.69
system_color_U_b[13]=0.0
system_color_U_b[14]=0.06
system_color_U_b[15]=0.79
system_color_U_b[16]=0.56
system_color_U_b[17]=0.74
system_color_U_b[18]=0.58
system_color_U_b[19]=0.72
system_color_U_b[20]=0.75
system_color_U_b[21]=0.81
system_color_U_b[22]=0.11
system_color_U_b[23]=0.67
system_color_U_b[24]=-1
system_color_U_b[25]=-1
system_color_U_b[26]=-1
system_color_U_b[27]=-1
system_color_U_b[28]=-1
system_color_U_b[29]=-1
system_color_U_b[30]=-1
system_color_U_b[31]=-1
system_color_U_b[32]=-1
system_color_U_b[33]=-1
system_color_U_b[34]=-1
system_color_U_b[35]=-1
system_color_U_b[36]=-1
system_color_U_b[37]=-1
system_color_U_b[38]=-1
system_color_U_b[39]=-1
system_color_U_b[40]=-1
system_color_U_b[41]=-1
system_color_U_b[42]=-1
system_color_U_b[43]=-1

skip_avg_all:
;
; Normalize color to non-accreting BCG's
;
system_color_U_b=system_color_U_b-0.77


;
; Create headers of table and data files
;
get_lun,unit
openw,unit,table_file
printf,unit,'%'
printf,unit,'% Table of Cluster Properties'
printf,unit,'%'
printf,unit,' '
printf,unit,'\documentclass{aastex}'
printf,unit,'\begin{document}'
printf,unit,'\begin{deluxetable}{cccccccccc}'
printf,unit,'\tabletypesize{\scriptsize}'
printf,unit,'\tablecolumns{10}'
printf,unit,'\tablewidth{0pc}'
printf,unit,'\tablecaption{System Properties}'
printf,unit,'\tablehead{'
printf,unit,'\colhead{} & \multicolumn{6}{c}{Central Values} & \colhead{} & \colhead{} & \colhead{} \\'
printf,unit,'\cline{2-7} \\'
printf,unit,'\colhead{} & \colhead{$r_{\rm{cent}}$} & \colhead{$t_{\rm{cool}}$} & \colhead{$kT$} & \colhead{$n_{\rm{e}}$} & \colhead{$S$} & \colhead{$P$} & \colhead{$\Delta(U-b)_{\rm{Nuc}}$} & \colhead{$\dot{M}$} & \colhead{$L_{\rm{H}\alpha}$}  \\'
printf,unit,'\colhead{System} & \colhead{(kpc)} & \colhead{(Gyr)} & \colhead{(keV)} & \colhead{(cm$^{-3}$)} & \colhead{(keV cm$^{2}$)} & \colhead{(10$^{-10}$ keV cm$^{-3}$)} & \colhead{(mag)} & \colhead{($M_{\odot}$ yr$^{-1}$)} & \colhead{($10^{40}$ erg s$^{-1}$)} }'
printf,unit,'\startdata'
close,unit
free_lun,unit

get_lun,unit
openw,unit,data_file
printf,unit,'# System Properties
printf,unit,' '
printf,unit,'# (1) System Name'
printf,unit,'# (2) Outer radius of central region [arcsec]'
printf,unit,'# (3) Outer radius of central region [kpc]'
printf,unit,'# (4) Central cooling time [yr]'
printf,unit,'# (5) Central cooling time low error [yr]'
printf,unit,'# (6) Central cooling time high error [yr]'
printf,unit,'# (7) Central kT [keV]'
printf,unit,'# (8) Central kT low error [keV]'
printf,unit,'# (9) Central kT high error [keV]'
printf,unit,'# (10) Central n_e [cm^-3]'
printf,unit,'# (11) Central n_e low error [cm^-3]'
printf,unit,'# (12) Central n_e high error [cm^-3]'
printf,unit,'# (13) Central entropy [keV cm^2]'
printf,unit,'# (14) Central entropy low error [keV cm^2]'
printf,unit,'# (15) Central entropy high error [keV cm^2]'
printf,unit,'# (16) Central pressure [keV cm^-3]'
printf,unit,'# (17) Central pressure low error [keV cm^-3]'
printf,unit,'# (18) Central pressure high error [keV cm^-3]'
printf,unit,'# (19) U-b color (relative to non-accreting BCG of U-b=0.77)'
printf,unit,'# (20) L_H-alpha [10^42 erg s^-1]'
printf,unit,'# (21) M_dot [M_solar/year]'
printf,unit,'# (22) M_dot low error [M_solar/year]'
printf,unit,'# (23) M_dot high error [M_solar/year]'
printf,unit,' '
close,unit
free_lun,unit


;
; Read cluster_properties file for each system
;
;
; If /DEPROJECTED is set, read "cluster_properties_deproj.dat"
;
if keyword_set(deprojected) then begin
   for i=0,n_sys-1 do begin
      if (system_dir[i] ne 'none') then begin
         print,' '
         print,'Now reading central properties for '+system_name[i]+'...'
         cd,system_dir[i]
         readcol,'cluster_properties_deproj.dat',skipline=20,r_cent_limit,r_cent_limit_kpc, $
                 ct_cent,ct_cent_lo,ct_cent_hi,kT_cent,kT_cent_lo,kT_cent_hi, $
                 n_e_cent,n_e_cent_lo,n_e_cent_hi,s_cent,s_cent_lo,s_cent_hi,pres_cent,pres_cent_lo,pres_cent_hi
         
                 
         ;
         ; Write to table file
         ;
         fmt1='$(a12,a3,f7.2,a3,f4.2,a3,f6.2)
         fmt2='$(a3,f5.3,a3,f5.1)
         fmt3='$(a3,f5.2,a3,f5.2)
         fmt3a='$(a3,f5.2,a11)
         fmt4='$(a3,f8.3,a3,f8.3,a3)'
         fmt4a='$(a12,f8.3,a3)'
         fmt4b='$(a3,f8.3,a13)'
         get_lun,unit
         openw,unit,table_file,/append
         printf,unit,fmt1,system_name[i],' & ',r_cent_limit_kpc,' & ',ct_cent/1e9,' & ',kT_cent
         printf,unit,fmt2,'& ',n_e_cent,' & ',s_cent
         if (system_color_U_b[i] eq -1.77) then begin
            printf,unit,fmt3a,'& ',pres_cent/1e-10,' & \nodata '
         endif else begin
            printf,unit,fmt3,'& ',pres_cent/1e-10,' & ',system_color_U_b[i]
         endelse
         if ((system_mdot[i] eq -1) and (system_L_Halpha[i] eq -1)) then begin
            printf,unit,'& \nodata & \nodata \\'
         endif else begin
            if (system_mdot[i] eq -1) then begin
               printf,unit,fmt4a,'& \nodata & ',system_L_Halpha[i],' \\'
            endif         
            if (system_L_Halpha[i] eq -1) then begin
               printf,unit,fmt4b,'& ',system_mdot[i],' & \nodata \\'
            endif 
            if ((system_mdot[i] ne -1) and (system_L_Halpha[i] ne -1)) then begin
               printf,unit,fmt4,'& ',system_mdot[i],' & ',system_L_Halpha[i],' \\'
            endif
         endelse
         close,unit
         free_lun,unit
         
         
         ;
         ; Write to data file
         ;
         get_lun,unit
         openw,unit,data_file,/append
         fmt='$(a10,2(3x,f9.3),3(3x,e13.5),6(3x,f9.4),3(3x,f9.4),3(3x,e13.5),5(3x,f9.4))'
         printf,unit,fmt,system_name[i],r_cent_limit,r_cent_limit_kpc,ct_cent,ct_cent_lo,ct_cent_hi,kT_cent,kT_cent_lo,kT_cent_hi, $
         n_e_cent,n_e_cent_lo,n_e_cent_hi,s_cent,s_cent_lo,s_cent_hi,pres_cent,pres_cent_lo,pres_cent_hi, $
         system_color_U_b[i],system_L_Halpha[i],system_mdot[i],system_mdotloerr[i],system_mdothierr[i]
         close,unit
         free_lun,unit 
                 
                 
         print,'...done.'
      endif
   endfor
endif


;
; If /PROJECTED is set, read "cluster_properties_proj.dat"
;
if keyword_set(projected) then begin
endif


;
; If /COOLING is set, read "cluster_properties_cooling.dat"
;
if keyword_set(cooling) then begin
endif


;
; Add ending material to table_file
;
get_lun,unit
openw,unit,table_file,/append
printf,unit,'\enddata'
printf,unit,'\end{deluxetable}'
printf,unit,'\end{document}'
close,unit
free_lun,unit 


;
; Call latex
;
cd,outfile_dir
cmdstring='/plato2/local/teTeX/bin/latex '+outfile_root+'.tex'
spawn,cmdstring


;
; Call dvips
;
cmdstring='dvips -t letter -o '+outfile_root+'.ps '+outfile_root+'.dvi'
spawn,cmdstring


;
; Summarize
;
print,' '
print,'MAKE_PROPERTIES_TABLE complete.'
print,'Table file written to '+table_file
print,'Data file written to '+data_file
print,'Postscript file is '+outfile_dir+outfile_root+'.ps'


;
; Return
;
return
end