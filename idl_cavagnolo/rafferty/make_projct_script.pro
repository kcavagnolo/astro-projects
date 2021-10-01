pro make_projct_script,nregions,nH_gal,kT_guess,ab_guess, $
          redshift,lo_energy,hi_energy,bin,inner_a,inner_b, $
          inner_pa,old_pipe=old_pipe,central=central,noflux=noflux
;-----------------------------------------------------------------------
;
; Name: MAKE_PROJCT_SCRIPT
;
; Purpose: Creates an XSPEC script to deproject the spectra extracted by
;	   EXTRACT_ANNULI with the projct*wabs*mekal model.
;          
;          
; Inputs:  Initial guesses for the parameters, energy range, and binning
;
;         
; Comments: Assumes standard pipeline naming conventions are followed
;           
;           
; Revision history:
;       written by DR, 2004-6-29
;-----------------------------------------------------------------------
binning=fix(bin)
nreg=fix(nregions)


;
; Open script file
;
get_lun,unit
openw,unit,'projct_wabs_mekal.xcm'
printf,unit,'# XSPEC script for PROJCT*MEKAL*WABS.'
printf,unit,'# To be run in the spectra directory.'
printf,unit,' '


;
; Set cosmology
;
printf,unit,'# set cosmology (H0 q0 Lamda0). NOTE: value of q0 ignored if Lamda0 is nonzero.'
printf,unit,'cosmo 70 0 0.7'
printf,unit,' '


;
; Write data reading and filtering code
;
printf,unit,'# read in data files'
if keyword_set(central) then begin
   for i=1,2 do begin
      if (binning eq 1) then specfile='central_reg'+strtrim(string(i),2)+'_sou.pi' else specfile='central_reg'+strtrim(string(i),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
      printf,unit,'data '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' '+specfile
      printf,unit,'ignore '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' 0.0-'+strtrim(string(lo_energy),2)+' '+strtrim(string(hi_energy),2)+'-100.0'
   endfor
   if keyword_set(old_pipe) then begin
      for i=3,nreg do begin
          if (binning eq 1) then specfile='deproj_reg'+strtrim(string(i),2)+'.pi' else specfile='deproj_reg'+strtrim(string(i),2)+'_g'+strtrim(string(binning),2)+'.pi'
          printf,unit,'data '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' '+specfile
          printf,unit,'ignore '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' 0.0-'+strtrim(string(lo_energy),2)+' '+strtrim(string(hi_energy),2)+'-100.0'
      endfor
   endif else begin
      for i=3,nreg do begin
         if (binning eq 1) then specfile='reg'+strtrim(string(i),2)+'_sou.pi' else specfile='reg'+strtrim(string(i),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
         printf,unit,'data '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' '+specfile
         printf,unit,'ignore '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' 0.0-'+strtrim(string(lo_energy),2)+' '+strtrim(string(hi_energy),2)+'-100.0'
      endfor
   endelse
endif else begin
   for i=1,nreg do begin
       if (binning eq 1) then specfile='reg'+strtrim(string(i),2)+'_sou.pi' else specfile='reg'+strtrim(string(i),2)+'_sou_g'+strtrim(string(binning),2)+'.pi'
       printf,unit,'data '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' '+specfile
       printf,unit,'ignore '+strtrim(string(i),2)+':'+strtrim(string(i),2)+' 0.0-'+strtrim(string(lo_energy),2)+' '+strtrim(string(hi_energy),2)+'-100.0'
   endfor
endelse


;
; Write preamble
;
printf,unit,' '
printf,unit,'set model_name "projct_wabs_mekal"'
printf,unit,'set xs_return_result 1'
printf,unit,'set tcl_precision 12'
printf,unit,' '
printf,unit,'# USER may change command echoing,'
printf,unit,'# it is a boolean value, can be 0 or 1  '
printf,unit,'# useful for debugging'
printf,unit,'set Echo_Mode 1'
printf,unit,'set xs_echo_script $Echo_Mode'
printf,unit,' '
printf,unit,'# USER may change the chatter level,'
printf,unit,'# ranges from 0 to 25'
printf,unit,'# useful for debugging '
printf,unit,'set Chatter_Level 10'
printf,unit,'chatter $Chatter_Level'
printf,unit,' '


;
; Set grid mode
;
printf,unit,'# USER may activate grid mode (set it to 1)'
printf,unit,'# Default is not to use grid (set Grid_Mode 0)'
printf,unit,'# Grid mode gives ability to fit model'
printf,unit,'# few times using the different set of'
printf,unit,'# initial parameters. The "best" model will be'
printf,unit,'# choosen based on a simple criterion'
printf,unit,'# of the lowest chi value. '
grid_mode='0'
printf,unit,'set Grid_Mode '+grid_mode
printf,unit,' '


;
; Set initial parameters
;
printf,unit,'# USER may change initial parameters'
printf,unit,'set Abundance '+strtrim(string(ab_guess),2)
printf,unit,'set Redshift '+strtrim(string(redshift),2)
printf,unit,'if {$Grid_Mode == 0} {'
printf,unit,'  set kT0 '+strtrim(string(kT_guess),2)
printf,unit,'  set Nh0 '+strtrim(string(nH_gal),2)
printf,unit,'} else {'
printf,unit,'# specify the grid only if you set Grid_Mode 1'
printf,unit,'  set kT0 {1.5 1.0 0.5 3. 5.}'
printf,unit,'  set Nh0 {1. 0.5 0.1 5. 10.} '
printf,unit,'}'
printf,unit,' '


;
; Set fit statistic
;
printf,unit,'# USER may change the weighting of chi statistics.'
printf,unit,'# It is an alternative approach'
printf,unit,'# for small number of counts. Continue to'
printf,unit,'# use chi but change weighting for error estimate.'
printf,unit,'# Available methods are:'
printf,unit,'# "standard", "gehrels", "churazov", "model" '
printf,unit,'set Weighting_Method "standard"'
printf,unit,'weight $Weighting_Method'
printf,unit,' '


;
; Set confidence level to 1 sigma
;
printf,unit,'# USER may change conf. level (in sigma units)'
printf,unit,'# for calculating parameter errors'
printf,unit,'# Right now it is 1.0 sigma (always use .)'
printf,unit,'set Conf_Level_Par 1.0'
printf,unit,' '

;
; Set low and high energies for flux calculation
;
printf,unit,'# USER may change EbandLo, EbandHi, FluxName,'
printf,unit,'# Conf_Level_Flux (in units of %), and Number_Runs'
printf,unit,'# for calculating fluxes and their errors over interested'
printf,unit,'# energy bands. Right now 2 bands are used: '
printf,unit,'# X-Ray band: (0.5-7) and Bolometric: (0.001-100).'
printf,unit,'# Right now 1 sigma  (68%) errors are calculated.'
printf,unit,'set EbandLo {0.5 0.001}'
printf,unit,'set EbandHi {7.0 100.0}'
printf,unit,'# Notice two types of flux will be calculated, the second'
printf,unit,'# one (marked with c) is corrected for column density.'
printf,unit,'# To write spectral results into fits file properly'
printf,unit,'# flux names should not have more than 8 characters.'
printf,unit,'set FluxName {FXray FBol FcXray FcBol}'
printf,unit,'set Conf_Level_Flux 68'
printf,unit,'set Number_Runs 100'
printf,unit,' '
printf,unit,'# set some other vars'
printf,unit,'set model_savfl "$model_name.xspecsav"'
printf,unit,'set fitsfl "source_mekal.spectra"'
printf,unit,'set temp_fitsfl "z_temp.fits"'
printf,unit,'set temp_headfl "z_head.txt"'
printf,unit,'set date_value [ exec date ]'
printf,unit,'setplot energy'
printf,unit,'ignore bad'
printf,unit,' '
printf,unit,'# get D.O.F.'
printf,unit,'tclout noticed 1'
printf,unit,'scan $xspec_tclout "%f%s%f" chnl_low dum chnl_high'
printf,unit,'set number_of_thawprms 3'
printf,unit,'set dof_value [expr '+strtrim(string(nreg),2)+'*($chnl_high - $chnl_low + 1 - $number_of_thawprms)]'
printf,unit,' '
printf,unit,'# check D.O.F'
printf,unit,'if {$dof_value < 1} {'
printf,unit,'  exit'
printf,unit,'} '
printf,unit,' '
printf,unit,'# Implement Grid Method'
printf,unit,'#=================Start Grid section============'
printf,unit,'if {$Grid_Mode} {'
printf,unit,'  set Chi0 100000'
printf,unit,'  set kT0_best [lindex $kT0 0]'
printf,unit,'  set Nh0_best [lindex $Nh0 0]'
printf,unit,'  for {set i 0} {$i < [llength $kT0]} {incr i} {'
printf,unit,'    for {set j 0} {$j < [llength $Nh0]} {incr j} {'
printf,unit,'      model wabs(mekal) & [lindex $Nh0 $j] & [lindex $kT0 $i] & 1 & $Abundance & 0 & 1 & 1 & /*'
printf,unit,'      fit'
printf,unit,'      fit'
printf,unit,'      fit'
printf,unit,'      tclout param 1'
printf,unit,'      scan $xspec_tclout "%f" Nh_result'
printf,unit,'      tclout param 2'
printf,unit,'      scan $xspec_tclout "%f" kT_result '
printf,unit,'      tclout stat'
printf,unit,'      scan $xspec_tclout "%f" chi_squared'
printf,unit,'      set red_chi_squared  [expr $chi_squared/$dof_value]'
printf,unit,'      if {$red_chi_squared < $Chi0} {'
printf,unit,'        set Chi0 $red_chi_squared'
printf,unit,'        set kT0_best [lindex $kT0 $i]'
printf,unit,'        set Nh0_best [lindex $Nh0 $j]'
printf,unit,'      }'
printf,unit,'    }'
printf,unit,'  }'
printf,unit,'  set kT0 $kT0_best'
printf,unit,'  set Nh0 $Nh0_best '
printf,unit,'}'
printf,unit,'#==================End Grid section============='
printf,unit,' '


;
; Define and fit the model
;
printf,unit,'# define and fit model'
printf,unit,'model projct*wabs(mekal)'
for i=1,nreg do begin
   printf,unit,strtrim(string(inner_a),2)
   printf,unit,strtrim(string(inner_b),2)
   printf,unit,strtrim(string(inner_pa),2)
   printf,unit,strtrim(string(nH_gal),2)
   printf,unit,strtrim(string(kT_guess*(1.0+(i-1.0)/(nreg-1.0))),2)
   printf,unit,'1'
   printf,unit,'$Abundance'
   printf,unit,'$Redshift'
   printf,unit,'1'
   printf,unit,'1'
endfor

;untie_kT='untie 5'
;thaw_kT='thaw 5'
;untie_ab='untie 7'
thaw_ab='thaw 7'
;untie_norm='untie 10'
;thaw_norm='thaw 10'
freeze_wabsnH='freeze 4'
;freeze_mekalnH='freeze 6'
;freeze_redshift='freeze 8'
;freeze_switch='freeze 9'
for i=1,nreg-1 do begin
;   untie_kT=untie_kT+','+strtrim(string(5+i*10),2)
;   thaw_kT=thaw_kT+','+strtrim(string(5+i*10),2)
;   untie_ab=untie_ab+','+strtrim(string(7+i*10),2)
   thaw_ab=thaw_ab+','+strtrim(string(7+i*10),2)
;   untie_norm=untie_norm+','+strtrim(string(10+i*10),2)
;   thaw_norm=thaw_norm+','+strtrim(string(10+i*10),2)
   freeze_wabsnH=freeze_wabsnH+','+strtrim(string(4+i*10),2)
;   freeze_mekalnH=freeze_mekalnH+','+strtrim(string(6+i*10),2)
;   freeze_redshift=freeze_redshift+','+strtrim(string(8+i*10),2)
;   freeze_switch=freeze_switch+','+strtrim(string(9+i*10),2)
endfor

;printf,unit,untie_kT
;printf,unit,thaw_kT
;printf,unit,untie_ab
printf,unit,thaw_ab
;printf,unit,untie_norm
;printf,unit,thaw_norm
printf,unit,freeze_wabsnH
;printf,unit,freeze_mekalnH
;printf,unit,freeze_redshift
;printf,unit,freeze_switch

printf,unit,'query yes'
printf,unit,'fit 100000'
printf,unit,'fit'
printf,unit,'fit'
printf,unit,' '


;
; Calculate parameter errors
;
printf,unit,'# calculate parameter errors'
for i=1,nreg do begin
   printf,unit,'error $Conf_Level_Par '+strtrim(string(5+(i-1)*10),2)+' maximum 10.0'
   printf,unit,'error $Conf_Level_Par '+strtrim(string(7+(i-1)*10),2)+' maximum 10.0'
   printf,unit,'error $Conf_Level_Par '+strtrim(string(10+(i-1)*10),2)+' maximum 10.0'
endfor
for i=1,nreg do begin
   printf,unit,'tclout error '+strtrim(string(5+(i-1)*10),2)
   printf,unit,'scan $xspec_tclout "%f %f" p'+strtrim(string(5+(i-1)*10),2)+'_err_low p'+strtrim(string(5+(i-1)*10),2)+'_err_high'
   printf,unit,'tclout error '+strtrim(string(7+(i-1)*10),2)
   printf,unit,'scan $xspec_tclout "%f %f" p'+strtrim(string(7+(i-1)*10),2)+'_err_low p'+strtrim(string(7+(i-1)*10),2)+'_err_high'
   printf,unit,'tclout error '+strtrim(string(10+(i-1)*10),2)
   printf,unit,'scan $xspec_tclout "%f %f" p'+strtrim(string(10+(i-1)*10),2)+'_err_low p'+strtrim(string(10+(i-1)*10),2)+'_err_high'
endfor
printf,unit,' '


;
; Create save file
;
printf,unit,'# create a save file'
printf,unit,'set flag_exists [file exists $model_savfl]'
printf,unit,'if {$flag_exists == 1} {'
printf,unit,'  file delete $model_savfl  '
printf,unit,'}'
printf,unit,'save all $model_savfl'
printf,unit,' '


;
; Get values and errors
;
printf,unit,'# get parameters and statistics'
for i=1,nreg do begin
   printf,unit,'tclout param '+strtrim(string(5+(i-1)*10),2)
   printf,unit,'scan $xspec_tclout "%f" kT'+strtrim(string(i),2)+'_result'
   printf,unit,'tclout param '+strtrim(string(7+(i-1)*10),2)
   printf,unit,'scan $xspec_tclout "%f" ab'+strtrim(string(i),2)+'_result'
   printf,unit,'tclout param '+strtrim(string(10+(i-1)*10),2)
   printf,unit,'scan $xspec_tclout "%f" norm'+strtrim(string(i),2)+'_result'
endfor
printf,unit,'tclout stat'
printf,unit,' '
printf,unit,'scan $xspec_tclout "%f" chi_squared'
printf,unit,'set red_chi_squared [expr $chi_squared/$dof_value]'
printf,unit,'plot ldata delchi'
printf,unit,' '
printf,unit,'# make a postscript image of spectra '
printf,unit,'setplot command FONT Roman'
printf,unit,'setplot command TIME OFF'
printf,unit,'setplot command WIN      1'
printf,unit,'setplot command la to  '
printf,unit,'setplot command LAB  Y  Counts sec\\\u-1 \\\dkeV\\\u-1\\\d'
printf,unit,'setplot command R    Y1 7.E-5 4.'
printf,unit,'setplot command WIN      2'
printf,unit,'#setplot command YAX  3'
;printf,unit,'if {$c_stat_flag == 0} {'
;printf,unit,'  setplot command R    Y2 -3 3'
;printf,unit,'}'
printf,unit,'setplot command LAB  X  Energy \[keV\]'
printf,unit,'setplot command WIN  ALL'
printf,unit,'setplot command v 0.25'
printf,unit,'setplot command r x 0.5 8'
printf,unit,'setplot command lw 3'
printf,unit,'plot'
printf,unit,'cpd /ps'
printf,unit,'plot'
printf,unit,'file copy -force pgplot.ps plot_mekal.ps'
printf,unit,'file delete pgplot.ps'
printf,unit,' '
printf,unit,'# output info to xspec_mekal.log'
printf,unit,'# log none does not actually close the pipe'
printf,unit,'# so use copy'
printf,unit,'log z_temp_xspec.log'
printf,unit,'show all'
printf,unit,'log none'
printf,unit,'file copy -force z_temp_xspec.log xspec_mekal.log'
printf,unit,'file delete z_temp_xspec.log'
printf,unit,' '

if (keyword_set(central) or keyword_set(noflux)) then goto, skip_flux	; skip flux calculation 

printf,unit,'# calculate interested fluxes and errors'
printf,unit,'set BandNum [llength $EbandLo]'
printf,unit,'set Bound_Index 0'
printf,unit,'dummyrsp 0.001 100'
printf,unit,'# retrieve flux info and log.'
printf,unit,'for {set ii 0} {$ii <= [expr $BandNum*2-1]} {incr ii} {'
printf,unit,'  if {$ii == $BandNum} {'
printf,unit,'# USER may add more newpar commands here.  '
printf,unit,'# Correct for NH column density'
printf,unit,'    newpar 4 0'
for i=1,nreg-1 do begin
   printf,unit,'    newpar '+strtrim(string(4+i*10),2)+' 0'
endfor
printf,unit,'  }'
printf,unit,'  if {$ii < $BandNum} {'
printf,unit,'    flux [lindex $EbandLo $ii] [lindex $EbandHi $ii] err $Number_Runs $Conf_Level_Flux'
printf,unit,'  } else {'
printf,unit,'    set iiii [expr $ii-$BandNum]'
printf,unit,'    flux [lindex $EbandLo $iiii] [lindex $EbandHi $iiii] err $Number_Runs $Conf_Level_Flux'
printf,unit,'  }'
for i=1,nreg do begin
   printf,unit,'  tclout flux '+strtrim(string(i),2)
   printf,unit,'  scan $xspec_tclout "%f %f %f" Ftemp'+strtrim(string(i),2)+' Ftemp'+strtrim(string(i),2)+'lo Ftemp'+strtrim(string(i),2)+'hi
endfor
Ftot='$Ftemp1'
Ftotlo='$Ftemp1lo'
Ftothi='$Ftemp1hi'
for i=1,nreg-1 do begin
   Ftot=Ftot+'+$Ftemp'+strtrim(string(i+1),2)
   Ftotlo=Ftotlo+'+$Ftemp'+strtrim(string(i+1),2)+'lo'
   Ftothi=Ftothi+'+$Ftemp'+strtrim(string(i+1),2)+'hi'
endfor
printf,unit,'  set Ftot [expr '+Ftot+']'
printf,unit,'  set Ftotlo [expr '+Ftotlo+']'
printf,unit,'  set Ftothi [expr '+Ftothi+']'
printf,unit,'  eval set [lindex $FluxName $ii] [string trim $Ftot]'
printf,unit,'  eval set [lindex $FluxName $ii]L [string trim $Ftotlo]'
printf,unit,'  eval set [lindex $FluxName $ii]U [string trim $Ftothi]'
printf,unit,'}'

skip_flux:
printf,unit,'# add flux and error info to xspec.log'
printf,unit,'set fileid [open xspec_mekal.log a]'
if (keyword_set(central) or keyword_set(noflux)) then goto, skip_flux_save
printf,unit,'for {set ii 0} {$ii <=[expr $BandNum*2-1]} {incr ii} {'
printf,unit,'  if {$ii < $BandNum} {'
printf,unit,'    set cmd "puts \$fileid \"Flux([lindex $EbandLo $ii]-[lindex $EbandHi $ii]):"  '
printf,unit,'  } else {'
printf,unit,'    set iii [expr $ii-$BandNum]'
printf,unit,'    set cmd "puts \$fileid \"Flux_corr([lindex $EbandLo $iii]-[lindex $EbandHi $iii]):"'
printf,unit,'  }'
printf,unit,'  set cmd "$cmd \$[lindex $FluxName $ii]  1_sigma_conf_range: (\$[lindex $FluxName $ii]L -"'
printf,unit,'  set cmd "$cmd \$[lindex $FluxName $ii]U)\""'
printf,unit,'  eval $cmd  '
printf,unit,'}'

skip_flux_save:
for i=1,nreg do begin
   printf,unit,'puts $fileid "1_sigma_conf_ranges for kT (region '+strtrim(string(i),2)+'):($p'+strtrim(string(5+(i-1)*10),2)+'_err_low - $p'+strtrim(string(5+(i-1)*10),2)+'_err_high)"'
   printf,unit,'puts $fileid "1_sigma_conf_ranges for Abundanc (region '+strtrim(string(i),2)+'):($p'+strtrim(string(7+(i-1)*10),2)+'_err_low - $p'+strtrim(string(7+(i-1)*10),2)+'_err_high)"'
   printf,unit,'puts $fileid "1_sigma_conf_ranges for Norm (region '+strtrim(string(i),2)+'):($p'+strtrim(string(10+(i-1)*10),2)+'_err_low - $p'+strtrim(string(10+(i-1)*10),2)+'_err_high)"'
endfor
printf,unit,'close $fileid'


;
; Write ouput code
;
printf,unit,'# prepare output for fits file'
printf,unit,'set fileid [open $temp_headfl w]'
printf,unit,'  puts $fileid "XTENSION=BINTABLE /binary table extension"'
printf,unit,'  puts $fileid "EXTNAME=$model_name /name of this binary table"'
;printf,unit,'  puts $fileid "DATE='$date_value' / Date of model"'
printf,unit,'  puts $fileid "NH0=$Nh0 / \[1.e22 cm-2\] Galactic column density"'
printf,unit,'  puts $fileid "KT0=$kT0 / \[keV\] Initial parameter of energy"'
printf,unit,'  puts $fileid "ABUNDANC=$Abundance / Initial parameter of Abundance"'
printf,unit,'  puts $fileid "REDSHIFT=$Redshift / Redshift"'
printf,unit,'  puts $fileid "ENG_LOW='+strtrim(string(lo_energy),2)+' / \[keV\] Low energy used"'
printf,unit,'  puts $fileid "ENG_HI='+strtrim(string(hi_energy),2)+' / \[keV\] High energy used"'
printf,unit,'  puts $fileid "CHNL_LOW=$chnl_low / Low channel used"'
printf,unit,'  puts $fileid "CHNL_HI=$chnl_high / High channel used"'
printf,unit,'  puts $fileid "DOF=$dof_value / Degree of freedom"'
printf,unit,'  puts $fileid "CHI_SQR=$red_chi_squared / Reduced chi-squared"'
printf,unit,'  puts $fileid "NUM_REG='+strtrim(string(nreg),2)+' / Number of regions"'
for i=1,nreg do begin
   printf,unit,'  puts $fileid "KT'+strtrim(string(i),2)+'=$kT'+strtrim(string(i),2)+'_result / \[keV\] Derived energy"'
   printf,unit,'  puts $fileid "KT'+strtrim(string(i),2)+'ERRU=$p'+strtrim(string(5+(i-1)*10),2)+'_err_high / \[keV\] Energy 1sigma upper limit"'
   printf,unit,'  puts $fileid "KT'+strtrim(string(i),2)+'ERRL=$p'+strtrim(string(5+(i-1)*10),2)+'_err_low / \[keV\] Energy 1sigma lower limit"'
   printf,unit,'  puts $fileid "AB'+strtrim(string(i),2)+'=$ab'+strtrim(string(i),2)+'_result / Derived abundance"'
   printf,unit,'  puts $fileid "AB'+strtrim(string(i),2)+'ERRU=$p'+strtrim(string(7+(i-1)*10),2)+'_err_high / Abundance 1sigma upper limit"'
   printf,unit,'  puts $fileid "AB'+strtrim(string(i),2)+'ERRL=$p'+strtrim(string(7+(i-1)*10),2)+'_err_low / Abundance 1sigma lower limit"'   
   printf,unit,'  puts $fileid "NORM'+strtrim(string(i),2)+'=$norm'+strtrim(string(i),2)+'_result / Derived normalization"'
   printf,unit,'  puts $fileid "N'+strtrim(string(i),2)+'ERRU=$p'+strtrim(string(10+(i-1)*10),2)+'_err_high / Normalization 1sigma upper limit"'
   printf,unit,'  puts $fileid "N'+strtrim(string(i),2)+'ERRL=$p'+strtrim(string(10+(i-1)*10),2)+'_err_low / Normalization 1sigma lower limit"'
endfor
if (keyword_set(central) or keyword_set(noflux)) then goto, skip_flux_write
printf,unit,'  set units "/ \\\[ergs cm-2 s-1\\\]"'
printf,unit,'  for {set ii 0} {$ii <=[expr $BandNum*2-1]} {incr ii} {'
printf,unit,'    if {$ii < $BandNum} {'
printf,unit,'      set iii $ii'
printf,unit,'      set comm "Flux"   '
printf,unit,'    } else {'
printf,unit,'      set iii [expr $ii-$BandNum]'
printf,unit,'      set comm "Corr. flux"'
printf,unit,'    }'
printf,unit,'      set cmd "puts \$fileid \"[lindex $FluxName $ii]=\$[lindex $FluxName $ii] $units $comm"'
printf,unit,'      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) keV band\""'
printf,unit,'      eval $cmd'
printf,unit,'      set cmd "puts \$fileid \"[lindex $FluxName $ii]U=\$[lindex $FluxName $ii]U $units $comm"'
printf,unit,'      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) 1sigma upper limit\""'
printf,unit,'      eval $cmd'
printf,unit,'      set cmd "puts \$fileid \"[lindex $FluxName $ii]L=\$[lindex $FluxName $ii]L $units $comm"'
printf,unit,'      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) 1sigma lower limit\""'
printf,unit,'      eval $cmd  '
printf,unit,'    } '

skip_flux_write:
printf,unit,'close $fileid'
printf,unit,' '
printf,unit,'# write temp fits'
printf,unit,'exec ftemplate template=$temp_headfl outfile=$temp_fitsfl'
printf,unit,' '
printf,unit,'# populate source.spectra fits file'
printf,unit,'set flag_exists [file exists $fitsfl]'
printf,unit,'if {$flag_exists == 1} {'
printf,unit,'  exec fstruct $fitsfl'
printf,unit,'  set HDU_number [ exec pget fstruct totalhdu ]'
printf,unit,'  for {set i 1} {$i < $HDU_number} {incr i} {'
printf,unit,'    exec fstruct $fitsfl+$i'
printf,unit,'    set extname [ exec pget fstruct extname ]'
printf,unit,'    if {$extname == $model_name} {'
printf,unit,'      exec fdelhdu $fitsfl+$i N Y '
printf,unit,'      break'
printf,unit,'    }'
printf,unit,'  }'
printf,unit,'  exec fappend $temp_fitsfl\[1\] $fitsfl history=no'
printf,unit,'  file delete $temp_fitsfl   '
printf,unit,'} else {'
printf,unit,'  exec "mv" $temp_fitsfl $fitsfl'
printf,unit,'}'
printf,unit,' '
printf,unit,'file delete $temp_headfl'
printf,unit,'exit'


;
; Close script file
;
close,unit
free_lun,unit


;
; Return
;
return
end