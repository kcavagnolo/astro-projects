;+
; NAME:
;   MAKE_XRAY_FLUX
;
; PURPOSE:
;   Creates an XSPEC script to deproject the spectra extracted by
;   EXTRACT_ANNULI with the projct*wabs*mekal model and to 
;   calculate the flux inside the cooling region
;
; CALLING SEQUENCE:
;   MAKE_XRAY_FLUX
;
; INPUTS:
;   name (string):     name of cluster
;   obsid (string):    obsid of observation
;   runname (string):  name of annuli to use
;   output (string):   name of the output Xspec file
;   nreg (int):        number of annuli to use
;   nh_gal (float):    galactic absorption
;   kt_guess (float):  initial guess of cluster temp
;   ab_guess (float):  initial guess of cluster abundance
;   redshift (float):  cluster redshift
;   lo_energy (float): lower energy to fit spec
;   hi_energy (float): upper energy to fit spec
;   emin (float):      lower energy to calc bolo lumin
;   emax (float):      upper energy to calc bolo lumin
;   inner_a (float):   initial guess at semi-major axis for projct
;   outer_b (float):   initial guess at semi-minor axis for projct
;   inner_pa (float):  initial guess of projct pos angle
;
; OPTIONAL KEYWORD INPUTS:
;   /GRIDMODE
;
; OUTPUTS:
;
; EXAMPLE:
;  make_xray_flux, 'RBS_0797', '7902', 'annuli', 'temp.xcm', 12, 0.022, 6.0, 0.3, 0.354, 0.5, 7.0, 0.01, 100.0, 0.0, 0.0, 0.0, 2.71, 90
;
; MODIFICATION HISTORY:
;   Written by DR, 2004-6-29
;   Updated by KWC, 2008-12-19
;-
;#####################
;#####################

PRO make_xray_flux, $
   name, obsid, runname, output, nreg, nH_gal, kT_guess, ab_guess, $
   redshift, lo_energy, hi_energy, emin, emax, inner_a, inner_b, $
   inner_pa, conf, fluxconf, gridmode=gridmode

;# convert all input to ints
nreg  = fix(nreg)
creator = 'KWC'
xspecv = '11.3.2ag'

;# open output script file
get_lun, OUT
openw, OUT, output
printf, OUT, '# Set cosmology.'
printf, OUT, 'cosmo 70 0 0.7'
printf, OUT, ' '

;# Write data reading and filtering code
printf, OUT, '# read in data files'
for i = 1, nreg do begin
   spec = name+'_'+obsid+'_'+runname+num2str(i)+'_src1_grp.pi'
   printf, OUT, 'data '+num2str(i)+':'+num2str(i)+' '+spec
   printf, OUT, 'ignore '+num2str(i)+':'+num2str(i)+' **-'+num2str(lo_energy)+' '+num2str(hi_energy)+'-**'
endfor
printf, OUT, ' '

;# Write preamble
printf, OUT, 'set model_name "projct_wabs_mekal_flux"'
printf, OUT, 'set xs_return_result 1'
printf, OUT, 'set tcl_precision 12'
printf, OUT, 'set xs_echo_script 1'
printf, OUT, 'chatter 10'
printf, OUT, ' '

;# Set grid mode
if keyword_set(gridmode) then begin
   printf, OUT, '# USER has activated grid mode'
   printf, OUT, '# Grid mode gives ability to fit model'
   printf, OUT, '# few times using the different set of'
   printf, OUT, '# initial parameters. The "best" model will be'
   printf, OUT, '# choosen based on a simple criterion'
   printf, OUT, '# of the lowest chi value. '
   printf, OUT, 'set Grid_Mode 1'
   printf, OUT, ' '
endif

;# Set initial parameters
printf, OUT, 'set Abundance '+num2str(ab_guess)
printf, OUT, 'set Redshift '+num2str(redshift)
if keyword_set(gridmode) then begin
   printf, OUT, 'set kT0 {1.5 1.0 0.5 3. 5.}'
   printf, OUT, 'set Nh0 {1. 0.5 0.1 5. 10.} '
endif else begin
   printf, OUT, 'set kT0 '+num2str(kT_guess)
   printf, OUT, 'set Nh0 '+num2str(nH_gal)
endelse
printf, OUT, ' '

;# Set fit statistic
printf, OUT, '# Available methods are:'
printf, OUT, '# "standard", "gehrels", "churazov", "model" '
printf, OUT, 'weight standard'
printf, OUT, 'set Conf_Level_Par '+num2str(conf)
printf, OUT, ' '

;# Set low and high energies for flux calculation
printf, OUT, 'set EbandLo {'+num2str(lo_energy)+' '+num2str(emin)+'}'
printf, OUT, 'set EbandHi {'+num2str(hi_energy)+' '+num2str(emax)+'}'
printf, OUT, 'set FluxName {FXray FBol FcXray FcBol}'
printf, OUT, 'set LuminName {LXray LBol LcXray LcBol}'
printf, OUT, 'set Conf_Level_Flux '+num2str(fluxconf)
printf, OUT, 'set Conf_Level_Lumin '+num2str(fluxconf)
printf, OUT, 'set Number_Runs 100'
printf, OUT, 'set model_savfl "$model_name.xspecsav"'
printf, OUT, 'set fitsfl "source_mekal_flux.spectra"'
printf, OUT, 'set temp_fitsfl "z_temp.fits"'
printf, OUT, 'set temp_headfl "z_head.txt"'
printf, OUT, 'set date_value [ exec date ]'
printf, OUT, 'setplot energy'
printf, OUT, 'ignore bad'
printf, OUT, ' '

;# get dofs
printf, OUT, 'tclout noticed 1'
printf, OUT, 'scan $xspec_tclout "%f%s%f" chnl_low dum chnl_high'
printf, OUT, 'set number_of_thawprms 3'
printf, OUT, 'set dof_value [expr '+num2str(nreg)+'*($chnl_high - $chnl_low + 1 - $number_of_thawprms)]'
printf, OUT, 'if {$dof_value < 1} {'
printf, OUT, '  exit'
printf, OUT, '}'
printf, OUT, ' '

;# run the grid if set
if keyword_set(gridmode) then begin
   printf, OUT, 'set Chi0 100000'
   printf, OUT, 'set kT0_best [lindex $kT0 0]'
   printf, OUT, 'set Nh0_best [lindex $Nh0 0]'
   printf, OUT, 'for {set i 0} {$i < [llength $kT0]} {incr i} {'
   printf, OUT, '  for {set j 0} {$j < [llength $Nh0]} {incr j} {'
   printf, OUT, '    model wabs(mekal) & [lindex $Nh0 $j] & [lindex $kT0 $i] & 1 & $Abundance & 0 & 1 & 1 & /*'
   printf, OUT, '    fit'
   printf, OUT, '    fit'
   printf, OUT, '    fit'
   printf, OUT, '    tclout param 1'
   printf, OUT, '    scan $xspec_tclout "%f" Nh_result'
   printf, OUT, '    tclout param 2'
   printf, OUT, '    scan $xspec_tclout "%f" kT_result '
   printf, OUT, '    tclout stat'
   printf, OUT, '    scan $xspec_tclout "%f" chi_squared'
   printf, OUT, '    set red_chi_squared  [expr $chi_squared/$dof_value]'
   printf, OUT, '    if {$red_chi_squared < $Chi0} {'
   printf, OUT, '      set Chi0 $red_chi_squared'
   printf, OUT, '      set kT0_best [lindex $kT0 $i]'
   printf, OUT, '      set Nh0_best [lindex $Nh0 $j]'
   printf, OUT, '    }'
   printf, OUT, '  }'
   printf, OUT, '}'
   printf, OUT, 'set kT0 $kT0_best'
   printf, OUT, 'set Nh0 $Nh0_best '
   printf, OUT, ' '
endif

;# Define and fit the model
printf, OUT, 'model projct*wabs(mekal)'
for i = 1, nreg do begin
   printf, OUT, num2str(inner_a)
   printf, OUT, num2str(inner_b)
   printf, OUT, num2str(inner_pa)
   printf, OUT, num2str(nH_gal)
   printf, OUT, num2str(kT_guess*(1.0+(i-1.0)/(nreg-1.0)))
   printf, OUT, '1'
   printf, OUT, '$Abundance'
   printf, OUT, '$Redshift'
   printf, OUT, '1'
   printf, OUT, '1'
endfor

;# set some optins
untie_kT = 'untie 5'
thaw_kT = 'thaw 5'
untie_ab = 'untie 7'
thaw_ab  =  'thaw 7'
untie_norm = 'untie 10'
thaw_norm = 'thaw 10'
freeze_wabsnH = 'freeze 4'
freeze_mekalnH = 'freeze 6'
freeze_redshift = 'freeze 8'
freeze_switch = 'freeze 9'
for i = 1, nreg-1 do begin
   thaw_ab = thaw_ab+', '+num2str(7+i*10)
   freeze_wabsnH = freeze_wabsnH+', '+num2str(4+i*10)
   untie_kT = untie_kT+', '+num2str(5+i*10)
   thaw_kT = thaw_kT+', '+num2str(5+i*10)
   untie_ab = untie_ab+', '+num2str(7+i*10)
   untie_norm = untie_norm+', '+num2str(10+i*10)
   thaw_norm = thaw_norm+', '+num2str(10+i*10)
   freeze_mekalnH = freeze_mekalnH+', '+num2str(6+i*10)
   freeze_redshift = freeze_redshift+', '+num2str(8+i*10)
   freeze_switch = freeze_switch+', '+num2str(9+i*10)
endfor
printf, OUT,thaw_ab
printf, OUT,freeze_wabsnH
;printf, OUT,untie_kT
;printf, OUT,thaw_kT
;printf, OUT,untie_ab
;printf, OUT,untie_norm
;printf, OUT,thaw_norm
;printf, OUT,freeze_mekalnH
;printf, OUT,freeze_redshift
;printf, OUT,freeze_switch
printf, OUT, ' '

;# some basic xspec stuff
printf, OUT, 'query yes'
printf, OUT, 'fit 100000 0.001'
printf, OUT, 'fit'
printf, OUT, 'fit'
printf, OUT, ' '

;# Create save file
printf, OUT, 'set flag_exists [file exists $model_savfl]'
printf, OUT, 'if {$flag_exists == 1} {'
printf, OUT, '  file delete $model_savfl  '
printf, OUT, '}'
printf, OUT, 'save all $model_savfl'
printf, OUT, ' '

;# Get values and errors
for i = 1,nreg do begin
   printf, OUT, 'tclout param '+num2str(5+(i-1)*10)
   printf, OUT, 'scan $xspec_tclout "%f" kT'+num2str(i)+'_result'
   printf, OUT, 'tclout param '+num2str(7+(i-1)*10)
   printf, OUT, 'scan $xspec_tclout "%f" ab'+num2str(i)+'_result'
   printf, OUT, 'tclout param '+num2str(10+(i-1)*10)
   printf, OUT, 'scan $xspec_tclout "%f" norm'+num2str(i)+'_result'
endfor
printf, OUT, 'tclout stat'
printf, OUT, 'scan $xspec_tclout "%f" chi_squared'
printf, OUT, 'set red_chi_squared [expr $chi_squared/$dof_value]'
printf, OUT, 'log z_temp_xspec.log'
printf, OUT, 'show all'
printf, OUT, 'log none'
printf, OUT, 'file copy -force z_temp_xspec.log xspec_mekal_flux.log'
printf, OUT, 'file delete z_temp_xspec.log'
printf, OUT, ' '

;# calculate fluxes
printf, OUT, 'set BandNum [llength $EbandLo]'
printf, OUT, 'set Bound_Index 0'
printf, OUT, 'dummyrsp 0.001 100'
printf, OUT, 'for {set ii 0} {$ii <=  [expr $BandNum*2-1]} {incr ii} {'
printf, OUT, '  if {$ii == $BandNum} {'
printf, OUT, '    newpar 4 0'
for i = 1,nreg-1 do begin
   printf, OUT, '    newpar '+num2str(4+i*10)+' 0'
endfor
printf, OUT, '  }'
printf, OUT, '  if {$ii < $BandNum} {'
printf, OUT, '    flux [lindex $EbandLo $ii] [lindex $EbandHi $ii] err $Number_Runs $Conf_Level_Flux'
printf, OUT, '  } else {'
printf, OUT, '    set iiii [expr $ii-$BandNum]'
printf, OUT, '    flux [lindex $EbandLo $iiii] [lindex $EbandHi $iiii] err $Number_Runs $Conf_Level_Flux'
printf, OUT, '  }'
for i = 1,nreg do begin
   printf, OUT, '  tclout flux '+num2str(i)
   printf, OUT, '  scan $xspec_tclout "%f %f %f" Ftemp'+num2str(i)+' Ftemp'+num2str(i)+'lo Ftemp'+num2str(i)+'hi'
endfor
Ftot = '$Ftemp1'
Ftotlo = '$Ftemp1lo'
Ftothi = '$Ftemp1hi'
for i = 1, nreg-1 do begin
   Ftot = Ftot+'+$Ftemp'+num2str(i+1)
   Ftotlo = Ftotlo+'+$Ftemp'+num2str(i+1)+'lo'
   Ftothi = Ftothi+'+$Ftemp'+num2str(i+1)+'hi'
endfor
printf, OUT, '  set Ftot [expr '+Ftot+']'
printf, OUT, '  set Ftotlo [expr '+Ftotlo+']'
printf, OUT, '  set Ftothi [expr '+Ftothi+']'
printf, OUT, '  eval set [lindex $FluxName $ii] [string trim $Ftot]'
printf, OUT, '  eval set [lindex $FluxName $ii]L [string trim $Ftotlo]'
printf, OUT, '  eval set [lindex $FluxName $ii]U [string trim $Ftothi]'
printf, OUT, '}'
printf, OUT, 'set fileid [open xspec_mekal_flux.log a]'
printf, OUT, 'for {set ii 0} {$ii <= [expr $BandNum*2-1]} {incr ii} {'
printf, OUT, '  if {$ii < $BandNum} {'
printf, OUT, '    set cmd "puts \$fileid \"Flux([lindex $EbandLo $ii]-[lindex $EbandHi $ii]):"  '
printf, OUT, '  } else {'
printf, OUT, '    set iii [expr $ii-$BandNum]'
printf, OUT, '    set cmd "puts \$fileid \"Flux_corr([lindex $EbandLo $iii]-[lindex $EbandHi $iii]):"'
printf, OUT, '  }'
printf, OUT, '  set cmd "$cmd \$[lindex $FluxName $ii]  1_sigma_conf_range: (\$[lindex $FluxName $ii]L -"'
printf, OUT, '  set cmd "$cmd \$[lindex $FluxName $ii]U)\""'
printf, OUT, '  eval $cmd  '
printf, OUT, '}'
printf, OUT, 'close $fileid'
printf, OUT, ' '

;# calculate lumins
printf, OUT, 'set BandNum [llength $EbandLo]'
printf, OUT, 'set Bound_Index 0'
printf, OUT, 'dummyrsp 0.001 100'
printf, OUT, 'for {set ii 0} {$ii <=  [expr $BandNum*2-1]} {incr ii} {'
printf, OUT, '  if {$ii < $BandNum} {'
printf, OUT, '    lumin [lindex $EbandLo $ii] [lindex $EbandHi $ii] '+num2str(redshift)+' err $Number_Runs $Conf_Level_Lumin'
printf, OUT, '  } else {'
printf, OUT, '    set iiii [expr $ii-$BandNum]'
printf, OUT, '    lumin [lindex $EbandLo $iiii] [lindex $EbandHi $iiii] '+num2str(redshift)+' err $Number_Runs $Conf_Level_Lumin'
printf, OUT, '  }'
for i = 1,nreg do begin
   printf, OUT, '  tclout lumin '+num2str(i)
   printf, OUT, '  scan $xspec_tclout "%f %f %f" Ltemp'+num2str(i)+' Ltemp'+num2str(i)+'lo Ltemp'+num2str(i)+'hi'
endfor
Ltot = '$Ltemp1'
Ltotlo = '$Ltemp1lo'
Ltothi = '$Ltemp1hi'
for i = 1, nreg-1 do begin
   Ltot = Ltot+'+$Ltemp'+num2str(i+1)
   Ltotlo = Ltotlo+'+$Ltemp'+num2str(i+1)+'lo'
   Ltothi = Ltothi+'+$Ltemp'+num2str(i+1)+'hi'
endfor
printf, OUT, '  set Ltot [expr '+Ltot+']'
printf, OUT, '  set Ltotlo [expr '+Ltotlo+']'
printf, OUT, '  set Ltothi [expr '+Ltothi+']'
printf, OUT, '  eval set [lindex $LuminName $ii] [string trim $Ltot]'
printf, OUT, '  eval set [lindex $LuminName $ii]L [string trim $Ltotlo]'
printf, OUT, '  eval set [lindex $LuminName $ii]U [string trim $Ltothi]'
printf, OUT, '}'
printf, OUT, 'set fileid [open xspec_mekal_lumin.log a]'
printf, OUT, 'for {set ii 0} {$ii <= [expr $BandNum*2-1]} {incr ii} {'
printf, OUT, '  if {$ii < $BandNum} {'
printf, OUT, '    set cmd "puts \$fileid \"Lumin([lindex $EbandLo $ii]-[lindex $EbandHi $ii]):"  '
printf, OUT, '  } else {'
printf, OUT, '    set iii [expr $ii-$BandNum]'
printf, OUT, '    set cmd "puts \$fileid \"Lumin_corr([lindex $EbandLo $iii]-[lindex $EbandHi $iii]):"'
printf, OUT, '  }'
printf, OUT, '  set cmd "$cmd \$[lindex $LuminName $ii]  1_sigma_conf_range: (\$[lindex $LuminName $ii]L -"'
printf, OUT, '  set cmd "$cmd \$[lindex $LuminName $ii]U)\""'
printf, OUT, '  eval $cmd  '
printf, OUT, '}'
printf, OUT, 'close $fileid'
printf, OUT, ' '

;# Write ouput code
printf, OUT, 'set fileid [open $temp_headfl w]'
printf, OUT, '  puts $fileid "XTENSION=BINTABLE /binary table extension"'
printf, OUT, '  puts $fileid "EXTNAME=$model_name /name of this binary table"'
GET_DATE,dte
printf, OUT, '  puts $fileid "DATE='+dte+' / Date of creation"'
printf, OUT, '  puts $fileid "CREATOR='+creator+' / Who made file"'
printf, OUT, '  puts $fileid "XSPECVER='+xspecv+' / Ver. of Xspec used"'
printf, OUT, '  puts $fileid "NH0=$Nh0 / \[1.e22 cm-2\] Galactic column density"'
printf, OUT, '  puts $fileid "KT0=$kT0 / \[keV\] Initial parameter of energy"'
printf, OUT, '  puts $fileid "ABUNDANC=$Abundance / Initial parameter of Abundance"'
printf, OUT, '  puts $fileid "REDSHIFT=$Redshift / Redshift"'
printf, OUT, '  puts $fileid "FIT_LOW='+num2str(lo_energy)+' / \[keV\] Low energy used in fit"'
printf, OUT, '  puts $fileid "FIT_HI='+num2str(hi_energy)+' / \[keV\] High energy used in fit"'
printf, OUT, '  puts $fileid "BOL_LOW='+num2str(emin)+' / \[keV\] Low energy used in bolo"'
printf, OUT, '  puts $fileid "BOL_HI='+num2str(emax)+' / \[keV\] High energy used in bolo"'
printf, OUT, '  puts $fileid "CHNL_LOW=$chnl_low / Low channel used"'
printf, OUT, '  puts $fileid "CHNL_HI=$chnl_high / High channel used"'
printf, OUT, '  puts $fileid "DOF=$dof_value / Degree of freedom"'
printf, OUT, '  puts $fileid "CHI_SQR=$red_chi_squared / Reduced chi-squared"'
printf, OUT, '  puts $fileid "NUM_REG='+num2str(nreg)+' / Number of regions"'
printf, OUT, '  set OUTs "/ \\\[ergs cm-2 sec-1\\\]"'
printf, OUT, '  for {set ii 0} {$ii <= [expr $BandNum*2-1]} {incr ii} {'
printf, OUT, '    if {$ii < $BandNum} {'
printf, OUT, '      set iii $ii'
printf, OUT, '      set comm "Flux"   '
printf, OUT, '    } else {'
printf, OUT, '      set iii [expr $ii-$BandNum]'
printf, OUT, '      set comm "Corr. flux"'
printf, OUT, '    }'
printf, OUT, '      set cmd "puts \$fileid \"[lindex $FluxName $ii]=\$[lindex $FluxName $ii] $OUTs $comm"'
printf, OUT, '      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) keV band\""'
printf, OUT, '      eval $cmd'
printf, OUT, '      set cmd "puts \$fileid \"[lindex $FluxName $ii]U=\$[lindex $FluxName $ii]U $OUTs $comm"'
printf, OUT, '      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) 1sigma upper limit\""'
printf, OUT, '      eval $cmd'
printf, OUT, '      set cmd "puts \$fileid \"[lindex $FluxName $ii]L=\$[lindex $FluxName $ii]L $OUTs $comm"'
printf, OUT, '      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) 1sigma lower limit\""'
printf, OUT, '      eval $cmd  '
printf, OUT, '    } '
printf, OUT, '  set OUTs "/ \\\[ergs sec-1\\\]"'
printf, OUT, '  for {set ii 0} {$ii <= [expr $BandNum*2-1]} {incr ii} {'
printf, OUT, '    if {$ii < $BandNum} {'
printf, OUT, '      set iii $ii'
printf, OUT, '      set comm "Lumin"   '
printf, OUT, '    } else {'
printf, OUT, '      set iii [expr $ii-$BandNum]'
printf, OUT, '      set comm "Corr. flux"'
printf, OUT, '    }'
printf, OUT, '      set cmd "puts \$fileid \"[lindex $LuminName $ii]=\$[lindex $LuminName $ii] $OUTs $comm"'
printf, OUT, '      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) keV band\""'
printf, OUT, '      eval $cmd'
printf, OUT, '      set cmd "puts \$fileid \"[lindex $LuminName $ii]U=\$[lindex $LuminName $ii]U $OUTs $comm"'
printf, OUT, '      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) 1sigma upper limit\""'
printf, OUT, '      eval $cmd'
printf, OUT, '      set cmd "puts \$fileid \"[lindex $LuminName $ii]L=\$[lindex $LuminName $ii]L $OUTs $comm"'
printf, OUT, '      set cmd "$cmd ([lindex $EbandLo $iii]-[lindex $EbandHi $iii]) 1sigma lower limit\""'
printf, OUT, '      eval $cmd  '
printf, OUT, '    } '
printf, OUT, 'close $fileid'
printf, OUT, ' '
printf, OUT, 'exec ftemplate template=$temp_headfl outfile=$temp_fitsfl'
printf, OUT, ' '
printf, OUT, 'set flag_exists [file exists $fitsfl]'
printf, OUT, 'if {$flag_exists == 1} {'
printf, OUT, '  exec fstruct $fitsfl'
printf, OUT, '  set HDU_number [ exec pget fstruct totalhdu ]'
printf, OUT, '  for {set i 1} {$i < $HDU_number} {incr i} {'
printf, OUT, '    exec fstruct $fitsfl+$i'
printf, OUT, '    set extname [ exec pget fstruct extname ]'
printf, OUT, '    if {$extname == $model_name} {'
printf, OUT, '      exec fdelhdu $fitsfl+$i N Y '
printf, OUT, '      break'
printf, OUT, '    }'
printf, OUT, '  }'
printf, OUT, '  exec fappend $temp_fitsfl\[1\] $fitsfl history=no'
printf, OUT, '  file delete $temp_fitsfl   '
printf, OUT, '} else {'
printf, OUT, '  exec "mv" $temp_fitsfl $fitsfl'
printf, OUT, '}'
printf, OUT, ' '
printf, OUT, 'file delete $temp_headfl'
printf, OUT, 'exit'

;# Close script file
free_lun, OUT
return
end
