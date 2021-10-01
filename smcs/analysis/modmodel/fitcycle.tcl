# Support routines for fitcycle.xcm
#
############################################################
#
# Find a parameter by name and the name of its units.
# Search can be limited to a single model component.
proc findParName {parName unitName {compNum 0}} {
    # Find the component with the named parameter
    if {$compNum} {
	set compStart $compNum
	set compEnd $compNum
    } else {
	tclout modcomp
	set compStart 1
	set compEnd $xspec_tclout
    }
    for {set icomp $compStart} {$icomp <= $compEnd} {incr icomp} {
	tclout compinfo $icomp
	set pstart [lindex $xspec_tclout 1]
	set pend [expr $pstart + [lindex $xspec_tclout 2] - 1]
	for {set ipar $pstart} {$ipar <= $pend} {incr ipar} {
	    tclout pinfo $ipar
	    if {[string compare [lindex $xspec_tclout 0] $parName] == 0
	        && [string compare [lindex $xspec_tclout 1] $unitName] == 0} {
		return [list $icomp [expr $ipar - $pstart]]
	    }
	}
    }
    return {}
}
############################################################
#
# Untie temperatures and norms 
proc breakLinks {} {
    # Find the component and offset of kT
    set kTloc [findParName kT keV]
    if {[llength $kTloc] != 2} {
	puts stderr "breakLinks: failed to find kT in model"
	return
    }
    set therm [lindex $kTloc 0]
    # Offset of the norm in the thermal model
    set normLoc [findParName norm "" $therm]
    if {[llength $normLoc] != 2} {
	puts stderr "breakLinks: no norm in model component $therm"
	return
    } 
    set kToff [lindex $kTloc 1]
    set normOff [lindex $normLoc 1]
    # Untie all from the first
    tclout datagrp
    set ndg $xspec_tclout
    for {set igrp 2} {$igrp <= $ndg} {incr igrp} {
	tclout compinfo $therm $igrp
	set pbase [lindex $xspec_tclout 1]
	set kTparNo [expr $pbase + $kToff]
	set normParNo [expr $pbase + $normOff]
#	untie $kTparNo $normParNo $ZparNo
	untie $kTparNo $normParNo
    }
    return
}
############################################################
#
# Front end sanity checks and numbers for accessing nfwmass model
proc frontend_nfwmass {btName mxName shName} {
    upvar $btName betapars $mxName maxshells $shName shells
    # Component 1 should be the mixing model
    tclout compinfo 1
    if {[string compare [lindex $xspec_tclout 0] nfwmass] != 0} {
	puts stderr "Expecting nfwmass as first model component"
	return 1
    }
    # Number of nfwmass parameters
    set mixpars [lindex $xspec_tclout 2]
    # Number of beta model parameters in nfwmass
    set betapars 4
    # Number of NFw parameters
    set nfwpars 2
    # Check names of last two fixed params and the first temperature
    tclout pinfo [expr $betapars - 1]
    set betname [lindex $xspec_tclout 0]
    tclout pinfo $betapars
    set swname [lindex $xspec_tclout 0]
    tclout pinfo [expr $betapars + 1]
    set ktfirst [lindex $xspec_tclout 0]
    if {[string compare $betname beta] != 0 \
	    || [string compare $swname switch] != 0 \
	    || ![string match {kT*} $ktfirst]} {
	puts stderr "Unexpected parameter names for nfwmass"
	return 1
    }
    # Maximum number of shells in clmass
    set maxshells [expr $mixpars - $betapars - $nfwpars]
    # Check the NFW params
    tclout pinfo [expr $betapars + $maxshells + 1]
    set anfw [lindex $xspec_tclout 0]
    tclout pinfo [expr $betapars + $maxshells + 2]
    set norm [lindex $xspec_tclout 0]
    if {[string compare $anfw nfwa] != 0
	|| [string compare $norm nfwpot] != 0} {
	puts stderr "Unexpected NFW parameters for nfwmass"
	return 1
    }
    # Get number of active shells
    tclout datagrp
    set shells $xspec_tclout
    if {$shells > $maxshells} {
	puts stderr "Too many shells for model - edit model.dat to increase"
	return 1
    }
    return 0
}
############################################################
#
# Does parameter linking and freezing required by the nfwmass model.  
# Use after the model has been set.
# Arguments determine settings for the beta model (defaults should
# match model.dat).
proc nfwmass_start {{rinner 0} {a 0} {beta 0.7} {sw 0}} {
# Sanity checks, etc
    if {[frontend_nfwmass fixedpars maxshells shells]} {
	return
    }
# Locate the thermal component
    set kTloc [findParName kT keV]
    if {[llength $kTloc] != 2} {
	puts stderr "No thermal component found"
	return
    }
    set thermComp [lindex $kTloc 0]
    set kToff [lindex $kTloc 1]
# Link mixing model temperature to thermal model - preserve temperature
# from thermal model
    for {set igrp 1} {$igrp <= $shells} {incr igrp} {
	tclout compinfo $thermComp $igrp
	set thpar [expr [lindex $xspec_tclout 1] + $kToff]
	tclout param $thpar
	set mixtpar [expr $fixedpars + $igrp]
	newpar $thpar =$mixtpar
	newpar $mixtpar [lindex $xspec_tclout 0]
    }
# Freeze any remaining shell temperatures
    if {$shells < $maxshells} {
	set ftstart [expr $fixedpars + $shells + 1]
	set ftend [expr $fixedpars + $maxshells]
	freeze $ftstart-$ftend
    }
# Link norms
    set normLoc [findParName norm "" $thermComp]
    if {[llength $normLoc] != 2} {
	puts stderr "Thermal norm not found"
	return
    }
    set normOff [lindex $normLoc 1]
    tclout compinfo $thermComp
    set nTarg [expr [lindex $xspec_tclout 1] + $normOff]
    for {set igrp 2} {$igrp <= $shells} {incr igrp} {
	tclout compinfo $thermComp $igrp
	set normPar [expr [lindex $xspec_tclout 1] + $normOff]
	newpar $normPar =$nTarg
    }
# Set and freeze beta model params 
    newpar 1 $rinner
    newpar 2 $a
    newpar 3 $beta
    newpar 4 $sw
    freeze 2
    freeze 3
    # Freeze density for the outermost shell when using the beta model
    if {$sw != 0} {
	set extraden [expr $fixedpars + $maxshells + $shells]
	freeze $extraden
    }
}
############################################################
#
# Form of the mass distribution for NFW potential
proc massform {x} {
    if {$x < 1.8e-3} {
	# Overkill here, but expand for small x
	return [expr $x * $x * (0.5 - $x * ((2.0 / 3.0) \
					    - $x * (0.75 - 0.8 * $x)))]
    } else {
	return [expr log (1.0 + $x) - $x / (1.0 + $x)]
    }
}
############################################################
#
# Form of the mean mass density in a shell for NFW
proc rhoform {xin xout} {
    return [expr ([massform $xout] - [massform $xin]) \
	    / (($xout - $xin) * ($xout * ($xout + $xin) + $xin * $xin))]
}
############################################################
#
# Mean densities for the shells for NFW mass model (model units)
proc NFWmeans {} {
    # NFW scale
    set aLoc [findParName nfwa "" 1]
    if {[llength $aLoc] != 2} {
	puts stderr "NFWmeans: nfwa not found"
	return
    }
    set aParNum [expr [lindex $aLoc 1] + 1]
    tclout param $aParNum
    set nfwa [lindex $xspec_tclout 0]
    # NFW potential norm
    set potLoc [findParName nfwpot "" 1]
    if {[llength $potLoc] !=2 } {
	puts stderr "NFWmeans: nfwpot not found"
	return
    }
    set potParNum [expr [lindex $potLoc 1] + 1]
    tclout param $potParNum
    set nfwpot [lindex $xspec_tclout 0]
    # Normalizing factor for the means densities
    set pi 3.1415926535897932384
    set denfac [expr 3.0 * $nfwpot / (4.0 * $pi * $nfwa * $nfwa)]
    # Record the inner radius
    tclout param 1
    set r(0) [lindex $xspec_tclout 0]
    tclout datasets
    set nspec $xspec_tclout
    # Requires spectra in radial order
    for {set ispec 1} {$ispec <= $nspec} {incr ispec} {
	tclout datagrp $ispec
	set dg $xspec_tclout
	if {![info exists r($dg)]} {
	    tclout xflt $ispec
	    set r($dg) [lindex $xspec_tclout 1]
	    set dgprev [expr $dg - 1]
	    if {![info exists r($dgprev)]} {
		puts stderr "NFWmeans: inner radius undefined"
		return
	    }
	    set xin [expr $r($dgprev) / $nfwa]
	    set xout [expr $r($dg) / $nfwa]
	    # XSPEC hides the tcl log function, so do this externally
	    lappend den [expr $denfac * [rhoform $xin $xout]]
	}
    }
    return $den
}
############################################################
#
# Switch mixing model from nfwmass to clmass 
proc nfwtocl {} {
# Record mean shell densities from the NFW fit
    set denest [NFWmeans]
# Get the model string - with sanity checks.
    # XSPEC repeats the model string for every data group
    tclout model
    set modstring $xspec_tclout
    set mslen [string length $modstring]
    tclout datagrp
    set ngrp $xspec_tclout
    if {[expr $mslen % $ngrp] != 0} {
	puts stderr "nfwtocl: model string length not a multiple of shells"
	return
    }
    set ml [expr $mslen / $ngrp]
    # This should be the actual model string
    set modplain [string range $modstring 0 [expr $ml - 1]]
    for {set igrp 1} {$igrp <= $ngrp} {incr igrp} {
	append rebuilt $modplain
    }
    if {[string compare $modstring $rebuilt] != 0} {
	puts stderr "nfwtocl: rebuilt model string does not match"
	return
    }
    # Is mixing model nfwmass?
    if {[string compare [string range $modplain 0 6] nfwmass] != 0} {
	puts stderr "nfwtocl: mixing model is not nfwmass"
	return
    }
# Collect beta model params
    tclout param 1
    set rinner [lindex $xspec_tclout 0]
    tclout param 2
    set abm [lindex $xspec_tclout 0]
    tclout param 3
    set beta [lindex $xspec_tclout 0]
    tclout param 4
    set swbm [lindex $xspec_tclout 0]
# Make new model string and apply it
    append newmod clmass [string range $modplain 7 end]
    editmod $newmod & /*
# Restore beta model params
    newpar 1 $rinner
    newpar 2 $abm
    newpar 3 $beta
    newpar 4 $swbm
    freeze 2 3
# Now fix the temperatures and densities
    set kTloc [findParName kT keV]
    if {[llength $kTloc] != 2} {
	puts stderr "nfwtocl: failed to find kT in model"
	return
    }
    set thcomp [lindex $kTloc 0]
    set kToff [lindex $kTloc 1]
    tclout compinfo 1
    set nclpars [lindex $xspec_tclout 2]
    set fixedpars 4
    set maxshells [expr ($nclpars - $fixedpars) / 2]
    for {set igrp 1} {$igrp <= $ngrp} {incr igrp} {
	tclout compinfo $thcomp $igrp
	set thpar [expr [lindex $xspec_tclout 1] + $kToff]
	tclout param $thpar
	set mixtpar [expr $fixedpars + $igrp]
	newpar $thpar =$mixtpar
	newpar $mixtpar [lindex $xspec_tclout 0]
	set rhopar [expr $fixedpars + $maxshells + $igrp]
	newpar $rhopar [lindex $denest [expr $igrp - 1]]
    }
    # Freeze any remaining shell parameters for clmass
    if {$ngrp < $maxshells} {
	set ftstart [expr $fixedpars + $ngrp + 1]
	set ftend [expr $fixedpars + $maxshells]
	set fdstart [expr $ftstart + $maxshells]
	set fdend [expr $ftend + $maxshells]
	freeze $ftstart-$ftend $fdstart-$fdend
    }
    # Freeze density for outermost shell when beta model is used
    if {$swbm != 0} {
	set extraden [expr $fixedpars + $maxshells + $ngrp]
	freeze $extraden
    }
}
############################################################
#
# Use fit values to determine mass, etc.
# dunit = distance unit for shell radii in arcsec
# obar = overdensity
proc nfwMass {dunit obar} {
    # Get nfwa
    set aLoc [findParName nfwa "" 1]
    if {[llength $aLoc] != 2} {
	puts stderr "nfwMass: no nfwa in first model component"
	return
    }
    set aPar [expr 1 + [lindex $aLoc 1]]
    tclout param $aPar
    set nfwa [lindex $xspec_tclout 0]
    # Get nfwpot
    set pLoc [findParName nfwpot "" 1]
    if {[llength $pLoc] != 2} {
	puts stderr "nfwMass: no nfwpot in first model component"
	return
    }
    set pPar [expr 1 + [lindex $pLoc 1]]
    tclout param $pPar
    set nfwpot [lindex $xspec_tclout 0]
    # Get redshift
    set kTloc [findParName kT keV] 
    set zLoc [findParName redshift "" [lindex $kTloc 0]]
    if {[llength $zLoc] != 2} {
	puts stderr "nfwMass: no redshift in thermal component"
	return
    }
    tclout compinfo [lindex $zLoc 0]
    set zPar [expr [lindex $xspec_tclout 1] + [lindex $zLoc 1]]
    tclout param $zPar
    set z [lindex $xspec_tclout 0]
    # Run nfwprops
    set props [exec ./support/nfwprops $z $dunit $nfwa $nfwpot $obar]
    if {[string compare Concentration_parameter: [lindex $props 6]] != 0} {
	puts stderr "nfwMass: expecting concentration parameter"
	return
    }
    set xconc [lindex $props 7]
    puts "Concentration paramter: $xconc"
    if {[string compare Enclosed_mass: [lindex $props 8]] != 0
	|| [string compare Msun [lindex $props 10]] != 0} {
	puts stderr "nfwMass: expecting enclosed mass"
	return
    }
    set mass [lindex $props 9]
    puts "Best fitting mass, M_$obar: $mass Msun"
    if {[string compare da/dB: [lindex $props 11]] != 0} {
	puts stderr "nfwMass: expecting da/dB"
	return
    }
    set dadb [lindex $props 12]
    puts stderr "da/dB: $dadb"
    # Get fit statistic before applying constraint
    tclout stat
    set bestfit $xspec_tclout
    tclout dof
    set dof [lindex $xspec_tclout 0]
    set maxlev [expr $bestfit / $dof]
    if {$maxlev >= 2.0} {
	set maxlev [expr floor ($maxlev + 1)]
    }
    # Constrain nfwa to follow the local maximum mass slope
    if {$dadb >= 0} {
	newpar $aPar =$nfwa+$dadb*($pPar-$nfwpot)
    } else {
	set mdadb [expr -$dadb]
	newpar $aPar =$nfwa-$mdadb*($pPar-$nfwpot)
    }
    fit
    if {$maxlev >= 2.0} {
	error maximum $maxlev $pPar
    } else {
	error $pPar
    }
    tclout error $pPar
    set errword [lindex $xspec_tclout 2]
    if {[string compare FFFFFFFFF $errword] != 0} {
	puts "nfwMass: problem in error calculation: $errword"
	return
    }
    set minpot [lindex $xspec_tclout 0]
    set maxpot [lindex $xspec_tclout 1]
    set maxanfw [expr $nfwa + ($maxpot - $nfwpot) * $dadb]
    set upper [exec ./support/nfwprops $z $dunit $maxanfw $maxpot $obar]
    set upmass [lindex $upper 9]
    puts "For a = $maxanfw, B = $maxpot, M_$obar = $upmass Msun"
    # Rotate constraint to get absolute maximum mass
}
