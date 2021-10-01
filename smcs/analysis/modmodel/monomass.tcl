# Procedures specific to the monomass model.
#
############################################################
#
# Allows generic check that the model is known
#
proc monomassKnown {} {
    return 1
}
#
############################################################
#
# Deduce maximum possible shells from total number of parameters
#
proc monomassMaxShells {mixpars} {
    return [expr ($mixpars - 4) / 2]
}
#
############################################################
#
# Model has density parameters
#
proc monomassHasDens {} {
    return 1
}
#
############################################################
#
# Does parameter linking and freezing required by the monomass model.  
# Arguments are settings for the beta model (defaults should
# match model.dat).
#
proc monomass_start {{rinner 0} {a 0} {beta 0.7} {sw 0}} {
#
# Sanity checks, etc
    if {[frontend fixedpars maxshells shells]} {
	return
    }
# Identify thermal model component
    set tmod [findParName kT keV]
    if {[llength $tmod] != 2} {
	puts stderr "monomass_start: no thermal component"
	return
    }
    set thermcomp [lindex $tmod 0]
    set thermoff [lindex $tmod 1]
    tclout compinfo $thermcomp
    set thermname [lindex $xspec_tclout 0]
#
# Linking and freezing
    # Link thermal model temperatures to monomass temperatures
    for {set igrp 1} {$igrp <= $shells} {incr igrp} {
	tclout compinfo $thermcomp $igrp
	if {[string compare [lindex $xspec_tclout 0] $thermname] != 0} {
	    puts stderr "monomass_start: failed redundant check"
	    return
	}
	set thpar [expr [lindex $xspec_tclout 1] + $thermoff]
	set mixtpar [expr $fixedpars + $igrp]
	newpar $thpar =$mixtpar
    }
    # Freeze remaining shell parameters for monomass
    if {$shells < $maxshells} {
	set ftstart [expr $fixedpars + $shells + 1]
	set ftend [expr $fixedpars + $maxshells]
	set fdstart [expr $ftstart + $maxshells]
	set fdend [expr $ftend + $maxshells]
	freeze $ftstart-$ftend $fdstart-$fdend
    }
#
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
#
############################################################
#
# Info for applying a fixed mass constraint at rcut.  Makes
# a list in radial order, with each element listing a
# parameter number and the associated volume.
# rcut - radius at which mass is fixed
# rbName refers to an ordered list of rbounds like that 
#        returned by shellBounds
#
proc monomassConstraintData {rcut rbName} {
    upvar $rbName rbounds
    set diffpars [denPars rbounds]
    set ri [rinOfRadial 0 rbounds]
    set vnorm [expr 4.0 * [mixmod_pi] / 3.0]
    foreach shell $rbounds dp $diffpars {
	if {[string length $dp] > 0} {
	    # Outer radius for current shell
	    set ro [lindex $shell 1]
	    if {$ro > $rcut} {
		set ro $rcut
	    }
	    # Volume between rcut and rinner to which the density
	    # difference for the current shell contributes
	    set vin [expr $vnorm * ($ro - $ri) \
			 * ($ro * ($ro + $ri) + $ri * $ri)]
	    # Record parameter number and effective volume
	    lappend mcdat [list [lindex $dp 1] $vin]
	}
    }
    return $mcdat
}
#
############################################################
#
# List shell densities in radial order
#
proc monomassShellDensities {rbName} {
    upvar $rbName rbounds
    set diffpars [denPars rbounds]
    set dprev 0.0
    # Sum density differences to make a list of shell densities
    for {set i [expr [llength $diffpars] - 1]} {$i >= 0} {incr i -1} {
	set pnum [lindex [lindex $diffpars $i] 1]
	tclout param $pnum
	set d [expr [lindex $xspec_tclout 0] + $dprev]
	if {[info exists dens]} {
	    set dens [linsert $dens 0 $d]
	} else {
	    set dens $d
	}
	set dprev $d
    }
    return $dens
}
