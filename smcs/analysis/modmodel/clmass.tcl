# Procedures specific to clmass.
#
############################################################
#
# Allows generic check that the model is known
#
proc clmassKnown {} {
    return 1
}
#
############################################################
#
# Deduce maximum possible shells from total number of parameters
#
proc clmassMaxShells {mixpars} {
    return [expr ($mixpars - 4) / 2]
}
#
############################################################
#
# Model has density parameters
#
proc clmassHasDens {} {
    return 1
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
proc clmassConstraintData {rcut rbName} {
    upvar $rbName rbounds
    set denpars [denPars rbounds]
    set vnorm [expr 4.0 * [mixmod_pi] / 3.0]
    foreach shell $rbounds dp $denpars {
	if {[string length $dp] > 0} {
	    # Outer radius for current shell
	    set ri [lindex $shell 0]
	    if {$ri >= $rcut} {
		set vin 0.0
	    } else {
		set ro [lindex $shell 1]
		if {$ro > $rcut} {
		    set ro $rcut
		}
		# Volume inside rcut to which the density
		# for the current shell contributes
		set vin [expr $vnorm * ($ro - $ri) \
			     * ($ro * ($ro + $ri) + $ri * $ri)]
	    }
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
proc clmassShellDensities {rbName} {
    upvar $rbName rbounds
    set denpars [denPars rbounds]
    foreach dp $denpars {
	set pnum [lindex $dp 1]
	tclout param $pnum
	lappend dens [lindex $xspec_tclout 0]
    }
    return $dens
}
