# This is a script to take a model and place it at another
# redshift, preserving of course the intrinsic luminosities.  It was originally
# intended for NGC 6240 where there were three continuum components
# plus 4 gaussians. 
# Now params file gives:
#  line_comps (model components with lines)
#  param_norms (model params with normalizations, except for lines)
#  param_line_norms (model params with line normalizations)
#  param_z (model params giving redshift)
# It assumes that the first parameter is always galactic Nh.

proc newz {paramsfile z H0 args} {
    set q0 0.0
    if {[llength $args] != 0} {
	set q0 [lindex $args 0]
    }

    global xs_return_result
    set xs_return_result 1

    # Read data from parameter file
    set fil [open $paramsfile "r"]
    # skip header lines
    set line_comps "#"
    while {[string index $line_comps 0] == "#"} {
    # set line_comps "7 8 9 10"
	gets $fil line_comps
    }
    # set param_norms "8 16 21"
    gets $fil param_norms
    # set param_line_norms "25 29 33 37"
    gets $fil param_line_norms
    # set param_z "6 10 14 18 20 24 28 32 36"
    gets $fil param_z

    set all_norms "$param_norms $param_line_norms"
    # set eranges "{0.2 0.5} {1.0 2.0} {3.0 10.0}"
    set eranges "{0.2 10.} {0.2 10.0} {0.2 10.0}"
    cosmo $H0 $q0

    puts "Param norms = $param_norms"
    puts "Param z = $param_z"
    puts "There are [llength $line_comps] lines in this model"
    if {[llength $line_comps] != [llength $param_line_norms]} {
	puts "Error... different number of line components and normalizations"
	return
    }

    set logf [open "newz_$z.log" "w"]
    puts $logf "Set cosmology to H0 = $H0, q0 = $q0"
    # Store original luminosities
    set orig_lum ""
    set origz [lindex [show par [lindex $param_z 0]] 0]
    puts $logf "Current z = $origz, new z = $z"
    flush $logf
    set j 0
    foreach erange $eranges {
	# set lum [lumin [lindex $erange 0] [lindex $erange 1] $origz]
	set lum [iso_lumin $all_norms $j $erange $origz]
	lappend orig_lum $lum
	puts $logf "L([lindex $erange 0] - [lindex $erange 1], z=$origz) = $lum"
	incr j
    }
    puts $logf "Initial luminosiites for z=$origz:"
    puts $logf $orig_lum
    #set tot 0.
    #foreach lum $orig_lum {
	#set tot [expr $tot + $lum]
    #}

    #puts $logf "Total lum = $tot"

    # Okay, now start to change the redshifts
    foreach par $param_z {
	newpar $par $z
    }
    
    # Do a zero-order (z/origz)^2 adjustment of all norms
    set adj [expr $origz*$origz/$z/$z]
    foreach param_norm $param_norms {
	set curnorm [lindex [show par $param_norm] 0]
	newp $param_norm [expr $curnorm*$adj]
    }
    foreach param_norm $param_line_norms {
	set curnorm [lindex [show par $param_norm] 0]
	newp $param_norm [expr $curnorm*$adj]
    }

    # Now iterate to get the original lum. back
    set n 10
    puts $logf "Adjusting norms"
    for {set j 0} {$j < $n} {incr j} {
	set adj_list ""
	puts $logf "Iteration [expr $j + 1]"
	set newlum ""
	set k 0
	foreach erange $eranges {
	    # lappend newlum [lumin [lindex $erange 0] [lindex $erange 1] $z]
	    lappend newlum [iso_lumin $all_norms $k $erange $z]
	    incr k
	}
	puts $logf "Current value of lums. = $newlum"
	
	for {set i 0} {$i < [llength $param_norms]} {incr i} {
	    # newp [lindex $param_norms $i] [lindex $save_norms $i]
	    set param_norm [lindex $param_norms $i]
	    set olum [lindex $orig_lum $i]
	    set curnorm [lindex [show par $param_norm] 0]
	    set adj [expr $olum / [lindex $newlum $i]]
	    lappend adj_list $adj
	    puts $logf "Adjusting norm $i = $curnorm by $adj"
	    newpar $param_norm [expr $curnorm*$adj]
	    if {$i == 2} {
		puts $logf "Adjusting line norms by $adj"
		foreach param_norm $param_line_norms {
		    set curnorm [lindex [show par $param_norm] 0]
		    newp $param_norm [expr $curnorm*$adj]
		}
	    }
	}
	flush $logf
    }

    puts $logf "Finished... final luminosties for z = $z:"
    set newlum ""
    set j 0
    foreach erange $eranges {
	# lappend newlum [lumin [lindex $erange 0] [lindex $erange 1] $z]
	lappend newlum [iso_lumin $all_norms $j $erange $z]
	incr j
    }
    puts $logf $newlum

    close $logf
    return
}

proc iso_lumin {param_norms i erange z} {
    # Returns the result of the lumin command when all norms except
    # [lindesx $param_norms $i] are set to 0

    # Set gal. NH component to zero
    set curNH [lindex [show par 1] 0]

    # Save current norms
    set save_norms ""
    foreach param_norm $param_norms {
	lappend save_norms [lindex [show par $param_norm] 0]
    }

    # Zero-out norms
    foreach param_norm $param_norms {
	newp $param_norm 0.
    }
    
    # Restore desired norm.
    newp [lindex $param_norms $i] [lindex $save_norms $i]

    # Get lum.
    set l [lumin [lindex $erange 0] [lindex $erange 1] $z]

    # Restore all norms
    set j 0
    foreach param_norm $param_norms {
	newp $param_norm [lindex $save_norms $j]
	incr j
    }

    # Restore gal. NH
    newp 1 $curNH
    return $l
}

