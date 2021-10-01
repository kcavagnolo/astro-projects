#
# modified 10/12/01 ejmc to insure that if deltachisquare is too large, then
#  will not continue to step (as per Andy's mods to his 24sep01 ftpsite 
#  distribution)
# modified 10/9/01 ejmc from xspec10ejmc script
# modified 8/8/01 ejmc, added ability to fit multiple times to get best fit
#  during step and the ability to change the fitdelta, also changed the 
#  chi square accuracy to 4 digits, from 3 digits
#
# calls steppar with index val1 val2 nsteps, repeats fit and steppar as nec.
# if new minimum is found, will return steps and chisq. values in other
# param if supplied
# by A. Ptak (ptak@neutron.gsfc.nasa.gov) Apr 1997

proc smart_step { index val1 val2 nsteps args } {

# fix for stopping if delta chi sq is too large

    if {$index == "help" || $index == "?"} {
 	puts "Usage: smart_step index val1 val2 nsteps \[maxdchi stepvals logfile]"
 	puts "Abort if delta fit stat. > maxdchi"
	puts "stepvals return the param and chisq values from the"
	puts "(final) steppar"
	return
    }

#    global xs_return_result
#    set old_xs_return_result $xs_return_result
#    set xs_return_result 1
    autosave off


# mod for fitdelta ejmc 8/8/01
    set fitdelta 0.0001
    set nfits 5

    query yes

    global env
    set host "localdomain"
    if {[array names env HOST] == "HOST"} {
	set host $env(HOST)
    } elseif {[array names env HOSTNAME] == "HOSTNAME"} {
	set host $env(HOSTNAME)
    }
    set tempfil "smart_step_$host\_[pid].xcm"
    set logflag 0


# fix for stopping if delta chi sq is too large
    if {[llength $args] > 2} {
#       set logfil [lindex $args 1]
        set logfil [lindex $args 2]
        if {[string length $logfil] > 0} {
            set logflag 1
            set logid [open $logfil "a+"]
        }
    }

# fix for stopping if delta chi sq is too large
    if {[llength $args] > 1} {
#	set arg1 [lindex $args 0]
	set arg1 [lindex $args 1]
	upvar $arg1 stepvals
	set stepvals [list]
    }

# fix for stopping if delta chi sq is too large
    set maxdchi 10000.
    if {[llength $args] > 0} {
        set maxdchi [lindex $args 0]
        puts "Will abort if delta fit stat exceeds $maxdchi"
    }


    set new_min 0

    puts [format "smart_step %d %g %g %d" $index $val1 $val2 $nsteps]
    if {$logflag} {
	puts $logid [format "smart_step %d %g %g %d" $index $val1 $val2 \
			 $nsteps]
    }

    if {$val1 > $val2} {
	set hi_val $val1
	set low_val $val2
	set reverse 1
    } else {
	set hi_val $val2
	set low_val $val1
	set reverse 0
    }

    puts [format "Stepping range = %g - %g" $low_val $hi_val]
    if {$logflag} {
	puts $logid [format "Stepping range = %g - %g" $low_val $hi_val]
    }
    
    
    # set param_vals [show param $index]
    tclout param $index
    set param_vals $xspec_tclout
    set current_param_val [lindex $param_vals 0]
#    set current_chisq [show param]
    set current_chisq [get_stat]

    puts ""
    puts ""
    if {[llength $param_vals] < 6} {
	puts "Warning: could not determine hard limits on parameter"
	autosave 1
	return -1
    } else {
	set delta [lindex $param_vals 1]
	set hard_low [lindex $param_vals 2] 
	set hard_hi [lindex $param_vals 5]
    }

    puts [format "Hard limits = %g, %g" $hard_low $hard_hi]
    if {$logflag} {
	puts $logid [format "Hard limits = %g, %g" $hard_low $hard_hi]
    }
    
#    puts [format "current_param_val = %g" $current_param_val]
    set low_range [expr $current_param_val - $low_val]
#    puts [format "hi_val = %g" $hi_val]
#    puts [format "hi_val = %g, current_param_val=%g" \
#	    $hi_val $current_param_val]
#    puts [format "current_param_val = %g" $current_param_val]
    set hi_range [expr $hi_val - $current_param_val]

    #
    set done 0
    set niter 0
    set last_chi [expr 2 * $current_chisq]

# main loop
    while {!$done} {
	#set current_chisq [show param]
	set current_chisq [get_stat]
	incr niter
	puts "smart_step iteration $niter"
	if {$logflag} {
	    puts $logid "smart_step iteration $niter"
	}
	if {$niter > 500 || $current_chisq >= $last_chi} {
	    puts "smart_step seems to be confused"
	    if {$logflag} {
		puts $logid "smart_step seems to be confused"
	    }
	    set new_min -1
	    set done 1
	    # Try to restore things
	    if {[file readable $tempfil]} {
		@$tempfil
		exec rm $tempfil
	    }
	    break
	}
	set last_chi $current_chisq
	set min_dchi 0
	if {$reverse} {
	    set low [expr $current_param_val + $hi_range]
	    set hi [expr $current_param_val - $low_range]
     	} else {
	    set low [expr $current_param_val - $low_range]
	    set hi [expr $current_param_val + $hi_range]
	}
	puts ""
	puts [format "steppar %d %g %g %d" $index $low $hi $nsteps]
	if {$logflag} {
	    puts $logid [format "steppar %d %g %g %d" $index $low $hi $nsteps]
	    puts $logid [format "%11s %11s %11s %11s" "Chi-Squared" "Delta" \
			     "Mod param" "Original"]
	    puts $logid [format "%11s %11s %11s %11s" "" "Chi-Squared" $index \
		    "Chi-Squared"]
	}

	set stepvals [list]
	set range [expr $hi_range + $low_range]
	set stepsize [expr $range / $nsteps]
# save model to be restored
	if { [file exists $tempfil] } {
	    exec rm $tempfil
	}
	save model $tempfil
	freeze $index
	for {set i 0} {$i <= $nsteps} {incr i} {
	    if {$reverse} {
		set val [expr $low - [expr $i * $stepsize]]
	    } else {
		set val [expr $low + [expr $i * $stepsize]]
	    }
	    chatter 0
	    newpar $index $val
#           fit 1000 0.0001
# replace above line and do this nfits times to make sure got good fit
            for {set ii 0} {$ii <= $nfits} {incr ii} {
              fit 1000 $fitdelta
            }
	    chatter 10
	    # set step_chi [show param]
	    set step_chi [get_stat]
	    lappend stepvals $val $step_chi
#	    set min_dchi [format "%.3f" [expr $step_chi - $current_chisq]]
	    set min_dchi [format "%.4f" [expr $step_chi - $current_chisq]]
#	    puts "min_dchi = $min_dchi"
	    puts [format "%f %f %g" $step_chi $min_dchi $val]
	    if {$logflag} {
#		puts $logid [format "%11.3f %11.3f %11.3g %11.3f" $step_chi \
#				 $min_dchi $val $current_chisq]
		puts $logid [format "%11.4f %11.4f %11.4g %11.4f" $step_chi \
				 $min_dchi $val $current_chisq]
#		puts $logid "min_dchi = $min_dchi"
		flush $logid
	    }
	    if {$min_dchi < -0.1} {
		break
	    }
# fix for stopping if delta chi sq is too large
            if {$min_dchi > $maxdchi} {
                break
            }
	}

        if {$min_dchi > $maxdchi} {
            puts [format \
            "Chisq exceeded max value of %f, discontinuing step" \
               $maxdchi]
	    if {$logflag} {
		puts $logid [format \
			"Chisq exceeded max value of %f, discontinuing step" \
			$maxdchi]
	    }
        }
    
	if {$min_dchi < -0.1} {
	    set new_min 1
	    puts [format \
		    "A new miminum was found at %g with dchi=%f, chisq=%f" \
		    $val $min_dchi $step_chi]
	    puts "Redoing fit..."
	    if {$logflag} {
		puts $logid [format \
		    "A new miminum was found at %g with dchi=%f, chisq=%f" \
		    $val $min_dchi $step_chi]
		puts $logid "Redoing fit..."
		flush $logid
	    }
	    thaw $index
	    chatter 1
	    fit
	    chatter 10
	    # set param_vals [show param $index]
	    tclout param $index
	    set param_vals $xspec_tclout
	    set current_param_val [lindex $param_vals 0]
	    if {($current_param_val == $hard_low) && $reverse} {
		puts "New best-fit value is at hard lower limit"
		if {$logflag} {
		    puts $logid "New best-fit value is at hard lower limit"
		}
		set done 1
	    }
	    if {($current_param_val == $hard_hi) && !$reverse} {
		puts "New best-fit value is at hard upper limit"
		if {$logflag} {
		    puts $logid "New best-fit value is at hard upper limit"
		}
		set done 1
	    }
	    if {!$done} {
		set current_chisq [get_stat]
		puts [format "Redoing steppar with param %d = %g, chisq=%f" \
			$index $current_param_val $current_chisq]
		if {$logflag} {
		    puts $logid [format \
			    "Redoing steppar with param %d = %g, chisq=%f %f" \
				     $index $current_param_val $current_chisq $last_chi]
		}
		if {[expr $current_param_val - $low_range] < $hard_low} {
		    set low_range [expr $current_param_val - $hard_low]
		    puts [format "Reset lower range to hard limit = %g" \
			    $hard_low]
		    if {$logflag} {
			puts $logid [format \
			    "Reset lower range to hard limit = %g" $hard_low]
		    }
		}
		if {[expr $current_param_val + $hi_range] > $hard_hi} {
		    set hi_range [expr $hard_hi - $current_param_val]
		    puts [format "Reset upper range to hard limit = %g" \
			    $hard_hi]
		    if {$logflag} {
			puts $logid [format \
			    "Reset upper range to hard limit = %g" $hard_hi]
		    }
		}
	    }
	} else {
	    puts "No new minimum was found during steppar"
	    puts "Exiting smart_step"
	    if {$logflag} {
		puts $logid "No new minimum was found during steppar"
		puts $logid "Exiting smart_step"
	    }
	    chatter 0
	    if {[file readable $tempfil]} {
		@$tempfil
		exec rm $tempfil
	    }
	    chatter 10
	    set done 1
	    puts ""
	    puts [format "%11s %11s %11s" "Chi-Squared" "Delta" "Mod param"]
	    puts [format "%11s %11s %11s" "" "Chi-Squared" $index]

            set nstepstmp [expr [llength stepvals] / 2 - 1]
	    for {set i 0} {$i <= $nstepstmp} {incr i} {
		set j [expr $i * 2]
		set chi [lindex $stepvals [expr $j + 1]]
#		puts [format "%11.3f %11.3f %11.3g" $chi [expr $chi - \
#			$current_chisq] [lindex $stepvals $j]]
		puts [format "%11.4f %11.4f %11.4g" $chi [expr $chi - \
			$current_chisq] [lindex $stepvals $j]]
	    }
	    
	}
    }

    # set xs_return_result $old_xs_return_result

    if {$logflag} {
	close $logid
    }

    if {[file exists $tempfil]} {
	exec rm $tempfil
    }

    autosave 1
    return $new_min
}

