# modified 10/9/01 ejmc to xspec11 script (from xspec10ejmc script)
# modified 8/8/01 ejmc, keep 4 decimal places on chi2 values so the estimates
#   of guesses and final error are better, changed log file so it appends
#
# A.Ptak, May 1997
# returns [list returnflag, lower bound, upper bound]
# returnflag codes (a better approach would be chmod-style flags):
# 0 aborted abnormally
# & 1 bounds found
# & 2 a new min was found
# & 4 hard limit reached before req. deltachisq for lower bound
# & 8 hard limit reached before req. deltachisq for upper bound
# & 16 lower steppar contains at least one rel. min.
# & 32 upper steppar contains at least one rel. min.
#
# 12/9/98, allowed absolute init_step values to be specified
# 12/21/98, now overwrites log file
# 12/4/00, updated for use with xspec 11

proc smart_error {index args} {

    # For some reason, this script fails unless these helper functions
    # are sourced first, which is bizarre because the error is not
    # along the lines of "function not found" but rather "wrong number
    # of params."
    # First check to see if XSPEC_HOME is defined
    set home [get_xspec_home]
    source "$home/_smart_error_nextstep.tcl"
    source "$home/interpolate.tcl"

    set deltachisq 2.706
    set init_step 1

    if {$index == "-h" || $index == "?"} {
	puts "Usage: smart_error index (optional_params=value)"
	puts "Gets error for fit parameter index, refitting as nec."
	puts "Optional parameters:"
	puts "dchi = desired delta-chisq, default = $deltachisq"
	puts "init_step = initial interval, in units of parameter\'s delta value"
	puts "            if positive, in units of parameter itself if negative"
	puts "            default = $init_step"
	puts "log = log file"
	puts "qdp = save final parameter steps in qdp file based on value"
	puts "param_name = parameter name for use in qdp file name"
	puts "xtitle = xtitle used in qdp file"
	puts ""
	puts "Returns result_flag lower_bound upper_bound"
	puts " result_flag codes:"
	puts " 0 aborted abnormally"
	puts " & 1 bounds found"
	puts " & 2 a new min was found"
	puts " & 4 hard limit reached before req. deltachisq for lower bound"
	puts " & 8 hard limit reached before req. deltachisq for upper bound"
	puts " & 16 lower steppar contains at least one rel. min."
	puts " & 32 upper steppar contains at least one rel. min."
	return 0
    }

    global xs_return_result
    set old_xs_return_result $xs_return_result
    set xs_return_result 1
    
    autosave off
    set returnflag 1

# Check keywords
    set logfil ""
    set logflag 0
    set qdproot ""
    set qdpflag 0
    set param_name "param$index"
    set xtitle "Param $index"
    if {[llength $args] > 0} {
#	puts "Parsing args"
	foreach keyw $args {
#	    puts $keyw
	    set eqpos [string first "=" $keyw]
	    if {$eqpos != -1} {
		set keyw_name [string range $keyw 0 [expr $eqpos - 1]]
		set keyw_val [string range $keyw [expr $eqpos + 1] [expr \
			     [string length $keyw] - 1]]
#		puts $keyw_name
#		puts $keyw_val
	        switch $keyw_name {
		    dchi {
			set deltachisq $keyw_val
		    }
		    log {
#			puts "log keyword is set"
			set logfil $keyw_val
			if {$logfil != ""} {
#			    set logid [open $logfil "w"]
			    set logid [open $logfil "a+"]
			    set logflag 1
			    puts $logid "smart_error $index $args"
			}
		    }
		    init_step {
			set init_step $keyw_val
			puts "Initial step factor = $init_step"
		    }
		    qdp {
			set qdproot $keyw_val
			set qdpflag 1
		    }
		    param_name {
			set param_name $keyw_val
		    }
		    xtitle {
			set xtitle $keyw_val
		    }
		}
	    }
	}
    }

# first get current best fit, delta, hard bounds

    thaw $index
    # set param_vals [show param $index]
    tclout param $index
    set param_vals $xspec_tclout
    set current_param_val [lindex $param_vals 0]
    puts [format "current_param_val = %f" $current_param_val]
    set delta [lindex $param_vals 1]
    set hard_low [lindex $param_vals 2]
    set hard_high [lindex $param_vals 5]
    set dchi_low_list 0
    set dchi_high_list 0
    set param_low_list $current_param_val
    set param_high_list $current_param_val
    set last_lower_guess -999.0
    set last_upper_guess -999.0

#     puts [format "Current param. value = %g" $current_param_val]
#     if {$logflag} {
# 	puts $logid [format "Current param. value = %g" $current_param_val]
#     }

    
    # get sqrt . of delta chisqr value
    set sqrt_dchi [expr sqrt($deltachisq)]

    # initial guess for range of param.
    if {$init_step > 0} {
	set range [expr $delta * $init_step]
    } else {
	set range [expr -1*$init_step]
    }
    if {$logflag} {
	puts $logid [format "Initial range guess = %f" $range]
    }
    set lower_guess [expr $current_param_val - $range]
    if {$lower_guess < $hard_low} {
	set lower_guess $hard_low
    }
    set upper_guess [expr $current_param_val + $range]
#    set range_fact [expr $current_param_val / $range]
    if {$upper_guess > $hard_high} {
	set upper_guess $hard_high
    }

    set low_bound $lower_guess
    set high_bound $upper_guess

    set nsteps 10
    set nstepshi 10
    set nstepslo 10

    set got_low_bound 0
    set got_high_bound 0
    set iterno 0
    set new_fit_count 0
    set low_hard_iter 0
    set high_hard_iter 0

    if {$current_param_val == $hard_low} {
	set low_bound $hard_low
	puts "Current param. value is already at hard min."
	if {$logflag} {
	    puts $logid "Current param. value is already at hard min."
	}
	set dchi_low_list 0
	set param_low_list $current_param_val
	set got_low_bound 1
        set lowlist [list]
	set returnflag [expr $returnflag | 4]
    }

    if {$current_param_val == $hard_high} {
	set high_bound $hard_high
	puts "Current param. value is already at hard max."
	if {$logflag} {
	    puts $logid "Current param. value is already at hard max."
	}
	set dchi_high_list 0
	set param_high_list $current_param_val
	set got_high_bound 1
        set highlist [list]
	set returnflag [expr $returnflag | 8]
    }

    set done [expr $got_low_bound && $got_high_bound]

    # main loop
    while {!$done} {
	set stepsize [expr $range / $nsteps]
	if {$new_fit_count > 100} {
	    puts "Breaking out since new fit count = $new_fit_count"
	    if {$logflag} {
		puts $logid "Breaking out since new fit count = $new_fit_count"
	    }
	    set returnflag 0
	}
#	set current_chisq [format "%.2f" [get_stat]]
	set current_chisq [format "%.4f" [get_stat]]
	puts [format "Current chisq = %f" $current_chisq]
	puts [format "Current parameter value = %g" $current_param_val]
	if {$got_low_bound} {
	    puts [format "Parameter lower bound = %g" $low_bound]
	} else {
	    puts [format "Parameter lower bound guess = %g" $lower_guess]
	}
	if {$got_high_bound} {
	    puts [format "Parameter upper bound = %g" $high_bound]
	} else {
	    puts [format "Parameter upper bound guess = %g" $upper_guess]
	}
	if {$logflag} {
	    puts $logid [format "Current chisq = %f" $current_chisq]
	    puts $logid [format "Current parameter value = %g" \
			     $current_param_val]
	    if {$got_low_bound} {
		puts $logid [format "Parameter lower bound = %g" $low_bound]
	    } else {
		puts $logid [format "Parameter lower bound guess = %g" \
				 $lower_guess]
	    }
	    if {$got_high_bound} {
		puts $logid [format "Parameter upper bound = %g" $high_bound]
	    } else {
		puts $logid [format "Parameter upper bound guess = %g" \
				 $upper_guess]
	    }
	}

	set new_min 0
	if {!$got_low_bound} {
	    if {$logflag} {
		close $logid
	    }
#	    set new_min [smart_step $index $current_param_val $lower_guess \
#		     $nsteps lowlist $logfil]
# for stopping step if dchi is too large (old line below), delete later
# needed for mod to stop at 2*chsqmax value
#           set lowlist [list]
	    set new_min [smart_step $index $current_param_val $lower_guess \
		     $nsteps [expr $deltachisq*2] lowlist $logfil]

	    autosave off
	    if {$logflag} {
		set logid [open $logfil "a+"]
	    }

	    if {$new_min == 1} {
		# set param_vals [show param $index]
		tclout param $index
		set param_vals $xspec_tclout
		set current_param_val [lindex $param_vals 0]
		if {$logflag} {
		    puts $logid "A new min. was found at $current_param_val"
		}
		set upper_guess [expr $current_param_val + $range]
		set lower_guess [expr $current_param_val - $range]
		set got_high_bound 0
		# Modified 10/16/02 to use > rather than == to avoid float prob
		if {$current_param_val < [expr $hard_low + $stepsize]} {
		    set got_low_bound 1
		    set low_bound $current_param_val
		    if {$logflag} {
			puts $logid "The new param. val. is at the hard lower limit"
		    }
		}

		if {$lower_guess < $hard_low} {
		    set lower_guess $hard_low
		}
		set low_hard_iter 0
		set high_hard_iter 0
		set iterno 0
		incr new_fit_count
		set returnflag [expr $returnflag | 2]
		continue
	    } elseif {$new_min == -1} {
		set got_high_bound 1
		set got_low_bound 1
		set done 1
		set returnflag 0
		break
	    }
		
	}
	if {!$got_high_bound} {
	    if {$logflag} {
		close $logid
	    }
#	    set new_min [smart_step $index $current_param_val $upper_guess \
#		     $nsteps highlist $logfil]
# for stopping step if dchi is too large, old line below, delete later
#
# needed for mod to stop at 2*chsqmax value
#           set highlist [list]
	    set new_min [smart_step $index $current_param_val $upper_guess \
 		     $nsteps [expr $deltachisq*2] highlist $logfil]

	    autosave off
	    if {$logflag} {
		set logid [open $logfil "a+"]
	    }
	    if {$new_min == 1} {
		# set param_vals [show param $index]
		tclout param $index
		set param_vals $xspec_tclout
		set current_param_val [lindex $param_vals 0]
		if {$logflag} {
		    puts $logid "A new min. was found at $current_param_val"
		}
		set lower_guess [expr $current_param_val - $range]
		set upper_guess [expr $current_param_val + $range]
		# Modified 10/16/02 to use > rather than == to avoid float prob
		if {$current_param_val > [expr $hard_high - $stepsize]} {
		    set high_bound $current_param_val
		    set got_high_bound 1
		    if {$logflag} {
			puts $logid "The new param. val. is at the hard upper limit"
		    }
		}
		set got_low_bound 0
		set low_hard_iter 0
		set high_hard_iter 0
		if {$upper_guess > $hard_high} {
		    set upper_guess $hard_high
		}
		set iterno 0
		incr new_fit_count
		set returnflag [expr $returnflag | 2]
		continue
	    } elseif {$new_min == -1} {
		set got_high_bound 1
		set got_low_bound 1
		set done 1
		set returnflag 0
		break
	    }
	}

        # We have succesfully steppar'ed without finding new mins.
	# Now at each side find out where our desired value of dchisq is


	if {!$got_low_bound} {
	    set dchi_low_list ""
	    set param_low_list ""
	}
	if {!$got_high_bound} {
	    set dchi_high_list ""
	    set param_high_list ""
	}
	set low_index -1
	set high_index -1
	puts [format "Current chisq = %f" $current_chisq]
	if {$logflag} {
	    puts $logid [format "Current chisq = %f" $current_chisq]
	}

# if stops on 1st real fit (not deltachisq=0), then have 4 entries, and
#   want nstepshi/lo to be 1
        set nstepslo [expr ([llength $lowlist ] / 2) - 1 ]
        set nstepshi [expr ([llength $highlist] / 2) - 1 ]
#       puts $logid "TESTlo nstepslo lowlist =  $nstepslo $lowlist"
#       puts $logid "TESThi nstepshi highlist = $nstepshi $highlist"

	for {set i 0} {$i <= $nsteps} {incr i} {

	    set j [expr $i * 2]

	    if { !$got_low_bound && $i <= $nstepslo } {
		set parval [lindex $lowlist $j]
#		set chi [format "%.2f" [lindex $lowlist [expr $j + 1]]]
		set chi [format "%.4f" [lindex $lowlist [expr $j + 1]]]
		set dchi [expr $chi - $current_chisq]
	        # correct for rounding error
		if {$dchi < 0} {
		    set dchi 0.
		}
		puts [format "%g %f %f" $parval $chi $dchi]
		if {($dchi >= $deltachisq) && ($low_index == -1)} {
		    set low_index $i
		    puts "Found lower bound"
		    if {$logflag} {
			puts $logid "Found lower bound"
		    }
		}
		lappend param_low_list $parval
		lappend dchi_low_list [expr sqrt($dchi)]
	    }

	    if {!$got_high_bound && $i <= $nstepshi } {
		set parval [lindex $highlist $j]
#		set chi [format "%.2f" [lindex $highlist [expr $j + 1]]]
		set chi [format "%.4f" [lindex $highlist [expr $j + 1]]]
		set dchi [expr $chi - $current_chisq]
	        # correct for rounding error
		if {$dchi < 0} {
		    set dchi 0.
		}
		puts [format "%g %f %f" $parval $chi $dchi]
		if {($dchi >= $deltachisq) && ($high_index == -1)} {
		    set high_index $i
		    puts "Found upper bound"
		    if {$logflag} {
		    puts $logid "Found upper bound"
		    }
		}
		lappend param_high_list $parval
		lappend dchi_high_list [expr sqrt($dchi)]
	    }
	}

	# If we bracketed the desired dchisq, intepolate, if not extrapolate
	if {!$got_low_bound} {
	    puts "_smart_error_nextstep $low_index $current_param_val \
		\"$param_low_list\" \"$dchi_low_list\" $sqrt_dchi $nstepslo $hard_low \
		$low_bound $got_low_bound"
	    _smart_error_nextstep $low_index $current_param_val \
		"$param_low_list" "$dchi_low_list" $sqrt_dchi $nstepslo $hard_low \
		low_bound got_low_bound
	    if {!$got_low_bound} {
		set lower_guess $low_bound
		if {$lower_guess == $last_lower_guess} {
		    set lower_guess [expr $lower_guess * 0.9]
		}
		puts [format "New lower guess = %g" $lower_guess]
		if {$logflag} {
		    puts $logid [format "New lower guess = %g" $lower_guess]
		}
		set last_lower_guess $lower_guess
		if {$lower_guess < $hard_low} {
		    if {$low_hard_iter == 0} {
			set lower_guess $hard_low
			incr low_hard_iter
			puts [format "Resetting lower guess to hard limit = %f" \
				  $hard_low]
			if {$logflag} {
			    puts $logid [format "Resetting lower guess to hard limit = %f" \
				  $hard_low]
			}
		    } else {
			set low_bound $hard_low
			set got_low_bound 1
			set dchi [lindex $dchi_low_list [expr $nstepslo - 1]]
			puts [format "Hit hard lower limit with delta chisq = %f" \
				  [expr $dchi * $dchi]]
			if {$logflag} {
			    puts $logid [format "Hit hard lower limit with delta chisq = %f" \
					     [expr $dchi * $dchi]]
			}
			set returnflag [expr $returnflag | 4]
		    }
		} else {
			set low_hard_iter 0
		}
	    }
	}
	
	if {!$got_high_bound} {
# need to insert puts here later 
	    _smart_error_nextstep $high_index $current_param_val \
		$param_high_list $dchi_high_list $sqrt_dchi $nstepshi \
		$hard_high high_bound got_high_bound
	    if {!$got_high_bound} {
		set upper_guess $high_bound
		if {$upper_guess == $last_upper_guess} {
                    set upper_guess [expr $upper_guess * 1.1]
                }
		puts [format "New upper guess = %g" $upper_guess]
		if {$logflag} {
		    puts $logid [format "New upper guess = %g" $upper_guess]
		}
		set last_upper_guess $upper_guess
		if {$upper_guess > $hard_high} {
		    if {$high_hard_iter == 0} {
			set upper_guess $hard_high
			incr high_hard_iter
			puts [format "Resetting upper guess to hard limit = %f" \
				  $hard_high]
			if {$logflag} {
			    puts $logid [format "Resetting upper guess to hard limit = %f" \
				  $hard_high]
			}
		    } else {
			set high_bound $hard_high
			set got_high_bound 1
			set dchi [lindex $dchi_high_list [expr $nstepshi - 1]]
			puts [format "Hit hard upper limit with delta chisq = %f" \
				[expr $dchi * $dchi]]
			if {$logflag} {
			    puts $logid [format "Hit hard upper limit with delta chisq = %f" \
					     [expr $dchi * $dchi]]
			}
			set returnflag [expr $returnflag | 8]
		    }
		} else {
		    set high_hard_iter 0
		}
	    }
	}
	set done [expr $got_low_bound && $got_high_bound]
	incr iterno
	puts "smart_error iteration no. $iterno"
	if {$logflag} {
	    puts $logid "smart_error iteration no. $iterno"
	}
	if {$iterno > 50} {
	    if {$logflag} {
		puts $logid "I must be confused"
	    }
	    puts "I must be confused"
            set returnflag 0
            break
	}
    }
    if {$returnflag & 1} {
	puts [format "Final error range: (%g - %g)" $low_bound $high_bound]
	if {$logflag} {
	    puts $logid [format "Final error range: (%g - %g)" \
			     $low_bound $high_bound]
	}

# check for rel. min.
	set last_val 0
	foreach val $dchi_low_list {
	    if {$val < $last_val} {
		set returnflag [expr $returnflag | 16]
	    }
	    set last_val $val
	}
	set last_val 0
	foreach val $dchi_high_list {
	    if {$val < $last_val} {
		set returnflag [expr $returnflag | 32]
	    }
	    set last_val $val
	}

	if {$qdpflag} {
	    if {[string length $qdproot] == 0 && $logflag} {
		set dotpos [string last "." $logfil]
		if {$dotpos == -1} {
		    set dotpos [string length $logfil]
		}
		set qdproot [string range $logfil 0  [expr $dotpos - 1]]
	    }
 	    set qdpfil [format "%s_%s.qdp" $qdproot $param_name]
	    if {$logflag} {
		puts $logid "Saving qdp file in $qdpfil"
	    }
	    set x [concat $param_low_list $param_high_list]
	    set y [concat $dchi_low_list $dchi_high_list]
	    #puts "x = $x"
# qdp2 seems to be causing errors for xspec11, so comment out
#  ejmc 10/9/01
#	    puts "qdp2 $qdpfil \"$x\" \"$y\" \"lab x $xtitle\" \
#		\"lab y sqrt(\\gD\\gx\\u2\\d)\" \
#		\"label 1 line 0 10 ls 2 just left pos [lindex [lsort \
#		-real $x] 0] $sqrt_dchi\""
#	    qdp2 $qdpfil $x $y "lab x $xtitle" \
#		    "lab y sqrt(\\gD\\gx\\u2\\d)" \
#		    "label 1 line 0 10 ls 2 just left pos [lindex [lsort \
#		    -real $x] 0] $sqrt_dchi"
	}
    }

    if {$logflag} {
        close $logid
    }

    if {$returnflag == 0} {
	set low_bound $lower_guess
	set high_bound $upper_guess
    }
    autosave 1
    return [list $returnflag $low_bound $high_bound]
}


