# modified ejmc 10/9/01 included mod from xspec10ejmc script to this xspec11 
#   script, changed overshoot value 1.1->1.2
# modified ejmc 8/8/01 to intentionally overshoot the chisquare value by 
#   1.1*1.1 = 1.21, so will not be as likely to have to re-do the step 
#   sequence if interpolation underestimates the guess value

proc _smart_error_nextstep {index current_param_val param_list dchi_list \
			       sqrt_dchi nsteps hard_bound bound got_bound} {
# This should be a member function in an O-O implimentation of TCL
    puts "In smart_error_nextstep"
#    puts [format "hard_bound = %g" $hard_bound]
    upvar $got_bound ref_got_bound
#    puts [format "ref_got_bound = %d" $ref_got_bound]
    upvar $bound ref_bound
    if {$index != -1} {
	set ref_got_bound 1
	if {$index == 0} {
	    set x1 0
	    set y1 $current_param_val
	} else {
	    set x1 [lindex $dchi_list [expr $index - 1]]
	    set y1 [lindex $param_list [expr $index - 1]]
	}
	set x2 [lindex $dchi_list $index]
	set y2 [lindex $param_list $index]
	puts [format "interpolate %g %g %g %g %g" $x1 $y1 $x2 $y2 $sqrt_dchi]
	set ref_bound [interpolate $x1 $y1 $x2 $y2 $sqrt_dchi]
    } else {
        # index = -1 
        # no bound found, so interpolate/extrapolate to the desired chisq 
        # value; if extrapolating, overshoot it intentionally
	set x1 [lindex $dchi_list [expr $nsteps - 1]]
	set y1 [lindex $param_list [expr $nsteps - 1]]
	set x2 [lindex $dchi_list [expr $nsteps]]
	set y2 [lindex $param_list [expr $nsteps]]
#	set dchi [expr ($x2 - $x1) * ($x2 - $x1)]
 	if {$x2 <= $x1} {    ;# rel. min or delta chisq. was too small
 	    set ref_bound [expr (2. * $y2) - $current_param_val]
 	} else {
            set fake_sqrt_dchi [expr 1.2 * $sqrt_dchi]
#           puts "TEST x1 y1 x2 y2 fake_sqrt_dchi $x1 $y1 $x2 $y2 \
#                   $fake_sqrt_dchi"
	    puts [format "interpolate %g %g %g %g %g" $x1 $y1 $x2 $y2 \
		    $fake_sqrt_dchi]
	    set ref_bound [interpolate $x1 $y1 $x2 $y2 $fake_sqrt_dchi]
 	}
    }
}

