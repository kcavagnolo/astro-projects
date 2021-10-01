proc fek_search {root args} {
# A. Ptak, June 1997
# Searches for Fe-K emission around 6.4 keV, first with E free
# if delta chisq is too small, gets upper limits for emission at 6.4 and 6.7
# keV

    if {$root == "-h" || $root == "?"} {
	puts "Usage: fek_search root (skipe=skipe skips=skips ewonly= z=z dchi=dchi)"
	puts "Searches for Fe-K emission around 6.4 keV, first with E free"
	puts "If delta chis is too small, gets upper limits for emission at 6.4 and"
	puts "and 6.7 keV.  Checks for lines with sigma = 0.01, 0.1, 0.5 keV"
	puts "log kept in root + .log"
	puts "If sign. Fe-K line is found, file is saved in root+fek.xcm"
	return
    }

# Get keywords
    set skipe ""
    set skips ""
    set ewonly 0
    set z 0.
    set dchi 4.605
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
		    skipe {
			set skipe [split $keyw_val _]
		    }
		    skips {
			set skips [split $keyw_val _]
		    }
		    ewonly {
			set ewonly 1
		    }
		    z {
			set z $keyw_val
		    }
		    dchi {
			set dchi $keyw_val
		    }
		}
	    }
	}
    }

    global xs_return_result
    set old_xs_return_result $xs_return_result
    set xs_return_result 1
    
    set sigmas "free 0.01 0.1 0.5"
    set energies "free 6.4 6.7"
    global xs_return_result
    set old_xs_return_result $xs_return_result
    set xs_return_result 1
    
    autosave off
    set logid [open "$root.log" "a"]
    # get orig. chisq
    set chisq [show param]
    puts "Original chisq = $chisq"
    puts $logid "Original chisq = $chisq"
#    set tempfil "fek_search_[pid].xcm"
#    save model $tempfil
    set skip ""
    if {$ewonly} {
	set skip "Nh_Gamma_E_Sigma"
	puts $logid "Only getting errors on EW"
    }
    query y
    foreach sigma $sigmas {
	set skipflag 0
	foreach skipval $skips {
	    if {$skipval == $sigma} {
		set skipflag 1
	    }
	}
	if {$skipflag} {
	    puts $logid "Skipping sigma=$sigma fits"
	    puts "Skipping sigma=$sigma fits"
	    continue
	}

	foreach E $energies {
	    set skipflag 0
	    foreach skipval $skipe {
		if {$skipval == $E} {
		    set skipflag 1
		}
	    }
	    if {$skipflag} {
		puts $logid "Skipping E=$E fits"
		puts "Skipping E=$E fits"
		continue
	    }

	    puts "Searching for line emission with sigma = $sigma, E = $E"
	    puts $logid "Searching for line emission with sigma = $sigma, E = $E"
	    
	    set saveroot [format "%s_s%s_e%s" $root $sigma $E]
	    set newfil [format "%s_new.xcm" $saveroot]
	    set fitdone 0
	    if {[file readable $newfil]} {
		puts $logid "Reading previous fit $newfil"
		@$newfil
		set fitdone 1
	    } elseif {[file readable $saveroot.xcm]} {
		puts $logid "Reading previous fit $saveroot.xcm"
		@$saveroot.xcm
		set fitdone 1
	    }

	    if {!$fitdone} {
		setup_mod plzga
		# Set limits on energy range to keep line from "running away"
		newpar 5 6.4 0.01 5.4 6.0 7.0 7.4
		# Also keep sigma reasonable
		newpar 6 0.01 0.01 0 0 1.0 1.0
		newpar 7 $z
		if {$sigma != "free"} {
		    newpar 6 $sigma
		    freeze 6
		}
		
		if {$E != "free"} {
		    newpar 5 $E
		    freeze 5
		}
		fit 200 0.0001
		save all $saveroot.xcm
		puts $logid "Saved all in $saveroot"
	    }
	    flush $logid
	    if {$ewonly} {
		append saveroot "_ew"
	    }
	    if {$E == "free"} {
		if {$sigma == "free"} {
		    smart_fit plzga logroot=$saveroot skip=$skip dchi=$dchi
		} else {
		    smart_fit plzga_sfrozen logroot=$saveroot skip=$skip dchi=$dchi
		}
	    } else {
		if {$sigma == "free"} {
		    smart_fit plzga_efrozen logroot=$saveroot skip=$skip dchi=$dchi
		} else {
		    smart_fit plzga_esfrozen logroot=$saveroot skip=$skip dchi=$dchi
		}
	    }
	}
    }

    close $logid
    autosave 1
    return 
    set xs_return_result $old_xs_return_result

}

