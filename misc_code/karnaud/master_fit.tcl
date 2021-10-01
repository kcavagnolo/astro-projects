proc master_fit {fitfile rootname} {
# Run setup_mod and smart_fit using the spectra in memory
# Saves initial fit as $rootname_$modname.xcm, logroot=$rootname_$modname
# Format of "fitfile":
# energy_lo energy_hi
# model1
# model2
# etc.
# end
# energy_lo energy_hi
# etc.
#
# First does "ignore 0.0-$energy_lo" and "ignore "$energy_hi-**"
# If an xcm file for a given fit already exists, then that is used,
# otherwise setup_mod.tcl is used to setup the model and start the fitting

    set logfil [open [format "%s_master_%s.log" $rootname [pid]] "w"]
    if {![file readable $fitfile]} {
	puts "Error: cannot read $fitfile"
	puts $logfil "Error: cannor read $fitfile"
	return
    }

    set fil [open $fitfile "r"]
    while {![eof $fil]} {
	gets $fil erange
	if {$erange != ""} {
	    ignore 0.0-[lindex $erange 0]
	    puts $logfil "ignore 0.0-[lindex $erange 0]"
	    ignore [lindex $erange 1]-**
	    puts $logfil "ignore [lindex $erange 1]-**"
	    gets $fil modl
	    while {$modl != "end"} {
		puts $logfil $modl
		flush $logfil
		set savefile [format "%s_%s_%.1f-%.1fkev" $rootname $modl \
				  [lindex $erange 0] [lindex $erange 1]]
		if {[file exists $savefile.log]} {
		    puts $logfil "$savefile.log exists, skipping this fit"
		    flush $logfil
		} else {
		    puts $logfil "$savefile.log does not exist... proceeding"
		    flush $logfil
		    if {![file exists $savefile.xcm]} {
			setup_mod $modl
			query y
			fit 200 0.0001
			save all $savefile.xcm
			puts $logfil $savefile.xcm
		    } else {
			if {[file exists $savefile\_new.xcm]} {
			    puts $logfil "$savefile\_new.xcm exists, reading it in"
			    @$savefile\_new.xcm
			} else {
			    puts $logfil "$savefile exists... reading it in"
			    @$savefile.xcm
			}
		    }
		    flush $logfil
		    smart_fit $modl logroot=$savefile
		}
		gets $fil modl
	    }
	}
    }
    close $fil
    close $logfil
    return
}
