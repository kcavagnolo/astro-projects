proc get_stat {args} {

    if {[lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
	puts "convenience function to return the current fit statistic value"
    }

    tclout stat
    return $xspec_tclout

}
