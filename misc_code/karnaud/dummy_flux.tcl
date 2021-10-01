proc dummy_flux {args} {
# A. Ptak, Apr 2002
# Revised 8/21/02 to properly handle case when more that one dataset is
# in memory

    if {[lindex $args 0] == "?" || [lindex $args 0] == "-h" || [llength $args] < 2} {
	puts "Usage: dummy_flux elo ehi"
	puts "Returns the flux from elo to ehi, using a dummy response so that the"
	puts "flux will be computed over the full energy range requested, regardless of the"
	puts "current response.  The original response is restored."
	return
    }
    set elo [lindex $args 0]
    set ehi [lindex $args 1]

    tclout datagrp
    set ngrps $xspec_tclout
    #puts "dummyrsp $elo $ehi"
    dummyrsp $elo $ehi
    #puts "flux $elo $ehi"
    flux $elo $ehi
    set f ""
    for {set i 1} {$i <= $ngrps} {incr i} {
	tclout flux $i $elo $ehi
	lappend f $xspec_tclout
    }
    #puts "Flux = $f"
    response
    return $f
}
