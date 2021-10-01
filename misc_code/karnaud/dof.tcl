proc dof {args} {

    if {[lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
	puts "dof returns no. of degrees of freedom in current model"
	return
    }
    

    tclout dof
    return $xspec_tclout

}
