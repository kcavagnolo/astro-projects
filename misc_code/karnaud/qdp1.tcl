proc qdp1 {qdpfile values args} {
# Creates a qdp file using values, where values is a list with elements
# "x1 y1 x2 y2 etc"
# optional args gives x title, y title
# by A. Ptak (ptak@neutron.gsfc.nasa.gov) Apr 1997 

    set nval [llength $values]
    puts [format "Number of values = %g" [expr $nval / 2]]

    if {$nval < 4} {
	puts "values should have at least two points"
#	puts $nval
	return
    }

    if {[expr $nval % 2] != 0} {
	puts "values must have an even number of elements"
    }

    set fileid [open $qdpfile "w"]
    puts $fileid "mark 17 on 2"
    if {[llength $args] > 0} {
      puts $fileid [format "label x %s" [lindex $args 0]]
    }
    if {[llength $args] > 1} {
      puts $fileid [format "label y %s" [lindex $args 1]]
    }

    for {set i 0} {$i < [expr $nval / 2]} {incr i} {
	set j [expr $i * 2]
	puts $fileid [format "%g %g" [lindex $values $j] [lindex $values \
		[expr $j + 1]]]
    }

    close $fileid

}

