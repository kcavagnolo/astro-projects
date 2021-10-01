proc qdp2 {qdpfile x y args} {
# Creates a qdp file using values in the lists x and y
# optional args are appended prior to data for extra formatting
# by A. Ptak (ptak@neutron.gsfc.nasa.gov) Apr 1997 

    puts "qdpfile = $qdpfile"
    puts "x = $x"
    puts "y = $y"

    set nval [llength $x]
    if {$nval != [llength $y]} {
	puts "Error: x and y lists are not the same length"
	return
    }

    puts [format "Number of values = %g" $nval]

    puts "qdpfile = $qdpfile"
    if {$nval < 2} {
	puts "x should have at least two points"
	puts $nval
	puts "x = $x"
	puts "y = $y"
	return
    }

    set fileid [open $qdpfile "w"]
    puts $fileid "mark 17 on 2"
    foreach item $args {
	puts $fileid $item
    }

    for {set i 0} {$i < $nval} {incr i} {
	puts $fileid [format "%g %g" [lindex $x $i] [lindex $y $i]]
    }

    close $fileid

}

