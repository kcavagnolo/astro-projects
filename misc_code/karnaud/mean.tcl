proc mean {vals} {

    set tot 0.
    foreach val $vals {
	set tot [expr $tot + $val]
    }

    return [expr $tot / [llength $vals]]
}
