proc interpolate {x1 y1 x2 y2 x} {
    if {$x1 == $x2} {
	return $y1
    }
    set dx [expr $x2 - $x1]
    set dy [expr $y2 - $y1]
    set slope [expr $dy / $dx]
    return [expr $y2 + $slope * ($x - $x2)]
}
