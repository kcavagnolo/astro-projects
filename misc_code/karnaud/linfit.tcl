# andy: this is my interpolation program
# I couldn't figure out what was wrong so i just rewrote it
# and avoided nesting expressions.
# It seems to work fine, but let me know if you see anything wrong with
# it.  
#

proc interpolate {X Y x} {


set length_string [llength $X]
set sigma .1

#calculate S

set S_total 0.0

for {set n 0} {$n < $length_string} {incr n} {
	set S [expr $S_total + [expr 1 / $sigma]]
	set S_total $S
}

#calculate Sx

set Sx_total 0.0

for {set n 0} {$n < $length_string} {incr n} {
	set Xloop [lindex $X $n]
	set XoverSigma [expr $Xloop / $sigma]
	set Sx [expr $Sx_total + $XoverSigma]
	set Sx_total $Sx
}

#calculate Sy
set Sy_total 0.0

for {set n 0} {$n < $length_string} {incr n} {
	set Yloop [lindex $Y $n]
	set YoverSigma [expr $Yloop / $sigma]
	set Sy [expr $Sy_total + $YoverSigma]
	set Sy_total $Sy
}

#calculate Sxx
set Sxx_total 0.0

for {set n 0} {$n < $length_string} {incr n} {
	set Xloop [lindex $X $n]
	set XX [expr $Xloop * $Xloop]
	set XXoverSigma [expr $XX / $sigma]
	set Sxx [expr $Sxx_total + $XXoverSigma]
	set Sxx_total $Sxx
}

#calculate Sxy
set Sxy_total 0.0

for {set n 0} {$n < $length_string} {incr n} {
	set Yloop [lindex $Y $n]
        set Xloop [lindex $X $n]
        set XY [expr $Xloop * $Yloop]
        set XYoverSigma [expr $XY / $sigma]
        set Sxy [expr $Sxy_total + $XYoverSigma]
        set Sxy_total $Sxy
}

#calculate delta

set delta [expr [expr $S * $Sxx] - [expr $Sx * $Sx]]

#calculate a
set partonea [expr $Sxx * $Sy]
set parttwoa [expr $Sx * $Sxy]
set a [expr [expr $partonea - $parttwoa] / $delta]

#calculate b
set partoneb [expr $S * $Sxy]
set parttwob [expr $Sx * $Sy]
set b [expr [expr $partoneb - $parttwob] / $delta]
puts "$a $b"
return [expr $a + [expr $b * $x]]

}
