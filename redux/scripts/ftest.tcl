proc ftest2 { args } {
# calculates the F test probability for adding a line to a model
# kaa 9/23/04

# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
	puts "Usage: ftest component iterations"
	puts "Calculates the F test probability for the specified component"
	puts "ftest2 <niter>"
	puts " "
	return
    }

# now check that the user actually has data read in and a model set up

    chatter 1
    data ABELL_0478_1669_r2500-50_src1_grp.pi
    ignore bad
    ignore **-0.7 7.0-**
    model wabs(mekal) & 0.15,0 & 1.0 & 1.0 & 0.3,1 & 0.0883 & 0.1
    fit 1000 0.1
    exec rm -f ftest_data.xcm
    save files ftest_data.xcm

# parse the arguments - first is the component number for the line
# and the second the number of iterations. The first is required.
    set niter 1000
    if {[llength $args] > 0} {
       set niter [lindex $args 0]
    }
    thaw 1
    fit 1000 0.1
    exec rm -f ftest_model.xcm
    save model ftest_model.xcm

# fit with the current model and save the statistic and dof
    tclout stat
    set statnhfree $xspec_tclout
    tclout dof
    set dofnhfree $xspec_tclout
    puts [format "** with NH-free %f %d" $statnhfree $dofnhfree]

# save the number of model parameters for the null hypothesis
    tclout modpar
    set npars $xspec_tclout

# thaw nh, fit and save the statistic and dof
    newpar 1 0.15,0
    fit 1000 0.1
    tclout stat
    set statnhfro $xspec_tclout
    tclout dof
    set dofnhfro $xspec_tclout
    puts [format "** with NH-frozen %f %d" $statnhfro $dofnhfro]

# calculate the F value for the real data
    set Fvalue [expr ($statnhfro-$statnhfree)/($dofnhfro-$dofnhfree)/($statnhfree/$dofnhfree)]
    puts [format "** F_data = %f" $Fvalue]

# put back the original model
    @ftest_model.xcm
    set txtfile "test.dat"
    exec rm -f $txtfile
    set fileid [open $txtfile w]
    set outstr " "
    append outstr [scan $Fvalue "%f"] " "
    puts $fileid $outstr

# start iteration round simulations
    set counter 0
    for {set iter 1} {$iter <= $niter} {incr iter} {
	puts [format "** iteration %d" $iter]

# get a set of model parameters and load into a Tcl list
	fit 1000 0.1
	tclout simpars
	regsub -all { +} [string trim $xspec_tclout] { } csimpars
        puts [format "** simulated parameters %s" $csimpars]
	set lsimpars [split $csimpars]

# load the parameters
	for {set ipar 1} {$ipar <= $npars} {incr ipar} {
	    set iparm1 [expr $ipar-1]
	    set parval [lindex $lsimpars $iparm1]
	    newpar $ipar $parval
	}

# fake the data
	exec rm -f test.fak
	set pstring "& y & & test.fak & /"
	fakeit $pstring

# fit and save the statistic
	fit 1000 0.1
        tclout stat
        set statnhfree $xspec_tclout
	tclout dof
	set dofnhfree $xspec_tclout
        puts [format "** with NH-free %f %d" $statnhfree $dofnhfree]

# fit and save the statistic
	newpar 1 0.15,0
	fit 1000 0.1
        tclout stat
        set statnhfro $xspec_tclout
	tclout dof
	set dofnhfro $xspec_tclout
        puts [format "** with NH-frozen %f %d" $statnhfro $dofnhfro]

# calculate the F value
	set Fsimvalue [expr ($statnhfro-$statnhfree)/($dofnhfro-$dofnhfree)/($statnhfree/$dofnhfree)]
        puts [format "** F_sim = %f" $Fsimvalue]

# write the free parameter values and fit statistic to the file. Note
# can use the saved parameter deltas to know which parameter should
# be written
	set outstr " "
	append outstr [scan $Fsimvalue "%f"] " "
	puts $fileid $outstr

# if this exceeds the F value from the original data then increment the counter
	if { $Fsimvalue > $Fvalue } {
	    incr counter
	}

# read the original data back in and refit to recover initial position
	@ftest_data.xcm
	@ftest_model.xcm

# end of iterations over simulations
    }

# write out the F test probability
    close $fileid
    puts [format "F test probability = %f" [expr {double($counter)/double($niter)}]]
    set outstr " "
    append outstr [scan [expr {double($counter)/double($niter)}] "%f"] " "
    puts $fileid $outstr

# reset the chatter level
    chatter 10
}
