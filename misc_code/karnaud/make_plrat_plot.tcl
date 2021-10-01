proc make_plrat_plot {filename sig maxnbins args} {
# Make a "pretty" ratio plot for the given model

# Get args
    set yr "0.91:1.09"
    set dev "vps"
    set qdp_cmds ""
    if {[llength $args] > 0} {
#	puts "Parsing args"
	foreach keyw $args {
#	    puts $keyw
	    set eqpos [string first "=" $keyw]
	    if {$eqpos != -1} {
		set keyw_name [string range $keyw 0 [expr $eqpos - 1]]
		set keyw_val [string range $keyw [expr $eqpos + 1] [expr \
			     [string length $keyw] - 1]]
#		puts $keyw_name
#		puts $keyw_val
	        switch $keyw_name {
		    yr {
			set yr $keyw_val
		    }
		    dev {
			set dev $keyw_val
		    }
		    qdp_cmds {
			set qdp_cmds $keyw_val
		    }
	       }
	    }
	}
    }

    regsub -all "_" $qdp_cmds " " qdp_cmds
    set yr [split $yr ":"]
    @$filename
    setplot rebin $sig $maxnbins
    puts "y-range = $yr"
    cpd /$dev
    setplot command "label F"
    setplot command "label T"
    setplot command "label X E (keV)"
    setplot command "label Y Data/Model"
    setplot command mark 17 on 1
    setplot command mark size 2 on 1
#    setplot command mark 18 on 2
#    setplot command mark size 2 on 2
#    setplot command mark 22 on 3
#    setplot command mark size 2 on 3
#    setplot command mark 12 on 4
#    setplot command mark size 2 on 4
    setplot command time off
    setplot command paper 14.0 1.0
#    setplot command rescale x 0.4 10.0
    setplot command rescale y $yr
    foreach cmd $qdp_cmds {
	puts $cmd
	setplot command $cmd
    }
#    setplot command we $filename\_rat.qdp
    plot rat
    if {[file exists pgplot.gif_2]} {
	exec mv pgplot.gif_2 pgplot.gif
    }
    set dev [string trimleft $dev "v"]
    exec mv pgplot.$dev $filename.$dev
    for {set i 0} {$i < 15} {incr i} {
	setplot delete
    }
}

