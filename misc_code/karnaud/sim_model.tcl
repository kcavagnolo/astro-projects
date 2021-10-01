proc sim_model {xcmfil args} {
# A. Ptak, Sept. 1997
# Modified 12/21/98 to save pid in sim_model.pid
# Modified 1/21/99 to actually impliment fitmod option
# Modified 2/1/99 to (hopefully) no quit when qdp files are not found

    global xs_return_result
    set xs_return_result 1
    set logroot ""
    set l [llength $args]
    set expt 20000.0
    set grpmin 20
    set sis_ig_lowe 0
    if {$xcmfil == "-h" || $xcmfil == "?" || $l < 3} {
        puts "Usage: sim_model xcmfil model nsims logroot grpmin=grpmin expt=expt noerr= setup= sis_ig_lowe="
	puts "Warning... input spectra must be unbinned!"
	puts "Simulates spectrum(a) in xcmfil nsims times, rebinning spectra"
	puts "to grpmin counts per bin before smart_fitting model"
	puts "Set grpmin to 0 to turn off rebinning"
	puts "Results from each iteration n are stored in logroot + _n.log"
	puts "Default expt = $expt"
	puts "Set noerr= if you only want fitting to be done, without running smart_fit"
#	puts "If fitmod is given, fit sim. of model using fitmod"
	puts "If setup=, do setup_mod model before fitting"
	puts "If modnorms is given, set params in modnorms to zero to also sim. bgd."
	puts "  Format = 1_2_5 etc."
	puts "If sis_ig_lowe=, ignore channels 14-21"
	return
    } 

    autosave off
    set model [lindex $args 0]
    set nsims [lindex $args 1]
    set logroot [lindex $args 2]
    if {$l > 3} {
	set args [lrange $args 3 [expr $l - 1]]
    } else {
	set args ""
    }

# Get keywords
    set grpmin 0
    set igstr ''
    set igflag 0
    set geterrors 1
    set setupmod 0
    set modnorms ""
    set fitmod ""
    if {[llength $args] > 0} {
	foreach keyw $args {
	    set eqpos [string first "=" $keyw]
	    if {$eqpos != -1} {
		set keyw_name [string range $keyw 0 [expr $eqpos - 1]]
		set keyw_val [string range $keyw [expr $eqpos + 1] [expr \
			[string length $keyw] - 1]]
	        switch $keyw_name {
		    expt {
			set expt $keyw_val
		    }
		    fitmod {
			set fitmod $keyw_val
		    }
		    grpmin {
			set grpmin $keyw_val
		    }
		    igstr {
			set igstr $keyw_val
			set igflag 1
		    }
		    modnorms {
			set modnorms $keyw_val
		    }
		    noerr {
			set geterrors 0
		    }
		    setup {
			set setupmod 1
		    }
		    sis_ig_lowe {
			set sis_ig_lowe 1
		    }
		}
	    }
	}
    }

    set modnorms [split $modnorms _]
    set logfil [format "%s.log" $logroot]
    set logid [open $logfil "w"]

    puts $logid "sim_model $xcmfil $args"
    puts $logid "modnorms = $modnorms"
    puts $logid "Exposure time = $expt"
    if {$grpmin > 0} {
	puts $logid "Grouping data to $grpmin counts/bin"
    } else {
	puts $logid "No rebinning will be done"
    }

# Clean up from any previous run
    if {[file exists "sim_model.pid"]} {
	set tempid [open sim_model.pid "r"]
	if {[gets $tempid lastpid] > 0} {
	    puts $logid "Last pid = $lastpid"
	    set flist [glob -nocomplain *$lastpid*]
	    foreach fil $flist {
		puts $logid "rm $fil"
		flush $logid
		file delete $fil
	    }
	}
	close $tempid
    }

# Create directories to hold the fakeit files, log files and xcm files
    if {![file isdirectory fak]} {
	file mkdir fak
    }
    if {![file isdirectory log]} {
	file mkdir log
    }
    if {![file isdirectory xcm]} {
	file mkdir xcm
    }
    if {![file isdirectory qdp]} {
	file mkdir qdp
    }

# Figure out how many data files there are
    set xcmid [open $xcmfil "r"]
    set datfils ""
    gets $xcmid line
    puts "Reading data file names from $xcmfil..."
    puts $logid "Reading data file names from $xcmfil..."
    flush $logid
    while {[lindex $line 0] == "data"} {
	lappend datfils fak/[file rootname [file tail \
						[lindex [split $line  \"] 1]]]
	gets $xcmid line
    }
    close $xcmid
    set ndata [llength $datfils]

    puts "Number of data files = $ndata"
    puts $logid "Number of data files = $ndata"
    foreach datafil $datfils {
	if {[file readable $datafil.fak]} {
	    puts "Deleting $datafil.fak"
	    puts $logid "Deleting $datafil.fak"
	    flush $logid
	    exec rm $datafil.fak
	}
    }


    # if fitmod is given, save it to a temp file (the main motivation here
    # is that sim_model can run for a while, and the original files might
    # be changed at some point during the run.  This ensures that all of
    # the simulations pertain to the same input.
    set fitmodelfil "sim_model_fit_[pid].xcm"
    if {$fitmod != ""} {
	source $fitmod
	save model $fitmodelfil
    }
    source $xcmfil
    set fakeitfil "sim_model_fakeit_[pid].xcm"
    set simmodelfil "sim_model_mod_[pid]_2.xcm"
    set tempid [open "sim_model.pid" "w"]
    puts $tempid [pid]
    close $tempid

    save model $simmodelfil
    set fakeitid [open $fakeitfil "w"]
    puts $fakeitid fakeit
    puts $fakeitid "y"
    puts $fakeitid "none"
    foreach datfil $datfils {
	puts $fakeitid $datfil.fak
	puts $fakeitid $expt
    }
    puts $fakeitid ""
    close $fakeitid

    if {[llength $modnorms] > 0} {
	puts $logid "The background will be simulated"
	set bgdfakeitfil "sim_model_bgdfakeit_[pid].xcm"
	set bgdfakeitid [open $bgdfakeitfil "w"]
	puts $bgdfakeitid fakeit
	puts $bgdfakeitid "y"
	puts $bgdfakeitid "none"
	foreach datfil $datfils {
	    puts $bgdfakeitid $datfil\_bgd.fak
	    puts $bgdfakeitid $expt
	}
	puts $fakeitid ""
	close $fakeitid
    }
    
    query y
	
    for {set j 0} {$j < $nsims} {incr j} {
	puts $logid "Iteration [expr $j + 1]"
	if {[file exists [format "xcm/%s_%s.xcm.gz" $logroot $j]]} {
	    puts $logid "[format "xcm/%s_%s.xcm.gz" $logroot $j] exists, skipping"
	    continue
	}
	puts $logid "Generating fake spectra"
	foreach datfil $datfils {
	    if {[file readable $datfil.fak]} {
		puts "Deleting $datfil.fak"
		puts $logid "Deleting $datfil.fak"
		flush $logid
		exec rm $datfil.fak
	    }
	}	
	# Read in data and model
	source $xcmfil
	puts $logid "Channels noticed after reading $xcmfil :"
	puts $logid [show notice]
	flush $logid
	# Do simulation
	puts $logid "Executing fakeit commands"
	source $fakeitfil
	# Save simulated spectra
	foreach datfil $datfils {
	    exec mv $datfil.fak $datfil\_$j.fak
	}
	# Fake bgd. also, if requested
	if {[llength $modnorms] > 0} {
	    puts $logid "Simulating background"
	    # Set norms to zero
	    foreach param $modnorms {
		new $param 0.
	    }
	    # Restore full channel rnge
	    set i 1
	    foreach datfil $datfils {
		if {$grpmin > 0} {
		    if {[string first "_s0_" $datfil ] != -1 || \
			    [string first "_s1_" $datfil ] != -1} {
			puts $logid "notice $i: 1-512"
			notice $i: 1-512
		    } else {
			puts $logid "notice $i: 1-1024"
			notice $i: 1-1024
		    }
		}
	    }
	    source $bgdfakeitfil
	    # Save simulated bgd. files
	    foreach datfil $datfils {
		exec mv $datfil\_bgd.fak $datfil\_bgd_$j.fak
	    }
	}
	# Now read sim. source spect., grp. data if nec.
	set i 1
	foreach datfil $datfils {
	    if {$grpmin > 0} {
		if {[string first "_s0_" $datfil ] != -1 || \
			[string first "_s1_" $datfil ] != -1} {
		    if {$sis_ig_lowe} {
			set bad "bad 1-21 343-512"
		    } else {
			set bad "bad 1-13 343-512"
		    }
		    set sis 1
		} else {
		    set bad "bad 0-69 850-1023"
		    set sis 0
		}
		set binfil [format "%s_b$grpmin.fak" $datfil]
		puts "$datfil $bad $binfil"
		if {[file exists $binfil]} {
		    puts "Deleting $binfil"
		    puts $logid "Deleting $binfil"
		    flush $logid
		    exec rm $binfil
		}
		exec grppha $datfil\_$j.fak [format "%s_%s_b$grpmin.fak" \
			$datfil $j] comm = \
			"$bad & group min $grpmin & exit"
		data $i:$i [format "%s_%s_b$grpmin.fak" $datfil $j]
		if {$sis} {
		    puts $logid "notice $i: 1-512"
		    notice $i: 1-512
		} else {
		    puts $logid "notice $i: 1-1024"
		    notice $i: 1-1024
		}
		ig  bad
	    } else {
		data $i:$i $datfil\_$j.fak
	    }
	
	    if {$igflag} {
		puts $logid "ig $i: $igstr"
		flush $logid
		ig $i: $igstr
	    }
	    incr i
	}
    
	if {[llength $modnorms] > 0} {
	    set i 1
	    puts $logid "Reading simulated backround spectra"
	    foreach datafil $datfils {
		back $i:$i $datafil\_bgd_$j.fak
		incr i
	    }
	}
	if {$fitmod != ""} {
	    source $fitmodelfil
	} else {
	    source $simmodelfil
	}
	puts $logid [show notice]
	flush $logid
	if {$setupmod} {
	    setup_mod $model
	    puts $logid "setup_mod $model"
	}
	fit 200 0.0001
	set savefile [format "%s_%s.xcm" $logroot $j]
	if {[file exists $savefile]} {
	    exec rm $savefile
	}
	save all $savefile
	
	if {$geterrors} {
	    puts $logid "Determining errors"
	    flush $logid
	    smart_fit $model logroot=[format "%s_%d" $logroot $j]
	} else {
	    puts $logid [show param]
	}
	eval exec gzip -f [glob fak/*fak]
	# Move files over to log and xcm directories
	set flist [glob -nocomplain $logroot\_$j*log]
	foreach fil $flist {
	    exec gzip -f $fil
	}
	set flist [glob $logroot\_$j*xcm]
	foreach fil $flist {
	    exec gzip -f $fil
	}
	set flist [glob -nocomplain $logroot\_$j*log.gz]
	foreach fil $flist {
	    file copy $fil log
	    file delete $fil
	}
	set flist [glob -nocomplain $logroot\_$j*xcm.gz]
	foreach fil $flist {
	    file copy $fil xcm
	    file delete $fil
	}
	set flist [glob -nocomplain $logroot\_$j*qdp]
	foreach fil $flist {
	    file copy $fil qdp
	    file delete $fil
	}
    }
    close $logid
#    sys rm $fakeitfil
#    sys rm $simmodelfil
}
