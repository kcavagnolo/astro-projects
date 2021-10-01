proc smart_fit {model args} {
#
# A. Ptak, May 1997
# Modified Dec 1998 to skip param error search when qdp files exists.
# Modified 1/21/99 to put param. log and qdp files in the dirs. log/ and qdp/
# Modified 8/8/01  ejmc to allow different fitdelta and multiple fits at 
#   beginning and upon finding new minimum, added init_step parameter so can 
#   pass this to smart_error
# Modified 10/9/01 ejmc from xspec10ejmc file 
# Note that a problem with dummy_flux with multiple data sets was fixed 8/21/02
# Modified 10/16/02 to allow for updating delta parameters in fits to speed up
# subsequent fitting

    global env
    set home [get_xspec_home]
    set config "$home/smart_fit_config.dat"
    
#    set config "/home/ptak/tcl/xspec/smart_fit_config.dat"
    set logroot ""

# mod for fitdelta ejmc 8/8/01
    set fitdelta 0.001
    set nfits 5

    set init_step 1

    if {$model == "-h" || $model == "?"} {
	puts "Usage: smart_fit model (logroot=logroot config=config dchi=dchi skip=skip init_step=init_step csvfile=csvfile flux_elow=flux_elow flux_ehi=flux_ehi update_delta=1)"
	puts "Computes errors for each param. in a model"
	puts "Give \"none\" for model to get errors on all parameters"
	puts "In the process, log and qdp files are created for each parameter"
	puts "\"smart_fit list\" gives list of known model setups"
	puts "Default logroot = smart_fit_modelname_date"
	puts "If a new min. is found, new filename = logroot+\"_new.xcm\""
	puts "Default config file = $config"
	puts "If dchi is given, overide default delta chisq"
	puts "   = 90\% conf. assuming all param. are \"interesting\""
	puts "If skip is given, skip error search for parameter skip"
        puts "Default init_step = $init_step, passed to smart_error"
	puts "If csvfile isn't given, logroot.csv is used instead"
	puts "If flux_elow, flux_ehi, flux is computed by xspec in that energy range"
	puts "N.B., there is no check that the energy bounds of the detector overlap "
	puts "flux_elow - flux_ehi."
	puts "Set update_delta to modify the \"delta\" parameters (second param. in xspec"
	puts "models to cache error stepping sizes."
	return
    }

    autosave off

    set returnflag 1

    set dchi_vals {2.706 4.605 6.251 7.779 9.236 10.645 12.017 13.362 14.684 \
	    15.987 17.275 18.549 19.812 21.064 22.307 23.542 24.769 25.989 \
	    27.204 28.412}
    
# Get keywords
    set logroot [format "smart_fit_%s_%s" $model [exec date +%d%h%y]]
    set dchiflag 0
    set skip ""
    set norms ""
    set csvfile ""
    set flux_elow 0.
    set flux_ehi 0.
    set update_delta 1
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
		    logroot {
			puts "log keyword is set"
			set logroot $keyw_val
		    }
		    config {
			set config $keyw_val
		    }
		    dchi {
			set dchiflag 1
			set dchi $keyw_val
		    }
		    skip {
			set skip $keyw_val
		    }
		    norms {
			set norms $keyw_val
                    }
                    init_step {
                        set init_step $keyw_val
                    }
		    csvfile {
			set csvfile $keywval
		    }
		    flux_elow {
			set flux_elow $keyw_val
		    }
		    flux_ehi {
			set flux_ehi $keyw_val
		    }
		    update_delta {
			set update_delta $keyw_val
		    }
	       }
	    }
	}
    }

    if {$csvfile == ""} {
	set csvfile [format "%s.csv" $logroot]
    }
    set logfil [format "%s.log" $logroot]
    set logid [open $logfil "a+"]

    set skip [split $skip @]
    puts $logid "skip = $skip"
    set logflag 1
    set host "localdomain"
    if {[array names env HOST] == "HOST"} {
	set host $env(HOST)
    } elseif {[array names env HOSTNAME] == "HOSTNAME"} {
	set host $env(HOSTNAME)
    }
    puts $logid "machinename $host: smart_fit $model $args"


# Read in model configurations
    if {![file readable $config]} {
	puts "Error... cannot read $config"
	if {$logflag} {
	    puts $logid "Error... cannot read $config"
	    close $logid
	}
	autosave 1
	return 0
    }

    set fileid [open $config "r"]
    set modlist ""
    puts "Reading $config..."
    while {[gets $fileid data] >= 0} {
	puts $data
	if {$data != ""} {
	    lappend modlist $data
	    set cur_mod $data
	    set models($cur_mod) ""
	    while {[lindex $data 0] != -1} {
		if {[gets $fileid data] == 0} {
		    puts "Error in $config file format"
		    if {$logflag} {
			puts $logid "Error in $config file format"
		    }
		    if {$logflag} {
			close $logid
		    }
		    autosave 1
		    return 0
		}
		if {[lindex $data 0] != -1} {
		    lappend models($cur_mod) $data
		}
	    }
	}
    }
    close $fileid

# Only need to display list of defined models?
    if {$model == "list"} {
	set searchid [array startsearch models]
	while {[array anymore models $searchid]} {
	    set element [array nextelement models $searchid]
	    set interesting ""
	    set parlist $models($element)
	    foreach par $parlist {
		lappend interesting [lindex $par 1]
	    }
	    puts "Model $element, [llength \
		    $interesting] \"interesting\" params: $interesting"
#	    if {$logflag} {
#		puts $logid \
#			"Model $element, [llength \
#			$interesting] \"interesting\" params: $interesting"
#	    }
	}
	if {$logflag} {
	    close $logid
	}
	autosave 1
	return 0
    }    


# Check to see if requested model is configured
    if {$model == "none"} {
	puts "Will get errors on all parameters"
    } else {
	if {[llength [array names models $model]] == 0} {
	    puts "Sorry, $model is not defined in $config"
	    if {$logflag} {
		puts $logid "Sorry, $model is not defined in $config"
		close $logid
	    }
	    autosave 1
	    return 0
	}
    }

    global xs_return_result
    set old_xs_return_result $xs_return_result
    set xs_return_result 1

# Get delta chisq, if nec.
    if {$model != "none"} {
	set mod_params $models($model)
    } else {
	# Build up mod_params
	set mod_params ""
	tclout modpar
	set nparams $xspec_tclout
	for {set i 1} {$i <= $nparams} {incr i} {
	    # find out if param is tied
	    tclout plink $i
	    if {$xspec_tclout != "F"} {
		continue
	    }
	    # find out if param is frozen
	    tclout param $i
	    if {[lindex $xspec_tclout 1] < 0.} {
		continue
	    }
	    tclout pinfo $i
	    set name [lindex $xspec_tclout 0]$i
	    set units ""
	    if {[llength $xspec_tclout] > 1} {
		set units "([lindex $xspec_tclout 1])"
	    }
	    set modparam "$i $name {$name $units}"
	    lappend mod_params $modparam
	}
    }
    foreach param $mod_params {
	puts $param
    }
    set nparams [llength $mod_params]
    puts "$nparams parameters in $model"
    if {$logflag} {
	puts $logid "$nparams parameters in $model"
    }

    if {!$dchiflag} {
	set dchi [lindex $dchi_vals [expr $nparams - 1]]
    }
    puts "dchi = $dchi"
    if {$logflag} {
        puts $logid "dchi = $dchi"
    }

# Create temporary and status files
    set tempfil "smart_fit_[pid].xcm"
    set statfil [format "%s_stat.log" $logroot]
#    set stattcl "smart_fit_[pid].tcl"

# Create param_log and param_qdp dirs if nec.
    set qdpdir param_qdp
    set logdir param_log
  
    if {![file exists $logdir]} {
	file mkdir $logdir
    }
    if {![file exists $qdpdir]} {
	file mkdir $qdpdir
    }

# Dump initial params and rates in case smart_fit crashes
    
    set f_gt_2kev [dummy_flux 2.0 10.0]
    set fstr "F(2.0-10.0 keV) ="
    # dummy_flux only returns fluxes, not photon fluxes
    #for {set i 1} {$i < [llength $f_gt_2kev]} {incr i 2} {
    #   	append fstr [format " %.4g" [lindex $f_gt_2kev $i]]
    #}
    for {set i 0} {$i < [llength $f_gt_2kev]} {incr i 1} {
	# flux may have more than one number
	set flst [lindex $f_gt_2kev $i]
	if {[llength $flst] > 1} {
	    set flst [lindex $flst 1]
	}
       	append fstr [format " %.4g" $flst]
    }
    puts $fstr
    if {$logflag} {
	puts $logid $fstr
    }
    
    set rates "Count rates: "
    # set cnts [show rates]
    tclout datagrp
    set ngrps $xspec_tclout
    #set cnts [show rates]
    #for {set i 0} {$i < [llength $cnts]} {incr i} {
    #   	append rates [format " %.4g" [lindex $cnts $i]]
    #}
    for {set i 1} {$i <= $ngrps} {incr i} {
	tclout rates $i
	append rates [format " %.4g" [lindex $xspec_tclout 0]]
    }
    puts $rates
    if {$logflag} {
	puts $logid $rates
	flush $logid
    }
    flush stdout

    puts $logid "Entry point parameter values:"
    foreach param $mod_params {
	set index [lindex $param 0]
	set name [lindex $param 1]
	tclout param $index
	set param_vals $xspec_tclout
	set param_val [lindex $param_vals 0]
	puts [format "%s = %.3g ()" $name $param_val]
	if {$logflag} {
	    puts $logid [format "%s = %.3g ()" $name $param_val]
	}
    }

# Main loop

    query y

# decided to skip this intial fit loop 10/9/01 ejmc
#   ASSUME model is best fit upon entry, otherwise let smartfit save
#   it as _new.xcm model.  Doing it other way likely will result in
#   fit that has no *.xcm file.......
#
##   fit 1000 0.0001
## replace above line and do this nfits times to make sure got good fit
#    for {set i 0} {$i <= $nfits} {incr i} {
#      fit 1000 $fitdelta
#    }

    set done 0

    if {[dof] < 1} {
	puts "dof = [dof], not enough data points to properly fit data, exiting"
	if {$logflag} {
	    puts $logid "dof = [dof], not enough data points to properly fit data, exiting"
	}

	set done 1
    } else {
	puts "Starting error search..."
	if {$logflag} {
	    puts $logid "Starting error search..."
	    flush $logid
	}
    }
    while {!$done} {
	set done 1
	foreach param $mod_params {
	    set index [lindex $param 0]
	    set name [lindex $param 1]
	    set eqw 0
	    if {[llength $param] > 3} {
		set eqw [lindex $param 3]
	    }
	    set skipflag 0
	    foreach skipval $skip {
		if {$skipval == $name} {
		    set skipflag 1
		}
	    }
	    # If param qdp file exists, skip error search since its
	    # already done
	    if {[file exists $qdpdir/$logroot\_$name.qdp]} {
		puts "$qdpdir/$logroot\_$name.qdp exists"
		puts $logid "$qdpdir/$logroot\_$name.qdp exists"
		set skipflag 1
	    }
	    if {$skipflag} {
	        puts -nonewline "Skipping $name, "
		if {$logflag} {
		    puts -nonewline $logid "Skipping $name, "
		}
		#set val [lindex [show param $index] 0]
		tclout param $index
		set val [lindex $xspec_tclout 0]
		puts "last fit value = $val"
		if {$logflag} {
		    puts $logid "last value = $val"
		}
		continue;
	    }
	    set xtitle [lindex $param 2]
	    puts "Getting error on parameter \#$index = $name"
	    if {$logflag} {
		puts $logid "Getting error on parameter \#$index = $name"
		flush $logid
	    }
	    set log "$logroot\_$name.log"
	    set result [smart_error $index dchi=$dchi log=$logdir/$log \
		    param_name=$name xtitle=$xtitle qdp=$qdpdir/$logroot \
                      init_step=$init_step]
	    autosave off
	    set resultflag [lindex $result 0]
#	    puts "resultflag = $resultflag"
	    if {$resultflag} {
		puts "smart_error exited normally"
		if {$logflag} {
		    puts $logid "smart_error exited normally"
		}
		if {$update_delta} {
		    set delta [expr ([lindex $result 2] - [lindex $result 1])/2.]
		    puts "Setting delta for parameter $index to $delta"
		    if {$logflag} {
			puts "Setting delta for parameter $index to $delta"
		    }
		    tclout param $index
		    set val [lindex $xspec_tclout 0]
		    newpar $index $val $delta
		}
	    }
	    if {$resultflag == 0} {
		#set val [lindex [show param $index] 0]
		tclout param $index
		set val [lindex $xspec_tclout 0]
		puts "An error occurred during the error search, last fit value = $val"
		if {$logflag} {
		    puts $logid "An error occurred during the error search, last value = $val"
		}
		set errors([lindex $param 0]) "$val"
		continue
	    }
	    set msg ""
	    if {$resultflag & 4} {
		lappend msg \
	          "Hard limit reached before req. deltachisq for lower bound"
	    }
	    if {$resultflag & 8} {
		lappend msg \
		  "Hard limit reached before req. deltachisq for upper bound"
	    }
	    if {$resultflag & 16} {
		lappend msg \
		  "Lower steppar contains at least one rel. min."
	    }
	    if {$resultflag & 32} {
		lappend msg \
		  "Upper steppar contains at least one rel. min."
	    }
	    foreach line $msg {
		puts $line
		if {$logflag} {
		    puts $logid $line
		}
	    }
	    if {$resultflag & 2} {
		if {[file readable "$logroot\_new.xcm"]} {
		    exec rm "$logroot\_new.xcm"
		}
		save all "$logroot\_new.xcm"
		puts "A new minimum was found, saved all as $logroot\_new.xcm,"
		puts "restarting error search"
		set skip ''
		puts "Set skip to null"
		if {$logflag} {
		    puts $logid "A new minimum was found, saved all as $logroot\_new.xcm,"
		    puts $logid "restarting error search"
		    puts $logid "Set skip to null"
		    flush $logid
		}
		set done 0
		# Remove qdp files
		set flist [glob -nocomplain $qdpdir/$logroot*qdp]
		foreach fil $flist {
		    file delete $fil
		    puts "rm $fil"
		    puts $logid "rm $fil"
		}
		tclout param $index
		set val [lindex $xspec_tclout 0]
		set errors([lindex $param 0]) "$val [lindex $result 1] [lindex $result 2]"
		break
	    } else {
		# set val [lindex [show param $index] 0]
		tclout param $index
		set val [lindex $xspec_tclout 0]
		set errors([lindex $param 0]) "$val [lindex $result 1] [lindex $result 2]"
		puts [format "%s = %.3g (%.3g - %.3g)"  $name $val \
			[lindex $result 1] [lindex $result 2]]
		if {$logflag} {
		    puts $logid [format "%s = %.3g (%.3g - %.3g)"  $name $val \
			    [lindex $result 1] [lindex $result 2]]
		    flush $logid
		}
		# Get EW range if nec.
		if {$eqw > 0} {
		    set ew [mean [eqwidth $eqw]]
		    if {[file readable $tempfil]} {
			exec rm $tempfil
		    }
		    save mo $tempfil
		    puts $logid "Stepping LineNorm down to lower error range"
		    set lowrange [expr $val - [lindex $result 1]]
		    for {set i 0} {$i < 11} {incr i} {
			set stepval [expr $val - ($i*$lowrange/10)]
			newpar $index $stepval
			puts $logid $stepval
			flush $logid
			freeze $index
			fit
		    }
		    set ew_low [mean [eqwidth $eqw]]
		    @$tempfil
		    set hirange [expr [lindex $result 2] - $val]
		    puts $logid "Stepping LineNorm up to upper error range"
		    for {set i 0} {$i < 11} {incr i} {
			set stepval [expr $val + ($i*$hirange/10)]
			newpar $index $stepval
			puts $logid $stepval
			freeze $index
			fit
		    }
		    set ew_hi [mean [eqwidth $eqw]]
		    set errors(ew$param) "$ew $ew_low $ew_hi"
		    puts [format "%s = %.3g (%.3g - %.3g)"  "EW" $ew \
			    $ew_low $ew_hi]
		    if {$logflag} {
			puts $logid [format "%s = %.3g (%.3g - %.3g)"  "EW" \
				$ew $ew_low $ew_hi]
			flush $logid
		    }  
		    @$tempfil
		    exec rm $tempfil
		}
	    }
	}
    
    }

# Output norms if given
    set norms [split $norms _]
#    puts $logid "norms = $norms"
    foreach norm $norms {
	# set val [lindex [show param $norm] 0]
	tclout param $index
	set val [lindex $xspec_tclout 0]
	puts "Norm $norm = $val"
	if {$logflag} {
	    puts $logid "Norm $norm = $val"
	}
    }
	
# Output final chisq, fluxes, count rates
    # set chisq [show param]
    set chisq [get_stat]
    puts [format "Final chisq(dof) = %.3f(%d)" $chisq [dof]]
    if {$logflag} {
	puts $logid [format "Final chisq(dof) = %.3f(%d)" $chisq [dof]]
    }

    set f_lt_2kev [dummy_flux 0.5 2.0]
    set fstr "F(0.5-2.0 keV) ="
    #for {set i 1} {$i < [llength $f_lt_2kev]} {incr i 2} {
    #   	append fstr [format " %.4g" [lindex $f_lt_2kev $i]]
    #}
    for {set i 0} {$i < [llength $f_lt_2kev]} {incr i} {
	set flst [lindex $f_lt_2kev $i]
	if {[llength $flst] > 1} {
	    set flst [lindex $flst 1]
	}
       	append fstr [format " %.4g" $flst]
    }
    puts $fstr
    if {$logflag} {
	puts $logid $fstr
    }

    set f_gt_2kev [dummy_flux 2.0 10.0]
    set fstr "F(2.0-10.0 keV) ="
    #for {set i 1} {$i < [llength $f_gt_2kev]} {incr i 2} {
    #   	append fstr [format " %.4g" [lindex $f_gt_2kev $i]]
    #}
    for {set i 0} {$i < [llength $f_gt_2kev]} {incr i} {
	# flux may have more than one number
	set flst [lindex $f_gt_2kev $i]
	if {[llength $flst] > 1} {
	    set flst [lindex $flst 1]
	}
       	append fstr [format " %.4g" $flst]
    }
    puts $fstr
    if {$logflag} {
	puts $logid $fstr
    }

    set f_user ""
    if {$flux_elow > 0.} { 
	set f_user [dummy_flux $flux_elow $flux_ehi]
	set f_user_str [format "F(%.1f-%.1f)" $flux_elow $flux_ehi]
	set fstr ""
	#for {set i 1} {$i < [llength $f_user]} {incr i 2} {
	#    append fstr [format " %.4g" [lindex $f_user $i]]
	#}
	for {set i 0} {$i < [llength $f_user]} {incr i} {
	    set flst [lindex $f_user $i]
	    if {[llength $flst] > 1} {
		set flst [lindex $flst 1]
	    }
	    append fstr [format " %.4g" $flst]
	}
	puts $logid [format "$f_user_str = %s" $fstr]
    }

    set rates "Count rates: "
    # set cnts [show rates]
    tclout datagrp
    set ngrps $xspec_tclout
    #set cnts [show rates]
    #for {set i 0} {$i < [llength $cnts]} {incr i} {
    #   	append rates [format " %.4g" [lindex $cnts $i]]
    #}
    for {set i 1} {$i <= $ngrps} {incr i} {
	tclout rates $i
	append rates [format " %.4g" [lindex $xspec_tclout 0]]
    }
    puts $rates
    if {$logflag} {
	puts $logid $rates
    }

    # Dump results to csv file

    # Get mapping of param number to model num
    tclout modcomp
    set nmods $xspec_tclout
    tclout modpar
    set npars $xspec_tclout
    set modnums ""

    for {set modnum 1} {$modnum <= $nmods} {incr modnum} {
	tclout compinfo $modnum
	set vals $xspec_tclout
	for {set comppar 0} {$comppar <= [lindex $vals 2]} {incr comppar} {
	    lappend modnums $modnum
	}
    }

    puts "Saving $csvfile"
    puts "number of params = $npars"
    set csvid [open $csvfile "w"]
    puts $csvid "model number,parameter number,parameter name,parameter value,parameter delta,lower error,upper error,delta statistic,min,max,tied"
    puts $csvid ",,data_file"
    puts $csvid ",,fit stat,$chisq"
    puts $csvid ",,fit stat name,chisq"
    puts $csvid ",,nmodels,$nmods"
    puts $csvid ",,nparams,$npars"
    tclout varpar
    puts $csvid ",,nfreeparams,$xspec_tclout"
    puts $csvid ",,dof,[dof]"
    #puts "errors names = [array names errors]"
    for {set parnum 1} {$parnum <= $npars} {incr parnum} {
	set parnum0 [expr $parnum - 1]
	tclout pinfo $parnum
	set parname [lindex $xspec_tclout 0]
	tclout param $parnum
	set parvals $xspec_tclout
	set val [lindex $parvals 0]
	set delta [lindex $parvals 1]
	set min [lindex $parvals 2]
	set max [lindex $parvals 5]
	tclout plink $parnum
	set tied -1
	if {[lindex $xspec_tclout 0] == "T"} {
	    set tied [lindex $xspec_tclout 1]
	    incr tied -1
	}
	# Have errors been computed?
	
	if {[lsearch [array names errors] $parnum] >= 0} {
	    set errs $errors($parnum)
	    puts "Found errors $errs for $parnum"
	    set errlow [lindex $errs 1]
	    set errhigh [lindex $errs 2]
	} else {
	    set errlow ""
	    set errhigh ""
	}
	puts "[lindex $modnums $parnum0],$parnum,$parname,$val,$delta,$errlow,$errhigh,$dchi,$min,$max,$tied"
	puts $csvid "[lindex $modnums $parnum0],$parnum,$parname,$val,$delta,$errlow,$errhigh,$dchi,$min,$max,$tied"
    }

#   tclout rates
# this doesn't work so get it for the first detector and report that
    tclout rates 1
    set modrate [lindex $xspec_tclout end]
    puts $csvid ",,model total rate,,$modrate"
    puts $csvid ",,F(0.5-2.0),[lindex $f_lt_2kev 0]"
    puts $csvid ",,F(2.0-10.0),[lindex $f_gt_2kev 0]"
    if {$flux_elow > 0.} {
	puts $csvid ",,$f_user_str,[lindex $f_user 0]"
    }
    set ewerrors [array get errors ew*]
    if {[llength $ewerrors] == 2} {
	set parname [lindex [lindex $ewerrors 0] 0]
	set vals [lindex $ewerrors 1]
	set val [lindex $vals 0]
	set errlo [lindex $vals 1]
	set errhi [lindex $vals 2]
	puts $csvid ",,EW,$ew,1,$errlo,$errhi,$dchi,0,10000,-1"
    }
    close $csvid
    
# Added 12/30/02
    if {$update_delta} {
	if {[file readable "$logroot\_new.xcm"]} {
	    exec rm "$logroot\_new.xcm"
	}
	save all "$logroot\_new.xcm"
	puts "Saving fit as $logroot\_new.xcm in order to save the revised \"delta\" values"
	puts "(to improve future error search efficiency)"
	if {$logflag} {
	    puts $logid "Saving fit as $logroot\_new.xcm in order to save the revised \"delta\" values (to improve future error search efficiency)"
	}
    }

# Exit cleanly
    if {$logflag} {
	close $logid
    }

    set xs_return_result $old_xs_return_result
    autosave 1
    return $returnflag
}




