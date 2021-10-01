proc setup_mod { modelname args} {
# by A. Ptak (ptak@neutron.gsfc.nasa.gov), Apr. 1997
# Revised for XSPEC 11, Dec. 2000

    global xs_return_result

    set old_xs_return_result $xs_return_result
    set xs_return_result 1

    set files [show files]
    set nfiles [llength $files]

    set Nh 0.1
    if {[llength $args] > 0} {
	set Nh [lindex $args 0]
    }

    switch $modelname {
	pl {
	    set Gamma 1.8
	    # get optional params.
	    if {[llength $args] > 1} {
		set Gamma [lindex $args 1]
	    }
	    #set fil [open setup_mod_[pid].xcm "w"]
	    set modstr "constant * wa(powerlaw) &"
	    set modstr "$modstr 1 -1 &"
	    set modstr "$modstr $Nh &"
	    set modstr "$modstr $Gamma &"
	    set modstr "$modstr 1e-4 &"
	    for {set i 1} {$i<$nfiles} {incr i} {
		set modstr "$modstr 1 0.01 &"
		set modstr "$modstr = 2 &"
		set modstr "$modstr = 3 &"
		set modstr "$modstr = 4 &"
	    }
	    set modstr "$modstr /*"
	    puts $modstr
	    model $modstr
	    #close $fil
	    #@setup_mod_[pid].xcm
	    #file delete setup_mod_[pid].xcm
	}
	rs {
	    set kT 5.0
	    set A 0.1
	    set z 0
	    # get optional params.
	    if {[llength $args] > 1} {
		set kT [lindex $args 1]
	    }
	    if {[llength $args] > 2} {
		set A [lindex $args 2]
	    }
	    if {[llength $args] > 3} {
		set z [lindex $args 3]
	    }
	    set modstr "constant * wabs (raymond) & "
	    set modstr "$modstr 1 -1 &"
	    set modstr "$modstr $Nh &"
	    set modstr "$modstr $kT 0.01 0.1 0.1 &"
	    set modstr "$modstr $A 0.01 &"
	    set modstr "$modstr $z &"
	    set modstr "$modstr 1e-4 &"
	    for {set i 1} {$i<$nfiles} {incr i} {
		set modstr "$modstr 1 0.01 &"
		set modstr "$modstr = 2 &"
		set modstr "$modstr = 3 &"
		set modstr "$modstr = 4 &"
		set modstr "$modstr = 5 &"
		set modstr "$modstr = 6 &"
	    }
	    set modstr "$modstr /*"
	    puts $modstr
	    model $modstr
	}
    
	 plga {
	     set Gamma 1.8
	     # get optional params.
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     
	     set modstr "constant * wabs (powerlaw gaussian) &"
	     set modstr "$modstr 1 -1 &" 
	     set modstr "$modstr $Nh &"
	     set modstr "$modstr $Gamma &"
	     set modstr "$modstr 1e-4 &"
	     set modstr "$modstr 6.4 0.01 &"
	     set modstr "$modstr 0.01 0.01 0.0 0.0 0.5 1.0 &"
	     set modstr "$modstr 1e-6 1e-6 &"
	     for {set i 1} {$i<$nfiles} {incr i} {
		 set modstr "$modstr 1 0.01 &"
		 set modstr "$modstr = 2 &"
		 set modstr "$modstr = 3 &"
		 set modstr "$modstr = 4 &"
		 set modstr "$modstr = 5 &"
		 set modstr "$modstr = 6 &"
		 set modstr "$modstr = 7 &"
	     }
	     set modstr "$modstr /*"
	     puts $modstr
	     model $modstr
	 }
	 plzga {
	     set Gamma 1.8
	     set z 0.
	     # get optional params.	
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     if {[llength $args] > 2} {
		 set z [lindex $args 2]
	     }	    
     
	     set modstr "constant * wabs (powerlaw zgauss) &"
	     set modstr "$modstr 1 -1 &" 
	     set modstr "$modstr $Nh &"
	     set modstr "$modstr $Gamma &"
	     set modstr "$modstr 1e-4 &"
	     set modstr "$modstr 6.4 0.01 &"
	     set modstr "$modstr 0.01 0.01 0.0 0.0 0.5 1.0 &"
	     set modstr "$modstr $z &"
	     set modstr "$modstr 1e-6 1e-6 &"
	     for {set i 1} {$i<$nfiles} {incr i} {
		 set modstr "$modstr 1 0.01 &"
		 set modstr "$modstr = 2 &"
		 set modstr "$modstr = 3 &"
		 set modstr "$modstr = 4 &"
		 set modstr "$modstr = 5 &"
		 set modstr "$modstr = 6 &"
		 set modstr "$modstr = 7 &"
	     }
	     set modstr "$modstr /*"
	     puts $modstr
	     model $modstr
	 }
	 plga_sfrozen {
	     set Gamma 1.8
	     # get optional params.
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     
	     set modstr "constant * wabs (powerlaw gaussian) &"
	     set modstr "$modstr 1 -1 &" 
	     set modstr "$modstr $Nh &"
	     set modstr "$modstr $Gamma &"
	     set modstr "$modstr 1e-4 &"
	     set modstr "$modstr 6.4 0.01 &"
	     set modstr "$modstr 0.01 -0.01 0.0 0.0 0.5 1.0 &"
	     set modstr "$modstr 1e-6 1e-6 &"
	     for {set i 1} {$i<$nfiles} {incr i} {
		 set modstr "$modstr 1 0.01 &"
		 set modstr "$modstr = 2 &"
		 set modstr "$modstr = 3 &"
		 set modstr "$modstr = 4 &"
		 set modstr "$modstr = 5 &"
		 set modstr "$modstr = 6 &"
		 set modstr "$modstr = 7 &"
	     }
	     set modstr "$modstr /*"
	     puts $modstr
	     model $modstr
	 }

	 plgapexrav_inc30 {
	     set Gamma 1.8
	     # get optional params.
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     set modstr "wabs*(pexrav + ga)"
	     set modstr "$modstr & $Nh 0.1"
	     set modstr "$modstr & $Gamma"
	     set modstr "$modstr & 100 -1"
	     set modstr "$modstr & 1 0.1"
	     set modstr "$modstr & 0 -1"
	     set modstr "$modstr & 1 -1"
	     set modstr "$modstr & 1 -1"
	     set modstr "$modstr & 0.866 -0.01"
	     set modstr "$modstr & 1e-2"
	     set modstr "$modstr & 6.4 0.01 4.5 5.0 6.7 7.1"
	     set modstr "$modstr & 0.01 0.01 0.0 0.0 0.5 1.0"
	     set modstr "$modstr & 1e-4 1e-5"
	     set modstr "$modstr /*"
	     puts $modstr
	     model $modstr
	 }
   
	 plgapexrav_inc30_sfrozen {
	     set Gamma 1.8
	     # get optional params.
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     set modstr "wabs*(pexrav + ga)"
	     set modstr "$modstr & $Nh 0.1"
	     set modstr "$modstr & $Gamma"
	     set modstr "$modstr & 100 -1"
	     set modstr "$modstr & 1 0.1"
	     set modstr "$modstr & 0 -1"
	     set modstr "$modstr & 1 -1"
	     set modstr "$modstr & 1 -1"
	     set modstr "$modstr & 0.866 -0.01"
	     set modstr "$modstr & 1e-2"
	     set modstr "$modstr & 6.4 0.01 4.5 5.0 6.7 7.1"
	     set modstr "$modstr & 0.01 -0.01 0.0 0.0 0.5 1.0"
	     set modstr "$modstr & 1e-4 1e-5"
	     set modstr "$modstr /*"
	     puts $modstr
	     model $modstr
	 }
   
	 plgapexrav_inc60 {
	     set Gamma 1.8
	     # get optional params.
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     set modstr "wabs*(pexrav + ga)"
	     set modstr "$modstr & $Nh 0.1"
	     set modstr "$modstr & $Gamma"
	     set modstr "$modstr & 100 -1"
	     set modstr "$modstr & 1 0.1"
	     set modstr "$modstr & 0 -1"
	     set modstr "$modstr & 1 -1"
	     set modstr "$modstr & 1 -1"
	     set modstr "$modstr & 0.5 -0.01"
	     set modstr "$modstr & 1e-2"
	     set modstr "$modstr & 6.4 0.01 4.5 5.0 6.7 7.1"
	     set modstr "$modstr & 0.01 0.01 0.0 0.0 0.5 1.0"
	     set modstr "$modstr & 1e-4 1e-5"
	     set modstr "$modstr /*"
	     puts $modstr
	     model $modstr
	 }
     
	 edplga {
	     puts "Setting up edplga"
	     set Gamma 1.8
	     # get optional params.
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     set modstr "edge*wabs*( powerlaw gaussian )"
	     set modstr "$modstr & 7.1 0.01 6.7 7.0 8.0 8.5"
	     set modstr "$modstr & 0.0 0.01"
	     set modstr "$modstr & $Nh 0.1"
	     set modstr "$modstr & $Gamma"
	     set modstr "$modstr & 1e-4"
	     set modstr "$modstr & 6.4 0.01 4.5 5.0 6.7 7.1"
	     set modstr "$modstr & 0.01 0.01 0.0 0.0 0.5 1.0"
	     set modstr "$modstr & 1e-4 1e-5"
	     puts $modstr
	     model $modstr
	 }

	 edplga_sfrozen {
	     puts "Setting up edplga"
	     set Gamma 1.8
	     # get optional params.
	     if {[llength $args] > 1} {
		 set Gamma [lindex $args 1]
	     }	    
	     set modstr "edge*wabs*( powerlaw gaussian )"
	     set modstr "$modstr & 7.1 0.01 6.7 7.0 8.0 8.5"
	     set modstr "$modstr & 0.0 0.01"
	     set modstr "$modstr & $Nh 0.1"
	     set modstr "$modstr & $Gamma"
	     set modstr "$modstr & 1e-4"
	     set modstr "$modstr & 6.4 0.01 4.5 5.0 6.7 7.1"
	     set modstr "$modstr & 0.01 -0.01 0.0 0.0 0.5 1.0"
	     set modstr "$modstr & 1e-4 1e-5"
	     puts $modstr
	     model $modstr
	 }


     	 default {
	    puts [format "Sorry, don't know about %s" $modelname]
	 }
    }
    renorm
    set xs_return_result $old_xs_return_result
}
