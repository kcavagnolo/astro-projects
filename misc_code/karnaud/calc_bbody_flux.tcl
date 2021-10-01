#Date: Wed, 22 Aug 2001 14:06:07 -0400 (EDT)
#From: Andrew Ptak <ptak@pha.jhu.edu>
#
#Hey Ed,
#Let me know if this is what you had in mind and then I'll play around with
#it some more.  To use it, source it within xspec and then do something
#like
#XSPEC> calc_bb_flux 5. 1150 1700
#5. = temp. of BB in eV

# modified ejmc 8/22/01

proc ev2ang {ev} {
    return [expr 1.24e4/$ev]
}

proc ang2ev {ang} {
    return [expr 1.24e4/$ang]
}

proc calc_bbody_flux {kt lamb1 lamb2} {
    puts "Blackbody temperature = $kt eV"
    set e2 [ang2ev $lamb1]
    set e1 [ang2ev $lamb2]
    puts "Desired energy range: ($lamb1 - $lamb2 A) <=> ([format "%10.4e" $e1] - [format "%10.5e" $e2] eV)"
#
    set flow 1.0e5
    set fhigh 1.0e5
#
#   set e10 [expr $e1 / $flow]
    set e10 [expr $kt / $flow]
#   set e20 [expr $e2 * $fhigh]
    set e20 [expr $kt * $fhigh]
    set e10kev [expr $e10 / 1000.]
    set e20kev [expr $e20 / 1000.]
    set e10kevmod [format "%10.5e" $e10kev]
    set e20kevmod [format "%10.5e" $e20kev]
    puts "Creating dummy response for range ($e10kevmod - $e20kevmod) keV"
    puts "dummyrsp $e10kevmod $e20kevmod 10000 log"
    dummyrsp $e10kevmod $e20kevmod 10000 log
    model bbody & [expr $kt * 1e-3] & 1.0
#   cpd /gif
#   file delete pgplot.gif
#   setplot wave
#   plot model
#   cpd /null
#   file delete bbmodel.gif
#   file rename pgplot.gif_2 bbmodel.gif
    puts "Flux in complete range ($e10kevmod $e20kevmod keV):"
    flux $e10kevmod $e20kevmod
    tclout flux
    set ftot $xspec_tclout
#   puts "flux [expr $e1 / 1000.] [expr $e2 / 1000.]"
    puts "Flux in desired range ([format "%10.5e" [expr $e1 / 1000.]] - [format "%10.5e" [expr $e2 / 1000.]] keV) <=> ( $lamb1 - $lamb2 )"
    flux [expr $e1 / 1000.] [expr $e2 / 1000.]
    tclout flux
    set f $xspec_tclout
#    puts "Ratio of flux in ($e1 - $e2) / ($e10 - $e20) eV = [expr $f / $ftot]"
    puts "Ratio of fluxes = [format "%10.5e" [expr $f / $ftot]]"
}

