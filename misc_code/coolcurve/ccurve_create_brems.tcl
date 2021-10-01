proc ccurve_create_brems {outfile itvar bandlo bandhi logtmin dlogt ntemps} {

#  XSPEC 11 tcl script for calculating emissivity as a function of temperature.
#  Copyright (C) 2001-2006 David K. Strickland (dks@pha.jhu.edu)
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#--------------------------------------------------------------------------
#  Command line options:
#  outfile	(char string) The name of the output file to use. 
#               Will be overwritten if it already exists.
#  itvar	(integer) The model parameter number that corresponds to
#               gas temperature.
#  bandlo       (float) The lower energy bound in keV for the energy band.
#  bandhi       (float) The upper energy bound in keV for the energy band.
#  logtmin      (float) Minimum temperature to use, in units of log T (K).
#  dlogt        (float) Logarithmic temperatre step.
#  ntemps       (integer) The number of temperature bins.

#  How to use this function:
#  (see also ccurve_create.README)
#  1. You must supply the model, and the model must have been preloaded
#     before this script is run. You set the N_H, metal abundances etc.
#     You should set the normalization constant so that the flux multiplied
#     by 1.0e-14 gives the emissivity in units of erg cm**3 s**-1. For most
#     hot plasma models like raymond or mekal this implies the norm should
#     be exactly 1.0. See the XSPEC help for you particular model.
#     How this works: For most plasma models the
#     norm = model_specific_const * EI / (4pi D^2)
#     while the flux would be f = EI lambda / (4pi D^2)
#     so for unit norm then lambda = flux * model_specific_const
#
#  2. Make sure the model you use has the hard upper and lower temperature
#     limits set to include the temperatures you plan on using!

#  Local variables:
#  For brems model model_specific_const = 3.02e-15
  set p_flux2emis 3.02e-15
  set p_kev 1.16045e7

#  Echo the name of this function to the screen.
  puts stdout "ccurve_create: Create an emissivity curve using XSPEC."
  puts stdout " "

#  Delete existing output file if it exists, then create a new one.
  file delete $outfile
  set p_out [open $outfile w 0644]

#  Save original spectral model. Will restore exaclt once finished.
  file delete ccurve_tmpmod.xcm
  save model ccurve_tmpmod.xcm
  puts stdout "  Saving current spectral model. Will restore later..."

#  Plotting for debug purposes. Comment out all plot commands later please.
  cpd /xserve

#  Set up our required fake response. Use 1000 bin linearly spaced over
#  band by default.
  dummyrsp $bandlo $bandhi 2000 log

#  Loop through the temperatures. Calculate kT in keV. Reset model 
#  Calculate flux. Convert to emissivity. Print out log T and emissivity.
  for {set i 1} {$i <= $ntemps} {incr i} {
    puts stdout "------------------------------------------------------------"
    set p_logt [expr ($logtmin + ($i-1)*$dlogt)]
    set p_temp [expr pow(10.0,$p_logt)]
    set p_kt [expr ($p_temp / $p_kev)]
    puts stdout "  $i  log T= $p_logt  T= $p_temp  kT = $p_kt"

    newpar $itvar $p_kt
    plot model
    flux $bandlo $bandhi
    tclout flux
    set p_flux [lindex $xspec_tclout 0]
#    puts stdout " kT= $p_kt flux= $p_flux"
    set p_emis [expr ($p_flux * $p_flux2emis)]
    puts stdout [format "  Result: %9.3e %9.3e" $p_logt $p_emis]
    puts $p_out [format "%9.3e %9.3e" $p_logt $p_emis]
  }

#  Restore old spectral model.
  @ccurve_tmpmod.xcm
  puts stdout "  Restored old spectral model."
  file delete ccurve_tmpmod.xcm

#  Close the output file.
  close $p_out

#  Clean up and return.
  puts stdout "  Emissivity date written to $outfile"
  return
}
