###############################################################################

# import Python modules
from sys import *
from os import *
from datetime import *
from math import *
import pdb

# import 3rd party modules
from numpy import *
from pylab import *
from scipy import *
import scipy.interpolate as interpolate

# import user modules
from error import *
from files import *
from aips import *
from acalc import *
from sphere import *
from parameter import *
from mpfit import *
from unwrap import *

###############################################################################

def read_solution_table( uvim, in_version = 0, full_polarization = True,
    allow_zero_reference = False ):
  
  # get / check some data
  if ( not table_exists( uvim, 'SN', in_version ) ):
    raise error( 'SN table not found' )
  wiz_uvim = wizardry( uvim )
  sn_table = wiz_uvim.table( 'SN', in_version )
  no_if = sn_table.keywords[ 'NO_IF' ]
  if ( no_if > 1 ):
    raise error( 'multiple IFs not supported' )
  no_pol = sn_table.keywords[ 'NO_POL' ]
  if ( not full_polarization ):
    no_pol = 1
  if ( no_pol > 2 ):
    raise error( 'more than 2 polarizations currently not supported' )
  antenna_count = sn_table.keywords[ 'NO_ANT' ]
  mean_gain = float32( sn_table.keywords[ 'MGMOD' ] )
  
  # copy SN table contents to solution table
  time = -1000000.
  solution_table = []
  solution_row = None
#  skip_times = [ float32( dhms_to_time( [ 0., 0., 0., 0. ] ) ),
#      float32( dhms_to_time( [ 699., 0., 0., 0. ] ) ) ]
  for row in sn_table:
    new_time = float32( row.time )
#    if ( new_time in skip_times ):
#      continue
    if ( new_time != time ):
      time = new_time
      reference_antenna = row.refant_1
      if ( solution_row != None ):
        if ( allow_zero_reference or ( solution_row[ 0 ][ 1 ] > 0 ) ):
          solution_table.append( solution_row )
      solution = [ 0. for x in range( 4 * no_pol ) ]
      solution_row = [ [ s for s in solution ] for i in range( antenna_count + 1 ) ]
      solution_row[ 0 ][ 0 ] = time
      solution_row[ 0 ][ 1 ] = reference_antenna
    else:
      if ( row.refant_1 != reference_antenna ):
        raise error( 'different reference antennas within single calibration time not supported' )
      if ( no_pol > 2 ):
        if ( row.refant_2 != reference_antenna ):
          raise error( 'different reference antennas within single calibration time not supported' )
    antenna_number = int( row.antenna_no )
    solution_row[ antenna_number ][ 0 ] = float32( row.real1 ) / mean_gain
    solution_row[ antenna_number ][ 1 ] = float32( row.imag1 ) / mean_gain
    solution_row[ antenna_number ][ 2 ] = float32( row.delay_1 )
    solution_row[ antenna_number ][ 3 ] = float32( row.weight_1 )
    if ( ( float32( row.real1 ) == get_aips_magic_value() ) or
         ( float32( row.imag1 ) == get_aips_magic_value() ) or
         ( float32( row.delay_1 ) == get_aips_magic_value() ) or
         ( float32( row.weight_1 ) <= 0. ) ):
      solution_row[ antenna_number ][ 3 ] = 0.
    if ( no_pol > 1 ):
      solution_row[ antenna_number ][ 4 ] = float32( row.real2 ) / mean_gain
      solution_row[ antenna_number ][ 5 ] = float32( row.imag2 ) / mean_gain
      solution_row[ antenna_number ][ 6 ] = float32( row.delay_2 )
      solution_row[ antenna_number ][ 7 ] = float32( row.weight_2 )
      if ( ( float32( row.real2 ) == get_aips_magic_value() ) or
           ( float32( row.imag2 ) == get_aips_magic_value() ) or
           ( float32( row.delay_2 ) == get_aips_magic_value() ) or
           ( float32( row.weight_2 ) <= 0. ) ):
        solution_row[ antenna_number ][ 7 ] = 0.
  if ( solution_row != None ):
    if ( allow_zero_reference or ( solution_row[ 0 ][ 1 ] > 0 ) ):
      solution_table.append( solution_row )
  
  return solution_table

###############################################################################

def write_solution_table( uvim, solution_table, out_version = 0,
    full_polarization = True ):
  
  # create new SN table
  if full_polarization:
    no_pol = len( solution_table[ 0 ][ 0 ] ) / 4
  else:
    no_pol = 1
  if ( no_pol > 2 ):
    raise error( 'more than 2 polarizations currently not supported' )
  antenna_count = len( solution_table[ 0 ] ) - 1
  new_sn_table = new_table( uvim, 'SN', out_version, no_if = 1, no_pol = no_pol )
  row = new_table_row( new_sn_table )
  
  # for each time stamp and antenna fill in row and store in new solution table
  for solution_row in solution_table:
    row.time = float32( solution_row[ 0 ][ 0 ] )
    row.source_id = 1
    row.subarray = 1
    row.freq_id = 1
    row.refant_1 = solution_row[ 0 ][ 1 ]
    if ( no_pol > 1 ):
      row.refant_2 = solution_row[ 0 ][ 1 ]
    # check for flagged times
    for solution in solution_row[ 1 : ]:
      if ( ( solution[ 3 ] <= 0. ) or ( solution[ 3 ] == get_aips_magic_value() ) ):
#        solution[ 0 : 4 ] = [ 1., 0., 0., 0. ]
        solution[ 3 ] = [ 0. ]
      if ( no_pol > 1 ):
        if ( ( solution[ 7 ] <= 0. ) or ( solution[ 7 ] == get_aips_magic_value() ) ):
#          solution[ 4 : 8 ] = [ 1., 0., 0., 0. ]
          solution[ 7 ] = 0.
    for i in range( 1, 1 + antenna_count ):
      [ sol_real, sol_imaginary, sol_delay, sol_weight ] = solution_row[ i ][ 0 : 4 ]
      row.antenna_no = i
      row.real1 = float32( sol_real )
      row.imag1 = float32( sol_imaginary )
      row.delay_1 = float32( sol_delay )
      row.weight_1 = float32( sol_weight )
      if ( no_pol > 1 ):
        [ sol_real, sol_imaginary, sol_delay, sol_weight ] = solution_row[ i ][ 4 : 8 ]
        row.antenna_no = i
        row.real2 = float32( sol_real )
        row.imag2 = float32( sol_imaginary )
        row.delay_2 = float32( sol_delay )
        row.weight_2 = float32( sol_weight )
      new_sn_table.append( row )

  # make two extra dummy rows (needed for SNFLG)
  if ( len( solution_table ) >= 2 ):
    sol1 = solution_table[ -2 ]
    sol2 = solution_table[ -1 ]
    time = sol2[ 0 ][ 0 ]
    dtime = sol2[ 0 ][ 0 ] - sol1[ 0 ][ 0 ]
    for a in range( 2 ):
      time = time + dtime
      row.time = float32( time )
      row.source_id = 1
      row.subarray = 1
      row.freq_id = 1
      row.refant_1 = 0
      if ( no_pol > 1 ):
        row.refant_2 = 0
      for i in range( 1, 1 + antenna_count ):
        row.antenna_no = i
        row.real1 = float32( 0. )
        row.imag1 = float32( 0. )
        row.delay_1 = float32( 0. )
        row.weight_1 = float32( 0. )
        if ( no_pol > 1 ):
          row.real2 = float32( 0. )
          row.imag2 = float32( 0. )
          row.delay_2 = float32( 0. )
          row.weight_2 = float32( 0. )
        new_sn_table.append( row )
  
  # add missing keywords and close new table
  new_sn_table.keywords[ 'NO_ANT' ] = antenna_count
  new_sn_table.keywords[ 'ORIGIN' ] = 1
  new_sn_table.close()
  
  return

###############################################################################

def invert_solutions( uvim, in_version = 0, out_version = 0 ):

  solution_table = read_solution_table( uvim, in_version = in_version )
  new_solution_table = []
  for solution_row in solution_table:
    new_solution_row = [ solution_row[ 0 ] ]
    antenna_number = 0
    for solution in solution_row[ 1 : ]:
      antenna_number = antenna_number + 1
      [ gain_real, gain_imag, delay, weight ] = solution
      gain = complex( gain_real, gain_imag )
      if ( gain == complex( 0., 0. ) ):
        new_solution = [ 0., 0., -delay, weight ]
      else:
        new_gain = 1. / gain
        new_solution = [ new_gain.real, new_gain.imag, - delay, weight ]
      new_solution_row.append( [ x for x in new_solution ] )
    new_solution_table.append( [ x for x in new_solution_row ] )
  write_solution_table( uvim, new_solution_table, out_version = out_version )

  return

###############################################################################

def re_reference_solutions( uvim, reference_antenna, in_version = 0, out_version = 0 ):
# Note that missing solutions for the reference antenna will result in missing solutions for all antennas
  solution_table = read_solution_table( uvim, in_version = in_version,
      allow_zero_reference = True )
  antenna_count = len( solution_table[ 0 ] ) - 1
  if ( ( reference_antenna < 1 ) or ( reference_antenna > antenna_count ) ):
    raise error( 'invalid reference antenna number' )
  new_solution_table = []
  for solution_row in solution_table:
    [ time, ref_ant, dummy, dummy2 ] = solution_row[ 0 ]
    new_solution_row = [ [ time, reference_antenna, 0., 0. ] ]
    [ ref_gain_real, ref_gain_imag, ref_delay, ref_weight ] = solution_row[ reference_antenna ]
    ref_gain = complex( ref_gain_real, ref_gain_imag )
    if ( ( ref_weight == 0. ) or ( ref_gain == complex( 0., 0. ) ) ):
      for solution in solution_row[ 1 : ]:
        new_solution = [ 0., 0., 0., 0. ]
        new_solution_row.append( [ x for x in new_solution ] )
    else:
      for solution in solution_row[ 1 : ]:
        [ gain_real, gain_imag, delay, weight ] = solution
        gain = complex( gain_real, gain_imag )
        if ( ( weight == 0. ) or ( gain == complex( 0., 0. ) ) ):
          new_solution = [ 0., 0., 0., 0. ]
        else:
          new_gain = gain / ref_gain
          new_solution = [ new_gain.real, new_gain.imag, delay, weight ]
        new_solution_row.append( [ x for x in new_solution ] )
    new_solution_table.append( [ x for x in new_solution_row ] )
  write_solution_table( uvim, new_solution_table, out_version = out_version )

  return

###############################################################################

def combine_solutions( uvim, in_version_1 = 0, in_version_2 = 0, out_version = 0,
    force_match = True, invert_2 = False, full_polarization = True ):
  
  v1 = in_version_1
  v2 = in_version_2
  if ( v2 == 0 ):
    v2 = uvim.table_highver( 'SN' )
  if ( ( v1 == 0 ) and ( v2 == uvim.table_highver( 'SN' ) ) ):
    i = v2 - 1
    while ( ( i > 0 ) and ( not table_exists( uvim, 'SN', i ) ) ):
      i = i - 1
    if ( i == 0 ):
      raise error( 'there are no 2 input SN tables available to combine' )
    v1 = i
  
  # read SN tables and check for table cross-consistency
  solution_table_1 = read_solution_table( uvim, in_version = v1, 
      full_polarization = full_polarization )
  solution_table_2 = read_solution_table( uvim, in_version = v2, 
      full_polarization = full_polarization )
  if ( ( not force_match ) and ( len( solution_table_1 ) != len( solution_table_2 ) ) ):
    raise error( 'the 2 input SN tables have different lengths (e.g. ' + 
        'different number of time stamps)' )
  wiz_uvim = wizardry( uvim )
  sn_table_1 = wiz_uvim.table( 'SN', v1 )
  sn_table_2 = wiz_uvim.table( 'SN', v2 )
  if ( sn_table_1.keywords[ 'NO_ANT' ] != sn_table_2.keywords[ 'NO_ANT' ] ):
    raise error( 'the 2 input SN tables have different header entries for NO_ANT' )
  if ( sn_table_1.keywords[ 'NO_IF' ] != sn_table_2.keywords[ 'NO_IF' ] ):
    raise error( 'the 2 input SN tables have different header entries for NO_IF' )
  if full_polarization:
    if ( sn_table_1.keywords[ 'NO_POL' ] != sn_table_2.keywords[ 'NO_POL' ] ):
      raise error( 'the 2 input SN tables have different header entries for NO_POL' )
    no_pol = len( solution_table_1[ 0 ][ 0 ] ) / 4
  else:
    no_pol = 1
  if ( no_pol > 2 ):
    raise error( 'more than 2 polarizations currently not supported' )
  
  # make tables match, even if some time stamps are missing
  if force_match:
    i1 = 0
    i2 = 0
    l1 = len( solution_table_1 )
    l2 = len( solution_table_2 )
    while ( ( i1 < l1 ) or ( i2 < l2 ) ):
      if ( i1 == l1 ):
        solution_table_1.append( [ [ x for x in solution_table_2[ i2 ][ 0 ] ] ] +
            [ [ 0. for x in y ] for y in solution_table_2[ i2 ][ 1 : ] ] )
        i2 = i2 + 1
      elif ( i2 == l2 ):
        solution_table_2.append( [ [ x for x in solution_table_1[ i1 ][ 0 ] ] ] +
            [ [ 0. for x in y ] for y in solution_table_1[ i1 ][ 1 : ] ] )
        i1 = i1 + 1
      elif ( solution_table_1[ i1 ][ 0 ][ 0 ] > solution_table_2[ i2 ][ 0 ][ 0 ] ):
        solution_table_1.append( [ [ x for x in solution_table_2[ i2 ][ 0 ] ] ] +
            [ [ 0. for x in y ] for y in solution_table_2[ i2 ][ 1 : ] ] )
        i2 = i2 + 1
      elif ( solution_table_1[ i1 ][ 0 ][ 0 ] < solution_table_2[ i2 ][ 0 ][ 0 ] ):
        solution_table_2.append( [ [ x for x in solution_table_1[ i1 ][ 0 ] ] ] +
            [ [ 0. for x in y ] for y in solution_table_1[ i1 ][ 1 : ] ] )
        i1 = i1 + 1
      else:
        i1 = i1 + 1
        i2 = i2 + 1
    solution_table_1.sort( cmp = lambda a, b: cmp( a[ 0 ][ 0 ], b[ 0 ][ 0 ] ) )
    solution_table_2.sort( cmp = lambda a, b: cmp( a[ 0 ][ 0 ], b[ 0 ][ 0 ] ) )
  
  # combine solutions from both tables
  # fail if timestamps or reference antennas don't match
  new_solution_table = []
  for i in range( len( solution_table_1 ) ):
    solution_row_1 = solution_table_1[ i ]
    solution_row_2 = solution_table_2[ i ]
    [ time_1, ref_ant_1 ] = solution_row_1[ 0 ][ 0 : 2 ]
    [ time_2, ref_ant_2 ] = solution_row_2[ 0 ][ 0 : 2 ]
    if ( time_1 != time_2 ):
      raise error( 'the 2 input SN tables have different time stamps' )
    if ( ref_ant_1 != ref_ant_2 ):
      # check if there are any valid solutions
      sol1_empty = ( [ sol[ 3 ] for sol in solution_row_1[ 1 : ] ] == 
          [ 0. for sol in solution_row_1[ 1 : ] ] )
      sol2_empty = ( [ sol[ 3 ] for sol in solution_row_2[ 1 : ] ] == 
          [ 0. for sol in solution_row_2[ 1 : ] ] )
      if ( no_pol > 1 ):
        sol1_empty = sol1_empty and ( [ sol[ 7 ] for sol in solution_row_1[ 1 : ] ] == 
            [ 0. for sol in solution_row_1[ 1 : ] ] )
        sol2_empty = sol2_empty and ( [ sol[ 7 ] for sol in solution_row_2[ 1 : ] ] == 
            [ 0. for sol in solution_row_2[ 1 : ] ] )
      if ( ( not sol1_empty ) and ( not sol2_empty ) ):
        raise error( 'the 2 input SN tables have different reference antennas' )
      elif ( sol1_empty ):
        ref_ant_1 = ref_ant_2
        solution_row_1[ 0 ][ 1 ] = ref_ant_2
    new_solution_row = [ solution_row_1[ 0 ] ]
    for j in range( 1, len( solution_row_1 ) ):
      [ gain_1_real, gain_1_imag, delay_1, weight_1 ] = solution_row_1[ j ][ 0 : 4 ]
      gain_1 = complex( gain_1_real, gain_1_imag )
      [ gain_2_real, gain_2_imag, delay_2, weight_2 ] = solution_row_2[ j ][ 0 : 4 ]
      gain_2 = complex( gain_2_real, gain_2_imag )
      if ( ( weight_1 == 0. ) or ( weight_2 == 0. ) ):
        new_solution = [ 0., 0., 0., 0. ]
      else:
        if invert_2:
          new_gain = gain_1 / gain_2
          new_delay = delay_1 - delay_2
        else:
          new_gain = gain_1 * gain_2
          new_delay = delay_1 + delay_2
        new_weight = 1. / sqrt( ( 1. / weight_1 )**2 + ( 1. / weight_2 )**2 )
        new_solution = [ new_gain.real, new_gain.imag, new_delay, new_weight ]
      if ( no_pol > 1 ):
        [ gain_1_real, gain_1_imag, delay_1, weight_1 ] = solution_row_1[ j ][ 4 : 8 ]
        gain_1 = complex( gain_1_real, gain_1_imag )
        [ gain_2_real, gain_2_imag, delay_2, weight_2 ] = solution_row_2[ j ][ 4 : 8 ]
        gain_2 = complex( gain_2_real, gain_2_imag )
        if ( ( weight_1 == 0. ) or ( weight_2 == 0. ) ):
          new_solution = new_solution + [ 0., 0., 0., 0. ]
        else:
          if invert_2:
            new_gain = gain_1 / gain_2
            new_delay = delay_1 - delay_2
          else:
            new_gain = gain_1 * gain_2
            new_delay = delay_1 + delay_2
          new_weight = 1. / sqrt( ( 1. / weight_1 )**2 + ( 1. / weight_2 )**2 )
          new_solution = new_solution + [ new_gain.real, new_gain.imag, new_delay, new_weight ]
      new_solution_row.append( [ x for x in new_solution ] )
    new_solution_table.append( [ x for x in new_solution_row ] )
  write_solution_table( uvim, new_solution_table, out_version = out_version,
      full_polarization = full_polarization )
  
  return

###############################################################################

def calculate_solution_table_phase_mean_rms_stddev( uvim, version_list = [ 0 ], antenna_list = [] ):

  mrs_list = []
  for version in version_list:
    phase_list = []
    solution_table = read_solution_table( uvim, in_version = version )  
    if ( len( antenna_list ) == 0 ):
      ant_count = len( solution_table[ 0 ] ) - 1
      ant_list = range( 1, 1 + ant_count )
    else:
      ant_count = len( antenna_list )
      ant_list = [ ant for ant in antenna_list ]
    for antenna in ant_list:
      for solution_row in solution_table:
        [ time, ref_ant, dummy, dummy2 ] = solution_row[ 0 ]
        [ gain_real, gain_imag, delay, weight ] = solution_row[ antenna ]
        gain = complex( gain_real, gain_imag )
        if ( ( weight != 0. ) and ( gain != complex( 0., 0. ) ) ):
          [ gain_amp, gain_phase ] = complex_to_r_phi( gain )
          phase_list.append( gain_phase )
    if ( len( phase_list ) > 0 ):
      mean_phase = sum( phase_list ) / float( len( phase_list ) )
      for phase in phase_list:
        mrs_list.append( [ phase, phase**2, ( phase - mean_phase )**2 ] )

  phase_mean = None
  phase_rms = None
  phase_stddev = None
  if ( len( mrs_list ) > 0 ):
    mrs_array = array( mrs_list, dtype = float64 )
    phase_mean = mrs_array[ : , 0 ].sum() / float( len( mrs_list ) )
    phase_rms = sqrt( mrs_array[ : , 1 ].sum() / float( len( mrs_list ) ) )
    phase_stddev = sqrt( mrs_array[ : , 2 ].sum() / float( len( mrs_list ) ) )

  return [ phase_mean, phase_rms, phase_stddev ]

###############################################################################

def calculate_solution_table_phase_rms( uvim, version = 0 ):
  
  solution_table = read_solution_table( uvim, in_version = version )  
  ant_count = len( solution_table[ 0 ] ) - 1
  cross_list = []
  for solution_row in solution_table:
    phase_list = []
    [ time, ref_ant, dummy, dummy2 ] = solution_row[ 0 ]
    for antenna in range( 1, 1 + ant_count ):
      [ gain_real, gain_imag, delay, weight ] = solution_row[ antenna ]
      gain = complex( gain_real, gain_imag )
      if ( ( weight != 0. ) and ( gain != complex( 0., 0. ) ) ):
        [ gain_amp, gain_phase ] = complex_to_r_phi( gain )
        phase_list.append( gain_phase )
    for i in range( len( phase_list ) ):
      for j in range( i + 1, len( phase_list ) ):
        cross_list.append( amodulo( ( phase_list[ i ] - phase_list[ j ] ) + 180., 360 ) - 180. )
        cross_list.append( amodulo( ( phase_list[ j ] - phase_list[ i ] ) + 180., 360 ) - 180. )
  
  if ( len( cross_list ) > 1 ):
    cross_array = array( cross_list, dtype = float64 )
    phase_rms = sqrt( ( cross_array**2 ).sum() / float( len( cross_array ) ) )
  else:
    phase_rms = None
  
  return phase_rms

###############################################################################

def calculate_solution_table_phase_error( uvim, version = 0, antenna_list = [], use_weights = False ):

  # read solution table
  solution_table = read_solution_table( uvim, in_version = version )  
  ant_count = len( solution_table[ 0 ] ) - 1
  if ( len( antenna_list ) == 0 ):
    ant_list = range( 1, 1 + ant_count )
  else:
    ant_list = [ ant for ant in antenna_list ]

  # step through time
  error2_list = []
  for solution_row in solution_table:

    # get non-zero antenna gains
    index_list = []
    [ time, ref_ant, dummy, dummy2 ] = solution_row[ 0 ]
    for antenna in range( 1, len( solution_row ) ):
      [ gain_real, gain_imag, delay, weight ] = solution_row[ antenna ]
      gain = complex( gain_real, gain_imag )
      if ( ( weight != 0. ) and ( gain != complex( 0., 0. ) ) ):
        index_list.append( antenna )
    phase_count = len( index_list )
    if ( phase_count < 2 ):
      continue

    # determine mean absolute phase difference per antenna
    phase_list = []
    if use_weights:
      weight_list = []
    for i in index_list:
      [ gain_real, gain_imag, delay, weight ] = solution_row[ i ]
      gain = complex( gain_real, gain_imag )
      [ gain_amp, gain_phase ] = complex_to_r_phi( gain )
      phase_list.append( gain_phase )
      if use_weights:
        weight_list.append( weight )
    dphase_array = array( phase_list, dtype = float64 )
    dphase_array = resize( dphase_array, ( phase_count, phase_count ) )
    dphase_array = transpose( dphase_array ) - dphase_array
    dphase_array = abs( amodulo( dphase_array + 180., 360. ) - 180. )
    if use_weights:
      dweight_array = array( weight_list, dtype = float64 )
      dweight_array = resize( ( 1. / dweight_array )**2, ( phase_count, phase_count ) )
      dweight_array = transpose( dweight_array ) + dweight_array
      dweight_array = 1. / sqrt( dweight_array )
      dphase_array = dweight_array * dphase_array
    chi_array = add.reduce( dphase_array, 1 )
    if use_weights:
      normalize = add.reduce( dweight_array, 1 ) - diagonal( dweight_array )
    else:
      normalize = float( phase_count - 1 )
    chi_array = chi_array / normalize

    # combine antenna errors into one number
    ant_error2_list = []
    for ant in ant_list:
      try:
        j = index_list.index( ant )
      except:
        continue
      ant_error2_list.append( chi_array[ j ]**2 )
    error2 = mean( ant_error2_list )
    error2_list.append( error2 )

  phase_error = sqrt( mean( error2_list ) )
  return phase_error

###############################################################################

def calculate_solution_table_phase_stddev( uvim, version_list = [ 0 ], antenna_list = [] ):

  if ( len( antenna_list ) == 0 ):
    ant_count = len( solution_table[ 0 ] ) - 1
    ant_list = range( 1, 1 + ant_count )
  else:
    ant_count = len( antenna_list )
    ant_list = [ ant for ant in antenna_list ]

  stddev_list = []
  for version in version_list:
    solution_table = read_solution_table( uvim, in_version = version )  
    for antenna in ant_list:
      phase_list = []
      for solution_row in solution_table:
        [ time, ref_ant, dummy, dummy2 ] = solution_row[ 0 ]
        [ gain_real, gain_imag, delay, weight ] = solution_row[ antenna ]
        gain = complex( gain_real, gain_imag )
        if ( ( weight != 0. ) and ( gain != complex( 0., 0. ) ) ):
          [ gain_amp, gain_phase ] = complex_to_r_phi( gain )
          phase_list.append( gain_phase )
    if ( len( phase_list ) > 0 ):
      mean_phase = sum( phase_list ) / float( len( phase_list ) )
    for phase in phase_list:
      stddev_list = ( phase - mean_phase )**2

  phase_stddev = None
  if ( len( stddev_list ) > 0 ):
    phase_rms = sqrt( sum( stddev_list ) / float( len( stddev_list ) ) )

  return phase_rms

###############################################################################

def copy_solution_tables( facets, uv, version = 0 ):
  facet_count = restore_parameter( facets, 'facet_count' )
  for i in range( 1, facet_count + 1 ):
    facet_i = get_facet( facets, i )
    call_aips_task( 'TACOP', indata = facet_i, inext = 'SN', invers = version, ncount = 1,
        outdata = uv, outvers = 0 )
  return

###############################################################################

def smooth_solutions_in_time( uvim, in_version = 0, out_version = 0,
    phase_window = 60., amplitude_window = 0., full_polarization = True,
    order = 0, short_baselines = False, loss_fraction = 0.05, max_gap = 5. ):
  # window widths in seconds; gap in minutes
  # assume solutions are in time order
  
  phase_time_radius = phase_window / ( 2. * 24. * 60. * 60. )
  amplitude_time_radius = amplitude_window / ( 2. * 24. * 60. * 60. )
  solution_table = read_solution_table( uvim, in_version = in_version,
      full_polarization = full_polarization )
  solution_count = len( solution_table )
  antenna_count = len( solution_table[ 0 ] ) - 1
  reference_antenna = solution_table[ 0 ][ 0 ][ 1 ]
  new_solution_table = []
  no_pol = len( solution_table[ 0 ][ 0 ] ) / 4
  if ( not full_polarization ):
    no_pol = 1
  if ( no_pol > 2 ):
    raise error( 'more than 2 polarizations are currently not supported' )
  solution_array = array( solution_table, dtype = float64 )
  time_array = solution_array[ : , 0, 0 ]
  if ( is_uv( uvim ) and short_baselines ):
    re_reference = True
  else:
    re_reference = False

  # build container
  for n in range( solution_count ):
    solution_row = solution_table[ n ]
    new_solution_row = [ solution_row[ 0 ] ]
    [ time, ref_ant ] = solution_row[ 0 ][ 0 : 2 ]
    if ( ref_ant != reference_antenna ):
      # check for any valid solutions
      sel = awhere( solution_array[ n, 1 : , 3 ] > 0. )
      if ( len( sel ) > 0 ):
        raise error( 'reference antenna changes within solution table' )
      if ( no_pol > 1 ):
        sel = awhere( solution_array[ n, 1 : , 7 ] > 0. )
        if ( len( sel ) > 0 ):
          raise error( 'reference antenna changes within solution table' )
      ref_ant = reference_antenna
    new_solution = [ time, ref_ant, 0., 0. ]
    if ( no_pol > 1 ):
      new_solution = new_solution + [ 0., 0., 0., 0. ]
    new_solution_row = [ [ x for x in new_solution ] ]
    for solution in solution_row[ 1 : ]:
      new_solution = [ 0., 0., 0., 0. ] # solution[ 3 ] ]
      if ( no_pol > 1 ):
        new_solution = new_solution + [ 0., 0., 0., 0. ] # solution[ 7 ] ]
      new_solution_row.append( [ x for x in new_solution ] )
    new_solution_table.append( [ x for x in new_solution_row ] )
  
  # smooth per antenna
  if re_reference:
    ant_ref_list = get_nearest_antennas( uvim, reference_antenna )
    if ( no_pol > 1 ):
      ant_ref_list2 = [ [ y for y in x ] for x in ant_ref_list ]
  for k in range( antenna_count ):
    
    # count solutions
    sol_count = 0
    for n in range( solution_count ):
      solution = solution_array[ n, k + 1 ]
      if ( solution[ 3 ] != 0. ):
        sol_count = sol_count + 1
    if ( sol_count > 0 ):
      
      if re_reference:
        # find suitable nearby reference antenna
        ant_indx = [ a[ 0 ] for a in ant_ref_list ].index( k + 1 )
        ref_ant = ant_ref_list[ ant_indx ][ 1 ]
        while ( ref_ant != reference_antenna ):
          ref_count = 0
          for n in range( solution_count ):
            solution = solution_array[ n, k + 1 ]
            if ( solution[ 3 ] != 0. ):
              ref_solution = solution_array[ n, ref_ant ]
              if ( ref_solution[ 3 ] != 0. ):
                ref_count = ref_count + 1
          loss_count = sol_count - ref_count
          if ( float( loss_count ) <= loss_fraction * float( sol_count ) ):
            break
          ref_ant_indx = [ a[ 0 ] for a in ant_ref_list ].index( ref_ant )
          ref_ant = ant_ref_list[ ref_ant_indx ][ 1 ]
          ant_ref_list[ ant_indx ][ 1 ] = ref_ant
      
      # get non-flagged (re-referenced) solutions
      ant_sol_table = []
      for n in range( solution_count ):
        solution = solution_array[ n, k + 1 ]
        if re_reference:
          ref_solution = solution_array[ n, ref_ant ]
        if ( solution[ 3 ] != 0. ):
          ant_sol = ( [ time_array[ n ] ] + 
              complex_to_r_phi( complex( solution[ 0 ], solution[ 1 ] ) ) + 
              [ solution[ 2 ] ] )
          if re_reference:
            if ( ref_solution[ 3 ] != 0. ):
              ant_sol[ 2 ] = amodulo( ( ant_sol[ 2 ] - complex_to_r_phi( 
                  complex( ref_solution[ 0 ], ref_solution[ 1 ] ) )[ 1 ] ) +
                      180., 360 ) - 180.
              ant_sol_table.append( ant_sol )
          else:
            ant_sol_table.append( ant_sol )
      
      # convert to array
      ant_sol_table.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
      ant_sol_array = array( ant_sol_table, dtype = float64 )
      
      # unwrap the phase (phase jumps greater than 180 are made smaller by adding n*360)
      if ( len( ant_sol_array ) < 20 ):
        phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
      else:
        phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.01 )
        if sometrue( isnan( phase_array ) ):
          phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.001 )
        if ( sometrue( isnan( phase_array ) ) or ( max( fabs( phase_array ) ) > 1.e4 ) ):
          phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
      ant_sol_array[ : , 2 ] = phase_array
      
      # loop over solution times
      for ant_sol in ant_sol_array:
        [ time, amp, phase, delay ] = ant_sol
        
        # get data to smooth
        sel = awhere( abs( ant_sol_array[ : , 0 ] - time ) <= amplitude_time_radius )
        amp_array = aget( ant_sol_array, sel )
        amp_offsets = amp_array[ : , 0 ] - time
        sel = awhere( abs( ant_sol_array[ : , 0 ] - time ) <= phase_time_radius )
        phase_array = aget( ant_sol_array, sel )
        phase_offsets = phase_array[ : , 0 ] - time

        # check for big gaps in data
        if ( len( amp_offsets ) > 1 ):
          damp_offsets = amp_offsets[ 1 : ] - amp_offsets[ : -1 ]
          sel = awhere( damp_offsets * 24. * 60. > max_gap )
          if ( len( sel ) > 0 ):
            min_amp_index = 0
            max_amp_index = len( amp_offsets )
            amp_index = awhere( abs( amp_offsets ) == 
                abs( amp_offsets ).min() )[ 0, 0 ]
            for s in sel:
              if ( s[ 0 ] < amp_index ):
                min_amp_index = s[ 0 ] + 1
              if ( s[ 0 ] >= amp_index ):
                max_amp_index = s[ 0 ] + 1
                break
            amp_array = amp_array[ min_amp_index : max_amp_index ]
            amp_offsets = amp_offsets[ min_amp_index : max_amp_index ]
        if ( len( phase_offsets ) > 1 ):
          dphase_offsets = phase_offsets[ 1 : ] - phase_offsets[ : -1 ]
          sel = awhere( dphase_offsets * 24. * 60. > max_gap )
          if ( len( sel ) > 0 ):
            min_phase_index = 0
            max_phase_index = len( phase_offsets )
            phase_index = awhere( abs( phase_offsets ) == 
                abs( phase_offsets ).min() )[ 0, 0 ]
            for s in sel:
              if ( s[ 0 ] < phase_index ):
                min_phase_index = s[ 0 ] + 1
              if ( s[ 0 ] >= phase_index ):
                max_phase_index = s[ 0 ] + 1
                break
            phase_array = phase_array[ min_phase_index : max_phase_index ]
            phase_offsets = phase_offsets[ min_phase_index : max_phase_index ]

        # smooth amplitudes and phases
        if ( ( len( amp_array ) > 0 ) and ( len( phase_array ) > 0 ) ):
          smooth_delay = phase_array[ : , 3 ].mean()
          smooth_amp = amp_array[ : , 1 ].mean()
          dim = min( len( phase_array ) - 1, order )
          P = zeros( ( len( phase_offsets ), dim + 1 ), dtype = phase_offsets.dtype )
          P[ : , 0 ] = 1.
          if ( dim >= 1 ):
            P[ : , 1 ] = phase_offsets
          if ( dim >= 2 ):
            P[ : , 2 ] = phase_offsets**2
          Pt = transpose( P )
          smooth_phase = dot( linalg.inv( dot( Pt, P ) ),
             dot( Pt, phase_array[ : , 2 ] ) )[ 0 ]
          smooth_gain = r_phi_to_complex( [ smooth_amp, smooth_phase ] )
          n = awhere( time_array == time )[ 0, 0 ]
          new_solution_table[ n ][ k + 1 ][ 0 : 4 ] = [ 
              smooth_gain.real, smooth_gain.imag, smooth_delay,
              solution_array[ n, k + 1, 3 ] ]
    
    if ( no_pol > 1 ):
      
      # count solutions
      sol_count = 0
      for n in range( solution_count ):
        solution = solution_array[ n, k + 1 ]
        if ( solution[ 7 ] != 0. ):
          sol_count = sol_count + 1
      if ( sol_count > 0 ):
        
        if re_reference:
          # find suitable nearby reference antenna
          ant_indx = [ a[ 0 ] for a in ant_ref_list ].index( k + 1 )
          ref_ant = ant_ref_list[ ant_indx ][ 1 ]
          while ( ref_ant != reference_antenna ):
            ref_count = 0
            for n in range( solution_count ):
              solution = solution_array[ n, k + 1 ]
              if ( solution[ 7 ] != 0. ):
                ref_solution = solution_array[ n, ref_ant ]
                if ( ref_solution[ 7 ] != 0. ):
                  ref_count = ref_count + 1
            loss_count = sol_count - ref_count
            if ( float( loss_count ) <= loss_fraction * float( sol_count ) ):
              break
            ref_ant_indx = [ a[ 0 ] for a in ant_ref_list ].index( ref_ant )
            ref_ant = ant_ref_list[ ref_ant_indx ][ 1 ]
            ant_ref_list[ ant_indx ][ 1 ] = ref_ant
        
        # get non-flagged (re-referenced) solutions
        ant_sol_table = []
        for n in range( solution_count ):
          solution = solution_array[ n, k + 1 ]
          if re_reference:
            ref_solution = solution_array[ n, ref_ant ]
          if ( solution[ 7 ] != 0. ):
            ant_sol = ( [ time_array[ n ] ] + 
                complex_to_r_phi( complex( solution[ 4 ], solution[ 5 ] ) ) + 
                [ solution[ 6 ] ] )
            if re_reference:
              if ( ref_solution[ 7 ] != 0. ):
                ant_sol[ 2 ] = amodulo( ( ant_sol[ 2 ] - complex_to_r_phi( 
                    complex( ref_solution[ 0 ], ref_solution[ 5 ] ) )[ 1 ] ) +
                        180., 360 ) - 180.
                ant_sol_table.append( ant_sol )
            else:
              ant_sol_table.append( ant_sol )
  
        # convert to array
        ant_sol_table.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
        ant_sol_array = array( ant_sol_table, dtype = float64 )
        
        # unwrap the phase (phase jumps greater than 180 are made smaller by adding n*360)
        if ( len( ant_sol_array ) < 20 ):
          phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
        else:
          phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.01 )
          if sometrue( isnan( phase_array ) ):
            phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.001 )
          if ( sometrue( isnan( phase_array ) ) or ( max( fabs( phase_array ) ) > 1.e4 ) ):
            phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
        ant_sol_array[ : , 2 ] = phase_array
        
        # loop over solution times
        for ant_sol in ant_sol_array:
          [ time, amp, phase, delay ] = ant_sol
          
          # get data to smooth
          sel = awhere( abs( ant_sol_array[ : , 0 ] - time ) <= amplitude_time_radius )
          amp_array = aget( ant_sol_array, sel )
          amp_offsets = amp_array[ : , 0 ] - time
          sel = awhere( abs( ant_sol_array[ : , 0 ] - time ) <= phase_time_radius )
          phase_array = aget( ant_sol_array, sel )
          phase_offsets = phase_array[ : , 0 ] - time
          
          # check for big gaps in data
          if ( len( amp_offsets ) > 1 ):
            damp_offsets = amp_offsets[ 1 : ] - amp_offsets[ : -1 ]
            sel = awhere( damp_offsets * 24. * 60. > max_gap )
            if ( len( sel ) > 0 ):
              min_amp_index = 0
              max_amp_index = len( amp_offsets )
              amp_index = awhere( abs( amp_offsets ) == 
                  abs( amp_offsets ).min() )[ 0, 0 ]
              for s in sel:
                if ( s[ 0 ] < amp_index ):
                  min_amp_index = s[ 0 ] + 1
                if ( s[ 0 ] >= amp_index ):
                  max_amp_index = s[ 0 ] + 1
                  break
              amp_array = amp_array[ min_amp_index : max_amp_index ]
              amp_offsets = amp_offsets[ min_amp_index : max_amp_index ]
          if ( len( phase_offsets ) > 1 ):
            dphase_offsets = phase_offsets[ 1 : ] - phase_offsets[ : -1 ]
            sel = awhere( dphase_offsets * 24. * 60. > max_gap )
            if ( len( sel ) > 0 ):
              min_phase_index = 0
              max_phase_index = len( phase_offsets )
              phase_index = awhere( abs( phase_offsets ) == 
                  abs( phase_offsets ).min() )[ 0, 0 ]
              for s in sel:
                if ( s[ 0 ] < phase_index ):
                  min_phase_index = s[ 0 ] + 1
                if ( s[ 0 ] >= phase_index ):
                  max_phase_index = s[ 0 ] + 1
                  break
              phase_array = phase_array[ min_phase_index : max_phase_index ]
              phase_offsets = phase_offsets[ min_phase_index : max_phase_index ]
          
          # smooth amplitudes and phases
          if ( ( len( amp_array ) > 0 ) and ( len( phase_array ) > 0 ) ):
            smooth_delay = phase_array[ : , 3 ].mean()
            smooth_amp = amp_array[ : , 1 ].mean()
            dim = min( len( phase_array ) - 1, order )
            P = zeros( ( len( phase_offsets ), dim + 1 ), dtype = phase_offsets.dtype )
            P[ : , 0 ] = 1.
            if ( dim >= 1 ):
              P[ : , 1 ] = phase_offsets
            if ( dim >= 2 ):
              P[ : , 2 ] = phase_offsets**2
            Pt = transpose( P )
            smooth_phase = dot( linalg.inv( dot( Pt, P ) ),
                dot( Pt, phase_array[ : , 2 ] ) )[ 0 ]
            smooth_gain = r_phi_to_complex( [ smooth_amp, smooth_phase ] )
            n = awhere( time_array == time )[ 0, 0 ]
            new_solution_table[ n ][ k + 1 ][ 4 : 8 ] = [ 
                smooth_gain.real, smooth_gain.imag, smooth_delay,
                solution_array[ n, k + 1, 7 ] ]
    
  # re-reference phases back to common reference antenna
  if re_reference:
    for solution_row in new_solution_table:
      amp_phase_list = [ complex_to_r_phi( complex( s[ 0 ], s[ 1 ] ) )
          for s in solution_row[ 1 : ] ]
      ra_list = [ [ x for x in y ] for y in ant_ref_list ]
      done = False
      while ( not done ):
        done = True
        for ra in ra_list:
          if ( ra[ 1 ] != reference_antenna ):
            if ( solution_row[ ra[ 1 ] ][ 3 ] > 0. ):
              p1 = amp_phase_list[ ra[ 0 ] - 1 ][ 1 ]
              p2 = amp_phase_list[ ra[ 1 ] - 1 ][ 1 ]
              amp_phase_list[ ra[ 0 ] - 1 ][ 1 ] = (
                  amodulo( ( p1 + p2 ) + 180., 360. ) - 180. )
            else:
              solution_row[ ra[ 0 ] ][ 3 ] = 0.
            ref_ant_indx = [ a[ 0 ] for a in ra_list ].index( ra[ 1 ] )
            ref_ant = ra_list[ ref_ant_indx ][ 1 ]
            ra[ 1 ] = ref_ant
            if ( ref_ant != reference_antenna ):
              done = False
      for i in range( len( amp_phase_list ) ):
        c = r_phi_to_complex( amp_phase_list[ i ] )
        solution_row[ i + 1 ][ 0 ] = c.real
        solution_row[ i + 1 ][ 1 ] = c.imag
      
      if ( no_pol > 1 ):
        amp_phase_list = [ complex_to_r_phi( complex( s[ 4 ], s[ 5 ] ) )
            for s in solution_row[ 1 : ] ]
        ra_list = [ [ x for x in y ] for y in ant_ref_list2 ]
        done = False
        while ( not done ):
          done = True
          for ra in ra_list:
            if ( ra[ 1 ] != reference_antenna ):
              if ( solution_row[ ra[ 1 ] ][ 7 ] > 0. ):
                p1 = amp_phase_list[ ra[ 0 ] - 1 ][ 1 ]
                p2 = amp_phase_list[ ra[ 1 ] - 1 ][ 1 ]
                amp_phase_list[ ra[ 0 ] - 1 ][ 1 ] = (
                    amodulo( ( p1 + p2 ) + 180., 360. ) - 180. )
              else:
                solution_row[ ra[ 0 ] ][ 7 ] = 0.
              ref_ant_indx = [ a[ 0 ] for a in ra_list ].index( ra[ 1 ] )
              ref_ant = ra_list[ ref_ant_indx ][ 1 ]
              ra[ 1 ] = ref_ant
              if ( ref_ant != reference_antenna ):
                done = False
        for i in range( len( amp_phase_list ) ):
          c = r_phi_to_complex( amp_phase_list[ i ] )
          solution_row[ i + 1 ][ 4 ] = c.real
          solution_row[ i + 1 ][ 5 ] = c.imag
  
  write_solution_table( uvim, new_solution_table, out_version = out_version,
      full_polarization = full_polarization )
  return

###############################################################################

def get_solution_weights( uvim, in_version = 0 ):
  solution_table = read_solution_table( uvim, in_version = in_version )
  weight_table = []
  for solution_row in solution_table:
    [ time, ref_ant, dummy, dummy2 ] = solution_row[ 0 ]
#    weight_row = [ time ]
    weight_row = []
    antenna_number = 0
    for solution in solution_row[ 1 : ]:
      antenna_number = antenna_number + 1
      [ gain_real, gain_imag, delay, weight ] = solution
      weight_row.append( weight )
    weight_table.append( weight_row )
  return ( weight_table )

###############################################################################

def apply_solution_table( uv, version = 0, keep_flags = True ):
  if ( version == 0 ):
    solution_version = uv.table_highver( 'SN' )
  else:
    solution_version = version
  new_uv = get_aips_file( uv.disk, uv.name, 'CAL', - 1, 'UV' )
  call_aips_task( 'SPLIT', indata = uv, docalib = 100, gainuse = solution_version,
      douvcomp = 0, flagver = -1,
      outdisk = new_uv.disk, outclass = new_uv.klass, outseq = new_uv.seq )
  if keep_flags:
    for i in range( 1, 1 + uv.table_highver( 'FG' ) ):
      if table_exists( uv, 'FG', i ):
        call_aips_task( 'TACOP', indata = uv, inext = 'FG', invers = i,
            ncount = 1, outdata = new_uv, outvers = i )
  return new_uv

###############################################################################

def determine_polarization_solution_offsets( uvim, in_version = 0, out_version = 0,
    normalize_gains = True ):
  
  # read SN tables and check for table cross-consistency
  solution_table = read_solution_table( uvim, in_version = in_version,
      full_polarization = True )
  no_pol = len( solution_table[ 0 ][ 0 ] ) / 4
  if ( no_pol != 2 ):
    raise error( 'number of polarization products is other than two' )
  
  # combine solutions from both tables
  new_solution_table = []
  for i in range( len( solution_table ) ):
    solution_row = solution_table[ i ]
    [ time, ref_ant ] = solution_row[ 0 ][ 0 : 2 ]
    new_solution_row = [ solution_row[ 0 ] ]
    for j in range( 1, len( solution_row ) ):
      [ gain_1_real, gain_1_imag, delay_1, weight_1 ] = solution_row[ j ][ 0 : 4 ]
      [ gain_2_real, gain_2_imag, delay_2, weight_2 ] = solution_row[ j ][ 4 : 8 ]
      if ( ( weight_1 == 0. ) or ( weight_2 == 0. ) ):
        new_solution = [ 0., 0., 0., 0., 0., 0., 0., 0. ]
      else:
        [ amp_1, phase_1 ] = complex_to_r_phi( complex( gain_1_real, gain_1_imag ) )
        [ amp_2, phase_2 ] = complex_to_r_phi( complex( gain_2_real, gain_2_imag ) )
        if normalize_gains:
          amp_2 = amp_2 / amp_1
          amp_1 = 1.
        new_phase = amodulo( ( phase_2 - phase_1 ) + 180., 360. ) - 180.
        new_gain = r_phi_to_complex( [ amp_2, new_phase ] )
        new_delay = delay_1 - delay_2
        new_weight = 1. / sqrt( ( 1. / weight_1 )**2 + ( 1. / weight_2 )**2 )
        new_solution = [ amp_1, 0., 0., new_weight,
              new_gain.real, new_gain.imag, new_delay, new_weight ]
      new_solution_row.append( [ x for x in new_solution ] )
    new_solution_table.append( [ x for x in new_solution_row ] )
  
  # save single polarizations
  for j in range( 1, len( solution_table[ 0 ] ) ):
    found_1 = False
    found_2 = False
    for i in range( len( solution_table ) ):
      if ( solution_table[ i ][ j ][ 3 ] > 0. ):
        found_1 = True
      if ( solution_table[ i ][ j ][ 7 ] > 0. ):
        found_2 = True
      if ( found_1 and found_2 ):
        break
    if ( found_1 and ( not found_2 ) ):
      for i in range( len( solution_table ) ):
        [ gain_1_real, gain_1_imag, delay_1, weight_1 ] = solution_table[ i ][ j ][ 0 : 4 ]
        if ( weight_1 > 0. ):
          [ amp_1, phase_1 ] = complex_to_r_phi( complex( gain_1_real, gain_1_imag ) )
          if normalize_gains:
            amp_1 = 1.
          new_solution_table[ i ][ j ][ 0 : 4 ] = [ amp_1, 0., 0., weight_1 ]
    elif ( ( not found_1 ) and found_2 ):
      for i in range( len( solution_table ) ):
        [ gain_2_real, gain_2_imag, delay_2, weight_2 ] = solution_table[ i ][ j ][ 4 : 8 ]
        if ( weight_2 > 0. ):
          [ amp_2, phase_2 ] = complex_to_r_phi( complex( gain_2_real, gain_2_imag ) )
          if normalize_gains:
            amp_2 = 1.
          new_solution_table[ i ][ j ][ 4 : 8 ] = [ amp_2, 0., 0., weight_2 ]
  
  write_solution_table( uvim, new_solution_table, out_version = out_version,
      full_polarization = True )
  
  return

###############################################################################

def get_nearest_antennas( uv, reference_antenna ):
  na_list = []
  aip_list = get_antenna_positions( uv )
  ai_list = [ a[ 0 ] for a in aip_list ]
  ri = ai_list.index( reference_antenna )
  ap_ref = aip_list[ ri ][ 1 ]
  ap_list = [ ap_ref ] + [ ai[ 1 ] for ai in aip_list ]
  apl = len( ap_list )
  ap = array( ap_list )
  dap = ap.repeat( apl, 0 ).reshape( apl, apl, 3 ) - ap
  dap2 = add.reduce( dap**2, 2 )
  for a in range( 1, apl ):
    sel = awhere( dap2[ 0 ] < dap2[ 0, a ] )[ 1 : ]
    if ( len( sel ) == 0. ):
      na_list.append( [ ai_list[ a - 1 ], reference_antenna ] )
    else:
      dap2_min = min( aget( dap2[ a ], sel ) )
      sel2 = awhere( dap2[ a ] == dap2_min )
      if ( sel2[ 0, 0 ] == 0 ):
        na_list.append( [ ai_list[ a - 1 ], reference_antenna ] )
      else:
        na_list.append( [ ai_list[ a - 1 ], ai_list[ sel2[ 0, 0 ] - 1 ] ] )
  return na_list

###############################################################################

def re_sample_solutions( uv, time_list = None, in_version = 0, out_version = 0, 
    interpolation_method = 'spline', weight_multiplier = 1., gap_time = 15.,
    force_reference = False, full_polarization = True, add_mirror_points = False,
    loss_fraction = 0.05 ):
# gap time in minutes
# TODO: implement force_reference for other than spline
  
  if ( time_list == None ):
    time_table = get_time_list( uv )
  else:
    time_table = time_list
  solution_table = read_solution_table( uv, in_version = in_version,
      full_polarization = full_polarization )
  solution_count = len( solution_table )
  antenna_count = len( solution_table[ 0 ] ) - 1
  for sol in solution_table:
    if ( sol[ 0 ][ 1 ] > 0 ):
      reference_antenna = sol[ 0 ][ 1 ]
      break
  no_pol = len( solution_table[ 0 ][ 0 ] ) / 4
  if ( not full_polarization ):
    no_pol = 1
  if ( no_pol > 2 ):
    raise error( 'more than 2 polarizations are currently not supported' )
  sol_time_table = []
  for solution_row in solution_table:
    [ sol_time, ref_ant ] = solution_row[ 0 ][ 0 : 2 ]
    if ( ref_ant != reference_antenna ):
      # check for any valid solutions
      sel = awhere( array( solution_row )[ 1 : , 3 ] > 0. )
      if ( len( sel ) > 0 ):
        raise error( 'reference antenna changes within solution table' )
      if ( no_pol > 1 ):
        sel = awhere( array( solution_row )[ 1 : , 7 ] > 0. )
        if ( len( sel ) > 0 ):
          raise error( 'reference antenna changes within solution table' )
      ref_ant = reference_antenna
      solution_row[ 0 ][ 1 ] = reference_antenna
    sol_time_table.append( sol_time )
  sol_time_array = array( sol_time_table, dtype = float64 )
  new_solution_table = []
  if ( weight_multiplier == 0. ):
    weight_offset = 1.
  else:
    weight_offset = 0.
  
  if ( ( interpolation_method == 'nearest' ) or ( len( sol_time_array ) == 1 ) ):
    
    solution_array = array( solution_table )
    for time in time_table:
      new_solution = [ time, reference_antenna, 0., 0. ]
      if ( no_pol > 1 ):
        new_solution = new_solution + [ 0., 0., 0., 0. ]
      new_solution_row = [ new_solution ]
      for i in range( antenna_count ):
        sel = awhere( solution_array[ : , i + 1, 3 ] > 0. )
        if ( len( sel ) > 0 ):
          sub_time_array = aget( solution_array[ : , 0, 0 ], sel )
          dtime_array = abs( sub_time_array - time )
          if ( dtime_array.min() <= ( gap_time / ( 24. * 60. ) ) ):
            wm = weight_multiplier
            wo = weight_offset
          else:
            wm = 0.
            wo = 0.
          n = awhere( dtime_array == dtime_array.min() )[ 0, 0 ]
          solution_row = solution_table[ sel[ n, 0 ] ]
          solution = solution_row[ i + 1 ]
          new_solution = ( [ x for x in solution[ 0 : 3 ] ] + 
              [ wm * solution[ 3 ] + wo ] )
        else:
          new_solution = [ 0., 0., 0., 0. ]

        if ( no_pol > 1 ):
          sel = awhere( solution_array[ : , i + 1, 7 ] > 0. )
          if ( len( sel ) > 0 ):
            sub_time_array = aget( solution_array[ : , 0, 0 ], sel )
            dtime_array = abs( sub_time_array - time )
            if ( dtime_array.min() <= ( gap_time / ( 24. * 60. ) ) ):
              wm = weight_multiplier
              wo = weight_offset
            else:
              wm = 0.
              wo = 0.
            n = awhere( dtime_array == dtime_array.min() )[ 0, 0 ]
            solution_row = solution_table[ sel[ n, 0 ] ]
            solution = solution_row[ i + 1 ]
            new_solution = ( new_solution + [ x for x in solution[ 4 : 7 ] ] + 
                [ wm * solution[ 7 ] + wo ] )
          else:
            new_solution = new_solution + [ 0., 0., 0., 0. ]
        new_solution_row.append( [ x for x in new_solution ] )
      new_solution_table.append( [ x for x in new_solution_row ] )
  
  elif ( ( interpolation_method == 'linear' ) or ( len( sol_time_array ) == 1 ) ):
    
    # create storage
    for time in time_table:
      dtime_array = sol_time_array - time
      dtime_min = ( abs( dtime_array ) ).min()
      n = awhere( abs( dtime_array ) == dtime_min )[ 0, 0 ]
      solution_row = solution_table[ n ]
      [ sol_time, sol_ref_ant ] = solution_row[ 0 ][ 0 : 2 ]
      new_solution = [ time, sol_ref_ant, 0., 0. ]
      if ( no_pol > 1 ):
        new_solution = new_solution + [ 0., 0., 0., 0. ]
      new_solution_row = [ [ x for x in new_solution ] ]
      for solution in solution_row[ 1 : ]:
        new_solution = [ 0., 0., 0., 0. ]
        if ( no_pol > 1 ):
          new_solution = new_solution + [ 0., 0., 0., 0. ]
        new_solution_row.append( [ x for x in new_solution ] )
      new_solution_table.append( [ x for x in new_solution_row ] )
    
    # interpolate individual antennas
    sol_array = array( solution_table )
    for i in range( antenna_count ):
      sel = awhere( sol_array[ : , i + 1, 3 ] > 0. )
      if ( len( sel ) > 0 ):
        sub_sol_array = aget( sol_array, sel )
        sub_time_array = sub_sol_array[ : , 0, 0 ]
        for n in range( len( time_table ) ):
          time = time_table[ n ]          
          dtime_array = sub_time_array - time
          sel_neg = awhere( ( dtime_array < 0. ) & 
              ( abs( dtime_array ) < gap_time / ( 24. * 60. ) ) )
          sel_pos = awhere( ( dtime_array >= 0. ) & 
              ( abs( dtime_array ) < gap_time / ( 24. * 60. ) ) )
          if ( ( len( sel_neg ) > 0 ) or ( len( sel_pos ) > 0 ) ):
            if ( len( sel_neg ) == 0 ):
              nl = sel[ sel_pos[ 0, 0 ], 0 ]
              if ( len( sel_pos ) == 1 ):
                nu = nl
              else: # ( len( sel_pos ) > 1 )
                nu = sel[ sel_pos[ 1, 0 ], 0 ]
            elif ( len( sel_pos ) == 0 ):
              nu = sel[ sel_neg[ -1, 0 ], 0 ]
              if ( len( sel_neg ) == 1 ):
                nl = nu
              else: # ( len( sel_neg ) > 1 )
                nl = sel[ sel_neg[ -2, 0 ], 0 ]
            else: # ( ( len( sel_neg ) > 0 ) and ( len( sel_pos ) > 0 ) )
              nu = sel[ sel_pos[ 0, 0 ], 0 ]
              nl = sel[ sel_neg[ -1, 0 ], 0 ]
            u_time = sol_time_array[ nu ]
            l_time = sol_time_array[ nl ]
            u_solution = sol_array[ nu, i + 1 ]
            l_solution = sol_array[ nl, i + 1 ]
            if ( nu == nl ):
              wu = 1.
              wl = 0.
            else:
              wl = ( time - u_time ) / ( l_time - u_time )
              wu = ( time - l_time ) / ( u_time - l_time )
            [ u_gain_real, u_gain_imag, u_delay, u_weight ] = u_solution[ 0 : 4 ]
            [ l_gain_real, l_gain_imag, l_delay, l_weight ] = l_solution[ 0 : 4 ]
            u_gain = complex( u_gain_real, u_gain_imag )
            [ u_gain_amplitude, u_gain_phase ] = complex_to_r_phi( u_gain )
            l_gain = complex( l_gain_real, l_gain_imag )
            [ l_gain_amplitude, l_gain_phase ] = complex_to_r_phi( l_gain )
            new_amplitude = wl * l_gain_amplitude + wu * u_gain_amplitude
            gain_dphase = ( amodulo( 
                ( u_gain_phase - l_gain_phase ) + 180., 360. ) - 180. )
            new_phase = ( amodulo( ( wl * l_gain_phase + wu * 
                ( l_gain_phase + gain_dphase ) ) + 180., 360. ) - 180. )
            new_delay = wl * l_delay + wu * u_delay
            new_weight = 1. / sqrt( ( wl / l_weight )**2 + ( wu / u_weight )**2 )
            new_weight = weight_multiplier * new_weight + weight_offset
            new_gain = r_phi_to_complex( [ new_amplitude, new_phase ] )
            new_solution_table[ n ][ i + 1 ][ 0 : 4 ] = [ 
               new_gain.real, new_gain.imag, new_delay, new_weight ]
      if ( no_pol > 1 ):
        sel = awhere( sol_array[ : , i + 1, 7 ] > 0. )
        if ( len( sel ) > 0 ):
          sub_sol_array = aget( sol_array, sel )
          sub_time_array = sub_sol_array[ : , 0, 0 ]
          for n in range( len( time_table ) ):
            time = time_table[ n ]          
            dtime_array = sub_time_array - time
            sel_neg = awhere( ( dtime_array < 0. ) & 
                ( abs( dtime_array ) < gap_time / ( 24. * 60. ) ) )
            sel_pos = awhere( ( dtime_array >= 0. ) & 
                ( abs( dtime_array ) < gap_time / ( 24. * 60. ) ) )
            if ( ( len( sel_neg ) > 0 ) or ( len( sel_pos ) > 0 ) ):
              if ( len( sel_neg ) == 0 ):
                nl = sel[ sel_pos[ 0, 0 ], 0 ]
                if ( len( sel_pos ) == 1 ):
                  nu = nl
                else: # ( len( sel_pos ) > 1 )
                  nu = sel[ sel_pos[ 1, 0 ], 0 ]
              elif ( len( sel_pos ) == 0 ):
                nu = sel[ sel_neg[ -1, 0 ], 0 ]
                if ( len( sel_neg ) == 1 ):
                  nl = nu
                else: # ( len( sel_neg ) > 1 )
                  nl = sel[ sel_neg[ -2, 0 ], 0 ]
              else: # ( ( len( sel_neg ) > 0 ) and ( len( sel_pos ) > 0 ) )
                nu = sel[ sel_pos[ 0, 0 ], 0 ]
                nl = sel[ sel_neg[ -1, 0 ], 0 ]
              u_time = sol_time_array[ nu ]
              l_time = sol_time_array[ nl ]
              u_solution = sol_array[ nu, i + 1 ]
              l_solution = sol_array[ nl, i + 1 ]
              if ( nu == nl ):
                wu = 1.
                wl = 0.
              else:
                wl = ( time - u_time ) / ( l_time - u_time )
                wu = ( time - l_time ) / ( u_time - l_time )
              [ u_gain_real, u_gain_imag, u_delay, u_weight ] = u_solution[ 4 : 8 ]
              [ l_gain_real, l_gain_imag, l_delay, l_weight ] = l_solution[ 4 : 8 ]
              u_gain = complex( u_gain_real, u_gain_imag )
              [ u_gain_amplitude, u_gain_phase ] = complex_to_r_phi( u_gain )
              l_gain = complex( l_gain_real, l_gain_imag )
              [ l_gain_amplitude, l_gain_phase ] = complex_to_r_phi( l_gain )
              new_amplitude = wl * l_gain_amplitude + wu * u_gain_amplitude
              gain_dphase = ( amodulo( 
                  ( u_gain_phase - l_gain_phase ) + 180., 360. ) - 180. )
              new_phase = ( amodulo( ( wl * l_gain_phase + wu * 
                  ( l_gain_phase + gain_dphase ) ) + 180., 360. ) - 180. )
              new_delay = wl * l_delay + wu * u_delay
              new_weight = 1. / sqrt( ( wl / l_weight )**2 + ( wu / u_weight )**2 )
              new_weight = weight_multiplier * new_weight + weight_offset
              new_gain = r_phi_to_complex( [ new_amplitude, new_phase ] )
              new_solution_table[ n ][ i + 1 ][ 4 : 8 ] = [ 
                 new_gain.real, new_gain.imag, new_delay, new_weight ]
  
  elif ( interpolation_method == 'spline' ):
    # after suggestions by SvdT
    # this is by far the most fancy interpolation
    
    # create storage
    # use nearest weight (meaning that flagged times are kept)
    for time in time_table:
      dtime_array = sol_time_array - time
      dtime_min = ( abs( dtime_array ) ).min()
      n = awhere( abs( dtime_array ) == dtime_min )[ 0, 0 ]
      solution_row = solution_table[ n ]
      [ sol_time, sol_ref_ant ] = solution_row[ 0 ][ 0 : 2 ]
      new_solution = [ time, sol_ref_ant, 0., 0. ]
      if ( no_pol > 1 ):
        new_solution = new_solution + [ 0., 0., 0., 0. ]
      new_solution_row = [ [ x for x in new_solution ] ]
      for solution in solution_row[ 1 : ]:
        if ( dtime_min > ( gap_time / ( 60. * 24. ) ) ):
          solution[ 3 ] = 0.
          if ( no_pol > 1 ):
            solution[ 7 ] = 0.
        new_solution = [ 0., 0., 0.,
            weight_multiplier * solution[ 3 ] + weight_offset ]
        if ( no_pol > 1 ):
          new_solution = new_solution + [ 0., 0., 0., 
              weight_multiplier * solution[ 7 ] + weight_offset ]
        new_solution_row.append( [ x for x in new_solution ] )
      new_solution_table.append( [ x for x in new_solution_row ] )
    
    # interpolate individual antennas
    ant_ref_list = get_nearest_antennas( uv, reference_antenna )
    if ( no_pol > 1 ):
      ant_ref_list2 = [ [ y for y in x ] for x in ant_ref_list ]
    time_array = array( time_table, dtype = float64 )
    for k in range( antenna_count ):
      
      # count solutions
      sol_count = 0
      for n in range( solution_count ):
        solution = solution_table[ n ][ k + 1 ]
        if ( solution[ 3 ] != 0. ):
          sol_count = sol_count + 1
      if ( sol_count > 0 ):
        
        # find suitable nearby reference antenna
        ant_indx = [ a[ 0 ] for a in ant_ref_list ].index( k + 1 )
        ref_ant = ant_ref_list[ ant_indx ][ 1 ]
        while ( ref_ant != reference_antenna ):
          ref_count = 0
          for n in range( solution_count ):
            solution = solution_table[ n ][ k + 1 ]
            if ( solution[ 3 ] != 0. ):
              ref_solution = solution_table[ n ][ ref_ant ]
              if ( ref_solution[ 3 ] != 0. ):
                ref_count = ref_count + 1
          loss_count = sol_count - ref_count
          if ( float( loss_count ) <= loss_fraction * float( sol_count ) ):
            break
          ref_ant_indx = [ a[ 0 ] for a in ant_ref_list ].index( ref_ant )
          ref_ant = ant_ref_list[ ref_ant_indx ][ 1 ]
          ant_ref_list[ ant_indx ][ 1 ] = ref_ant
      
        # get non-flagged re-referenced solutions
        ant_sol_table = []
        for n in range( solution_count ):
          solution = solution_table[ n ][ k + 1 ]
          ref_solution = solution_table[ n ][ ref_ant ]
          if ( ( solution[ 3 ] != 0. ) and ( ref_solution[ 3 ] != 0. ) ):
            ant_sol = ( [ sol_time_table[ n ] ] + 
                complex_to_r_phi( complex( solution[ 0 ], solution[ 1 ] ) ) + 
                [ solution[ 2 ] ] )
            ant_sol[ 2 ] = amodulo( ( ant_sol[ 2 ] - complex_to_r_phi( 
                complex( ref_solution[ 0 ], ref_solution[ 1 ] ) )[ 1 ] ) +
                    180., 360 ) - 180.
            ant_sol_table.append( ant_sol )
        
        # add mirror points around gaps
        if add_mirror_points:
          add_ant_sol_table = []
          add_ant_sol_table.append( [ 2. * ant_sol_table[ - 1 ][ 0 ] - 
              ant_sol_table[ - 2 ][ 0 ] ] + ant_sol_table[ - 1 ][ 1 : 4 ] )
          add_ant_sol_table.append( [ 2. * ant_sol_table[ 0 ][ 0 ] -
              ant_sol_table[ 1 ][ 0 ] ] + ant_sol_table[ 0 ][ 1 : 4 ] )
          for n in range( 2, len( ant_sol_table ) - 1 ):
            if ( ( ant_sol_table[ n ][ 0 ] - ant_sol_table[ n - 1 ][ 0 ] ) > 
                ( gap_time / ( 60. * 24. ) ) ):
              add_ant_sol_table.append( [ 2. * ant_sol_table[ n - 1 ][ 0 ] -
                  ant_sol_table[ n - 2 ][ 0 ] ] + ant_sol_table[ n - 1 ][ 1 : 4 ] )
              add_ant_sol_table.append( [ 2. * ant_sol_table[ n ][ 0 ] -
                  ant_sol_table[ n + 1 ][ 0 ] ] + ant_sol_table[ n ][ 1 : 4 ] )
          ant_sol_table = ant_sol_table + add_ant_sol_table
        
        # convert to array
        ant_sol_table.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
        ant_sol_array = array( ant_sol_table, dtype = float64 )
        
        # unwrap the phase (phase jumps greater than 180 are made smaller by adding n*360)
        if ( len( ant_sol_array ) < 20 ):
          phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
        else:
          phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.01 )
          if sometrue( isnan( phase_array ) ):
            phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.001 )
          if ( sometrue( isnan( phase_array ) ) or ( max( fabs( phase_array ) ) > 1.e4 ) ):
            phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
        
        # use splines to interpolate over time
        tck_amplitude = interpolate.splrep( ant_sol_array[ : , 0 ], ant_sol_array[ : , 1 ] )
        amplitude_interpolated = interpolate.splev( time_array, tck_amplitude )
        tck_phase = interpolate.splrep( ant_sol_array[ : , 0 ], phase_array )
        phase_interpolated = interpolate.splev( time_array, tck_phase )
        tck_delay = interpolate.splrep( ant_sol_array[ : , 0 ], ant_sol_array[ : , 3 ] )
        delay_interpolated = interpolate.splev( time_array, tck_delay )
        
        # store results
        for n in range( len( time_table ) ):
          gain = r_phi_to_complex( [ float( amplitude_interpolated[ n ] ), 
              float( phase_interpolated[ n ] ) ] )
          new_solution_table[ n ][ k + 1 ][ 0 : 3 ] = [ gain.real, gain.imag,
              float( delay_interpolated[ n ] ) ]
        
      if ( no_pol > 1 ):
        
        # count solutions
        sol_count = 0
        for n in range( solution_count ):
          solution = solution_table[ n ][ k + 1 ]
          if ( solution[ 7 ] != 0. ):
            sol_count = sol_count + 1
        if ( sol_count == 0 ):
          continue
        
        # find suitable nearby reference antenna
        ant_indx = [ a[ 0 ] for a in ant_ref_list2 ].index( k + 1 )
        ref_ant = ant_ref_list2[ ant_indx ][ 1 ]
        while ( ref_ant != reference_antenna ):
          ref_count = 0
          for n in range( solution_count ):
            solution = solution_table[ n ][ k + 1 ]
            if ( solution[ 7 ] != 0. ):
              ref_solution = solution_table[ n ][ ref_ant ]
              if ( ref_solution[ 7 ] != 0. ):
                ref_count = ref_count + 1
          loss_count = sol_count - ref_count
          if ( float( loss_count ) < loss_fraction * float( sol_count ) ):
            break
          ref_ant_indx = [ a[ 0 ] for a in ant_ref_list2 ].index( ref_ant )
          ref_ant = ant_ref_list2[ ref_ant_indx ][ 1 ]
          ant_ref_list2[ ant_indx ][ 1 ] = ref_ant
        
        # get non-flagged re-referenced solutions
        ant_sol_table = []
        for n in range( solution_count ):
          solution = solution_table[ n ][ k + 1 ]
          ref_solution = solution_table[ n ][ ref_ant ]
          if ( ( solution[ 7 ] != 0. ) and ( ref_solution[ 7 ] != 0. ) ):
            ant_sol = ( [ sol_time_table[ n ] ] + 
                complex_to_r_phi( complex( solution[ 4 ], solution[ 5 ] ) ) + 
                [ solution[ 6 ] ] )
            ant_sol[ 2 ] = amodulo( ( ant_sol[ 2 ] - complex_to_r_phi( 
                complex( ref_solution[ 4 ], ref_solution[ 5 ] ) )[ 1 ] ) +
                    180., 360 ) - 180.
            ant_sol_table.append( ant_sol )
        
        # add mirror points around gaps
        if add_mirror_points:
          add_ant_sol_table = []
          add_ant_sol_table.append( [ 2. * ant_sol_table[ - 1 ][ 0 ] -
              ant_sol_table[ - 2 ][ 0 ] ] + ant_sol_table[ - 1 ][ 1 : 4 ] )
          add_ant_sol_table.append( [ 2. * ant_sol_table[ 0 ][ 0 ] -
              ant_sol_table[ 1 ][ 0 ] ] + ant_sol_table[ 0 ][ 1 : 4 ] )
          for n in range( 2, len( ant_sol_table ) - 1 ):
            if ( ( ant_sol_table[ n ][ 0 ] - ant_sol_table[ n - 1 ][ 0 ] ) > 
                ( gap_time / ( 60. * 24. ) ) ):
              add_ant_sol_table.append( [ 2. * ant_sol_table[ n - 1 ][ 0 ] - 
                  ant_sol_table[ n - 2 ][ 0 ] ] + ant_sol_table[ n - 1 ][ 1 : 4 ] )
              add_ant_sol_table.append( [ 2. * ant_sol_table[ n ][ 0 ] -
                  ant_sol_table[ n + 1 ][ 0 ] ] + ant_sol_table[ n ][ 1 : 4 ] )
          ant_sol_table = ant_sol_table + add_ant_sol_table
        
        # convert to array
        ant_sol_table.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
        ant_sol_array = array( ant_sol_table, dtype = float64 )
        
        # unwrap the phase (phase jumps greater than 180 are made smaller by adding n*360)
        if ( len( ant_sol_array ) < 20 ):
          phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
        else:
          phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.1 )
          if sometrue( isnan( phase_array ) ):
            phase_array = aunwrap_phase( ant_sol_array[ : , 2 ], alpha = 0.01 )
          if ( sometrue( isnan( phase_array ) ) or ( max( fabs( phase_array ) ) > 1.e4 ) ):
            phase_array = adegrees( unwrap( aradians( ant_sol_array[ : , 2 ] ) ) )
        
        # use splines to interpolate over time
        tck_amplitude = interpolate.splrep( ant_sol_array[ : , 0 ], ant_sol_array[ : , 1 ] )
        amplitude_interpolated = interpolate.splev( time_array, tck_amplitude )
        tck_phase = interpolate.splrep( ant_sol_array[ : , 0 ], phase_array )
        phase_interpolated = interpolate.splev( time_array, tck_phase )
        tck_delay = interpolate.splrep( ant_sol_array[ : , 0 ], ant_sol_array[ : , 3 ] )
        delay_interpolated = interpolate.splev( time_array, tck_delay )
        
        # store results
        for n in range( len( time_table ) ):
          gain = r_phi_to_complex( [ float( amplitude_interpolated[ n ] ), 
              float( phase_interpolated[ n ] ) ] )
          new_solution_table[ n ][ k + 1 ][ 4 : 7 ] = [ gain.real, gain.imag,
              float( delay_interpolated[ n ] ) ]
    
    # re-reference phases back to common reference antenna
    for solution_row in new_solution_table:
      amp_phase_list = [ complex_to_r_phi( complex( s[ 0 ], s[ 1 ] ) )
          for s in solution_row[ 1 : ] ]
      ra_list = [ [ x for x in y ] for y in ant_ref_list ]
      done = False
      while ( not done ):
        done = True
        for ra in ra_list:
          if ( ra[ 1 ] != reference_antenna ):
            if ( solution_row[ ra[ 1 ] ][ 3 ] > 0. ):
              p1 = amp_phase_list[ ra[ 0 ] - 1 ][ 1 ]
              p2 = amp_phase_list[ ra[ 1 ] - 1 ][ 1 ]
              amp_phase_list[ ra[ 0 ] - 1 ][ 1 ] = (
                  amodulo( ( p1 + p2 ) + 180., 360. ) - 180. )
            else:
              solution_row[ ra[ 0 ] ][ 3 ] = 0.
            ref_ant_indx = [ a[ 0 ] for a in ra_list ].index( ra[ 1 ] )
            ref_ant = ra_list[ ref_ant_indx ][ 1 ]
            ra[ 1 ] = ref_ant
            if ( ref_ant != reference_antenna ):
              done = False
      for i in range( len( amp_phase_list ) ):
        c = r_phi_to_complex( amp_phase_list[ i ] )
        solution_row[ i + 1 ][ 0 ] = c.real
        solution_row[ i + 1 ][ 1 ] = c.imag
      
      if ( no_pol > 1 ):
        amp_phase_list = [ complex_to_r_phi( complex( s[ 4 ], s[ 5 ] ) )
            for s in solution_row[ 1 : ] ]
        ra_list = [ [ x for x in y ] for y in ant_ref_list2 ]
        done = False
        while ( not done ):
          done = True
          for ra in ra_list:
            if ( ra[ 1 ] != reference_antenna ):
              if ( solution_row[ ra[ 1 ] ][ 7 ] > 0. ):
                p1 = amp_phase_list[ ra[ 0 ] - 1 ][ 1 ]
                p2 = amp_phase_list[ ra[ 1 ] - 1 ][ 1 ]
                amp_phase_list[ ra[ 0 ] - 1 ][ 1 ] = (
                    amodulo( ( p1 + p2 ) + 180., 360. ) - 180. )
              else:
                solution_row[ ra[ 0 ] ][ 7 ] = 0.
              ref_ant_indx = [ a[ 0 ] for a in ra_list ].index( ra[ 1 ] )
              ref_ant = ra_list[ ref_ant_indx ][ 1 ]
              ra[ 1 ] = ref_ant
              if ( ref_ant != reference_antenna ):
                done = False
        for i in range( len( amp_phase_list ) ):
          c = r_phi_to_complex( amp_phase_list[ i ] )
          solution_row[ i + 1 ][ 4 ] = c.real
          solution_row[ i + 1 ][ 5 ] = c.imag
  
  else:
    raise error( 'unknown interpolation method: %s' % ( interpolation ) )
  
  write_solution_table( uv, new_solution_table, out_version = out_version,
      full_polarization = full_polarization )
  
  return

###############################################################################
