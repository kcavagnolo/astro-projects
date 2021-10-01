###############################################################################

# import Python modules
from sys import *
from os import *
from datetime import *
from math import *

# import 3rd party modules
#import numarray as na
from numpy import *

# import user modules
from files import *
from aips import *
from sphere import *
from parameter import *
from skymodel import *
from image import *
from solutions import *
from error import *

###############################################################################

def calibrate_uv_old( uv, pbm_facets, facet_list = [], scale_weights = False, sigma = 0.,
    signal_to_noise = 10., reference_antenna = 0, conversion_method = 'DFT',
    do_amplitude = False, amplitude_interval = 5., phase_interval_min = 0.,
    apply_solutions = False, calib_params = {}, interpolation_method = '',
    frequency_correction = False ):
  
  # check input UV format
  if ( uv.header.ctype[ 0 : 4 ] != ['COMPLEX', 'STOKES', 'FREQ', 'IF' ] ):
    raise error( 'input UV visibility axes are not in IF-FREQ-STOKES order' )
  if ( uv.header.naxis[ 0 ] != 3 ):
    raise error( 'input UV visibilities are compressed' )
  
  # generate model UV data set
  model_uv = add_model( uv, pbm_facets, facet_list = facet_list, replace = True,
      sigma = sigma, frequency_correction = frequency_correction,
      apply_solutions = apply_solutions, conversion_method = conversion_method, )
  
  # make copy to calibrate
  cal_uv = get_aips_file( uv.disk, uv.name, 'CALUV', -1, 'UV' )
  call_aips_task( 'MOVE', indata = model_uv, outdata = cal_uv, userid = get_aips_userid() )
  
  # get handles to UV data
  wiz_uv = wizardry( uv )
  wiz_model_uv = wizardry( model_uv )
  wiz_cal_uv = wizardry( cal_uv )
  for group in wiz_uv:
    break
  for model_group in wiz_model_uv:
    break
  
  # loop through cal UV data
  t = 0.
  for cal_group in wiz_cal_uv:
    if ( ( model_group.time != cal_group.time ) or ( model_group.baseline != cal_group.baseline ) ):
      raise error( 'model and output UV do not have the same layout' )
    while ( ( group.time != cal_group.time ) or ( group.baseline != cal_group.baseline ) ):
      dummy = group.next()
    if ( model_group.time != t ):
      t = model_group.time
      print repr( time_to_dhms( t ) )
    
    # divide measured visibilities by model visibilities
    # write to cal UV data
    vis = group.visibility
    model_vis = model_group.visibility
    cal_vis = cal_group.visibility
    for i in range( uv.header.naxis[ 3 ] ): # IF
      for f in range( uv.header.naxis[ 2 ] ): # channel
        for s in range( uv.header.naxis[ 1 ] ): # stokes
          if ( ( vis[ i, f, s, 2 ] == 0. ) or ( model_vis[ i, f, s, 2 ] == 0. ) ):
            cal_vis[ i, f, s, 2 ] = 0.
          else:
            v = complex( vis[ i, f, s, 0 ], vis[ i, f, s, 1 ] )
            mv = complex( model_vis[ i, f, s, 0 ], model_vis[ i, f, s, 1 ] )
            cv = v / mv
            cal_vis[ i, f, s, 0 ] = cv.real
            cal_vis[ i, f, s, 1 ] = cv.imag
            if scale_weights:
              cal_vis[ i, f, s, 2 ] = abs( mv ) * cal_vis[ i, f, s, 2 ]
#    cal_group.visibility = na.array( cal_vis, type = na.Float32 )
    cal_group.update()
    dummy = group.next()
    dummy = model_group.next()
  
  # determine total flux in model
  pbm_facet_count = restore_parameter( pbm_facets, 'facet_count' )
  total_model_flux = 0.
  for i in range( 1, pbm_facet_count + 1 ):
    pbm_facet_i = get_facet( pbm_facets, i )
    total_model_flux = total_model_flux + get_model_flux( pbm_facet_i )
  
  # calculate noise per integration interval
#  time_count = restore_parameter( uv, 'time_count' )
  time_count = len( get_time_list( uv ) )
  try:
    cpb_noise = restore_parameter( uv, 'cpb_noise' )
  except:
    if ( sigma > 0. ):
      raise error( 'cpb_noise is not yet defined' )
    cpb_noise = 0.
  noise_per_interval = cpb_noise * sqrt( float( time_count ) )
  
  # calculate solution interval
  integration_time = restore_parameter( uv, 'integration_time' ) / 60.
  sn_per_interval = total_model_flux / noise_per_interval
  interval_count = ceil( signal_to_noise / sn_per_interval )
  solution_interval = integration_time * interval_count
  
  # calibrate UV data against 1 Jy point source model
  if do_amplitude:
    solmode = 'A&P'
    solution_interval = amplitude_interval
  else:
    solmode = 'P'
    solution_interval = max( [ solution_interval, phase_interval_min ] )
  dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', - 1, 'UV' )
  channel_count = get_channel_count( uv )
  call_aips_task( 'CALIB', indata = cal_uv, smodel = [ 1, 0, 0, 0, 0, 0, 0 ],
      flux = sigma * cpb_noise, nmaps = pbm_facet_count, cmodel = 'COMP',
      outdata = dummy_uv, cmethod = conversion_method, snver = 0,
      refant = reference_antenna, solint = solution_interval, solsub = 1,
      solmin = 1, weightit = 1, soltype = 'L1R', solmode = solmode,
      aparm = [ 4, 0, 0, 0, 0, 0, 1, 0, 0 ], cparm = [ 0, 1, 0, 0, 0, 0 ],
      docalib = - 1, gainuse = - 1, ichansel = [ [ 1, channel_count, 1, 1 ] ],
      **calib_params )
  dummy_uv.zap()
  
  # TODO: flagging based on solutions
  
  # copy solutions o original UV data
  call_aips_task( 'TACOP', indata = cal_uv, inext = 'SN', invers = 0, ncount = 1,
      outdata = uv, outvers = 0 )
  cal_uv.zap()
  
  if ( interpolation_method == '' ):
    if do_amplitude:
      im = 'linear'
    else:
      im = 'spline'
  else:
    im = interpolation_method
  
  # resample solutions to UV database time grid
  in_version = uv.table_highver( 'SN' )
  out_version = in_version + 1
  try:
    re_sample_solutions( uv, in_version = in_version, out_version = out_version,
        weight_multiplier = 1. / sqrt( float( interval_count ) ), interpolation_method = im )
  except:
    re_sample_solutions( uv, in_version = in_version, out_version = out_version,
        weight_multiplier = 1. / sqrt( float( interval_count ) ), interpolation_method = 'linear' )
  else:
    pass
  uv.zap_table( 'SN', in_version )
  
  # store parameters
  store_parameter( uv, 'solution_interval', solution_interval )
  
  return

###############################################################################

def calibrate_uv_old2( uv, pb_facets, facet_list = [], scale_weights = False, sigma = 0.,
    signal_to_noise = 10., reference_antenna = 0, conversion_method = 'DFT',
    do_amplitude = False, amplitude_interval = 5., phase_interval_min = 0.,
    apply_solutions = False, calib_params = {}, interpolation_method = '',
    frequency_correction = False, div_uv = None, keep_div_uv = False ):
  
  # check input UV format
  if ( uv.header.ctype[ 0 : 4 ] != ['COMPLEX', 'STOKES', 'FREQ', 'IF' ] ):
    raise error( 'input UV visibility axes are not in IF-FREQ-STOKES order' )
  if ( uv.header.naxis[ 0 ] != 3 ):
    raise error( 'input UV visibilities are compressed' )
  
  # generate model UV data set
  if ( div_uv != None ):
    cal_uv = div_uv
  else:
    mdl_uv = add_model( uv, pb_facets, facet_list = facet_list, replace = True,
        sigma = sigma, frequency_correction = frequency_correction, keep_solutions = False,
        apply_solutions = apply_solutions, conversion_method = conversion_method, )
    while table_exists( mdl_uv, 'SN', 0 ):
      mdl_uv.zap_table( 'SN', 0 )
    while table_exists( mdl_uv, 'FG', 0 ):
      mdl_uv.zap_table( 'FG', 0 )
    while table_exists( mdl_uv, 'PS', 0 ):
      mdl_uv.zap_table( 'PS', 0 )
    while table_exists( mdl_uv, 'NI', 0 ):
      mdl_uv.zap_table( 'NI', 0 )
    while table_exists( mdl_uv, 'FG', 0 ):
      mdl_uv.zap_table( 'OB', 0 )
    while table_exists( mdl_uv, 'HI', 0 ):
      mdl_uv.zap_table( 'HI', 0 )
    
    # divide measured UV by model UV
    # process UV data per time interval :-(
    tl = get_time_list( mdl_uv )
    dt = restore_parameter( uv, 'integration_time' ) / ( 60. * 60. * 24. )
    bare_uv = get_aips_file( uv.disk, uv.name, 'BARE', - 1, 'UV' )
    call_aips_task( 'MOVE', indata = uv, outdata = bare_uv, userid = get_aips_userid() )
    while table_exists( bare_uv, 'SN', 0 ):
      bare_uv.zap_table( 'SN', 0 )
    while table_exists( bare_uv, 'FG', 0 ):
      bare_uv.zap_table( 'FG', 0 )
    while table_exists( bare_uv, 'PS', 0 ):
      bare_uv.zap_table( 'PS', 0 )
    while table_exists( bare_uv, 'NI', 0 ):
      bare_uv.zap_table( 'NI', 0 )
    while table_exists( bare_uv, 'FG', 0 ):
      bare_uv.zap_table( 'OB', 0 )
    while table_exists( bare_uv, 'HI', 0 ):
      bare_uv.zap_table( 'HI', 0 )
    dbare_uv = get_aips_file( uv.disk, uv.name, 'DBARE', - 1, 'UV' )
    cal_uv = get_aips_file( uv.disk, uv.name, 'CAL', - 1, 'UV' )
    timerang = time_to_dhms( tl[ 0 ] - 0.4 * dt ) + time_to_dhms( tl[ 0 ] + 0.4 * dt )
    call_aips_task( 'UVCOP', indata = bare_uv, outdata = dbare_uv, flagver = -1,
        timerang = timerang )
    call_aips_task( 'UVMTH', in2data = dbare_uv, indata = mdl_uv, outdata = cal_uv,
        docalib = -1, flagver = -1, opcode = 'DIV', timerang = timerang )
    dbare_uv.zap()
    dcal_uv = get_aips_file( uv.disk, uv.name, 'DCAL', - 1, 'UV' )
    for t in tl[ 1 : ]:
      timerang = time_to_dhms( t - 0.4 * dt ) + time_to_dhms( t + 0.4 * dt )
      call_aips_task( 'UVCOP', indata = bare_uv, outdata = dbare_uv, flagver = -1,
          timerang = timerang )
      call_aips_task( 'UVMTH', in2data = dbare_uv, indata = mdl_uv, outdata = dcal_uv,
          docalib = -1, flagver = -1, opcode = 'DIV', timerang = timerang )
      dbare_uv.zap()
      dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', - 1, 'UV' )
      call_aips_task( 'DBCON', indata = cal_uv, in2data = dcal_uv, outdata = dummy_uv, doarray = 1 )
      cal_uv.zap()
      dcal_uv.zap()
      dummy_uv.rename( name = cal_uv.name, klass = cal_uv.klass, seq = cal_uv.seq )
    bare_uv.zap()
    mdl_uv.zap()
  
  # determine total flux in model
  pbm_facet_count = restore_parameter( pb_facets, 'facet_count' )
  total_model_flux = 0.
  for i in range( 1, pbm_facet_count + 1 ):
    pbm_facet_i = get_facet( pb_facets, i )
    total_model_flux = total_model_flux + get_model_flux( pbm_facet_i )
  
  # calculate noise per integration interval
#  time_count = restore_parameter( uv, 'time_count' )
  time_count = len( get_time_list( uv ) )
  try:
    cpb_noise = restore_parameter( uv, 'cpb_noise' )
  except:
    if ( sigma > 0. ):
      raise error( 'cpb_noise is not yet defined' )
    cpb_noise = 0.
  noise_per_interval = cpb_noise * sqrt( float( time_count ) )
  
  # calculate solution interval
  integration_time = restore_parameter( uv, 'integration_time' ) / 60.
  sn_per_interval = total_model_flux / noise_per_interval
  interval_count = ceil( signal_to_noise / sn_per_interval )
  solution_interval = integration_time * interval_count
  
  # calibrate UV data against 1 Jy point source model
  if do_amplitude:
    solmode = 'A&P'
    solution_interval = amplitude_interval
  else:
    solmode = 'P'
    solution_interval = max( [ solution_interval, phase_interval_min ] )
  dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', - 1, 'UV' )
  channel_count = get_channel_count( uv )
  call_aips_task( 'CALIB', indata = cal_uv, smodel = [ 1, 0, 0, 0, 0, 0, 0 ],
      flux = sigma * cpb_noise, nmaps = pbm_facet_count, cmodel = 'COMP',
      outdata = dummy_uv, cmethod = conversion_method, snver = 0,
      refant = reference_antenna, solint = solution_interval, solsub = 1,
      solmin = 1, weightit = 0, soltype = 'L1R', solmode = solmode,
      aparm = [ 4, 0, 0, 0, 0, 0, 1, 0, 0 ], cparm = [ 0, 1, 0, 0, 0, 0 ],
      docalib = - 1, gainuse = - 1, ichansel = [ [ 1, channel_count, 1, 1 ] ], 
      **calib_params )
  dummy_uv.zap()
  
  # TODO: flagging based on solutions
  
  # copy solutions to original UV data
  call_aips_task( 'TACOP', indata = cal_uv, inext = 'SN', invers = 0, ncount = 1,
      outdata = uv, outvers = 0 )
  if ( ( not keep_div_uv ) and ( cal_uv == None ) ):
    cal_uv.zap()
    cal_uv = None
  
  if ( interpolation_method == '' ):
    if do_amplitude:
      im = 'linear'
    else:
      im = 'spline'
  else:
    im = interpolation_method
  
  # resample solutions to UV database time grid
  in_version = uv.table_highver( 'SN' )
  out_version = in_version + 1
  try:
    re_sample_solutions( uv, in_version = in_version, out_version = out_version,
        weight_multiplier = 1. / sqrt( float( interval_count ) ), interpolation_method = im )
  except:
    re_sample_solutions( uv, in_version = in_version, out_version = out_version,
        weight_multiplier = 1. / sqrt( float( interval_count ) ), interpolation_method = 'linear' )
  else:
    pass
  uv.zap_table( 'SN', in_version )
  
  # store parameters
  store_parameter( uv, 'solution_interval', solution_interval )
  
  return cal_uv

###############################################################################

def divide_uv( uv, model_uv, apply_flags = True, flag_version = 0 ):
# flag table is expected to be attached to uv

  # apply flags
  if ( apply_flags and table_exists( uv, 'FG', flag_version ) ):
    flag_uv = apply_flag_table( uv )
  else:
    flag_uv = uv

  # divide UV by model UV
  temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )
  div_uv = get_aips_file( uv.disk, uv.name, 'DIV', -1, 'UV' )
  call_aips_task( 'DIFUV', indata = flag_uv, in2data = model_uv,
      outdata = temp_uv, optype = 'DIV', solint = 0 )

  # TODO: figure out the weight modification scheme
#  call_aips_task( 'WTMOD', indata = temp_uv, outdata = div_uv,
#      aparm = [ 2., 0., 0. ] )
#  temp_uv.zap()
  temp_uv.rename( name = div_uv.name, klass = div_uv.klass, seq = div_uv.seq )

  # cleanup
  if ( flag_uv != uv ):
    flag_uv.zap()
  
  return div_uv

###############################################################################

def calibrate_model( uv, facets, reference_antenna = 0, phase_interval = 0.,
    do_amplitude = False, amplitude_interval = 5., interpolation_method = '',
    apply_solutions = True, keep_solutions = True, solution_version = 0,
    flag_solutions = True, fast_flag = False, keep_flags = True, sigma = 0., 
    model_version = 0, facet_list = [], conversion_method = 'DFT',
    calib_params = {}, frequency_correction = False, snr_limit = 2. ):
  
  # work copies
  flag_solns = flag_solutions
  flag_uv = uv
  
  # determine solution mode
  if flag_solns:
    if apply_solutions:
      if ( len( facet_list ) > 0 ):
        facet = get_facet( facets, facet_list[ 0 ] )
      else:
        facet = facets
      if table_exists( facet, 'SN', solution_version ):
        sol_mode = 2
      elif table_exists( uv, 'SN', solution_version ):
        sol_mode = 1
      else:
        sol_mode = 0
        flag_solns = False
        
  # determine solution flagging mode
  if ( flag_solns and fast_flag and ( sol_mode == 2 ) ):
    # get selected facet list with non-empty models
    if ( len( facet_list ) > 0 ):
      temp_facet_count = len( facet_list )
      temp_facet_list = [ i for i in facet_list ]
    else:
      try:
        temp_facet_count = restore_parameter( facets, 'facet_count' )
      except:
        temp_facet_count = 1
      temp_facet_list = range( 1, temp_facet_count + 1 )
    for i in range( 1, 1 + max( temp_facet_list ) ):
      if ( i in temp_facet_list ):
        facet = get_facet( facets, i )
        if model_table_empty( facet, model_version ):
          temp_facet_list.remove( i )
    temp_facet_count = len( temp_facet_list )
    if ( temp_facet_count > 1 ):
      # try to do fast flagging
      try:
        facet_count = restore_parameter( facets, 'facet_count' )
        added_facet_count = restore_parameter( facets, 'added_facet_count' )
      except:
        pass
      else:
        flag_solns = False
        start_j = temp_facet_count - 1
        if ( added_facet_count > 0 ):
          orig_facet_count = facet_count - added_facet_count
          for j in range( temp_facet_count ):
            if ( temp_facet_list[ j ] > orig_facet_count ):
              start_j = max( 0, j - 1 )
              break
        for j in range( start_j, temp_facet_count ):
          if ( j == start_j ):
            flag_version = -1
          else:
            flag_version = 0
          if ( j == temp_facet_count - 1 ):
            apply_flags = True
          else:
            apply_flags = False
          facet = get_facet( facets, temp_facet_list[ j ] )
          flag_uv = flag_bad_solutions( uv, uvim = facet,
              apply_flags = apply_flags, flag_version = flag_version,
              solution_version = solution_version, keep_flags = keep_flags )
  
  # create model uv
  model_uv = make_model_uv( flag_uv, facets, facet_list = facet_list,
    apply_solutions = apply_solutions, keep_solutions = keep_solutions,
    conversion_method = conversion_method, model_version = model_version,
    solution_version = solution_version, flag_solutions = flag_solns,
    frequency_correction = frequency_correction, keep_flags = keep_flags,
    flux_scale = 1., sigma = sigma )
  
  # divide real UV by model UV
  if ( model_uv == None ):
    sub_uv = None
    out_version = -1
  else:
    div_uv = divide_uv( flag_uv, model_uv, apply_flags = True )
    model_uv.zap()
    
    # calibrate UV data against 1 Jy point source model
    if do_amplitude:
      solmode = 'A&P'
      solution_interval = amplitude_interval
    else:
      solmode = 'P'
      solution_interval = phase_interval
    dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', - 1, 'UV' )
    channel_count = get_channel_count( uv )
    call_aips_task( 'CALIB', indata = div_uv, smodel = [ 1, 0, 0, 0, 0, 0, 0 ],
        outdata = dummy_uv, cmethod = conversion_method, snver = 0,
        refant = reference_antenna, solint = solution_interval, solsub = 1,
        solmin = 1, weightit = 0, soltype = 'L1R', solmode = solmode,
        aparm = [ 4, 0, 0, 0, 0, 0, snr_limit, 0, 0 ], cparm = [ 0, 1, 0, 0, 0, 0 ],
        docalib = - 1, gainuse = - 1, ichansel = [ [ 1, channel_count, 1, 1 ] ], 
        cmodel = 'COMP', **calib_params )
    dummy_uv.zap()
    
    # copy solutions to original UV data
    call_aips_task( 'TACOP', indata = div_uv, inext = 'SN', invers = 0, ncount = 1,
        outdata = uv, outvers = 0 )
    div_uv.zap()
    
    # resample solutions to UV database time grid
    if ( interpolation_method == '' ):
      if do_amplitude:
        im = 'linear'
      else:
        im = 'spline'
    else:
      im = interpolation_method
    in_version = uv.table_highver( 'SN' )
    out_version = in_version + 1
    try:
      re_sample_solutions( uv, in_version = in_version, out_version = out_version,
          interpolation_method = im, gap_time = solution_interval )
#      uv.zap_table( 'SN', in_version )
    except:
      re_sample_solutions( uv, in_version = in_version, out_version = out_version,
          interpolation_method = 'linear', gap_time = solution_interval )
#      uv.zap_table( 'SN', in_version )
    else:
      out_version = in_version
  
  # cleanup
  if ( flag_uv != uv ):
    flag_uv.zap()
  
  return out_version

###############################################################################

def calibrate_uv( uv, model_uv, reference_antenna = 0, phase_interval = 0.,
    do_amplitude = False, amplitude_interval = 5., interpolation_method = '',
    apply_flags = True, calib_params = {}, snr_limit = 2. ):
  
  # divide model out
  div_uv = divide_uv( uv, model_uv, apply_flags = apply_flags )
  
  # calibrate UV data against 1 Jy point source model
  if do_amplitude:
    solmode = 'A&P'
    solution_interval = amplitude_interval
  else:
    solmode = 'P'
    solution_interval = phase_interval
  dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', -1, 'UV' )
  channel_count = get_channel_count( uv )
  call_aips_task( 'CALIB', indata = div_uv, smodel = [ 1, 0, 0, 0, 0, 0, 0 ],
      outdata = dummy_uv, cmethod = 'DFT', snver = 0,
      refant = reference_antenna, solint = solution_interval, solsub = 1,
      solmin = 1, weightit = 0, soltype = 'L1R', solmode = solmode,
      aparm = [ 4, 0, 0, 0, 0, 0, snr_limit, 0, 0 ], cparm = [ 0, 1, 0, 0, 0, 0 ],
      docalib = -1, gainuse = -1, ichansel = [ [ 1, channel_count, 1, 1 ] ], 
      cmodel = 'COMP', **calib_params )
  dummy_uv.zap()
  
  # copy solutions to original UV data
  call_aips_task( 'TACOP', indata = div_uv, inext = 'SN', invers = 0, ncount = 1,
      outdata = uv, outvers = 0 )
  div_uv.zap()
  
  # resample solutions to UV database time grid
  if ( interpolation_method == '' ):
    if do_amplitude:
      im = 'linear'
    else:
      im = 'spline'
  else:
    im = interpolation_method
  in_version = uv.table_highver( 'SN' )
  out_version = in_version + 1
  try:
    re_sample_solutions( uv, in_version = in_version, out_version = out_version,
        interpolation_method = im, gap_time = solution_interval )
#    uv.zap_table( 'SN', in_version )
  except:
    re_sample_solutions( uv, in_version = in_version, out_version = out_version,
        interpolation_method = 'linear', gap_time = solution_interval )
#    uv.zap_table( 'SN', in_version )
  
  return out_version

###############################################################################
