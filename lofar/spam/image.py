###############################################################################

# import Python modules
from sys import *
from os import *
from math import *
import pdb

# import 3rd party modules
from numpy import *

# import user modules
from files import *
from aips import *
from solutions import *
from sphere import *
from parameter import *
from skymodel import *
from error import *
#from flag import *

###############################################################################

def prepare_uv_data( uv, stokes = 'I', solution_version = None, flag_version = None, 
    diameter = None ):
  
  # TODO: put in primary beam parameters at different frequencies for GMRT, VLA and WSRT (and others ??)
  prep_uv = get_aips_file( uv.disk, uv.name, 'PREP', - 1, 'UV' )
  
  # do some data checks
  uv = get_aips_file( uv.disk, uv.name, uv.klass, uv.seq, 'UV' )
  if ( not uv.exists() ):
    raise error( 'AIPS UV data %s not found' % ( str( uv ) ) )
  if ( uv.header.object == 'MULTI' ):
    raise error( 'only single source UV data is currently supported' )
  try:
    if_index = uv.header.ctype.index( 'IF' )
  except:
    pass
  else:
    if ( uv.header.naxis[ if_index ] != 1 ):
      raise error( 'only single IF UV data is currently supported' )
#  if uv.header.naxis[ uv.header.ctype.index( 'STOKES' ) ] != 1:
#    raise error( 'only single stokes UV data is currently supported' )
  
  # convert epoch B1950. to J2000.
  epoch = get_epoch( uv )
  if ( epoch == 1950. ):
    call_aips_task( 'UVFIX', indata = uv, outdata = prep_uv )
  elif ( epoch == 2000. ):
    call_aips_task( 'MOVE', indata = uv, outdata = prep_uv, userid = get_aips_userid() )
  else:
    raise error( 'AIPS UV data has unknown epoch %s' % ( repr( epoch ) ) )
  
  # apply flags
  if ( flag_version != None ):
    if table_exists( prep_uv, 'FG', flag_version ):
      flag_uv = apply_flag_table( prep_uv, version = flag_version )
      prep_uv.zap()
      flag_uv.rename( name = prep_uv.name, klass = prep_uv.klass, seq = prep_uv.seq )
  
  # apply solutions
  if ( solution_version != None ):
    if table_exists( prep_uv, 'SN', solution_version ):
      cal_uv = apply_solution_table( prep_uv, version = solution_version )
      prep_uv.zap()
      cal_uv.rename( name = prep_uv.name, klass = prep_uv.klass, seq = prep_uv.seq )
  
  # convert to stokes
  if ( stokes != '' ):
    stokes_uv = get_aips_file( uv.disk, uv.name, stokes, - 1, 'UV' )
    call_aips_task( 'SPLIT', indata = prep_uv, stokes = stokes, douvcomp = 0,
        outdisk = stokes_uv.disk, outclass = stokes_uv.klass, outseq = stokes_uv.seq )
    prep_uv.zap()
    stokes_uv.rename( name = prep_uv.name, klass = prep_uv.klass, seq = prep_uv.seq )
  
  # sort data to time-baseline order
  sort_uv = get_aips_file( uv.disk, uv.name, 'SORT', - 1, 'UV' )
  call_aips_task( 'UVSRT', indata = prep_uv, outdata = sort_uv, sort = 'TB' )
  prep_uv.zap()
  sort_uv.rename( name = prep_uv.name, klass = prep_uv.klass, seq = prep_uv.seq )
  
  # determine several useful parameters
  scale_table = array( get_frequency_list( prep_uv ) ) / get_frequency( prep_uv )
  uvw_table = []
  time_table = []
  weight_table = []
  wiz_uv = wizardry( prep_uv )
  for group in wiz_uv:
    uvw_table.append( [ uvw for uvw in group.uvw ] )
    if time_table == []:
      time_table.append( group.time )
    elif group.time != time_table[ -1 ]:
      time_table.append( group.time )
    vis = array( group.visibility, dtype = float32 )
    weights = transpose( vis )[ 2 ].ravel().sum()
    weight_table.append( float( weights ) )
  uvw_table = array( uvw_table, dtype = float32 )
  time_table = array( time_table, dtype = float32 )
  weight_table = array( weight_table, dtype = float32 )
  delta_time_table = time_table[ 1 : ] - time_table[ : -1 ]
  integration_time = 24. * 60. * 60. * median( delta_time_table ) # seconds
  uv2_table = array( [ ( uv**2 ).sum() for uv in uvw_table[ : , 0 : 2 ] ], dtype = float32 )
  uv_max = scale_table.max() * sqrt( uv2_table.max() ) # lambda
  uv_min = scale_table.min() * sqrt( uv2_table.min() ) # lambda
  w_table = array( uvw_table[ : , 2 ], dtype = float32 )
  w_max = scale_table.max() * ( abs( w_table ) ).max()
  time_count = len( time_table )
  time_min = time_table[ 0 ]
  time_max = time_table[ -1 ]
  visibility_count = int( len( uvw_table ) * get_channel_count( prep_uv ) )
  sum_weights = weight_table.sum()
#  beam_size = float( around( 1.22 * 3600. * degrees( 1. / uv_max ), 1 ) ) # arcsec
  beam_size = float( around( 1. * 3600. * degrees( 1. / uv_max ), 1 ) ) # arcsec
  cell_size = around( beam_size / 4., 1 ) # arcsec / pixel
  
  # get array specific parameters
  if ( diameter != None ):
    dish_diameter = diameter
    pbparm = [ 0., 0., 0., 0., 0., 0., 0. ]
  elif ( prep_uv.header.telescop.find( 'GMRT' ) != -1  ):
    dish_diameter = 45.
    frequency = get_central_frequency( prep_uv )
    if ( frequency > 140.e6 ) and ( frequency < 170.e6 ):
      pbparm = [ 0.3, 1., -4.04, 76.2, -68.8, 22.03, 0. ]
    elif ( frequency > 200.e6 ) and ( frequency < 280.e6 ):
      pbparm = [ 0.3, 1., -3.366, 46.159, -29.963, 7.529, 0. ]
    elif ( frequency > 300.e6 ) and ( frequency < 350.e6 ):
      pbparm = [ 0.3, 1., -3.397, 47.192, -30.931, 7.803, 0. ] 
    elif ( frequency > 30.e6 ) and ( frequency < 80.e6 ):
      pbparm = [ 0.3, 1., -4., 60., -50., 15., 0. ]
    else:
      raise error( 'beam shape of telescope at %d MHz is unknown' % (
          int( frequency / 1.e6 ) ) )
  elif ( prep_uv.header.telescop.find( 'VLA' ) != -1  ):
    dish_diameter = 25.
    frequency = get_central_frequency( prep_uv )
    if ( frequency > 65.e6 ) and ( frequency < 85.e6 ):
      pbparm = [ 0.3, 1., -0.897, 2.71, -0.242, 0., 0. ]
    elif ( frequency > 315.e6 ) and ( frequency < 335.e6 ):
      pbparm = [ 0.3, 1., -0.935, 3.23, -0.378, 0., 0. ]
    else:
      raise error( 'beam shape of telescope at %d MHz is unknown' % (
          int( frequency / 1.e6 ) ) )
  elif ( prep_uv.header.telescop.find( 'WSRT' ) != -1  ):
    dish_diameter = 25.
    frequency = get_central_frequency( prep_uv )
    # primary beam shape = cos(C*nu*r)^6, nu in GHz and r in degrees
    if ( frequency > 1.30e9 ) and ( frequency < 1.60e9 ):
      # C = 68.
      pbparm = [ 0.3, 1., -1.070370370, 5.091967688, -1.423131628, 0.2608246907,
          -0.03324696029 ]
    else:
      raise error( 'beam shape of telescope at %d MHz is unknown' % (
          int( frequency / 1.e6 ) ) )
  elif ( ( prep_uv.header.telescop.find( 'LOFAR36' ) != -1  ) or 
         ( prep_uv.header.telescop.find( 'LOFAR49' ) != -1  ) ):
    dish_diameter = 25.
    pbparm = [ 0., 0., 0., 0., 0., 0., 0. ]
  elif ( prep_uv.header.telescop.find( 'LOFAR' ) != -1  ):
    dish_diameter = 50.
    pbparm = [ 0., 0., 0., 0., 0., 0., 0. ]
  else:
    raise error( 'telescope array %s not supported' % ( prep_uv.header.telescop.strip() ) )
  
  # store parameters with UV data
  store_parameter( prep_uv, 'visibility_count', visibility_count )
  store_parameter( prep_uv, 'sum_weights', sum_weights )
  store_parameter( prep_uv, 'uv_min', uv_min )
  store_parameter( prep_uv, 'uv_max', uv_max )
  store_parameter( prep_uv, 'w_max', w_max )
  store_parameter( prep_uv, 'time_count', time_count )
  store_parameter( prep_uv, 'integration_time', integration_time )
  store_parameter( prep_uv, 'time_min', time_min )
  store_parameter( prep_uv, 'time_max', time_max )
  store_parameter( prep_uv, 'beam_size', beam_size )
  store_parameter( prep_uv, 'cell_size', cell_size )
  store_parameter( prep_uv, 'dish_diameter', dish_diameter )
  store_parameter( prep_uv, 'pbparm3', pbparm[ 2 ] )
  store_parameter( prep_uv, 'pbparm4', pbparm[ 3 ] )
  store_parameter( prep_uv, 'pbparm5', pbparm[ 4 ] )
  store_parameter( prep_uv, 'pbparm6', pbparm[ 5 ] )
  store_parameter( prep_uv, 'pbparm7', pbparm[ 6 ] )
  
  return prep_uv

###############################################################################

def image_cpb_facets( uv, apply_solutions = False, solution_version = 0,
    gain = 0.1, factor = 0., imagr_params = {} ):

  # make initial central primary beam facets (no cleaning)
  cell_size = restore_parameter( uv, 'cell_size' )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  cpb_facet_count = restore_parameter( uv, 'cpb_facet_count' )
  cpb_facet_size = restore_parameter( uv, 'pb_facet_size' )
  cpb_facet_file_name = restore_parameter( uv, 'pb_facet_file_name' )
  channel_count = get_channel_count( uv )
  cpb_facets = get_aips_file( uv.disk, 'CPB', 'ICL001', - 1, 'MA' )
  if ( apply_solutions and table_exists( uv, 'SN', solution_version ) ):
    docalib = 100
    gainuse = solution_version
  else:
    docalib = -1
    gainuse = -1
  call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, in2disk = uv.disk,
      outdisk = cpb_facets.disk, overlap = 2, outver = 0, docalib = docalib,
      gainuse = gainuse, outname = cpb_facets.name, outseq = cpb_facets.seq,
      cellsize = [ cell_size, cell_size ], do3dimag = 1, niter = 100000,
      flux = 100000., imsize = [ cpb_facet_size, cpb_facet_size ], dotv = 0,
      boxfile = cpb_facet_file_name, cmethod = '', minpatch = cpb_facet_size - 1,
      gain = gain, nfield = cpb_facet_count, maxpixel = 0, factor = factor,
      imagrprm = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
      flagver = -1, bcomp = [ 0 for j in range( 64 ) ], 
      uvsize = [ uv_size, uv_size ], **imagr_params )

  # remove empty model tables
  for i in range( 1, cpb_facet_count + 1 ):
    cpb_facet_i = get_facet( cpb_facets, i )
    cpb_facet_i.zap_table( 'CC', 0 )

  # store parameters  
  store_parameter( cpb_facets, 'facet_count', cpb_facet_count )
  store_parameter( cpb_facets, 'facet_file_name', cpb_facet_file_name )

  return cpb_facets

###############################################################################

def measure_cpb_noise( uv, facets, blank_edge = 8, keep_image = False,
    edge_weight = 0. ):
  
  # smooth overlapping facets
  if ( ( edge_weight == None ) or ( edge_weight >= 1. ) ):
    weightit = 0
  else:
    facet_size = get_image_size( facets )
    pixel_size = get_pixel_size( facets )
    xc = float( facet_size[ 0 ] - 1 ) / 2.
    yc = float( facet_size[ 1 ] - 1 ) / 2.
    rc = min( [ ( xc - float( blank_edge ) ) * pixel_size[ 0 ],
                ( yc - float( blank_edge ) ) * pixel_size[ 1 ] ] )
    weightit = ( 1. - max( [ 0.0001, edge_weight ] ) ) / rc
  
  # combine facets into image
  cpb_facet_count = restore_parameter( uv, 'cpb_facet_count' )
  cpb_image_size = restore_parameter( uv, 'cpb_image_size' )
  
  # blank circular edge area of facet
  temp_facets = get_aips_file( facets.disk, 'TEMP', facets.klass, -1, 'MA' )
  for i in range( 1, cpb_facet_count + 1 ):
    facet_i = get_facet( facets, i )
    temp_facet_i = get_facet( temp_facets, i )
    # trick to overcome noise measurement problem with uncleaned facets
    if ( facet_i.header.niter == 0 ):
      call_aips_task( 'CCMOD', indata = facet_i, invers = -1, opcode = 'POIN',
          flux = 0., pixxy = [ 1., 1. ] )
      call_aips_task( 'CCRES', indata = facet_i, in2data = facet_i, invers = 0,
          outdata = temp_facet_i, optype = 'ADD' )
      # temporary bug fix
      if ( not table_exists( temp_facet_i, 'CC', 1 ) ):
        table_file_name = get_aips_file_name( temp_facet_i, table = 'CC',
            version = 1 )
        if file_exists( table_file_name ):
          remove_file( table_file_name )
      facet_i.zap_table( 'CC', 0 )
    else:
      call_aips_task( 'MOVE', indata = facet_i, outdata = temp_facet_i,
          userid = get_aips_userid() )
    fill_facet( temp_facet_i, do_edge_circle = True )
  
  # combine facets into image
  cpb_image = get_aips_file( facets.disk, 'CPB', 'FLATN', - 1, 'MA' )
  coordinates = degdeg_to_hmsdms( get_radec( facets ) )
  call_aips_task( 'FLATN', indata = temp_facets, nfield = cpb_facet_count,
      outdata = cpb_image, imsize = [ cpb_image_size, cpb_image_size ],
      edgskp = blank_edge, weightit = weightit, reweight = [ 3, 0.5 ],
      coordina = coordinates )
  
  # measure noise in image
  [ cpb_avg, cpb_noise ] = call_aips_task( 'IMEAN', indata = cpb_image,
      pixavg = 0., pixstd = 0., outputs = [ 'pixavg', 'pixstd' ] )
  [ cpb_avg, cpb_noise ] = call_aips_task( 'IMEAN', indata = cpb_image,
      pixavg = cpb_avg, pixstd = cpb_noise, outputs = [ 'pixavg', 'pixstd' ],
      pixrange = [ -10. * cpb_noise, 10. * cpb_noise ]  )
  if ( cpb_noise == 0. ):
    cpb_noise = get_image_rms( cpb_image )
  
  # clean up
  if ( not keep_image ):
    cpb_image.zap()
  for i in range( 1, cpb_facet_count + 1 ):
    temp_facet_i = get_facet( temp_facets, i )
    temp_facet_i.zap()
  
  # store parameters with UV data
  store_parameter( uv, 'cpb_noise', cpb_noise )
  
  return cpb_noise

###############################################################################

def calibrate_pb( uv, pbm_facets, sigma = 0., signal_to_noise = 10.,
    reference_antenna = 0, conversion_method = 'DFT', do_amplitude = False,
    amplitude_interval = 5., phase_interval_min = 1. / 60.,
    apply_solutions = False, calib_params = {}, interpolation_method = '',
    snr_limit = 2.5, print_info = False ):
  
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
  
  # calibrate UV data
  if do_amplitude:
    solmode = 'A&P'
    solution_interval = amplitude_interval
  else:
    solmode = 'P'
    solution_interval = max( [ solution_interval, phase_interval_min ] )
  if print_info:
    print '... calibration solution interval = %s sec' % ( 
        repr( 60. * solution_interval ) )
  if ( apply_solutions and table_exists( uv, 'SN', 0 ) ):
    docalib = 100
    gainuse = uv.table_highver( 'SN' )
  else:
    docalib = - 1
    gainuse = - 1
  dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', - 1, 'UV' )
  channel_count = get_channel_count( uv )
  call_aips_task( 'CALIB', indata = uv, in2data = pbm_facets, invers = 0,
      ncomp = [ 0 ], flux = sigma * cpb_noise, nmaps = pbm_facet_count,
      cmodel = 'COMP', outdata = dummy_uv, cmethod = conversion_method,
      snver = 0, refant = reference_antenna, solint = solution_interval,
      solsub = 1, solmin = 1, weightit = 1, soltype = 'L1R', solmode = solmode,
      aparm = [ 4, 0, 0, 0, 0, 0, snr_limit, 0, 0 ], cparm = [ 0, 1, 0, 0, 0, 0 ],
      docalib = docalib, gainuse = gainuse, 
      ichansel = [ [ 1, channel_count, 1, 1 ] ], **calib_params )
  dummy_uv.zap()
  
  # TODO: flagging based on solutions
  
#  if ( interval_count > 1 ):
  if True:
    if ( interpolation_method == '' ):
      if do_amplitude:
        im = 'linear'
      else:
        im = 'spline'
    else:
      im = interpolation_method
    
    if print_info:
      print '... interpolating solutions to visibility time grid'
    # resample solutions to UV database time grid
    in_version = uv.table_highver( 'SN' )
    out_version = in_version + 1
    if ( interval_count <= 0 ):
      interval_count = 1
    try:
      re_sample_solutions( uv, in_version = in_version, out_version = out_version,
          weight_multiplier = 1. / sqrt( float( interval_count ) ), 
          interpolation_method = im, gap_time = 5. * solution_interval )
    except:
      re_sample_solutions( uv, in_version = in_version, out_version = out_version,
          weight_multiplier = 1. / sqrt( float( interval_count ) ), 
          interpolation_method = 'linear', gap_time = 5. * solution_interval )
    else:
      pass
#    uv.zap_table( 'SN', in_version )
  
  # store parameters
  store_parameter( uv, 'solution_interval', solution_interval )
  
  return

###############################################################################

def fit_gaussian_to_peak( facet, pos = [], offset_ratio_max = 0.5, double_ratio_min = 0.25,
    return_double_fit = False ):

  fit_results = []

  if ( pos == [] ):
    [ max_flux, [ max_x, max_y ] ] = get_image_extremum( facet, force_positive = True )
  else:
    [ max_x, max_y ] = [ int( around( pos[ 0 ] ) ), int( around( pos[ 1 ] ) ) ]
    max_flux = get_pixel_value( facet, [ max_x, max_y ] )
  facet_size = get_image_size( facet )
  [ beam_bmaj_pix, beam_bmin_pix, beam_bpa_pix ] = convert_beam_size( facet, to_pixel = True )
  x_min = max( [ 1, max_x - 32 ] )
  x_max = min( [ facet_size[ 0 ], max_x + 31 ] )
  y_min = max( [ 1, max_y - 32 ] )
  y_max = min( [ facet_size[ 1 ], max_y + 31 ] )
  good_fit = True

  # check if fit is done on beam
  if ( [ beam_bmaj_pix, beam_bmin_pix, beam_bpa_pix ] == [ 0., 0., 0. ] ):
    try:
      [ fmax, fpos, fwidth, domax, dopos, dowidth ] = call_aips_task( 'JMFIT', indata = facet,
          blc = [ x_min, y_min, 0, 0, 0, 0, 0 ], trc = [ x_max, y_max, 0, 0, 0, 0, 0 ], ngauss = 1, niter = 200,
          gmax = [ max_flux, 0, 0, 0 ], gpos = [ [ max_x, max_y ], [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ],
          gwidth = [ [ 4., 4., 0. ], [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ],
          domax = [ 1, 0, 0, 0 ], dopos = [ [ 1, 1 ], [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ], 
          dowidth = [ [ 1, 1, 1 ], [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ], 
          dooutput = - 1, domodel = - 1, outvers = - 1, offset = 0.,
          outputs = [ 'fmax', 'fpos', 'fwidth', 'domax', 'dopos', 'dowidth' ] )
    except:
      pass
    else:
      peak_flux = fmax[ 0 ]
      peak_flux_err = domax[ 0 ]
      [ peak_x, peak_y ] = fpos[ 0 ]
      [ peak_x_err, peak_y_err ] = dopos[ 0 ]
      [ bmaj_pix, bmin_pix, bpa_pix ] = fwidth[ 0 ]
      [ bmaj_err_pix, bmin_err_pix, bpa_err_pix ] = dowidth[ 0 ]
      # convert bmaj, bmin and bpa to arcsec
      # TODO: use errors 
      [ int_bmaj, int_bmin, int_bpa ] = convert_beam_size( facet, beam = [ bmaj_pix, bmin_pix, bpa_pix ] )
      fit_results.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )
    return fit_results

  # fit gaussian to peak
  try:
    [ fmax, fpos, fwidth, domax, dopos, dowidth ] = call_aips_task( 'JMFIT', indata = facet,
        blc = [ x_min, y_min, 0, 0, 0, 0, 0 ], trc = [ x_max, y_max, 0, 0, 0, 0, 0 ], ngauss = 1, niter = 200,
        gmax = [ max_flux, 0, 0, 0 ], gpos = [ [ max_x, max_y ], [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ],
        gwidth = [ [ beam_bmaj_pix, beam_bmin_pix, beam_bpa_pix ], [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ],
        domax = [ 1, 0, 0, 0 ], dopos = [ [ 1, 1 ], [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ], 
        dowidth = [ [ 1, 1, 1 ], [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ], 
        dooutput = - 1, domodel = - 1, outvers = - 1, offset = 0.,
        outputs = [ 'fmax', 'fpos', 'fwidth', 'domax', 'dopos', 'dowidth' ] )
  except:
    pass
  else:
    peak_flux = fmax[ 0 ]
    peak_flux_err = domax[ 0 ]
    [ peak_x, peak_y ] = fpos[ 0 ]
    [ peak_x_err, peak_y_err ] = dopos[ 0 ]
    [ bmaj_pix, bmin_pix, bpa_pix ] = fwidth[ 0 ]
    [ bmaj_err_pix, bmin_err_pix, bpa_err_pix ] = dowidth[ 0 ]
    # convert bmaj, bmin and bpa to arcsec
    # TODO: use errors 
    [ int_bmaj, int_bmin, int_bpa ] = convert_beam_size( facet, beam = [ bmaj_pix, bmin_pix, bpa_pix ] )

    # sanity check on fit
    if ( ( ( peak_x - max_x )**2 + ( peak_y - max_y )**2 ) < ( offset_ratio_max * beam_bmaj_pix )**2 ):
      fit_results.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )
    else:
      # fitted peak is offset a long way from the peak flux pixel
      # maybe we're dealing with a close double here
      if ( peak_flux > ( ( 1. + double_ratio_min ) / 2. ) * max_flux ):
        # try fitting a double gaussian
        try:
          [ fmax, fpos, fwidth, domax, dopos, dowidth ] = call_aips_task( 'JMFIT', indata = facet,
              blc = [ x_min, y_min, 0, 0, 0, 0, 0 ], trc = [ x_max, y_max, 0, 0, 0, 0, 0 ], ngauss = 2,
              gmax = [ max_flux, peak_flux, 0, 0 ], niter = 200,
              gpos = [ [ max_x, max_y ], [ peak_x, peak_y ], [ 0, 0 ], [ 0, 0 ] ],
              gwidth = [ [ beam_bmaj_pix, beam_bmin_pix, beam_bpa_pix ], 
                         [ beam_bmaj_pix, beam_bmin_pix, beam_bpa_pix ], [ 0, 0, 0 ], [ 0, 0, 0 ] ],
              domax = [ 1, 1, 0, 0 ], dopos = [ [ 1, 1 ], [ 1, 1 ], [ 0, 0 ], [ 0, 0 ] ], 
              dowidth = [ [ 1, 1, 1 ], [ 1, 1, 1 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ], 
              dooutput = -1, domodel = -1, outvers = -1, offset = 0.,
              outputs = [ 'fmax', 'fpos', 'fwidth', 'domax', 'dopos', 'dowidth' ] )
        except:
          pass
        else:
          # use the component closest to the peak flux pixel
          [ x1, y1 ] = fpos[ 0 ]
          [ x2, y2 ] = fpos[ 1 ]
          r1_2 = ( x1 - max_x )**2 + ( y1 - max_y )**2
          r2_2 = ( x2 - max_x )**2 + ( y2 - max_y )**2
          if ( r1_2 <= r2_2 ):
            closest_index = 0
            other_index = 1
          else:
            closest_index = 1
            other_index = 1
          peak_flux = fmax[ closest_index ]
          peak_flux_err = domax[ closest_index ]
          [ peak_x, peak_y ] = fpos[ closest_index ]
          [ peak_x_err, peak_y_err ] = dopos[ closest_index ]
          [ bmaj_pix, bmin_pix, bpa_pix ] = fwidth[ closest_index ]
          [ bmaj_err_pix, bmin_err_pix, bpa_err_pix ] = dowidth[ closest_index ]
          # convert bmaj, bmin and bpa to arcsec
          # TODO: use errors 
          [ int_bmaj, int_bmin, int_bpa ] = convert_beam_size( facet, beam = [ bmaj_pix, bmin_pix, bpa_pix ] )

          # recheck fitted peak distance
          if ( ( ( peak_x - max_x )**2 + ( peak_y - max_y )**2 ) < ( offset_ratio_max * beam_bmaj_pix )**2 ):
            fit_results.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )
            if return_double_fit:
              peak_flux = fmax[ other_index ]
              peak_flux_err = domax[ other_index ]
              [ peak_x, peak_y ] = fpos[ other_index ]
              [ peak_x_err, peak_y_err ] = dopos[ other_index ]
              [ bmaj_pix, bmin_pix, bpa_pix ] = fwidth[ other_index ]
              [ bmaj_err_pix, bmin_err_pix, bpa_err_pix ] = dowidth[ other_index ]
              # convert bmaj, bmin and bpa to arcsec
              # TODO: use errors 
              [ int_bmaj, int_bmin, int_bpa ] = convert_beam_size( facet, 
                  beam = [ bmaj_pix, bmin_pix, bpa_pix ] )
              fit_results.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )

  if ( len( fit_results ) > 1 ):
    fit_results.sort( cmp = lambda a, b: cmp( b[ 1 ], a[ 1 ] ) )

  return fit_results

###############################################################################

def get_facet_rms( facet, remove_components = False ):
  if remove_components and ( not model_table_empty( facet, 0 ) ):
    temp_facet = get_aips_file( facet.disk, 'TEMP', facet.klass, -1, 'MA' )
    call_aips_task( 'CCRES', indata = facet, in2data = facet, invers = 0,
          outdata = temp_facet, optype = 'SUB' )
    # temporary bug fix
    if ( not table_exists( temp_facet, 'CC', 1 ) ):
      table_file_name = get_aips_file_name( temp_facet, table = 'CC',
          version = 1 )
      if file_exists( table_file_name ):
        remove_file( table_file_name )
    rms = get_image_rms( temp_facet )
    temp_facet.zap()
  else:
    rms = get_image_rms( facet )
  return rms

###############################################################################

def add_clean_boxes( uv, facets, sigma_min, facet_file_name = '', 
    peak_flux_ratio_max = None, clean_box_radius = 10, keep_boxes = True,
    new_boxes_per_facet_max = 100, facet_based_noise = False, print_info = False ):

  if ( facet_file_name == '' ):
    used_facet_file_name = restore_parameter( facets, 'facet_file_name' )
  else:
    used_facet_file_name = facet_file_name

  # if requested, remove clean box definitions from current boxfile
  facet_count = restore_parameter( facets, 'facet_count' )
#  try:
#    added_facet_count = restore_parameter( facets, 'added_facet_count' )
#  except:
#    added_facet_count = 0
#  original_facet_count = facet_count - added_facet_count
  temp_facet_file_name = used_facet_file_name + '.TEMP'
  if file_exists( temp_facet_file_name ):
    remove_file( temp_facet_file_name )
  if keep_boxes:
    copy_file( used_facet_file_name, temp_facet_file_name )
  else:
    facet_list = range( 1, facet_count + 1 )
    extract_facet_definitions( used_facet_file_name, facet_list, temp_facet_file_name,
        include_clean_boxes = False )

  # blank non-search area and determine peak in local noise
  # determine minimum flux for sources
  peak_flux_max = 0.
  peak_rms = restore_parameter( uv, 'cpb_noise' )
  res_facets = get_aips_file( facets.disk, 'RES', facets.klass, - 1, 'MA' )
  for i in range( 1, facet_count + 1 ):
    facet_i = get_facet( facets, i )
    res_facet_i = get_facet( res_facets, i )
    call_aips_task( 'MOVE', indata = facet_i, outdata = res_facet_i, userid = get_aips_userid(), opcode = '' )
    fill_facet( res_facet_i, facet_file_name = temp_facet_file_name, do_edge_circle = True )
    [ peak_flux, pos ] = get_image_extremum( res_facet_i, force_positive = True )
    if ( peak_flux > peak_flux_max ):
      peak_flux_max = peak_flux
#    rms = get_facet_rms( res_facet_i, remove_components = False )
    if facet_based_noise:
      [ avg, rms ] = call_aips_task( 'IMEAN', indata = res_facet_i, pixavg = 0., pixstd = 0.,
          outputs = [ 'pixavg', 'pixstd' ] )
      [ avg, rms ] = call_aips_task( 'IMEAN', indata = res_facet_i, pixavg = avg, pixstd = rms,
          pixrange = [ - 10. * rms, 10. * rms  ], outputs = [ 'pixavg', 'pixstd' ] )
      if ( rms > peak_rms ):
        peak_rms = rms
  flux_min = sigma_min * peak_rms
  if ( peak_flux_ratio_max == None ):
    peak_flux_min = flux_min
  else:
    peak_flux_min = max( [ flux_min, peak_flux_max / peak_flux_ratio_max ] )
  if print_info:
    print 'clean box flux min = ' + repr( peak_flux_min )
  store_parameter( facets, 'clean_box_flux_min', peak_flux_min )

  # search for positive peaks and add clean boxes to them
  facet_has_box = [ False for i in range( facet_count ) ]
  if ( peak_flux_max >= peak_flux_min ):
#    # first handle added facets
#    if( added_facet_count > 0 ):
#      for i in range( 1 + original_facet_count, 1 + facet_count ):
#        res_facet_i = get_facet( res_facets, i )
#        pos = get_pixel_reference( res_facet_i )
#        pixel_value = get_pixel_value( res_facet_i, pos, to_float = False )
#        if ( pixel_value != get_aips_magic_value() ):
#          add_circular_clean_box( temp_facet_file_name, i, pos, clean_box_radius )
#          fill_facet( res_facet_i, facet_file_name = temp_facet_file_name, do_edge_circle = True )
#          if print_info:
#            print '... adding clean box to facet ' + repr( i ) + ' at position ' + repr( pos )
#          facet_has_box[ i - 1 ] = True
#          if keep_boxes:
#            remove_rectangular_clean_box( temp_facet_file_name, i, [ 0, 0 ], [ 0, 0 ] )
#    # then handle all other facets
#    for i in range( 1, 1 + original_facet_count ):
    for i in range( 1, 1 + facet_count ):
      res_facet_i = get_facet( res_facets, i )
      [ peak_flux, pos ] = get_image_maximum( res_facet_i )
      new_boxes_count = 0
      while ( ( peak_flux >= peak_flux_min ) and ( new_boxes_count < new_boxes_per_facet_max ) ):
        # mark source area in residual facet
        add_circular_clean_box( temp_facet_file_name, i, pos, clean_box_radius )
        fill_facet( res_facet_i, facet_file_name = temp_facet_file_name, do_edge_circle = True,
            value = 0. )
        # check if source is closest to center of current facet
        peak_radec = calculate_source_radec( res_facet_i, pos )
        overlapping_facet_list = [ i ] + get_facet_overlap( res_facet_i )
        [ [ main_i, main_pos ] ] = find_source_facets( facets, peak_radec, primary_facet_only = True,
            facet_list = overlapping_facet_list )
        if ( i == main_i ):
          if print_info:
            print '... adding clean box to facet ' + repr( i ) + ' at position ' + repr( pos )
          facet_has_box[ i - 1 ] = True
          new_boxes_count = new_boxes_count + 1
          if ( keep_boxes and ( new_boxes_count == 1 ) ):
            remove_circular_clean_box( temp_facet_file_name, i, [ 5, 5 ], 1 )
            remove_rectangular_clean_box( temp_facet_file_name, i, [ 0, 0 ], [ 0, 0 ] )
        else:
          # double check that clean box was already added to previous main facet
#          while ( ( main_i < i ) or ( main_i > original_facet_count ) ):
          while ( main_i < i ):
            res_facet_j = get_facet( res_facets, main_i )
            pixel_value = get_pixel_value( res_facet_j, main_pos, to_float = False )
            if ( pixel_value == get_aips_magic_value() ):
              break
            else:
              # if not, find next main facet
              overlapping_facet_list.remove( main_i )
              [ [ main_i, main_pos ] ] = find_source_facets( facets, peak_radec, 
                  primary_facet_only = True, facet_list = overlapping_facet_list )
              if ( i == main_i ):
                if print_info:
                  print '... adding clean box to facet ' + repr( i ) + ' at position ' + repr( pos )
                facet_has_box[ i - 1 ] = True
                new_boxes_count = new_boxes_count + 1
                if ( keep_boxes and ( new_boxes_count == 1 ) ):
                  remove_circular_clean_box( temp_facet_file_name, i, [ 5, 5 ], 1 )
                  remove_rectangular_clean_box( temp_facet_file_name, i, [ 0, 0 ], [ 0, 0 ] )
          if ( i != main_i ):
            remove_circular_clean_box( temp_facet_file_name, i, pos, clean_box_radius )
        # blank the clean area
        fill_facet( res_facet_i, facet_file_name = temp_facet_file_name, do_edge_circle = True )
        # find next peak in residual facet
        [ peak_flux, pos ] = get_image_maximum( res_facet_i )

  # delete residual facets
  remove_facets( res_facets )

  # write dummy clean boxes to facets without clean box
  if ( not keep_boxes ):
    for i in range( 1, facet_count + 1 ):
      if ( not facet_has_box[ i - 1 ] ):
#        add_circular_clean_box( temp_facet_file_name, i, [ 5, 5 ], 1 )
        add_rectangular_clean_box( temp_facet_file_name, i, [ 0, 0 ], [ 0, 0 ] )

  # replace old facet file with new one
  move_file( temp_facet_file_name, used_facet_file_name )

  return

###############################################################################

def add_centered_pb_facets( uv, pb_facets, sigma_min, blank_radius = 10,
    restore_components = False, apply_solutions = False, print_info = False,
    new_facet_count_max = None, peel_pb_facets = None, gain = 0.1, factor = 0.,
    imagr_params = {}, facet_list = [] ):
# IMPORTANT: supply exactly the same imaging parameters as used for production of the pb_facets
  
  # restore parameters
  pb_facet_file_name = restore_parameter( uv, 'pb_facet_file_name' )
  pb_facet_size = get_image_size( pb_facets )
  pb_facet_count = restore_parameter( uv, 'pb_facet_count' )
  try:
    old_added_facet_count = restore_parameter( uv, 'added_pb_facet_count' )
  except:
    old_added_facet_count = 0
  old_pb_facet_count = pb_facet_count - old_added_facet_count
  temp_facet_file_name = pb_facet_file_name + '.TEMP'
  
  # save clean box information
  clean_box_list = get_clean_boxes( pb_facet_file_name, range( 1, pb_facet_count + 1 ) )
  clean_box_radec_list = []
  for clean_box in clean_box_list:
    [ i, a, b, c, d ] = clean_box
    facet = get_facet( pb_facets, i )
    if ( a == - 1 ): # circular area
      radec = calculate_source_radec( facet, [ c, d ] )
    else: # rectangular area
      radec = calculate_source_radec( facet, [ float( a + c ) / 2., float( b + d ) / 2. ] )
    # link clean box to facets
    if ( i in range( 1, 1 + old_pb_facet_count ) ):
      clean_box_radec_list.append( [ i, radec ] )
    else:
      clean_box_radec_list.append( [ - 1, radec ] )
  
  # create list of facets to add
  new_facet_list = []
  if ( peel_pb_facets != None ):
    for i in range( 1, 1 + restore_parameter( peel_pb_facets, 'facet_count' ) ):
      if ( len( facet_list ) > 0 ):
        if ( not i in facet_list ):
          continue
      facet = get_facet( peel_pb_facets, i )
      pos = get_pixel_reference( facet )
      fit_peak = get_pixel_value( facet, pos )
      peak_radec = get_radec( facet )
      new_facet_list.append( [ fit_peak, peak_radec ] )
  else:
    # restore model components
    if restore_components:
      facets = restore_model_components( pb_facets, imagr_params = imagr_params )
    else:
      facets = pb_facets
    # blank non-search area and determine peak in local noise
    res_facets = get_aips_file( pb_facets.disk, 'RES', pb_facets.klass, - 1, 'MA' )
    peak_rms = restore_parameter( uv, 'cpb_noise' )
    for i in range( 1, old_pb_facet_count + 1 ):
      if ( len( facet_list ) > 0 ):
        if ( not i in facet_list ):
          continue
      facet_i = get_facet( facets, i )
      res_facet_i = get_facet( res_facets, i )
      call_aips_task( 'MOVE', indata = facet_i, outdata = res_facet_i,
          userid = get_aips_userid(), opcode = '' )
      fill_facet( res_facet_i, do_edge_circle = True )
      if restore_components:
        facet_i.zap()
    flux_min = sigma_min * peak_rms
    if print_info:
      print '... minimum source flux = %s Jy' % ( repr( flux_min ) )
    # search for positive peaks
    extract_facet_definitions( pb_facet_file_name, range( 1, 1 + old_pb_facet_count ),
      temp_facet_file_name, include_clean_boxes = False )
    for i in range( 1, old_pb_facet_count + 1 ):
      res_facet_i = get_facet( res_facets, i )
      [ peak_flux, pos ] = get_image_extremum( res_facet_i, force_positive = True )
      while ( peak_flux >= flux_min ):
        fit_results = fit_gaussian_to_peak( res_facet_i, pos = pos,
            return_double_fit = True )
        if ( len( fit_results ) == 0 ):
          fit_pos = [ float( p ) for p in pos ]
          fit_peak = peak_flux
        else:
          [ fit_pos, fit_peak, fit_beam ] = fit_results[ 0 ]
        # blank source area in residual facet
        add_circular_clean_box( temp_facet_file_name, i, pos, blank_radius )
        fill_facet( res_facet_i, facet_file_name = temp_facet_file_name,
            do_edge_circle = True )
        # check if source is closest to center of current facet
        peak_radec = calculate_source_radec( res_facet_i, fit_pos )
        overlapping_facet_list = [ i ] + get_facet_overlap( res_facet_i )
        for j in range( old_pb_facet_count + 1, pb_facet_count + 1 ):
          if ( j in overlapping_facet_list ):
            overlapping_facet_list.remove( j )
        [ [ main_i, main_pos ] ] = find_source_facets( pb_facets, peak_radec,
            primary_facet_only = True, facet_list = overlapping_facet_list )
        if ( i == main_i ):
          # save new facet radec
          new_facet_list.append( [ fit_peak, peak_radec ] )
        else:
          # double check that peak was already associated with previous main facet
          while ( main_i < i ):
            res_facet_j = get_facet( res_facets, main_i )
            pixel_value = get_pixel_value( res_facet_j, main_pos, to_float = False )
            if ( pixel_value == 0. ):
              break
            else:
              # if not, find next main facet
              overlapping_facet_list.remove( main_i )
              [ [ main_i, main_pos ] ] = find_source_facets( pb_facets, peak_radec, 
                  primary_facet_only = True, facet_list = overlapping_facet_list )
              if ( i == main_i ):
                new_facet_list.append( [ fit_peak, peak_radec ] )
          if ( i != main_i ):
            remove_circular_clean_box( temp_facet_file_name, i, pos, blank_radius )
        # mark the actual clean area with zero
        fill_facet( res_facet_i, facet_file_name = temp_facet_file_name,
            do_edge_circle = True, value = 0. )
        # find next peak in residual facet
        [ peak_flux, pos ] = get_image_extremum( res_facet_i, force_positive = True )
    if print_info:
      print '... found %s sources above minimum flux' % ( repr( len( new_facet_list ) ) )
    # delete residual facets
    remove_facets( res_facets )
    remove_file( temp_facet_file_name )
    # remove restored facets
    if restore_components:
      remove_facets( facets )
  
  # remove previously added facets
  new_facet_file_name = pb_facet_file_name + '.NEW'
  extract_facet_definitions( pb_facet_file_name, range( 1, 1 + old_pb_facet_count ),
      new_facet_file_name, include_clean_boxes = False )
  for i in range( 1 + old_pb_facet_count, 1 + pb_facet_count ):
    pb_facet_i = get_facet( pb_facets, i )
    pb_beam_i = get_facet_beam( pb_facet_i )
    pb_facet_i.zap()
    pb_beam_i.zap()
  if print_info:
    print '... removed %s previously added facets' % ( repr( old_added_facet_count ) )
  
  # add new facets
  new_facet_list.sort( cmp = lambda a, b: cmp( b[ 0 ], a[ 0 ] ) )
  new_added_facet_count = len( new_facet_list )
  if ( new_facet_count_max != None ):
    new_added_facet_count = min( [ new_added_facet_count, new_facet_count_max ] )
  for i in range( new_added_facet_count ):
    peak_radec = new_facet_list[ i ][ 1 ]
    new_i = 1 + old_pb_facet_count + i
    add_facet( new_facet_file_name, peak_radec, pb_facet_size, facet_id = new_i,
        add_clean_box = False )
  new_pb_facet_count = old_pb_facet_count + new_added_facet_count
  # generate new facets images and beams
  if ( new_added_facet_count > 0 ):
    # set calibration parameters
    if apply_solutions:
      solution_switch = 100
      solution_version = uv.table_highver( 'SN' )
    else:
      solution_switch = 0
      solution_version = - 1
    # extract new facet definitions
    temp_facet_list = range( 1 + old_pb_facet_count, 1 + new_pb_facet_count )
    extract_facet_definitions( new_facet_file_name, temp_facet_list, temp_facet_file_name )
    # make beams and dirty images of new facets
    temp_facets = get_aips_file( pb_facets.disk, 'TEMP', 'ICL001', - 1, 'MA' )
    cell_size = restore_parameter( uv, 'cell_size' )
    uv_size = restore_parameter( uv, 'pb_image_size' )
    channel_count = get_channel_count( uv )
    model_version = pb_facets.table_highver( 'CC' )
    call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, flagver = -1,
        docalib = solution_switch, gainuse = solution_version, gain = gain,
        outdisk = temp_facets.disk, outname = temp_facets.name, maxpixel = 0,
        outseq = temp_facets.seq, outver = model_version, in2disk = uv.disk,
        imsize = pb_facet_size, do3dimag = 1, cellsize = [ cell_size, cell_size ],
        niter = 100000, flux = 100000., dotv = 0, boxfile = temp_facet_file_name,
        overlap = 2, nfield = new_added_facet_count, allokay = 0, factor = factor,
        imagrprm = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
        cmethod = '', minpatch = pb_facet_size[ 0 ] - 1,
        uvsize = [ uv_size, uv_size ], **imagr_params )
    # rename facets and beams to place them behind the already existing facets
    for i in range( 1, 1 + new_added_facet_count ):
      temp_facet_i = get_facet( temp_facets, i )
      temp_beam_i = get_facet_beam( temp_facet_i )
      new_i = new_pb_facet_count - new_added_facet_count + i
      facet_i = get_facet( pb_facets, new_i )
      beam_i = get_facet_beam( facet_i )
      if facet_i.exists():
        facet_i.clrstat()
        facet_i.zap()
      if beam_i.exists():
        beam_i.clrstat()
        beam_i.zap()
      temp_facet_i.rename( name = facet_i.name, klass = facet_i.klass, seq = facet_i.seq )
      temp_beam_i.rename( name = beam_i.name, klass = beam_i.klass, seq = beam_i.seq )
    # update parameters
    store_parameter( pb_facets, 'facet_count', new_pb_facet_count )
    store_parameter( pb_facets, 'added_facet_count', new_added_facet_count )
    store_parameter( uv, 'pb_facet_count', new_pb_facet_count )
    store_parameter( uv, 'added_pb_facet_count', new_added_facet_count )
  # update facet overlap info
  determine_facet_overlap( pb_facets )
  if print_info:
    print '... added %s centered facets' % ( repr( new_added_facet_count ) )
  
  # store clean box information in new facet configuration
  facet_has_clean_box = [ False for i in range( new_pb_facet_count ) ]
  for k in range( len( clean_box_list ) ):
    [ i, a, b, c, d ] = clean_box_list[ k ]
    [ j, radec ] = clean_box_radec_list[ k ]
    # skip over empty clean boxes
    if ( ( [ a, b, c, d ] == [ - 1, 1, 5, 5 ] ) or
         ( [ a, b, c, d ] == [ 0, 0, 0, 0 ] ) ):
      continue
    # find target facet
    if ( j > 0 ):
      facet = get_facet( pb_facets, j )
      overlap_list = [ j ] + get_facet_overlap( facet )
      source_facet_list = find_source_facets( pb_facets, radec, primary_facet_only = True,
          facet_list = overlap_list )
    else:
      source_facet_list = find_source_facets( pb_facets, radec, primary_facet_only = True )
    if ( len( source_facet_list ) == 0 ):
      # clean box lies outside all facets, so discard
      if print_info:
        print "WARNING: discarding clean box at radec %s" % ( 
            repr( degdeg_to_hmsdms( radec ) ) )
      continue
    [ main_i, main_pos ] = source_facet_list[ 0 ]
    if ( main_i != j ):
      # recalculate clean box coordinates in new facet
      # TODO: should we adjust clean box size to compensate for fractional pixel shift ?
      if ( a == - 1 ): # circular area
        [ c, d ] = [ int( around( main_pos[ 0 ] ) ), int( around( main_pos[ 1 ] ) ) ]
      else:
        [ dx, dy ] = [ abs( float( c - a ) / 2. ), abs( float( d - b ) / 2. ) ]
        [ a, b ] = [ int( around( main_pos[ 0 ] - dx ) ), 
            int( around( main_pos[ 1 ] - dy ) ) ]
        [ c, d ] = [ int( around( main_pos[ 0 ] + dx ) ),
            int( around( main_pos[ 1 ] + dy ) ) ]
    if ( a == - 1 ): # circular area
      add_circular_clean_box( new_facet_file_name, main_i, [ c, d ], b )
    else: # rectangular area
      add_rectangular_clean_box( new_facet_file_name, main_i, [ a, b ], [ c, d ] )
    if ( not facet_has_clean_box[ main_i - 1 ] ):
      facet_has_clean_box[ main_i - 1 ] = True
  
  # add empty clean boxes
  for i in range( 1, 1 + new_pb_facet_count ):
    if ( not facet_has_clean_box[ i - 1 ] ):
#      add_circular_clean_box( new_facet_file_name, i, [ 5, 5 ], 1 )
      add_rectangular_clean_box( new_facet_file_name, i, [ 0, 0 ], [ 0, 0 ] )
  
  # replace facet file
  move_file( pb_facet_file_name, pb_facet_file_name + '.OLD' )
  remove_file( temp_facet_file_name )
  move_file( new_facet_file_name, pb_facet_file_name )

  return new_added_facet_count

###############################################################################

def re_center_pb_facets( uv, facets, facet_file_name = '', search_radius = 10.,
    max_shift = 8., gain = 0.1, factor = 0., imagr_params = {} ):
# shifts facets by a fraction of a pixel to center sources
# clean boxes stay where they are
# TODO: should we update the facet overlap info (probably not)?

  pb_facet_count = restore_parameter( uv, 'pb_facet_count' )
  try:
    added_pb_facet_count = restore_parameter( uv, 'added_pb_facet_count' )
  except:
    return
  first_pb_facet_count = pb_facet_count - added_pb_facet_count
  first_facet_list = range( 1, 1 + first_pb_facet_count )
  added_facet_list = range( 1 + first_pb_facet_count, 1 + pb_facet_count )

  # create temporary copy of facet file
  if ( facet_file_name == '' ):
    used_facet_file_name = restore_parameter( facets, 'facet_file_name' )
  else:
    used_facet_file_name = facet_file_name
  temp_facet_file_name = used_facet_file_name + '.TEMP'
  extract_facet_definitions( used_facet_file_name, added_facet_list,
      temp_facet_file_name, include_clean_boxes = False )

  # blank non-search area
  res_facets = get_aips_file( facets.disk, 'RES', facets.klass, - 1, 'MA' )
  i = 0
  facets_changed = False
  for j in added_facet_list:
    i = i + 1
    facet_j = get_facet( facets, j )
    res_facet_i = get_facet( res_facets, i )
    call_aips_task( 'MOVE', indata = facet_j, outdata = res_facet_i, 
        userid = get_aips_userid(), opcode = '' )
    pos = get_pixel_reference( res_facet_i )
    add_circular_clean_box( temp_facet_file_name, i, pos, search_radius )
    fill_facet( res_facet_i, facet_file_name = temp_facet_file_name, invert = True )
    fit_results = fit_gaussian_to_peak( res_facet_i, pos = pos, return_double_fit = True )
    if ( len( fit_results ) == 0 ):
      [ peak, peak_pos ] = get_image_extremum( res_facet_i, force_positive = True )
      if ( peak_pos == pos ):
        res_facet_i.zap()
        continue
      fit_pos = [ float( p ) for p in peak_pos ]
    else:
      [ fit_pos, fit_peak, fit_beam ] = fit_results[ 0 ]
    shift2 = ( fit_pos[ 0 ] - pos[ 0 ] )**2 + ( fit_pos[ 1 ] - pos[ 1 ] )**2
    if ( shift2 <= max_shift**2 ):
      peak_radec = calculate_source_radec( res_facet_i, fit_pos )
      facet_size = get_image_size( res_facet_i )
      replace_facet( used_facet_file_name, j, facet_size, peak_radec, keep_boxes = True )
      facets_changed = True
    res_facet_i.zap()

  # generate new facets images and beams
  if facets_changed:

    # extract new facet definitions
    extract_facet_definitions( used_facet_file_name, added_facet_list,
        temp_facet_file_name, include_clean_boxes = False )

    # make dirty images of new facets
    temp_facets = get_aips_file( facets.disk, 'TEMP', 'ICL001', - 1, 'MA' )
    cell_size = restore_parameter( uv, 'cell_size' )
    uv_size = restore_parameter( uv, 'pb_image_size' )
    channel_count = get_channel_count( uv )
    facet_size = get_image_size( facets )
    call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, docalib = 0,
        gainuse = -1, outdisk = temp_facets.disk, outname = temp_facets.name,
        outseq = temp_facets.seq, outver = 0, in2disk = uv.disk, flagver = -1,
        cellsize = [ cell_size, cell_size ], imsize = facet_size, do3dimag = 1,
        niter = 100000, flux = 100000., boxfile = temp_facet_file_name, dotv = 0,
        overlap = 2, nfield = len( added_facet_list ), maxpixel = 0, factor = factor,
        imagrprm = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
        cmethod = '', minpatch = facet_size[ 0 ] - 1, gain = gain, allokay = 0,
        uvsize = [ uv_size, uv_size ], **imagr_params )

    i = 0
    for j in added_facet_list:
      i = i + 1
      temp_facet_i = get_facet( temp_facets, i )
      temp_beam_i = get_facet_beam( temp_facet_i )
      facet_j = get_facet( facets, j )
      beam_j = get_facet_beam( facet_j )

      # if present, copy solution tables (keep version number)
      if table_exists( facet_j, 'SN', 0 ):
        solution_version_high = facet_j.table_highver( 'SN' )
        for solution_version in range( 1, 1 + solution_version_high ):
          call_aips_task( 'TACOP', indata = facet_j, inext = 'SN',
              invers = solution_version, ncount = 1,
              outdata = temp_facet_i, outvers = solution_version )

      # if present, copy parameter tables (keep version number)
      if table_exists( facet_j, 'PS', 0 ):
        parameter_version_high = facet_j.table_highver( 'PS' )
        for parameter_version in range( 1, 1 + parameter_version_high ):
          call_aips_task( 'TACOP', indata = facet_j, inext = 'PS',
              invers = parameter_version, ncount = 1,
              outdata = temp_facet_i, outvers = parameter_version )

      # remove old facets and beams
      facet_j.zap()
      beam_j.zap()

      # rename facets and beams to place them behind the already existing facets
      temp_facet_i.rename( name = facet_j.name, klass = facet_j.klass, seq = facet_j.seq )
      temp_beam_i.rename( name = beam_j.name, klass = beam_j.klass, seq = beam_j.seq )

  remove_file( temp_facet_file_name )

  return

###############################################################################

def restore_model_components( facets, facet_list = [], model_version = 0,
    cross_restore = True, convolve = False, imagr_params = {} ):
  
  # filter imagr params
  r_params = {}
  restoring_beam = False
  if ( ( 'bmaj' in imagr_params ) or ( 'bmin' in imagr_params ) or 
      ( 'bpa' in imagr_params ) ):
    if ( ( 'bmaj' in imagr_params ) and ( 'bmin' in imagr_params ) and 
        ( 'bpa' in imagr_params ) ):
      r_params[ 'bmaj' ] = imagr_params[ 'bmaj' ]
      r_params[ 'bmin' ] = imagr_params[ 'bmin' ]
      r_params[ 'bpa' ] = imagr_params[ 'bpa' ]
      restoring_beam = True
    else:
      raise error( 'bmaj/bmin/bpa need to be specified together' )
  
  # TODO: cross-restoration with specified facet_list
  if ( len( facet_list ) > 0 ):
    rst_facet_list = [ i for i in facet_list ]
  else:
    facet_count = restore_parameter( facets, 'facet_count' )
    rst_facet_list = range( 1, facet_count + 1 )
#  if ( cross_restore and ( len( rst_facet_list ) != facet_count ) ):
#    raise error( 'no proper cross-restoration for limited number of facets implemented' )
  
  # make working copies of facets and remove model tables
  temp_facets = get_aips_file( facets.disk, 'TEMP', facets.klass, - 1, 'MA' )
  for i in rst_facet_list:
    facet_i = get_facet( facets, i )
    temp_facet_i = get_facet( temp_facets, i )
    call_aips_task( 'MOVE', indata = facet_i, outdata = temp_facet_i,
        userid = get_aips_userid() )
    while table_exists( temp_facet_i, 'CC', 0 ):
      temp_facet_i.zap_table( 'CC', 0 )
    while table_exists( temp_facet_i, 'SN', 0 ):
      temp_facet_i.zap_table( 'SN', 0 )
    while table_exists( temp_facet_i, 'PS', 0 ):
      temp_facet_i.zap_table( 'PS', 0 )
  
  # convolve background to restoring beam
  if ( convolve and restoring_beam ):
    cvl_facets = get_aips_file( facets.disk, 'CONVL', facets.klass, - 1, 'MA' )
    for i in rst_facet_list:
      # trick to change 'PRODUCT' header keyword from 'RESIDUAL' to 'DIRTY'
      temp_facet_i = get_facet( temp_facets, i )
      cvl_facet_i = get_facet( cvl_facets, i )
      new_table( temp_facet_i, 'CC', 0 )
      call_aips_task( 'CCRES', indata = temp_facet_i, in2data = temp_facet_i,
          invers = 0, outdata = cvl_facet_i, optype = 'ADD', **r_params )
      temp_facet_i.zap()
      # temporary bug fix
#      cvl_facet_i.zap_table( 'CC', 0 )
      if table_exists( cvl_facet_i, 'CC', 1 ):
        cvl_facet_i.zap_table( 'CC', 1 )
      else:
        table_file_name = get_aips_file_name( cvl_facet_i, table = 'CC',
            version = 1 )
        if file_exists( table_file_name ):
          remove_file( table_file_name )
      cvl_facet_i.rename( name = temp_facet_i.name, klass = temp_facet_i.klass,
          seq = temp_facet_i.seq )
      # measure beam size and store in header
      facet_i = get_facet( facets, i )
      beam_i = get_facet_beam( facet_i )
      [ fit_pos, fit_peak, fit_beam ] = ( fit_gaussian_to_peak( beam_i ) )[ 0 ]
      set_beam_size( temp_facet_i, fit_beam )
      # convolve background
      cvl_facet_i = get_facet( cvl_facets, i )
      call_aips_task( 'CONVL', indata = temp_facet_i, outdata = cvl_facet_i, 
          opcode = 'GAUS', bmaj = r_params[ 'bmaj' ], bmin = r_params[ 'bmin' ],
          bpa = r_params[ 'bpa' ], factor = 0. )
      temp_facet_i.zap()
      cvl_facet_i.rename( name = temp_facet_i.name, klass = temp_facet_i.klass,
          seq = temp_facet_i.seq )
  
  # make sure the same model version is used throughout
  if ( model_version == 0 ):
    facet = get_facet( facets, rst_facet_list[ 0 ] )
    mdl_version = facet.table_highver( 'CC' )
  else:
    mdl_version = model_version
  
  # restore model components to facets
  rst_facets = get_aips_file( facets.disk, 'RST', facets.klass, - 1, 'MA' )
  for i in rst_facet_list:
    facet_i = get_facet( facets, i )
    if ( not model_table_empty( facet_i, mdl_version ) ):
      temp_facet_i = get_facet( temp_facets, i )
      rst_facet_i = get_facet( rst_facets, i )
      call_aips_task( 'CCRES', indata = temp_facet_i, in2data = facet_i,
          invers = mdl_version, outdata = rst_facet_i, optype = 'ADD', **r_params )
      temp_facet_i.zap()
      # temporary bug fix
#      rst_facet_i.zap_table( 'CC', 0 )
      if table_exists( rst_facet_i, 'CC', 1 ):
        rst_facet_i.zap_table( 'CC', 1 )
      else:
        table_file_name = get_aips_file_name( rst_facet_i, table = 'CC',
            version = 1 )
        if file_exists( table_file_name ):
          remove_file( table_file_name )
      rst_facet_i.rename( name = temp_facet_i.name, klass = temp_facet_i.klass,
          seq = temp_facet_i.seq )
  
  # cross-restoration of model components into facet image (not into CC table !)
  if ( cross_restore and ( len( rst_facet_list ) > 1 ) ):
    for i in rst_facet_list:
      facet_i = get_facet( facets, i )
      if ( not model_table_empty( facet_i, mdl_version ) ):
        cross_facet_list = get_facet_overlap( facet_i )
        for j in cross_facet_list:
          temp_facet_j = get_facet( temp_facets, j )
          rst_facet_j = get_facet( rst_facets, j )
          call_aips_task( 'CCRES', indata = temp_facet_j, in2data = facet_i,
              invers = mdl_version, outdata = rst_facet_j, optype = 'ADD', **r_params )
          temp_facet_j.zap()
          # temporary bug fix
#          rst_facet_j.zap_table( 'CC', 1 )
          if table_exists( rst_facet_j, 'CC', 1 ):
            rst_facet_j.zap_table( 'CC', 1 )
          else:
            table_file_name = get_aips_file_name( rst_facet_j, table = 'CC',
                version = 1 )
            if file_exists( table_file_name ):
              remove_file( table_file_name )
          rst_facet_j.rename( name = temp_facet_j.name, klass = temp_facet_j.klass, 
              seq = temp_facet_j.seq )
  
  # make sure we put the right model table back at the right place
  # also restore solution tables if present
  for i in rst_facet_list:
    facet_i = get_facet( facets, i )
    temp_facet_i = get_facet( temp_facets, i )
    rst_facet_i = get_facet( rst_facets, i )
    temp_facet_i.rename( name = rst_facet_i.name, klass = rst_facet_i.klass,
        seq = rst_facet_i.seq )
    while table_exists( rst_facet_i, 'CC', 0 ):
      rst_facet_i.zap_table( 'CC', 0 )
    for i in range( 1, 1 + facet_i.table_highver( 'CC' ) ):
      if table_exists( facet_i, 'CC', i ):
        call_aips_task( 'TACOP', indata = facet_i, inext = 'CC', invers = i, 
            ncount = 1, outdata = rst_facet_i, outvers = i )
    while table_exists( rst_facet_i, 'SN', 0 ):
      rst_facet_i.zap_table( 'SN', 0 )
    for i in range( 1, 1 + facet_i.table_highver( 'SN' ) ):
      if table_exists( facet_i, 'SN', i ):
        call_aips_task( 'TACOP', indata = facet_i, inext = 'SN', ncount = 1,
            invers = i, outdata = rst_facet_i, outvers = i )
    while table_exists( rst_facet_i, 'PS', 0 ):
      rst_facet_i.zap_table( 'PS', 0 )
    for i in range( 1, 1 + facet_i.table_highver( 'PS' ) ):
      if table_exists( facet_i, 'PS', i ):
        call_aips_task( 'TACOP', indata = facet_i, inext = 'PS', ncount = 1,
            invers = i, outdata = rst_facet_i, outvers = i )
  
  return rst_facets

###############################################################################

def image_clean_pb_facets( uv, sigma = 3., improvement_limit = 0.05,
    apply_solutions = False, conversion_method = 'DFT', do_sdi_clean = False,
    restore_components = True, add_boxes = True, box_sigma = 5.,
    keep_boxes = False, frequency_correction = False, imagr_params = {},
    print_info = False, gain = 0.1, factor = 0., facet_based_noise = False ):

  # extract beam size
#  try:
#    beam = [ imagr_params[ 'bmaj' ], imagr_params[ 'bmin' ], imagr_params[ 'bpa' ] ]
#  except:
#    beam = []
  
  # filter imagr params
#  i_params = imagr_params.copy()
#  i_params[ 'bmaj' ] = - 1
#  i_params[ 'bmin' ] = 0
#  i_params[ 'bpa' ] = 0

  # image facets (but no cleaning yet)
  if frequency_correction:
    dish_diameter = restore_parameter( uv, 'dish_diameter' )
  else:
    dish_diameter = 0.
  cell_size = restore_parameter( uv, 'cell_size' )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  pb_facet_count = restore_parameter( uv, 'pb_facet_count' )
  pb_facet_size = restore_parameter( uv, 'pb_facet_size' )
  pb_facet_file_name = restore_parameter( uv, 'pb_facet_file_name' )
  channel_count = get_channel_count( uv )
  pb_facets = get_aips_file( uv.disk, 'PB', 'ICL001', -1, 'MA' )
  work_uv = get_aips_file( uv.disk, uv.name, 'IMAGR', -1, 'UV' )
  if apply_solutions:
    solution_switch = 100
    solution_version = uv.table_highver( 'SN' )
  else:
    solution_switch = 0
    solution_version = -1
  if do_sdi_clean:
    sdi_param = 0.1
  else:
    sdi_param = 0.

  # image dirty facets
  call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, gain = gain,
      docalib = solution_switch, gainuse = solution_version, overlap = 2,
      outdisk = pb_facets.disk, outname = pb_facets.name, outseq = pb_facets.seq,
      outver = 0, in2data = work_uv,  boxfile = pb_facet_file_name, flagver = -1,
      cellsize = [ cell_size, cell_size ], allokay = 0, dotv = 0,
      imsize = [ pb_facet_size, pb_facet_size ], do3dimag = 1, niter = 100000,
      flux = 100000., cmethod = conversion_method, minpatch = pb_facet_size - 1,
      imagrprm = [ dish_diameter, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, 0 ], nfield = pb_facet_count, maxpixel = 0, factor = factor,
      uvsize = [ uv_size, uv_size ], **imagr_params )

  # store parameters to facets
  store_parameter( pb_facets, 'facet_count', pb_facet_count )
  store_parameter( pb_facets, 'facet_file_name', pb_facet_file_name )

  # determine noise
  cpb_noise = measure_cpb_noise( uv, pb_facets )

  # determine facet overlap
  determine_facet_overlap( pb_facets )

  # clean facets in steps to converge to noise level
  model_version = pb_facets.table_highver( 'CC' )
  clean_i = 0
  noise_reduction = 1.
  boxes = keep_boxes
  while ( noise_reduction > improvement_limit ):
    clean_i = clean_i + 1
    last_cpb_noise = cpb_noise

    # automatically generate clean boxes
    if add_boxes:
      if ( clean_i == 1 ):
        peak_flux_ratio_max = 10.
      else:
        peak_flux_ratio_max = 100.
      add_clean_boxes( uv, pb_facets, box_sigma, keep_boxes = boxes,
          peak_flux_ratio_max = peak_flux_ratio_max,
          new_boxes_per_facet_max = 50, print_info = print_info,
          facet_based_noise = facet_based_noise )
      boxes = True

      if print_info:
        print '... cleaning down to %s mJy' % ( repr( 1.e3 * sigma * cpb_noise ) )
    
    # start/continue clean on facets
    call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, flagver = -1,
        docalib = solution_switch, gainuse = solution_version, in2data = work_uv,
        outdisk = pb_facets.disk, outname = pb_facets.name, outseq = pb_facets.seq,
        outver = model_version + 1, cellsize = [ cell_size, cell_size ],
        flux = 0.95 * sigma * cpb_noise, dotv = 0, do3dimag = 1, niter = 100000,
        imsize = [ pb_facet_size, pb_facet_size ], boxfile = pb_facet_file_name,
        cmethod = conversion_method, minpatch = pb_facet_size - 1, factor = factor,
        overlap = 2, gain = gain, nfield = pb_facet_count, allokay = 2,
        imagrprm = [ dish_diameter, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0 ], bcomp = [ 100000 for i in range( 64 ) ],
        maxpixel = 0, uvsize = [ uv_size, uv_size ], **imagr_params )
    store_parameter( pb_facets, 'clean_flux_min', sigma * cpb_noise )

    # determine noise reduction
    cpb_noise = measure_cpb_noise( uv, pb_facets )
    noise_reduction = 1. - ( cpb_noise / last_cpb_noise )
    if print_info:
      print '... relative noise reduction = %s percent' % ( 
          repr( 100. * noise_reduction ) )

    # add/remove cleaned flux from images
    if ( noise_reduction <= improvement_limit ):
      if restore_components:
        used_version = model_version
        if print_info:
          print '... restoring cleaned flux'
      else:
        for i in range( 1, pb_facet_count + 1 ):
          pb_facet_i = get_facet( pb_facets, i )
          scale_model_flux( pb_facet_i, -1. )
        used_version = 0
        if print_info:
          print '... removing cleaned flux'
      rst_facets = restore_model_components( pb_facets, imagr_params = imagr_params,
          model_version = used_version )
      for i in range( 1, pb_facet_count + 1 ):
        pb_facet_i = get_facet( pb_facets, i )
        pb_facet_i.zap()
        rst_facet_i = get_facet( rst_facets, i )
        rst_facet_i.rename( name = pb_facets.name, seq = pb_facets.seq )
        if ( not restore_components ):
          pb_facet_i.zap_table( 'CC', 0 )
    
    # combine cleaned flux with previous
    if print_info:
      print '... combining clean flux tables'
    for i in range( 1, pb_facet_count + 1 ):
      pb_facet_i = get_facet( pb_facets, i )
      if ( not model_table_empty( pb_facet_i, model_version + 1 ) ):
        combine_model_tables( pb_facet_i, new_version = model_version,
            version_list = [ model_version, model_version + 1 ],
            keep_model_tables = False )
      else:
        pb_facet_i.zap_table( 'CC', model_version + 1 )
  
  # restore model components
#  if restore_components:
#    rst_facets = restore_model_components( pb_facets, imagr_params = imagr_params )
#    for i in range( 1, pb_facet_count + 1 ):
#      pb_facet_i = get_facet( pb_facets, i )
#      pb_facet_i.zap()
#      rst_facet_i = get_facet( rst_facets, i )
#      rst_facet_i.rename( name = pb_facets.name, seq = pb_facets.seq )
  
  # remove UV workfile
  work_uv.zap()

  store_parameter( pb_facets, 'clean_flux_min', sigma * cpb_noise )

  return pb_facets

###############################################################################

def re_image_clean_facet( uv, facet, facet_file_name, clean_flux_min,
    do_sdi_clean = False, conversion_method = 'DFT', subtract = True,
    frequency_correction = False, remake_beam = True, solution_version = 0, 
    model_version = 0, imagr_params = {}, gain = 0.1, factor = 0. ):
# model_version = 0 means adding a new model table, so no merging
# if model_version does not exist means adding a new model table, so no merging

  # filter imagr params
#  i_params = imagr_params.copy()
#  i_params[ 'bmaj' ] = - 1
#  i_params[ 'bmin' ] = 0
#  i_params[ 'bpa' ] = 0

  # get parameters
  if frequency_correction:
    dish_diameter = restore_parameter( uv, 'dish_diameter' )
  else:
    dish_diameter = 0.
  i = get_facet_number( facet )
  cell_size = get_pixel_size( facet, make_absolute = True )
  facet_size = get_image_size( facet )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  channel_count = get_channel_count( uv )
  if do_sdi_clean:
    sdi_param = 0.1
  else:
    sdi_param = 0.
  if ( model_version == 0 ):
    new_model_version = facet.table_highver( 'CC' ) + 1
    merge_model_tables = False
  elif ( not table_exists( facet, 'CC', model_version ) ):
    new_model_version = model_version
    merge_model_tables = False
  else:
    new_model_version = facet.table_highver( 'CC' ) + 1
    merge_model_tables = True

  # extract facet definition
  work_facet_file_name = facet_file_name + '.%03d' % ( i )
  extract_facet_definitions( facet_file_name, [ i ], work_facet_file_name )

  # copy facet and beam
  facet_beam = get_facet_beam( facet )
  work_facet = get_aips_file( facet.disk, 'WORK', 'ICL001', - 1, 'MA' )
  work_beam = get_facet_beam( work_facet )
  if ( not remake_beam ):
    call_aips_task( 'MOVE', indata = facet, outdata = work_facet,
        userid = get_aips_userid() )
    call_aips_task( 'MOVE', indata = facet_beam, outdata = work_beam,
        userid = get_aips_userid() )

  # copy SN from facet to UV
  call_aips_task( 'TACOP', indata = facet, inext = 'SN', invers = solution_version,
      ncount = 1, outdata = uv, outvers = 0 )

  # re-image facet, while applying SN to UV
  if remake_beam:
    call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, docalib = 100,
        gainuse = 0, outdisk = work_facet.disk, outname = work_facet.name,
        outseq = work_facet.seq, outver = new_model_version, in2disk = uv.disk,
        cellsize = cell_size, imsize = facet_size, do3dimag = 1, niter = 100000,
        flux = 0.99 * clean_flux_min, boxfile = work_facet_file_name,
        cmethod = conversion_method, minpatch = facet_size[ 0 ] - 1, dotv = 0,
        overlap = 2, gain = gain, nfield = 1, allokay = 0,  maxpixel = 0,
        imagrprm = [ dish_diameter, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0 ], bcomp = [ 0 for j in range( 64 ) ], flagver = -1,
        factor = factor, uvsize = [ uv_size, uv_size ], **imagr_params )
    for table in facet.tables:
      table_version = table[ 0 ]
      table_type = ( table[ 1 ].split() )[ 1 ]
      if ( not table_type in [ 'HI', 'CC', 'CG' ] ):
        call_aips_task( 'TACOP', indata = facet, outdata = work_facet, ncount = 1,
            inext = table_type, invers = table_version, outvers = table_version )
      elif ( merge_model_tables and ( table_type == 'CC' ) and 
          ( table_version == model_version ) ):
        call_aips_task( 'TACOP', indata = facet, outdata = work_facet, ncount = 1,
            inext = table_type, invers = table_version, outvers = table_version )
  else:
    call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, docalib = 100,
        gainuse = 0, outdisk = work_facet.disk, outname = work_facet.name,
        outseq = work_facet.seq,outver = new_model_version, in2disk = uv.disk,
        cellsize = cell_size, imsize = facet_size, do3dimag = 1, niter = 100000,
        flux = 0.95 * clean_flux_min, boxfile = work_facet_file_name, gain = gain,
        cmethod = conversion_method, minpatch = facet_size[ 0 ] - 1, dotv = 0,
        overlap = 2, nfield = 1, allokay = 1, maxpixel = 0, factor = factor,
        imagrprm = [ dish_diameter, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0 ], bcomp = [ 0 for j in range( 64 ) ], flagver = -1,
        uvsize = [ uv_size, uv_size ], **imagr_params )
  remove_file( work_facet_file_name )
  uv.zap_table( 'SN', 0 )
  
  model_component_count = get_model_component_count( work_facet, new_model_version )
  model_flux = get_model_flux( work_facet, model_version = new_model_version )
  
  # remove cleaned flux from image
  if ( abs( model_flux ) > 0. ):
    temp_facet = get_aips_file( facet.disk, 'TEMP', facet.klass, -1, 'MA' )
    call_aips_task( 'CCRES', indata = work_facet, in2data = work_facet,
          invers = new_model_version, outdata = temp_facet, optype = 'SUB' )
    # temporary bug fix
#    temp_facet.zap_table( 'CC', 0 )
    if table_exists( temp_facet, 'CC', 1 ):
      temp_facet.zap_table( 'CC', 1 )
    else:
      table_file_name = get_aips_file_name( temp_facet, table = 'CC', version = 1 )
      if file_exists( table_file_name ):
        remove_file( table_file_name )
    for i in range( 1, 1 + work_facet.table_highver( 'CC' ) ):
      if table_exists( work_facet, 'CC', i ):
        call_aips_task( 'TACOP', indata = work_facet, inext = 'CC', invers = i, 
            ncount = 1, outdata = temp_facet, outvers = i )
    while table_exists( temp_facet, 'SN', 0 ):
      temp_facet.zap_table( 'SN', 0 )
    for i in range( 1, 1 + work_facet.table_highver( 'SN' ) ):
      if table_exists( work_facet, 'SN', i ):
        call_aips_task( 'TACOP', indata = work_facet, inext = 'SN', ncount = 1,
            invers = i, outdata = temp_facet, outvers = i )
    while table_exists( temp_facet, 'PS', 0 ):
      temp_facet.zap_table( 'PS', 0 )
    for i in range( 1, 1 + work_facet.table_highver( 'PS' ) ):
      if table_exists( work_facet, 'PS', i ):
        call_aips_task( 'TACOP', indata = work_facet, inext = 'PS', ncount = 1,
            invers = i, outdata = temp_facet, outvers = i )
    work_facet.zap()
    temp_facet.rename( name = work_facet.name, klass = work_facet.klass,
        seq = work_facet.seq )
  
  if ( subtract and ( abs( model_flux ) > 0. ) ) :
    sub_uv = subtract_model( uv, work_facet, facet_list = [ 1 ],
        apply_solutions = True, solution_version = solution_version,
        keep_solutions = True, model_version = new_model_version,
        conversion_method = conversion_method, flag_solutions = False,
        frequency_correction = frequency_correction )
    uv.zap()
    sub_uv.rename( name = uv.name, klass = uv.klass, seq = uv.seq )
  
  if merge_model_tables:
    if ( abs( model_flux ) > 0. ):
      combine_model_tables( work_facet, new_version = model_version,
          version_list = [ model_version, new_model_version ] )
    else:
      work_facet.zap_table( 'CC', new_model_version )

  # replace original facet and beam back with work versions
  facet.zap()
  if facet_beam.exists():
    facet_beam.zap()
  work_facet.rename( name = facet.name, klass = facet.klass, seq = facet.seq )
  work_beam.rename( name = facet_beam.name, klass = facet_beam.klass,
      seq = facet_beam.seq )

  return [ model_component_count, model_flux ]

###############################################################################

def re_image_clean_pb_facets( uv, pb_facets, facet_list = [], sigma = 5.,
    conversion_method = 'DFT', apply_solutions = False, db_contrast = 0.25,
    do_sdi_clean = False, restore_components = False, add_boxes = False,
    box_sigma = 5., center_facets = False, frequency_correction = False,
    solution_version = 0, remake_beams = True, imagr_params = {},
    print_info = False, fast_flag = False, model_version = 0,
    gain = 0.1, factor = 0., facet_based_noise = False ):
  
  # TODO: define proper flux limit for switching between facets
  
  if ( ( len( facet_list ) > 0 ) and remake_beams ):
    raise error( 'cannot remake beams for a subset of facets' )
  
  # filter imagr params
#  i_params = imagr_params.copy()
#  if ( not restore_components ):
#    i_params[ 'bmaj' ] = - 1
#    i_params[ 'bmin' ] = 0
#    i_params[ 'bpa' ] = 0
  
  # initialise some parameters
  restart = False
  if frequency_correction:
    dish_diameter = restore_parameter( uv, 'dish_diameter' )
  else:
    dish_diameter = 0.
  cell_size = restore_parameter( uv, 'cell_size' )
  cell_size = [ cell_size, cell_size ]
  facet_size = restore_parameter( uv, 'pb_facet_size' )
  facet_size = [ facet_size, facet_size ]
  channel_count = get_channel_count( uv )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  pb_facet_file_name = restore_parameter( uv, 'pb_facet_file_name' )
  if ( len( facet_list ) > 0 ):
    facet_lst = [ i for i in facet_list ]
    facet_count = len( facet_lst )
    facet_file_name = pb_facet_file_name + '.SEL'
    extract_facet_definitions( pb_facet_file_name, facet_lst, facet_file_name )
    facets = get_aips_file( pb_facets.disk, 'SEL', pb_facets.klass, -1, 'MA' )
    # rename selected facets to form consecutive series
    j = 0
    for i in facet_lst:
      j = j + 1
      pb_facet_i = get_facet( pb_facets, i )
      pb_beam_i = get_facet_beam( pb_facet_i )
      facet_j = get_facet( facets, j )
      beam_j = get_facet_beam( facet_j )
      pb_facet_i.rename( name = facet_j.name, klass = facet_j.klass, seq = facet_j.seq )
      pb_beam_i.rename( name = beam_j.name, klass = beam_j.klass, seq = beam_j.seq )
  else:
    facet_count = restore_parameter( uv, 'pb_facet_count' )
    facet_lst = range( 1, 1 + facet_count )
    facet_file_name = pb_facet_file_name
    facets = get_facet( pb_facets, 1 )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  flux = sigma * cpb_noise
  if do_sdi_clean:
    sdi_param = 0.1
  else:
    sdi_param = 0.
  mdl_version = 1
  for i in range( 1, facet_count + 1 ):
    facet_i = get_facet( facets, i )
    mdl_version = max( mdl_version, facet_i.table_highver( 'CC' ) + 1 )
  
  # re-center added facets
  if center_facets:
    re_center_pb_facets( uv, facets, facet_file_name = facet_file_name,
        imagr_params = imagr_params )
  
  if apply_solutions:
    try:
      facet_based = table_exists( facets, 'SN', solution_version )
    except:
      facet_based = False
    if facet_based: # solutions available per facet
      # flag bad solutions beforehand
      start_j = 1
      if fast_flag:
        start_j = facet_count
        try:
          added_facet_count = restore_parameter( pb_facets, 'added_facet_count' )
        except:
          added_facet_count = 0
        if ( added_facet_count > 0 ):
          orig_facet_count = facet_count - added_facet_count
          for j in range( facet_count ):
            if ( facet_lst[ j ] > orig_facet_count ):
              if ( j > 0 ):
                start_j = j
              else:
                 start_j = 1
              break
      for j in range( start_j, 1 + facet_count ):
        if ( j == 1 ):
          flag_version = -1
        else:
          flag_version = 0
        if ( j == facet_count ):
          apply_flags = True
        else:
          apply_flags = False
        facet = get_facet( facets, j )
        work_uv = flag_bad_solutions( uv, uvim = facet,
            apply_flags = apply_flags, flag_version = flag_version,
            solution_version = solution_version, keep_flags = False )

      # remove larger tables to speed up things
      while table_exists( work_uv, 'NI', 0 ):
        work_uv.zap_table( 'NI', 0 )
      while table_exists( work_uv, 'OB', 0 ):
        work_uv.zap_table( 'OB', 0 )
      while table_exists( work_uv, 'SN', 0 ):
        work_uv.zap_table( 'SN', 0 )
      while table_exists( work_uv, 'FG', 0 ):
        work_uv.zap_table( 'FG', 0 )
      
      # check for restart
      if ( ( model_version > 0 ) and table_exists( facets, 'CC', model_version ) ):
        restart = True
        temp_uv = subtract_model( work_uv, facets, apply_solutions = True,
            conversion_method = conversion_method, keep_solutions = True,
            frequency_correction = frequency_correction, flag_solutions = False,
            solution_version = solution_version, model_version = model_version )
        work_uv.zap()
        temp_uv.rename( name = work_uv.name, klass = work_uv.klass, seq = work_uv.seq )
      
      # image all facets and determine extrema
      work_facets = get_aips_file( facets.disk, 'WORK', facets.klass, -1, 'MA' )
      extremum_list = [ 0. ]
      for i in range( 1, facet_count + 1 ):
        facet_i = get_facet( facets, i )
        [ facet_components, facet_flux ] = re_image_clean_facet( work_uv, facet_i,
            facet_file_name, 100000., model_version = mdl_version,
            do_sdi_clean = do_sdi_clean, conversion_method = conversion_method,
            subtract = False, frequency_correction = frequency_correction,
            remake_beam = remake_beams, solution_version = solution_version,
            gain = gain, factor = factor, imagr_params = imagr_params )
        work_facet_i = get_facet( work_facets, i )
        call_aips_task( 'MOVE', indata = facet_i, outdata = work_facet_i,
            userid = get_aips_userid() )
        fill_facet( work_facet_i, facet_file_name = facet_file_name, invert = True,
            do_edge_circle = True )
        facet_extremum = get_image_extremum( work_facet_i )
        if ( facet_extremum == None ):
          extremum_list.append( - 100000. )
        else:
          extremum_list.append( abs( facet_extremum[ 0 ] ) )
        work_facet_i.zap()
      
      # clean and re-image facets
      total_components = 0
      total_flux = 0.
      old_i = -1
      old_clean_flux = 1.e9
      while ( max( extremum_list ) > flux ):
        # get facet with max_abs_residual
        i = extremum_list.index( max( extremum_list ) )
        facet_i = get_facet( facets, i )
        if print_info:
          print '... processing facet %s' % ( repr( i ) )
        # determine facet min clean flux level (TODO: is this correct???)
        clean_flux = float( max( [ flux, db_contrast * max( extremum_list ) ] ) )
        # check for endless loops
        if ( ( i == old_i ) and ( clean_flux == old_clean_flux ) ):
          if print_info:
            print '...... WARNING: detecting endless loop, skipping facet '
          new_i = extremum_list.index( max( extremum_list[ 0 : i ] + 
              extremum_list[ i + 1 : ] ) )
          extremum_list[ i ] = 0.99 * extremum_list[ new_i ]
          continue
        old_i = i
        old_clean_flux = clean_flux
        # re-image and clean facet
        [ facet_components, facet_flux ] = re_image_clean_facet( work_uv, facet_i,
            facet_file_name, clean_flux, model_version = mdl_version,
            do_sdi_clean = do_sdi_clean, conversion_method = conversion_method,
            subtract = True, frequency_correction = frequency_correction,
            remake_beam = False, solution_version = solution_version,
            gain = gain, factor = factor, imagr_params = imagr_params )
        total_components = total_components + facet_components
        total_flux = total_flux + facet_flux
        if print_info:
          print '... total # components = %s' % ( repr( total_components ) )
          print '... total cleaned flux = %s' % ( repr( total_flux ) )
        # determine new extremum
        work_facet_i = get_facet( work_facets, i )
        call_aips_task( 'MOVE', indata = facet_i, outdata = work_facet_i,
            userid = get_aips_userid() )
        fill_facet( work_facet_i, facet_file_name = facet_file_name, invert = True, 
            do_edge_circle = True )
        [ facet_extremum, pos ] = get_image_extremum( work_facet_i )
        extremum_list[ i ] = abs( facet_extremum )
        work_facet_i.zap()
      
      # when cleaning is done, re-image all residual facets with solutions applied
      if ( abs( total_flux ) > 0. ):
        for i in range( 1, facet_count + 1 ):
          facet_i = get_facet( facets, i )
          [ facet_components, facet_flux ] = re_image_clean_facet( work_uv, facet_i,
              facet_file_name, 100000., model_version = mdl_version,
              do_sdi_clean = do_sdi_clean, conversion_method = conversion_method,
              subtract = False, frequency_correction = frequency_correction,
              remake_beam = False, solution_version = solution_version,
              gain = gain, factor = factor, imagr_params = imagr_params )
      work_uv.zap()
      
      # on restart, merge old and new clean components
      if restart:
        for i in range( 1, facet_count + 1 ):
          facet_i = get_facet( facets, i )
          combine_model_tables( facet_i, new_version = model_version,
              version_list = [ model_version, mdl_version ] )
      
      # restore clean components to facets
      if restore_components:
        rst_facets = restore_model_components( facets, facet_list = facet_lst, 
            imagr_params = imagr_params, model_version = model_version )
        for i in facet_lst:
          facet_i = get_facet( facets, i )
          facet_i.zap()
          rst_facet_i = get_facet( rst_facets, i )
          rst_facet_i.rename( name = facet_i.name, klass = facet_i.klass,
              seq = facet_i.seq )
        
    else: # solution available for all facets
      if ( not table_exists( uv, 'SN', solution_version ) ):
        raise error( 'solution table does not exist' )

      # drop bad and missing solutions
      work_uv = flag_bad_solutions( uv, solution_version = solution_version,
          flag_version = -1, apply_flags = True )
      
      # check for restart
      if ( ( model_version > 0 ) and table_exists( facets, 'CC', model_version ) ):
        restart = True
        temp_uv = subtract_model( work_uv, facets, apply_solutions = True,
            conversion_method = conversion_method, keep_solutions = True,
            frequency_correction = frequency_correction, flag_solutions = False,
            solution_version = solution_version, model_version = model_version )
        work_uv.zap()
        temp_uv.rename( name = work_uv.name, klass = work_uv.klass, seq = work_uv.seq )
      
      if remake_beams:
        temp_facets = get_aips_file( facets.disk, 'TEMP', facets.klass, -1, 'MA' )
        call_aips_task( 'IMAGR', indata = work_uv, nchav = channel_count,
            docalib = 100, gainuse = solution_version, outdisk = temp_facets.disk,
            outname = temp_facets.name, outseq = temp_facets.seq, outver = mdl_version,
            imsize = facet_size, in2disk = uv.disk, do3dimag = 1, niter = 100000,
#            imsize = facet_size, in2disk = uv.disk, do3dimag = 0, niter = 100000,
            flux = 0.99 * flux, boxfile = facet_file_name, cmethod = conversion_method,
            minpatch = facet_size[ 0 ] - 1, allokay = 0, dotv = 0, overlap = 2,
            gain = gain, nfield = facet_count, cellsize = cell_size, flagver = -1,
            imagrprm = [ dish_diameter, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 1, 0, 0 ], maxpixel = 0, factor = factor, 
            bcomp = [ 0 for j in range( 64 ) ], uvsize = [ uv_size, uv_size ],
            **imagr_params )
        
        for i in range( 1, 1 + facet_count ):
          facet = get_facet( facets, i )
          beam = get_facet_beam( facet )
          temp_facet = get_facet( temp_facets, i )
          temp_beam = get_facet_beam( temp_facet )
          if facet.exists():
            for table in facet.tables:
              table_version = table[ 0 ]
              table_type = ( table[ 1 ].split() )[ 1 ]
              if ( not table_type in [ 'HI', 'CC', 'CG' ] ):
                call_aips_task( 'TACOP', indata = facet, outdata = temp_facet,
                    inext = table_type, invers = table_version,
                    outvers = table_version, ncount = 1 )
            facet.zap()
          if beam.exists():
            beam.zap()
          temp_facet.rename( name = facet.name, klass = facet.klass, seq = facet.seq )
          temp_beam.rename( name = beam.name, klass = beam.klass, seq = beam.seq )
      else:
        call_aips_task( 'IMAGR', indata = work_uv, nchav = channel_count,
            docalib = 100, gainuse = solution_version, outdisk = facets.disk,
            outname = facets.name, outseq = facets.seq, outver = mdl_version,
            imsize = facet_size, in2disk = uv.disk, do3dimag = 1, niter = 100000,
            flux = 0.95 * flux, boxfile = facet_file_name,
#            do3dimag = 0, niter = 100000, flux = 0.95 * flux, boxfile = facet_file_name,
            cmethod = conversion_method, minpatch = facet_size[ 0 ] - 1, 
            allokay = 1, dotv = 0, overlap = 2, gain = gain, nfield = facet_count,
            cellsize = cell_size, imagrprm = [ dish_diameter, 0, 0, sdi_param,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ], maxpixel = 0,
            factor = factor, flagver = -1, bcomp = [ 0 for j in range( 64 ) ],
            uvsize = [ uv_size, uv_size ], **imagr_params )
      work_uv.zap()
      
      # add or remove clean components from facets
      if ( not restore_components ):
        for i in facet_lst:
          facet_i = get_facet( facets, i )
          scale_model_flux( facet_i, -1., in_version = mdl_version, out_version = 0 )
        rst_facets = restore_model_components( facets, facet_list = facet_lst, 
            imagr_params = imagr_params, model_version = 0 )
        for i in facet_lst:
          rst_facet_i = get_facet( rst_facets, i )
          rst_facet_i.zap_table( 'CC', 0 )
          facet_i = get_facet( facets, i )
          facet_i.zap()
          rst_facet_i.rename( name = facet_i.name, klass = facet_i.klass,
              seq = facet_i.seq )
      elif restart:
        rst_facets = restore_model_components( facets, facet_list = facet_lst, 
            imagr_params = imagr_params, model_version = model_version )
        for i in facet_lst:
          rst_facet_i = get_facet( rst_facets, i )
          facet_i = get_facet( facets, i )
          facet_i.zap()
          rst_facet_i.rename( name = facet_i.name, klass = facet_i.klass,
              seq = facet_i.seq )
      
      # on restart, merge old and new clean components
      if restart:
        for i in range( 1, facet_count + 1 ):
          facet_i = get_facet( facets, i )
          combine_model_tables( facet_i, new_version = model_version,
              version_list = [ model_version, mdl_version ] )
  
  else: # do not apply solutions

    # check for restart
    if ( ( model_version > 0 ) and table_exists( facets, 'CC', model_version ) ):
      restart = True
      work_uv = subtract_model( work_uv, facets, apply_solutions = True,
          conversion_method = conversion_method, keep_solutions = True,
          frequency_correction = frequency_correction, flag_solutions = False,
          solution_version = solution_version, model_version = model_version )
    else:
      work_uv = uv
    
    if remake_beams:
      temp_facets = get_aips_file( facets.disk, facets.name, facets.klass, - 1, 'MA' )
      call_aips_task( 'IMAGR', indata = work_uv, nchav = channel_count, docalib = -1,
          gainuse = 0, outdisk = temp_facets.disk, outname = temp_facets.name,
          outseq = temp_facets.seq, outver = mdl_version, imsize = facet_size,
          in2disk = uv.disk, do3dimag = 1, niter = 100000, flux = 0.99 * flux,
          boxfile = facet_file_name, cmethod = conversion_method, dotv = 0,
          minpatch = facet_size[ 0 ] - 1, overlap = 2, gain = gain, maxpixel = 0,
          nfield = facet_count, allokay = 0, cellsize = cell_size, factor = factor,
          imagrprm = [ dish_diameter, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 1, 0, 0 ], bcomp = [ 0 for j in range( 64 ) ], flagver = -1,
          uvsize = [ uv_size, uv_size ], **imagr_params )
      for i in range( 1, 1 + facet_count ):
        facet = get_facet( facets, i )
        beam = get_facet_beam( facet )
        temp_facet = get_facet( temp_facets, i )
        temp_beam = get_facet_beam( temp_facet )
        if facet.exists():
          for table in facet.tables:
            table_version = table[ 0 ]
            table_type = ( table[ 1 ].split() )[ 1 ]
            if ( not table_type in [ 'HI', 'CC', 'CG' ] ):
#            if True:
              call_aips_task( 'TACOP', indata = facet, outdata = temp_facet,
                  inext = table_type, invers = table_version,
                  outvers = table_version, ncount = 1 )
          facet.zap()
        if beam.exists():
          beam.zap()
        temp_facet.rename( name = facet.name, klass = facet.klass, seq = facet.seq )
        temp_beam.rename( name = beam.name, klass = beam.klass, seq = beam.seq )
    else:
      call_aips_task( 'IMAGR', indata = work_uv, nchav = channel_count, docalib = -1,
          gainuse = 0, outdisk = facets.disk, outname = facets.name, flagver = -1,
          outseq = facets.seq, outver = mdl_version, imsize = facet_size, 
          in2disk = uv.disk, do3dimag = 1, niter = 100000, flux = 0.95 * flux,
          boxfile = facet_file_name, cmethod = conversion_method, dotv = 0,
          minpatch = facet_size[ 0 ] - 1, overlap = 2, gain = gain, allokay = 1,
          nfield = facet_count, imagrprm = [ dish_diameter, 0, 0, sdi_param,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ], maxpixel = 0,
          factor = factor, cellsize = cell_size, bcomp = [ 0 for j in range( 64 ) ],
          uvsize = [ uv_size, uv_size ], **imagr_params )
    if ( work_uv != uv ):
      work_uv.zap()
    
    # add or remove clean components from facets
    if ( not restore_components ):
      for i in facet_lst:
        facet_i = get_facet( facets, i )
        scale_model_flux( facet_i, -1., in_version = mdl_version, out_version = 0 )
      rst_facets = restore_model_components( facets, facet_list = facet_lst, 
          imagr_params = imagr_params, model_version = 0 )
      for i in facet_lst:
        rst_facet_i = get_facet( rst_facets, i )
        rst_facet_i.zap_table( 'CC', 0 )
        facet_i = get_facet( facets, i )
        facet_i.zap()
        rst_facet_i.rename( name = facet_i.name, klass = facet_i.klass,
            seq = facet_i.seq )
    elif restart:
      rst_facets = restore_model_components( facets, facet_list = facet_lst, 
          imagr_params = imagr_params, model_version = model_version )
      for i in facet_lst:
        rst_facet_i = get_facet( rst_facets, i )
        facet_i = get_facet( facets, i )
        facet_i.zap()
        rst_facet_i.rename( name = facet_i.name, klass = facet_i.klass,
            seq = facet_i.seq )
    
    # on restart, merge old and new clean components
    if restart:
      for i in range( 1, facet_count + 1 ):
        facet_i = get_facet( facets, i )
        combine_model_tables( facet_i, new_version = model_version,
            version_list = [ model_version, mdl_version ] )
  
  store_parameter( pb_facets, 'clean_flux_min', flux )
  
  # if needed, rename facets back to original
  for i in facet_list:
    facet_i = get_facet( facets, i )
    beam_i = get_facet_beam( facet_i )
    pb_facet_i = get_facet( pb_facets, i )
    pb_beam_i = get_facet_beam( pb_facet_i )
    facet_i.rename( name = pb_facet_i.name, klass = pb_facet_i.klass,
        seq = pb_facet_i.seq )
    beam_i.rename( name = pb_beam_i.name, klass = pb_beam_i.klass,
        seq = pb_beam_i.seq )
    remove_file( facet_file_name )
  
  # determine noise level
  cpb_noise = measure_cpb_noise( uv, pb_facets )
  
  # automatically add new clean boxes for next round of cleaning
  if add_boxes:
    keep_boxes = ( not restore_components )
    add_clean_boxes( uv, pb_facets, box_sigma, keep_boxes = keep_boxes,
        print_info = print_info, facet_based_noise = facet_based_noise )
  
  return

###############################################################################

def combine_pb_facets( uv, pb_facets, facet_list = [], restore_components = True,
    blank_edge = 8, save_tables = True, save_info = True, imagr_params = {},
    edge_weight = 0. ):
  
  # smooth overlapping facets
  if ( ( edge_weight == None ) or ( edge_weight >= 1. ) ):
    weightit = 0
  else:
    facet_size = get_image_size( pb_facets )
    pixel_size = get_pixel_size( pb_facets )
    xc = float( facet_size[ 0 ] - 1 ) / 2.
    yc = float( facet_size[ 1 ] - 1 ) / 2.
    rc = min( [ ( xc - float( blank_edge ) ) * pixel_size[ 0 ],
                ( yc - float( blank_edge ) ) * pixel_size[ 1 ] ] )
    weightit = ( 1. - max( [ 0.0001, edge_weight ] ) ) / rc

  # initialise some parameters
  pb_image_size = restore_parameter( uv, 'pb_image_size' )
  model_version = pb_facets.table_highver( 'CC' )
  if ( len( facet_list ) > 0 ):
    facet_count = len( facet_list )
    facets = get_aips_file( pb_facets.disk, 'SEL', pb_facets.klass, -1, 'MA' )
    # rename selected facets to form consecutive series
    for i in facet_list:
      facet_i = get_facet( pb_facets, i )
      facet_i.rename( name = facets.name, 
          klass = facets.klass[ 0 : 3 ] + '%03d' % ( facet_list.index( i ) + 1 ),
          seq = facets.seq )
  else:
    facet_count = restore_parameter( pb_facets, 'facet_count' )
    facets = get_facet( pb_facets, 1 )

  # restore clean components to facets
  temp_facets = get_aips_file( facets.disk, 'TEMP', facets.klass, -1, 'MA' )
  if restore_components:
    rst_facets = restore_model_components( pb_facets, imagr_params = imagr_params )
    for i in range( 1, facet_count + 1 ):
      temp_facet_i = get_facet( temp_facets, i )
      rst_facet_i = get_facet( rst_facets, i )
      rst_facet_i.rename( name = temp_facet_i.name, klass = temp_facet_i.klass,
          seq = temp_facet_i.seq )
  else:
    for i in range( 1, facet_count + 1 ):
      temp_facet_i = get_facet( temp_facets, i )
      facet_i = get_facet( facets, i )
      call_aips_task( 'MOVE', indata = facet_i, outdata = temp_facet_i,
          userid = get_aips_userid() )

  # blank circular edge area of facet
  # get total clean flux
  total_clean_flux = 0.
  for i in range( 1, facet_count + 1 ):
    temp_facet_i = get_facet( temp_facets, i )
    fill_facet( temp_facet_i, do_edge_circle = True, blank_edge = blank_edge )
    total_clean_flux = total_clean_flux + get_model_flux( temp_facet_i )

  # combine residual facets into image
  pb_image = get_aips_file( pb_facets.disk, pb_facets.name, 'FLATN', -1, 'MA' )
  coordinates = degdeg_to_hmsdms( get_radec( temp_facets ) )
  call_aips_task( 'FLATN', indata = temp_facets, nfield = facet_count,
      outdata = pb_image, imsize = [ pb_image_size, pb_image_size ],
      edgskp = blank_edge, weightit = weightit, reweight = [ 3, 0.5 ],
      coordina = coordinates )

  # save model tables and solution tables to image
  if save_tables:
    for i in range( 1, facet_count + 1 ):
      temp_facet_i = get_facet( temp_facets, i )
      if table_exists( temp_facet_i, 'CC', 0 ):
        call_aips_task( 'TACOP', indata = temp_facet_i, outdata = pb_image,
            inext = 'CC', invers = 0, ncount = 1, outvers = 0 )
      if table_exists( temp_facet_i, 'SN', 0 ):
        call_aips_task( 'TACOP', indata = temp_facet_i, outdata = pb_image,
            inext = 'SN', invers = 0, ncount = 1, outvers = 0 )

  # clean up temporary facets
  for i in range( 1, facet_count + 1 ):
    temp_facet_i = get_facet( temp_facets, i )
    temp_facet_i.zap()

  # if needed, rename facets back to original
  if ( len( facet_list ) > 0 ):
    for i in facet_list:
      facet_i = get_aips_file( facets.disk, facets.name, 
          facets.klass[ 0 : 3 ] + '%03d' % ( facet_list.index( i ) + 1 ),
          facets.seq, 'MA' )
      facet_i.rename( name = pb_facets.name, seq = pb_facets.seq,
          klass = pb_facets.klass[ 0 : 3 ] + '%03d' % ( i ) )

  if save_info:
    try:
      clean_box_flux_min = restore_parameter( pb_facets, 'clean_box_flux_min' )
    except:
      pass
    else:
      store_parameter( pb_image, 'clean_box_flux_min', clean_box_flux_min )
    try:
      clean_flux_min = restore_parameter( pb_facets, 'clean_flux_min' )
    except:
      pass
    else:
      store_parameter( pb_image, 'clean_flux_min', clean_flux_min )
    cpb_noise = restore_parameter( uv, 'cpb_noise' )
    store_parameter( pb_image, 'residual_rms', cpb_noise )
    store_parameter( pb_image, 'total_clean_flux', total_clean_flux )

  return pb_image

###############################################################################

def selfcal_image_clean_pb( uv, pb_facets, sigma = 5., signal_to_noise = 10.,
    conversion_method = 'DFT', reference_antenna = 0, improvement_limit = 0.02,
    try_final_amplitude = False, imagr_params = {}, do_sdi_clean = False,
    keep_failed = True, selfcal_cycle_min = 2, restore_components = False,
    amplitude_factor = 10., center_facets = False, add_boxes = False, box_sigma = 5.,
    frequency_correction = False, calib_params = {}, remake_beams = False,
    phase_interval_min = 0., amplitude_interval = 5., snr_limit = 2.5,
    print_info = False, facet_based_noise = False ):
  
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  noise_improvement = 1.
  selfcal_i = 0
#  remake = remake_beams
  while ( selfcal_i < selfcal_cycle_min ) or ( noise_improvement > improvement_limit ):
    selfcal_i = selfcal_i + 1
    last_cpb_noise = cpb_noise

    # selfcal and image pb
    calibrate_pb( uv, pb_facets, sigma = 0., signal_to_noise = signal_to_noise,
        reference_antenna = reference_antenna, conversion_method = conversion_method,
        calib_params = calib_params, phase_interval_min = phase_interval_min,
        snr_limit = snr_limit, print_info = print_info )
    solution_version = uv.table_highver( 'SN' )
    re_image_clean_pb_facets( uv, pb_facets, sigma = sigma, apply_solutions = True,
        add_boxes = add_boxes, box_sigma = box_sigma, do_sdi_clean = do_sdi_clean,
        conversion_method = conversion_method, center_facets = center_facets,
        frequency_correction = frequency_correction, remake_beams = remake_beams,
        restore_components = restore_components, imagr_params = imagr_params,
        print_info = print_info, facet_based_noise = facet_based_noise )
#    remake = False

    # determine noise reduction
    cpb_noise = restore_parameter( uv, 'cpb_noise' )
    noise_improvement = 1. - ( cpb_noise / last_cpb_noise )

    # only keep solutions when noise improvement was detected
    if ( selfcal_i == 1 ):
      last_solution_version = solution_version
    elif ( noise_improvement >= 0. ):
      uv.zap_table( 'SN', last_solution_version )
      last_solution_version = solution_version
    else:
      uv.zap_table( 'SN', solution_version )
      solution_version = last_solution_version
      cpb_noise = last_cpb_noise
      store_parameter( uv, 'cpb_noise', cpb_noise )

  if try_final_amplitude:
    selfcal_i = selfcal_i + 1
    last_cpb_noise = cpb_noise

    # apply phase solutions
    temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )
    call_aips_task( 'SPLIT', indata = uv, docalib = 100, gainuse = solution_version,
        douvcomp = 0, flagver = - 1, outdisk = temp_uv.disk, outclass = temp_uv.klass,
        outseq = temp_uv.seq )

    # selfcal and image pb
    calibrate_pb( temp_uv, pb_facets, sigma = 0., 
        signal_to_noise = amplitude_factor * signal_to_noise, do_amplitude = True,
        reference_antenna = reference_antenna, conversion_method = conversion_method,
        calib_params = calib_params, amplitude_interval = amplitude_interval,
        snr_limit = snr_limit, print_info = print_info )
    solution_version = temp_uv.table_highver( 'SN' )
    re_image_clean_pb_facets( temp_uv, pb_facets, sigma = sigma, apply_solutions = True,
        add_boxes = False, do_sdi_clean = do_sdi_clean, center_facets = False,
        conversion_method = conversion_method, imagr_params = imagr_params,
        frequency_correction = frequency_correction, remake_beams = remake_beams,
        restore_components = restore_components, box_sigma = False,
        print_info = print_info )
    
    # determine noise reduction
    cpb_noise = restore_parameter( temp_uv, 'cpb_noise' )
    noise_improvement = 1. - ( cpb_noise / last_cpb_noise )
    
    # only keep solutions when noise improvement was detected
    if ( noise_improvement >= 0. ):
      # combine amplitude solutions with phase solutions
      call_aips_task( 'TACOP', indata = temp_uv, inext = 'SN', ncount = 1,
          invers = solution_version, outdata = uv )
      solution_version = uv.table_highver( 'SN' )
      combine_solutions( uv, in_version_1 = last_solution_version,
           in_version_2 = solution_version )
      uv.zap_table( 'SN', last_solution_version )
      uv.zap_table( 'SN', solution_version )
      solution_version = uv.table_highver( 'SN' )
    else:
      uv.zap_table( 'SN', solution_version )
      solution_version = last_solution_version
      cpb_noise = last_cpb_noise
      store_parameter( uv, 'cpb_noise', cpb_noise )

    # clean up
    temp_uv.zap()

  if ( noise_improvement < -improvement_limit ):
    re_image_clean_pb_facets( uv, pb_facets, sigma = sigma, apply_solutions = True,
        add_boxes = False, do_sdi_clean = do_sdi_clean, center_facets = False,
        conversion_method = conversion_method, imagr_params = imagr_params,
        frequency_correction = frequency_correction, remake_beams = remake_beams,
        restore_components = restore_components, box_sigma = False,
        print_info = print_info )
  
  return

###############################################################################

def subtract_model_old( uv, facets, facet_list = [], sigma = 0., apply_solutions = False, 
    keep_solutions = False, flux_scale = 1., conversion_method = 'DFT', model_version = 0,
    frequency_correction = False ):
# for adding a model, use add_model() instead of flux_scale = - 1
# TODO: add solution_version keyword

  # initialise some parameters
  if ( len( facet_list ) > 0 ):
    sub_facet_count = len( facet_list )
    sub_facet_list = [ i for i in facet_list ]
  else:
    sub_facet_count = restore_parameter( facets, 'facet_count' )
    sub_facet_list = range( 1, sub_facet_count + 1 )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  flux = sigma * cpb_noise
  if frequency_correction:
    dish_diameter = restore_parameter( uv, 'dish_diameter' )

  if ( model_version == 0 ):
    sub_facet_i = get_facet( facets, sub_facet_list[ 0 ] )
    sub_model_version = sub_facet_i.table_highver( 'CC' )
  else:
    sub_model_version = model_version

  # check model component tables
  check_facet_list = [ i for i in sub_facet_list ]
  for i in check_facet_list:
    sub_facet_i = get_facet( facets, i )
    if ( not table_exists( sub_facet_i, 'CC', sub_model_version ) ):
      raise error( 'version numbers of model component tables are inconsistent' )
    if model_table_empty( sub_facet_i, sub_model_version ):
      sub_facet_list.remove( i )
  sub_facet_count = len( sub_facet_list )

  # check for empty facet list
  if ( sub_facet_count == 0 ):

    # just make copy of input UV
    sub_uv = get_aips_file( uv.disk, uv.name, 'SUB', - 1, 'UV' )
    call_aips_task( 'MOVE', indata = uv, outdata = sub_uv, userid = get_aips_userid() )

    # remove SN tables when requested
    if ( not keep_solutions ):
      while table_exists( sub_uv, 'SN', 0 ):
        sub_uv.zap_table( 'SN', 0 )

  else: # ( sub_facet_count > 0 )

    # copy and rename selected facets to form consecutive series
    # merge clean components to speed up things
    sub_facets = get_aips_file( facets.disk, facets.name, 'SUB001', - 1, 'MA' )
    for i in range( 1, 1 + sub_facet_count ):
      facet = get_facet( facets, sub_facet_list[ i - 1 ] )
      sub_facet = get_facet( sub_facets, i )
      call_aips_task( 'MOVE', indata = facet, outdata = sub_facet, userid = get_aips_userid() )
      call_aips_task( 'CCMRG', indata = sub_facet, invers = sub_model_version, outvers = 0 )
    sub_model_version = sub_facets.table_highver( 'CC' )

    if ( flux_scale != 1. ):
      for i in range( 1, 1 + sub_facet_count ):
        sub_facet = get_facet( sub_facets, i )
        scale_model_flux( sub_facet, flux_scale, in_version = sub_model_version, out_version = 0 )
      sub_model_version = sub_facets.table_highver( 'CC' )

    # handle different solution possibilities
    solution_switch = 0
    if apply_solutions:
      if table_exists( sub_facets, 'SN', 0 ):
        solution_switch = 2
        solution_version = sub_facets.table_highver( 'SN' )
        for i in range( 1, sub_facet_count + 1 ):
          sub_facet_i = get_facet( sub_facets, i )
          next_solution_version = sub_facet_i.table_highver( 'SN' )
          if ( next_solution_version != solution_version ):
            raise error( 'version numbers of solution tables are inconsistent' )
      elif table_exists( uv, 'SN', 0 ):
        solution_switch = 1
        solution_version = uv.table_highver( 'SN' )
      else:
        raise error( 'no solution table found to apply' )

    # apply no solution
    if ( solution_switch == 0 ):

      # subtract multiple facet model
      sub_uv = get_aips_file( uv.disk, uv.name, 'SUB', -1, 'UV' )
      if frequency_correction:
        call_aips_task( 'OOSUB', indata = uv, in2data = sub_facets, invers = model_version,
            nmaps = sub_facet_count, outdata = sub_uv, flux = flux, cmodel = 'COMP',
            cmethod = conversion_method, bparm = [ dish_diameter, 0, 0 ], factor = 1. )
      else:
        call_aips_task( 'UVSUB', indata = uv, in2data = sub_facets, invers = model_version,
            nmaps = sub_facet_count, outdata = sub_uv, flux = flux, cmodel = 'COMP',
            cmethod = conversion_method, factor = 1. )

    # apply single solution for all facets
    elif ( solution_switch == 1 ):

      # apply SN
      cal_uv = get_aips_file( uv.disk, uv.name, 'CAL', -1, 'UV' )
      call_aips_task( 'SPLIT', indata = uv, docalib = 100, gainuse = solution_version, douvcomp = 0,
          flagver = - 1, outdisk = cal_uv.disk, outclass = cal_uv.klass, outseq = cal_uv.seq )

      # subtract multiple facet model
      sub_cal_uv = get_aips_file( uv.disk, uv.name, 'SUBCAL', -1, 'UV' )
      if frequency_correction:
        call_aips_task( 'OOSUB', indata = cal_uv, in2data = sub_facets, invers = model_version,
            nmaps = sub_facet_count, outdata = sub_cal_uv, flux = flux, cmodel = 'COMP',
            cmethod = conversion_method, bparm = [ dish_diameter, 0, 0 ], factor = 1. )
      else:
        call_aips_task( 'UVSUB', indata = cal_uv, in2data = sub_facets, invers = model_version,
            nmaps = sub_facet_count, outdata = sub_cal_uv, flux = flux, cmodel = 'COMP',
            cmethod = conversion_method, factor = 1. )
      cal_uv.zap()

      # copy inverted SN to UV
      call_aips_task( 'CLINV', indata = uv, inext = 'SN', invers = solution_version,
          outdata = sub_cal_uv, outvers = 0 )
#      snver = sub_cal_uv.table_highver( 'SN' ) + 1
#      call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = solution_version,
#          outdata = sub_cal_uv, outvers = snver )
#      call_aips_task( 'SNCOR', indata = sub_cal_uv, snver = snver, opcode = 'PHNEG' )

      # apply inverted SN
      sub_uv = get_aips_file( uv.disk, uv.name, 'SUB', -1, 'UV' )
      call_aips_task( 'SPLIT', indata = sub_cal_uv, docalib = 100, gainuse = 0, douvcomp = 0,
          flagver = - 1, outdisk = sub_uv.disk, outclass = sub_uv.klass, outseq = sub_uv.seq )
      sub_cal_uv.zap()

    # apply solution per facet
    else: # ( solution_switch == 2 )

      # make work copy of UV data
      sub_uv = get_aips_file( uv.disk, uv.name, 'SUB', - 1, 'UV' )
      call_aips_task( 'MOVE', indata = uv, outdata = sub_uv, userid = get_aips_userid() )

      # cycle over facets
      for i in range( 1, sub_facet_count + 1 ):
        sub_facet_i = get_facet( sub_facets, i )

        # copy SN from facet to UV
        call_aips_task( 'TACOP', indata = sub_facet_i, inext = 'SN', invers = solution_version,
            ncount = 1, outdata = sub_uv, outvers = 0 )

        # apply SN
        cal_uv = get_aips_file( uv.disk, uv.name, 'CAL', -1, 'UV' )
        call_aips_task( 'SPLIT', indata = sub_uv, docalib = 100, gainuse = 0, douvcomp = 0,
            flagver = - 1, outdisk = cal_uv.disk, outclass = cal_uv.klass, outseq = cal_uv.seq )

        # subtract single facet model
        sub_cal_uv = get_aips_file( uv.disk, uv.name, 'SUBCAL', -1, 'UV' )
        if frequency_correction:
          call_aips_task( 'OOSUB', indata = cal_uv, in2data = sub_facet_i, invers = model_version,
              nmaps = 1, outdata = sub_cal_uv, flux = flux, cmodel = 'COMP',
              cmethod = conversion_method, bparm = [ dish_diameter, 0, 0 ], factor = 1. )
        else:
          call_aips_task( 'UVSUB', indata = cal_uv, in2data = sub_facet_i, invers = model_version,
              nmaps = 1, outdata = sub_cal_uv, flux = flux, cmodel = 'COMP',
              cmethod = conversion_method, factor = 1. )
        cal_uv.zap()

        # copy inverted SN to UV
        call_aips_task( 'CLINV', indata = sub_uv, inext = 'SN', invers = 0,
            outdata = sub_cal_uv, outvers = 0 )
#        snver = sub_cal_uv.table_highver( 'SN' ) + 1
#        call_aips_task( 'TACOP', indata = sub_uv, inext = 'SN', invers = 0,
#            outdata = sub_cal_uv, outvers = snver )
#        call_aips_task( 'SNCOR', indata = sub_cal_uv, snver = snver, opcode = 'PHNEG' )
#        sub_uv.zap()

        # apply inverted SN
        call_aips_task( 'SPLIT', indata = sub_cal_uv, docalib = 100, gainuse = 0, douvcomp = 0,
            flagver = - 1, outdisk = sub_uv.disk, outclass = sub_uv.klass, outseq = sub_uv.seq )
        sub_cal_uv.zap()

    # remove facet copies
    for i in range( 1, 1 + sub_facet_count ):
      sub_facet = get_facet( sub_facets, i )
      sub_facet.zap()

    # copy SN table when requested
    if ( keep_solutions and table_exists( uv, 'SN', 0 ) ):
      solution_version = uv.table_highver( 'SN' )
      call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = solution_version, ncount = 1,
          outdata = sub_uv, outvers = 0 )

  return sub_uv

###############################################################################

def subtract_model_old2( uv, facets, facet_list = [], sigma = 0., flux_scale = 1.,
    apply_solutions = False, keep_solutions = False, conversion_method = 'DFT',
    model_version = 0, solution_version = 0, frequency_correction = False,
    replace = False, flag_solutions = True ):
# for adding a model, use add_model() instead of flux_scale = - 1

  # initialise some parameters
  if ( len( facet_list ) > 0 ):
    sub_facet_count = len( facet_list )
    sub_facet_list = [ i for i in facet_list ]
  else:
    try:
      sub_facet_count = restore_parameter( facets, 'facet_count' )
    except:
      sub_facet_count = 1
    sub_facet_list = range( 1, sub_facet_count + 1 )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  flux = sigma * cpb_noise
  if frequency_correction:
    dish_diameter = restore_parameter( uv, 'dish_diameter' )
  sub_model_version = model_version
  
  if replace:
    opcode = 'MODL'
  else:
    opcode = ''
  
  # check model component tables
  check_facet_list = [ i for i in sub_facet_list ]
  for i in check_facet_list:
    sub_facet_i = get_facet( facets, i )
    if model_table_empty( sub_facet_i, sub_model_version ):
      sub_facet_list.remove( i )
  sub_facet_count = len( sub_facet_list )
  
  # check for empty facet list
  sub_uv = get_aips_file( uv.disk, uv.name, 'SUB', - 1, 'UV' )
  if ( sub_facet_count == 0 ):
  
    # just make copy of input UV
    call_aips_task( 'MOVE', indata = uv, outdata = sub_uv, userid = get_aips_userid() )
  
  else: # ( sub_facet_count > 0 )
    
    # copy and rename selected facets to form consecutive series
    # merge clean components to speed up things
    sub_facets = get_aips_file( facets.disk, facets.name, 'SUB001', - 1, 'MA' )
    for i in range( 1, 1 + sub_facet_count ):
      facet = get_facet( facets, sub_facet_list[ i - 1 ] )
      sub_facet = get_facet( sub_facets, i )
      call_aips_task( 'MOVE', indata = facet, outdata = sub_facet,
          userid = get_aips_userid() )
      call_aips_task( 'CCMRG', indata = sub_facet, invers = sub_model_version,
          outvers = 0 )
    sub_model_version = sub_facets.table_highver( 'CC' )
    
    if ( flux_scale != 1. ):
      for i in range( 1, 1 + sub_facet_count ):
        sub_facet = get_facet( sub_facets, i )
        scale_model_flux( sub_facet, flux_scale, in_version = sub_model_version,
            out_version = 0 )
      sub_model_version = sub_facets.table_highver( 'CC' )
    
    # handle different solution possibilities
    sol_switch = 0
    if apply_solutions:
      if table_exists( sub_facets, 'SN', solution_version ):
        sol_switch = 2
        sol_version = solution_version
      elif table_exists( uv, 'SN', 0 ):
        sol_switch = 1
        if ( solution_version == 0 ):
          sol_version = uv.table_highver( 'SN' )
        else:
          sol_version = solution_version
      else:
        raise error( 'no solution table found to apply' )
    
    # apply no solution
    if ( sol_switch == 0 ):
      
      # subtract multiple facet model
      if frequency_correction:
        call_aips_task( 'OOSUB', indata = uv, in2data = sub_facets,
            invers = model_version, nmaps = sub_facet_count, outdata = sub_uv,
            flux = flux, cmodel = 'COMP', cmethod = conversion_method,
            bparm = [ dish_diameter, 0, 0 ], opcode = opcode, factor = 1. )
      else:
        call_aips_task( 'UVSUB', indata = uv, in2data = sub_facets,
            invers = model_version, nmaps = sub_facet_count, outdata = sub_uv,
            flux = flux, cmodel = 'COMP', cmethod = conversion_method,
            opcode = opcode, factor = 1. )
    
    # apply single solution for all facets
    elif ( sol_switch == 1 ):
      
      # flag bad solutions (important for DIFUV)
      if flag_solutions:
         flag_uv = flag_bad_solutions( uv, solution_version = sol_version,
             flag_version = -1, apply_flags = True )
      else:
        flag_uv = uv
      
      # generate model UV data set
      model_uv = get_aips_file( uv.disk, uv.name, 'MODEL', - 1, 'UV' )
      if frequency_correction:
        call_aips_task( 'OOSUB', indata = flag_uv, in2data = sub_facets,
            invers = model_version, nmaps = sub_facet_count, outdata = model_uv,
            flux = flux, cmodel = 'COMP', cmethod = conversion_method,
            bparm = [ dish_diameter, 0, 0 ], opcode = 'MODL', factor = 1. )
      else:
        call_aips_task( 'UVSUB', indata = flag_uv, in2data = sub_facets,
            invers = model_version, nmaps = sub_facet_count, outdata = model_uv,
            flux = flux, cmodel = 'COMP', cmethod = conversion_method,
            opcode = 'MODL', factor = 1. )
      
      # apply inverted SN to model UV
      call_aips_task( 'CLINV', indata = uv, inext = 'SN', invers = sol_version,
          outdata = model_uv, outvers = 0 )
#      snver = model_uv.table_highver( 'SN' ) + 1
#      call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = sol_version,
#          outdata = model_uv, outvers = snver )
#      call_aips_task( 'SNCOR', indata = model_uv, snver = snver, opcode = 'PHNEG' )
      cal_model_uv = get_aips_file( uv.disk, uv.name, 'CALMDL', - 1, 'UV' )
      call_aips_task( 'SPLIT', indata = model_uv, docalib = 100, gainuse = 0,
          douvcomp = 0, flagver = - 1, outdisk = cal_model_uv.disk,
          outclass = cal_model_uv.klass, outseq = cal_model_uv.seq )
      model_uv.zap()
      
      if replace:
        # rename
        cal_model_uv.rename( name = sub_uv.name, klass = sub_uv.klass,
            seq = sub_uv.seq )
      
      else:
        # subtract model UV from UV
        # note that DIFUV adds weights
        temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )
        call_aips_task( 'DIFUV', indata = flag_uv, in2data = cal_model_uv,
            outdata = temp_uv, solint = 0 )
        cal_model_uv.zap()

        # adjust weights to original value
        call_aips_task( 'WTMOD', indata = temp_uv, outdata = sub_uv,
            aparm = [ 2., 0., 0. ] )
        temp_uv.zap()

      if flag_solutions:
        flag_uv.zap()

    # apply solution per facet
    else: # ( sol_switch == 2 )
      
      # define some names
      model_uv = get_aips_file( uv.disk, uv.name, 'MODEL', - 1, 'UV' )
      cal_model_uv = get_aips_file( uv.disk, uv.name, 'CALMDL', - 1, 'UV' )
      temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )

      # flag bad solutions (important for DIFUV)
      if flag_solutions:
        flag_bad_solutions( uv, uvim = sub_facets, solution_version = sol_version,
            flag_version = -1 )
        for i in range( 2, sub_facet_count ):
          sub_facet = get_facet( sub_facets, i )
          flag_bad_solutions( uv, uvim = sub_facet, solution_version = sol_version )
        sub_facet = get_facet( sub_facets, sub_facet_count )
        flag_uv = flag_bad_solutions( uv, uvim = sub_facet,
            solution_version = sol_version, apply_flags = True )
      else:
        flag_uv = get_aips_file( uv.disk, uv.name, 'FLAG', -1, 'UV' )
        call_aips_task( 'MOVE', indata = uv, outdata = flag_uv,
            userid = get_aips_userid() )
      
      # remove larger tables to speed up things
      while table_exists( flag_uv, 'NI', 0 ):
        flag_uv.zap_table( 'NI', 0 )
      while table_exists( flag_uv, 'OB', 0 ):
        flag_uv.zap_table( 'OB', 0 )
      while table_exists( flag_uv, 'SN', 0 ):
        flag_uv.zap_table( 'SN', 0 )
      
      # cycle over facets
      for i in range( 1, sub_facet_count + 1 ):
        sub_facet = get_facet( sub_facets, i )

        # generate model UV data set
        if frequency_correction:
          call_aips_task( 'OOSUB', indata = flag_uv, in2data = sub_facet,
              invers = model_version, nmaps = 1, outdata = model_uv, flux = flux,
              cmodel = 'COMP', cmethod = conversion_method, opcode = 'MODL',
              bparm = [ dish_diameter, 0, 0 ], factor = 1. )
        else:
          call_aips_task( 'UVSUB', indata = flag_uv, in2data = sub_facet,
              invers = model_version, nmaps = 1, outdata = model_uv, flux = flux,
              cmodel = 'COMP', cmethod = conversion_method, opcode = 'MODL', factor = 1. )
        
        # apply inverted SN to model UV
        sol2_version = model_uv.table_highver( 'SN' ) + 1
        call_aips_task( 'TACOP', indata = sub_facet, inext = 'SN', ncount = 1,
            invers = sol_version, outdata = model_uv, outvers = sol2_version )
        call_aips_task( 'CLINV', indata = model_uv, inext = 'SN',
            invers = sol2_version, outdata = model_uv, outvers = sol2_version + 1 )
#        call_aips_task( 'SNCOR', indata = model_uv, snver = sol2_version,
#            opcode = 'PHNEG' )
        call_aips_task( 'SPLIT', indata = model_uv, docalib = 100, flagver = -1,
            gainuse = 0, douvcomp = 0, outdisk = cal_model_uv.disk,
            outclass = cal_model_uv.klass, outseq = cal_model_uv.seq )
        model_uv.zap()
        
        if ( replace and ( i == 1 ) ):
          flag_uv.zap()
          cal_model_uv.rename( name = flag_uv.name, klass = flag_uv.klass,
              seq = flag_uv.seq )
          cal_model_uv = get_aips_file( uv.disk, uv.name, 'CALMDL', -1, 'UV' )
        else:
          # subtract model UV from UV
          call_aips_task( 'DIFUV', indata = flag_uv, in2data = cal_model_uv,
              outdata = temp_uv, solint = 0 ) 
          flag_uv.zap()
          cal_model_uv.zap()
          
          # adjust weights to original value
          call_aips_task( 'WTMOD', indata = temp_uv, outdata = flag_uv,
              aparm = [ 2., 0., 0. ] )
          temp_uv.zap()

      # copy back large tables (SN tables are handled below)
      flag_uv.rename( name = sub_uv.name, klass = sub_uv.klass, seq = sub_uv.seq )
      for version in range( 1, 1 + uv.table_highver( 'NI' ) ):
        if table_exists( uv, 'NI', version ):
          call_aips_task( 'TACOP', indata = uv, inext = 'NI', ncount = 1,
              invers = version, outdata = sub_uv, outvers = version )
      for version in range( 1, 1 + uv.table_highver( 'OB' ) ):
        if table_exists( uv, 'OB', version ):
          call_aips_task( 'TACOP', indata = uv, inext = 'OB', ncount = 1,
              invers = version, outdata = sub_uv, outvers = version )
    
    # remove facet copies
    for i in range( 1, 1 + sub_facet_count ):
      sub_facet = get_facet( sub_facets, i )
      sub_facet.zap()
  
  # copy or remove SN tables when requested
  if ( keep_solutions and ( not table_exists( sub_uv, 'SN', 0 ) ) ):
    for version in range( 1, 1 + uv.table_highver( 'SN' ) ):
      if table_exists( uv, 'SN', version ):
        call_aips_task( 'TACOP', indata = uv, inext = 'SN', ncount = 1,
            invers = version, outdata = sub_uv, outvers = version )
  if ( ( not keep_solutions ) and table_exists( sub_uv, 'SN', 0 ) ):
    while table_exists( sub_uv, 'SN', 0 ):
      sub_uv.zap_table( 'SN', 0 )
  
  return sub_uv

###############################################################################

def add_model_old( uv, m_facets, facet_list = [], sigma = 0., apply_solutions = False,
    keep_solutions = False, conversion_method = 'DFT', model_version = 0,
    frequency_correction = False, flux_scale = 1. ):
# TODO: add solution_version keyword
  
  sub_uv = subtract_model_old( uv, m_facets, facet_list = facet_list, sigma = sigma,
     apply_solutions = apply_solutions, keep_solutions = keep_solutions,
     conversion_method = conversion_method, model_version = model_version,
     frequency_correction = frequency_correction, flux_scale = - flux_scale )
  add_uv = get_aips_file( uv.disk, uv.name, 'ADD', - 1, 'UV' )
  sub_uv.rename( name = add_uv.name, klass = add_uv.klass, seq = add_uv.seq )
  
  return add_uv

###############################################################################

def add_model_old2( uv, m_facets, facet_list = [], sigma = 0., apply_solutions = False,
    keep_solutions = False, conversion_method = 'DFT', model_version = 0,
    solution_version = 0, frequency_correction = False, flux_scale = 1.,
    replace = False, flag_solutions = True ): #, drop_missing_solutions = True ):
  
  sub_uv = subtract_model_old2( uv, m_facets, facet_list = facet_list, sigma = sigma,
     apply_solutions = apply_solutions, keep_solutions = keep_solutions,
     conversion_method = conversion_method, model_version = model_version,
     frequency_correction = frequency_correction, flux_scale = - flux_scale,
     solution_version = solution_version, replace = replace,
     flag_solutions = flag_solutions ) #, drop_missing_solutions = drop_missing_solutions )
  add_uv = get_aips_file( uv.disk, uv.name, 'ADD', - 1, 'UV' )
  sub_uv.rename( name = add_uv.name, klass = add_uv.klass, seq = add_uv.seq )
  
  return add_uv

###############################################################################

def merge_uv( up_uv, down_uv, keep_solutions = True ):

  new_uv = get_aips_file( up_uv.disk, up_uv.name, 'MERGE', - 1, 'UV' )
  call_aips_task( 'DBCON', indata = up_uv, in2data = down_uv, outdata = new_uv, doarray = 1 )

  # restore solution table
  while table_exists( new_uv, 'SN', 0 ):
    new_uv.zap_table( 'SN', 0 )
  if ( keep_solutions and ( up_uv != None ) ):
    if table_exists( up_uv, 'SN', 0 ):
      call_aips_task( 'TACOP', indata = up_uv, inext = 'SN', invers = 0, ncount = 1,
          outdata = new_uv, outvers = 0 )

  return new_uv

###############################################################################

def split_uv_on_time_range( uv, time_rts, keep_solutions = True, time_list = [] ):
  
  # retrieve relevant time ranges
  time_rise = time_rts[ 0 ]
  time_transit = time_rts[ 1 ]
  time_set = time_rts[ 2 ]
  if ( len( time_list ) > 0 ):
    time_array = array( time_list )
    time_min = time_array.min()
    time_max = time_array.max()
  else:
    time_min = restore_parameter( uv, 'time_min' )
    time_max = restore_parameter( uv, 'time_max' )
  
  # determine what time ranges source is visible during observing run
  ud_list = []
  if ( time_rise == time_set ):
    if ( time_rise == time_transit ): # source never visible
      ud_list.append( [ 'DOWN', time_min, time_max ] )
    else: # source always visible
      ud_list.append( [ 'UP', time_min, time_max ] )
  else:
    time_up_before_min = time_min - ( ( time_min - time_rise ) % 1. )
    time_down_before_min = time_min - ( ( time_min - time_set ) % 1. )
    time_up_after_max = time_max + ( ( time_rise - time_max ) % 1. )
    time_down_after_max = time_max + ( ( time_set - time_max ) % 1. )
    # source is visible at start of observing run
    if ( time_up_before_min > time_down_before_min ): 
      time_up = time_up_before_min
      time_down = time_down_before_min + 1.
      while ( time_up < time_max ):
        ud_list.append( [ 'UP', max( [ time_up, time_min ] ), 
            min( [ time_down, time_max ] ) ] )
        time_up = time_up + 1.
        if ( time_down < time_max ):
          ud_list.append( [ 'DOWN', time_down, min( [ time_up, time_max ] ) ] )
        time_down = time_down + 1.
    # source is not visible at start of observing run
    else: 
      time_down = time_down_before_min
      time_up = time_up_before_min + 1.
      while ( time_down < time_max ):
        ud_list.append( [ 'DOWN', max( [ time_down, time_min ] ), 
            min( [ time_up, time_max ] ) ] )
        time_down = time_down + 1.
        if ( time_up < time_max ):
          ud_list.append( [ 'UP', time_up, min( [ time_down, time_max ] ) ] )
        time_up = time_up + 1.
  
  # split all up-times and down-times
  up_count = 0
  down_count = 0
  for row in ud_list:
    if ( len( time_list ) > 0 ):
      sel = awhere( ( time_array >= row[ 1 ] ) & ( time_array <= row[ 2 ] ) )
      if ( len( sel ) == 0 ):
        continue
    if ( row[ 0 ] == 'UP' ):
      up_uv = get_aips_file( uv.disk, uv.name, 'UP%02d' % ( up_count + 1 ), -1,
          'UV' )
      try:
        call_aips_task( 'UVCOP', indata = uv, outdata = up_uv, 
            uvcopprm = [ 1, 0, 0, 0, 0 ], timerang = time_to_dhms( row[ 1 ] ) +
            time_to_dhms( row[ 2 ] ) )
      except: # no data selected
        pass
      else:
        up_count = up_count + 1
    else: # row[ 0 ] == 'DOWN'
      down_uv = get_aips_file( uv.disk, uv.name, 'DOWN%02d' % ( down_count + 1 ),
          -1, 'UV' )
      try:
        call_aips_task( 'UVCOP', indata = uv, outdata = down_uv,
            uvcopprm = [ 1, 0, 0, 0, 0 ], timerang = time_to_dhms( row[ 1 ] ) +
            time_to_dhms( row[ 2 ] ) )
      except: # no data selected
        pass
      else:
        down_count = down_count + 1
  
  # combine up-times
  if ( up_count > 0 ):
    new_up_uv = get_aips_file( uv.disk, uv.name, 'UP', - 1, 'UV' )
    up_uv = get_aips_file( uv.disk, uv.name, 'UP01', 0, 'UV' )
    up_uv.rename( name = new_up_uv.name, klass = new_up_uv.klass,
        seq = new_up_uv.seq )
    if ( up_count > 1 ):
      for i in range( 2, up_count + 1 ):
        next_up_uv = get_aips_file( up_uv.disk, up_uv.name, 'UP%02d' % ( i ),
            up_uv.seq, 'UV' )
        merged_uv = merge_uv( new_up_uv, next_up_uv )
        new_up_uv.zap()
        next_up_uv.zap()
        merged_uv.rename( name = new_up_uv.name, klass = new_up_uv.klass,
            seq = new_up_uv.seq )
  else:
    new_up_uv = None
  
  # combine down-times
  if ( down_count > 0 ):
    new_down_uv = get_aips_file( uv.disk, uv.name, 'DOWN', - 1, 'UV' )
    down_uv = get_aips_file( uv.disk, uv.name, 'DOWN01', 0, 'UV' )
    down_uv.rename( name = new_down_uv.name, klass = new_down_uv.klass,
        seq = new_down_uv.seq )
    if ( down_count > 1 ):
      for i in range( 2, down_count + 1 ):
        next_down_uv = get_aips_file( new_down_uv.disk, down_uv.name, 
            'DOWN%02d' % ( i ), down_uv.seq, 'UV' )
        merged_uv = merge_uv( new_down_uv, next_down_uv )
        new_down_uv.zap()
        next_down_uv.zap()
        merged_uv.rename( name = new_down_uv.name, klass = new_down_uv.klass,
            seq = new_down_uv.seq )
  else:
    new_down_uv = None
  
  # remove solution tables, as they are messed up
  if ( new_up_uv != None ):
    while table_exists( new_up_uv, 'SN', 0 ):
      new_up_uv.zap_table( 'SN', 0 )
  if ( new_down_uv != None ):
    while table_exists( new_down_uv, 'SN', 0 ):
      new_down_uv.zap_table( 'SN', 0 )
  
  # restore original solution table
  if ( keep_solutions and ( table_exists( uv, 'SN', 0 ) ) ):
    if ( new_up_uv != None ):
      call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = 0, ncount = 1,
          outdata = new_up_uv, outvers = 0 )
    if ( new_down_uv != None ):
      call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = 0, ncount = 1,
          outdata = new_down_uv, outvers = 0 )
  
  return [ new_up_uv, new_down_uv ]

###############################################################################

def subtract_as_model( uv, facets, facet_list = [], sigma = 0., apply_solutions = False,
    keep_solutions = False, conversion_method = 'DFT', model_version = 0 ):

  if ( len( facet_list ) == 0 ):
    used_facet_count = restore_parameter( facets, 'facet_count' )
    used_facet_list = range( 1, 1 + used_facet_count )
  else:
    used_facet_count = len( facet_list )
    used_facet_list = [ i for i in facet_list ]
  time_list = get_time_list( uv )

  # subtract source models over correct time ranges
  sub_uv = get_aips_file( uv.disk, uv.name, 'SUB', - 1, 'UV' )
  call_aips_task( 'MOVE', indata = uv, outdata = sub_uv, userid = get_aips_userid() )
  for i in used_facet_list:
    facet_i = get_facet( facets, i )
    if ( abs( get_model_flux( facet_i ) ) > 0. ):
      rts_times = calculate_rise_transit_set_times( sub_uv, radec = get_radec( facet_i ) )
      [ up_uv, down_uv ] = split_uv_on_time_range( sub_uv, rts_times, 
          time_list = time_list )
      if ( up_uv != None ):
        sub_up_uv = subtract_model( up_uv, facets, facet_list = [ i ], sigma = sigma,
            apply_solutions = apply_solutions, keep_solutions = keep_solutions,
            conversion_method = conversion_method, model_version = model_version,
            frequency_correction = False )
        sub_uv.zap()
        up_uv.zap()
        if ( down_uv != None ):
          sub_up_down_uv = merge_uv( sub_up_uv, down_uv )
          down_uv.zap()
          sub_up_uv.zap()
          sub_up_down_uv.rename( name = sub_uv.name, klass = sub_uv.klass, seq = sub_uv.seq )
        else:
          sub_up_uv.rename( name = sub_uv.name, klass = sub_uv.klass, seq = sub_uv.seq )
      elif ( down_uv != None ):
        down_uv.zap()
      
  return sub_uv

###############################################################################

def add_as_model( uv, facets, facet_list = [], sigma = 0., apply_solutions = False,
    keep_solutions = False, conversion_method = 'DFT', model_version = 0 ):

  if ( len( facet_list ) == 0 ):
    used_facet_count = restore_parameter( facets, 'facet_count' )
    used_facet_list = range( 1, 1 + used_facet_count )
  else:
    used_facet_count = len( facet_list )
    used_facet_list = [ i for i in facet_list ]
  time_list = get_time_list( uv )

  # subtract source models over correct time ranges
  add_uv = get_aips_file( uv.disk, uv.name, 'ADD', - 1, 'UV' )
  call_aips_task( 'MOVE', indata = uv, outdata = add_uv, userid = get_aips_userid() )
  for i in used_facet_list:
    facet_i = get_facet( facets, i )
    if ( abs( get_model_flux( facet_i ) ) > 0. ):
      rts_times = calculate_rise_transit_set_times( add_uv, radec = get_radec( facet_i ) )
      [ up_uv, down_uv ] = split_uv_on_time_range( add_uv, rts_times,
          time_list = time_list )
      if ( up_uv != None ):
        add_up_uv = add_model( up_uv, facets, facet_list = [ i ], sigma = sigma,
            apply_solutions = apply_solutions, keep_solutions = keep_solutions,
            conversion_method = conversion_method, model_version = model_version,
            frequency_correction = False )
        add_uv.zap()
        up_uv.zap()
        if ( down_uv != None ):
          add_up_down_uv = merge_uv( add_up_uv, down_uv )
          down_uv.zap()
          add_up_uv.zap()
          add_up_down_uv.rename( name = add_uv.name, klass = add_uv.klass, seq = add_uv.seq )
        else:
          add_up_uv.rename( name = add_uv.name, klass = add_uv.klass, seq = add_uv.seq )
      elif ( down_uv != None ):
        down_uv.zap()

  return add_uv

###############################################################################

def image_clean_o_facets( uv, apply_solutions = False, sigma = 3.,
    do_sdi_clean = False, conversion_method = 'DFT', box_sigma = 5.,
    gain = 0.1, factor = 0., imagr_params = {}, print_info = False ):
  
  # make outlier facets
  # assume that outlier facets are above horizon during observations
  cell_size = restore_parameter( uv, 'cell_size' )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  o_facet_count = restore_parameter( uv, 'o_facet_count' )
  o_facet_size = restore_parameter( uv, 'o_facet_size' )
  o_facet_file_name = restore_parameter( uv, 'o_facet_file_name' )
  channel_count = get_channel_count( uv )
  o_facets = get_aips_file( uv.disk, 'O', 'ICL001', -1, 'MA' )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  flux = sigma * cpb_noise
  if apply_solutions:
    solution_switch = 100
    solution_version = uv.table_highver( 'SN' )
  else:
    solution_switch = 0
    solution_version = -1
  if do_sdi_clean:
    sdi_param = 0.1
  else:
    sdi_param = 0.
  
  # image dirty facets
  call_aips_task( 'IMAGR', indata = uv, nchav = channel_count,
      docalib = solution_switch, gainuse = solution_version, 
      outdisk = o_facets.disk, outname = o_facets.name, outseq = o_facets.seq,
      do3dimag = 1, nfield = o_facet_count, allokay = 0, outver = 0,
      cellsize = [ cell_size, cell_size ], imsize = [ o_facet_size, o_facet_size ],
      niter = 100000, flux = 100000., boxfile = o_facet_file_name,
      in2disk = uv.disk, cmethod = conversion_method, minpatch = o_facet_size - 1,
      dotv = 0, overlap = 2, gain = gain, imagrprm = [ 0, 0, 0, sdi_param, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ], maxpixel = 0, factor = factor,
      bcomp = [ 0 for j in range( 64 ) ], flagver = -1, 
      uvsize = [ uv_size, uv_size ], **imagr_params )
  
  # store parameters
  store_parameter( o_facets, 'facet_count', o_facet_count )
  store_parameter( o_facets, 'facet_file_name', o_facet_file_name )
  
  # remove empty model tables
  for i in range( 1, o_facet_count + 1 ):
    o_facet_i = get_facet( o_facets, i )
    if model_table_empty( o_facet_i, 0 ):
      o_facet_i.zap_table( 'CC', 0 )
  
  # automatically generate clean boxes
  determine_facet_overlap( o_facets )
  add_clean_boxes( uv, o_facets, box_sigma, keep_boxes = False,
      peak_flux_ratio_max = 10., new_boxes_per_facet_max = 20 )
  
  # re-image and clean facets
  call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, docalib = solution_switch,
      gainuse = solution_version, outdisk = o_facets.disk, outname = o_facets.name,
      outseq = o_facets.seq, do3dimag = 1, nfield = o_facet_count, allokay = 1,
      outver = 0, cellsize = [ cell_size, cell_size ], boxfile = o_facet_file_name,
      imsize = [ o_facet_size, o_facet_size ], niter = 100000, flux = 0.95 * flux,
      in2disk = uv.disk, cmethod = conversion_method, minpatch = o_facet_size - 1,
      dotv = 0, overlap = 2, gain = gain, bcomp = [ 0 for j in range( 64 ) ],
      imagrprm = [ 0, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
      maxpixel = 0, factor = factor, flagver = -1, uvsize = [ uv_size, uv_size ],
      **imagr_params )
  
  return o_facets
  
###############################################################################

def image_clean_s_facets( uv, apply_solutions = False, sigma = 5.,
    do_sdi_clean = False, conversion_method = 'DFT', box_sigma = 5.,
    gain = 0.1, factor = 0., imagr_params = {} ):

  # make initial Sun facets
  cell_size = restore_parameter( uv, 'cell_size' )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  s_facet_count = restore_parameter( uv, 's_facet_count' )
  if s_facet_count > 1:
    raise error( 'currently only one Sun facet supported' )
  s_facet_size = restore_parameter( uv, 's_facet_size' )
  s_facet_file_name = restore_parameter( uv, 's_facet_file_name' )
  channel_count = get_channel_count( uv )
  s_facets = get_aips_file( uv.disk, 'S', 'ICL001', -1, 'MA' )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  flux = sigma * cpb_noise
  if apply_solutions:
    solution_switch = 100
    solution_version = uv.table_highver( 'SN' )
  else:
    solution_switch = 0
    solution_version = -1
  if do_sdi_clean:
    sdi_param = 0.1
  else:
    sdi_param = 0.
  time_list = get_time_list( uv )

  # image dirty facets
  call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, docalib = solution_switch,
      gainuse = solution_version, outdisk = s_facets.disk, outname = s_facets.name,
      outseq = s_facets.seq, do3dimag = 1, nfield = s_facet_count, allokay = 0,
      outver = 0, cellsize = [ cell_size, cell_size ], boxfile = s_facet_file_name,
      imsize = [ s_facet_size, s_facet_size ], niter = 100000, flux = 100000.,
      in2disk = uv.disk, cmethod = conversion_method, minpatch = s_facet_size - 1,
      dotv = 0, overlap = 2, gain = gain, bcomp = [ 0 for j in range( 64 ) ],
      imagrprm = [ 0, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
      maxpixel = 0, factor = factor, flagver = -1, uvsize = [ uv_size, uv_size ],
      **imagr_params )

  # store parameters
  store_parameter( s_facets, 'facet_count', s_facet_count )
  store_parameter( s_facets, 'facet_file_name', s_facet_file_name )
  determine_facet_overlap( s_facets )

  # remove empty model tables
  for i in range( 1, s_facet_count + 1 ):
    s_facet_i = get_facet( s_facets, i )
    s_facet_i.zap_table( 'CC', 0 )

  # re-image and clean Sun facet using UV data only while above the horizon
  rts_times = calculate_rise_transit_set_times( uv, radec = get_radec( s_facets ) )
  [ uv_up, uv_down ] = split_uv_on_time_range( uv, rts_times,
      time_list = time_list )
  if ( uv_up != None ):
    call_aips_task( 'IMAGR', indata = uv_up, nchav = channel_count, flagver = -1,
        docalib = solution_switch, gainuse = solution_version, do3dimag = 1,
        outdisk = s_facets.disk, outname = s_facets.name, outseq = s_facets.seq,
        nfield = s_facet_count, allokay = 1, outver = 0, niter = 100000,
        cellsize = [ cell_size, cell_size ], imsize = [ s_facet_size, s_facet_size ],
        flux = 0.95 * flux, boxfile = s_facet_file_name, in2disk = uv_up.disk,
        cmethod = conversion_method, minpatch = s_facet_size - 1, dotv = 0,
        overlap = 2, gain = gain, bcomp = [ 0 for j in range( 64 ) ], maxpixel = 0,
        imagrprm = [ 0, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
        factor = factor, uvsize = [ uv_size, uv_size ], **imagr_params )
    uv_up.zap()
  else:
    pass # leave the facet as is
  if ( uv_down != None ):
    uv_down.zap()

  # remove empty model tables
  for i in range( 1, s_facet_count + 1 ):
    s_facet_i = get_facet( s_facets, i )
    s_facet_i.zap_table( 'CC', 0 )

  # automatically generate clean boxes
  add_clean_boxes( uv, s_facets, box_sigma, keep_boxes = False, peak_flux_ratio_max = 10.,
      new_boxes_per_facet_max = 20 )

  # re-image and clean Sun facet using UV data only while above the horizon
  rts_times = calculate_rise_transit_set_times( uv, radec = get_radec( s_facets ) )
  [ uv_up, uv_down ] = split_uv_on_time_range( uv, rts_times,
      time_list = time_list )
  if ( uv_up != None ):
    call_aips_task( 'IMAGR', indata = uv_up, nchav = channel_count, flagver = -1,
        docalib = solution_switch, gainuse = solution_version, outver = 0,
        outdisk = s_facets.disk, outname = s_facets.name, outseq = s_facets.seq, 
        cellsize = [ cell_size, cell_size ], imsize = [ s_facet_size, s_facet_size ],
        do3dimag = 1, niter = 100000, flux = 0.95 * flux, boxfile = s_facet_file_name,
        in2disk = uv_up.disk,  nfield = s_facet_count, cmethod = conversion_method,
        minpatch = s_facet_size - 1, dotv = 0, overlap = 2, gain = gain, allokay = 1,
        imagrprm = [ 0, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
        maxpixel = 0, factor = factor, uvsize = [ uv_size, uv_size ], **imagr_params )
    uv_up.zap()
  else:
    pass # leave the facet as is
  if ( uv_down != None ):
    uv_down.zap()

  return s_facets

###############################################################################

def image_clean_a_facets( uv, apply_solutions = False, sigma = 5.,
    do_sdi_clean = False, conversion_method = 'DFT', box_sigma = 5.,
    gain = 0.1, factor = 0., imagr_params = {} ):

  # make initial A-team facets (no cleaning)
  cell_size = restore_parameter( uv, 'cell_size' )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  a_facet_count = restore_parameter( uv, 'a_facet_count' )
  a_facet_size = restore_parameter( uv, 'a_facet_size' )
  a_facet_file_name = restore_parameter( uv, 'a_facet_file_name' )
  channel_count = get_channel_count( uv )
  a_facets = get_aips_file( uv.disk, 'A', 'ICL001', - 1, 'MA' )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  flux = sigma * cpb_noise
  if apply_solutions:
    solution_switch = 100
    solution_version = 0 # uv.table_highver( 'SN' )
  else:
    solution_switch = 0
    solution_version = -1
  if do_sdi_clean:
    sdi_param = 0.1
  else:
    sdi_param = 0.
  time_list = get_time_list( uv )

  # image dirty facets
  # this call also adds empty model tables to the facet
  call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, docalib = solution_switch,
      gainuse = solution_version, outdisk = a_facets.disk, outname = a_facets.name,
      outseq = a_facets.seq, outver = 0, cellsize = [ cell_size, cell_size ],
      imsize = [ a_facet_size, a_facet_size ], do3dimag = 1, niter = 100000,
      boxfile = a_facet_file_name, in2disk = uv.disk, cmethod = conversion_method,
      minpatch = a_facet_size - 1, dotv = 0, overlap = 2, gain = gain,
      imagrprm = [ 0, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ],
      bcomp = [ 0 for j in range( 64 ) ], maxpixel = 0, factor = factor, flagver = -1,
      allokay = 0, flux = 100000., nfield = a_facet_count, uvsize = [ uv_size, uv_size ],
      **imagr_params )

  # store parameters
  store_parameter( a_facets, 'facet_count', a_facet_count )
  store_parameter( a_facets, 'facet_file_name', a_facet_file_name )
  determine_facet_overlap( a_facets )

  # re-image A-team facets using UV data only while source above the horizon
  a_list = []
  new_facets = get_aips_file( a_facets.disk, 'NEW_A', a_facets.klass, - 1, 'MA' )
  for i in range( 1, a_facet_count + 1 ):
    a_facet = get_facet( a_facets, i )
    a_beam = get_facet_beam( a_facet )
    rts_times = calculate_rise_transit_set_times( uv, radec = get_radec( a_facet ) )
    [ uv_up, uv_down ] = split_uv_on_time_range( uv, rts_times,
        time_list = time_list )
    if ( uv_up != None ):
      new_facet = get_facet( new_facets, 1 )
      new_beam = get_facet_beam( new_facet )
      new_facet_file_name = a_facet_file_name + '.%03d' % ( i )
      extract_facet_definitions( a_facet_file_name, [ i ], new_facet_file_name )
      call_aips_task( 'IMAGR', indata = uv_up, nchav = channel_count, outver = 0,
          docalib = solution_switch, gainuse = solution_version, allokay = 0,
          outdisk = new_facet.disk, outname = new_facet.name, outseq = new_facet.seq,
          cellsize = [ cell_size, cell_size ], imsize = [ a_facet_size, a_facet_size ],
          do3dimag = 1, niter = 100000, flux = 100000., boxfile = new_facet_file_name,
          in2disk = uv_up.disk, cmethod = conversion_method, gain = gain, nfield = 1,
          minpatch = a_facet_size - 1, dotv = 0, overlap = 2, maxpixel = 0,
          imagrprm = [ 0, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          1, 0, 0 ], bcomp = [ 0 for j in range( 64 ) ], factor = factor, flagver = -1,
          uvsize = [ uv_size, uv_size ], **imagr_params )
      remove_file( new_facet_file_name )
      call_aips_task( 'TACOP', indata = a_facet, inext = 'PS', invers = 0, ncount = 1,
          outdata = new_facet, outvers = 0 )
      a_facet.zap()
      a_beam.zap()
      new_facet.rename( name = a_facet.name, klass = a_facet.klass, seq = a_facet.seq )
      new_beam.rename( name = a_beam.name, klass = a_beam.klass, seq = a_beam.seq )
      uv_up.zap()
      peak_flux = get_image_maximum( a_facet )
      a_list.append( [ i, peak_flux ] )
    else:
      pass # leave the facet as is
    if ( uv_down != None ):
      uv_down.zap()
  a_list.sort( cmp = lambda a, b: cmp( b[ 1 ], a[ 1 ] ) )

  # automatically generate clean boxes
  add_clean_boxes( uv, a_facets, box_sigma, keep_boxes = False, peak_flux_ratio_max = 10.,
      new_boxes_per_facet_max = 20 )

  # re-image and clean A-team facets using UV data only while above the horizon
  for a in a_list:
    [ i, peak_flux ] = a
    a_facet = get_facet( a_facets, i )
    a_beam = get_facet_beam( a_facet )
    rts_times = calculate_rise_transit_set_times( uv, radec = get_radec( a_facet ) )
    [ uv_up, uv_down ] = split_uv_on_time_range( uv, rts_times,
        time_list = time_list )
    if ( uv_up != None ):
      new_facet = get_facet( a_facets, i )
      new_beam = get_facet_beam( new_facet )
      new_facet.rename( name = new_facets.name, klass = new_facets.klass,
          seq = new_facets.seq )
      new_beam.rename( name = new_facets.name,
          klass = get_facet_beam( new_facet ).klass,
          seq = new_facets.seq )
      new_facet_file_name = a_facet_file_name + '.%03d' % ( i )
      extract_facet_definitions( a_facet_file_name, [ i ], new_facet_file_name )
      if model_table_empty( new_facet, 0 ):
        new_facet.zap_table( 'CC', 0 )
      call_aips_task( 'IMAGR', indata = uv_up, nchav = channel_count, outver = 0,
          docalib = solution_switch, gainuse = solution_version, do3dimag = 1,
          outdisk = new_facet.disk, outname = new_facet.name, outseq = new_facet.seq,
          cellsize = [ cell_size, cell_size ], imsize = [ a_facet_size, a_facet_size ],
          niter = 100000, flux = 0.95 * flux, in2disk = uv_up.disk, flagver = -1,
          boxfile = new_facet_file_name, cmethod = conversion_method, nfield = 1,
          minpatch = a_facet_size - 1, dotv = 0, overlap = 2, gain = gain,
          imagrprm = [ 0, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          1, 0, 0 ], bcomp = [ 0 for j in range( 64 ) ], maxpixel = 0, factor = factor,
          allokay = 1, uvsize = [ uv_size, uv_size ], **imagr_params )
      new_facet.rename( name = a_facet.name, klass = a_facet.klass, seq = a_facet.seq )
      new_beam.rename( name = a_beam.name, klass = a_beam.klass, seq = a_beam.seq )
      uv_up.zap()
      remove_file( new_facet_file_name )
    else:
      pass # leave the facet as is
    if ( uv_down != None ):
      uv_down.zap()

  return a_facets

###############################################################################

def image_clean_pbo_facets( uv, sigma = 5., improvement_limit = 0.05,
    apply_solutions = False, do_sdi_clean = False, restore_components = True,
    conversion_method = 'DFT', add_boxes = True, box_sigma = 5.,
    keep_boxes = False, frequency_correction = False, gain = 0.1, factor = 0.,
    imagr_params = {}, print_info = False, facet_based_noise = False ):
  
  # make copy of UV data
  temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', -1, 'UV' )
  call_aips_task( 'MOVE', indata = uv, outdata = temp_uv, userid = get_aips_userid() )
  
  # combine primary beam and nearby outlier facet definitions
  pb_facet_file_name = restore_parameter( uv, 'pb_facet_file_name' )
  o_facet_file_name = restore_parameter( uv, 'o_facet_file_name' )
  pbo_facet_file_name = pb_facet_file_name + '.PBO'
  [ pb_facet_count, o_facet_count ] = merge_facet_definitions( 
      pb_facet_file_name, o_facet_file_name, pbo_facet_file_name )
  if ( ( pb_facet_count != restore_parameter( uv, 'pb_facet_count' ) ) or
      ( o_facet_count != restore_parameter( uv, 'o_facet_count' ) ) ):
    raise error( 'facet counts do not match' )
  pbo_facet_count = pb_facet_count + o_facet_count
  
  # image primary beam and nearby outlier facets
  store_parameter( temp_uv, 'pb_facet_count', pbo_facet_count )
  store_parameter( temp_uv, 'pb_facet_file_name', pbo_facet_file_name )
  pbo_facets = image_clean_pb_facets( temp_uv, sigma = sigma,
      improvement_limit = improvement_limit, apply_solutions = apply_solutions,
      do_sdi_clean = do_sdi_clean, restore_components = restore_components,
      keep_boxes = keep_boxes, conversion_method = conversion_method,
      add_boxes = add_boxes, box_sigma = box_sigma, print_info = print_info,
      frequency_correction = frequency_correction, gain = gain, factor = factor,
      imagr_params = imagr_params, facet_based_noise = facet_based_noise )
  
  # split primary beam and nearby outlier facet definitions
  pb_facet_list = range( 1, 1 + pb_facet_count )
  o_facet_list = range( 1 + pb_facet_count, 1 + pb_facet_count + o_facet_count )
  remove_file( pb_facet_file_name )
  extract_facet_definitions( pbo_facet_file_name, pb_facet_list, pb_facet_file_name )
  if ( len( o_facet_list ) > 0 ):
    remove_file( o_facet_file_name )
    extract_facet_definitions( pbo_facet_file_name, o_facet_list, o_facet_file_name )
  remove_file( pbo_facet_file_name )
  
  # split primary beam and nearby outlier facets
  pb_facets = pbo_facets
  store_parameter( pb_facets, 'facet_count', pb_facet_count )
  store_parameter( pb_facets, 'facet_file_name', pb_facet_file_name )
  if ( len( o_facet_list ) > 0 ):
    o_facets = get_aips_file( pbo_facets.disk, 'O', pbo_facets.klass, -1, 'MA' )
    j = 0
    for i in o_facet_list:
      j = j + 1
      pbo_facet = get_facet( pbo_facets, i )
      pbo_beam = get_facet_beam( pbo_facet )
      o_facet = get_facet( o_facets, j )
      o_beam = get_facet_beam( o_facet )
      pbo_facet.rename( name = o_facet.name, klass = o_facet.klass, seq = o_facet.seq )
      pbo_beam.rename( name = o_beam.name, klass = o_beam.klass, seq = o_beam.seq )
    store_parameter( o_facets, 'facet_count', o_facet_count )
    store_parameter( o_facets, 'facet_file_name', o_facet_file_name )
  else:
    o_facets = None

  # recalculate overlaps
  determine_facet_overlap( pb_facets )
  determine_facet_overlap( o_facets )
  
  # save noise level and delete copy of UV
  cpb_noise = restore_parameter( temp_uv, 'cpb_noise' )
  store_parameter( uv, 'cpb_noise', cpb_noise )
  temp_uv.zap()
  
  return [ pb_facets, o_facets ]

###############################################################################

def flag_bad_solutions( uv, uvim = None, solution_version = 0, flag_version = 0,
    apply_flags = False, keep_flags = False, int_factor = 0.5 ):
# flagver < 0: create new table; flagver = 0: append to highest table
  
  # if needed, copy solution table from other file
  if ( uvim == None ):
    sn_version = solution_version
  else:
    call_aips_task( 'TACOP', indata = uvim, inext = 'SN', invers = solution_version,
        ncount = 1, outdata = uv, outvers = 0 )
    sn_version = uv.table_highver( 'SN' )
  
  # pick flag table version
  if ( flag_version > 0 ):
    if table_exists( uv, 'FG', flag_version ):
      fl_version = flag_version
    else:
      fl_table = -1
  elif ( flag_version == 0 ):
    fl_version = uv.table_highver( 'FG' )
    if ( fl_version == 0 ):
      fl_version = -1
  else: #( flag_version < 0 )
    fl_version = -1
  
  # make flag table based on failed solutions
  dt = restore_parameter( uv, 'integration_time' ) * int_factor
  call_aips_task( 'SNFLG', indata = uv, invers = sn_version, optype = '',
      cutoff = 1.e-12, dparm = [ 1000, dt ], flagver = fl_version )
  if ( uvim != None ):
    uv.zap_table( 'SN', sn_version )
  new_fl_version = uv.table_highver( 'FG' )
  if ( fl_version > 0 ):
    uv.zap_table( 'FG', fl_version )
    call_aips_task( 'TACOP', indata = uv, outdata = uv, inext = 'FG',
        invers = new_fl_version, ncount = 1, outvers = fl_version )
    uv.zap_table( 'FG', new_fl_version )
  else:
    fl_version = new_fl_version
  
  # apply flag table
  flag_uv = None
  if apply_flags:
    flag_uv = apply_flag_table( uv, version = fl_version, keep_solutions = True )
    if ( not keep_flags ):
      uv.zap_table( 'FG', fl_version )
  
  return flag_uv

###############################################################################

def apply_flag_table( uv, version = 0, keep_solutions = True ):
  
  if ( version == 0 ):
    flag_version = uv.table_highver( 'FG' )
  else:
    flag_version = version
  
  # apply flag table
  flag_uv = get_aips_file( uv.disk, uv.name, 'FLAG', - 1, 'UV' )
  call_aips_task( 'UVCOP', indata = uv, outdata = flag_uv, flagver = flag_version )
  
  if ( not keep_solutions ):
    while table_exists( flag_uv, 'SN', 0 ):
      flag_uv.zap_table( 'SN', 0 )
  
  return flag_uv

###############################################################################

def make_model_uv( uv, facets, facet_list = [], sigma = 0., flux_scale = 1.,
    apply_solutions = False, keep_solutions = True, conversion_method = 'DFT',
    model_version = 0, solution_version = 0, frequency_correction = False,
    flag_solutions = True, keep_flags = True, merge_model = False ):
# returns model UV or None when model is empty  

  # initialise some parameters
  if ( len( facet_list ) > 0 ):
    temp_facet_count = len( facet_list )
    temp_facet_list = [ i for i in facet_list ]
  else:
    try:
      temp_facet_count = restore_parameter( facets, 'facet_count' )
    except:
      temp_facet_count = 1
    temp_facet_list = range( 1, temp_facet_count + 1 )
  if ( sigma > 0. ):
    cpb_noise = restore_parameter( uv, 'cpb_noise' )
    flux = sigma * cpb_noise
  else:
    flux = 0.
  if frequency_correction:
    dish_diameter = restore_parameter( uv, 'dish_diameter' )
  
  # handle different solution possibilities
  sol_switch = 0
  if apply_solutions:
    if table_exists( get_facet( facets, temp_facet_list[ 0 ] ), 'SN', solution_version ):
      sol_switch = 2
    elif table_exists( uv, 'SN', solution_version ):
      sol_switch = 1
  
  # copy and rename selected facets
  # merge clean components to speed up things
  temp_facets = get_aips_file( facets.disk, 'TEMP', 'ICL001', -1, 'MA' )
  j = 0
  for i in range( 1, 1 + max( temp_facet_list ) ):
    if ( i in temp_facet_list ):
      facet = get_facet( facets, i )
      if model_table_empty( facet, model_version ):
        temp_facet_list.remove( i )
        continue
      j = j + 1
      temp_facet = get_facet( temp_facets, j )
      call_aips_task( 'MOVE', indata = facet, outdata = temp_facet,
          userid = get_aips_userid() )
      if merge_model:
        call_aips_task( 'CCMRG', indata = temp_facet, invers = model_version,
            outvers = 0 )
        mdl_version = temp_facet.table_highver( 'CC' )
      else:
        mdl_version = model_version
      if ( ( sol_switch == 2 ) and ( j > 1 ) ):
        scale_model_flux( temp_facet, -flux_scale, in_version = mdl_version )
      else:
        scale_model_flux( temp_facet, flux_scale, in_version = mdl_version )
  temp_facet_count = len( temp_facet_list )
  if ( temp_facet_count == 0 ):
    return None
  
  # apply no solution
  model_uv = get_aips_file( uv.disk, uv.name, 'MODEL', - 1, 'UV' )
  if ( sol_switch == 0 ):
    if frequency_correction:
      call_aips_task( 'OOSUB', indata = uv, in2data = temp_facets,
          invers = 0, nmaps = temp_facet_count, outdata = model_uv,
          flux = flux, cmodel = 'COMP', cmethod = conversion_method,
          bparm = [ dish_diameter, 0, 0 ], opcode = 'MODL', factor = 1. )
    else:
      call_aips_task( 'UVSUB', indata = uv, in2data = temp_facets,
          invers = 0, nmaps = temp_facet_count, outdata = model_uv,
          flux = flux, cmodel = 'COMP', cmethod = conversion_method,
          opcode = 'MODL', factor = 1. )
  
  # apply single solution for all facets
  elif ( sol_switch == 1 ):
    # flag bad solutions (important for DIFUV)
    if flag_solutions:
      flag_uv = flag_bad_solutions( uv, solution_version = solution_version,
          flag_version = -1, apply_flags = True, keep_flags = keep_flags )
    else:
      flag_uv = uv
    # generate model UV data set
    if frequency_correction:
      call_aips_task( 'OOSUB', indata = flag_uv, in2data = temp_facets,
          invers = 0, nmaps = temp_facet_count, outdata = model_uv,
          flux = flux, cmodel = 'COMP', cmethod = conversion_method,
          bparm = [ dish_diameter, 0, 0 ], opcode = 'MODL', factor = 1. )
    else:
      call_aips_task( 'UVSUB', indata = flag_uv, in2data = temp_facets,
          invers = 0, nmaps = temp_facet_count, outdata = model_uv,
          flux = flux, cmodel = 'COMP', cmethod = conversion_method,
          opcode = 'MODL', factor = 1. )
    if ( flag_uv != uv ):
      flag_uv.zap()
    # apply inverted SN to model UV
    call_aips_task( 'CLINV', indata = uv, inext = 'SN', invers = solution_version,
        outdata = model_uv, outvers = 0 )
#    snver = model_uv.table_highver( 'SN' ) + 1
#    call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = solution_version,
#        outdata = model_uv, outvers = snver )
#    call_aips_task( 'SNCOR', indata = model_uv, snver = snver, opcode = 'PHNEG' )
    cal_model_uv = get_aips_file( uv.disk, uv.name, 'CALMDL', - 1, 'UV' )
    call_aips_task( 'SPLIT', indata = model_uv, docalib = 100, gainuse = 0,
        douvcomp = 0, flagver = - 1, outdisk = cal_model_uv.disk,
        outclass = cal_model_uv.klass, outseq = cal_model_uv.seq )
    model_uv.zap()
    cal_model_uv.rename( name = model_uv.name, klass = model_uv.klass,
        seq = model_uv.seq )
  
  # apply solution per facet
  else: # ( sol_switch == 2 )
    # define some names
    temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )
    cal_model_uv = get_aips_file( uv.disk, uv.name, 'CALMDL', - 1, 'UV' )
    # flag bad solutions (important for DIFUV)
    if flag_solutions:
      for j in range( 1, 1 + temp_facet_count ):
        if ( j == 1 ):
          flag_version = -1
        else:
          flag_version = 0
        if ( j == temp_facet_count ):
          apply_flags = True
        else:
          apply_flags = False
        temp_facet = get_facet( temp_facets, j )
        flag_uv = flag_bad_solutions( uv, uvim = temp_facet,
            apply_flags = apply_flags, flag_version = flag_version,
            solution_version = solution_version, keep_flags = keep_flags )
    else:
      flag_uv = get_aips_file( uv.disk, uv.name, 'FLAG', -1, 'UV' )
      call_aips_task( 'MOVE', indata = uv, outdata = flag_uv,
          userid = get_aips_userid() )
    # remove larger tables to speed up things
    while table_exists( flag_uv, 'NI', 0 ):
      flag_uv.zap_table( 'NI', 0 )
    while table_exists( flag_uv, 'OB', 0 ):
      flag_uv.zap_table( 'OB', 0 )
    while table_exists( flag_uv, 'SN', 0 ):
      flag_uv.zap_table( 'SN', 0 )
    # cycle over facets
    for j in range( 1, 1 + temp_facet_count ):
      temp_facet = get_facet( temp_facets, j )
      # generate model UV data set per facet
      if frequency_correction:
        call_aips_task( 'OOSUB', indata = flag_uv, in2data = temp_facet,
            invers = 0, nmaps = 1, outdata = temp_uv, flux = flux,
            cmodel = 'COMP', cmethod = conversion_method, opcode = 'MODL',
            bparm = [ dish_diameter, 0, 0 ], factor = 1. )
      else:
        call_aips_task( 'UVSUB', indata = flag_uv, in2data = temp_facet,
            invers = 0, nmaps = 1, outdata = temp_uv, flux = flux,
            cmodel = 'COMP', cmethod = conversion_method, opcode = 'MODL',
            factor = 1. )
      # apply inverted SN to model UV
      call_aips_task( 'CLINV', indata = temp_facet, inext = 'SN',
          invers = 0, outdata = temp_uv, outvers = 0 )
#      snver = temp_uv.table_highver( 'SN' ) + 1
#      call_aips_task( 'TACOP', indata = temp_facet, inext = 'SN', invers = 0,
#          outdata = temp_uv, outvers = snver )
#      call_aips_task( 'SNCOR', indata = temp_uv, snver = snver, opcode = 'PHNEG' )
      call_aips_task( 'SPLIT', indata = temp_uv, docalib = 100, gainuse = 0,
          flagver = -1, douvcomp = 0, outdisk = cal_model_uv.disk,
          outclass = cal_model_uv.klass, outseq = cal_model_uv.seq )
      temp_uv.zap()
      # combine model UV from different facets
      if ( j == 1 ):
        cal_model_uv.rename( name = model_uv.name, klass = model_uv.klass,
            seq = model_uv.seq )
        cal_model_uv = get_aips_file( uv.disk, uv.name, 'CALMDL', -1, 'UV' )
      else:
        # subtract model UV from UV
        call_aips_task( 'DIFUV', indata = model_uv, in2data = cal_model_uv,
            outdata = temp_uv, solint = 0 ) 
        model_uv.zap()
        cal_model_uv.zap()
        # adjust weights to original value
        call_aips_task( 'WTMOD', indata = temp_uv, outdata = model_uv,
              aparm = [ 2., 0., 0. ] )
        temp_uv.zap()
    # copy back large tables (SN tables are handled below)
    flag_uv.zap()
    for version in range( 1, 1 + uv.table_highver( 'NI' ) ):
      if table_exists( uv, 'NI', version ):
        call_aips_task( 'TACOP', indata = uv, inext = 'NI', ncount = 1,
            invers = version, outdata = model_uv, outvers = version )
    for version in range( 1, 1 + uv.table_highver( 'OB' ) ):
      if table_exists( uv, 'OB', version ):
        call_aips_task( 'TACOP', indata = uv, inext = 'OB', ncount = 1,
              invers = version, outdata = model_uv, outvers = version )
  
  # remove facet copies
  for j in range( 1, 1 + temp_facet_count ):
    temp_facet = get_facet( temp_facets, j )
    temp_facet.zap()
  
  # copy or remove SN tables when requested
  if ( keep_solutions and ( not table_exists( model_uv, 'SN', 0 ) ) ):
    for version in range( 1, 1 + uv.table_highver( 'SN' ) ):
      if table_exists( uv, 'SN', version ):
        call_aips_task( 'TACOP', indata = uv, inext = 'SN', ncount = 1,
            invers = version, outdata = model_uv, outvers = version )
  if ( ( not keep_solutions ) and table_exists( model_uv, 'SN', 0 ) ):
    while table_exists( model_uv, 'SN', 0 ):
      model_uv.zap_table( 'SN', 0 )
  
  return model_uv

###############################################################################

def subtract_uv( uv, model_uv, apply_flags = True, flag_version = 0 ):
# flag table is expected to be attached to uv

  # apply flags
  if ( apply_flags and table_exists( uv, 'FG', flag_version ) ):
    flag_uv = apply_flag_table( uv )
  else:
    flag_uv = uv

  # subtract model UV from UV
  # adjust weights to original value
  temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )
  sub_uv = get_aips_file( uv.disk, uv.name, 'SUB', - 1, 'UV' )
  call_aips_task( 'DIFUV', indata = flag_uv, in2data = model_uv,
      outdata = temp_uv, solint = 0 )
  call_aips_task( 'WTMOD', indata = temp_uv, outdata = sub_uv,
      aparm = [ 2., 0., 0. ] )
  temp_uv.zap()

  # cleanup
  if ( flag_uv != uv ):
    flag_uv.zap()
  
  return sub_uv

###############################################################################

def subtract_model( uv, facets, facet_list = [], sigma = 0., merge_model = False,
    apply_solutions = False, keep_solutions = True, conversion_method = 'DFT',
    model_version = 0, solution_version = 0, frequency_correction = False,
    flag_solutions = True, fast_flag = False, keep_flags = True ):
  
  # work copies
  flag_solns = flag_solutions
  flag_uv = uv
  
  # determine solution mode
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
    flux_scale = 1., sigma = sigma, merge_model = merge_model )
  
  # subtract model uv from flagged uv
  if ( model_uv == None ):
    sub_uv = None
  else:
    sub_uv = subtract_uv( flag_uv, model_uv, apply_flags = flag_solns )
    model_uv.zap()
    
    # when requested delete solutions
    if ( not keep_solutions ):
      while table_exists( sub_uv, 'SN', 0 ):
        sub_uv.zap_table( 'SN', 0 )
  
  # cleanup
  if ( flag_uv != uv ):
    flag_uv.zap()
  
  return sub_uv

###############################################################################

def add_model( uv, facets, facet_list = [], sigma = 0., merge_model = False,
    apply_solutions = False, keep_solutions = True, conversion_method = 'DFT',
    model_version = 0, solution_version = 0, frequency_correction = False,
    flag_solutions = True, fast_flag = False, keep_flags = True ):
  
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
  
  # create inverted model uv
  imodel_uv = make_model_uv( flag_uv, facets, facet_list = facet_list,
    apply_solutions = apply_solutions, keep_solutions = keep_solutions,
    conversion_method = conversion_method, model_version = model_version,
    solution_version = solution_version, flag_solutions = flag_solns,
    frequency_correction = frequency_correction, keep_flags = keep_flags,
    flux_scale = -1., sigma = sigma, merge_model = merge_model )
  
  # subtract inverted model uv from flagged uv ( = add model )
  if ( imodel_uv == None ):
    add_uv = None
  else:
    add_uv = get_aips_file( uv.disk, uv.name, 'ADD', -1, 'UV' )
    sub_uv = subtract_uv( flag_uv, imodel_uv, apply_flags = True )
    sub_uv.rename( name = add_uv.name, klass = add_uv.klass, seq = add_uv.seq )
    imodel_uv.zap()
    
    # when requested delete solutions
    if ( not keep_solutions ):
      while table_exists( add_uv, 'SN', 0 ):
        add_uv.zap_table( 'SN', 0 )
  
  # cleanup
  if ( flag_uv != uv ):
    flag_uv.zap()
  
  return add_uv

###############################################################################
