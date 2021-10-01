###############################################################################

# import Python modules
from sys import *
from os import *
from datetime import *
from math import *

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

def get_source_list( facets, flux_min, peak_flux_ratio_max = None, area_ratio_max = None,
    size_ratio_min = 0.75, blank_factor = 2.0 ):

  facet_count = restore_parameter( facets, 'facet_count' )
  facet_list = range( 1, facet_count + 1 )
  facet_size = get_image_size( facets )
  source_list = []

  # create copies of facets to blank
  peak_facets = get_aips_file( facets.disk, 'PEAK', facets.klass, -1, 'MA' )
  for i in facet_list:
    facet_i = get_facet( facets, i )
    peak_facet_i = get_facet( peak_facets, i )
    call_aips_task( 'MOVE', indata = facet_i, outdata = peak_facet_i, userid = get_aips_userid(), opcode = '' )
    fill_facet( peak_facet_i, do_edge_circle = True ) # blank facet boundaries

  # determine minimum flux for sources
  if ( peak_flux_ratio_max != None ):
    peak_flux_max = 0.
    for i in facet_list:
      peak_facet_i = get_facet( peak_facets, i )
      [ max_flux, max_pos ] = get_image_extremum( peak_facet_i, force_positive = True )
      if ( max_flux > peak_flux_max ):
        peak_flux_max = max_flux
    peak_flux_min = max( [ flux_min, peak_flux_max / peak_flux_ratio_max ] )
  else:
    peak_flux_min = flux_min

  # find and fit sources
  res_facets = get_aips_file( facets.disk, 'RES', facets.klass, -1, 'MA' )
  for i in facet_list:
    facet_i = get_facet( facets, i )
    [ beam_bmaj, beam_bmin, beam_bpa ] = get_beam_size( facet_i )
    res_facet_i = get_facet( res_facets, i )
    call_aips_task( 'MOVE', indata = facet_i, outdata = res_facet_i, userid = get_aips_userid(),
        opcode = '' )
    peak_facet_i = get_facet( peak_facets, i )
    [ max_flux, max_pos ] = get_image_extremum( peak_facet_i, force_positive = True )
    peak_facet_i.zap()

    while ( max_flux >= peak_flux_min ):

      fit_results = fit_gaussian_to_peak( res_facet_i, pos = max_pos, return_double_fit = True ) 
      blank_list = []

      if ( len( fit_results ) == 0 ):

        [ peak_x, peak_y ] = [ float( p ) for p in max_pos ]
        peak_flux = 0.
        if ( beam_bmaj != 0. ):
          [ int_bmaj, int_bmin, int_bpa ] = [ beam_bmaj, beam_bmin, beam_bpa ]
        else:
          pixel_size = max( get_pixel_size( facet_i ) )
          [ int_bmaj, int_bmin, int_bpa ] = [ pixel_size * 4., pixel_size * 4., 0. ]
        blank_list.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )

      else:

        for fit in fit_results:
          [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] = fit

          # check fit against clean beam size
          if ( beam_bmaj != 0. ):
            if ( ( int_bmaj < size_ratio_min * beam_bmaj ) or ( int_bmin < size_ratio_min * beam_bmin ) ):
              [ peak_x, peak_y ] = max_pos
              peak_flux = 0.
              [ int_bmaj, int_bmin, int_bpa ] = [ beam_bmaj, beam_bmin, beam_bpa ]
              blank_list.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )
              continue

          # check fit against selection criteria
          if ( beam_bmaj != 0. ):
            area_ratio = ( int_bmaj * int_bmin ) / ( beam_bmaj * beam_bmin )
            int_flux = peak_flux * area_ratio
            if ( area_ratio_max != None ):
              if ( area_ratio > area_ratio_max ):
                blank_list.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )
                continue
          else: # special case when fitting to dirty beam facets
            int_flux = peak_flux

          # check if source is nearest to this facet center, and if so, add to source list
          blank_list.append( [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] )
          peak_radec = calculate_source_radec( res_facet_i, [ peak_x, peak_y ] )
          overlapping_facet_list = [ i ] + get_facet_overlap( facet_i )
          [ [ main_i, main_pos ] ] = find_source_facets( facets, peak_radec, primary_facet_only = True,
              facet_list = overlapping_facet_list )
          if ( i == main_i ):
            source_list.append( [ i, [ peak_x, peak_y ], peak_flux, int_flux, 
                [ int_bmaj, int_bmin, int_bpa ] ] )

      for blank in blank_list:
        [ [ peak_x, peak_y ], peak_flux, [ int_bmaj, int_bmin, int_bpa ] ] = blank

        # subtract fitted source from residual facet
        # put central source area to zero to prevent fitting on residuals
        [ bmaj_pix, bmin_pix, bpa_pix ] = convert_beam_size( facet_i, beam = [ int_bmaj, int_bmin, int_bpa ],
            to_pixel = True )
        old_res_facet_i = get_aips_file( res_facet_i.disk, res_facet_i.name, res_facet_i.klass,
            res_facet_i.seq, 'MA' )
        old_res_facet_i.rename( name = 'OLDRES', seq = 0 )
        call_aips_task( 'IMMOD', indata = old_res_facet_i, outdata = res_facet_i, opcode = 'GAUS', ngaus = 1,
            fmax = [ - peak_flux, 0, 0, 0 ], fpos = [ [ peak_x, peak_y ], [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ],
            fwidth = [ [ bmaj_pix, bmin_pix, bpa_pix ], [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ], factor = 1 )
        old_res_facet_i.zap()
        fill_source( res_facet_i, [ peak_x, peak_y ], 
            beam = [ blank_factor * int_bmaj, blank_factor * int_bmin, int_bpa ], value = 0. )

      # blank facet boundaries and get next extremum
      call_aips_task( 'MOVE', indata = res_facet_i, outdata = peak_facet_i, userid = get_aips_userid() )
      fill_facet( peak_facet_i, do_edge_circle = True )
      [ max_flux, max_pos ] = get_image_extremum( peak_facet_i, force_positive = True )
      peak_facet_i.zap()

    res_facet_i.zap()

  # sort source by integrated flux
  if ( len( source_list ) > 0 ):
    source_list.sort( cmp = lambda a, b: cmp( b[ 3 ], a[ 3 ] ) )

  return source_list

###############################################################################

def calibrate_facets( uv, facets, facet_count, sigma_min = 0., snr_limit = 2.5,
    signal_to_noise_min = 10., reference_antenna = 0, conversion_method = 'DFT',
    do_amplitude = False, amplitude_interval = 5., phase_interval_min = 0.,
    calib_params = {} ):

  # determine total flux in model
  total_model_flux = 0.
  for i in range( 1, facet_count + 1 ):
    facet_i = get_facet( facets, i )
    total_model_flux = total_model_flux + get_model_flux( facet_i )

  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  if ( do_amplitude ):
    solution_interval = amplitude_interval
    interval_count = 1.
  else:
    # calculate noise per integration interval
#    time_count = restore_parameter( uv, 'time_count' )
    time_count = len( get_time_list( uv ) )
    noise_per_interval = cpb_noise * sqrt( float( time_count ) )

    # calculate solution interval
    integration_time = restore_parameter( uv, 'integration_time' ) / 60.
    sn_per_interval = total_model_flux / noise_per_interval
    interval_count = ceil( ( signal_to_noise_min / sn_per_interval )**2 )
    solution_interval = integration_time * interval_count
    solution_interval = max( [ solution_interval, phase_interval_min ] )

  # phase calibrate UV data 
  if do_amplitude:
    solmode = 'A&P'
    normalize_gains = 0
  else:
    solmode = 'P'
    normalize_gains = 1
  dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', - 1, 'UV' )
  channel_count = get_channel_count( uv )
  call_aips_task( 'CALIB', indata = uv, in2data = facets, invers = 0, 
      ncomp = [ 0 ], flux = sigma_min * cpb_noise, nmaps = facet_count,
      cmodel = 'COMP', outdata = dummy_uv, cmethod = conversion_method,
      snver = 0, refant = reference_antenna, solint = solution_interval,
      solsub = 1, solmin = 1, soltype = 'L1R', solmode = solmode, 
      aparm = [ 4, 0, 0, 0, 0, 0, snr_limit, 0, 0 ],
      cparm = [ 0, normalize_gains, 0, 0, 0, 0 ],
      ichansel = [ [ 1, channel_count, 1, 1 ] ],  **calib_params )
  dummy_uv.zap()

  # resample solutions to UV database time grid
  in_version = uv.table_highver( 'SN' )
  out_version = in_version + 1
#  if ( interval_count > 1 ):
  if do_amplitude:
    re_sample_solutions( uv, in_version = in_version, out_version = out_version,
        weight_multiplier = 1. / sqrt( float( interval_count ) ), 
        interpolation_method = 'linear', force_reference = True,
        gap_time = solution_interval )
  else:
    try:
      re_sample_solutions( uv, in_version = in_version, out_version = out_version,
          weight_multiplier = 1. / sqrt( float( interval_count ) ),
          interpolation_method = 'spline', force_reference = True,
          gap_time = solution_interval )
    except:
      re_sample_solutions( uv, in_version = in_version, out_version = out_version,
          weight_multiplier = 1. / sqrt( float( interval_count ) ),
          interpolation_method = 'linear', force_reference = True,
          gap_time = solution_interval )
#    uv.zap_table( 'SN', in_version )
#  else:
#    call_aips_task( 'TACOP', indata = uv, invers = in_version, inext = 'SN', ncount = 1,
#        outdata = uv, outvers = out_version )

  return solution_interval

###############################################################################

def image_clean_facets( uv, facets, facet_count, facet_file_name, clean_flux_min, 
    conversion_method = 'DFT', model_version = 0, apply_solutions = True,
    do_sdi_clean = False, restore_components = False, frequency_correction = False,
    gain = 0.1, factor = 0., imagr_params = {} ):

  # filter imagr params
  i_params = imagr_params.copy()
  if ( not restore_components ):
    i_params[ 'bmaj' ] = - 1

  # set parameters
  if frequency_correction:
    dish_diameter = restore_parameter( uv, 'dish_diameter' )
  else:
    dish_diameter = 0.
  cell_size = get_pixel_size( facets, make_absolute = True )
  facet_size = get_image_size( facets )
  channel_count = get_channel_count( uv )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  if apply_solutions:
    docalib = 100
    gainuse = 0
  else:
    docalib = 0
    gainuse = -1
  if do_sdi_clean:
    sdi_param = 0.1
  else:
    sdi_param = 0.

  # remove old model
  if ( model_version != 0 ) and ( table_exists( facets, 'CC', model_version ) ):
    for i in range( 1, facet_count + 1 ):
      facet_i = get_facet( facets, i )
      facet_i.zap_table( 'CC', model_version )

  # image facets
  call_aips_task( 'IMAGR', indata = uv, nchav = channel_count, docalib = docalib,
      gainuse = gainuse, outver = model_version, niter = 100000, flagver = -1,
      outdisk = facets.disk, outname = facets.name, outseq = facets.seq,
      in2disk = uv.disk, cellsize = cell_size, imsize = facet_size, do3dimag = 1,
      flux = 0.95 * clean_flux_min, boxfile = facet_file_name, dotv = 0,
      cmethod = conversion_method, minpatch = facet_size[ 0 ] - 1, overlap = 2,
      gain = gain, nfield = facet_count, bcomp = [ 0 for i in range( 64 ) ],
      imagrprm = [ dish_diameter, 0, 0, sdi_param, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, 0 ], maxpixel = 0, factor = factor, allokay = 1,
      uvsize = [ uv_size, uv_size ], **i_params )

  return

###############################################################################

def selfcal_image_clean_facets( uv, in_facets, facet_file_name = '', 
    facet_list = [], sigma_min = 5., signal_to_noise_min = 10., snr_limit = 2.5,
    improvement_limit = 0.02, selfcal_cycle_min = 2, reference_antenna = 0,
    conversion_method = 'DFT', try_final_amplitude = False, do_sdi_clean = False,
    amplitude_interval = 5., restore_components = False, convergence_limit = -0.1,
    frequency_correction = False, imagr_params = {}, calib_params = {} ):

  # initialise some parameters
  if ( len( facet_list ) > 0 ):
    facet_count = len( facet_list )
    if ( facet_file_name == '' ):
      ok_facet_file_name = restore_parameter( in_facets, 'facet_file_name' )
    else:
      ok_facet_file_name = facet_file_name
    sel_facet_file_name = ok_facet_file_name + '.SEL'
    extract_facet_definitions( ok_facet_file_name, facet_list, sel_facet_file_name )
    facets = get_aips_file( in_facets.disk, 'SEL', in_facets.klass, -1, 'MA' )
    # rename selected facets to form consecutive series
    for i in facet_list:
      facet_i = get_facet( in_facets, i )
      beam_i = get_facet_beam( facet_i )
      facet_i.rename( name = facets.name, 
          klass = facets.klass[ 0 : 3 ] + '%03d' % ( facet_list.index( i ) + 1 ), seq = facets.seq )
      beam_i.rename( name = facets.name, 
          klass = facets.klass[ 0 ] + 'BM%03d' % ( facet_list.index( i ) + 1 ), seq = facets.seq )
  else:
    facet_count = restore_parameter( in_facets, 'facet_count' )
    if ( facet_file_name == '' ):
      sel_facet_file_name = restore_parameter( in_facets, 'facet_file_name' )
    else:
      sel_facet_file_name = facet_file_name
    facets = get_aips_file( in_facets.disk, in_facets.name, in_facets.klass, in_facets.seq, 'MA' )
  channel_count = get_channel_count( uv )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  facets = get_aips_file( facets.disk, facets.name, facets.klass, facets.seq, 'MA' )
  facet_size = get_image_size( facets )
  model_version = facets.table_highver( 'CC' ) + 1

  # TODO: allow multi facet peeling
  if ( facet_count > 1 ):
    raise error( 'peeling multiple facets at once not implemented yet' )

  # determine signal/noise level in facets
  [ avg, noise ] = call_aips_task( 'IMEAN', indata = facets, pixavg = 0., pixstd = 0., 
      outputs = [ 'pixavg', 'pixstd' ] )
  [ avg, noise ] = call_aips_task( 'IMEAN', indata = facets, pixavg = avg, pixstd = noise, 
      pixrange = [ - 10. * noise, 10. * noise  ], outputs = [ 'pixavg', 'pixstd' ] )
  signal = get_model_flux( facets )
  initial_sn = signal / noise
  sn = initial_sn

  print 'signal = %s, noise = %s, S/N = %s' % ( repr( signal ), repr( noise ), repr( sn ) )

  # enter selfcal loop
  sn_improvement = 1.
  selfcal_i = 0
  while ( ( selfcal_i < selfcal_cycle_min ) or ( sn_improvement > improvement_limit ) ):
    selfcal_i = selfcal_i + 1
    last_sn = sn

    print '... selfcal loop %s' % repr( selfcal_i )

    # self-calibrate UV on model
    solution_interval = calibrate_facets( uv, facets, facet_count, sigma_min = 0.,
        signal_to_noise_min = signal_to_noise_min, calib_params = calib_params,
        reference_antenna = reference_antenna, snr_limit = snr_limit,
        conversion_method = conversion_method )
    solution_version = uv.table_highver( 'SN' )

    # image and clean
    image_clean_facets( uv, facets, facet_count, sel_facet_file_name, clean_flux_min = sigma_min * cpb_noise,
        model_version = model_version, do_sdi_clean = do_sdi_clean, restore_components = False,
        frequency_correction = frequency_correction, imagr_params = imagr_params )

    # determine signal/noise level and S/N improvement
    noise = get_facet_rms( facets, remove_components = False )
    signal = get_model_flux( facets )
    sn = signal / noise
    sn_improvement = ( sn / last_sn ) - 1.

    print '... signal = %s, noise = %s, S/N = %s' % ( repr( signal ), repr( noise ), repr( sn ) )
    print '... S/N improvement = %s' % ( repr( sn_improvement ) )

    # only keep solutions when S/N improvement was detected
    if ( selfcal_i == 1 ):
      last_solution_version = solution_version
      last_solution_interval = solution_interval
    elif ( sn_improvement >= 0. ):
      uv.zap_table( 'SN', last_solution_version - 1 )
      uv.zap_table( 'SN', last_solution_version )
      last_solution_version = solution_version
      last_solution_interval = solution_interval
    else: # ( sn_improvement < 0. )
      uv.zap_table( 'SN', solution_version - 1 )
      uv.zap_table( 'SN', solution_version )
      solution_version = last_solution_version
      solution_interval = last_solution_interval
      sn = last_sn
      image_clean_facets( uv, facets, facet_count, sel_facet_file_name,
          clean_flux_min = sigma_min * cpb_noise, model_version = model_version,
          do_sdi_clean = do_sdi_clean, restore_components = False, imagr_params = imagr_params )

  # allow for single amplitude & phase selfcal
  if try_final_amplitude:
    convergence = True
    selfcal_i = selfcal_i + 1
    last_sn = sn

    # apply phase solutions to UV data
    temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )
    call_aips_task( 'SPLIT', indata = uv, docalib = 100, gainuse = solution_version, douvcomp = 0,
        flagver = - 1, outdisk = temp_uv.disk, outclass = temp_uv.klass, outseq = temp_uv.seq )

    # self-calibrate UV on model
    temp_solution_interval = calibrate_facets( temp_uv, facets, facet_count,
        sigma_min = 0., signal_to_noise_min = signal_to_noise_min, 
        calib_params = calib_params, reference_antenna = reference_antenna,
        conversion_method = conversion_method, snr_limit = snr_limit,
        do_amplitude = True, amplitude_interval = amplitude_interval )
    temp_solution_version = temp_uv.table_highver( 'SN' )

    # image and clean
    image_clean_facets( temp_uv, facets, facet_count, sel_facet_file_name, 
        clean_flux_min = sigma_min * cpb_noise, model_version = model_version, do_sdi_clean = do_sdi_clean,
        restore_components = False, imagr_params = imagr_params )

    # determine noise level and noise improvement
    noise = get_facet_rms( facets, remove_components = False )
    signal = get_model_flux( facets )
    sn = signal / noise
    sn_improvement = ( sn / last_sn ) - 1.

    # only keep solutions when noise improvement was detected
    if ( sn_improvement >= 0. ):
      # combine tables
      call_aips_task( 'TACOP', indata = temp_uv, inext = 'SN', invers = temp_solution_version, ncount = 1,
          outdata = uv, outvers = 0 )
      temp_solution_version = uv.table_highver( 'SN' )
      combine_solutions( uv, in_version_1 = solution_version, in_version_2 = temp_solution_version )
      solution_version = uv.table_highver( 'SN' )
      uv.zap_table( 'SN', last_solution_version - 1 )
      uv.zap_table( 'SN', last_solution_version )
    else: # ( sn_improvement < 0. )
      sn = last_sn
      image_clean_facets( uv, facets, facet_count, sel_facet_file_name,
          clean_flux_min = sigma_min * cpb_noise, model_version = model_version,
          do_sdi_clean = do_sdi_clean, restore_components = False,
          frequency_correction = frequency_correction, imagr_params = imagr_params )

    # clean up
    temp_uv.zap()

  # save solutions to all facets
  # store parameters
  # remove solutions from UV data
  if ( not ( try_final_amplitude and ( sn_improvement >= 0. ) ) ):
    call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = solution_version - 1, ncount = 1,
        outdata = facets, outvers = 0 )
    uv.zap_table( 'SN', solution_version - 1 )
  call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = solution_version, ncount = 1,
      outdata = facets, outvers = 0 )
  uv.zap_table( 'SN', solution_version )
  store_parameter( facets, 'solution_interval', solution_interval )

  # if needed, rename facets back to original
  if ( len( facet_list ) > 0 ):
    for i in facet_list:
      facet_i = get_facet( facets, facet_list.index( i ) + 1 )
      beam_i = get_facet_beam( facet_i )
      facet_i.rename( name = in_facets.name, klass = in_facets.klass[ 0 : 3 ] + '%03d' % ( i ),
          seq = in_facets.seq )
      beam_i.rename( name = in_facets.name, klass = in_facets.klass[ 0 ] + 'BM%03d' % ( i ),
          seq = in_facets.seq )

  # restore clean components to facets
  if restore_components:
    rst_facets = restore_model_components( in_facets, facet_list = facet_list, cross_restore = False,
        imagr_params = imagr_params )
    for i in facet_list:
      in_facet_i = get_facet( in_facets, i )
      in_facet_i.zap()
      rst_facet_i = get_facet( rst_facets, i )
      rst_facet_i.rename( name = in_facet_i.name, klass = in_facet_i.klass, seq = in_facet_i.seq )

  # delete temp facet name
  if ( len( facet_list ) > 0 ):
    remove_file( sel_facet_file_name )

  overall_sn_improvement = ( sn / initial_sn ) - 1.
  if ( overall_sn_improvement < convergence_limit ):
    return False
  else:
    return True

###############################################################################

def selfcal_image_clean_facet( uv, facet, facet_file_name, sigma_min = 5.,
    signal_to_noise_min = 10., improvement_limit = 0.02,
    selfcal_cycle_min = 4, reference_antenna = 0, conversion_method = 'DFT',
    do_sdi_clean = False, restore_components = False, re_center_model = False,
    imagr_params = {}, calib_params = {}, convergence_limit = - 0.2,
    frequency_correction = False, print_info = False, snr_limit = 2.5,
    amplitude_interval = 5., offset_ratio_max = 2., allow_pixel_peak = True,
    flux_rejection_ratio = 0.5, phase_interval_min = 0., resolve_power = 0.,
    amplitude_noise_factor = 1.5, amplitude_snr = 300. ):
  
  # initialise some parameters
  channel_count = get_channel_count( uv )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  facet_size = get_image_size( facet )
  model_version = facet.table_highver( 'CC' ) + 1
  facet_radec = get_radec( facet )

  # determine signal/noise level in facets
  signal = get_model_flux( facet )
  initial_signal = signal
  [ avg, noise ] = call_aips_task( 'IMEAN', indata = facet, pixavg = 0., 
      pixstd = 0., outputs = [ 'pixavg', 'pixstd' ] )
  [ avg, noise ] = call_aips_task( 'IMEAN', indata = facet, pixavg = avg,
      pixstd = noise, pixrange = [ - 10. * noise, 10. * noise  ], 
      outputs = [ 'pixavg', 'pixstd' ] )
  initial_sn = signal / noise
  sn = initial_sn
  if print_info:
    print 'signal = %s, noise = %s, S/N = %s' % ( repr( signal ), repr( noise ), 
        repr( sn ) )

  # get selected facet
  i = get_facet_number( facet )
  sel_facet_file_name = facet_file_name + '.SEL'
  extract_facet_definitions( facet_file_name, [ i ], sel_facet_file_name )
  beam = get_facet_beam( facet )
  sel_facet = get_aips_file( facet.disk, 'SEL', 'ICL001', - 1, 'MA' )
  sel_beam = get_facet_beam( sel_facet )
  call_aips_task( 'MOVE', indata = facet, outdata = sel_facet,
      userid = get_aips_userid() )
  call_aips_task( 'MOVE', indata = beam, outdata = sel_beam,
      userid = get_aips_userid() )

  # enter selfcal loop
  sn_improvement = 1.
  selfcal_i = 0
  while ( ( selfcal_i < selfcal_cycle_min ) or 
      ( sn_improvement > improvement_limit ) ):
    selfcal_i = selfcal_i + 1
    last_sn = sn
    if print_info:
      print '... selfcal loop %s' % repr( selfcal_i )

    # determine total / peak flux ratio
    total_flux = get_model_flux( sel_facet )
    rst_facet = restore_model_components( sel_facet, facet_list = [ 1 ],
        cross_restore = False, imagr_params = imagr_params )
    fit_results = fit_gaussian_to_peak( rst_facet, return_double_fit = True,
        offset_ratio_max = offset_ratio_max )
    if ( ( len( fit_results ) == 0 ) and allow_pixel_peak ):
      # use maximum pixel position
      if print_info:
        print 'WARNING: using maximum pixel position instead'
      fill_facet( rst_facet, blank_edge = ( min( facet_size ) - 10  ) / 2,
          do_edge_circle = True )
      max_results = get_image_maximum( rst_facet )
      fit_results = [ [ [ float( max_results[ 1 ][ 0 ] ), 
          float( max_results[ 1 ][ 1 ] ) ], max_results[ 0 ], [ 0., 0., 0. ] ] ]
    rst_facet.zap()
    if ( len( fit_results ) == 0 ):
      if re_center_model:
        if print_info:
          print ( 'WARNING: gaussian fit to peeling source failed while ' + 
              're-centering model' )
        sn = 1.
        sn_improvement = - 100.
        break
      else:
        if print_info:
          print ( 'WARNING: gaussian fit to peeling source failed while ' + 
              'determining total / peak ratio' )
        total_peak_ratio = 1.
    else:
      [ fit_pos, fit_peak, fit_beam ] = fit_results[ 0 ]
      total_peak_ratio = total_flux / fit_peak
      if print_info:
        print '... total / peak ratio = %s' % ( repr( total_peak_ratio ) )

    # center facet on flux peak
    if re_center_model:
      if ( selfcal_i == 1 ):
        fit_count = len( fit_results )
      elif ( len( fit_results ) != fit_count ):
        if print_info:
          print ( 'WARNING: number of gaussians in fit to peeling source ' + 
              'changed while re-centering source model' )
        sn = 1.
        sn_improvement = - 100.
        break
      fit_radec = calculate_source_radec( sel_facet, fit_pos )
      if ( len( fit_results ) > 1 ):
        [ fit_pos_2, fit_peak_2, fit_beam_2 ] = fit_results[ 1 ]
        fit_radec_2 = calculate_source_radec( sel_facet, fit_pos_2 )
#        [ r, phi ] = calculate_angular_separation( fit_radec, fit_radec_2 )
#        r = r * fit_peak_2 / ( fit_peak + fit_peak_2 )
#        fit_radec = calculate_offset_position( fit_radec, r, phi )
        if ( fit_peak_2 > fit_peak ):
          fit_radec = fit_radec_2
      set_radec( sel_facet, fit_radec, shift_model = True )
      set_radec( sel_facet, facet_radec, shift_model = False )
    
    # self-calibrate UV on model
    # minimum SNR is increased to reflect extendedness of source
    resolve_factor = max( 1., total_peak_ratio )**resolve_power
    solution_interval = calibrate_facets( uv, sel_facet, 1, sigma_min = 0.,
        signal_to_noise_min = signal_to_noise_min * resolve_factor,
        calib_params = calib_params, reference_antenna = reference_antenna, 
        conversion_method = conversion_method, snr_limit = snr_limit,
        phase_interval_min = phase_interval_min )
    solution_version = uv.table_highver( 'SN' )
    
    # image and clean
    image_clean_facets( uv, sel_facet, 1, sel_facet_file_name, 
        clean_flux_min = sigma_min * cpb_noise, model_version = model_version,
        do_sdi_clean = do_sdi_clean, restore_components = False,
        frequency_correction = frequency_correction, imagr_params = imagr_params )
    
    # determine signal/noise level and S/N improvement
    [ avg, noise ] = call_aips_task( 'IMEAN', indata = sel_facet, pixavg = 0.,
        pixstd = 0., outputs = [ 'pixavg', 'pixstd' ] )
    [ avg, noise ] = call_aips_task( 'IMEAN', indata = sel_facet, pixavg = avg,
        pixstd = noise, pixrange = [ - 10. * noise, 10. * noise  ],
        outputs = [ 'pixavg', 'pixstd' ] )
    signal = get_model_flux( sel_facet )
    flux_ratio = signal / initial_signal
    if ( flux_ratio < flux_rejection_ratio ):
      if print_info:
        print 'WARNING: source flux dropped below threshold, aborting selfcal'
      sn = 1.
      sn_improvement = - 100.
      break
    sn = signal / noise
    sn_improvement = ( sn / last_sn ) - 1.
    if print_info:
      print '... signal = %s, noise = %s, S/N = %s' % ( repr( signal ),
          repr( noise ), repr( sn ) )
      print '... S/N improvement = %s' % ( repr( sn_improvement ) )

    # only keep solutions when S/N improvement was detected
    if ( selfcal_i == 1 ):
      last_solution_version = solution_version
      last_solution_interval = solution_interval
    elif ( ( selfcal_i <= selfcal_cycle_min ) or ( sn_improvement >= 0. ) ):
      uv.zap_table( 'SN', last_solution_version - 1 )
      uv.zap_table( 'SN', last_solution_version )
      last_solution_version = solution_version
      last_solution_interval = solution_interval
    else:
      uv.zap_table( 'SN', solution_version - 1 )
      uv.zap_table( 'SN', solution_version )
      solution_version = last_solution_version
      solution_interval = last_solution_interval
      sn = last_sn
      image_clean_facets( uv, sel_facet, 1, sel_facet_file_name,
          clean_flux_min = sigma_min * cpb_noise, model_version = model_version,
          do_sdi_clean = do_sdi_clean, restore_components = False,
          frequency_correction = frequency_correction, imagr_params = imagr_params )
  
  # allow for single amplitude & phase selfcal
  if ( ( noise > amplitude_noise_factor * cpb_noise ) and 
      ( sn > amplitude_snr ) and ( sn_improvement > - 99. ) ):
    while True: # so we can use break command
      
      convergence = True
      selfcal_i = selfcal_i + 1
      last_sn = sn
      if print_info:
        print '... (a&p) selfcal loop %s' % repr( selfcal_i )
      
      # center facet on flux peak
      if re_center_model:
        rst_facets = restore_model_components( sel_facet, facet_list = [ 1 ],
            cross_restore = False, imagr_params = imagr_params )
        rst_facet = get_facet( rst_facets, 1 )
        fit_results = fit_gaussian_to_peak( rst_facet, return_double_fit = True,
            offset_ratio_max = offset_ratio_max )
        if ( ( len( fit_results ) == 0 ) and allow_pixel_peak ):
          # use maximum pixel position
          if print_info:
            print 'WARNING: using maximum pixel position instead'
          fill_facet( rst_facet, blank_edge = ( min( facet_size ) - 10  ) / 2,
              do_edge_circle = True )
          max_results = get_image_maximum( rst_facet )
          fit_results = [ [ [ float( max_results[ 1 ][ 0 ] ),
              float( max_results[ 1 ][ 1 ] ) ], max_results[ 0 ], [ 0., 0., 0. ] ] ]
        rst_facet.zap()
        if ( len( fit_results ) != fit_count ):
          if print_info:
            print ( 'WARNING: number of gaussians in peeling source fit ' +
                'changed while re-centering source model' )
          sn = 1.
          sn_improvement = - 100.
        
        [ fit_pos, fit_peak, fit_beam ] = fit_results[ 0 ]
        fit_radec = calculate_source_radec( sel_facet, fit_pos )
        if ( len( fit_results ) > 1 ):
          [ fit_pos_2, fit_peak_2, fit_beam_2 ] = fit_results[ 1 ]
          fit_radec_2 = calculate_source_radec( sel_facet, fit_pos_2 )
#          [ r, phi ] = calculate_angular_separation( fit_radec, fit_radec_2 )
#          r = r * fit_peak_2 / ( fit_peak + fit_peak_2 )
#          fit_radec = calculate_offset_position( fit_radec, r, phi )
          if ( fit_peak_2 > fit_peak ):
            fit_radec = fit_radec_2
        set_radec( sel_facet, fit_radec, shift_model = True )
        set_radec( sel_facet, facet_radec, shift_model = False )
      
      # apply phase solutions to UV data
#      temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', - 1, 'UV' )
#      call_aips_task( 'SPLIT', indata = uv, docalib = 100, douvcomp = 0,
#          gainuse = solution_version, flagver = - 1, outdisk = temp_uv.disk,
#          outclass = temp_uv.klass, outseq = temp_uv.seq )
      temp_uv = apply_solution_table( uv )
      
      # self-calibrate UV on model
      temp_solution_interval = calibrate_facets( temp_uv, sel_facet, 1,
          sigma_min = 0., signal_to_noise_min = signal_to_noise_min,
          calib_params = calib_params, reference_antenna = reference_antenna,
          conversion_method = conversion_method, do_amplitude = True,
          amplitude_interval = amplitude_interval, snr_limit = snr_limit )
      temp_solution_version = temp_uv.table_highver( 'SN' )
      
      # image and clean
      image_clean_facets( temp_uv, sel_facet, 1, sel_facet_file_name,
          clean_flux_min = sigma_min * cpb_noise, model_version = model_version,
          do_sdi_clean = do_sdi_clean, restore_components = False,
          frequency_correction = frequency_correction, imagr_params = imagr_params )
      
      # determine noise level and noise improvement
      [ avg, noise ] = call_aips_task( 'IMEAN', indata = sel_facet, pixavg = 0.,
          pixstd = 0., outputs = [ 'pixavg', 'pixstd' ] )
      [ avg, noise ] = call_aips_task( 'IMEAN', indata = sel_facet, pixavg = avg,
          pixstd = noise, pixrange = [ - 10. * noise, 10. * noise  ],
          outputs = [ 'pixavg', 'pixstd' ] )
      signal = get_model_flux( sel_facet )
      flux_ratio = signal / initial_signal
      if ( flux_ratio < flux_rejection_ratio ):
        if print_info:
          print 'WARNING: source flux dropped below threshold, aborting selfcal'
        sn = 1.
        sn_improvement = - 100.
        break
      sn = signal / noise
      sn_improvement = ( sn / last_sn ) - 1.
      if print_info:
        print '... signal = %s, noise = %s, S/N = %s' % ( repr( signal ),
            repr( noise ), repr( sn ) )
        print '... S/N improvement = %s' % ( repr( sn_improvement ) )
      
      # only keep solutions when noise improvement was detected
      if ( sn_improvement >= 0. ):
        # combine tables
        call_aips_task( 'TACOP', indata = uv, inext = 'SN', ncount = 1,
            invers = solution_version, outdata = temp_uv, outvers = 0 )
        combine_solutions( temp_uv, force_match = True )
        call_aips_task( 'TACOP', indata = temp_uv, inext = 'SN', ncount = 1,
            invers = 0, outdata = uv, outvers = 0 )
        solution_version = uv.table_highver( 'SN' )
        uv.zap_table( 'SN', last_solution_version - 1 )
      else:
        sn = last_sn
        image_clean_facets( uv, sel_facet, 1, sel_facet_file_name,
            clean_flux_min = sigma_min * cpb_noise, model_version = model_version,
            do_sdi_clean = do_sdi_clean, restore_components = False,
            frequency_correction = frequency_correction, imagr_params = imagr_params )
      
      # clean up
      temp_uv.zap()
      break
  
  # do final check
  overall_sn_improvement = ( sn / initial_sn ) - 1.
  if ( overall_sn_improvement < convergence_limit ):
    try:
      uv.zap_table( 'SN', solution_version - 1 )
    except:
      pass
    try:
      uv.zap_table( 'SN', solution_version )
    except:
      pass
    sel_facet.zap()
    sel_beam.zap()
    remove_file( sel_facet_file_name )
    return False
  
  # save solutions to facet
  # store parameters
  # remove solutions from UV data
  call_aips_task( 'TACOP', indata = uv, inext = 'SN', ncount = 1,
      invers = solution_version - 1, outdata = sel_facet, outvers = 0 )
  call_aips_task( 'TACOP', indata = uv, inext = 'SN', invers = solution_version,
      ncount = 1, outdata = sel_facet, outvers = 0 )
  uv.zap_table( 'SN', solution_version - 1 )
  uv.zap_table( 'SN', solution_version )
  store_parameter( sel_facet, 'solution_interval', solution_interval )
  
  # restore clean components to facet
  if restore_components:
    rst_facet = restore_model_components( sel_facet, facet_list = [ 1 ],
        cross_restore = False, imagr_params = imagr_params )
    sel_facet.zap()
    rst_facet.rename( name = sel_facet.name, klass = sel_facet.klass,
        seq = sel_facet.seq )
  
  # delete temp facet file
  remove_file( sel_facet_file_name )
  
  # replace original facet 
  facet.zap()
  beam.zap()
  sel_facet.rename( name = facet.name, klass = facet.klass, seq = facet.seq )
  sel_beam.rename( name = beam.name, klass = beam.klass, seq = beam.seq )
  return True

###############################################################################

def peel_s_facets( uv, facets, sigma_min = 5., signal_to_noise_min = 5., 
    solution_interval_max = 60., improvement_limit = 0.02, apply_solutions = True,
    reference_antenna = 0, imagr_params = {}, calib_params = {}, snr_limit = 2.5 ):

  # calculate required signal per integration interval
#  time_count = restore_parameter( uv, 'time_count' )
  time_count = len( get_time_list( uv ) )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  integration_time = restore_parameter( uv, 'integration_time' )
  facet_file_name = restore_parameter( facets, 'facet_file_name' )
  noise_per_interval_min = cpb_noise * sqrt( float( time_count ) )
  signal_per_interval_min = signal_to_noise_min * noise_per_interval_min
  signal_min = signal_per_interval_min / sqrt( floor( solution_interval_max / integration_time ) )

  # get list of bright enough, compact enough sources to peel
  source_list = get_source_list( facets, signal_min )

  # make a copy of input UV
  peel_uv = get_aips_file( uv.disk, uv.name, 'PEELS', -1, 'UV' )
  call_aips_task( 'MOVE', indata = uv, outdata = peel_uv, userid = get_aips_userid(), opcode = '' )

  # if model is bright enough, try peeling it
  if ( len( source_list ) != 0 ):

    # split UV to get timeranges where source is above horizon
    rts_times = calculate_rise_transit_set_times( uv, radec = get_radec( facets ) )
    [ up_uv, down_uv ] = split_uv_on_time_range( uv, rts_times )

    if ( up_uv != None ):
      peel_uv.zap()

      # add model back to UV
      add_up_uv = add_model( up_uv, facets, sigma = 0., apply_solutions = apply_solutions,
          keep_solutions = True, flag_solutions = False )
      up_uv.zap()

      # use an initial point source model for peeling
      total_model_flux = 0.
      facet_count = restore_parameter( facets, 'facet_count' )
      for i in range( 1, facet_count + 1 ):
        facet_i = get_facet( facets, i )
        total_model_flux = total_model_flux + get_model_flux( facet_i )
      for i in range( 1, facet_count + 1 ):
        facet_i = get_facet( facets, i )
        facet_pixel_ref = get_pixel_reference( facet_i )
        if ( i == 1 ):
          call_aips_task( 'CCMOD', indata = facets, invers = -1, opcode = 'POIN', 
              flux = total_model_flux, pixxy = facet_pixel_ref )
        else:
          call_aips_task( 'CCMOD', indata = facets, invers = -1, opcode = 'POIN',
              flux = 0., pixxy = facet_pixel_ref )

      # selfcal and image facets
      selfcal_image_clean_facets( add_up_uv, facets, sigma_min = sigma_min,
          reference_antenna = reference_antenna, do_sdi_clean = False,
          signal_to_noise_min = signal_to_noise_min, snr_limit = snr_limit,
          improvement_limit = improvement_limit, imagr_params = imagr_params,
          calib_params = calib_params )

      # subtract updated model from UV
      peel_up_uv = subtract_model( add_up_uv, facets, sigma = sigma_min,
          apply_solutions = True, keep_solutions = True, flag_solutions = True )
      add_up_uv.zap()

      # combine UV
      dummy_uv = merge_uv( peel_up_uv, down_uv )
      peel_up_uv.zap()
      dummy_uv.rename( name = peel_uv.name, klass = peel_uv.klass, seq = peel_uv.seq )
      peel_uv = get_aips_file( peel_uv.disk, peel_uv.name, peel_uv.klass, peel_uv.seq, 'UV' )

    if ( down_uv != None ):
      down_uv.zap()

  return peel_uv

###############################################################################

def peel_a_facets( uv, facets, sigma_min = 5., signal_to_noise_min = 10., 
    solution_interval_max = 60., improvement_limit = 0.05, apply_solutions = True,
    reference_antenna = 0, imagr_params = {}, calib_params = {}, snr_limit = 2.,
    signal_multiplier = 10., print_info = False, try_final_amplitude = False,
    amplitude_interval = 5., convergence_limit = -0.5, conversion_method = 'DFT',
    phase_interval_min = 0., resolve_power = 0.5 ):
  
  facet_file_name = restore_parameter( facets, 'facet_file_name' )
  
  # calculate required signal per integration interval
#  time_count = restore_parameter( uv, 'time_count' )
  time_count = len( get_time_list( uv ) )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  integration_time = restore_parameter( uv, 'integration_time' )
  facet_file_name = restore_parameter( facets, 'facet_file_name' )
  noise_per_interval_min = cpb_noise * sqrt( float( time_count ) )
  signal_per_interval_min = signal_to_noise_min * noise_per_interval_min
  signal_min = signal_per_interval_min / sqrt( floor( solution_interval_max / 
      integration_time ) )
  
  # allow for a broader initial selection of sources, as the initial flux measurement
  # might be affected by ionospheric phase errors
  # fainter sources which need too long a calibration solution interval are rejected later on
  signal_min = signal_min / signal_multiplier
  if print_info:
    print '... minimum peeling peak flux = %s Jy' % ( repr( signal_min ) )
  
  # get list of bright enough, compact enough sources to peel
  source_list = get_source_list( facets, signal_min, area_ratio_max = None )

  
  # make a copy of input UV
  peel_uv = get_aips_file( uv.disk, uv.name, 'PEELA', - 1, 'UV' )
  call_aips_task( 'MOVE', indata = uv, outdata = peel_uv, 
      userid = get_aips_userid(), opcode = '' )
  
  # if some sources are bright enough, try to peel them
  peel_facets = get_aips_file( facets.disk, 'PEELA', facets.klass, - 1, 'MA' )
  peel_facet_file_name = facet_file_name + '.PEELA'
  j = 0
  facet_list = []
  facets_done = []
  for source in source_list:
    # split UV to get timeranges where source is above horizon
    [ i, pos, peak_flux, int_flux, shape ] = source
    if ( i in facets_done ):
      continue
    facet_i = get_facet( facets, i )
    rts_times = calculate_rise_transit_set_times( peel_uv, radec = get_radec( facet_i ) )
    [ up_uv, down_uv ] = split_uv_on_time_range( peel_uv, rts_times )
    
    if ( ( up_uv != None ) and ( not i in facet_list ) ):
      
      # copy facet and beam
      j = j + 1
      facet_list.append( i )
      beam_i = get_facet_beam( facet_i )
      facet_j = get_facet( peel_facets, j )
      beam_j = get_facet_beam( facet_j )
      call_aips_task( 'MOVE', indata = facet_i, outdata = facet_j, 
          userid = get_aips_userid() )
      call_aips_task( 'MOVE', indata = beam_i, outdata = beam_j, 
          userid = get_aips_userid() )
      if ( j == 1 ):
        call_aips_task( 'TACOP', indata = get_facet( facets, 1 ), inext = 'PS', 
            invers = 0, ncount = 1, outdata = facet_j, outvers = 0 )
      extract_facet_definitions( facet_file_name, facet_list, peel_facet_file_name )
#      store_parameter( peel_facets, 'facet_count', j )
      
      # add model back to UV
      add_up_uv = add_model( up_uv, peel_facets, facet_list = [ j ], sigma = 0.,
          apply_solutions = apply_solutions, keep_solutions = True,
          flag_solutions = False )
      up_uv.zap()
      
      # use an initial central point source model for peeling
      facet_pixel_ref = get_pixel_reference( facet_j )
      call_aips_task( 'CCMOD', indata = facet_j, invers = - 1, opcode = 'POIN', 
          flux = int_flux, pixxy = facet_pixel_ref )
      
      # selfcal and image facets
      converge = selfcal_image_clean_facet( add_up_uv, facet_j, 
          facet_file_name = peel_facet_file_name, snr_limit = snr_limit,
          sigma_min = sigma_min, do_sdi_clean = False, re_center_model = False, 
          signal_to_noise_min = signal_to_noise_min, print_info = print_info, 
          try_final_amplitude = try_final_amplitude, restore_components = True,
          amplitude_interval = amplitude_interval, flux_rejection_ratio = 0.2,
          improvement_limit = improvement_limit, resolve_power = resolve_power,
          convergence_limit = convergence_limit, frequency_correction = False,
          conversion_method = conversion_method, imagr_params = imagr_params,
          calib_params = calib_params, reference_antenna = reference_antenna,
          phase_interval_min = phase_interval_min )
      
      if converge:
        facets_done.append( i )
        solution_interval = 60. * restore_parameter( facet_j, 'solution_interval' )
        if ( solution_interval > solution_interval_max ):
          converge = False
      
      if converge:
        peel_uv.zap()
        
        # subtract updated model from UV
        peel_up_uv = subtract_model( add_up_uv, peel_facets, facet_list = [ j ],
          sigma = sigma_min, apply_solutions = True, keep_solutions = True,
          flag_solutions = True )
        add_up_uv.zap()
        
        # combine UV
        if ( down_uv != None ):
          dummy_uv = merge_uv( peel_up_uv, down_uv )
          peel_up_uv.zap()
          down_uv.zap()
          dummy_uv.rename( name = peel_uv.name, klass = peel_uv.klass, seq = peel_uv.seq )
        else:
          peel_up_uv.rename( name = peel_uv.name, klass = peel_uv.klass, seq = peel_uv.seq )
      
      else:
        add_up_uv.zap()
        if ( down_uv != None ):
          down_uv.zap()
        facet_j.zap()
        beam_j.zap()
        remove_facet( peel_facet_file_name, j )
        j = j - 1
        facet_list = facet_list[ : - 1 ]
    
    else:
      if ( up_uv != None ):
        up_uv.zap()
      if ( down_uv != None ):
        down_uv.zap()
  
  if ( j == 0 ):
    peel_facets = None
  else:
    store_parameter( peel_facets, 'facet_count', j )
  
  return [ peel_uv, peel_facets ]

###############################################################################

def peel_o_facets( uv, facets, sigma_min = 5., signal_to_noise_min = 8.,
    solution_interval_max = 60., try_final_amplitude = False, snr_limit = 2.5,
    improvement_limit = 0.02, apply_solutions = True, reference_antenna = 0,
    imagr_params = {}, calib_params = {} ):

  # calculate required signal per integration interval
#  time_count = restore_parameter( uv, 'time_count' )
  time_count = len( get_time_list( uv ) )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  integration_time = restore_parameter( uv, 'integration_time' )
  facet_file_name = restore_parameter( facets, 'facet_file_name' )
  noise_per_interval_min = cpb_noise * sqrt( float( time_count ) )
  signal_per_interval_min = signal_to_noise_min * noise_per_interval_min
  signal_min = signal_per_interval_min / sqrt( floor( solution_interval_max / 
      integration_time ) )

  # get list of bright enough, compact enough sources to peel
  source_list = get_source_list( facets, signal_min, area_ratio_max = 3.**2 )

  # make a copy of input UV
  peel_uv = get_aips_file( uv.disk, uv.name, 'PEELO', -1, 'UV' )
  call_aips_task( 'MOVE', indata = uv, outdata = peel_uv,
      userid = get_aips_userid(), opcode = '' )

  # if some sources are bright enough, try to peel them
  for source in source_list:
    [ i, pos, peak_flux, int_flux, shape ] = source
    facet_i = get_facet( facets, i )

    # add model back to UV
    add_uv = add_model( peel_uv, facets, facet_list = [ i ], sigma = 0., 
        apply_solutions = apply_solutions, keep_solutions = True,
        flag_solutions = False )
    peel_uv.zap()

    # use an initial central point source model for peeling
    facet_pixel_ref = get_pixel_reference( facet_i )
    call_aips_task( 'CCMOD', indata = facet_i, invers = -1, opcode = 'POIN', 
        flux = int_flux, pixxy = facet_pixel_ref )

    # selfcal and image facet
    selfcal_image_clean_facets( add_uv, facets, facet_list = [ i ], 
        sigma_min = sigma_min, signal_to_noise_min = signal_to_noise_min,
        try_final_amplitude = try_final_amplitude, do_sdi_clean = False,
        improvement_limit = improvement_limit, imagr_params = imagr_params,
        reference_antenna = reference_antenna, calib_params = calib_params,
        snr_limit = snr_limit )

    # subtract updated model from UV
    dummy_uv = subtract_model( add_uv, facets, facet_list = [ i ], sigma = sigma_min,
        apply_solutions = True, keep_solutions = True, flag_solutions = True )
    add_uv.zap()
    dummy_uv.rename( name = peel_uv.name, klass = peel_uv.klass, seq = peel_uv.seq )
    peel_uv = get_aips_file( peel_uv.disk, peel_uv.name, peel_uv.klass, peel_uv.seq, 'UV' )

  return peel_uv

###############################################################################

def replace_model_solutions_with_peel_solutions( uv, facets, peel_facets,
    facet_list = [], max_beam_offset = sqrt( 2. ), version = 0,
    re_grid_solutions = True, normalize_amplitudes = False ):
  if ( len( facet_list ) == 0 ):
    peel_facet_count = restore_parameter( peel_facets, 'facet_count' )
    peel_facet_list = range( 1, 1 + peel_facet_count )
  else:
    peel_facet_count = len( facet_list )
    peel_facet_list = [ i for i in facet_list ]
  if re_grid_solutions:
    time_list = get_time_list( uv )
    gap_time = restore_parameter( uv, 'integration_time' ) / ( 4. * 60. )
  for i in peel_facet_list:
    peel_facet_i = get_facet( peel_facets, i )
    peel_radec = get_radec( peel_facet_i )
    source_facet_list = find_source_facets( facets, peel_radec,
        primary_facet_only = True )
    [ bmaj, bmin, bpa ] = get_beam_size( peel_facet_i )
    if ( len( source_facet_list ) > 0 ):
      [ j, pos ] = source_facet_list[ 0 ]
      facet_j = get_facet( facets, j )
      radec = get_radec( facet_j )
      [ radius, angle ] = calculate_angular_separation( peel_radec, radec )
      if ( 3600. * radius < max_beam_offset * bmaj ):
        if normalize_amplitudes:
          snver = uv.table_highver( 'SN' ) + 1
          call_aips_task( 'TACOP', indata = peel_facet_i, inext = 'SN',
              invers = version, ncount = 1, outdata = uv, outvers = snver )
          call_aips_task( 'SNCOR', indata = uv, snver = snver, opcode = 'NORM' )
          call_aips_task( 'TACOP', indata = uv, inext = 'SN',
              invers = 0, ncount = 1, outdata = facet_j, outvers = 0 )
          for ver in range( snver, 1 + uv.table_highver( 'SN' ) ):
            uv.zap_table( 'SN', ver )
        else:
          call_aips_task( 'TACOP', indata = peel_facet_i, inext = 'SN',
              invers = version, ncount = 1, outdata = facet_j, outvers = 0 )
        if re_grid_solutions:
          re_sample_solutions( facet_j, gap_time = gap_time, time_list = time_list,
          interpolation_method = 'nearest', force_reference = True )
  return

###############################################################################

def peel_pbo_facets( uv, facets, sigma_min = 5., signal_to_noise_min = 10.,
    apply_solutions = True, solution_interval_max = 60., print_info = False,
    restore_input_components = True, use_catalog = True, calib_params = {},
    relative_solutions = False, max_relativity = 3, keep_solutions = True,
    clean_box_ratio = 5., double_search_ratio = 10., improvement_limit = 0.02,
    signal_multiplier = sqrt( 2. ), input_catalog = [], assoc_radius = 1. / 60.,
    assoc_ratio = 1.e9, frequency_correction = False, conversion_method = 'DFT',
    max_peel_count = 100, convergence_limit = - 0.2, imagr_params = {},
    phase_interval_min = 0., reject_multiples = True, offset_ratio_max = 1.,
    reference_antenna = 0, resolve_power = 0.5, snr_limit = 2.5, gain = 0.1,
    restore_output_components = True, amplitude_interval = 5., factor = 0.,
    amplitude_noise_factor = 1.5, amplitude_snr = 300., clean_box_radius = 10 ):
# try_final_amplitude = False, 
  
  # calculate required signal per integration interval
#  time_count = restore_parameter( uv, 'time_count' )
  time_count = len( get_time_list( uv ) )
  cpb_noise = restore_parameter( uv, 'cpb_noise' )
  integration_time = restore_parameter( uv, 'integration_time' )
  facet_file_name = restore_parameter( facets, 'facet_file_name' )
  noise_per_interval_min = cpb_noise * sqrt( float( time_count ) )
  signal_per_interval_min = signal_to_noise_min * noise_per_interval_min
  signal_min = signal_per_interval_min / sqrt( solution_interval_max / integration_time )

  # allow for a broader initial selection of sources, as the initial flux measurement
  # might be affected by ionospheric phase errors
  # fainter sources which need too long a calibration solution interval are rejected later on
  signal_min = signal_min / signal_multiplier
  if print_info:
    print '... minimum peeling peak flux = %s Jy' % ( repr( signal_min ) )

  # if requested, restore model components to facets
  # get list of bright enough, compact enough sources to peel
  facet_count = restore_parameter( facets, 'facet_count' )
  if restore_input_components:
    restore_facets = restore_model_components( facets, imagr_params = imagr_params )
    source_list = get_source_list( restore_facets, signal_min, area_ratio_max = 3.**2 )
    for i in range( 1, facet_count + 1 ):
      restore_facet_i = get_facet( restore_facets, i )
      restore_facet_i.zap()
  else:
    source_list = get_source_list( facets, signal_min, area_ratio_max = 3.**2 )
  if print_info:
    print '... found %s possible peeling sources' % ( repr( len( source_list ) ) )

  # detect double component sources
  close_double_list = []
  for s in range( len( source_list ) ): # source_i in source_list:
#    multiple_source = False
    # get source info
    source_i = source_list[ s ]
    [ i, pos_i, peak_flux_i, int_flux_i, shape_i ] = source_i
    facet_i = get_facet( facets, i )
    source_radec_i = calculate_source_radec( facet_i, pos_i )
    [ bmaj_i, bmin_i, bpa_i ] = get_beam_size( facet_i )
    # check if source is close double with other source(s)
    for source_j in source_list[ s + 1 : ]:
      [ j, pos_j, peak_flux_j, int_flux_j, shape_j ] = source_j
      facet_j = get_facet( facets, j )
      source_radec_j = calculate_source_radec( facet_j, pos_j )
      [ radius, angle ] = calculate_angular_separation( source_radec_i, source_radec_j )
      if ( 3600. * radius < double_search_ratio * bmaj_i ):
        close_double_list.append( source_i )
        close_double_list.append( source_j )
#        multiple_source = True
#    if multiple_source:
        if print_info:
          print '... sources at RADECs %s and %s are part of multiple component source' % (
              repr( degdeg_to_hmsdms( source_radec_i, precision = [ 3, 2 ] ) ),
              repr( degdeg_to_hmsdms( source_radec_j, precision = [ 3, 2 ] ) ) )

  # detect multiple (>2) component sources
  multi_source = [ False for source in close_double_list ]
  for s in range( 0, len( close_double_list ), 2 ):
    source_i = close_double_list[ s ]
    source_j = close_double_list[ s + 1 ]
    for k in range( len( close_double_list ) ):
      source_k = close_double_list[ k ]
      if ( not ( k in [ s, s + 1 ] ) ):
        if ( ( source_k == source_i ) or ( source_k == source_j ) ):
          multi_source[ s ] = True
          multi_source[ s + 1 ] = True

  # if requested, keep first double association of multiples
  if ( not reject_multiples ):
    for s in range( 0, len( close_double_list ), 2 ):
      if ( multi_source[ s ] or multi_source[ s + 1 ] ):
        source_i = close_double_list[ s ]
        source_j = close_double_list[ s + 1 ]
        if ( ( not source_i in close_double_list[ 0 : s ] ) and 
             ( not source_j in close_double_list[ 0 : s ] ) ):
          multi_source[ s ] = False
          multi_source[ s + 1 ] = False

  # merge doubles only, add to temporary source list
  temp_double_list = []
  for s in range( 0, len( close_double_list ), 2 ):
    source_i = close_double_list[ s ]
    [ i, pos_i, peak_flux_i, int_flux_i, shape_i ] = source_i
    facet_i = get_facet( facets, i )
    source_radec_i = calculate_source_radec( facet_i, pos_i )
    if multi_source[ s ]:
      if print_info:
        print '... discarding multiple (>2) component source at RADEC = ' + repr(
            degdeg_to_hmsdms( source_radec_i, precision = [ 3, 2 ] ) )
    else:
      source_j = close_double_list[ s + 1 ]
      [ j, pos_j, peak_flux_j, int_flux_j, shape_j ] = source_j
      facet_j = get_facet( facets, j )
      source_radec_j = calculate_source_radec( facet_j, pos_j )
      if print_info:
        print '... combining double components at RADECs = %s and %s' % ( 
            repr( degdeg_to_hmsdms( source_radec_i, precision = [ 3, 2 ] ) ),
            repr( degdeg_to_hmsdms( source_radec_j, precision = [ 3, 2 ] ) ) )
        if ( ( ( peak_flux_i > peak_flux_j ) and ( int_flux_i < int_flux_j ) ) or 
             ( ( peak_flux_i < peak_flux_j ) and ( int_flux_i > int_flux_j ) ) ): 
          print ( '...... WARNING: order of peak fluxes and total fluxes are ' + 
              'different, using peak fluxes' )
      if ( j == i ):
        k = i
        facet_k = facet_i
      else:
        if ( ( pos_i[ 0 ]**2 + pos_i[ 1 ]**2 ) < ( pos_j[ 0 ]**2 + pos_j[ 1 ]**2 ) ):
          k = i
          facet_k = facet_i
          pos_j = calculate_source_position( facet_k, source_radec_j )
        else:
          k = j
          facet_k = facet_j
          pos_i = calculate_source_position( facet_k, source_radec_i )
      # put clean box position in between sources
      [ radius, angle ] = calculate_angular_separation( source_radec_i, source_radec_j )
      r_ik = 0.5 * radius
      clean_radec_k = calculate_offset_position( source_radec_i, r_ik, angle )
      clean_pos_k = calculate_source_position( facet_k, clean_radec_k )
      bmajmin_k = ( 3600. * radius + clean_box_ratio * 
          ( shape_i[ 0 ] + shape_j[ 0 ] ) ) / clean_box_ratio
      shape_k = [ bmajmin_k, bmajmin_k, angle ]
      # put new facet at position of component with highest peak
      int_flux_k = int_flux_i + int_flux_j
      if ( peak_flux_i > peak_flux_j ):
        peak_flux_k = peak_flux_i
        pos_k = pos_i
      else:
        peak_flux_k = peak_flux_j
        pos_k = pos_j
      source_k = [ k, pos_k, peak_flux_k, int_flux_k, shape_k, clean_pos_k ]
      temp_double_list.append( source_k )
#      # look up double in catalog list
#      if use_catalog:
#        for assoc in assoc_list:
#          if ( assoc[ 0 ] == source_i ):
#            assoc_i = assoc[ 1 ]
#            model_i = assoc[ 2 ]
#          if ( assoc[ 0 ] == source_j ):
#            assoc_j = assoc[ 1 ]
#            model_j = assoc[ 2 ]
#        if ( assoc_i or assoc_j ):
#          if ( assoc_i and assoc_j ):
#            [ radius, angle ] = calculate_angular_separation( model_i[ 0 ], model_j[ 0 ] )
#            model_flux_k = model_i[ 1 ] + model_j[ 1 ]
#            r_ik = 0.5 * radius # ( model_j[ 1 ] / model_flux_k ) * radius
#            model_radec_k = calculate_offset_position( model_i[ 0 ], r_ik, angle )
#            assoc_list.append( [ source_k, True, [ model_radec_k, model_flux_k ] ] )
#          elif assoc_i:
#            assoc_list.append( [ source_k, True, model_i ] )
#          else: # assoc_j
#            assoc_list.append( [ source_k, True, model_j ] )
#          if ( ( assoc[ 0 ] == source_k ) and assoc[ 1 ] ):
#            model_k = assoc[ 2 ]
#        else: # not ( assoc_i or assoc_j )
#          assoc_list.append( [ temp_source_k, False, [ source_radec_k, int_flux_k ] ] )

  # remove multiples from original source list
  for s in range( len( close_double_list ) ):
    source_i = close_double_list[ s ]
    try:
      source_list.remove( source_i )
    except:
      pass

  # add double and single sources to temporary source list
  temp_source_list = []
  for s in temp_double_list:
    temp_source_list.append( s )
  for s in source_list:
    [ i, pos, peak_flux, int_flux, shape ] = s
    temp_source_list.append( [ i, pos, peak_flux, int_flux, shape, pos ] )
  if print_info:
    print '... using %s peeling sources' % ( repr( len( temp_source_list ) ) )

  # sort list of candidate peeling sources by model component flux
  # TODO: what about double sources with both components in different facets?
  # for now, still sort by fitted total flux
  source_list = []
  double_list = []
  for s in temp_source_list:
    [ i, pos, peak_flux, int_flux, shape, clean_pos ] = s
    facet_i = get_facet( facets, i )
    [ bmaj, bmin, bpa ] = convert_beam_size( facet_i, beam = shape, to_pixel = True )
    clean_radius = int( ceil( clean_box_ratio * 
        float( max( 2 * clean_box_radius, bmaj ) ) / 2. ) ) + 1
    model_flux = get_model_flux_from_position_area( facet_i, clean_pos, clean_radius )
    source_list.append( [ i, pos, peak_flux, int_flux, model_flux, shape, clean_pos ] )
    if s in temp_double_list:
      double_list.append( [ i, pos, peak_flux, int_flux, model_flux, shape, clean_pos ] )
#  source_list.sort( cmp = lambda a, b: cmp( b[ 4 ], a[ 4 ] ) )
#  source_list.sort( cmp = lambda a, b: cmp( b[ 3 ], a[ 3 ] ) )
  source_list.sort( cmp = lambda a, b: cmp( b[ 2 ], a[ 2 ] ) )

  # associate candidate peeling sources with catalog sources
  assoc_list = []
  if use_catalog:
    if ( len( input_catalog ) > 0 ):
      # copy input list
      model_list = []
      for s in input_catalog:
        model_list.append( [ s[ 0 ][ 0 : 2 ], s[ 1 ] ] )
    else:
      radec = get_radec( uv )
      field_size = restore_parameter( uv, 'field_size' )
      search_radius = 0.6 * field_size
      frequency = get_central_frequency( uv )
      epoch = get_epoch( uv )
      model_list = generate_source_list( radec, search_radius, frequency, epoch = epoch,
          use_nvss = True, use_wenss = False, spectral_index = - 0.8 )
      # TODO: use other catalog with more accurate positions ?
  for source in source_list:
    [ i, pos_i, peak_flux_i, int_flux_i, model_flux_i, shape_i, clean_box_i ] = source
    source_radec = calculate_source_radec( get_facet( facets, i ), pos_i )
    if use_catalog:
      min_flux = int_flux_i / assoc_ratio
      candidate_list = []
      for model in model_list:
        [ model_radec, model_flux ] = model
        if ( model_flux > min_flux ):
#        if True:
          [ radius, angle ] = calculate_angular_separation( source_radec, model_radec )
          if ( radius < assoc_radius ):
            candidate_list.append( [ model, radius ] )
      if ( len( candidate_list ) == 0 ):
        if print_info:
          print ( '... WARNING: found no catalog match at RADEC = %s, ' % 
              ( repr( degdeg_to_hmsdms( source_radec, precision = [ 3, 2 ] ) ) ) + 
              'using measured position' ) 
        assoc_list.append( [ source, False, [ source_radec, int_flux_i ], 0. ] )
      elif ( len( candidate_list ) > 1 ):
        # handle multiple candidates, sort by flux
        candidate_list.sort( cmp = lambda a, b: cmp( b[ 0 ][ 1 ], a[ 0 ][ 1 ] ) )
        candidate_1 = candidate_list[ 0 ]
        candidate_2 = candidate_list[ 1 ]
        if ( source in double_list ):
          # when original is double, pick the closest of the brightest two candidates
          if print_info:
            print ( '... WARNING: found multiple catalog matches at RADEC = %s, ' % 
                ( repr( degdeg_to_hmsdms( source_radec, precision = [ 3, 2 ] ) ) ) + 
                'using closest' )
          if ( candidate_1[ 1 ] < candidate_2[ 1 ] ):
            assoc_list.append( [ source, True ] + candidate_1 )
            model_list.remove( candidate_1[ 0 ] )
          else:
            assoc_list.append( [ source, True ] + candidate_1 )
            model_list.remove( candidate_2[ 0 ] )
        # when original is single, merge brightest two candidate positions with flux as weight
        else:
          if print_info:
            print ( '... WARNING: found multiple catalog matches at RADEC = %s, ' %
                ( repr( degdeg_to_hmsdms( source_radec, precision = [ 3, 2 ] ) ) ) + 
                'using average' )
          [ radius, angle ] = calculate_angular_separation( candidate_1[ 0 ][ 0 ],
              candidate_2[ 0 ][ 0 ] )
          model_flux = candidate_1[ 0 ][ 1 ] + candidate_2[ 0 ][ 1 ]
          radius = radius * candidate_2[ 0 ][ 1 ] / model_flux
          model_radec = calculate_offset_position( candidate_1[ 0 ][ 0 ], radius, angle )
          [ radius, angle ] = calculate_angular_separation( source_radec, model_radec )
          assoc_list.append( [ source, True, [ model_radec, model_flux ], radius ] )
          model_list.remove( candidate_1[ 0 ] )
          model_list.remove( candidate_2[ 0 ] )
      else: # ( len( candidate_list ) == 1 )
        if print_info:
          print ( '... found single catalog match for source at RADEC = %s' %
              ( repr( degdeg_to_hmsdms( source_radec, precision = [ 3, 2 ] ) ) ) )
        assoc_list.append( [ source, True ] + candidate_list[ 0 ] )
        model_list.remove( candidate_list[ 0 ][ 0 ] )
    else:
      assoc_list.append( [ source, False, [ source_radec, int_flux_i ], 0. ] )

  # if sources are bright enough, try to peel them
  facet_file_name = restore_parameter( facets, 'facet_file_name' )
  peel_facet_file_name = facet_file_name + '.PEELPB'
  if file_exists( peel_facet_file_name ):
    remove_file( peel_facet_file_name )
  peel_facets = get_aips_file( facets.disk, 'PEELPB', facets.klass, - 1, 'MA' )

  # make a copy of input UV
  peel_uv = get_aips_file( uv.disk, uv.name, 'PEELPB', - 1, 'UV' )
  call_aips_task( 'MOVE', indata = uv, outdata = peel_uv, 
      userid = get_aips_userid(), opcode = '' )
  while table_exists( peel_uv, 'NI', 0 ):
    peel_uv.zap_table( 'NI', 0 )
  while table_exists( peel_uv, 'OB', 0 ):
    peel_uv.zap_table( 'OB', 0 )
  
  peel_i = 0
  for source_i in source_list:
    old_peel_uv = get_aips_file( peel_uv.disk, peel_uv.name, peel_uv.klass, 
        peel_uv.seq, 'UV' )
    peel_uv = get_aips_file( peel_uv.disk, peel_uv.name, peel_uv.klass, - 1, 'UV' )
    call_aips_task( 'MOVE', indata = old_peel_uv, outdata = peel_uv, 
        userid = get_aips_userid(), opcode = '' )

    peel_i = peel_i + 1
    peel_facet_i = get_facet( peel_facets, peel_i )
    peel_beam_i = get_facet_beam( peel_facet_i )

    # get source info
    [ i, pos, peak_flux, int_flux, model_flux, shape, clean_pos ] = source_i
    facet_i = get_facet( facets, i )
    source_radec = calculate_source_radec( facet_i, pos )
    clean_radec = calculate_source_radec( facet_i, clean_pos )
    [ bmaj, bmin, bpa ] = convert_beam_size( facet_i, beam = shape, to_pixel = True )
    clean_radius = int( ceil( clean_box_ratio * 
        float( max( 2 * clean_box_radius, bmaj ) ) / 2. ) ) + 1
    if print_info:
      if ( pos == clean_pos ):
        print '... peeling single source %s at RADEC %s' % ( repr( peel_i ),
            repr( degdeg_to_hmsdms( source_radec, precision = [ 3, 2 ] ) ) )
      else:
        print '... peeling double source %s at RADEC %s' % ( repr( peel_i ),
            repr( degdeg_to_hmsdms( clean_radec, precision = [ 3, 2 ] ) ) )

    # define new facet with peel source in center
    # add facet definition to new facet file
    peel_facet_i_radec = source_radec
    if use_catalog:
      for assoc in assoc_list:
        [ assoc_source, assoc_found, [ assoc_radec, assoc_flux ], assoc_radius ] = assoc
        if ( assoc_source == source_i ):
          if assoc_found:
            # set facet center to catalog position
            peel_facet_i_radec = assoc_radec
            if print_info:
              print '... replacing source RADEC %s with catalog RADEC %s' % ( 
                  repr( degdeg_to_hmsdms( source_radec, precision = [ 3, 2 ] ) ),
                  repr( degdeg_to_hmsdms( assoc_radec, precision = [ 3, 2 ] ) ) )
          break
    peel_facet_i_size = get_image_size( facet_i )
    peel_facet_i_pixel_ref = get_pixel_reference( facet_i )
    add_facet( peel_facet_file_name, peel_facet_i_radec, peel_facet_i_size, facet_id = peel_i,
        add_clean_box = False )

    # make dirty image of residual facet
    peel_facet_i_pixel_size = get_pixel_size( facet_i, make_absolute = True )
    temp_facet = get_aips_file( peel_facets.disk, 'TEMP', 'ICL001', - 1, 'MA' )
    temp_facet_file_name = peel_facet_file_name + '.TEMP'
    extract_facet_definitions( peel_facet_file_name, [ peel_i ], temp_facet_file_name )
    channel_count = get_channel_count( peel_uv )
    if ( apply_solutions and table_exists( peel_uv, 'SN', 0 ) ):
      sol_switch = 100
      sol_vers = peel_uv.table_highver( 'SN' )
    else:
      sol_switch = 0
      sol_vers = - 1
    uv_size = restore_parameter( peel_uv, 'pb_image_size' )
    call_aips_task( 'IMAGR', indata = peel_uv, nchav = channel_count, flagver = -1,
        outdisk = temp_facet.disk, outname = temp_facet.name, outseq = temp_facet.seq,
        outver = 0, in2disk = peel_uv.disk, cellsize = peel_facet_i_pixel_size, 
        imsize = peel_facet_i_size, overlap = 2, cmethod = conversion_method,
        do3dimag = 1, niter = 100000, flux = 100000., boxfile = temp_facet_file_name, 
        nfield = 1, maxpixel = 0, factor = factor, minpatch = peel_facet_i_size[ 0 ] - 1,
        allokay = 0, docalib = sol_switch, gainuse = sol_vers, dotv = 0, gain = gain,
        imagrprm = [ 0 for iii in range( 20 ) ], uvsize = [ uv_size, uv_size ],
        **imagr_params )
    remove_file( temp_facet_file_name )
    temp_beam = get_facet_beam( temp_facet )
    temp_beam.rename( name = peel_beam_i.name, klass = peel_beam_i.klass,
        seq = peel_beam_i.seq )
    temp_facet.rename( name = peel_facet_i.name, klass = peel_facet_i.klass,
        seq = peel_facet_i.seq )
    model_version = peel_facet_i.table_highver( 'CC' )

    # add clean box to new facet file
    peel_facet_i_clean_box_pos = calculate_source_position( peel_facet_i, clean_radec )
    add_circular_clean_box( peel_facet_file_name, peel_i, peel_facet_i_clean_box_pos,
        clean_radius )

    # look for instances of source in all facets
    overlapping_facet_list = [ i ] + get_facet_overlap( facet_i )
    facet_source_list = find_source_facets( facets, clean_radec,
        facet_list = overlapping_facet_list )
    for facet_source in facet_source_list:
      [ j, pos_j ] =  facet_source
      facet_j = get_facet( facets, j )

      # extract source model and transfer model components to peel facet
      # add source model back to UV
      temp_facet = get_aips_file( peel_facets.disk, 'TEMP', 'ICL001', - 1, 'MA' )
      call_aips_task( 'MOVE', indata = facet_j, outdata = temp_facet, 
          userid = get_aips_userid() )
      [ bmaj, bmin, bpa ] = convert_beam_size( facet_j, beam = shape, to_pixel = True )
      clean_radius = int( ceil( clean_box_ratio * 
          float( max( 2 * clean_box_radius, bmaj ) ) / 2. ) ) + 1
      temp_facet_file_name = facet_file_name + '.TEMP'
      extract_facet_definitions( facet_file_name, [ j ], temp_facet_file_name, 
          include_clean_boxes = False, include_bcomp = False )
      add_circular_clean_box( temp_facet_file_name, 1, 
          [ int( around( pos_j[ 0 ] ) ), int( around( pos_j[ 1 ] ) ) ], clean_radius )
      source_model_version = extract_model_components( temp_facet, 
          facet_file_name = temp_facet_file_name )
      if ( abs( get_model_flux( temp_facet ) ) > 0. ):
        transfer_model_components( temp_facet, source_model_version, peel_facet_i,
            model_version )
        add_uv = add_model( peel_uv, temp_facet, facet_list = [ 1 ], sigma = 0.,
            apply_solutions = apply_solutions, keep_solutions = True,
            frequency_correction = frequency_correction, flag_solutions = True,
            conversion_method = conversion_method )
        peel_uv.zap()
        add_uv.rename( name = peel_uv.name, klass = peel_uv.klass, seq = peel_uv.seq )
      temp_facet.zap()
      remove_file( temp_facet_file_name )

    # determine peeling solutions relative to best available previous solutions
    if relative_solutions:
      cal_uv = get_aips_file( peel_uv.disk, peel_uv.name, 'CAL', - 1, 'UV' )
      # relative to previous facet solutions
      if ( ( max_relativity >= 3 ) and table_exists( facet_i, 'SN', 0 ) ):
        relativity = 3
        call_aips_task( 'TACOP', indata = facet_i, inext = 'SN', invers = 0, ncount = 1,
            outdata = peel_uv, outvers = 0 )
        re_sample_solutions( peel_uv, interpolation_method = 'spline' )
        ref_ant = get_reference_antenna( peel_uv, 0 )
        call_aips_task( 'SPLIT', indata = peel_uv, docalib = 100, gainuse = 0, 
            douvcomp = 0, flagver = -1, outdisk = cal_uv.disk, 
            outclass = cal_uv.klass, outseq = cal_uv.seq )
        peel_uv.zap_table( 'SN', 0 )
        peel_uv.zap_table( 'SN', 0 )
      # relative to peeling solutions brightest source
      elif ( ( max_relativity >= 2 ) and table_exists( peel_facets, 'SN', 0 ) ):
        relativity = 2
        call_aips_task( 'TACOP', indata = get_facet( peel_facets, 1 ),
            inext = 'SN', invers = 0, ncount = 1, outdata = peel_uv, outvers = 0 )
        ref_ant = get_reference_antenna( peel_uv, 0 )
        re_sample_solutions( peel_uv, interpolation_method = 'spline' )
        call_aips_task( 'SPLIT', indata = peel_uv, docalib = 100, gainuse = 0,
            douvcomp = 0, flagver = -1, outdisk = cal_uv.disk,
            outclass = cal_uv.klass, outseq = cal_uv.seq )
        peel_uv.zap_table( 'SN', 0 )
        peel_uv.zap_table( 'SN', 0 )
      # relative to selfcal solutions
      elif ( ( max_relativity >= 1 ) and table_exists( peel_uv, 'SN', 0 ) ):
        relativity = 1
        ref_ant = get_reference_antenna( peel_uv, 0 )
        re_sample_solutions( peel_uv, interpolation_method = 'spline' )
        call_aips_task( 'SPLIT', indata = peel_uv, docalib = 100, gainuse = 0,
            douvcomp = 0, flagver = -1, outdisk = cal_uv.disk,
            outclass = cal_uv.klass, outseq = cal_uv.seq )
        peel_uv.zap_table( 'SN', 0 )
      else:
        relativity = 0
        cal_uv = get_aips_file( peel_uv.disk, peel_uv.name, peel_uv.klass,
            peel_uv.seq, 'UV' )
    else:
      relativity = 0
      cal_uv = get_aips_file( peel_uv.disk, peel_uv.name, peel_uv.klass,
          peel_uv.seq, 'UV' )
      ref_ant = reference_antenna

    # selfcal and image peel facet
    # use the source model as starting model
    converge = selfcal_image_clean_facet( cal_uv, peel_facet_i, 
        facet_file_name = peel_facet_file_name, sigma_min = sigma_min,
        do_sdi_clean = False, signal_to_noise_min = signal_to_noise_min,
        snr_limit = snr_limit, # try_final_amplitude = try_final_amplitude,
        amplitude_interval = amplitude_interval,
        re_center_model = use_catalog, print_info = print_info,
        improvement_limit = improvement_limit,
        convergence_limit = convergence_limit,
        reference_antenna = reference_antenna,
        conversion_method = conversion_method,
        restore_components = restore_output_components,
        imagr_params = imagr_params, calib_params = calib_params,
        phase_interval_min = phase_interval_min,
        frequency_correction = frequency_correction,
        offset_ratio_max = offset_ratio_max,
        resolve_power = resolve_power,
        amplitude_noise_factor = amplitude_noise_factor,
        amplitude_snr = amplitude_snr )
    
    if ( relativity > 0 ):
      cal_uv.zap()
    
    # check final solution interval
    if ( not converge ):
      if print_info:
        print '... source rejected - peeling diverged'
    else:
      solution_interval = 60. * restore_parameter( peel_facet_i, 'solution_interval' )
      if ( solution_interval > solution_interval_max ):
        converge = False
        if print_info:
          print '... source rejected - peeling interval (%s sec) too high' % (
              solution_interval )
    if ( not converge ):
      peel_beam_i = get_facet_beam( peel_facet_i )
      peel_facet_i.zap()
      peel_beam_i.zap()
      remove_facet( peel_facet_file_name, peel_i )
      peel_i = peel_i - 1
      peel_uv.zap()
      peel_uv = get_aips_file( old_peel_uv.disk, old_peel_uv.name, old_peel_uv.klass,
          old_peel_uv.seq, 'UV' )
      # TODO: should we continue here to peel the next source or break off ?
      continue # break
    
    # combine relative peeling solutions with previous solutions solutions
    if ( relativity > 0 ):
      if ( relativity == 3 ):
        call_aips_task( 'TACOP', indata = facet_i, inext = 'SN', invers = 0, 
            ncount = 1, outdata = peel_uv, outvers = 0 )
      elif ( relativity == 2 ):
        call_aips_task( 'TACOP', indata = get_facet( peel_facets, 1 ),
            inext = 'SN', invers = 0, ncount = 1, outdata = peel_uv, outvers = 0 )
      selfcal_solution_version = peel_uv.table_highver( 'SN' ) # + 0
      re_sample_solutions( peel_uv, interpolation_method = 'spline' ) # + 1
      peel_solution_version = peel_facet_i.table_highver( 'SN' )
      call_aips_task( 'TACOP', indata = peel_facet_i, inext = 'SN', # + 2
          outdata = peel_uv, ncount = 1 )
      combine_solutions( peel_uv, force_match = True ) # + 3
      peel_uv.zap_table( 'SN', selfcal_solution_version + 1 )
      peel_uv.zap_table( 'SN', selfcal_solution_version + 2 )
      solution_interval = restore_parameter( peel_facet_i, 'solution_interval' )
      re_sample_solutions( peel_uv, force_reference = True, # + 4
        gap_time = solution_interval )
      peel_uv.zap_table( 'SN', selfcal_solution_version + 3 )
      if ( relativity >= 2 ):
        peel_uv.zap_table( 'SN', selfcal_solution_version )
      call_aips_task( 'TACOP', indata = peel_uv, inext = 'SN', ncount = 1,
          outdata = peel_facet_i )
      peel_uv.zap_table( 'SN', selfcal_solution_version + 4 )
    
    # subtract updated model from UV
    old_peel_uv.zap()
    sub_uv = subtract_model( peel_uv, peel_facets, facet_list = [ peel_i ],
        sigma = 0., apply_solutions = True, keep_solutions = True,
        frequency_correction = frequency_correction, flag_solutions = True,
        conversion_method = conversion_method )
    peel_uv.zap()
    sub_uv.rename( name = old_peel_uv.name, klass = old_peel_uv.klass,
        seq = old_peel_uv.seq )
    peel_uv = get_aips_file( old_peel_uv.disk, old_peel_uv.name, old_peel_uv.klass, 
        old_peel_uv.seq, 'UV' )

    # hard limit the number of peel sources
    if ( peel_i >= max_peel_count ):
      break

  # store parameters
  if ( peel_i > 0 ):
    store_parameter( peel_facets, 'facet_count', peel_i )
    store_parameter( peel_facets, 'facet_file_name', peel_facet_file_name )
  else:
    peel_facets = None

  # remove solutions from peel uv
  while ( peel_uv.table_highver( 'SN' ) > 0 ):
    peel_uv.zap_table( 'SN', 0 )
  if keep_solutions:
    call_aips_task( 'TACOP', indata = uv, outdata = peel_uv, inext = 'SN', invers = 0,
        outvers = 0, ncount = 1 )
  
  # copy back NI and OB tables
  ni_version = uv.table_highver( 'NI' )
  for v in range( 1, 1 + ni_version ):
    if table_exists( uv, 'NI', v ):
      call_aips_task( 'TACOP', indata = uv, outdata = peel_uv, invers = v,
          outvers = v, inext = 'NI', ncount = 1 )
  ob_version = uv.table_highver( 'OB' )
  for v in range( 1, 1 + ob_version ):
    if table_exists( uv, 'OB', v ):
      call_aips_task( 'TACOP', indata = uv, outdata = peel_uv, invers = v,
          outvers = v, inext = 'OB', ncount = 1 )
  
  return [ peel_uv, peel_facets ]

###############################################################################
