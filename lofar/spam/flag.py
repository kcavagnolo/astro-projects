###############################################################################

# import Python modules
from sys import *
from os import *
from datetime import *
from math import *

# import user modules
from files import *
from aips import *
from acalc import *
from parameter import *
from mpfit import *
from solutions import *
from plot import *
from error import *
from image import *

###############################################################################

def flag_uv_data( uv, flag_params = [], keep_solutions = True, flag_version = 0 ):
  
  if ( flag_version > 0 ):
    fl_version = flag_version
  else:
    fl_version = uv.table_highver( 'FG' ) + 1
  for uvflg_params in flag_params:
    call_aips_task( 'UVFLG', indata = uv, outfgver = fl_version, opcode = 'FLAG',
        **uvflg_params )
  
  # apply flag table
  flag_uv = apply_flag_table( uv, version = flag_version,
      keep_solutions = keep_solutions )
  
  return flag_uv

###############################################################################

def make_time_images( uv, image_size = 1024, apply_solutions = True, solution_version = 0,
    imagr_params = {}, apply_flags = True, flag_version = 0, print_info = False, epsilon = 1.e-8,
    keep_images = False, time_step = 10. ):
# works best if source model is subtracted from uv
  
  if apply_solutions:
    if table_exists( uv, 'SN', solution_version ):
      docalib = 100
      gainuse = solution_version
    else:
      docalib = -1
      gainuse = -1
  else:
    docalib = -1
    gainuse = -1
  nchav = get_channel_count( uv )
  cell_size = restore_parameter( uv, 'cell_size' )
  uv_size = restore_parameter( uv, 'pb_image_size' )
  time_array = array( get_time_list( uv ) )
#  dtime = time_step / ( 60. * 24. )
  
  # apply previous flags
  if ( apply_flags and table_exists( uv, 'FG', flag_version ) ):
    flag_uv = apply_flag_table( uv, version = flag_version, keep_solutions = True )
  else:
    flag_uv = uv
  
  # get reference noise
  temp_image = get_aips_file( uv.disk, 'T0000', 'IIM001', -1, 'MA' )
  temp_beam = get_facet_beam( temp_image )
  call_aips_task( 'IMAGR', indata = flag_uv, nchav = nchav, nfield = 1, niter = 0,
      cellsize = [ cell_size, cell_size ], do3dimag = 1, outdisk = temp_image.disk,
      outname = temp_image.name, outseq = temp_image.seq, docalib = docalib,
      imsize = [ image_size, image_size ], gainuse = gainuse, flagver = -1,
      dotv = 0, allok = 0, uvsize = [ uv_size, uv_size ], **imagr_params )
  fill_facet( temp_image, do_edge_circle = True )
  [ temp_avg, temp_noise ] = call_aips_task( 'IMEAN', indata = temp_image,
      pixavg = 0., pixstd = 0., outputs = [ 'pixavg', 'pixstd' ] )
  [ temp_avg, temp_noise ] = call_aips_task( 'IMEAN', indata = temp_image,
      pixavg = temp_avg, pixstd = temp_noise, pixrange = [ - 10. * temp_noise,
      10. * temp_noise  ], outputs = [ 'pixavg', 'pixstd' ] )
  if ( temp_noise == 0. ):
    temp_noise = get_image_rms( temp_image )
  if ( not keep_images ):
    temp_image.zap()
  temp_beam.zap()
  reference_noise = temp_noise
  if print_info:
    print '... reference noise = %s' % ( repr( reference_noise ) )
  
  # loop over time
  noise_list = []
  timerang_list = []
  t = 0
#  time = time_array[ 0 ]
  int_time = restore_parameter( uv, 'integration_time' )
  n = 0
  dn = int( ceil( time_step * 60. / int_time ) )
#  while ( time < time_array[ -1 ] ):
  while ( n < len( time_array ) ):
#    sel = awhere( ( time_array > time ) & ( time_array < time + dtime ) )
#    if ( len( sel ) == 0 ):
#      time = time + dtime
#      continue
#    sel = arange( n, n + dn ).reshape( dn, 1 )
    t = t + 1
    time_low = time_to_dhms( time_array[ n ] - 0.5 * int_time / ( 24. * 3600. ) )
    time_high = time_to_dhms( time_array[ min( n + dn, len( time_array ) ) - 1 ] + 
        0.5 * int_time / ( 24. * 3600. ) )
    timerang_list.append( time_low + time_high )
#    time = time + dtime
    n = n + dn
    if print_info:
      print '... time range %s = %s - %s' % ( repr( t ), repr( time_low ), 
          repr( time_high ) )
    call_aips_task( 'UVFLG', indata = flag_uv, outfgver = 0, opcode = 'FLAG', 
        reason = 'test', timerang = time_low + time_high )
    flagver = flag_uv.table_highver( 'FG' )
    temp_image = get_aips_file( uv.disk, 'T%04d' % ( t ), 'IIM001', -1, 'MA' )
    temp_beam = get_facet_beam( temp_image )
    call_aips_task( 'IMAGR', indata = flag_uv, nchav = nchav, nfield = 1, niter = 0,
        cellsize = [ cell_size, cell_size ], uvsize = [ uv_size, uv_size ],
        outdisk = temp_image.disk, outname = temp_image.name, outseq = temp_image.seq, 
        imsize = [ image_size, image_size ], do3dimag = 1, docalib = docalib,
        gainuse = gainuse, flagver = flagver, dotv = 0, allok = 0, **imagr_params )
    fill_facet( temp_image, do_edge_circle = True )
    [ temp_avg, temp_noise ] = call_aips_task( 'IMEAN', indata = temp_image,
        pixavg = 0., pixstd = 0., outputs = [ 'pixavg', 'pixstd' ] )
    [ temp_avg, temp_noise ] = call_aips_task( 'IMEAN', indata = temp_image,
        pixavg = temp_avg, pixstd = temp_noise, pixrange = [ -10. * temp_noise,
        10. * temp_noise  ], outputs = [ 'pixavg', 'pixstd' ] )
    flag_uv.zap_table( 'FG', flagver )
    if ( temp_noise == 0. ):
      temp_noise = get_image_rms( temp_image )
    if ( not keep_images ):
      temp_image.zap()
    temp_beam.zap()
    if ( temp_noise == reference_noise ):
      noise_list.append( 1.e9 )
    else:
      noise_list.append( temp_noise )
  min_t = 1 + noise_list.index( min( noise_list ) )
  min_timerang = timerang_list[ min_t - 1 ]
  min_noise = noise_list[ min_t - 1 ]
  if print_info:
    print '... minimum noise time range %s (%s)= %s' % ( repr( min_t ), repr( min_timerang ),
        repr( min_noise ) )
  
  if ( apply_flags and table_exists( uv, 'FG', flag_version ) ):
    flag_uv.zap()
  
  return noise_list

###############################################################################

def make_antenna_images( uv, apply_solutions = True, solution_version = 0,
    imagr_params = {}, apply_flags = True, flag_version = 0, print_info = False,
    epsilon = 1.e-8, skip_antennas = [], keep_images = False, max_facets = 0,
    facet_list = [] ):
# works best if source model is subtracted from uv
  
  # extract beam size
  try:
    beam = [ imagr_params[ 'bmaj' ], imagr_params[ 'bmin' ], imagr_params[ 'bpa' ] ]
  except:
    beam = []
  
  # apply flags and solutions
  if ( apply_flags and table_exists( uv, 'FG', flag_version ) ):
    flag_uv = apply_flag_table( uv, version = flag_version, keep_solutions = True )
  else:
    flag_uv = uv
  if ( apply_solutions and table_exists( uv, 'SN', solution_version ) ):
    cal_uv = apply_solution_table( flag_uv, version = solution_version )
  else:
    cal_uv = flag_uv
  if ( ( flag_uv != uv ) and ( flag_uv != cal_uv ) ):
    flag_uv.zap()
  if ( len( facet_list ) > 0 ):
    facet_file_name = restore_parameter( cal_uv, 'pb_facet_file_name' )
    temp_facet_file_name = facet_file_name + '.TEMP'
    extract_facet_definitions( facet_file_name, facet_list, temp_facet_file_name )
    cpb_count = restore_parameter( cal_uv, 'cpb_facet_count' )
    store_parameter( cal_uv, 'cpb_facet_count', len( facet_list ) )
    store_parameter( cal_uv, 'pb_facet_file_name', temp_facet_file_name )
  elif ( max_facets > 0 ):
    cpb_count = restore_parameter( cal_uv, 'cpb_facet_count' )
    store_parameter( cal_uv, 'cpb_facet_count', max_facets )
  
  # get reference noise
  noise_list = []
  i_params = imagr_params.copy()
  cpb_facets = image_cpb_facets( cal_uv, imagr_params = i_params )
  reference_noise = measure_cpb_noise( cal_uv, cpb_facets, keep_image = keep_images )
  remove_facets( cpb_facets )
  if print_info:
    print '... reference noise = %s' % ( repr( reference_noise ) )
  noise_list.append( reference_noise )
  if keep_images:
    cpb_image = get_aips_file( uv.disk, 'CPB', 'FLATN', 0, 'MA' )
    new_image = get_aips_file( uv.disk, 'CPB', 'A00', -1, 'MA' )
    cpb_image.rename( name = new_image.name, klass = new_image.klass,
        seq = new_image.seq )
  
  # loop over antennas
  antenna_count = len( cal_uv.antennas )
  for a in range( 1, 1 + antenna_count ):
    if ( a in skip_antennas ):
      noise_list.append( 1.e9 )
      continue
    if print_info:
      print '... antenna %s' % ( repr( a ) )
    i_params[ 'antennas' ] = [ - a ]
    cpb_facets = image_cpb_facets( cal_uv, imagr_params = i_params )
    temp_noise = measure_cpb_noise( cal_uv, cpb_facets, keep_image = keep_images )
    remove_facets( cpb_facets )
    if ( abs( temp_noise - reference_noise ) / reference_noise > epsilon ):
      noise_list.append( temp_noise )
    else:
      noise_list.append( 1.e9 )
    if keep_images:
      cpb_image = get_aips_file( uv.disk, 'CPB', 'FLATN', 0, 'MA' )
      new_image = get_aips_file( uv.disk, 'CPB', 'A%02d' % ( a ), -1, 'MA' )
      cpb_image.rename( name = new_image.name, klass = new_image.klass,
          seq = new_image.seq )
  min_a = noise_list.index( min( noise_list ) )
  min_noise = noise_list[ min_a ]
  if print_info:
    print '... minimum noise antenna %s = %s' % ( repr( min_a ), repr( min_noise ) )
  
  # cleanup
  if ( cal_uv != uv ):
    cal_uv.zap()
  elif ( max_facets > 0 ) :
    restore_parameter( uv, 'cpb_facet_count', cpb_count )
  
  return noise_list

###############################################################################

def make_baseline_images( uv, antenna, apply_solutions = True, solution_version = 0,
    imagr_params = {}, apply_flags = True, flag_version = 0, print_info = False,
    epsilon = 1.e-8, skip_antennas = [], keep_images = False, max_facets = 0,
    facet_list = [] ):
# works best if source model is subtracted from uv
  
  # extract beam size
  try:
    beam = [ imagr_params[ 'bmaj' ], imagr_params[ 'bmin' ], imagr_params[ 'bpa' ] ]
  except:
    beam = []
  
  # apply flags and solutions
  if ( apply_flags and table_exists( uv, 'FG', flag_version ) ):
    flag_uv = apply_flag_table( uv, version = flag_version, keep_solutions = True )
  else:
    flag_uv = uv
  if ( apply_solutions and table_exists( uv, 'SN', solution_version ) ):
    cal_uv = apply_solution_table( flag_uv, version = solution_version )
  else:
    cal_uv = flag_uv
  if ( ( flag_uv != uv ) and ( flag_uv != cal_uv ) ):
    flag_uv.zap()
  if ( len( facet_list ) > 0 ):
    facet_file_name = restore_parameter( cal_uv, 'pb_facet_file_name' )
    temp_facet_file_name = facet_file_name + '.TEMP'
    extract_facet_definitions( facet_file_name, facet_list, temp_facet_file_name )
    cpb_count = restore_parameter( cal_uv, 'cpb_facet_count' )
    store_parameter( cal_uv, 'cpb_facet_count', len( facet_list ) )
    store_parameter( cal_uv, 'pb_facet_file_name', temp_facet_file_name )
  elif ( max_facets > 0 ):
    cpb_count = restore_parameter( cal_uv, 'cpb_facet_count' )
    store_parameter( cal_uv, 'cpb_facet_count', max_facets )
 
  # get reference noise
  noise_list = []
  i_params = imagr_params.copy()
  cpb_facets = image_cpb_facets( cal_uv, imagr_params = i_params )
  reference_noise = measure_cpb_noise( cal_uv, cpb_facets, keep_image = keep_images )
  remove_facets( cpb_facets )
  if print_info:
    print '... reference noise = %s' % ( repr( reference_noise ) )
  noise_list.append( reference_noise )
  if keep_images:
    cpb_image = get_aips_file( uv.disk, 'CPB', 'FLATN', 0, 'MA' )
    new_image = get_aips_file( uv.disk, 'CPB', 'B0000', -1, 'MA' )
    cpb_image.rename( name = new_image.name, klass = new_image.klass,
        seq = new_image.seq )
  
  # loop over antennas
  antenna_count = len( cal_uv.antennas )
  i_params[ 'antennas' ] = [ - antenna ]
  for a in range( 1, 1 + antenna_count ):
    if ( ( a in skip_antennas ) or ( a == antenna ) ):
      noise_list.append( 1.e9 )
      continue
    if print_info:
      print '... baseline %s-%s' % ( repr( antenna ), repr( a ) )
    i_params[ 'baseline' ] = [ -a ]
    cpb_facets = image_cpb_facets( cal_uv, imagr_params = i_params )
    temp_noise = measure_cpb_noise( cal_uv, cpb_facets, keep_image = keep_images )
    remove_facets( cpb_facets )
    if ( abs( temp_noise - reference_noise ) / reference_noise > epsilon ):
      noise_list.append( temp_noise )
    else:
      noise_list.append( 1.e9 )
    if keep_images:
      cpb_image = get_aips_file( uv.disk, 'CPB', 'FLATN', 0, 'MA' )
      new_image = get_aips_file( uv.disk, 'CPB', 'B%02d%02d' % ( antenna, a ),
          -1, 'MA' )
      cpb_image.rename( name = new_image.name, klass = new_image.klass,
          seq = new_image.seq )
  min_a = noise_list.index( min( noise_list ) )
  min_noise = noise_list[ min_a ]
  if print_info:
    print '... minimum noise baseline %s-%s = %s' % ( repr( antenna ),
        repr( min_a ), repr( min_noise ) )
    
  # cleanup
  if ( cal_uv != uv ):
    cal_uv.zap()
  elif ( len( facet_list ) > 0 ):
    store_parameter( uv, 'cpb_facet_count', cpb_count )
    store_parameter( uv, 'pb_facet_file_name', facet_file_name )
  elif ( max_facets > 0 ) :
    store_parameter( uv, 'cpb_facet_count', cpb_count )
  
  return noise_list

###############################################################################

def remove_antenna_images( uv ):
  antenna_count = len( uv.antennas )
  for a in range( antenna_count + 1 ):
    try:
      image = get_aips_file( uv.disk, 'CPB', 'A%02d' % ( a ), 0, 'MA' )
      while image.exists():
        image.zap()
        image = get_aips_file( uv.disk, 'CPB', 'A%02d' % ( a ), 0, 'MA' )
    except:
      continue
  return

###############################################################################

def remove_baseline_images( uv ):
  antenna_count = len( uv.antennas )
  for a in range( antenna_count + 1 ):
    for b in range( antenna_count + 1 ):
      try:
        image = get_aips_file( uv.disk, 'CPB', 'B%02d%02d' % ( a, b ), 0, 'MA' )
        while image.exists():
          image.zap()
          image = get_aips_file( uv.disk, 'CPB', 'B%02d%02d' % ( a, b ), 0, 'MA' )
      except:
        continue
  return

###############################################################################

def flag_undulations( uv, image, cutoff = 6., extend = 1, print_info = False ):
  
  # cut image to nearest power of two
  image_size = get_image_size( image )
  new_image_size = [ int( 2.**floor( log( image_size[ 0 ] ) / log( 2. ) ) ), 
      int( 2.**floor( log( image_size[ 1 ] ) / log( 2. ) ) ) ]
  xinc = 1
  yinc = 1
  if ( new_image_size[ 0 ] != new_image_size[ 1 ] ):
    if ( new_image_size[ 0 ] > new_image_size[ 1 ] ):
      xinc = new_image_size[ 0 ] / new_image_size[ 1 ]
    else:
      yinc = new_image_size[ 1 ] / new_image_size[ 0 ]
  blc = [ 1 + int( ceil( float( image_size[ 0 ] - xinc * new_image_size[ 0 ] ) / 2. ) ),
      1 + int( floor( float( image_size[ 1 ] - yinc * new_image_size[ 1 ] ) / 2. ) ) ]
  trc = [ int( ceil( float( image_size[ 0 ] + xinc * new_image_size[ 0 ] ) / 2. ) ),
      int( floor( float( image_size[ 1 ] + yinc * new_image_size[ 1 ] ) / 2. ) ) ]
  sub_image = get_aips_file( image.disk, image.name, 'SUB', -1, 'MA' )
  call_aips_task( 'SUBIM', indata = image, outdata = sub_image,
      blc = blc, trc = trc, xinc = xinc, yinc = yinc )
  
  # remove tables to speed up things
  cc_max = sub_image.table_highver( 'CC' )
  for i in range( 1, 1 + cc_max ):
    if table_exists( sub_image, 'CC', i ):
      sub_image.zap_table( 'CC', i )
  sn_max = sub_image.table_highver( 'SN' )
  for i in range( 1, 1 + sn_max ):
    if table_exists( sub_image, 'SN', i ):
      sub_image.zap_table( 'SN', i )
  
  # clip flux above 5 sigma
  im_rms = get_image_rms( sub_image )
  im_max = abs( get_image_extremum( sub_image )[ 0 ] )
  while ( im_max > 5. * im_rms ):
    clip_image( sub_image, [ -5. * im_rms, 5. * im_rms ],
        [ get_aips_magic_value(), get_aips_magic_value() ] )
    im_rms = get_image_rms( sub_image )
    im_max = abs( get_image_extremum( sub_image )[ 0 ] )
  
  # replace image blanks by zeros
  fft_image = get_aips_file( image.disk, image.name, 'FFT', -1, 'MA' )
  call_aips_task( 'REMAG', indata = sub_image, outdata = fft_image,
      pixval = 0. )
  sub_image.zap()
  
  # FFT image
  fft_real = get_aips_file( image.disk, image.name, 'UVREAL', -1, 'MA' )
  fft_imag = get_aips_file( image.disk, image.name, 'UVIMAG', -1, 'MA' )
  outseq = max( fft_real.seq, fft_imag.seq )
  call_aips_task( 'FFT', indata = fft_image, opcode = 'MARE',
      outdisk = fft_real.disk, outname = fft_real.name, outseq = outseq )
  fft_image.zap()
  fft_real = get_aips_file( image.disk, image.name, 'UVREAL', outseq, 'MA' )
  fft_imag = get_aips_file( image.disk, image.name, 'UVIMAG', outseq, 'MA' )
  
  # combine FFT results into amplitude and phase images
  fft_amp = get_aips_file( image.disk, image.name, 'FFTA', -1, 'MA' )
  call_aips_task( 'COMB', indata = fft_real, in2data = fft_imag,
      outdata = fft_amp, opcode = 'POLI', aparm = [ 1., 1., 0., 0. ] )
#  fft_phs = get_aips_file( image.disk, image.name, 'FFTP', -1, 'MA' )
#  call_aips_task( 'COMB', indata = fft_real, in2data = fft_imag,
#      outdata = fft_phs, opcode = 'POLA', aparm = [ 1., 0., 0., 0. ] )
  fft_real.zap()
  fft_imag.zap()
  
  # get FFT image info
  pix = get_image_pixels( fft_amp, flip = False ) - 1.
  for ctype in fft_amp.header.ctype:
    if ( ctype.find( 'UU' ) != - 1 ):
      u_index = fft_amp.header.ctype.index( ctype )
    if ( ctype.find( 'VV' ) != - 1 ):
      v_index = fft_amp.header.ctype.index( ctype )
  uv_ref = [ int( round( fft_amp.header.crpix[ u_index ] ) ) - 1,
      int( round( fft_amp.header.crpix[ v_index ] ) ) - 1 ]
  uv_size = [ fft_amp.header.cdelt[ u_index ], fft_amp.header.cdelt[ v_index ] ]
  factor = get_frequency( uv ) / get_frequency( image )
  du = factor * uv_size[ 0 ]
  dv = factor * uv_size[ 1 ]
  fft_amp.zap()
  
  # apply UV track mask
  mask = azeros( pix )
  sel = []
  group_count = 0
  for group in wizardry( uv ):
    group_count = group_count + 1
    uvw = group.uvw
    du = int( round( uvw[ 0 ] / ( uv_size[ 0 ] * factor ) ) )
    dv = int( round( uvw[ 1 ] / ( uv_size[ 1 ] * factor ) ) )
    u1 = uv_ref[ 0 ] + du - extend
    u2 = uv_ref[ 0 ] + du + extend + 1
    u3 = uv_ref[ 0 ] - du - extend
    u4 = uv_ref[ 0 ] - du + extend + 1
    v1 = uv_ref[ 1 ] + dv - extend
    v2 = uv_ref[ 1 ] + dv + extend + 1
    v3 = uv_ref[ 1 ] - dv - extend
    v4 = uv_ref[ 1 ] - dv + extend + 1
    mask[ u1 : u2, v1 : v2 ] = 1.
    mask[ u3 : u4, v3 : v4 ] = 1.
#    mask[ uv_ref[ 0 ] + du, uv_ref[ 1 ] + dv ] = 2.
#    mask[ uv_ref[ 0 ] - du, uv_ref[ 1 ] - dv ] = 2.
#    pix[ u1 : u2, v1 : v2 ] *= - asign( pix[ u1 : u2, v1 : v2 ] )
#    pix[ u3 : u4, v3 : v4 ] *= - asign( pix[ u3 : u4, v3 : v4 ] )
  pix = pix * mask
  
  # identify amplitude outliers
  sel = array( awhere( pix > 0. ), dtype = float32 )
  data_list = []
  for s in sel:
    u = factor * ( s[ 0 ] - uv_ref[ 0 ] ) * uv_size[ 0 ]
    v = factor * ( s[ 1 ] - uv_ref[ 1 ] ) * uv_size[ 1 ]
    data_list.append( [ s[ 0 ], s[ 1 ], u, v, u**2 + v**2,
        pix[ s[ 0 ], s[ 1 ] ] ] )
  data_list.sort( cmp = lambda a, b: cmp( a[ 4 ], b[ 4 ] ) )
  bins = int( floor( sqrt( float( len( data_list ) ) ) ) )
  for i in range( bins ):
    bin = data_list[ i * bins : min( ( i + 1 ) * bins, len( data_list ) ) ]
    data = array( bin )
    rms = sqrt( mean( data[ : , 5 ]**2 ) )
    sel = awhere( data[ : , 5 ] > cutoff * rms )
    while ( len( sel ) > 0 ):
      bin2 = [ [ y for y in x ] for x in bin ]
      for j in range( len( sel ) ):
        k = sel[ j, 0 ]
        d = data[ k ]
        u1 = int( d[ 0 ] ) - extend
        u2 = int( d[ 0 ] ) + extend + 1
        u3 = 2 * uv_ref[ 0 ] - int( d[ 0 ] ) - extend
        u4 = 2 * uv_ref[ 0 ] - int( d[ 0 ] ) + extend + 1
        v1 = int( d[ 1 ] ) - extend
        v2 = int( d[ 1 ] ) + extend + 1
        v3 = 2 * uv_ref[ 1 ] - int( d[ 1 ] ) - extend
        v4 = 2 * uv_ref[ 1 ] - int( d[ 1 ] ) + extend + 1
        pix[ u1 : u2, v1 : v2 ] *= -asign( pix[ u1 : u2, v1 : v2 ] )
        pix[ u3 : u4, v3 : v4 ] *= -asign( pix[ u3 : u4, v3 : v4 ] )
        bin.remove( bin2[ k ] )
      data = array( bin )
      rms = sqrt( mean( data[ : , 5 ]**2 ) )
      sel = awhere( data[ : , 5 ] > cutoff * rms )
  
  # convert pixels to UV ranges and flag
  # TODO: incorporate xinc & yinc
  flag_uv = get_aips_file( uv.disk, uv.name, 'UFLAG', -1, 'UV' )
  sel = awhere( pix[ 0 : uv_ref[ 0 ] + 1 ] < 0. )
  if ( len( sel ) == 0 ):
    call_aips_task( 'MOVE', indata = uv, outdata = flag_uv,
        userid = get_aips_userid() )
    return flag_uv
  size = len( pix )
  adu = abs( factor * 0.51 * uv_size[ 0 ] )
  adv = abs( factor * 0.51 * uv_size[ 1 ] )
  for s in sel:
    au = abs( factor * float( s[ 0 ] - uv_ref[ 0 ] ) * uv_size[ 0 ] )
    av = abs( factor * float( s[ 1 ] - uv_ref[ 1 ] ) * uv_size[ 1 ] )
    if ( not flag_uv.exists() ):
      call_aips_task( 'UVWAX', indata = uv, outdata = flag_uv,
          aparm = [ au-adu, au+adu, av-adv, av+adv, 0, 0 ] )
    else:
      dummy_uv = get_aips_file( uv.disk, uv.name, 'DUMMY', -1, 'UV' )
      while table_exists( flag_uv, 'SN', 0 ):
        flag_uv.zap_table( 'SN', 0 )
      while table_exists( flag_uv, 'NI', 0 ):
        flag_uv.zap_table( 'NI', 0 )
      while table_exists( flag_uv, 'OB', 0 ):
        flag_uv.zap_table( 'OB', 0 )
      call_aips_task( 'UVWAX', indata = flag_uv, outdata = dummy_uv,
          aparm = [ au-adu, au+adu, av-adv, av+adv, 0, 0 ] )
      flag_uv.zap()
      dummy_uv.rename( name = flag_uv.name, klass = flag_uv.klass,
          seq = flag_uv.seq )
  
  if flag_uv.exists():
    # copy tables back
    if ( table_exists( uv, 'SN', 0 ) and ( not table_exists( flag_uv, 'SN', 0 ) ) ):
      high = uv.table_highver( 'SN' )
      for i in range( 1, 1 + high ):
        if table_exists( uv, 'SN', i ):
          call_aips_task( 'TACOP', indata = uv, outdata = flag_uv, inext = 'SN',
              invers = i, outvers = i, ncount = 1 )
    if ( table_exists( uv, 'NI', 0 ) and ( not table_exists( flag_uv, 'NI', 0 ) ) ):
      high = uv.table_highver( 'NI' )
      for i in range( 1, 1 + high ):
        if table_exists( uv, 'NI', i ):
          call_aips_task( 'TACOP', indata = uv, outdata = flag_uv, inext = 'NI',
              invers = i, outvers = i, ncount = 1 )
    if ( table_exists( uv, 'OB', 0 ) and ( not table_exists( flag_uv, 'OB', 0 ) ) ):
      high = uv.table_highver( 'OB' )
      for i in range( 1, 1 + high ):
        if table_exists( uv, 'OB', i ):
          call_aips_task( 'TACOP', indata = uv, outdata = flag_uv, inext = 'OB',
              invers = i, outvers = i, ncount = 1 )
  else:
    call_aips_task( 'MOVE', indata = uv, outdata = flag_uv,
        userid = get_aips_userid() )
  
  if print_info:
    flagged = 2 * len( sel )
    pixels = len( data_list )
    fraction = float( flagged ) / float( pixels )
    print '... flagged %s percent of data' % ( repr( 100. * fraction ) )
        
  return flag_uv

###############################################################################

def make_baseline_flags( uv, apply_solutions = True, solution_version = 0, 
    max_facets = 0, imagr_params = {}, print_info = False, # improvement = 0., # 0.0025,
    measure_final_noise = True, keep_final_image = False, flag_version = 0,
    fix_beam = False, antennas = [] ):
  
  if ( ( flag_version ==  0 ) or ( not table_exists( uv, 'FG', flag_version ) ) ) :
    flagver = uv.table_highver( 'FG' ) + 1
  else:
    flagver = flag_version
  baseline_list = []
  
  # filter imagr params
  rmss = make_antenna_images( uv, apply_solutions = apply_solutions,
      solution_version = solution_version, imagr_params = imagr_params,
      apply_flags = True, flag_version = flagver, print_info = print_info,
      epsilon = 1e-8, skip_antennas = range( 1000 ), keep_images = True,
      max_facets = max_facets )
  ref_im = get_aips_file( uv.disk, 'CPB', 'A00', 0, 'MA' )
  [ bmaj, bmin, bpa ] = get_beam_size( ref_im )
  ref_im.zap()
  i_params = imagr_params.copy()
  if fix_beam:
    i_params[ 'bmaj' ] = bmaj
    i_params[ 'bmin' ] = bmin
    i_params[ 'bpa' ] = bpa
  
  rmss = make_antenna_images( uv, apply_solutions = apply_solutions,
      solution_version = solution_version, imagr_params = i_params,
      apply_flags = True, flag_version = flagver, print_info = print_info,
      epsilon = 1e-8, skip_antennas = [], keep_images = True,
      max_facets = max_facets )
  rmss = array( rmss )
  dead_antennas = awhere( rmss > 1.e6 ).ravel().tolist()
#  sel = awhere( 1. - ( rmss / rmss[ 0 ] ) > improvement )
#  if ( len( sel ) == 0 ):
#    return baseline_list
#  antennas = sel.ravel().tolist()
#  rmss = aget( rmss, sel ).tolist()
#  ar_list = []
#  for i in range( len( antennas ) ):
#    ar_list.append( [ antennas[ i ], rmss[ i ] ] )
#  ar_list.sort( cmp = lambda a, b: cmp( a[ 1 ], b[ 1 ] ) )
#  antennas = [ x[ 0 ] for x in ar_list ]
  antenna_count = len( rmss ) - 1
  ref_im = get_aips_file( uv.disk, 'CPB', 'A00', 0, 'MA' )
  for a in range( 1, 1 + antenna_count ):
    im = get_aips_file( uv.disk, 'CPB', 'A%02d' % ( a ), 0, 'MA' )
    rms = 0.
    if ( not a in dead_antennas ):
      res_im = get_aips_file( uv.disk, 'CPB', 'M%02d' % ( a ), -1, 'MA' )
      call_aips_task( 'COMB', indata = im, in2data = ref_im, outdata = res_im,
          opcode = 'SUM', aparm = [ 1., -1. ] )
      rms = get_image_rms( res_im )
      res_im.zap()
    im.zap()
    rmss[ a ] = rms
  ref_im.zap()
  sel = awhere( rmss[ 1 : ] > 0. )
  if ( len( sel ) == 0 ):
    return baseline_list
  sel = sel + 1
  ants = sel.ravel().tolist()
  rmss = aget( rmss, sel ).tolist()
  ar_list = []
  for i in range( len( ants ) ):
    ar_list.append( [ ants[ i ], rmss[ i ] ] )
  ar_list.sort( cmp = lambda a, b: cmp( b[ 1 ], a[ 1 ] ) )
  ants = [ x[ 0 ] for x in ar_list ]
  if print_info:
    print '... processing antennas in the following order: '
    print repr( ants )
  
  for i in range( len( ants ) ):
    if ( len( antennas ) > 0 ):
      if ( not i in antennas ):
        continue
    if print_info:
      print '... processing antenna ' + repr( ants[ i ] )
    rmss = make_baseline_images( uv, ants[ i ], imagr_params = i_params,
        apply_solutions = apply_solutions, solution_version = solution_version,
        apply_flags = True, flag_version = flagver, print_info = print_info,
        epsilon = 1e-8, skip_antennas = dead_antennas + ants[ 0 : i ],
        keep_images = False, max_facets = max_facets )
    rmss = array( rmss )
    sel = awhere( 1. - ( rmss / rmss[ 0 ] ) > 0. )
    if ( len( sel ) == 0 ):
#      continue
      break
    baselines = sel.ravel().tolist()
    if ( len( baselines ) > 0 ):
      if print_info:
        print '... flagging baselines % s - %s' % ( repr( ants[ i ] ),
            repr( baselines ) )
      call_aips_task( 'UVFLG', indata = uv, outfgver = flagver,
          antennas = [ ants[ i ] ], baseline = baselines, reason = 'ripples' )
      for b in baselines:
        baseline_list.append( [ ants[ i ], b ] )
  
  if measure_final_noise:
    rmss = make_antenna_images( uv, apply_solutions = apply_solutions,
      solution_version = solution_version, imagr_params = imagr_params,
      apply_flags = True, flag_version = flagver, print_info = print_info,
      epsilon = 1e-8, skip_antennas = range( 1000 ), keep_images = keep_final_image,
      max_facets = max_facets )
  
  return baseline_list

###############################################################################
