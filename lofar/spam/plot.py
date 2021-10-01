###############################################################################

# import Python modules
from sys import *
from os import *
from datetime import *
from math import *
from thread import *

# import 3rd party modules
from numpy import *
from pylab import *
#from matplotlib import *
#from RO.DS9 import *

# import user modules
from error import *
from files import *
from aips import *
from skymodel import *
from acalc import *
from ionosphere import *

###############################################################################

def plot_image( im, xy_range = None, z_range = None, file_name = None, dpi = 100, xyz_labels = None,
    plot_title = None, marker_list = None ):

  pixels = transpose( get_image_pixels( im ) )
  if ( xy_range == None ):
    im_size = get_image_size( im )
    im_pixel_ref = get_pixel_reference( im )
    im_xy_range = [ - im_pixel_ref[ 0 ] + 0.5, im_size[ 0 ] - im_pixel_ref[ 0 ] + 0.5,
                    - im_pixel_ref[ 1 ] + 0.5, im_size[ 1 ] - im_pixel_ref[ 1 ] + 0.5 ]
  else:
    im_xy_range = xy_range
  if ( z_range == None ):
    im_min = get_image_minimum( im )
    im_max = get_image_maximum( im )
    im_z_range = [ im_min[ 0 ], im_max[ 0 ] ]
  else:
    im_z_range = z_range
  if ( xyz_labels == None ):
    xyz_labels = [ 'pixels', 'pixels', 'Jy' ]

  clf()
  imshow( pixels, vmin = im_z_range[ 0 ], vmax = im_z_range[ 1 ], extent = im_xy_range,
      origin = 'lower', interpolation = 'nearest' )
  axis( im_xy_range, 'scaled' )
  xlabel( xyz_labels[ 0 ] )
  ylabel( xyz_labels[ 1 ] )
  if ( plot_title != None ):
    title( plot_title )
  hot()
  colorbar()
  hold( True )

  if ( marker_list != None ):
    for marker in marker_list:
      [ x, y, r, s ] = marker
      plot( [ x - im_pixel_ref[ 0 ] ], [ y - im_pixel_ref[ 1 ] ], s,
          markeredgewidth = 2., markersize = 2. * r, markeredgecolor = s[ 0 : 1 ], 
#          markerfacecolor = None ) # can be uncommented with new version of matplotlib/gtk
          markerfacecolor = s[ 0 : 1 ] )
      axis( im_xy_range, 'scaled' )

  if ( file_name == None ):
#    hold( True )
#    plot_process_id = start_new_thread( show, () )
    show()
  else:
    savefig( file_name, dpi = dpi )
  return

###############################################################################

def make_movie_from_images( image_file_name_list, movie_file_name, format = 'avi',
    frames_per_second = 10 ):

# This routine requires mencoder and ImageMagick's convert to be installed

  # currently only 10 fps supported
  if ( frames_per_second != 10 ):
    raise error( 'currently only 10 fps supported' )

  # currently only png input images supported
  extension = image_file_name_list[ 0 ][ - 4 : ]
  if ( extension.lower() != '.png' ):
    raise error( 'currently only PNG format images supported' )

  # check output format
  # check output filename extension
  if ( ( format == 'avi' ) or ( format == 'mpg' ) or ( format == 'gif' ) ):
    extension = movie_file_name[ - 4 : ]
    if ( extension.lower() == '.' + format ):
      output_file_name = movie_file_name
    else:
      output_file_name = movie_file_name + '.' + format
  else:
    raise error( 'unknown output format: ' + repr( format ) )

  # make consecutive series of images
  j = 0
  for image_file_name in image_file_name_list:
    if ( format == 'mpg' ):
      # copy every image 3 times
      copy_file( image_file_name, output_file_name + '_temp_%05d.png' % ( j ) )
      j = j + 1
      copy_file( image_file_name, output_file_name + '_temp_%05d.png' % ( j ) )
      j = j + 1
      copy_file( image_file_name, output_file_name + '_temp_%05d.png' % ( j ) )
      j = j + 1
    else:
      copy_file( image_file_name, output_file_name + '_temp_%05d.png' % ( j ) )
      j = j + 1
  image_count = j

  # combine separate frame files into one animation
  if file_exists( output_file_name ):
    remove_file( output_file_name )
  if ( format == 'avi' ):
    system( 'mencoder "mf://' + output_file_name + '_temp_*.png" -mf type=png:fps=' + repr( int( frames_per_second ) ) +
        ' -really-quiet -ovc lavc -ffourcc DX50 -noskip -oac copy -o ' + output_file_name )
#  elif ( format == 'mpg' ):
#    system( 'mencoder "mf://' + output_file_name + '_temp_*.png" -mf type=png:fps=' + 
#        repr( int( 3. * frames_per_second ) ) + ' -really-quiet -of mpeg -mpegopts ' +
#        'format=mpeg1:tsaf -ovc lavc -lavcopts vcodec=mpeg1video:vmax_b_frames=0:vmax_p_frames=8 ' +
#        '-oac lavc -lavcopts acodec=mp2 -o ' + output_file_name +
#        ' -ofps ' + repr( int( 3. * frames_per_second ) ) )
  elif ( format == 'mpg' ):
#    system( 'mencoder "mf://' + output_file_name + '_temp_*.png" -mf type=png:fps=' + 
#        repr( int( 3. * frames_per_second ) ) + ' -really-quiet -of mpeg -mpegopts ' +
#        'format=mpeg1:tsaf -ovc lavc -lavcopts vcodec=mpeg1video:vmax_b_frames=0 ' +
#        '-oac copy -o ' + output_file_name +
#        ' -ofps ' + repr( int( 3. * frames_per_second ) ) )
    system( 'mencoder "mf://' + output_file_name + '_temp_*.png" -mf type=png:fps=' + 
        repr( int( 3. * frames_per_second ) ) + ' -really-quiet -of mpeg -mpegopts ' +
        'format=mpeg2:tsaf -ovc lavc -lavcopts vcodec=mpeg2video:vmax_b_frames=0 ' +
        '-oac copy -o ' + output_file_name +
        ' -ofps ' + repr( int( 3. * frames_per_second ) ) )
  else: # ( format == 'gif' ):
    system( '/usr/bin/convert -loop 0 -delay ' + repr( int( 100. / float( frames_per_second ) ) ) + ' ' + 
        output_file_name + '_temp_*.png ' + output_file_name )

  # delete consecutive image files
  for j in range( image_count ):
    image_file_name = output_file_name + '_temp_%05d.png' % ( j )
    remove_file( image_file_name )

  return

###############################################################################

def plot_source( facets, radec, z_range = None ):
  [ i, pos ] = find_source_facets( facets, radec, primary_facet_only = True )[ 0 ]
  facet = get_facet( facets, i )
  plot_image( facet, z_range = z_range, marker_list = [ pos + [ 15., 'bo' ] ] )
  return

###############################################################################

def plot_fit_errors( uv, version = 0 ):
  ni_table = wizardry( uv ).table( 'NI', version )
  time_list = []
  weight_list = []
  for row in ni_table:
    time_list.append( row.time )
    weight_list.append( row.weight )
  time_array = array( time_list )
  weight_array = array( weight_list )
  sel = awhere( weight_array <= 0. )
  weight_array = aput( weight_array, sel, 1. )
  error_array = 1. / weight_array
  error_array = aput( error_array, sel, 0. )
  plot( time_array, error_array, 'k.' )
  show()
  return

###############################################################################

def make_mkl_phase_screen_images( uv, antenna, facets = None, facet_list = [],
    time_steps = [], print_info = False, field_factor = 1., grid_size = 101,
    fit_version = 0, image_prefix = 'prtfil/' ):
  
  # define grid
  radec = get_radec( uv )
  field_size = field_factor * restore_parameter( uv, 'field_size' )
  cell_size = field_size / float( grid_size )
  radec_array = zeros( ( grid_size, grid_size, 2 ), dtype = float64 )
  for y in range( grid_size ):
    for x in range( grid_size ):
      xx = float( x ) - ( float( grid_size - 1 ) / 2. )
      yy = float( y ) - ( float( grid_size - 1 ) / 2. )
      r = sqrt( xx**2 + yy**2 ) * cell_size
      if ( r < field_size / 2. ):
        phi = degrees( atan2( -xx, yy ) )
        radec_array[ y, x, 0 : 2 ] = array( 
            calculate_offset_position( radec, r, phi ), dtype = float64 )
  grid_sel = awhere( alltrue( radec_array[ : , : ] != array( [ 0., 0. ],
      dtype = float64 ), axis = -1 ) )
  radec_table = aget( radec_array, grid_sel ).reshape( len( grid_sel ), 2 ).tolist()
  grid_max = ( float( grid_size ) / 2. ) * cell_size
  grid_min = - grid_max

  # read model fit information
  wiz_uv = wizardry( uv )
  ni_table = wiz_uv.table( 'NI', fit_version )
  model = ni_table.keywords[ 'MODEL' ]
  if ( type( model ) == type( [] ) ):
    model = model[ 0 ]
  model = model.strip()
  if ( model != 'mkl' ):
    raise error( 'unknown model: %s' % ( model ) )
  order = ni_table.keywords[ 'NUM_COEF' ]
  reference_frequency = float32( ni_table.keywords[ 'REF_FREQ' ] )
  beta = float32( ni_table.keywords[ 'BETA' ] )
  r_0 = float32( ni_table.keywords[ 'R_0' ] )
  layer_count = int( ni_table.keywords[ 'LAYERS' ] )
  layer_heights = []
  layer_weights = []
  for l in range( layer_count ):
    layer_heights.append( float32( ni_table.keywords[ 'HEIGHT%d' % ( l + 1 ) ] ) )
    layer_weights.append( float32( ni_table.keywords[ 'WEIGHT%d' % ( l + 1 ) ] ) )
  if ( facets != None ):
    if ( len( facet_list ) == 0 ):
      used_facet_list = range( 1, 1 + restore_parameter( facets, 'facet_count' ) )
    else:
      used_facet_list = facet_list
  else:
    used_facet_list = []
  ob_table = wiz_uv.table( 'OB', ni_table.version )
  try:
    iterations = ob_table.keywords[ 'ITER' ]
  except:
    iterations = 4
  
  # read ionospheric fit table
  fit_time_list = []
  fit_count_list = []
  fit_coef_table = []
  fit_weight_list = []
  for row in ni_table:
    fit_time_list.append( float32( row.time ) )
    fit_count_list.append( row.antenna_no )
    fit_coef_table.append( [ float32( coef ) for coef in row.coef ] )
    fit_weight_list.append( float32( row.weight ) )
  fit_time_count = len( fit_time_list )
  fit_coef_table = array( fit_coef_table, dtype = float64 )
  
  # remove bad fits
  fit_weight_array = array( fit_weight_list, dtype = float64 )
  sel = awhere( fit_weight_array > 0. )
  sel = sel.ravel().tolist()
  
  # read ionospheric pierce point table
  pierce_time_list = []
  pierce_X_table = []
  pierce_za_list = []
  pierce_index_table = []
  for row in ob_table:
    pierce_time_list.append( float32( row.time ) )
    pierce_X_table.append( [ float32( x ) for x in row.orbxyz[ 0 : 2 ] ] )
    pierce_za_list.append( float32( row.orbxyz[ 2 ] ) )
    pierce_index_table.append( [ int( round( row.orientation ) ) - 1, 
        row.subarray - 1, row.antenna_no - 1 ] )
  pierce_time_count = len( pierce_time_list )
  pierce_time_array = array( pierce_time_list, dtype = float64 )
  pierce_index_array = array( pierce_index_table, dtype = int64 )
  pierce_X_array = array( pierce_X_table, dtype = float64 )
  pierce_za_array = array( pierce_za_list, dtype = float64 )
  
  # get other relevant data
  calibration_data = get_phase_calibration_data( uv, facets,
      time_info = True, source_info = True, antenna_info = True,
      calibration_info = False, facet_list = used_facet_list,
      print_info = print_info )
  time_table = calibration_data[ 0 ]
  center_table = calibration_data[ 1 ]
  source_table = calibration_data[ 2 ]
  array_table = calibration_data[ 3 ]
  antenna_table = calibration_data[ 4 ]
  source_count = len( source_table )
  antenna_count = len( antenna_table )
  time_count = len( time_table )
  
  # generate time table
  fit_time_table = []
  fit_gst_list = get_gst_list( uv, fit_time_list )
  for n in range( fit_time_count ):
    fit_time_table.append( [ fit_time_list[ n ], fit_gst_list[ n ] ] )

  # calculate source positions in plot
  dsource_table = []
  for source in source_table:
    [ r, phi ] = calculate_angular_separation( radec, source )
    dsource_table.append( [ -r * sin( radians( phi ) ), r * cos( radians( phi ) ) ] )
  dsource_array = array( dsource_table, dtype = float64 )
#???  
#  # generate reference antenna table
#  reference_list = []
#  for k in range( source_count ):
#    reference_list.append( r )

  # loop over time stamps
  for nn in range( time_count ):

    if ( len( time_steps ) > 0 ):
      if ( not nn in time_steps ):
        continue

    if print_info:
      print '... time step n = ', nn
    
    # save fit weight
    fit_available = True
    try:
      n = fit_time_list.index( time_table[ nn ][ 0 ] )
    except ValueError:
      fit_available = False
    
    # only process non-rejected fits
    if ( not n in sel ):
      fit_available = False

    weight = 1. / 360.
    if fit_available:
    
      # get model fit parameters
      P = fit_coef_table[ n ]
      weight = fit_weight_list[ n ]
      
      if print_info:
        print '...... calculating base vectors'
      
      active_antennas = []
      Xpl_table = []
      pzal_table = []
      Bl_table = []
      for l in range( layer_count ):
        Xp_table = []
        pza_table = []
      
        # get pierce points 
        sel2 = awhere( ( pierce_time_array == fit_time_list[ n ] ) & 
            ( pierce_index_array[ : , 0 ] == l ) )
        Xp_table = Xp_table + aget( pierce_X_array, sel2 ).tolist()
        pza_table = pza_table + aget( pierce_za_array, sel2 ).tolist()
        for i in aget( pierce_index_array[ : , 2 ], sel2 ).tolist():
          if ( not i in active_antennas ):
             active_antennas.append( i )
  
        Xp_table = array( Xp_table, dtype = float64 )
        pza_table = array( pza_table, dtype = float64 )
        
        # calculate structure matrix
        p_count = len( Xp_table )
        if ( p_count != fit_count_list[ n ] ):
          raise error( 'pierce count does not match' )
        D_table = resize( Xp_table, ( p_count, p_count, 2 ) )
        D_table = transpose( D_table, ( 1, 0, 2 ) ) - D_table
        D_table = add.reduce( D_table**2, 2 )
        D_table = ( D_table / ( r_0**2 ) )**( beta / 2. )
        
        # calculate covariance matrix C
        # calculate partial product for interpolation B
        # reforce symmetry
        C_table = - D_table / 2.
        C_table = transpose( C_table - ( add.reduce( C_table, 0 ) / 
            float( p_count ) ) )
        B_table = add.reduce( C_table, 0 ) / float( p_count )
        C_table = C_table - B_table
        C_table = ( C_table + transpose( C_table ) ) / 2.
        
        # incorporate airmass functions and layer weights
        # add layer C to total C
        A_table = resize( 1. / cos( aradians( pza_table ) ), ( p_count, p_count ) )
        A_table = A_table * transpose( A_table )
        if ( l == 0 ):
          Cl_table = C_table * A_table * ( layer_weights[ l ]**2 )
        else:
          Cl_table = Cl_table + C_table * A_table * ( layer_weights[ l ]**2 )
        
        # save tables per height layer
        Xpl_table.append( Xp_table )
        pzal_table.append( pza_table )
        Bl_table.append( B_table )
      
      # convert to arrays
      Xpl_table = array( Xpl_table, dtype = float64 )
      pzal_table = array( pzal_table, dtype = float64 )
      Bl_table = array( Bl_table, dtype = float64 )
      
      # eigenvalue decomposition
      # reforce symmetry
      # select subset of base vectors
      [ U_table, S, Ut_table ] = linalg.svd( Cl_table )
      U_table = ( U_table + transpose( Ut_table ) ) / 2.
      U_table = U_table[ : , 0 : order ]
      S = S[ 0 : order ]
      
      # calculate interpolation matrix
      F_table = dot( U_table, P / S )
      
      if print_info:
        print '...... calculating pierce point coordinates'
    
      Xl_table = []
      zal_table = []
      ref_list = []
      ref_table = []
      for l in range( layer_count ):
        
        X_table = []
        za_table = []
        
        # get pierce point coordinates
        pierce_table = calculate_pierce_coordinates( fit_time_table[ n ], 
            center_table, radec_table, array_table,
            antenna_table[ antenna - 1 : antenna ], 
            height = layer_heights[ l ], iterations = iterations )
        
        # put all new pierce points into one array
        j = 0
        for pierce_info in pierce_table:
          [ X, za, [ k, i ] ] = pierce_info
          X_table.append( X )
          za_table.append( za )
        Xl_table.append( X_table )
        zal_table.append( za_table )
        
      Xl_table = array( Xl_table, dtype = float64 )
      zal_table = array( zal_table, dtype = float64 )
      
      if print_info:
        print '...... generating solutions'
      
      # calculate pierce point model solutions
      phi_table = phi_mkl_model( layer_weights, Xl_table, zal_table, Xpl_table, 
          pzal_table, Bl_table, F_table, beta = beta, r_0 = r_0 )

    if print_info:
      print '...... generating plot'
    phi_grid = zeros( ( grid_size, grid_size ), dtype = float64 )
    if fit_available:
      phi_grid = aput( phi_grid, grid_sel, phi_table )
      phi_grid = amodulo( phi_grid + 180., 360. ) - 180.

    # remove gradient
    if False:
      l = layer_weights.index( max( layer_weights ) )
      Xp_table = Xpl_table[ l ] / cos( aradians( pzal_table[ l ] ) ).reshape( 
        ( p_count, 1 ) )
      F_table = dot( U_table, P / S )
      phi_offset = phi_mkl_model( layer_weights,
          azeros( Xpl_table[ : , 0 : 1, : ] ), aones( pzal_table[ : , 0 : 1 ] ),
          Xpl_table, pzal_table, Bl_table, F_table, beta = beta, r_0 = r_0 )
      T1 = linalg.inv( dot( transpose( Xp_table ), Xp_table ) )
      T2 = dot( transpose( Xp_table ), dot( U_table, P ) - phi_offset )
      G = dot( T1, T2 )

    # plot phase grid below measured phases
    clf()
    imshow( phi_grid, extent = ( grid_min, grid_max, grid_min, grid_max ),
        interpolation = 'nearest', vmin = -180., vmax = 180., origin = 'lower' )
    axis( [ grid_min, grid_max, grid_min, grid_max ], 'scaled' )
    hsv()
    hold( True )

    plot( dsource_array[ : , 0 ], dsource_array[ : , 1 ], 'k+' )
    axis( [ grid_min, grid_max, grid_min, grid_max ], 'scaled' )
    hsv()
    xlabel( r'$\Delta$RA [deg]' )
    ylabel( r'$\Delta$DEC [deg]' )
    title( r'ant %2d, n=%3d (%s), $\sigma_{\phi}$=%4.1f deg' % (
        antenna, nn, time_to_string( time_table[ nn ][ 0 ] ), 1. / weight ) )
    cb = colorbar()
    cb.ax.set_ylabel( r'phase [deg]' )

    # save plot to image file
#    show()
    image_file_name = image_prefix + '%05d.png' % ( nn )
    if file_exists( image_file_name ):
      remove_file( image_file_name )
    savefig( image_file_name, dpi = 75 )
  
  return

###############################################################################

def make_pmkl_phase_screen_images( uv, antenna, facets = None, facet_list = [],
    time_steps = [], print_info = False, field_factor = 1., grid_size = 101,
    fit_version = 0, image_prefix = 'prtfil/', 
    plot_structure = [ True, True, True ] ):
# plot_structure = [ gradient, 2nd order, KL ]
  
  # define grid
  radec = get_radec( uv )
  field_size = field_factor * restore_parameter( uv, 'field_size' )
  cell_size = field_size / float( grid_size )
  radec_array = zeros( ( grid_size, grid_size, 2 ), dtype = float64 )
  for y in range( grid_size ):
    for x in range( grid_size ):
      xx = float( x ) - ( float( grid_size - 1 ) / 2. )
      yy = float( y ) - ( float( grid_size - 1 ) / 2. )
      r = sqrt( xx**2 + yy**2 ) * cell_size
      if ( r < field_size / 2. ):
        phi = degrees( atan2( -xx, yy ) )
        radec_array[ y, x, 0 : 2 ] = array( 
            calculate_offset_position( radec, r, phi ), dtype = float64 )
  grid_sel = awhere( alltrue( radec_array[ : , : ] != array( [ 0., 0. ],
      dtype = float64 ), axis = -1 ) )
  radec_table = aget( radec_array, grid_sel ).reshape( len( grid_sel ), 2 ).tolist()
  grid_max = ( float( grid_size ) / 2. ) * cell_size
  grid_min = - grid_max
  
  # read model fit information
  wiz_uv = wizardry( uv )
  ni_table = wiz_uv.table( 'NI', fit_version )
  model = ni_table.keywords[ 'MODEL' ]
  if ( type( model ) == type( [] ) ):
    model = model[ 0 ]
  model = model.strip()
  if ( model != 'pmkl' ):
    raise error( 'unknown model: %s' % ( model ) )
  order = ni_table.keywords[ 'NUM_COEF' ] - 5
  reference_frequency = float32( ni_table.keywords[ 'REF_FREQ' ] )
  beta = float32( ni_table.keywords[ 'BETA' ] )
  r_0 = float32( ni_table.keywords[ 'R_0' ] )
  layer_count = int( ni_table.keywords[ 'LAYERS' ] )
  layer_heights = []
  layer_weights = []
  for l in range( layer_count ):
    layer_heights.append( float32( ni_table.keywords[ 'HEIGHT%d' % ( l + 1 ) ] ) )
    layer_weights.append( float32( ni_table.keywords[ 'WEIGHT%d' % ( l + 1 ) ] ) )
  l_max = layer_weights.index( max( layer_weights ) )
  if ( facets != None ):
    if ( len( facet_list ) == 0 ):
      used_facet_list = range( 1, 1 + restore_parameter( facets, 'facet_count' ) )
    else:
      used_facet_list = facet_list
  else:
    used_facet_list = []
  ob_table = wiz_uv.table( 'OB', ni_table.version )
  try:
    iterations = ob_table.keywords[ 'ITER' ]
  except:
    iterations = 4
  
  # read ionospheric fit table
  fit_time_list = []
  fit_count_list = []
  fit_poly_table = []
  fit_coef_table = []
  fit_weight_list = []
  for row in ni_table:
    fit_time_list.append( float32( row.time ) )
    fit_count_list.append( row.antenna_no )
    fit_poly_table.append( [ float32( pol ) for pol in row.coef[ 0 : 5 ] ] )
    fit_coef_table.append( [ float32( coef ) for coef in row.coef[ 5 : ] ] )
    fit_weight_list.append( float32( row.weight ) )
  fit_time_count = len( fit_time_list )
  fit_poly_table = array( fit_poly_table, dtype = float64 )
  fit_coef_table = array( fit_coef_table, dtype = float64 )
  
  # remove bad fits
  fit_weight_array = array( fit_weight_list, dtype = float64 )
  sel = awhere( fit_weight_array > 0. )
  sel = sel.ravel().tolist()
  
  # read ionospheric pierce point table
  pierce_time_list = []
  pierce_X_table = []
  pierce_za_list = []
  pierce_index_table = []
  for row in ob_table:
    pierce_time_list.append( float32( row.time ) )
    pierce_X_table.append( [ float32( x ) for x in row.orbxyz[ 0 : 2 ] ] )
    pierce_za_list.append( float32( row.orbxyz[ 2 ] ) )
    pierce_index_table.append( [ int( round( row.orientation ) ) - 1, 
        row.subarray - 1, row.antenna_no - 1 ] )
  pierce_time_count = len( pierce_time_list )
  pierce_time_array = array( pierce_time_list, dtype = float64 )
  pierce_index_array = array( pierce_index_table, dtype = int64 )
  pierce_X_array = array( pierce_X_table, dtype = float64 )
  pierce_za_array = array( pierce_za_list, dtype = float64 )
  
  # get other relevant data
  calibration_data = get_phase_calibration_data( uv, facets,
      time_info = True, source_info = True, antenna_info = True,
      calibration_info = False, facet_list = used_facet_list,
      print_info = print_info )
  time_table = calibration_data[ 0 ]
  center_table = calibration_data[ 1 ]
  source_table = calibration_data[ 2 ]
  array_table = calibration_data[ 3 ]
  antenna_table = calibration_data[ 4 ]
  source_count = len( source_table )
  antenna_count = len( antenna_table )
  time_count = len( time_table )
  
  # generate time table
  fit_time_table = []
  fit_gst_list = get_gst_list( uv, fit_time_list )
  for n in range( fit_time_count ):
    fit_time_table.append( [ fit_time_list[ n ], fit_gst_list[ n ] ] )
  
  # calculate source positions in plot
  dsource_table = []
  for source in source_table:
    [ r, phi ] = calculate_angular_separation( radec, source )
    dsource_table.append( [ -r * sin( radians( phi ) ), r * cos( radians( phi ) ) ] )
  dsource_array = array( dsource_table, dtype = float64 )
#???  
#  # generate reference antenna table
#  reference_list = []
#  for k in range( source_count ):
#    reference_list.append( r )
  
  # loop over time stamps
  for nn in range( time_count ):

    if ( len( time_steps ) > 0 ):
      if ( not nn in time_steps ):
        continue
    
    if print_info:
      print '... time step n = ', nn
    
    # save fit weight
    fit_available = True
    try:
      n = fit_time_list.index( time_table[ nn ][ 0 ] )
    except ValueError:
      fit_available = False
    
    # only process non-rejected fits
    if ( not n in sel ):
      fit_available = False
    
    weight = 1. / 360.
    if fit_available:
      
      # get model fit parameters
      poly = fit_poly_table[ n ]
      P = fit_coef_table[ n ]
      weight = fit_weight_list[ n ]
      
      if print_info:
        print '...... calculating base vectors'
      
      active_antennas = []
      Xpl_table = []
      pzal_table = []
      Bl_table = []
      for l in range( layer_count ):
        Xp_table = []
        pza_table = []
        
        # get pierce points 
        sel2 = awhere( ( pierce_time_array == fit_time_list[ n ] ) & 
            ( pierce_index_array[ : , 0 ] == l ) )
        Xp_table = Xp_table + aget( pierce_X_array, sel2 ).tolist()
        pza_table = pza_table + aget( pierce_za_array, sel2 ).tolist()
        for i in aget( pierce_index_array[ : , 2 ], sel2 ).tolist():
          if ( not i in active_antennas ):
             active_antennas.append( i )
        
        Xp_table = array( Xp_table, dtype = float64 )
        pza_table = array( pza_table, dtype = float64 )
        
        # calculate structure matrix
        p_count = len( Xp_table )
        if ( p_count != fit_count_list[ n ] ):
          raise error( 'pierce count does not match' )
        if ( plot_structure[ 2 ] ):
          D_table = resize( Xp_table, ( p_count, p_count, 2 ) )
          D_table = transpose( D_table, ( 1, 0, 2 ) ) - D_table
          D_table = add.reduce( D_table**2, 2 )
          D_table = ( D_table / ( r_0**2 ) )**( beta / 2. )
          
          # calculate covariance matrix C
          # calculate partial product for interpolation B
          # reforce symmetry
          C_table = - D_table / 2.
          C_table = transpose( C_table - ( add.reduce( C_table, 0 ) / 
              float( p_count ) ) )
          B_table = add.reduce( C_table, 0 ) / float( p_count )
          C_table = C_table - B_table
          C_table = ( C_table + transpose( C_table ) ) / 2.
          
          # incorporate airmass functions and layer weights
          # add layer C to total C
          A_table = resize( 1. / cos( aradians( pza_table ) ), ( p_count, p_count ) )
          A_table = A_table * transpose( A_table )
          if ( l == 0 ):
            Cl_table = C_table * A_table * ( layer_weights[ l ]**2 )
          else:
            Cl_table = Cl_table + C_table * A_table * ( layer_weights[ l ]**2 )
        
        # save tables per height layer
        Xpl_table.append( Xp_table )
        pzal_table.append( pza_table )
        if ( plot_structure[ 2 ] ):
          Bl_table.append( B_table )
      
      # convert to arrays
      Xpl_table = array( Xpl_table, dtype = float64 )
      pzal_table = array( pzal_table, dtype = float64 )
      if ( plot_structure[ 2 ] ):
        Bl_table = array( Bl_table, dtype = float64 )
        
        # eigenvalue decomposition
        # reforce symmetry
        # select subset of base vectors
        [ U_table, S, Ut_table ] = linalg.svd( Cl_table )
        U_table = ( U_table + transpose( Ut_table ) ) / 2.
        U_table = U_table[ : , 0 : order ]
        S = S[ 0 : order ]
        
        # calculate interpolation matrix
        F_table = dot( U_table, P / S )
      
      if print_info:
        print '...... calculating pierce point coordinates'
      
      Xl_table = []
      zal_table = []
      ref_list = []
      ref_table = []
      for l in range( layer_count ):
        
        X_table = []
        za_table = []
        
        # get pierce point coordinates
        pierce_table = calculate_pierce_coordinates( fit_time_table[ n ], 
            center_table, radec_table, array_table,
            antenna_table[ antenna - 1 : antenna ], 
            height = layer_heights[ l ], iterations = iterations )
        
        # put all new pierce points into one array
        j = 0
        for pierce_info in pierce_table:
          [ X, za, [ k, i ] ] = pierce_info
          X_table.append( X )
          za_table.append( za )
        Xl_table.append( X_table )
        zal_table.append( za_table )
      
      Xl_table = array( Xl_table, dtype = float64 )
      zal_table = array( zal_table, dtype = float64 )
      
      if print_info:
        print '...... generating solutions'
      
      # calculate pierce point model solutions
      if ( not plot_structure[ 0 ] ):
        poly[ 0 : 2 ] = [ 0., 0. ]
      if ( not plot_structure[ 1 ] ):
        poly[ 2 : 5 ] = [ 0., 0., 0. ]
      phi_table = phi_poly_model( Xl_table[ l_max ], poly ) / cos( 
          aradians( zal_table[ l_max ] ) )
      if ( plot_structure[ 2 ] ):
        phi_mkl_table = phi_mkl_model( layer_weights, Xl_table, zal_table,
            Xpl_table, pzal_table, Bl_table, F_table, beta = beta, r_0 = r_0 )
        phi_table = phi_table + phi_mkl_table
    
    if print_info:
      print '...... generating plot'
    phi_grid = zeros( ( grid_size, grid_size ), dtype = float64 )
    if fit_available:
      phi_grid = aput( phi_grid, grid_sel, phi_table )
      phi_grid = amodulo( phi_grid + 180., 360. ) - 180.
    
    # plot phase grid below measured phases
    clf()
    imshow( phi_grid, extent = ( grid_min, grid_max, grid_min, grid_max ),
        interpolation = 'nearest', vmin = -180., vmax = 180., origin = 'lower' )
    axis( [ grid_min, grid_max, grid_min, grid_max ], 'scaled' )
    hsv()
    hold( True )
    
    plot( dsource_array[ : , 0 ], dsource_array[ : , 1 ], 'k+' )
    axis( [ grid_min, grid_max, grid_min, grid_max ], 'scaled' )
    hsv()
    xlabel( r'$\Delta$RA [deg]' )
    ylabel( r'$\Delta$DEC [deg]' )
    title( r'ant %2d, n=%3d (%s), $\sigma_{\phi}$=%4.1f deg' % (
        antenna, nn, time_to_string( time_table[ nn ][ 0 ] ), 1. / weight ) )
    cb = colorbar()
    cb.ax.set_ylabel( r'phase [deg]' )
    
    # save plot to image file
#    show()
    image_file_name = image_prefix + '%05d.png' % ( nn )
    if file_exists( image_file_name ):
      remove_file( image_file_name )
    savefig( image_file_name, dpi = 75 )
  
  return

###############################################################################
