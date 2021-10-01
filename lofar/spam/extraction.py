###############################################################################

# import Python modules
from sys import *
from os import *
from math import *
import random

# import 3rd party modules
from numpy import *

# import user modules
from acalc import *
from files import *
from aips import *
from sphere import *
from parameter import *
from skymodel import *
from error import *

###############################################################################

def extract_source_catalog_table( im, catalog_version = 0, sigma_min = 5., iterations = 5,
    max_gaussians_per_island = 4, rms_boxsize = 256, rms_stepsize = 32, make_residual_map = True,
    keep_rms_map = False ):

  # handle inputs
  if ( iterations > 10 ):
    raise error( 'number of iterations may not exceed 10' )
  if ( max_gaussians_per_island > 4 ):
    raise error( 'number of gaussians per island may not exceed 4' )
  if make_residual_map:
    res_image = get_aips_file( im.disk, im.name, 'RESID', - 1, 'MA' )
    doresid = 1
  else:
    doresid = 0
  if ( max_gaussians_per_island == 1 ):
    doall = 0
  else:
    doall = 1
  dowidth = []
  for i in range( max_gaussians_per_island ):
    dowidth.append( [ 1, 1, 1 ] )
  for i in range( max_gaussians_per_island, 4 ):
    dowidth.append( [ 0, 0, 0 ] )

  # remove blanked borders of image
  image = crop_image( im )
#  [ flux_max, pos_max ] = get_image_maximum( image )
#  if ( flux_min >= flux_max ):
#    raise error( 'peak flux in image does not exceed flux threshold' )

  # calculate limits for iterations
  cparm = [ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. ]
  for i in range( iterations ):
    cparm[ i ] =  2.**float( iterations - i - 1 ) * sigma_min

  # calculate rms image
  rms_image = get_aips_file( im.disk, im.name, 'RMS', - 1, 'MA' )
  call_aips_task( 'RMSD', indata = image, outdata = rms_image, imsize = [ rms_boxsize, rms_boxsize ],
     xinc = rms_stepsize, yinc = rms_stepsize )

  # do source extraction
  call_aips_task( 'SAD', indata = image, invers = 0, outvers = - 1, in2data = rms_image,
      docrt = 0, ngauss = 40000, cparm = cparm, icut = 0., bwsmear = 0, sort = 'S', doall = doall,
      dowidth = dowidth, gain = 0.1, dparm = [ sigma_min, 0, 0, 0, 0, 0, 0, 0, 2, 0 ],  
      doresid = doresid, outdata = res_image )

  # copy catalog table to original image
  call_aips_task( 'TACOP', indata = image, inext = 'MF', invers = 0, ncount = 1,
      outdata = im, outvers = catalog_version )
  image.zap()
  if ( not keep_rms_map ):
    rms_image.zap()

  if make_residual_map:
    return res_image
  else:
    return

###############################################################################

# format example
#
# Title: AIPS MODEL FIT COMPONENTS TABLE
# Created by      SAD   on 07-DEC-2007 14:19:27
# Last written by SAD   on 07-DEC-2007 14:19:27
# Ncol  39  Nrow     109    Sort cols:
#     Table has     7 keyword-value pairs:
#    REVISION =            3
#    DEPTH1   =            1
#    DEPTH2   =            1
#    DEPTH3   =            1
#    DEPTH4   =            1
#    DEPTH5   =            1
#    REALRMS  =  8.7719038128853D-02
#    Table can be written as a FITS ASCII table
# 
# COL. NO.      1          2          3          4           5          6
#      ROW  PLANE      PEAK INT   I FLUX     DELTAX     DELTAY      MAJOR AX
#   NUMBER             JY/BEAM    JY         DEGREES    DEGREES     DEGREES
#        1  1.000E+00  4.439E+01  4.480E+01  2.695E+00  -1.503E+00  2.240E-02
# 
# COL. NO.      7          8          9          10         11         12
#      ROW  MINOR AX   POSANGLE   Q FLUX     U FLUX     V FLUX     ERR PEAK
#   NUMBER  DEGREES    DEGREES    JY         JY         JY         JY/BEAM
#        1  2.228E-02  1.698E+01  0.000E+00  0.000E+00  0.000E+00  8.772E-02
# 
# COL. NO.      13         14         15         16         17         18
#      ROW  ERR FLUX   ERR DLTX   ERR DLTY   ERR MAJA   ERR MINA   ERR PA
#   NUMBER  JY         DEGREES    DEGREES    DEGREES    DEGREES    DEGREES
#        1  1.529E-01  1.868E-05  1.878E-05  4.425E-05  4.397E-05  1.235E+01
# 
# COL. NO.      19         20         21         22         23         24
#      ROW  ERR QFLX   ERR UFLX   ERR VFLX   TYPE MOD   D0 MAJOR   D0 MINOR
#   NUMBER  JY         JY         JY                    DEGREES    DEGREES
#        1  0.000E+00  0.000E+00  0.000E+00  1.000E+00  2.786E-03  1.142E-03
# 
# COL. NO.      25         26         27         28         29         30
#      ROW  D0 POSAN   D- MAJOR   D- MINOR   D- POSAN   D+ MAJOR   D+ MINOR
#   NUMBER  DEGREES    DEGREES    DEGREES    DEGREES    DEGREES    DEGREES
#        1  1.698E+01  2.405E-03  0.000E+00  5.718E+00  3.122E-03  1.809E-03
# 
# COL. NO.      31         32         33         34         35         36
#      ROW  D+ POSAN   RES RMS    RES PEAK   RES FLUX   CENTER X   CENTER Y
#   NUMBER  DEGREES    JY/BEAM    JY/BEAM    JY         PIXELS     PIXELS
#        1  2.933E+01  1.130E-01  3.329E-01  1.960E-01  3.214E+03  3.605E+03
# 
# COL. NO.          37             38             39
#      ROW      MAJ AXIS       MIN AXIS       PIXEL PA
#   NUMBER      PIXELS         PIXELS         DEGREES
#        1      7.330E+00      7.282E+00      1.807E+01

def read_aips_model_fit_table( image, version = 0 ):

  if ( not table_exists( image, 'MF', catalog_version ) ):
    raise error( 'source catalog table does not exist' )
  wiz_im = wizardry( image )
  cat_table = wiz_im.table( 'MF', catalog_version )

  cat_list = [ [ 'ID', 'RA', 'DEC', 'PEAK', 'FLUX', 'MAJ', 'MIN', 'PA', 'PEAK_RES',
      'RA_ERROR', 'DEC_ERROR', 'PEAK_ERROR', 'FLUX_ERROR', 'MAJ_ERROR', 'MIN_ERROR', 'PA_ERROR' ] ]
  id = 0
  for row in cat_table:
    id = id + 1
    [ ra, dec ] = calculate_source_radec( image, [ row.center_x, row.center_y ] )
    peak = row.peak_int
    flux = row.i_flux
    maj = row.major_ax
    min = row.minor_ax
    pa = row.posangle
    peak_res = row.res_peak
    ra_error = row.err_dltx
    dec_error = row.err_dlty
    peak_error = row.err_peak
    flux_error = row.err_flux
    maj_error = row.err_maja
    min_error = row.err_mina
    pa_error = row.err_pa
    cat_list.append( [ id, ra, dec, peak, flux, maj, min, pa, peak_res,
        ra_error, dec_error, peak_error, flux_error, maj_error, min_error, pa_error ] )

  return cat_list


###############################################################################

# format example
#             RA           Dec          Peak    Flux    IRMS  Fit Maj Fit min   PA    res. RMS res Peak  PixX    PixY   Field
#    1  12 28 46.5232  -21 26 49.634  1726.27  2047.85 210.435  94.902  79.999 -122.4    0.047    0.099  3233.7  1914.4 1300-208
#                7.20           6.29   217.24   257.71          12.913   9.310   27.5 

def read_obit_source_catalog( cat_file_name ):

  if ( not file_exists( cat_file_name ) ):
    raise error( 'source catalog %s was not found' % cat_file_name )

  cat_list = [ [ 'ID', 'RA', 'DEC', 'PEAK', 'FLUX', 'MAJ', 'MIN', 'PA', 'PEAK_RES',
      'RA_ERROR', 'DEC_ERROR', 'PEAK_ERROR', 'FLUX_ERROR', 'MAJ_ERROR', 'MIN_ERROR', 'PA_ERROR' ] ]
  cat_file = file( cat_file_name, mode = 'r' )

  header_found = False
  error_line = False
  for line in cat_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( not header_found ):
      if ( words[ 0 ] == 'RA' ):
        if ( line != '             RA           Dec          Peak    Flux    IRMS  ' + 
            'Fit Maj Fit min   PA    res. RMS res Peak  PixX    PixY   Field\n' ):
          raise error( 'unexpected row format' )
        header_found = True
    elif ( not error_line ):
      id = int( words[ 0 ] )
      [ ra, dec ] = hmsdms_to_degdeg( [ float( words[ 1 ] ), float( words[ 2 ] ), float( words[ 3 ] ),
          float( words[ 4 ] ), float( words[ 5 ] ), float( words[ 6 ] ) ] )
      peak = float( words[ 7 ] ) / 1.e3
      flux = float( words[ 8 ] ) / 1.e3
      maj = float( words[ 10 ] )
      min = float( words[ 11 ] )
      pa = float( words[ 12 ] )
      peak_res = float( words[ 14 ] ) * peak
      error_line = True
    else:
      ra_error = float( words[ 0 ] ) / 3600.
      dec_error = float( words[ 1 ] ) / 3600.
      peak_error = float( words[ 2 ] ) / 1.e3
      flux_error = float( words[ 3 ] ) / 1.e3
      maj_error = float( words[ 4 ] )
      min_error = float( words[ 5 ] )
      pa_error = float( words[ 6 ] )
      cat_list.append( [ id, ra, dec, peak, flux, maj, min, pa, peak_res, 
          ra_error, dec_error, peak_error, flux_error, maj_error, min_error, pa_error ] )
      error_line = False

  cat_file.close()

  return cat_list

###############################################################################

def associate_sources( radec_list_1, radec_list_2, assoc_radius = 100. ):
# assoc_radius in arcsec

  assoc_list = []
  if ( ( len( radec_list_1 ) == 0 ) or ( len( radec_list_2 ) == 0 ) ):
    return assoc_list

  # calculate angular distances
  radius_array = zeros( ( len( radec_list_1 ), len( radec_list_2 ) ), dtype = float64 )
  for i in range( len( radec_list_1 ) ):
    radec_i = radec_list_1[ i ]
    for j in range( len( radec_list_2 ) ):
      radec_j = radec_list_2[ j ]
      [ r, phi ] = calculate_angular_separation( radec_i, radec_j )
      radius_array[ i, j ] = 3600. * r

  # filter out obvious source matches
  for i in range( len( radec_list_1 ) ):
    j = awhere( radius_array[ i ] == radius_array[ i ].min() )[ 0, 0 ]
    if ( i == awhere( radius_array[ : , j ] == radius_array[ : , j ].min() )[ 0, 0 ] ):
      if ( radius_array[ i, j ] < assoc_radius ):
        assoc_list.append( [ i, j ] )
      else:
        assoc_list.append( [ i, - 1 ] )
        assoc_list.append( [ - 1, j ] )
      # exclude sources from further source matching
      radius_array[ i, : ] = 1.e10
      radius_array[ : , j ] = 1.e10

  # TODO: more fancy source matching
  for i in range( len( radec_list_1 ) ):
    if ( radius_array[ i ].min() < 1.e10 ):
      assoc_list.append( [ i, - 1 ] )
  for j in range( len( radec_list_2 ) ):
    if ( radius_array[ : , j ].min() < 1.e10 ):
      assoc_list.append( [ - 1, j ] )

  return assoc_list

###############################################################################

# format example
# src# flag code tot_Jy err CenPkJy err MaxPkJy err CenRA err CenDec err MaxRA err MaxDec err bmaj_asec_fw err bmin_asec_fw err bpa_deg err deconv_bmaj_bmin_bpa_asec_fw  &errors rms_isl num_gaus
# fmt 76 "(2(i5,1x),a4,1x,6(1Pe11.3,1x),8(0Pf13.9,1x),12(0Pf10.5,1x),1(1Pe11.3,1x),i3)"
#    1     0 S      4.412E+01   1.387E-01   4.401E+01   8.014E-02   4.401E+01   8.014E-02 197.913289400   0.001773634 -22.278580020   0.026014849 197.913289400   0.001773634 -22.278580020   0.026014849   80.55641    0.14753   79.65808    0.14426   10.85062    6.54240    9.45851    0.14753    0.00000    0.14426   10.85062    6.54240   1.823E-01    1

# gaul# island# flag tot_Jy err peak_Jy err   RA err DEC err  xpos_pix err ypos_pix err bmaj_asec_fw err bmin_asec_fw err bpa_deg err deconv_bmaj_bmin_bpa_asec_fw &errors src_rms src_av isl_rms isl_av spin e_spin src#  blc1  blc2  trc1 trc2

def read_bdsm_source_catalog( cat_file_name, use_centroid = True ):
  if ( not file_exists( cat_file_name ) ):
    raise error( 'source catalog %s was not found' % cat_file_name )
  if use_centroid:
    col_list = [ 'src#', 'flag', 'code', 'CenRA', 'CenDec', 'CenPkJy', 'tot_Jy',
        'bmaj_asec_fw', 'bmin_asec_fw', 'bpa_deg' ]
  else:
    col_list = [ 'src#', 'flag', 'code', 'MaxRA', 'MaxDec', 'MaxPkJy', 'tot_Jy',
        'bmaj_asec_fw', 'bmin_asec_fw', 'bpa_deg' ]
  cat_list = [ [ 'ID', 'RA', 'DEC', 'PEAK', 'FLUX', 'MAJ', 'MIN', 'PA',
      'RA_ERROR', 'DEC_ERROR', 'PEAK_ERROR', 'FLUX_ERROR', 'MAJ_ERROR', 'MIN_ERROR', 'PA_ERROR' ] ]
  cat_file = file( cat_file_name, mode = 'r' )
  header_found = False
  for line in cat_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( not header_found ):
      if ( words[ 0 ] == col_list[ 0 ] ):
        try:
          index_list = [ words.index( col ) for col in col_list ]
        except:
          raise error( 'unexpected row format' )
        index_count = len( index_list )
        err_list = [ words[ index + 1 ] == 'err' for index in index_list[ 3 : index_count ] ]
        if ( False in err_list ):
          raise error( 'unexpected row format' )
        for index in index_list[ 3 : index_count ]:
          index_list.append( index + 1 )
      elif ( words[ 0 ] == 'fmt' ):
        header_found = True
    else:
      if ( int( words[ index_list[ 1 ] ] ) > 0 ): # flag
        continue
#      if ( words[ index_list[ 2 ] ] != 'S' ): # code
#        continue
      id = int( words[ index_list[ 0 ] ] )
      cat_list.append( [ id ] + [ float( words[ index ] ) for index in index_list[ 3 : ] ] )
  cat_file.close()

  return cat_list

###############################################################################

# Gaussian list made by bdsm.f
# Image_name BOOTES_GMRT_150_PBCOR
# Image_size_x  2737
# Image_size_y  2737
# Island_list_name BOOTES_GMRT_150_PBCOR.524
# Number_of_islands  637
# Number_of_sources  672
# Number_of_gaussians  742
# Max_gaussians_per_island  6
# RMS_map map
# Sigma   0.00188698388
# Detect_threshold   5.
# src# isl# flag code tot_Jy err CenPkJy err MaxPkJy err CenRA err CenDec err MaxRA err MaxDec err bmaj_asec_fw err bmin_asec_fw err bpa_deg err deconv_bmaj_bmin_bpa_asec_fw  &errors rms_isl num_gaus blc1 blc2 trc1 trc2
# fmt 85 "(3(i5,1x),a4,1x,6(1Pe11.3,1x),8(0Pf13.9,1x),12(0Pf10.5,1x),1(1Pe11.3,1x),i3,4(1x,i5))"
#     1     1     0 C      4.192E+00   9.394E-03   4.172E+00   5.408E-03   4.172E+00   5.408E-03 219.704046843   0.000440722  33.837557532   0.004750058 219.704046843   0.000440722  33.837557532   0.004750058   26.04024    0.03666   22.06899    0.02634   81.86934    0.29090    3.93741    0.03666    0.00000    0.02634  125.97794    0.29090   1.444E-01   1   337  1050   393  1068

def read_raw_bdsm_source_catalog( cat_file_name, drop_flagged = True, print_info = True ):
  if ( not file_exists( cat_file_name ) ):
    raise error( 'source catalog %s was not found' % cat_file_name )
  cat_file = file( cat_file_name, mode = 'r' )
  header_found = False
  cat_list = []
  dropped_count = 0
  for line in cat_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( not header_found ):
      if ( 'src#' in words ):
        flag_index = words.index( 'flag' )
        deconv_index = words.index( 'deconv_bmaj_bmin_bpa_asec_fw' )
        words = words[ 0 : deconv_index ] + [ 'deconv_bmaj_asec_fw', 'err', 
            'deconv_bmin_asec_fw', 'err', 'deconv_bpa_deg', 'err' ] + words[ deconv_index + 2 : ]
        for i in range( len( words ) ):
          if ( words[ i ] == 'err' ):
            words[ i ] = words[ i - 1 ] + '_err'
        cat_list.append( words )
      elif ( words[ 0 ] == 'fmt' ):
        header_found = True
    else:
      if ( len( words ) != len( cat_list[ 0 ] ) ):
         raise error( 'data row length deviates from header row length' )
      if ( drop_flagged and ( int( words[ flag_index ] ) > 0 ) ):
        dropped_count = dropped_count + 1
        continue
      values = []
      for word in words:
        if ( '.' in word ):
          try:
            value = float( word )
          except:
            value = word
        else:
          try:
            value = int( word )
          except:
            value = word
        values.append( value )
      cat_list.append( [ value for value in values ] )
  cat_file.close()
  if ( print_info and ( dropped_count > 0 ) ):
    print '... dropped %s flagged source components' % ( repr( droped_count ) )
  return cat_list

###############################################################################

# gaul# island# flag tot_Jy err peak_Jy err   RA err DEC err  xpos_pix err ypos_pix err bmaj_asec_fw err bmin_asec_fw err bpa_deg err deconv_bmaj_bmin_bpa_asec_fw &errors src_rms src_av isl_rms isl_av spin e_spin src#  blc1  blc2  trc1 trc2

def read_bdsm_gaussian_catalog( cat_file_name ):
  if ( not file_exists( cat_file_name ) ):
    raise error( 'source catalog %s was not found' % cat_file_name )
  col_list = [ 'gaul#', 'flag', 'RA', 'DEC', 'peak_Jy', 'tot_Jy',
      'bmaj_asec_fw', 'bmin_asec_fw', 'bpa_deg', 'xpos_pix', 'ypos_pix' ]
  cat_list = [ [ 'ID', 'RA', 'DEC', 'PEAK', 'FLUX', 'MAJ', 'MIN', 'PA', 'X', 'Y',
      'RA_ERROR', 'DEC_ERROR', 'PEAK_ERROR', 'FLUX_ERROR', 'MAJ_ERROR', 'MIN_ERROR', 'PA_ERROR',
      'X_ERROR', 'Y_ERROR' ] ]
  cat_file = file( cat_file_name, mode = 'r' )
  header_found = False
  for line in cat_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( not header_found ):
      if ( words[ 0 ] == col_list[ 0 ] ): # gaul#
        try:
          index_list = [ words.index( col ) for col in col_list ]
        except:
          raise error( 'unexpected row format' )
        index_count = len( index_list )
        err_list = [ words[ index + 1 ] == 'err' for index in index_list[ 2 : index_count ] ]
        if ( False in err_list ):
          raise error( 'unexpected row format' )
        for index in index_list[ 2 : index_count ]:
          index_list.append( index + 1 )
      elif ( words[ 0 ] == 'fmt' ):
        header_found = True
    else:
      if ( int( words[ index_list[ 1 ] ] ) > 0 ): # flag
        continue
      id = int( words[ index_list[ 0 ] ] )
      cat_list.append( [ id ] + [ float( words[ index ] ) for index in index_list[ 2 : ] ] )
  cat_file.close()
  return cat_list

###############################################################################

def match_source_catalogs( cat_list_1, cat_list_2, assoc_radius = 100., allow_double = False ):
  ra_index_1 = [ 'RA' in word for word in cat_list_1[ 0 ] ].index( True )
  dec_index_1 = [ 'DE' in word for word in cat_list_1[ 0 ] ].index( True )
  ra_index_2 = [ 'RA' in word for word in cat_list_2[ 0 ] ].index( True )
  dec_index_2 = [ 'DE' in word for word in cat_list_2[ 0 ] ].index( True )
  radec_list_1 = []
  for source in cat_list_1[ 1 : ]:
    radec_list_1.append( [ source[ ra_index_1 ], source[ dec_index_1 ] ] )
  radec_list_2 = []
  for source in cat_list_2[ 1 : ]:
    radec_list_2.append( [ source[ ra_index_2 ], source[ dec_index_2 ] ] )
  assoc_list = associate_sources( radec_list_1, radec_list_2, assoc_radius = assoc_radius )
  sub_cat_list_1 = [ cat_list_1[ 0 ] ]
  sub_cat_list_2 = [ cat_list_2[ 0 ] ]
  if allow_double:
    double_index_list_1 = []
    double_index_list_2 = []
    double_radec_list_1 = []
    double_radec_list_2 = []
  for assoc in assoc_list:
    if ( not - 1 in assoc ):
      sub_cat_list_1.append( [ x for x in cat_list_1[ assoc[ 0 ] + 1 ] ] )
      sub_cat_list_2.append( [ x for x in cat_list_2[ assoc[ 1 ] + 1 ] ] )
    elif allow_double:
      if ( assoc[ 0 ] == - 1 ):
        double_index_list_2.append( assoc[ 1 ] )
        double_radec_list_2.append( radec_list_2[ assoc[ 1 ] ] )
      else:
        double_index_list_1.append( assoc[ 0 ] )
        double_radec_list_1.append( radec_list_1[ assoc[ 0 ] ] )
  if allow_double:
    assoc_list_1 = associate_sources( double_radec_list_1, radec_list_2, assoc_radius = assoc_radius )
    assoc_list_2 = associate_sources( radec_list_1, double_radec_list_2, assoc_radius = assoc_radius )
    for assoc in assoc_list_1:
      if ( not - 1 in assoc ):
        sub_cat_list_1.append( [ x for x in cat_list_1[ double_index_list_1[ assoc[ 0 ] ] + 1 ] ] )
        sub_cat_list_2.append( [ x for x in cat_list_2[ assoc[ 1 ] + 1 ] ] )
    for assoc in assoc_list_2:
      if ( not - 1 in assoc ):
        sub_cat_list_1.append( [ x for x in cat_list_1[ assoc[ 0 ] + 1 ] ] )
        sub_cat_list_2.append( [ x for x in cat_list_2[ double_index_list_2[ assoc[ 1 ] ] + 1 ] ] )
  return [ sub_cat_list_1, sub_cat_list_2 ]

###############################################################################

def add_point_sources_to_image_old( image, flux_min, flux_list, radec_list = [], check_radius = 10,
    add_sources = True, print_info = False ):

  if ( len( radec_list ) > 0 ):
    if ( len( radec_list ) != len( flux_list ) ):
      raise error( 'lengths of flux list and RA DEC list do not match' )

  # make list of valid source pixel positions
  new_radec_list = []
  pos_list = []
  pixel_map = get_image_pixels( image )
  image_size = list( pixel_map.shape )
  sub_image_mask = zeros( ( 2 * check_radius + 1, 2 * check_radius + 1 ), dtype = bool )
  [ X, Y ] = indices( ( 2 * check_radius + 1, 2 * check_radius + 1 ), dtype = int32 )
  R2 = ( X - float( check_radius ) )**2 + ( Y - float( check_radius ) )**2
  putmask( sub_image_mask, R2 <= check_radius**2, True )
  if ( len( radec_list ) > 0 ):
    for radec in radec_list:
      pos = calculate_source_position( image, radec )
      if ( ( pos[ 0 ] < 0.5 + float( check_radius ) ) or 
           ( pos[ 0 ] > float( image_size[ 0 ] ) + 0.5 - float( check_radius ) ) or
           ( pos[ 1 ] < 0.5 + float( check_radius ) ) or 
           ( pos[ 1 ] > float( image_size[ 1 ] ) + 0.5 - float( check_radius ) ) ):
        pos = [ - 1, - 1 ]
      else:
        int_pos = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask == False, 0. )
        sel = awhere( ( sub_image == get_aips_magic_value() ) | ( abs( sub_image ) > flux_min ) )
        if ( len( sel ) == 0 ):
          new_radec_list.append( radec )
          # blank selected area in image to prevent re-selection
          sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                                 int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
          putmask( sub_image, sub_image_mask == True, get_aips_magic_value() )
          pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                     int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ] = sub_image
        else:
          pos = [ - 1, - 1 ]
          new_radec_list.append( [ -1., -1. ] )
          if print_info:
            print '... source position %s has been rejected' % ( repr( degdeg_to_hmsdms( radec ) ) )
      pos_list.append( pos )
  else:
    for i in range( len( flux_list ) ):
      pos_found = False
      while ( not pos_found ):
        pos = [ random.random() * float( image_size[ 0 ] ) + 0.5, 
                random.random() * float( image_size[ 1 ] ) + 0.5 ]
        if ( ( pos[ 0 ] < 0.5 + float( check_radius ) ) or 
             ( pos[ 0 ] > float( image_size[ 0 ] ) + 0.5 - float( check_radius ) ) or
             ( pos[ 1 ] < 0.5 + float( check_radius ) ) or 
             ( pos[ 1 ] > float( image_size[ 1 ] ) + 0.5 - float( check_radius ) ) ):
           continue
        int_pos = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask == False, 0. )
        sel = awhere( ( sub_image == get_aips_magic_value() ) | ( abs( sub_image ) > flux_min ) )
        if ( len( sel ) > 0 ):
          continue
        pos_list.append( pos )
        radec = calculate_source_radec( image, pos )
        new_radec_list.append( radec )
        pos_found = True
        # blank selected area in image to prevent re-selection
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask == True, get_aips_magic_value() )
        pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                   int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ] = sub_image

  if ( not add_sources ):
    return [ None, new_radec_list ]

  # add model sources to image
  new_image = get_aips_file( image.disk, image.name, 'ADDSRC', - 1, 'MA' )
  call_aips_task( 'MOVE', indata = image, outdata = new_image, userid = get_aips_userid() )
  sub_flux_list = []
  sub_pos_list = []
  sub_beam_list = []
  sub_count = 0
  added_source_count = 0
  for i in range( len( flux_list ) ):
    pos = pos_list[ i ]
    if ( pos != [ - 1, - 1 ] ):
      sub_flux_list.append( flux_list[ i ] )
      sub_pos_list.append( pos )
      sub_beam_list.append( convert_beam_size( image, to_pixel = True ) )
      sub_count = sub_count + 1
      added_source_count = added_source_count + 1
      if ( sub_count == 4 ):
        temp_image = get_aips_file( image.disk, image.name, 'TEMP', - 1, 'MA' )
        call_aips_task( 'IMMOD', indata = new_image, outdata = temp_image, factor = 1., opcode = 'GAUS',
            ngaus = sub_count, fmax = sub_flux_list, fpos = sub_pos_list, fwidth = sub_beam_list )
        new_image.zap()
        temp_image.rename( name = new_image.name, klass = new_image.klass, seq = new_image.seq )
        sub_flux_list = []
        sub_pos_list = []
        sub_beam_list = []
        sub_count = 0
  if ( sub_count != 0 ):
    temp_image = get_aips_file( image.disk, image.name, 'TEMP', - 1, 'MA' )
    call_aips_task( 'IMMOD', indata = new_image, outdata = temp_image, factor = 1., opcode = 'GAUS',
        ngaus = sub_count, fmax = sub_flux_list, fpos = sub_pos_list, fwidth = sub_beam_list )
    new_image.zap()
    temp_image.rename( name = new_image.name, klass = new_image.klass, seq = new_image.seq )

  if ( added_source_count == 0 ):
    new_image = None

  return [ new_image, new_radec_list ]

###############################################################################

def add_point_sources_to_image( image, flux_min, flux_list, radec_list = [], check_radius = 10,
    add_sources = True, print_info = False ):

  if ( len( radec_list ) > 0 ):
    if ( len( radec_list ) != len( flux_list ) ):
      raise error( 'lengths of flux list and RA DEC list do not match' )

  # make list of valid source pixel positions
  new_radec_list = []
  pos_list = []
  pixel_map = get_image_pixels( image )
  image_size = list( pixel_map.shape )
  sub_image_mask = zeros( ( 2 * check_radius + 1, 2 * check_radius + 1 ), dtype = bool )
  [ X, Y ] = indices( ( 2 * check_radius + 1, 2 * check_radius + 1 ), dtype = int32 )
  R2 = ( X - float( check_radius ) )**2 + ( Y - float( check_radius ) )**2
  putmask( sub_image_mask, R2 <= check_radius**2, True )
  if ( len( radec_list ) > 0 ):
    for radec in radec_list:
      pos = calculate_source_position( image, radec )
      if ( ( pos[ 0 ] < 0.5 + float( check_radius ) ) or 
           ( pos[ 0 ] > float( image_size[ 0 ] ) + 0.5 - float( check_radius ) ) or
           ( pos[ 1 ] < 0.5 + float( check_radius ) ) or 
           ( pos[ 1 ] > float( image_size[ 1 ] ) + 0.5 - float( check_radius ) ) ):
        pos = [ - 1, - 1 ]
      else:
        int_pos = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask == False, 0. )
        sel = awhere( ( sub_image == get_aips_magic_value() ) | ( abs( sub_image ) > flux_min ) )
        if ( len( sel ) == 0 ):
          new_radec_list.append( radec )
          # blank selected area in image to prevent re-selection
          sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                                 int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
          putmask( sub_image, sub_image_mask == True, get_aips_magic_value() )
          pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                     int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ] = sub_image
        else:
          pos = [ - 1, - 1 ]
          new_radec_list.append( [ -1., -1. ] )
          if print_info:
            print '... source position %s has been rejected' % ( repr( degdeg_to_hmsdms( radec ) ) )
      pos_list.append( pos )
  else:
    for i in range( len( flux_list ) ):
      pos_found = False
      while ( not pos_found ):
        pos = [ random.random() * float( image_size[ 0 ] ) + 0.5, 
                random.random() * float( image_size[ 1 ] ) + 0.5 ]
        if ( ( pos[ 0 ] < 0.5 + float( check_radius ) ) or 
             ( pos[ 0 ] > float( image_size[ 0 ] ) + 0.5 - float( check_radius ) ) or
             ( pos[ 1 ] < 0.5 + float( check_radius ) ) or 
             ( pos[ 1 ] > float( image_size[ 1 ] ) + 0.5 - float( check_radius ) ) ):
           continue
        int_pos = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask == False, 0. )
        sel = awhere( ( sub_image == get_aips_magic_value() ) | ( abs( sub_image ) > flux_min ) )
        if ( len( sel ) > 0 ):
          continue
        pos_list.append( pos )
        radec = calculate_source_radec( image, pos )
        new_radec_list.append( radec )
        pos_found = True
        # blank selected area in image to prevent re-selection
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask == True, get_aips_magic_value() )
        pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                   int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ] = sub_image

  if ( not add_sources ):
    return [ None, new_radec_list ]

  # add model sources to image
  temp_image = get_aips_file( image.disk, image.name, 'TEMP', - 1, 'MA' )
  call_aips_task( 'MOVE', indata = image, outdata = temp_image, userid = get_aips_userid() )
  pixel_ref = get_pixel_reference( temp_image )
  pixel_scale = get_pixel_size( temp_image, make_absolute = False )
  model_table = new_table( temp_image, 'CC', 0 )
  model_row = new_table_row( model_table )
  added_source_count = 0
  for i in range( len( flux_list ) ):
    pos = pos_list[ i ]
    if ( pos != [ - 1, - 1 ] ):
      model_row.deltax = ( pos[ 0 ] - pixel_ref[ 0 ] ) * pixel_scale[ 0 ] / 3600.
      model_row.deltay = ( pos[ 1 ] - pixel_ref[ 1 ] ) * pixel_scale[ 1 ] / 3600.
      model_row.flux = flux_list[ i ]
      model_table.append( model_row )
      added_source_count = added_source_count + 1
  model_table.close()
  if ( added_source_count > 0 ):
    new_image = get_aips_file( image.disk, image.name, 'ADDSRC', - 1, 'MA' )
    model_version = temp_image.table_highver( 'CC' )
    call_aips_task( 'CCRES', indata = temp_image, in2data = temp_image, invers = model_version,
        outdata = new_image, optype = 'ADD' )
    new_image.zap_table( 'CC', 0 )
  else:
    new_image = None
  temp_image.zap()

  return [ new_image, new_radec_list ]

###############################################################################

def add_point_sources_to_noise_image( image, noise, flux_list, radec_list = [],
    check_radius = 10., print_info = False ):

  if ( len( radec_list ) > 0 ):
    if ( len( radec_list ) != len( flux_list ) ):
      raise error( 'lengths of flux list and RA DEC list do not match' )

  # make list of valid source pixel positions
  new_radec_list = []
  pos_list = []
  pixel_map = get_image_pixels( image )
  image_size = list( pixel_map.shape )
  sub_image_mask = zeros( ( 2 * check_radius + 1, 2 * check_radius + 1 ), dtype = bool )
  [ X, Y ] = indices( ( 2 * check_radius + 1, 2 * check_radius + 1 ), dtype = int32 )
  R2 = ( X - float( check_radius ) )**2 + ( Y - float( check_radius ) )**2
  putmask( sub_image_mask, R2 <= check_radius**2, True )
  if ( len( radec_list ) > 0 ):
    for radec in radec_list:
      pos = calculate_source_position( image, radec )
      if ( ( pos[ 0 ] < 0.5 + float( check_radius ) ) or 
           ( pos[ 0 ] > float( image_size[ 0 ] ) + 0.5 - float( check_radius ) ) or
           ( pos[ 1 ] < 0.5 + float( check_radius ) ) or 
           ( pos[ 1 ] > float( image_size[ 1 ] ) + 0.5 - float( check_radius ) ) ):
        pos = [ - 1, - 1 ]
      else:
        int_pos = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask, 0. )
        sel = awhere( sub_image == get_aips_magic_value() )
        if ( len( sel ) == 0 ):
          new_radec_list.append( radec )
        else:
          pos = [ - 1, - 1 ]
          new_radec_list.append( [ -1., -1. ] )
          if print_info:
            print '... source position %s has been rejected' % ( repr( degdeg_to_hmsdms( radec ) ) )
      pos_list.append( pos )
  else:
    for i in range( len( flux_list ) ):
      pos_found = False
      while ( not pos_found ):
        pos = [ random.random() * float( image_size[ 0 ] ) + 0.5, 
                random.random() * float( image_size[ 1 ] ) + 0.5 ]
        if ( ( pos[ 0 ] < 0.5 + float( check_radius ) ) or 
             ( pos[ 0 ] > float( image_size[ 0 ] ) + 0.5 - float( check_radius ) ) or
             ( pos[ 1 ] < 0.5 + float( check_radius ) ) or 
             ( pos[ 1 ] > float( image_size[ 1 ] ) + 0.5 - float( check_radius ) ) ):
           continue
        int_pos = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
        sub_image = pixel_map[ int_pos[ 0 ] - check_radius : int_pos[ 0 ] + check_radius + 1, 
                               int_pos[ 1 ] - check_radius : int_pos[ 1 ] + check_radius + 1 ]
        putmask( sub_image, sub_image_mask, 0. )
        sel = awhere( sub_image == get_aips_magic_value() )
        if ( len( sel ) > 0 ):
          continue
        pos_list.append( pos )
        radec = calculate_source_radec( image, pos )
        new_radec_list.append( radec )
        pos_found = True

  # add model sources to image
  new_image = get_aips_file( image.disk, image.name, 'ADDSRC', - 1, 'MA' )
  call_aips_task( 'MOVE', indata = image, outdata = new_image, userid = get_aips_userid() )
  sub_flux_list = []
  sub_pos_list = []
  sub_beam_list = []
  sub_count = 0
  flux = noise
  factor = 0.
  for i in range( len( flux_list ) ):
    pos = pos_list[ i ]
    if ( pos != [ - 1, - 1 ] ):
      sub_flux_list.append( flux_list[ i ] )
      sub_pos_list.append( pos )
      sub_beam_list.append( convert_beam_size( image, to_pixel = True ) )
      sub_count = sub_count + 1
      if ( sub_count == 4 ):
        temp_image = get_aips_file( image.disk, image.name, 'TEMP', - 1, 'MA' )
        call_aips_task( 'IMMOD', indata = new_image, outdata = temp_image, factor = factor,
            flux = flux, opcode = 'GAUS', ngaus = sub_count, fmax = sub_flux_list,
            fpos = sub_pos_list, fwidth = sub_beam_list )
        new_image.zap()
        temp_image.rename( name = new_image.name, klass = new_image.klass, seq = new_image.seq )
        sub_flux_list = []
        sub_pos_list = []
        sub_beam_list = []
        sub_count = 0
        flux = 0.
        factor = 1.
  if ( sub_count != 0 ):
    temp_image = get_aips_file( image.disk, image.name, 'TEMP', - 1, 'MA' )
    call_aips_task( 'IMMOD', indata = new_image, outdata = temp_image, factor = factor,
        flux = flux, opcode = 'GAUS', ngaus = sub_count, fmax = sub_flux_list,
        fpos = sub_pos_list, fwidth = sub_beam_list )
    new_image.zap()
    temp_image.rename( name = new_image.name, klass = new_image.klass, seq = new_image.seq )

  return [ new_image, new_radec_list ]

###############################################################################

def write_catalog( cat, file_name ):
  row_length = len( cat[ 0 ] )
  for row in cat:
    if ( len( row ) != row_length ):
      raise error( 'catalog should have rows of equal length' )
  if file_exists( file_name ):
    remove_file( file_name )
  cat_file = file( file_name, mode = 'w' )
  line = ''
  for field in cat[ 0 ]:
    line = line + field + '  '
  line = line[ : - 2 ] + '\n'
  cat_file.write( line )
  for row in cat[ 1 : ]:
    line = ''
    for field in row:
      line = line + repr( field ) + '  '
    line = line[ : - 2 ] + '\n'
    cat_file.write( line )
  cat_file.close()
  return

###############################################################################

def read_catalog( file_name ):
  if ( not file_exists( file_name ) ):
    raise error( 'catalog file %s does not exists' % ( file_name ) )
  cat = []
  cat_file = file( file_name, mode = 'r' )
  line_count = 0
  for line in cat_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    line_count = line_count + 1
    if ( line_count == 1 ):
      row_length = len( words )
      cat.append( words )
    else:
      if ( len( words ) != row_length ):
        raise error( 'catalog should have rows of equal length' )
      values = []
      for word in words:
        if ( '.' in word ):
          values.append( float( word ) )
        else:
          values.append( int( word ) )
      cat.append( values )
  cat_file.close()
  return cat

###############################################################################

def convert_catalog_epoch_from_b1950_to_j2000( cat_list ):
  new_cat_list = [ cat_list[ 0 ] ]
  ra_index = cat_list[ 0 ].index( 'RA' )
  dec_index = cat_list[ 0 ].index( 'DEC' )
  for cat in cat_list[ 1 : ]:
    new_cat = [ field for field in cat ]
    radec = [ cat[ ra_index ], cat[ dec_index ] ]
    new_radec = convert_b1950_to_j2000( radec )
    new_cat[ ra_index ] = new_radec[ 0 ]
    new_cat[ dec_index ] = new_radec[ 1 ]
    new_cat_list.append( new_cat )
  return new_cat_list

###############################################################################

def convert_catalog_epoch_from_j2000_to_b1950( cat_list ):
  new_cat_list = [ cat_list[ 0 ] ]
  ra_index = cat_list[ 0 ].index( 'RA' )
  dec_index = cat_list[ 0 ].index( 'DEC' )
  for cat in cat_list[ 1 : ]:
    new_cat = [ field for field in cat ]
    radec = [ cat[ ra_index ], cat[ dec_index ] ]
    new_radec = convert_j2000_to_b1950( radec )
    new_cat[ ra_index ] = new_radec[ 0 ]
    new_cat[ dec_index ] = new_radec[ 1 ]
    new_cat_list.append( new_cat )
  return new_cat_list

###############################################################################

def correct_catalog_for_pb_attenuation( uvim, cat_list, cutoff = 0.1,
    invert = False, offset_rp = None ):
  new_cat_list = [ cat_list[ 0 ] ]
  ra_index = cat_list[ 0 ].index( 'RA' )
  dec_index = cat_list[ 0 ].index( 'DEC' )
  try:
    peak_index = cat_list[ 0 ].index( 'PEAK' )
  except:
    peak_index = - 1
  try:
    flux_index = cat_list[ 0 ].index( 'FLUX' )
  except:
    flux_index = - 1
  try:
    peak_err_index = cat_list[ 0 ].index( 'PEAK_ERROR' )
  except:
    peak_err_index = - 1
  try:
    flux_err_index = cat_list[ 0 ].index( 'FLUX_ERROR' )
  except:
    flux_err_index = - 1
  radec_list = []
  for cat in cat_list[ 1 : ]:
    radec = [ cat[ ra_index ], cat[ dec_index ] ]
    if ( offset_rp != None ):
      [ r, p ] = offset_rp
      radec = calculate_offset_position( radec, r, p )
    radec_list.append( radec )
  A_list = get_primary_beam_attenuations( uvim, radec_list )
  for i in range( len( A_list ) ):
    A = A_list[ i ]
    if ( A >= cutoff ):
      if invert:
        A = 1. / A
      new_cat = [ field for field in cat_list[ i + 1 ] ]
      if ( peak_index > - 1 ):
        new_cat[ peak_index ] = new_cat[ peak_index ] / A
      if ( flux_index > - 1 ):
        new_cat[ flux_index ] = new_cat[ flux_index ] / A
      if ( peak_err_index > - 1 ):
        new_cat[ peak_err_index ] = new_cat[ peak_err_index ] / A
      if ( flux_err_index > - 1 ):
        new_cat[ flux_err_index ] = new_cat[ flux_err_index ] / A
      new_cat_list.append( new_cat )
  return new_cat_list

###############################################################################

def generate_power_law_flux_list( count, alpha = - 2., flux_limits = [ 0.1, 1. ] ):
  C0 = flux_limits[ 0 ]**( alpha + 1. )
  C1 = flux_limits[ 1 ]**( alpha + 1. ) - C0
  flux_list = []
  for i in range( count ):
    r = random.random()
    flux = ( C0 + C1 * r )**( 1. / ( alpha + 1. ) )
    flux_list.append( flux )
  flux_list.sort( cmp = lambda a, b: cmp( b, a ) )
  return flux_list

###############################################################################

def correct_catalog_for_b1950_to_j2000( cat_list ):
  new_cat_list = [ cat_list[ 0 ] ]
  ra_index = cat_list[ 0 ].index( 'RA' )
  dec_index = cat_list[ 0 ].index( 'DEC' )
  for cat in cat_list[ 1 : ]:
    radec = [ cat[ ra_index ], cat[ dec_index ] ]
    new_radec = convert_b1950_to_j2000( radec )
    new_cat = [ field for field in cat ]
    new_cat[ ra_index ] = new_radec[ 0 ]
    new_cat[ dec_index ] = new_radec[ 1 ]
    new_cat_list.append( new_cat )
  return new_cat_list

###############################################################################

def write_starbase_catalog( cat, file_name ):
  row_length = len( cat[ 0 ] )
  for row in cat:
    if ( len( row ) != row_length ):
      raise error( 'catalog should have rows of equal length' )
  if file_exists( file_name ):
    remove_file( file_name )
  cat_file = file( file_name, mode = 'w' )
  line = file_name.split( '/' )[ - 1 ] + '\n'
  cat_file.write( line )
  line = ''
  for field in cat[ 0 ]:
    line = line + field + '\t'
  line = line[ : - 1 ] + '\n'
  cat_file.write( line )
  line = '-----\n'
  cat_file.write( line )
  for row in cat[ 1 : ]:
    line = ''
    for field in row:
      if ( type( field ) == type( '' ) ):
        line = line + field + '\t'
      else:
        line = line + repr( field ) + '\t'
    line = line[ : - 1 ] + '\n'
    cat_file.write( line )
  cat_file.close()
  return

###############################################################################

def read_starbase_catalog( file_name, drop_strings = False, new_header_row = None ):
  if ( not file_exists( file_name ) ):
    raise error( 'catalog file %s does not exists' % ( file_name ) )
  cat = []
  cat_file = file( file_name, mode = 'r' )
  line_count = 0
  table_found = False
  for line in cat_file:
    words = [ word.strip() for word in line.split( '\t' ) ]
    if ( len( words ) == 0 ):
      continue
    if ( len( words[ 0 ] ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( words[ 0 ][ 0 ] == '[EOD]' ):
      break
    if ( not table_found ):
      if ( words[ 0 ][ 0 ] == '-' ):
        row_length = len( last_words )
        cat.append( last_words )
        table_found = True
      else:
        last_words = [ w for w in words ]
    else:
      if ( len( words ) != row_length ):
        raise error( 'catalog should have rows of equal length' )
      values = []
      for word in words:
        try:
          if ( '.' in word ):
            values.append( float( word ) )
          else:
            values.append( int( word ) )
        except ValueError:
          values.append( word )
      cat.append( values )
  cat_file.close()
  
  if ( drop_strings and ( len( cat ) > 1 ) ):
    indices = range( len( cat[ 0 ] ) )
    for line in cat[ 1 : ]:
      for i in range( len( line ) ):
        if ( i in indices ):
          if ( type( line[ i ] ) == type( '' ) ):
            indices.remove( i )
    cat2 = []
    for line in cat:
      cat2.append( [ line[ i ] for i in indices ] )
    cat = cat2
  
  if ( new_header_row != None ):
    if ( len( new_header_row ) == len( cat[ 0 ] ) ):
      cat[ 0 ] = [ x for x in new_header_row ]
    elif ( drop_strings and ( len( new_header_row ) == row_length ) ):
      cat[ 0 ] = [ new_header_row[ i ] for i in indices ]
    else:
      raise error( 'new header row does not match number of columns' )
  
  return cat

###############################################################################

def write_lions_catalog( cat, file_name ):
  row_length = len( cat[ 0 ] )
  for row in cat:
    if ( len( row ) != row_length ):
      raise error( 'catalog should have rows of equal length' )
  if file_exists( file_name ):
    remove_file( file_name )
  cat_file = file( file_name, mode = 'w' )
  id = 1
  ra_index = cat[ 0 ].index( 'RA' )
  dec_index = cat[ 0 ].index( 'DEC' )
  flux_index = cat[ 0 ].index( 'FLUX' )
  for row in cat[ 1 : ]:
    line = ''
    for field in [ id, row[ ra_index ], row[ dec_index ], row[ flux_index ] ]:
      line = line + repr( field ).ljust( 20 )
    line = line.strip() + '\n'
    cat_file.write( line )
    id = id + 1
  cat_file.close()
  return

###############################################################################

def merge_catalog_components( cat_list, max_separation = 120. ):
# max_separation in arcsec
  
  ra_index = cat_list[ 0 ].index( 'RA' )
  dec_index = cat_list[ 0 ].index( 'DEC' )
  flux_index = cat_list[ 0 ].index( 'FLUX' )
  flux_err_index = cat_list[ 0 ].index( 'FLUX_ERROR' )
  try:
    peak_index = cat_list[ 0 ].index( 'PEAK' )
  except:
    peak_index = flux_index
  
  # group multiples
  merge_ids = [ 0 for l in cat_list ]
  merge_id = 0
  for i in range( 1, len( cat_list ) ):
    radec1 = [ cat_list[ i ][ ra_index ], cat_list[ i ][ dec_index ] ]
    for j in range( i + 1, len( sp5sc_cat ) ):
      radec2 = [ cat_list[ j ][ ra_index ], cat_list[ j ][ dec_index ] ]
      [ r, p ] = calculate_angular_separation( radec1, radec2 )
      if ( r < max_separation / 3600. ):
        if ( merge_ids[ i ] == 0 ):
          if ( merge_ids[ j ] == 0 ):
            merge_id = merge_id + 1
            merge_ids[ i ] = merge_id
            merge_ids[ j ] = merge_id
          else:
            merge_ids[ i ] = merge_ids[ j ]
        else:
          if ( merge_ids[ j ] == 0 ):
            merge_ids[ j ] = merge_ids[ i ]
          else:
            if ( merge_ids[ i ] != merge_ids[ j ] ):
              old_merge_id = merge_ids[ j ]
              for k in range( 1, len( cat_list ) ):
                if ( merge_ids[ k ] == old_merge_id ):
                  merge_id[ k ] = merge_id[ i ]
  
  # make merged catalog
  if ( merge_id == 0 ):
    merged_cat_list = [ [ x for x in s ] for s in cat_list ]
  else:
    merged_cat_list = [ [ x for x in cat_list[ 0 ] ] ]
    for i in range( 1, len( cat_list ) ):
      if ( merge_ids[ i ] == 0 ):
        merged_cat_list.append( [ x for x in cat_list[ i ] ] )
    for k in range( 1, merge_id + 1 ):
      try:
        i = merge_ids.index( k )
      except:
        continue
      else:
        l = [ x for x in cat_list[ i ] ]
        for j in range( i + 1, len( cat_list ) ):
          if ( merge_ids[ j ] == k ):
            m = [ x for x in cat_list[ j ] ]
            if ( l[ peak_index ] > m[ peak_index ] ):
              l[ flux_index ] = l[ flux_index ] + m[ flux_index ]
              l[ flux_err_index ] = sqrt( l[ flux_err_index ]**2 + m[ flux_err_index ]**2 )
            else:
              m[ flux_index ] = m[ flux_index ] + l[ flux_index ]
              m[ flux_err_index ] = sqrt( m[ flux_err_index ]**2 + l[ flux_err_index ]**2 )
              l = [ x for x in m ]
        merged_cat_list.append( l )
  
  # sort on peak flux
  merged_cat_list.sort( cmp = lambda a, b: cmp( b[ peak_index ], a[ peak_index ] ) )
  
  return merged_cat_list

###############################################################################

def read_ska_catalog( file_name, header_row = False ):
  if ( not file_exists( file_name ) ):
    raise error( 'catalog file %s does not exists' % ( file_name ) )
  cat = []
  row_length = 4
  if header_row:
    cat.append( [ 'ID', 'RA', 'DEC', 'FLUX' ] )
  cat_file = file( file_name, mode = 'r' )
  line_count = 0
  table_found = False
  for line in cat_file:
    words = [ word.strip() for word in line.split( '\t' ) ]
    if ( len( words ) == 0 ):
      continue
    if ( len( words[ 0 ] ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( len( words ) != row_length ):
      raise error( 'catalog should have rows of equal length' )
    values = []
    for word in words:
      try:
        if ( '.' in word ):
          values.append( float( word ) )
        else:
          values.append( int( word ) )
      except ValueError:
        values.append( word )
    cat.append( values )
  cat_file.close()
  return cat

###############################################################################

# format example
#
#Title: AIPS VL
#Created by      FITLD on 03-DEC-2010 10:38:11
#Last written by FITLD on 03-DEC-2010 10:38:11
#Ncol  18  Nrow 1810672    Sort cols:   1 (ASCEND)
#    Table has    30 keyword-value pairs:
#   REVISION =            1
#   BM_MAJOR =  1.2500000186265D-02
#   BM_MINOR =  1.2500000186265D-02
#   BM_PA    =  0.0000000000000D+00
#   SORTORT  =            1
#   NUM_INDE =      1810672
#   INDEX00  =            1
#   INDEX01  =        74608
#   INDEX02  =       148837
#   INDEX03  =       223936
#   INDEX04  =       299369
#   INDEX05  =       373984
#   INDEX06  =       450586
#   INDEX07  =       525193
#   INDEX08  =       600541
#   INDEX09  =       674935
#   INDEX10  =       749192
#   INDEX11  =       823509
#   INDEX12  =       898254
#   INDEX13  =       972989
#   INDEX14  =      1047472
#   INDEX15  =      1121930
#   INDEX16  =      1195172
#   INDEX17  =      1267782
#   INDEX18  =      1351430
#   INDEX19  =      1436113
#   INDEX20  =      1512483
#   INDEX21  =      1590676
#   INDEX22  =      1663460
#   INDEX23  =      1735545
#   Table can be written as a FITS ASCII table
# 
#COL. NO.        1             2            3          4          5           6           7
#     ROW  RA(2000)      DEC(2000)      PEAK INT   MAJOR AX   MINOR AX   POSANGLE    Q CENTER
#  NUMBER  DEGREE        DEGREE         JY/BEAM    DEGREE     DEGREE     DEGREEE     JY/BEAM
#       1  3.999606D-04  -3.411935D+01  2.483E-03  1.447E-02  1.250E-02  -1.259E+01  -4.436E-04
#       2  5.354161D-04  -3.844127D+01  2.388E-03  2.212E-02  1.250E-02  -2.727E+01  -1.512E-06
#
#COL. NO.       8          9          10         11         12         13         14          15
#     ROW  U CENTER    P FLUX     I RMS      POL RMS    RES RMS    RES PEAK   RES FLUX    CENTER X
#  NUMBER  JY/BEAM     JY         JY/BEAM    JY/BEAM    JY/BEAM    JY/BEAM    JY          PIXEL
#       1  -7.046E-04  7.322E-04  4.803E-04  3.008E-04  3.477E-04  8.829E-04  -5.658E-04  5.119E+02
#       2   1.209E-04  7.179E-04  4.837E-04  3.070E-04  3.629E-04  8.158E-04  -5.483E-04  5.119E+02
#
#COL. NO.          16            17            18
#     ROW      CENTER Y       FIELD         JD PROCE
#  NUMBER      PIXEL                        DAY
#       1      9.643E+02      C0000M36       2450823
#       2      8.870E+02      C0000M40       2450823

def read_aips_catalog_table( image, version = 0 ):
  # WARNING: NOT YET DONE

  
  if ( not table_exists( image, 'VL', catalog_version ) ):
    raise error( 'source catalog table does not exist' )
  wiz_im = wizardry( image )
  cat_table = wiz_im.table( 'VL', catalog_version )

  cat_list = [ [ 'ID', 'RA', 'DEC', 'PEAK', 'FLUX', 'MAJ', 'MIN', 'PA', 'RMS' ] ]
  id = 0
  for row in cat_table:
    id = id + 1
    ra = row[ 'ra(2000)' ]
    dec = row[ 'dec(2000)' ]
    peak = row.peak_int
    flux = row.i_flux
    maj = row.major_ax
    min = row.minor_ax
    pa = row.posangle
    peak_res = row.res_peak
    ra_error = row.err_dltx
    dec_error = row.err_dlty
    peak_error = row.err_peak
    flux_error = row.err_flux
    maj_error = row.err_maja
    min_error = row.err_mina
    pa_error = row.err_pa
    cat_list.append( [ id, ra, dec, peak, flux, maj, min, pa, peak_res,
        ra_error, dec_error, peak_error, flux_error, maj_error, min_error, pa_error ] )

  return cat_list


###############################################################################
