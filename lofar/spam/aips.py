###############################################################################

# import Python modules
from os import *
from sys import *
from math import *
from datetime import *

# import 3rd party modules
from numpy import *

# import ParselTongue modules
import Obit
import AIPS
from AIPSTask import *
from AIPSData import *
import Wizardry.AIPSData

# import user modules
from files import *
from sphere import *
from error import *

# HISTORY:
# 20090925 HTI: Replaced numarray by numpy.

###############################################################################

class _aips_plane_iter( object ):

  def __init__( self, data, err ):
    self._err = err
    self._data = data
    self._count = -1
    self._desc = self._data.Desc.Dict
    self._size = 1
    for i in xrange( 0, 2 ):
      self._size = self._size * self._desc[ 'inaxes' ][ i ]
    self._len = 1
    for i in xrange( 2, self._desc['naxis'] ):
      self._len = self._len * self._desc[ 'inaxes' ][ i ]
    self._blc = [ 1 for i in xrange( self._desc[ 'naxis' ] ) ]
    self._trc = [ 1 for i in xrange( self._desc[ 'naxis' ] ) ]
    self._trc[ 0 : 2 ] = self._desc[ 'inaxes' ][ 0 : 2 ]
    self._dirty = False
    return

  def __len__( self ):
    return self._size

  def next( self ):
    self._count = self._count + 1
    if ( self._count >= self._len ):
      if self._dirty:
        self.update()
      raise StopIteration
    self._fill()
    return self

  def _fill( self ):
    if self._dirty:
      self.update()
    dummy = self._count
    for i in xrange( 2, self._desc[ 'naxis' ] ):
      self._blc[ i ] = ( dummy % self._desc[ 'inaxes' ][ i ] ) + 1
      self._trc[ i ] = self._blc[ i ]
      dummy = int( floor( dummy / self._desc[ 'inaxes' ][ i ] ) )
    self._data.ReadPlane( self._err, blc = self._blc, trc = self._trc )
    if self._err.isErr:
      raise RuntimeError, "Reading image pixels"
    shape = []
    for i in xrange( 0, 2 ):
      shape.insert( 0, self._desc[ 'inaxes' ][ i ] )
    shape = tuple( shape )
    self._buffer = fromstring( self._data.PixBuf, 
        dtype = float32 ).reshape( shape )
  def update( self ):
    if self._dirty:
      self._data.PixBuf[ : ] = self._buffer.tostring()
      Obit.ImageWrite( self._data.me, self._err.me )
      if self._err.isErr:
        raise RuntimeError, "Writing image pixels"
      self._dirty = False
    return

  def _get_blc( self ):
    return self._blc
  blc = property( _get_blc )

  def _get_trc( self ):
      return self._trc
  trc = property( _get_trc )

  def _get_pixels( self ):
    pixels = array( self._buffer, dtype = float32 )
    return pixels
  def _set_pixels( self, pixels ):
    self._buffer = array( pixels, dtype = float32 )
    self._dirty = True
    return
  pixels = property( _get_pixels, _set_pixels )

###############################################################################

class aips_image( Wizardry.AIPSData.AIPSImage ):

  _initialised = False
  _iter = None
  _len = None

  def _initialise( self ):
    if not self._initialised:
      self._len = 1
      for i in xrange( 2, self._data.Desc.Dict[ 'naxis' ] ):
        self._len = self._len * self._data.Desc.Dict[ 'inaxes' ][ i ]
      self._initialised = True
    return

  def __len__( self ):
    if not self._initialised:
      self._initialise()
    return self._len

  def __getitem__( self, index ):
    if not self._initialised:
      self._initialise()
    if index >= self._len:
      raise IndexError
    if self._iter == None:
      self.__iter__()
    self._iter._count = index
    self._iter._fill()
    return self._iter

  def __iter__( self ):
    if not self._initialised:
      self._initialise()
    self._data.Open( 3, self._err )
    if self._err.isErr:
      raise RuntimeError
    self._iter = _aips_plane_iter( self._data, self._err )
    return self._iter

  def update( self ):
    _AIPSData.update( self )

  def add_table( self, name, version, **kwds ):
    if not name.startswith('AIPS '):
      name = 'AIPS ' + name
#    if ( ( name != 'AIPS Hi' ) and ( name != 'AIPS PS' ) and ( name != 'AIPS SN' ) ):
    if True:
      return self.attach_table( name, version, **kwds )
#    if ( version == 0 ):
#      version = Obit.ImageGetHighVer( self._data.me, name ) + 1
#    no_parms = 0
#    if ( 'no_parms' in kwds ):
#      no_parms = kwds[ 'no_parms' ]
#    if ( 'no_if' in kwds ):
#      no_if = kwds[ 'no_if' ]
#    if 'no_pol' in kwds:
#      no_pol = kwds[ 'no_pol' ]
#    data = Obit.ImageCastData( self._data.me )
#    if ( name == 'AIPS Hi' ):
#      Obit.TableHistory( data, [ version ], 3, name, self._err.me )
#    elif ( name == 'AIPS PS' ):
#      Obit.TablePS( data, [ version ], 3, name, self._err.me )
#    elif ( name == 'AIPS SN' ):
#      Obit.TableSN( data, [ version ], 3, name, no_pol, no_if, self._err.me )
#    else:
#      msg = 'Attaching %s tables is not implemented yet' % name
#      raise NotImplementedError, msg
#    if self._err.isErr:
#      raise RuntimeError
#    return Wizardry.AIPSData._AIPSTable( self._data, name, version )

###############################################################################

class aips_uv_data( Wizardry.AIPSData.AIPSUVData ):

  def add_table( self, name, version, **kwds ):
    if not name.startswith('AIPS '):
      name = 'AIPS ' + name
    if ( name != 'AIPS OB' ):
#    if True:
      return self.attach_table( name, version, **kwds )
    if ( version == 0 ):
      version = Obit.UVGetHighVer( self._data.me, name ) + 1
#    no_parms = 0
#    if ( 'no_parms' in kwds ):
#      no_parms = kwds[ 'no_parms' ]
#    if ( 'no_if' in kwds ):
#      no_if = kwds[ 'no_if' ]
#    if 'no_pol' in kwds:
#      no_pol = kwds[ 'no_pol' ]
    data = Obit.UVCastData( self._data.me )
    if ( name == 'AIPS OB' ):
      Obit.TableOB( data, [ version ], 3, name, self._err.me )
#    elif ( name == 'AIPS NI' ):
#      Obit.TableNI( data, [ version ], 3, name, kwds[ 'num_coef' ], self._err.me )
#    elif ( name == 'AIPS PS' ):
#      Obit.TablePS( data, [ version ], 3, name, self._err.me )
    else:
      msg = 'Attaching %s tables is not implemented yet' % name
      raise NotImplementedError, msg
    if self._err.isErr:
      raise RuntimeError
    return Wizardry.AIPSData._AIPSTable( self._data, name, version )

###############################################################################

def get_aips_userid():
  return AIPS.userno

###############################################################################

def get_aips_magic_value():
  mv_array = fromstring( 'INDEFINE', dtype = float32 )
  return mv_array[ 0 ]

###############################################################################

def get_aips_file( aips_disk, aips_name, aips_class, aips_seq, aips_type,
    use_sequence_convention = True ):

  data = None
  if ( ( aips_type == 'UV' ) or ( aips_type == 'MA' ) ):
    # fully specified AIPS file
    if ( ( aips_disk > 0 ) and ( aips_seq > 0 ) ):
      if ( aips_type == 'UV' ):
        data = AIPSUVData( aips_name, aips_class, aips_disk, aips_seq )
      else:
        data = AIPSImage( aips_name, aips_class, aips_disk, aips_seq )
    # find highest sequence on specified disk
    elif ( ( aips_disk > 0 ) and ( aips_seq == 0 ) ):
      seq = 0
      cat_disk = AIPSCat( aips_disk )[ aips_disk ]
      for cat in cat_disk:
        if ( [ cat.type, cat.klass, cat.name ] == [ aips_type, aips_class, aips_name ] ):
          if ( cat.seq > seq ):
            seq = cat.seq
            if ( aips_type == 'UV' ):
              data = AIPSUVData( aips_name, aips_class, aips_disk, seq )
            else:
              data = AIPSImage( aips_name, aips_class, aips_disk, seq )
    # find highest available sequence on specified disk
    elif ( ( aips_disk > 0 ) and ( aips_seq < 0 ) ):
      if use_sequence_convention:
        # use convention seq = ddss with dd = disk and ss = seq
        seq = aips_disk * 100
        cat_disk = AIPSCat( aips_disk )[ aips_disk ]
        for cat in cat_disk:
          if ( [ cat.type, cat.klass, cat.name ] == [ aips_type, aips_class, aips_name ] ):
            if ( ( cat.seq >= aips_disk * 100 ) and ( cat.seq < ( aips_disk + 1 ) * 100 ) and ( cat.seq >= seq ) ):
              seq = cat.seq + 1
        if ( seq < ( aips_disk + 1 ) * 100 ):
          if ( aips_type == 'UV' ):
            data = AIPSUVData( aips_name, aips_class, aips_disk, seq )
          else:
            data = AIPSImage( aips_name, aips_class, aips_disk, seq )
      else:
        # search all disks for lowest available sequence number
        seq = 1
        cat_table = AIPSCat()
        for disk in range( 1, len( AIPS.disks ) ):
          cat_disk = cat_table[ disk ]
          for cat in cat_disk:
            if ( [ cat.seq, cat.type, cat.klass, cat.name,  ] == [ aips_seq, aips_type, aips_class, aips_name ] ):
              if ( cat.seq >= seq ):
                seq = cat.seq + 1
        if ( aips_type == 'UV' ):
          data = AIPSUVData( aips_name, aips_class, aips_disk, seq )
        else:
          data = AIPSImage( aips_name, aips_class, aips_disk, seq )
    # find AIPS file on unspecified disk
    elif ( ( aips_disk <= 0 ) and ( aips_seq > 0 ) ):
      if ( use_sequence_convention and ( aips_seq > 100 ) ):
        # use convention seq = ddss with dd = disk and ss = seq
        disk = aips_seq / 100
        if ( disk >= len( AIPS.disks ) ):
          disk = 1
        if ( aips_type == 'UV' ):
          data = AIPSUVData( aips_name, aips_class, disk, aips_seq )
        else:
          data = AIPSImage( aips_name, aips_class, disk, aips_seq )
      else:
        cat_table = AIPSCat()
        for disk in range( len( AIPS.disks ) - 1, 0, - 1 ): # search in reverse order
          cat_disk = cat_table[ disk ]
          for cat in cat_disk:
            if ( [ cat.seq, cat.type, cat.klass, cat.name,  ] == [ aips_seq, aips_type, aips_class, aips_name ] ):
              if ( aips_type == 'UV' ):
                data = AIPSUVData( aips_name, aips_class, disk, aips_seq )
              else:
                data = AIPSImage( aips_name, aips_class, disk, aips_seq )
              break
          if ( data != None ):
            break
    # find highest sequence on unspecified disk
    elif ( ( aips_disk <= 0 ) and ( aips_seq == 0 ) ):
      seq = 0
      cat_table = AIPSCat()
      for disk in range( 1, len( AIPS.disks ) ):
        cat_disk = cat_table[ disk ]
        for cat in cat_disk:
          if ( [ cat.type, cat.klass, cat.name ] == [ aips_type, aips_class, aips_name ] ):
            if ( cat.seq > seq ):
              seq = cat.seq
              if ( aips_type == 'UV' ):
                data = AIPSUVData( aips_name, aips_class, disk, seq )
              else:
                data = AIPSImage( aips_name, aips_class, disk, seq )
    # find highest available sequence on unspecified disk -> disk = 1
    elif ( ( aips_disk <= 0 ) and ( aips_seq < 0 ) ):
      if use_sequence_convention:
        # use convention seq = ddss with dd = disk and ss = seq
        # allow continuation above seq = 0199, map to 36ss etc
        disk = 1
        seq = disk * 100
        cat_disk = AIPSCat( disk )[ disk ]
        for cat in cat_disk:
          if ( [ cat.type, cat.klass, cat.name ] == [ aips_type, aips_class, aips_name ] ):
            if ( cat.seq >= seq ):
              seq = cat.seq + 1
          if ( ( seq >= ( disk + 1 ) * 100 ) and ( seq < len( AIPS.disks ) * 100 ) ):
            seq = len( AIPS.disks ) * 100
        if ( aips_type == 'UV' ):
          data = AIPSUVData( aips_name, aips_class, disk, seq )
        else:
          data = AIPSImage( aips_name, aips_class, disk, seq )
      else:
        # search all disks for lowest available sequence number
        seq = 1
        cat_table = AIPSCat()
        for disk in range( 1, len( AIPS.disks ) ):
          cat_disk = cat_table[ disk ]
          for cat in cat_disk:
            if ( [ cat.seq, cat.type, cat.klass, cat.name,  ] == [ aips_seq, aips_type, aips_class, aips_name ] ):
              if ( cat.seq >= seq ):
                seq = cat.seq + 1
        if ( aips_type == 'UV' ):
          data = AIPSUVData( aips_name, aips_class, 1, seq )
        else:
          data = AIPSImage( aips_name, aips_class, 1, seq )
  else:
    raise error( 'unknown requested file type:%s' % ( aips_type ) )
  if ( data == None ):
    raise error( 'AIPS file %d.%s.%s.%d.%s could not be found or allocated' % 
        ( aips_disk, aips_name, aips_class, aips_seq, aips_type ) )
  return data

###############################################################################

def call_aips_task( task_name, **keywords ):
  task = AIPSTask( task_name )
  output_list = None
  for key in keywords.keys():
    if ( key == 'outputs' ):
      output_list = keywords[ key ]
    else:
      keyword = keywords[ key ]
      if isinstance( keyword, ndarray ):
        keyword = keyword.tolist()
      if isinstance( keyword, list ):
        element_list = [ None ]
        for element in keyword:
          if isinstance( element, ndarray ):
            element = element.tolist()
          if isinstance( element, list ):
            sub_element_list = [ None ]
            for sub_element in element:
              if ( type( sub_element ) in [ float_, float32, float64 ] ): # , float96 ] ):
                sub_element = float( sub_element )
              elif ( type( sub_element ) in [ bool, bool_, bool8, intc, intp, int_, int0, int8,
                  int16, int32, int64, uintc, uintp, uint, uint0, uint8, uint16, uint32, uint64 ] ):
                sub_element = int( sub_element )
              sub_element_list.append( sub_element )
            element_list.append( sub_element_list )
          else:
            if ( type( element ) in [ float_, float32, float64 ] ): # , float96 ] ):
              element = float( element )
            elif ( type( element ) in [ bool, bool_, bool8, intc, intp, int_, int0, int8,
                int16, int32, int64, uintc, uintp, uint, uint0, uint8, uint16, uint32, uint64 ] ):
              element = int( element )
            element_list.append( element )
        setattr( task, key, element_list )
      else:
        if ( type( keyword ) in [ float_, float32, float64 ] ): # , float96 ] ):
          keyword = float( keyword )
        elif ( type( keyword ) in [ bool, bool_, bool8, intc, intp, int_, int0, int8,
            int16, int32, int64, uintc, uintp, uint, uint0, uint8, uint16, uint32, uint64 ] ):
          keyword = int( keyword )
        setattr( task, key, keyword )
  task.go()
  if output_list != None:
    outputs = []
    for output in output_list:
      element = getattr( task, output )
      if isinstance( element, list ):
        element_list = []
        for sub_element in element[ 1 : ]:
          if isinstance( sub_element, list ):
            element_list.append( sub_element[ 1 : ] )
          else:
            element_list.append( sub_element )
        outputs.append( element_list )
      else:
        outputs.append( element )
    return outputs
  return

###############################################################################

def wizardry( uvim ):
  if isinstance( uvim, AIPSUVData ):
    wiz = aips_uv_data( uvim )
  else:
    wiz = aips_image( uvim )
  return wiz

###############################################################################

def table_exists( uvim, table_id, table_version ):
  table_found = False
  for table in uvim.tables:
    if ( table[ 1 ] == 'AIPS ' + table_id ):
      if ( table_version == 0 ) or ( table[ 0 ] == table_version ):
       table_found = True
  return table_found

###############################################################################

def get_radec( uvim ):
  for ctype in uvim.header.ctype:
    if ( ctype.find( 'RA' ) != - 1 ):
      ra_index = uvim.header.ctype.index( ctype )
    if ( ctype.find( 'DEC' ) != - 1 ):
      dec_index = uvim.header.ctype.index( ctype )
  return [ uvim.header.crval[ ra_index ], uvim.header.crval[ dec_index ] ]

###############################################################################

def set_radec( uvim, radec, shift_model = False ):
  for ctype in uvim.header.ctype:
    if ( ctype.find( 'RA' ) != - 1 ):
      ra_index = uvim.header.ctype.index( ctype )
    if ( ctype.find( 'DEC' ) != - 1 ):
      dec_index = uvim.header.ctype.index( ctype )
  wiz_uvim = wizardry( uvim )
  if ( shift_model and table_exists( uvim, 'CC', 0 ) ):
    model_version = uvim.table_highver( 'CC' )
    model_list = []
    pixel_size = get_pixel_size( uvim, make_absolute = False )
    pixel_ref = get_pixel_reference( uvim )
    cc_table = wiz_uvim.table( 'CC', model_version )
    for cc in cc_table:
      cc_x = pixel_ref[ 0 ] + 3600. * cc.deltax / pixel_size[ 0 ]
      cc_y = pixel_ref[ 1 ] + 3600. * cc.deltay / pixel_size[ 1 ]
      cc_radec = calculate_source_radec( uvim, [ cc_x, cc_y ] )
      model_list.append( [ cc_radec, cc.flux ] )
    cc_table.close()
  wiz_uvim.header.crval[ ra_index ] = radec[ 0 ]
  wiz_uvim.header.crval[ dec_index ] = radec[ 1 ]
  wiz_uvim.header.update()
  wiz_uvim = wizardry( uvim )
  if ( shift_model and table_exists( uvim, 'CC', 0 ) ):
    model_version = uvim.table_highver( 'CC' )
    uvim.zap_table( 'CC', model_version )
    cc_table = new_table( uvim, 'CC', model_version )
    cc_row = new_table_row( cc_table )
    for model in model_list:
      cc_row.flux = model[ 1 ]
      [ cc_x, cc_y ] = calculate_source_position( uvim, model[ 0 ] )
      cc_row.deltax = pixel_size[ 0 ] * ( cc_x - pixel_ref[ 0 ] ) / 3600.
      cc_row.deltay = pixel_size[ 1 ] * ( cc_y - pixel_ref[ 1 ] ) / 3600.
      cc_table.append( cc_row )
    cc_table.close()
  return

###############################################################################

def get_frequency( uvim ):
  freq_index = uvim.header.ctype.index( 'FREQ' )
  return uvim.header.crval[ freq_index ]

###############################################################################

def get_channel_count( uvim ):
  freq_index = uvim.header.ctype.index( 'FREQ' )
  return uvim.header.naxis[ freq_index ]

###############################################################################

def get_channel_width( uvim ):
  freq_index = uvim.header.ctype.index( 'FREQ' )
  return abs( uvim.header.cdelt[ freq_index ] )

###############################################################################

def get_bandwidth( uvim ):
  return get_channel_count( uvim ) * get_channel_width( uvim )

###############################################################################

def get_frequency_list( uvim ):
  freq_list = []
  freq_index = uvim.header.ctype.index( 'FREQ' )
#  for freq_i in range( uvim.header.naxis[ freq_index ] ):
  for freq_i in range( 1, 1 + uvim.header.naxis[ freq_index ] ):
#    freq_list.append( uvim.header.crval[ freq_index ] + ( float( freq_i ) + 1.5
#        - uvim.header.crpix[ freq_index ] ) * uvim.header.cdelt[ freq_index ] )
    freq_list.append( uvim.header.crval[ freq_index ] + ( float( freq_i )
        - uvim.header.crpix[ freq_index ] ) * uvim.header.cdelt[ freq_index ] )
  return freq_list

###############################################################################

def get_central_frequency( uvim ):
  return float( array( get_frequency_list( uvim ), dtype = float64 ).mean() )

###############################################################################

def get_central_wavelength( uvim ):
  return ( 299792458. / get_central_frequency( uvim ) )

###############################################################################

def get_image_size( im ):
  for ctype in im.header.ctype:
    if ( ctype.find( 'RA' ) != - 1 ):
      ra_index = im.header.ctype.index( ctype )
    elif ( ctype.find( 'XA' ) != - 1 ):
      ra_index = im.header.ctype.index( ctype )
    if ( ctype.find( 'DEC' ) != - 1 ):
      dec_index = im.header.ctype.index( ctype )
    elif ( ctype.find( 'XEC' ) != - 1 ):
      dec_index = im.header.ctype.index( ctype )
  return [ im.header.naxis[ ra_index ], im.header.naxis[ dec_index ] ]

###############################################################################

def get_pixel_size( im, make_absolute = True ):
  for ctype in im.header.ctype:
    if ( ctype.find( 'RA' ) != - 1 ):
      ra_index = im.header.ctype.index( ctype )
    if ( ctype.find( 'DEC' ) != - 1 ):
      dec_index = im.header.ctype.index( ctype )
  if make_absolute:
    pix_size = [ abs( 3600. * im.header.cdelt[ ra_index ] ), abs( 3600. * im.header.cdelt[ dec_index ] ) ]
  else:
    pix_size = [ 3600. * im.header.cdelt[ ra_index ], 3600. * im.header.cdelt[ dec_index ] ]
  return pix_size

###############################################################################

def get_pixel_reference( im ):
  for ctype in im.header.ctype:
    if ( ctype.find( 'RA' ) != - 1 ):
      ra_index = im.header.ctype.index( ctype )
    if ( ctype.find( 'DEC' ) != - 1 ):
      dec_index = im.header.ctype.index( ctype )
  pix_reference = [ im.header.crpix[ ra_index ], im.header.crpix[ dec_index ] ]
  return pix_reference

###############################################################################

def get_image_rotation( im ):
  for ctype in im.header.ctype:
    if ( ctype.find( 'RA' ) != - 1 ):
      ra_index = im.header.ctype.index( ctype )
    if ( ctype.find( 'DEC' ) != - 1 ):
      dec_index = im.header.ctype.index( ctype )
  if ( ( im.header.crota[ ra_index ] != 0. ) or ( im.header.crota[ dec_index ] != 0. ) ):
    if ( ( im.header.crota[ ra_index ] != 0. ) and ( im.header.crota[ dec_index ] != 0. ) ):
      raise error( "don't know how to handle rotations on both RA and DEC axes" )
    elif ( im.header.crota[ ra_index ] != 0. ):
      rot = im.header.crota[ ra_index ]
    else:
      rot = im.header.crota[ dec_index ]
  else:
    rot = 0.
  return rot

###############################################################################

def get_beam_size( im ):
  bmaj = 3600. * im.header.bmaj
  bmin = 3600. * im.header.bmin
  return [ bmaj, bmin, im.header.bpa ]

###############################################################################

def set_beam_size( im, beam ):
  [ bmaj, bmin, bpa ] = beam
  wizim = wizardry( im )
  wizim.header.bmaj = bmaj / 3600.
  wizim.header.bmin = bmin / 3600.
  wizim.header.bpa = bpa
  wizim.header.update()
  return

###############################################################################

def get_model_flux_from_position_area( facet, pos, radius, model_version = 0 ):
# radius in pixels
  model_flux = 0.
  [ x, y ] = pos
  pixel_size = get_pixel_size( facet, make_absolute = False )
  pixel_ref = get_pixel_reference( facet )
  radius2 = float( radius )**2
  if table_exists( facet, 'CC', model_version ):
    wiz_facet = wizardry( facet )
    cc_table = wiz_facet.table( 'CC', model_version )
    for cc in cc_table:
      cc_x = pixel_ref[ 0 ] + 3600. * cc.deltax / pixel_size[ 0 ]
      cc_y = pixel_ref[ 1 ] + 3600. * cc.deltay / pixel_size[ 1 ]
      r2 = ( cc_x - pos[ 0 ] )**2 + ( cc_y - pos[ 1 ] )**2
      if ( r2 <= radius2 ):
        model_flux = model_flux + cc.flux
    cc_table.close()
  return model_flux

###############################################################################

def get_model_flux( facet, model_version = 0 ):
  cc_flux = 0.
  if table_exists( facet, 'CC', model_version ):
    wiz_facet = wizardry( facet )
    cc_table = wiz_facet.table( 'CC', model_version )
    for cc in cc_table:
      cc_flux = cc_flux + cc.flux
    cc_table.close()
  return cc_flux

###############################################################################

def get_model_component_count( facet, cc_version ):
  cc_count = 0
  if table_exists( facet, 'CC', cc_version ):
    wiz_facet = wizardry( facet )
    cc_table = wiz_facet.table( 'CC', cc_version )
    for cc in cc_table:
      cc_count = cc_count + 1
  cc_table.close()
  return cc_count

###############################################################################

def model_table_empty( facet, cc_version ):
  table_empty = True
  if table_exists( facet, 'CC', cc_version ):
    wiz_facet = wizardry( facet )
    cc_table = wiz_facet.table( 'CC', cc_version )
    for cc in cc_table:
      if ( cc.flux != 0. ):
        table_empty = False
        break
    cc_table.close()
  return table_empty

###############################################################################

def change_source_name( uv, old_name, new_name ):
  wiz_uv = wizardry( uv )
  su_table = wiz_uv.table( 'SU', 1 )
  for row in su_table:
    if ( row.source.strip() == old_name.strip() ):
      row.source = new_name.strip().ljust( len( row.source ) )
      row.update()
  su_table.close()
  su_table = wiz_uv.table( 'SU', 1 )
  return

###############################################################################

def add_circular_clean_box( facet_file_name, facet_id, pos, clean_box_radius ):
# clean_box_radius in pixels
  if ( not file_exists( facet_file_name ) ):
    raise error( 'facet file %s does not exist' % facet_file_name )
  facet_file = file( facet_file_name, mode = 'a' )
  facet_file.write( '%05d  %d  %d  %d %d\n' %
      ( facet_id, - 1, int( around( clean_box_radius ) ),
      int( around( pos[ 0 ] ) ), int( around( pos[ 1 ] ) ) ) )
  facet_file.close()
  return

###############################################################################

def add_rectangular_clean_box( facet_file_name, facet_id, pos1, pos2 ):
# clean_box_radius in pixels
  if ( not file_exists( facet_file_name ) ):
    raise error( 'facet file %s does not exist' % facet_file_name )
  facet_file = file( facet_file_name, mode = 'a' )
  facet_file.write( '%05d  %d %d  %d %d\n' %
      ( facet_id, int( around( pos1[ 0 ] ) ), int( around( pos1[ 1 ] ) ),
                  int( around( pos2[ 0 ] ) ), int( around( pos2[ 1 ] ) ) ) )
  facet_file.close()
  return

###############################################################################

def remove_rectangular_clean_box( facet_file_name, facet_id, pos1, pos2 ):
# clean_box_radius in pixels
  if ( not file_exists( facet_file_name ) ):
    raise error( 'facet file %s does not exist' % facet_file_name )
  # scan box file for line(s), keep all other lines
  keep_lines = []
  facet_file = file( facet_file_name, mode = 'r' )
  for line in facet_file:
    line_found = False
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( len( words ) == 5 ):
      try:
        line_id = int( words[ 0 ] )
      except:
        pass
      else:
        if ( ( line_id == facet_id ) and 
            ( int( words[ 1 ] ) == int( around( pos1[ 0 ] ) ) ) and
            ( int( words[ 2 ] ) == int( around( pos1[ 1 ] ) ) ) and
            ( int( words[ 3 ] ) == int( around( pos2[ 0 ] ) ) ) and
            ( int( words[ 4 ] ) == int( around( pos2[ 1 ] ) ) ) ):
          line_found = True
    if ( not line_found ):
      keep_lines.append( line )
  # write kept lines to box file
  if ( len( keep_lines ) > 0 ):
    remove_file( facet_file_name )
    facet_file = file( facet_file_name, mode = 'w' )
    for line in keep_lines:
      facet_file.write( line )
    facet_file.close()

  return

###############################################################################

def remove_circular_clean_box( facet_file_name, facet_id, pos, clean_box_radius ):
# clean_box_radius in pixels
  if ( not file_exists( facet_file_name ) ):
    raise error( 'facet file %s does not exist' % facet_file_name )
  # scan box file for line(s), keep all other lines
  keep_lines = []
  facet_file = file( facet_file_name, mode = 'r' )
  for line in facet_file:
    line_found = False
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( len( words ) == 5 ):
      try:
        line_id = int( words[ 0 ] )
      except:
        pass
      else:
        if ( ( line_id == facet_id ) and ( int( words[ 1 ] ) == - 1 ) and
            ( int( words[ 2 ] ) == int( around( clean_box_radius ) ) ) and
            ( int( words[ 3 ] ) == int( around( pos[ 0 ] ) ) ) and
            ( int( words[ 4 ] ) == int( around( pos[ 1 ] ) ) ) ):
          line_found = True
    if ( not line_found ):
      keep_lines.append( line )
  # write kept lines to box file
  if ( len( keep_lines ) > 0 ):
    remove_file( facet_file_name )
    facet_file = file( facet_file_name, mode = 'w' )
    for line in keep_lines:
      facet_file.write( line )
    facet_file.close()

  return

###############################################################################

def add_facet( facet_file_name, radec, facet_size, facet_id = 0, add_clean_box = True ):

# TODO: take proper care of roundoff towards upper boundary of hmsdms in boxfile
# Note that AIPS doesn't seem to mind

  if file_exists( facet_file_name ):
    if ( facet_id == 0 ):
      new_id = get_facet_count( facet_file_name, count_gaps = True ) + 1
    else:
      # check if field ID is not in use
      facet_file = file( facet_file_name, mode = 'r' )
      for line in facet_file:
        words = [ word.strip() for word in line.split() ]
        if ( len( words ) == 0 ):
          continue
        if ( words[ 0 ][ 0 ] == '#' ):
          continue
        if ( words[ 0 ] == 'C' ) or ( words[ 0 ] == 'F' ):
          facet_number = int( words[ 1 ] )
          if ( facet_number == facet_id ):
            raise error( 'facet ID is already in use' )
      facet_file.close()
      new_id = facet_id
    facet_file = file( facet_file_name, mode = 'a' )
  else:
    if ( facet_id == 0 ):
      new_id = 1
    else:
      new_id = facet_id
    facet_file = file( facet_file_name, mode = 'w' )
  ra_dec = degdeg_to_hmsdms( radec, precision = [ 3, 2 ] )
  if ( asign( ra_dec[ 3 ] ) > 0. ):
    facet_line = ( 'C  %d  %d %d  %02d %02d %06.3f   %02d %02d %05.2f\n' %
        ( new_id, facet_size[ 0 ] - 12, facet_size[ 1 ] - 12,
        ra_dec[ 0 ], ra_dec[ 1 ], ra_dec[ 2 ], ra_dec[ 3 ], ra_dec[ 4 ], ra_dec[ 5 ] ) )
  else:
    facet_line = ( 'C  %d  %d %d  %02d %02d %06.3f  -%02d %02d %05.2f\n' % 
        ( new_id, facet_size[ 0 ] - 12, facet_size[ 1 ] - 12,
        ra_dec[ 0 ], ra_dec[ 1 ], ra_dec[ 2 ], - ra_dec[ 3 ], ra_dec[ 4 ], ra_dec[ 5 ] ) )
  facet_file.write( facet_line )
  facet_file.close()

  if add_clean_box:
    add_circular_clean_box( facet_file_name, new_id,
        [ 1 + int( floor( ( facet_size[ 0 ] - 1. ) / 2. ) ), 1 + int( ceil( ( facet_size[ 1 ] - 1. ) / 2. ) ) ],
        int( around( ( min( facet_size ) - 12 + 1 ) / 2 ) ) )

  return

###############################################################################

def remove_facet( facet_file_name, facet_id ):

  if ( not file_exists( facet_file_name ) ):
    raise error( 'facet file %s does not exist' % facet_file_name )
  # scan box file for line(s), keep all other lines
  keep_lines = []
  facet_file = file( facet_file_name, mode = 'r' )
  for line in facet_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    try:
      facet_number = int( words[ 0 ] )
    except:
      try:
        facet_number = int( words[ 1 ] )
      except:
        keep_lines.append( line )
      else:
        if not ( ( words[ 0 ] in [ 'C', 'F' ] ) and ( facet_number == facet_id ) ):
          keep_lines.append( line )
    else:
      if ( facet_number != facet_id ):
        keep_lines.append( line )

  # write kept lines to box file
  remove_file( facet_file_name )
  facet_file = file( facet_file_name, mode = 'w' )
  for line in keep_lines:
    facet_file.write( line )
  facet_file.close()

  return

###############################################################################

def replace_facet( facet_file_name, facet_id, facet_size, radec, keep_boxes = False ):

  if ( not file_exists( facet_file_name ) ):
    raise error( 'facet file %s does not exist' % facet_file_name )
  # scan box file for line(s), keep all other lines
  keep_lines = []
  facet_file = file( facet_file_name, mode = 'r' )
  for line in facet_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    try:
      facet_number = int( words[ 0 ] )
    except:
      try:
        facet_number = int( words[ 1 ] )
      except:
        keep_lines.append( line )
      else:
        if ( ( words[ 0 ] != 'C' ) or ( facet_number != facet_id ) ):
          keep_lines.append( line )
        else:
          ra_dec = degdeg_to_hmsdms( radec, precision = [ 3, 2 ] )
          if ( asign( ra_dec[ 3 ] ) > 0. ):
            facet_line = ( 'C  %d  %d %d  %02d %02d %06.3f   %02d %02d %05.2f\n' % 
                ( facet_id, facet_size[ 0 ] - 12, facet_size[ 1 ] - 12,
                ra_dec[ 0 ], ra_dec[ 1 ], ra_dec[ 2 ], ra_dec[ 3 ], ra_dec[ 4 ], ra_dec[ 5 ] ) )
          else:
            facet_line = ( 'C  %d  %d %d  %02d %02d %06.3f  -%02d %02d %05.2f\n' % 
                ( facet_id, facet_size[ 0 ] - 12, facet_size[ 1 ] - 12,
                ra_dec[ 0 ], ra_dec[ 1 ], ra_dec[ 2 ], - ra_dec[ 3 ], ra_dec[ 4 ], ra_dec[ 5 ] ) )
          keep_lines.append( facet_line )
    else:
      if ( keep_boxes or ( facet_number != facet_id ) ):
        keep_lines.append( line )

  # write kept lines to box file
  if ( len( keep_lines ) > 0 ):
    remove_file( facet_file_name )
    facet_file = file( facet_file_name, mode = 'w' )
    for line in keep_lines:
      facet_file.write( line )
    facet_file.close()

  return

###############################################################################

def get_facet_count( facet_file_name, count_gaps = False ):
  facet_count = 0
  facet_list = get_facet_list( facet_file_name )
  if count_gaps:
    for facet in facet_list:
      if ( facet[ 0 ] > facet_count ):
        facet_count = facet[ 0 ]
  else:
    facet_count = len( facet_list )
  return facet_count

###############################################################################

def get_facet_list( facet_file_name ):
  radec_list = []
  if file_exists( facet_file_name ):
    facet_file = file( facet_file_name, mode = 'r' )
    for line in facet_file:
      words = [ word.strip() for word in line.split() ]
      if ( len( words ) == 0 ):
        continue
      if ( words[ 0 ][ 0 ] == '#' ):
        continue
      if ( words[ 0 ] == 'C' ):
        facet_id = int( words[ 1 ] )
        hmsdms = [ float( word ) for word in words[ 4 : 10 ] ]
        radec_list.append( [ facet_id, hmsdms_to_degdeg( hmsdms ) ] )
      elif ( words[ 0 ] == 'F' ):
        raise error( 'interpretation of RA/DEC shift not implemented' )
    facet_file.close()
  return radec_list

###############################################################################

def get_history_size( uvim ):
  if not table_exists( uvim, 'HI', 1 ):
    raise error( 'AIPS HI table does not exist' )
  hi_table = uvim.history
  decade = 0.
  decade_found = False
  while ( not decade_found ):
    decade = decade + 1.
    index = int( pow( 10., decade ) )
    try:
      dummy = hi_table[ index ]
    except IndexError:
      decade_found = True
  index_low = int( pow( 10., decade - 1. ) )
  index_high = int( pow( 10., decade ) )
  size_found = False
  while ( not size_found ):
    index = int( around( float( index_low + index_high ) / 2. ) )
    try:
      dummy = hi_table[ index ]
    except IndexError:
      index_high = index
    else:
      index_low = index
    if ( index_high == index_low + 1 ):
      size_found = True

  return index_high

###############################################################################

def write_history( uvim, strings ):
  if not table_exists( uvim, 'HI', 1 ):
    raise error( 'AIPS HI table does not exist' )
  wiz_uvim = wizardry( uvim )
  hi_table = wiz_uvim.history
  for string in strings:
    hi_table.append( string )
  hi_table.close()

###############################################################################

def read_history( uvim, strings = [], count = 1, word_match = False ):
# strings = search terms
# count = 0: return all occurrences
# count = N: return first N occurrences
# count = -N: return last N occurrences in reverse order

  if not table_exists( uvim, 'HI', 1 ):
    raise error( 'AIPS HI table does not exist' )
  hi_table = uvim.history
  hi_list = []
  if ( count >= 0 ):
    found_count = 0
    for line in hi_table:
      if word_match:
        words = [ word.strip() for word in line.split() ]
      if ( len( strings ) == 0 ):
        hi_list.append( line )
        found_count = found_count + 1
      else:
        found = True
        for string in strings:
          if ( line.find( string ) == - 1 ):
            found = False
            break
          elif word_match:
            try:
              dummy = words.index( string )
            except:
              found = False
              break
        if found:
          hi_list.append( line )
          found_count = found_count + 1
      if ( found_count == count ):
        break
  else:
    history_size = get_history_size( uvim )
    found_count = 0
    for index in range( history_size - 1, -1, -1 ):
      line = hi_table[ index ]
      if word_match:
        words = [ word.strip() for word in line.split() ]
      if len( strings ) == 0:
        hi_list.append( line )
        found_count = found_count + 1
      else:
        found = True
        for string in strings:
          if ( line.find( string ) == - 1 ):
            found = False
            break
          elif word_match:
            try:
              dummy = words.index( string )
            except:
              found = False
              break
        if found:
            hi_list.append( line )
            found_count = found_count + 1
      if ( found_count == - count ):
        break

  return hi_list

###############################################################################

def get_array_position( uv, antenna_version = 0 ):
  wiz_uv = wizardry( uv )
  an_table = wiz_uv.table( 'AN', antenna_version )
  array_xyz = array( [ an_table.keywords[ 'ARRAYX' ], an_table.keywords[ 'ARRAYY' ],
      an_table.keywords[ 'ARRAYZ' ] ], dtype = float64 )
  an_table.close()
  return array_xyz.tolist()

###############################################################################

def get_antenna_positions( uv, antenna_version = 0 ):
  array_xyz = array( get_array_position( uv, antenna_version = antenna_version ),
      dtype = float64 )
  wiz_uv = wizardry( uv )
  an_table = wiz_uv.table( 'AN', antenna_version )
  antenna_list = []
  if alltrue( array_xyz == azeros( array_xyz ) ):
    for row in an_table:
      antenna_no = row.nosta
      antenna_xyz = array( row.stabxyz, dtype = float64 )
      # fix Y-coordinate if needed (adopted from James Anderson's AlbusIonosphere code)
      array_name = an_table.keywords['ARRNAM']
      if ( type( array_name ) == type( [] ) ):
        array_name = array_name[ 0 ]
      array_name = array_name.strip()
      if ( ( array_name != 'ATCA' ) and ( array_name != 'ATLBA' ) ):
        antenna_xyz[ 1 ] = - antenna_xyz[ 1 ]
      antenna_list.append( [ antenna_no, antenna_xyz.tolist() ] )
  else:
    array_geo_llh = array( xyz_to_geo_llh( array_xyz.tolist() ), dtype = float64 )
    [ lon, lat ] = aradians( array_geo_llh[ 0 : 2 ] )
    rotation = array( [ [   cos( lon ), sin( lon ), 0. ],
                        [ - sin( lon ), cos( lon ), 0. ],
                        [           0.,         0., 1. ] ], dtype = float64 )
    for row in an_table:
      antenna_no = row.nosta
      dxyz = dot( array( row.stabxyz, dtype = float64 ), rotation )
      antenna_xyz = array_xyz + dxyz
      antenna_list.append( [ antenna_no, antenna_xyz.tolist() ] )
    an_table.close()
  antenna_list.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
  return antenna_list

###############################################################################

def get_mean_antenna_position( uv, antenna_version = 0 ):
  antenna_list = get_antenna_positions( uv, antenna_version )
  antenna_array = transpose( array( [ antenna[ 1 ] for antenna in antenna_list ],
      dtype = float64 ) )
  array_xyz = [ float( ( antenna_array[ 0 ] ).mean() ), float( ( antenna_array[ 1 ] ).mean() ),
      float( ( antenna_array[ 2 ] ).mean() ) ]
  return array_xyz

###############################################################################

def get_local_antenna_positions( uv, antenna_version = 0 ):
  antenna_list = []
  array_xyz = array( get_array_position( uv, antenna_version ), dtype = float64 )
  if alltrue( array_xyz == azeros( array_xyz ) ):
     used_mean_position = True
     array_xyz = array( 
         get_mean_antenna_position( uv, antenna_version = antenna_version ),
         dtype = float64 )
  else:
     used_mean_position = False
  array_geo_llh = array( xyz_to_geo_llh( array_xyz.tolist() ), dtype = float64 )
  [ lon, lat ] = aradians( array_geo_llh[ 0 : 2 ] )
  rotation = array( [ [ 0., - sin( lat ), cos( lat ) ], 
                      [ 1.,           0.,         0. ],
                      [ 0.,   cos( lat ), sin( lat ) ] ], dtype = float64 )
  wiz_uv = wizardry( uv )
  an_table = wiz_uv.table( 'AN', antenna_version )
  for row in an_table:
    antenna_no = row.nosta
    antenna_xyz = array( row.stabxyz, dtype = float64 )
    if used_mean_position:
      # fix Y-coordinate if needed (adopted from James Anderson's AlbusIonosphere code)
      array_name = an_table.keywords['ARRNAM']
      if ( type( array_name ) == type( [] ) ):
        array_name = array_name[ 0 ]
      array_name = array_name.strip()
      if ( array_name != 'ATCA' ) and ( array_name != 'ATLBA' ):
        antenna_xyz[ 1 ] = - antenna_xyz[ 1 ]
      antenna_xyz = antenna_xyz - array_xyz
    local_xyz = dot( antenna_xyz, rotation )
    antenna_list.append( [ antenna_no, local_xyz.tolist() ] )
  an_table.close()
  antenna_list.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
  return antenna_list

###############################################################################

def get_antenna_names( uv ):
  antenna_list = []
  wiz_uv = wizardry( uv )
  an_table = wiz_uv.table( 'AN', 0 )
  for row in an_table:
    antenna_no = row.nosta
    ant_name = row.anname
    antenna_list.append( [ antenna_no, ant_name ] )
  an_table.close()
  antenna_list.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
  return antenna_list

###############################################################################

def get_gst_list( uv, time_list = [ 0. ] ):
# time in days since reference date
# TODO: check if implementation below is correct
  wiz_uv = wizardry( uv )
  an_table = wiz_uv.table( 'AN', 0 )
  gst0 = an_table.keywords[ 'GSTIA0' ] # degrees
  deg_per_day = an_table.keywords[ 'DEGPDY' ]
  time_system = an_table.keywords[ 'TIMSYS' ]
  if ( type( time_system ) == type( [] ) ):
    time_system = time_system[ 0 ]
  time_system = time_system.strip()
  try:
    dat_utc = an_table.keywords[ 'DATUTC' ] # sec
  except:
    dat_utc = 0.
  if ( time_system == 'UTC' ):
    iat_utc = an_table.keywords[ 'IATUTC' ] # sec
  elif ( time_system == 'IAT' ):
    iat_utc = 0.
  else:
    raise error( 'time system %s unknown' % ( time_system ) )
  day_offset_array = array( time_list, dtype = float64 ) + ( iat_utc - dat_utc ) / 86400.
  gst_array = amodulo( day_offset_array * deg_per_day + gst0, 360. )
  an_table.close()
  return gst_array.tolist()

###############################################################################

def calculate_rise_transit_set_times( uv, radec = None, alt_limit = 0. ):
  array_xyz = array( get_array_position( uv ), dtype = float64 )
  if alltrue( array_xyz == azeros( array_xyz ) ):
    array_xyz = array( get_mean_antenna_position( uv ), dtype = float64 )
  [ lon, lat, h ] = xyz_to_geo_llh( array_xyz.tolist() )
  if ( radec == None ):
    ra_dec = get_radec( uv )
  else:
    ra_dec = radec
  obs_epoch = get_observing_epoch( uv )
  [ ra, dec ] = convert_radec_from_j2000( ra_dec, obs_epoch )
  gst0 = ( ( get_gst_list( uv ) )[ 0 ] ) % 360. # GST at midnight on reference date
  ha0 = ( gst0 - ra + lon ) % 360. # HA of object at midnight on ref date
  time_transit = ( ( - ha0 ) % 360. ) / 360. # transit occurs at hour angle 0 by default
  ha = calculate_hour_angles_at_elevation_limit( lat, dec, elevation_limit = alt_limit )
  time_rise = ( ( ha[ 0 ] - ha0 ) % 360. ) / 360.
  time_set = ( ( ha[ 1 ] - ha0 ) % 360. ) / 360.
  return [ time_rise, time_transit, time_set ]

###############################################################################

def extract_facet_definitions( facet_file_name, facet_list, new_facet_file_name, 
    include_clean_boxes = True, new_clean_box_radius = 0, include_bcomp = True,
    include_weights = True ):

  facet_file = file( facet_file_name, mode = 'r' )
  if file_exists( new_facet_file_name ):
    remove_file( new_facet_file_name )
  new_facet_file = file( new_facet_file_name, mode = 'w' )
  for line in facet_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( words[ 0 ] == 'C' ):
      facet_number = int( words[ 1 ] )
      if ( facet_number in facet_list ):
        new_facet_number = facet_list.index( facet_number ) + 1
        new_line = '%s  %s  %s %s  %s %s %s  %s %s %s\n' % ( words[ 0 ], repr( new_facet_number ),
            words[ 2 ], words[ 3 ], words[ 4 ], words[ 5 ], words[ 6 ], words[ 7 ], words[ 8 ], words[ 9 ] )
        new_facet_file.write( new_line )
    elif ( words[ 0 ] == 'F' ):
      facet_number = int( words[ 1 ] )
      if ( facet_number in facet_list ):
        new_facet_number = facet_list.index( facet_number ) + 1
        new_line = '%s  %s  %s %s  %s %s\n' % ( words[ 0 ], repr( new_facet_number ),
            words[ 2 ], words[ 3 ], words[ 4 ], words[ 5 ] )
        new_facet_file.write( new_line )
    elif ( words[ 0 ] == 'B' ):
      if include_bcomp:
        facet_number = int( words[ 1 ] )
        if ( facet_number in facet_list ):
          new_facet_number = facet_list.index( facet_number ) + 1
          new_line = '%s  %s  %s\n' % ( words[ 0 ], repr( new_facet_number ), words[ 2 ] )
          new_facet_file.write( new_line )
    elif ( words[ 0 ] == 'W' ):
      if include_weights:
        new_facet_file.write( line )
    else: # must be a clean box definition
      if include_clean_boxes:
        facet_number = int( words[ 0 ] )
        if ( facet_number in facet_list ):
          new_facet_number = facet_list.index( facet_number ) + 1
          if ( int( words[ 1 ] ) == - 1 ):
            if ( new_clean_box_radius > 0 ):
              new_line = '%05d  %s  %d  %s %s\n' % ( int( new_facet_number ), 
                  words[ 1 ], int( new_clean_box_radius ), words[ 3 ], words[ 4 ] )
            else:
              new_line = '%05d  %s  %s  %s %s\n' % ( int( new_facet_number ), 
                  words[ 1 ], words[ 2 ], words[ 3 ], words[ 4 ] )
          else:
            new_line = '%05d  %s %s  %s %s\n' % ( int( new_facet_number ), 
                words[ 1 ], words[ 2 ], words[ 3 ], words[ 4 ] )
          new_facet_file.write( new_line )
  facet_file.close()
  new_facet_file.close()
  return

###############################################################################

def merge_facet_definitions( facet_file_name_1, facet_file_name_2, new_facet_file_name ):
  
  # open facet files
  facet_file_1 = file( facet_file_name_1, mode = 'r' )
  facet_file_2 = file( facet_file_name_2, mode = 'r' )
  if file_exists( new_facet_file_name ):
    remove_file( new_facet_file_name )
  new_facet_file = file( new_facet_file_name, mode = 'w' )
  
  # copy contents of first facet file
  # look for highest facet number
  facet_count_1 = 0
  for line in facet_file_1:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( words[ 0 ] == 'C' ):
      facet_number = int( words[ 1 ] )
      if ( facet_number > facet_count_1 ):
        facet_count_1 = facet_number
    elif ( words[ 0 ] == 'F' ):
      facet_number = int( words[ 1 ] )
      if ( facet_number > facet_count_1 ):
        facet_count_1 = facet_number
    new_facet_file.write( line )
  facet_file_1.close()
  
  # copy contents of second facet file
  # adjust facet numbers
  facet_count_2 = 0
  for line in facet_file_2:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( words[ 0 ] == 'C' ):
      facet_number = int( words[ 1 ] )
      if ( facet_number > facet_count_2 ):
        facet_count_2 = facet_number
      new_facet_number = facet_count_1 + facet_number
      new_line = '%s  %s  %s %s  %s %s %s  %s %s %s\n' % ( words[ 0 ], 
          repr( new_facet_number ), words[ 2 ], words[ 3 ], words[ 4 ],
          words[ 5 ], words[ 6 ], words[ 7 ], words[ 8 ], words[ 9 ] )
    elif ( words[ 0 ] == 'F' ):
      facet_number = int( words[ 1 ] )
      if ( facet_number > facet_count_2 ):
        facet_count_2 = facet_number
      new_facet_number = facet_count_1 + facet_number
      new_line = '%s  %s  %s %s  %s %s\n' % ( words[ 0 ], repr( new_facet_number ),
          words[ 2 ], words[ 3 ], words[ 4 ], words[ 5 ] )
    elif ( ( words[ 0 ] == 'B' ) or ( words[ 0 ] == 'W' ) ):
      new_line = line
    else: # must be a clean box definition
      facet_number = int( words[ 0 ] )
      new_facet_number = facet_count_1 + facet_number
      if ( int( words[ 1 ] ) == - 1 ):
        new_line = '%05d  %s  %s  %s %s\n' % ( int( new_facet_number ), 
            words[ 1 ], words[ 2 ], words[ 3 ], words[ 4 ] )
      else:
        new_line = '%05d  %s %s  %s %s\n' % ( int( new_facet_number ), 
            words[ 1 ], words[ 2 ], words[ 3 ], words[ 4 ] )
    new_facet_file.write( new_line )
  facet_file_2.close()
  
  new_facet_file.close()
  
  return [ facet_count_1, facet_count_2 ]

###############################################################################

def fill_facet( facet, facet_file_name = '', invert = False, blank_edge = 6,
    do_edge_circle = False, value = get_aips_magic_value() ):

  # copy facet pixel contents
  facet_size = get_image_size( facet )
  wiz_facet = wizardry( facet )
  plane = wiz_facet[ 0 ]
  pixels = transpose( plane.pixels.copy() )

  [ X, Y ] = indices( tuple( facet_size ), dtype = int32 )
  mask = zeros( shape = pixels.shape, dtype = bool )
  if ( facet_file_name != '' ):
    # extract facet definition and clean boxes
    i = get_facet_number( facet )
    facet_i_file_name = facet_file_name + '.%03d' % ( i )
    extract_facet_definitions( facet_file_name, [ i ], facet_i_file_name,
        include_bcomp = False, include_weights = False )

    # read clean box definitions from facet file
    # fill mask with ones in clean box areas
    facet_i_file = file( facet_i_file_name, mode = 'r' )
    for line in facet_i_file:
      words = [ word.strip() for word in line.split() ]
      if ( len( words ) == 0 ):
        continue
      if ( words[ 0 ][ 0 ] == '#' ):
        continue
      try:
        facet_number = int( words[ 0 ] )
      except ValueError:
        pass
      else:
        if ( int( words[ 1 ] ) == - 1 ): # circular area
          rc = int( words[ 2 ] )
          xc = int( words[ 3 ] ) - 1
          yc = int( words[ 4 ] ) - 1
          rc2 = float( rc )**2
          R2 = ( X - float( xc ) )**2 + ( Y - float( yc ) )**2
          putmask( mask, R2 <= rc2, True )
        else: # rectangular area
          x1 = int( words[ 1 ] ) - 1
          y1 = int( words[ 2 ] ) - 1
          x2 = int( words[ 3 ] ) - 1
          y2 = int( words[ 4 ] ) - 1
          putmask( mask, ( X >= x1 ) & ( X <= x2 ) & ( Y >= y1 ) & ( Y <= y2 ), True )
    facet_i_file.close()
    remove_file( facet_i_file_name )

  # invert mask when requested
  if invert:
    mask = ( mask == False )

  # exclude edge pixels of facet
  if ( blank_edge > 0 ):
    if do_edge_circle:
      # do circle
      xc = float( facet_size[ 0 ] - 1 ) / 2.
      yc = float( facet_size[ 1 ] - 1 ) / 2.
      rc = min( [ xc, yc ] ) - float( blank_edge )
      if ( rc > 0. ):
        rc2 = rc**2
        R2 = ( X - float( xc ) )**2 + ( Y - float( yc ) )**2
        putmask( mask, R2 > rc2, True )
      else:
        raise error( 'size of blanking circle is too large' )
    else:
      # do simple rectangle
      mask[ 0 : blank_edge, : ] = True
      mask[ facet_size[ 0 ] - blank_edge : facet_size[ 0 ], : ] = True
      mask[ : , 0 : blank_edge ] = True
      mask[ : , facet_size[ 1 ] - blank_edge : facet_size[ 1 ] ] = True

  # write blanks at masked values
  putmask( pixels, ( mask == True ), value )
  plane.pixels = transpose( pixels )
  plane.update()

  return

###############################################################################

def get_pixel_value( image, pos, to_float = True ):
  image_size = get_image_size( image )
  [ x, y ] = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
  if ( ( x < 0 ) or ( x > image_size[ 0 ] - 1 ) or ( y < 0 ) or ( y > image_size[ 1 ] - 1 ) ):
    value = get_aips_magic_value()
  else:
    pixels = get_image_pixels( image )
    value = pixels[ x, y ]
  if to_float:
    value = float( value )
  return value

###############################################################################

def set_pixel_value( image, pos, value ):
  image_size = get_image_size( image )
  [ x, y ] = [ int( around( pos[ 0 ] ) ) - 1, int( around( pos[ 1 ] ) ) - 1 ]
  if ( ( x < 0 ) or ( x > image_size[ 0 ] - 1 ) or ( y < 0 ) or ( y > image_size[ 1 ] - 1 ) ):
    raise error( 'specified position outside image domain' )
  wiz_im = wizardry( image )
  plane = wiz_im[ 0 ]
  pixels = transpose( plane.pixels.copy() )
  pixels[ x, y ] = value
  plane.pixels = transpose( pixels )
  plane.update()
  return

###############################################################################

def get_image_minimum( im ):
  avg = get_image_mean( im )
  if ( avg != None ):
    pixels = get_image_pixels( im )
    mask = ( pixels == get_aips_magic_value() )
    if ( mask.sum() > 0L ):
      putmask( pixels, mask, avg )
    im_min = pixels.min()
    xy_min = where( pixels == im_min )
    x_min = xy_min[ 0 ][ 0 ] + 1
    y_min = xy_min[ 1 ][ 0 ] + 1
    return [ float( im_min ), [ x_min, y_min ] ]
  else:
    return None
    
###############################################################################

def get_image_maximum( im ):
  avg = get_image_mean( im )
  if ( avg != None ):
    pixels = get_image_pixels( im )
    mask = ( pixels == get_aips_magic_value() )
    if ( mask.sum() > 0L ):
      putmask( pixels, mask, avg )
    im_max = pixels.max()
    xy_max = where( pixels == im_max )
    x_max = xy_max[ 0 ][ 0 ] + 1
    y_max = xy_max[ 1 ][ 0 ] + 1
    return [ float( im_max ), [ x_max, y_max ] ]
  else:
    return None

###############################################################################

def get_image_extremum( im, force_positive = False ):
  im_max = get_image_maximum( im )
  if ( ( im_max == None ) or force_positive ):
    extremum = im_max
  else:
    im_min = get_image_minimum( im )
    if ( abs( im_min[ 0 ] ) > abs( im_max[ 0 ] ) ):
      extremum = im_min
    else:
      extremum = im_max
  return extremum

###############################################################################

def extract_model_components( facet, model_version = 0, facet_file_name = '', split = False ):

  # create clean box mask
  facet_size = get_image_size( facet )
  wiz_facet = wizardry( facet )
  if ( facet_file_name == '' ):
    mask = ones( shape = tuple( facet_size ), dtype = bool )
  else:
    # extract facet definition and clean boxes
    i = get_facet_number( facet )
    facet_i_file_name = facet_file_name + '.%03d' % ( i )
    extract_facet_definitions( facet_file_name, [ i ], facet_i_file_name,
        include_bcomp = False, include_weights = False )

    # read clean box definitions from facet file
    # fill mask with ones in clean box areas
    [ X, Y ] = indices( tuple( facet_size ), dtype = int32 )
    mask = zeros( shape = tuple( facet_size ), dtype = bool )
    facet_i_file = file( facet_i_file_name, mode = 'r' )
    for line in facet_i_file:
      words = [ word.strip() for word in line.split() ]
      if ( len( words ) == 0 ):
        continue
      if ( words[ 0 ][ 0 ] == '#' ):
        continue
      try:
        facet_number = int( words[ 0 ] )
      except ValueError:
        pass
      else:
        if ( int( words[ 1 ] ) == - 1 ): # circular area
          rc = int( words[ 2 ] )
          xc = int( words[ 3 ] ) - 1
          yc = int( words[ 4 ] ) - 1
          rc2 = float( rc )**2
          R2 = ( X - float( xc ) )**2 + ( Y - float( yc ) )**2
          putmask( mask, R2 <= rc2, True )
        else: # rectangular area
          x1 = int( words[ 1 ] ) - 1
          y1 = int( words[ 2 ] ) - 1
          x2 = int( words[ 3 ] ) - 1
          y2 = int( words[ 4 ] ) - 1
          putmask( mask, ( X >= x1 ) & ( X <= x2 ) & ( Y >= y1 ) & ( Y <= y2 ), True )
    facet_i_file.close()
    remove_file( facet_i_file_name )

  # extract clean components within clean box(es)
  pix_scale = get_pixel_size( facet, make_absolute = False )
  pix_ref = get_pixel_reference( facet )
  cc_table = wiz_facet.table( 'CC', model_version )
  cc_highest_version = wiz_facet.table_highver( 'CC' )
  cc_src_table = new_table( facet, 'CC', cc_highest_version + 1 )
  if split:
    cc_res_table = new_table( facet, 'CC', cc_highest_version + 2 )
  for cc in cc_table:
    dx = 3600. * cc.deltax / pix_scale[ 0 ]
    dy = 3600. * cc.deltay / pix_scale[ 1 ]
    x = int( around( pix_ref[ 0 ] + dx ) ) - 1
    y = int( around( pix_ref[ 1 ] + dy ) ) - 1
    if ( mask[ x, y ] == True ):
      cc_src_table.append( cc )
    elif split:
      cc_res_table.append( cc )
  if split:
    cc_versions = [ cc_src_table.version, cc_res_table.version ]
  else:
    cc_versions = cc_src_table.version
  cc_table.close()
  cc_src_table.close()
  if split:
    cc_res_table.close()

  return cc_versions

###############################################################################

def combine_model_tables( im, version_list = [], new_version = 0, keep_model_tables = False ):

  # make list of tables to merge
  if ( version_list == [] ):
    cc_list = []
    for cc_version in range( 1, im.table_highver( 'CC' ) + 1 ):
      if table_exists( im, 'CC', cc_version ):
        cc_list.append( cc_version )
  else:
    cc_list = version_list

  # determine new cc table version
  if ( new_version == 0 ):
    if keep_model_tables:
      cc_version = im.table_highver( 'CC' ) + 1
    else:
      cc_version = min( cc_list )
  else:
    cc_version = new_version

  # if needed, move existing table to free new cc table version number
  try:
    cc_index = cc_list.index( cc_version )
  except ValueError:
    pass
  else:
    out_version = im.table_highver( 'CC' ) + 1
    call_aips_task( 'TACOP', indata = im, inext = 'CC', invers = cc_version, ncount = 1, outdata = im,
        outvers = out_version )
    cc_list[ cc_index ] = out_version
    im.zap_table( 'CC', cc_version )

  # copy contents of selected tables to new table and merge
  wizim = wizardry( im )
  out_version = im.table_highver( 'CC' ) + 1
  new_cc_table = new_table( im, 'CC', out_version )
  for version in cc_list:
    cc_table = wizim.table( 'CC', version )
    for cc in cc_table:
      new_cc_table.append( cc )
    cc_table.close()
    if ( not keep_model_tables ):
      im.zap_table( 'CC', version )
  new_cc_table.close()
  call_aips_task( 'CCMRG', indata = im, invers = out_version, outvers = cc_version )
  im.zap_table( 'CC', out_version )

  return out_version

###############################################################################

def get_time_list( uv ):
  time_list = []
  wiz_uv = wizardry( uv )
  for group in wiz_uv:
    if ( len( time_list ) == 0 ):
      time_list.append( group.time )
    elif ( group.time != time_list[ - 1 ] ):
      time_list.append( group.time )
  return time_list

###############################################################################

def get_reference_antenna( uvim, sn_version ):
  wiz_uvim = wizardry( uvim )
  sn_table = wiz_uvim.table( 'SN', sn_version )
  for row in sn_table:
    reference_antenna = row.refant_1
    break
  sn_table.close()
  return reference_antenna

###############################################################################

def convert_beam_size( im, beam = None, to_pixel = False ):
# TODO: add rotation for non-central beams
# TODO: check for mirroring when pixel_size < 0
  if ( beam == None ):
    [ bmaj, bmin, bpa ] = get_beam_size( im ) # beam in arcsec
    if not to_pixel:
      return [ bmaj, bmin, bpa ]
  else:
    [ bmaj, bmin, bpa ] = beam
    if beam == [ 0., 0., 0. ]:
      return beam
  [ scale_x, scale_y ] = get_pixel_size( im, make_absolute = True )
  if to_pixel: # convert from arcsec to pixels, else from pixels to arcsec
    scale_x = 1. / scale_x
    scale_y = 1. / scale_y
  new_bmaj_x = scale_x * sin( radians( bpa ) ) * bmaj
  new_bmaj_y = scale_y * cos( radians( bpa ) ) * bmaj
  new_bmaj = sqrt( new_bmaj_x**2 + new_bmaj_y**2 )
  new_bmin_x = scale_x * cos( radians( bpa ) ) * bmin
  new_bmin_y = scale_y * sin( radians( bpa ) ) * bmin
  new_bmin = sqrt( new_bmin_x**2 + new_bmin_y**2 )
  new_bpa = degrees( atan2( new_bmaj_x, new_bmaj_y ) ) % 180.
  return [ new_bmaj, new_bmin, new_bpa ]

###############################################################################

def put_sub_image( im, subim, blc ):

#  if ( im == subim ):
#    raise error( 'subimage and target image are the same' )
  im_size = get_image_size( im )
  wiz_im = wizardry( im )
  plane = wiz_im[ 0 ]
  im_pixels = transpose( plane.pixels.copy() )
  subim_size = get_image_size( subim )
  wiz_subim = wizardry( subim )
  subim_pixels = transpose( wiz_subim.pixels.copy() )

  im_x_min = max( [ 1, blc[ 0 ] ] ) - 1
  im_x_max = min( [ im_size[ 0 ], blc[ 0 ] + subim_size[ 0 ] - 1 ] ) - 1
  im_y_min = max( [ 1, blc[ 1 ] ] ) - 1
  im_y_max = min( [ im_size[ 1 ], blc[ 1 ] + subim_size[ 1 ] - 1 ] ) - 1

  subim_x_min = max( [ 1, - blc[ 0 ] + 2 ] ) - 1
  subim_x_max = min( [ subim_size[ 0 ], im_size[ 0 ] - blc[ 0 ] + 1 ] ) - 1
  subim_y_min = max( [ 1, - blc[ 1 ] + 2 ] ) - 1
  subim_y_max = min( [ subim_size[ 1 ], im_size[ 1 ] - blc[ 1 ] + 1 ] ) - 1

  if ( ( im_x_min > im_x_max ) or ( im_y_min > im_y_max ) or 
      ( subim_x_min > subim_x_max ) or ( subim_y_min > subim_y_max ) ) :
    raise error( 'subimage and target image do not overlap' )

  im_overlap = im_pixels[ im_x_min : im_x_max + 1, im_y_min : im_y_max + 1 ]
  subim_overlap = subim_pixels[ subim_x_min : subim_x_max + 1, subim_y_min : subim_y_max + 1 ]
  mask = ( subim_overlap == get_aips_magic_value() )
  im_pixels[ im_x_min : im_x_max + 1, im_y_min : im_y_max + 1 ] = (
      im_overlap * array( mask == True, dtype = im_pixels.dtype ) + 
      subim_overlap * array( mask == False, dtype = im_pixels.dtype ) )
  plane.pixels = transpose( im_pixels )
  plane.update()

  return

###############################################################################

def fill_source( facet, pos, beam = None, invert = False, value = None ):

  # copy facet pixel contents
  facet_size = get_image_size( facet )
  wiz_facet = wizardry( facet )
  plane = wiz_facet[ 0 ]
  pixels = transpose( plane.pixels.copy() )

  # get source position and shape in pixels
  [ xc, yc ] = pos
  [ bmaj, bmin, bpa ] = convert_beam_size( facet, beam = beam, to_pixel = True )

  [ X, Y ] = indices( tuple( facet_size ), dtype = int32 )
  mask = zeros( shape = pixels.shape, dtype = bool )
  NR2 = ( ( ( ( X - float( xc ) ) * cos( radians( bpa ) ) + 
              ( Y - float( yc ) ) * sin( radians( bpa ) ) ) / ( bmin / 2. ) )**2 + 
          ( ( ( Y - float( yc ) ) * cos( radians( bpa ) ) -
              ( X - float( xc ) ) * sin( radians( bpa ) ) ) / ( bmaj / 2. ) )**2 )
  putmask( mask, NR2 <= 1., True )

  # invert when requested
  if invert:
    mask = ( mask == False )

  # write blanks at masked values
  if ( value == None ):
    putmask( pixels, mask, get_aips_magic_value() )
  else:
    putmask( pixels, mask, value )
  plane.pixels = transpose( pixels )
  plane.update()

  return

###############################################################################

def replace_pixels( im, value, new_value ):

  # copy facet pixel contents
  facet_size = get_image_size( im )
  wiz_im = wizardry( im )
  plane = wiz_im[ 0 ]
  pixels = transpose( plane.pixels.copy() )

  # replace values and store
  mask = ( pixels == value )
  putmask( pixels, mask, new_value )
  plane.pixels = transpose( pixels )
  plane.update()

  return

###############################################################################

def get_facet( facets, i ):
  if ( i < 1000 ):
    facet_i = get_aips_file( facets.disk, facets.name, facets.klass[ 0 : 3 ] + '%03d' % ( i ),
        facets.seq, 'MA' )
  else:
    facet_i = get_aips_file( facets.disk, facets.name, facets.klass[ 0 : 2 ] + '%04d' % ( i ),
        facets.seq, 'MA' )
  return facet_i

###############################################################################

def get_facet_number( facet ):
  try:
    number = int( facet.klass[ 2 : ] )
  except ValueError:
    number = int( facet.klass[ 3 : ] )
  else:
    pass
  return number

###############################################################################

def get_facet_beam( facet_i ):
  number = get_facet_number( facet_i )
  if ( number < 1000 ):
    beam_i = get_aips_file( facet_i.disk, facet_i.name,
        facet_i.klass[ 0 ] + 'BM' + facet_i.klass[ 3 : 6 ], facet_i.seq, 'MA' )
  else:
    beam_i = get_aips_file( facet_i.disk, facet_i.name,
        facet_i.klass[ 0 ] + 'B' + facet_i.klass[ 2 : 6 ], facet_i.seq, 'MA' )
  return beam_i

###############################################################################

def calculate_source_radec( facet, pos ):
  facet_radec = get_radec( facet )
  pixel_ref = get_pixel_reference( facet )
  pixel_size = get_pixel_size( facet, make_absolute = False )
  dx = ( pos[ 0 ] - pixel_ref[ 0 ] ) * pixel_size[ 0 ] / 3600.
  dy = ( pos[ 1 ] - pixel_ref[ 1 ] ) * pixel_size[ 1 ] / 3600.
  radius = sqrt( dx**2 + dy**2 )
  angle = degrees( atan2( dx, dy ) )
  projection = get_image_projection( facet )
  if ( projection == 'ARC' ):
    pass
  elif ( projection == 'SIN' ):
    radius = degrees( asin( radians( radius ) ) )
  elif ( projection == 'TAN' ):
    radius = degrees( atan( radians( radius ) ) )
  else:
    raise error( 'projection %s not supported (yet)' % projection )
  facet_rot = get_image_rotation( facet )
  if ( facet_rot != 0. ):
    angle = angle - facet_rot
  source_radec = calculate_offset_position( facet_radec, radius, angle )
  return source_radec

###############################################################################

def calculate_source_position( facet, radec, to_grid = False ):
  facet_radec = get_radec( facet )
  pixel_ref = get_pixel_reference( facet )
  pixel_size = get_pixel_size( facet, make_absolute = False )
  [ radius, angle ] = calculate_angular_separation( facet_radec, radec )
  projection = get_image_projection( facet )
  if ( projection == 'ARC' ):
    pass
  elif ( projection == 'SIN' ):
    radius = degrees( sin( radians( radius ) ) )
  elif ( projection == 'TAN' ):
    radius = degrees( tan( radians( radius ) ) )
  else:
    raise error( 'projection %s not supported (yet)' % projection )
  facet_rot = get_image_rotation( facet )
  if ( facet_rot != 0. ):
    angle = angle + facet_rot
  dx = 3600. * radius * sin( radians( angle ) ) / pixel_size[ 0 ]
  dy = 3600. * radius * cos( radians( angle ) ) / pixel_size[ 1 ]
  pos_x = pixel_ref[ 0 ] + dx
  pos_y = pixel_ref[ 1 ] + dy
  if to_grid:
    pos_x = float( around( pos_x ) )
    pos_y = float( around( pos_y ) )
  return [ pos_x, pos_y ]

###############################################################################

def get_image_pixels( im, flip = True ):
  wiz_im = wizardry( im )
  plane = wiz_im[ 0 ]
  pixels = plane.pixels.copy()
  if flip:
    pixels = transpose( pixels )
  return pixels

###############################################################################

def get_epoch( uvim ):
  return uvim.header.epoch

###############################################################################

def set_epoch( uvim, epoch ):
  wiz_uvim = wizardry( uvim )
  wiz_uvim.header.epoch = epoch
  wiz_uvim.header.update()
  wiz_uvim = wizardry( uvim )
  return

###############################################################################

#def calculate_source_shift( uv, radec ):
#  uv_radec = get_radec( uv )
#  [ radius, angle ] = calculate_angular_separation( uv_radec, radec )
#  dx = 3600. * radius * sin( radians( angle ) )
#  dy = 3600. * radius * cos( radians( angle ) )
#  return [ dx, dy ]

###############################################################################

def get_image_mean( im ):
  pixels = get_image_pixels( im )
  mask = ( pixels == get_aips_magic_value() )
  if sometrue( mask ):
    putmask( pixels, mask, 0. )
    count = ( mask == False ).sum()
    if ( count > 0 ):
      mean = float( pixels.sum() / float( count ) )
    else:
      mean = None
  else:
    mean = float( pixels.mean() )
  return mean

###############################################################################

def get_image_rms( im ):
  pixels = get_image_pixels( im )
  mask = ( pixels == get_aips_magic_value() )
  if sometrue( mask ):
    putmask( pixels, mask, 0. )
    count = ( mask == False ).sum()
    if ( count > 0 ):
      rms = float( sqrt( ( pixels**2 ).sum() / float( count ) ) )
    else:
      rms = None
  else:
    rms = float( sqrt( ( pixels**2 ).mean() ) )
  return rms

###############################################################################

def clip_image( im, limits, values = None, epsilon = 1.e-7 ):
  wiz_im = wizardry( im )
  plane = wiz_im[ 0 ]
  pixels = transpose( plane.pixels.copy() )
  sel_min = awhere( pixels < limits[ 0 ] )
  sel_max = awhere( pixels > limits[ 1 ] )
  if ( values == None ):
    pixels = aput( pixels, sel_min, ( 1. - epsilon ) * limits[ 0 ] )
    pixels = aput( pixels, sel_max, ( 1. - epsilon ) * limits[ 1 ] )
  else:
    pixels = aput( pixels, sel_min, values[ 0 ] )
    pixels = aput( pixels, sel_max, values[ 1 ] )
  plane.pixels = transpose( pixels )
  plane.update()
  return

###############################################################################

def get_image_stddev( im ):
  avg = get_image_mean( im )
  if ( avg != None ):
    pixels = get_image_pixels( im )
    mask = ( pixels == get_aips_magic_value() )
    if sometrue( mask ):
      putmask( pixels, mask, avg )
      count = ( mask == False ).sum()
      if ( count > 0 ):
        stddev = float( sqrt( ( ( pixels - avg )**2 ).sum() / float( count ) ) )
      else:
        stddev = None
    else:
      stddev = float( sqrt( ( ( pixels - pixels.mean() )**2 ).mean() ) )
  else:
    stddev = None
  return stddev

###############################################################################

def get_observing_epoch( uv ):
  [ year, month, day ] = [ int( a ) for a in uv.header.date_obs.split( '-' ) ]
  days = ( date( year, month, day ) - date( year, 1, 1 ) ).days
  epoch = year + ( float( days ) / 365.25 )
  return epoch

###############################################################################

def transfer_model_components( in_facet, in_version, out_facet, out_version ):
# Note: this function will put clean components at fractional pixel positions.
# This requires the use of cmethod = 'DFT' for AIPS tasks like CALIB or UVSUB

  in_pix_ref = get_pixel_reference( in_facet )
  in_pix_scale = get_pixel_size( in_facet, make_absolute = False )
  out_pix_ref = get_pixel_reference( out_facet )
  out_pix_scale = get_pixel_size( out_facet, make_absolute = False )
  out_facet_size = get_image_size( out_facet )
  wiz_in_facet = wizardry( in_facet )
  wiz_out_facet = wizardry( out_facet )

  # transfer CC components to output CC table
  in_cc_table = wiz_in_facet.table( 'CC', in_version )
  # create new empty CC table if necessary
  if table_exists( out_facet, 'CC', out_version ):
    out_cc_table = wiz_out_facet.table( 'CC', out_version )
  else:
    out_cc_table = new_table( out_facet, 'CC', out_version )
  for cc in in_cc_table:
    x = in_pix_ref[ 0 ] + 3600. * cc.deltax / in_pix_scale[ 0 ]
    y = in_pix_ref[ 1 ] + 3600. * cc.deltay / in_pix_scale[ 1 ]
    radec = calculate_source_radec( in_facet, [ x, y ] )
    [ x, y ] = calculate_source_position( out_facet, radec )
    if ( ( x >= 0.5 ) and ( x <= out_facet_size[ 0 ] + 0.5 ) and 
         ( y >= 0.5 ) and ( y <= out_facet_size[ 1 ] + 0.5 ) ):
      cc.deltax = ( x - out_pix_ref[ 0 ] ) * out_pix_scale[ 0 ] / 3600.
      cc.deltay = ( y - out_pix_ref[ 1 ] ) * out_pix_scale[ 1 ] / 3600.
      out_cc_table.append( cc )
  in_cc_table.close()
  out_cc_table.close()

  return 

###############################################################################

def new_table( uvim, table, version = 0, **keywords ):
  if ( version == 0 ):
    new_version = uvim.table_highver( table ) + 1
  else:
    new_version = version
  wiz_uvim = wizardry( uvim )
  ntable = wiz_uvim.add_table( table, new_version, **keywords )
  return ntable

###############################################################################

def new_table_row( table, **keywords ):
  row = Wizardry.AIPSData.AIPSTableRow( table, **keywords )
  return row

###############################################################################

def copy_keywords( in_uvim, out_uvim ):
  wiz_out_uvim = wizardry( out_uvim )
  for keyword in in_uvim.keywords:
    temp = in_uvim.keywords[ keyword ]
    if ( type( temp ) == type( [] ) ):
      temp = temp[ 0 ]
    wiz_out_uvim.keywords[ keyword ] = temp
    wiz_out_uvim.keywords.update()
  return

###############################################################################

def aips_file_name_to_string( uvim ):
  if isinstance( uvim, AIPSUVData ):
    string = '%s.%s.%s.%s.UV' % ( repr( uvim.disk ), uvim.name, uvim.klass, repr( uvim.seq ) )
  else:
    string = '%s.%s.%s.%s.MA' % ( repr( uvim.disk ), uvim.name, uvim.klass, repr( uvim.seq ) )
  return string

###############################################################################

def get_time_list_from_fit_table( uv, fit_version = 0 ):
  wiz_uv = wizardry( uv )
  ni_table = wiz_uv.table( 'NI', fit_version )
  time_list = []
  for row in ni_table:
    time_list.append( row.time )
  time_count = len( time_list )
  return time_list

###############################################################################

def get_time_list_from_solution_table( uv, solution_version = 0 ):
  wiz_uv = wizardry( uv )
  sn_table = wiz_uv.table( 'SN', solution_version )
  time_list = []
  old_time = - 1000000.
  for row in sn_table:
    if ( row.time > old_time ):
      time_list.append( row.time )
      old_time = row.time
  time_count = len( time_list )
  return time_list

###############################################################################

def find_aips_cno( uvim ):
  cno = -1
  aips_disk = uvim.disk
  aips_name = uvim.name
  aips_class = uvim.klass
  aips_seq = uvim.seq
  if isinstance( uvim, AIPSUVData ):
    aips_type = 'UV'
  else:
    aips_type = 'MA'
  cat_disk = AIPSCat( aips_disk )[ aips_disk ]
  for cat in cat_disk:
    if ( [ cat.name, cat.klass, cat.seq, cat.type ] == [ aips_name, aips_class,
        aips_seq, aips_type ] ):
      cno = cat.cno
      break
  return cno

###############################################################################

def get_aips_cno( disk, cno ):
  uvim = None
  cat_disk = AIPSCat( disk )[ disk ]
  for cat in cat_disk:
    if ( cat.cno == cno ):
      uvim = get_aips_file( disk, cat.name, cat.klass, cat.seq, cat.type )
      break
  return uvim

###############################################################################

def read_fits_uv( file_name, uv, compress = False ):
  if ( not file_exists( file_name ) ):
    raise error( 'UV FITS file %s not found' % ( file_name ) )
  if compress:
    douvcomp = 1
  else:
    douvcomp = 0
  call_aips_task( 'FITLD', datain = file_name, optype = 'UV', outdata = uv,
      douvcomp = douvcomp )
  return

###############################################################################

def write_fits_uv( uv, file_name, overwrite = True ):
  if ( ( not overwrite ) and ( file_exists( file_name ) ) ):
    raise error( 'UV FITS file %s already exists' % ( file_name ) )
  elif ( overwrite and ( file_exists( file_name ) ) ):
    remove_file( file_name )
  call_aips_task( 'FITTP', indata = uv, intype = 'UV', dataout = file_name )
  return

###############################################################################

def read_fits_image( file_name, im ):
  if ( not file_exists( file_name ) ):
    raise error( 'image FITS file %s not found' % ( file_name ) )
  call_aips_task( 'FITLD', datain = file_name, optype = 'IM', outdata = im )
  return

###############################################################################

def write_fits_image( im, file_name, overwrite = True, compress = False, include_tables = False,
    crop = False, output_pixel_size = None, output_image_size = None ):

  if ( ( not overwrite ) and ( file_exists( file_name ) ) ):
    raise error( 'image FITS file %s already exists' % ( file_name ) )
  elif ( overwrite and ( file_exists( file_name ) ) ):
    remove_file( file_name )
  
  # remove tables
  temp_im = get_aips_file( im.disk, im.name, 'TEMP', - 1, 'MA' )
  call_aips_task( 'MOVE', indata = im, outdata = temp_im, userid = get_aips_userid() )
  for table in temp_im.tables:
    table_version = table[ 0 ]
    table_type = table[ 1 ][ -2 : ]
    temp_im.zap_table( table_type, table_version )
  
  # scale image
  if ( ( output_pixel_size != None ) or ( output_image_size != None ) ):
    scale_im = scale_image( temp_im, output_pixel_size, output_image_size )
    temp_im.zap()
  else:
    scale_im = temp_im
  
  # crop image
  if ( crop or ( ( output_pixel_size != None ) and ( output_image_size == None ) ) ):
    crop_im = crop_image( scale_im )
    scale_im.zap()
  else:
    crop_im = scale_im
  
  # for image compression, replace blanks with zeros
  if compress:
    format = 1
    replace_pixels( crop_im, get_aips_magic_value(), 0. )
  else:
    format = 0
  
  # copy back tables
  if include_tables:
    tables_done = []
    for table in im.tables:
      table_version = table[ 0 ]
      table_type = table[ 1 ][ -2 : ]
      if ( not table_type in tables_done ):
        call_aips_task( 'TACOP', indata = im, inext = table_type, invers = table_version, ncount = 0,
            outdata = crop_im, outvers = table_version )
        tables_done.append( table_type )
  
  # write image to FITS file
  call_aips_task( 'FITTP', indata = crop_im, intype = 'MA', dataout = file_name,
      format = format )
  crop_im.zap()
  
  return

###############################################################################

def crop_image( im, blank_value = get_aips_magic_value() ):

  pixels = get_image_pixels( im )
  x_size = pixels.shape[ 0 ] 
  y_size = pixels.shape[ 1 ] 
  x_min = 0
  x_max = x_size - 1
  y_min = 0
  y_max = y_size - 1

  # search for lower x boundary
  for x in range( x_size ):
    if sometrue( pixels[ x, : ] != get_aips_magic_value() ):
      x_min = x
      break

  # try a quick scan on upper x boundary, otherwise a thorough search
  if ( ( x_min > 0 ) and ( x_size - x_min > x_min ) ):
    if ( alltrue( pixels[ x_size - x_min, : ] == blank_value ) and
        sometrue( pixels[ x_size - x_min - 1, : ] != blank_value ) ):
      x_max = x_size - x_min - 1
  if ( x_max == x_size - 1 ):
    for x in range( x_size - 1, x_min - 1, - 1 ):
      if sometrue( pixels[ x, : ] != blank_value ):
        x_max = x
        break

  # try a quick scan on lower y boundary, otherwise a thorough search
  if ( ( x_min == 0 ) and sometrue( pixels[ : , 0 ] != blank_value ) ):
    y_min = 0
  else:
    if ( x_min <= y_max ):
      if ( alltrue( pixels[ : , x_min - 1 ] == blank_value ) and
          sometrue( pixels[ : , x_min ] != blank_value ) ):
        y_min = x_min      
    if ( y_min == 0 ):
      for y in range( y_size ):
        if sometrue( pixels[ : ,y ] != blank_value ):
          y_min = y
          break

  # try a quick scan on upper y boundary, otherwise a thorough search
  if ( ( y_min > 0 ) and ( y_size - y_min > y_min ) ):
    if ( alltrue( pixels[ : , y_size - y_min ] == blank_value ) and
        sometrue( pixels[ : , y_size - y_min - 1 ] != blank_value ) ):
      y_max = y_size - y_min - 1
  if ( y_max == y_size - 1 ):
    for y in range( y_size - 1, y_min - 1, - 1 ):
      if sometrue( pixels[ : , y ] != blank_value ):
        y_max = y
        break

  crop_im = get_aips_file( im.disk, im.name, 'CROP', - 1, 'MA' )
  if ( ( x_min != 0 ) or ( x_max != x_size - 1 ) or ( y_min != 0 ) or ( y_max != y_size - 1 ) ):
    call_aips_task( 'SUBIM', indata = im, outdata = crop_im, blc = [ x_min + 1, y_min + 1 ],
        trc = [ x_max + 1, y_max + 1 ] ) 
  else:
    call_aips_task( 'MOVE', indata = im, outdata = crop_im, userid = get_aips_userid() )

  return crop_im

###############################################################################

def scale_image( im, new_pixel_size, new_image_size ):
  
  scale_im = get_aips_file( im.disk, im.name, 'SCALE', - 1, 'MA' )
  if ( ( new_pixel_size == None ) and ( new_image_size == None ) ):
#    raise error( 'both output pixel size and output image size are not specified' )
    call_aips_task( 'MOVE', indata = im, outdata = scale_im, userid = get_aips_userid() )
  
  # rescale image
  image_size = get_image_size( im )
  pixel_size = get_pixel_size( im )
  if ( new_pixel_size != None ):
    scale = pixel_size[ 0 ] / new_pixel_size[ 0 ]
    dscale = ( pixel_size[ 1 ] / new_pixel_size[ 1 ] ) / scale
  else:
    scale = 0.
    dscale = 0.
  if ( new_image_size != None ):
    imsize = new_image_size
    if ( new_pixel_size == None ):
      scale = float( image_size[ 0 ] ) / float( new_image_size[ 0 ] )
      dscale = ( float( image_size[ 1 ] ) / float( new_image_size[ 1 ] ) ) / scale
  else:
    if ( new_pixel_size != None ):
      imsize = [ int( ceil( float( image_size[ 0 ] ) * scale ) ) + 5,
          int( ceil( float( image_size[ 1 ] ) * scale * dscale ) ) + 5 ]
  call_aips_task( 'OGEOM', indata = im, outdata = scale_im, imsize = imsize,
      aparm = [ 0, 0, 0, scale, dscale, 0, 0 ] )
  
  return scale_im

###############################################################################

def scale_model_flux( facet, flux_scale, in_version = 0, out_version = 0 ):

  if ( not table_exists( facet, 'CC', in_version ) ):
    raise error( 'input model component table does not exist' )
  if ( in_version == 0 ):
    old_version = facet.table_highver( 'CC' )
  else:
    old_version = in_version
  if ( out_version == 0 ):
    new_version = facet.table_highver( 'CC' ) + 1
  else:
    if table_exists( facet, 'CC', out_version ):
      raise error( 'output model component table already exist' )
    new_version = out_version

  wiz_facet = wizardry( facet )
  old_cc_table = wiz_facet.table( 'CC', old_version )
  new_cc_table = new_table( facet, 'CC', new_version )
  for cc in old_cc_table:
    cc.flux = flux_scale * cc.flux
    new_cc_table.append( cc )
  old_cc_table.close()
  new_cc_table.close()

  return

###############################################################################

def get_image_projection( im ):
  for ctype in im.header.ctype:
    if ( ctype.find( 'RA' ) != - 1 ):
      ra_index = im.header.ctype.index( ctype )
    if ( ctype.find( 'DEC' ) != - 1 ):
      dec_index = im.header.ctype.index( ctype )
  ra_proj = im.header.ctype[ ra_index ][ - 3 : ]
  dec_proj = im.header.ctype[ dec_index ][ - 3 : ]
  if ( ra_proj != dec_proj ):
    raise error( 'RA and DEC have different projections' )
  return ra_proj

###############################################################################

def get_clean_boxes( facet_file_name, facet_list ):
# clean_box_radius in pixels
  clean_box_list = []
  if ( not file_exists( facet_file_name ) ):
    raise error( 'facet file %s does not exist' % facet_file_name )
  facet_file = file( facet_file_name, mode = 'r' )
  for line in facet_file:
    words = [ word.strip() for word in line.split() ]
    if ( len( words ) == 0 ):
      continue
    if ( words[ 0 ][ 0 ] == '#' ):
      continue
    if ( len( words ) == 5 ):
      try:
        line_id = int( words[ 0 ] )
      except:
        pass
      else:
        if ( line_id in facet_list ):
          clean_box_list.append( [ int( word ) for word in words ] )
  facet_file.close()
  return clean_box_list

###############################################################################

def remove_facets( facets, include_beams = True, max_facet_count = 9999 ):
  if ( facets.exists() ):
    try:
      facet_count = restore_parameter( facets, 'facet_count' )
    except:
      facet_count = max_facet_count
  else:
    facet_count = max_facet_count
  for i in range( 1, 1 + facet_count ):
    facet = get_facet( facets, i )
    if ( facet.exists() ):
      facet.clrstat()
      facet.zap()
    if include_beams:
      beam = get_facet_beam( facet )
      if ( beam.exists() ):
        beam.clrstat()
        beam.zap()
  return

###############################################################################

def clear_aips_disk( aips_disk ):
  aips_cat = AIPSCat( aips_disk )[ aips_disk ]
  for aips_file in aips_cat:
    uvim = get_aips_file( aips_disk, aips_file.name, aips_file.klass, aips_file.seq, aips_file.type )
    uvim.clrstat()
    uvim.zap()
  return

###############################################################################

def copy_facets( facets, new_facets, include_beams = True, max_facet_count = 9999 ):
  if ( facets.exists() ):
    try:
      facet_count = restore_parameter( facets, 'facet_count' )
    except:
      facet_count = max_facet_count
  else:
    facet_count = max_facet_count
  for i in range( 1, 1 + facet_count ):
    facet = get_facet( facets, i )
    new_facet = get_facet( new_facets, i )
    if ( facet.exists() ):
      call_aips_task( 'MOVE', indata = facet, outdata = new_facet, userid = get_aips_userid() )
    if include_beams:
      beam = get_facet_beam( facet )
      new_beam = get_facet_beam( new_facet )
      if ( beam.exists() ):
        call_aips_task( 'MOVE', indata = beam, outdata = new_beam, userid = get_aips_userid() )
  return

###############################################################################

def calculate_pbparm_attenuation( freq, radius, pbparms ):
  [ cutoff, apply_pbparms, pbparm3, pbparm4, pbparm5, pbparm6, pbparm7 ] = pbparms
  if ( apply_pbparms > 0. ):
    X = ( ( freq / 1.e9 ) * ( radius * 60. ) )**2
    A = ( 1.0 + ( X * pbparm3 / 1.e3 ) + ( X**2 * pbparm4 / 1.e7 ) + ( X**3 * pbparm5 / 1.e10 ) + 
        ( X**4 * pbparm6 / 1.e13 ) + ( X**5 * pbparm7 / 1.e16 ) )
    if ( A < cutoff ):
      A = 0.
    else:
      dXdR = 2 * ( ( freq / 1.e9 ) * ( radius * 60. ) ) * ( ( freq / 1.e9 ) * ( 60. ) )
      dAdX = ( ( pbparm3 / 1.e3 ) + ( 2 * X * pbparm4 / 1.e7 ) + ( 3 * X**2 * pbparm5 / 1.e10 ) + 
          ( 4 * X**3 * pbparm6 / 1.e13 ) + ( 5 * X**4 * pbparm7 / 1.e16 ) )
      if ( dAdX * dXdR > 0. ):
        A = 0.
  else:
    A = 1.
  return A

###############################################################################

def fix_antenna_table( uv, version = 0 ):
# This is to fix a bug in AIPS++ ms2uvfix
  
  if ( version == 0 ):
    an_version = uv.table_highver( 'AN' )
  else:
    an_version = version
  
  # make working copy of antenna table
  call_aips_task( 'TACOP', indata = uv, inext = 'AN', invers = an_version, ncount = 1,
      outdata = uv, outvers = 0 )
  wiz_uv = wizardry( uv )
  an_table = wiz_uv.table( 'AN', 0 )
  
  # get array reference position
  array_xyz = array( [ an_table.keywords[ 'ARRAYX' ], an_table.keywords[ 'ARRAYY' ],
      an_table.keywords[ 'ARRAYZ' ] ], dtype = float64 )
  if alltrue( array_xyz == azeros( array_xyz ) ):
    # nothing to be done, so exit
    uv.zap_table( 'AN', 0 )
    return
  array_geo_llh = array( xyz_to_geo_llh( array_xyz.tolist() ), dtype = float64 )
  [ lon, lat ] = aradians( array_geo_llh[ 0 : 2 ] )
  rotation = array( [ [ cos( lon ), - sin( lon ), 0. ],
                      [ sin( lon ),   cos( lon ), 0. ],
                      [         0.,           0., 1. ] ], dtype = float64 )
  
  # rotate delta xyz of individual antennas
  for row in an_table:
    dxyz = dot( array( row.stabxyz, dtype = float64 ), rotation )
    row.stabxyz = dxyz.tolist()
    row.update()
  an_table.close()
  
  # replace original antenna table with working copy
  uv.zap_table( 'AN', an_version )
  call_aips_task( 'TACOP', indata = uv, inext = 'AN', invers = 0, ncount = 1,
      outdata = uv, outvers = an_version )
  uv.zap_table( 'AN', 0 )
  
  return

###############################################################################

def time_to_string( time ):
  dhms = time_to_dhms( time )
  string = '%03dd%02dh%02dm%05.2fs' % ( dhms[ 0 ], dhms[ 1 ], dhms[ 2 ], dhms[ 3 ] )
  return string

###############################################################################

def radec_to_string( radec ):
  hmsdms = degdeg_to_hmsdms( radec )
  string = '%02dh%02dm%05.2f' % ( hmsdms[ 0 ], hmsdms[ 1 ], hmsdms[ 2 ] )
  if ( hmsdms[ 3 ] < 0. ):
    string = string + ' -'
    hmsdms[ 3 ] = -hmsdms[ 3 ]
  else:
    string = string + ' +'
  string = string + "%02dd%02d'"  % ( hmsdms[ 3 ], hmsdms[ 4 ] )
  string = string + '%04.1f"' % ( hmsdms[ 5 ] )
  return string

###############################################################################

def get_evla_antennas( uv ):
  ant_names = uv.antennas
  evla_list = []
  for i in range( len( ant_names ) ):
    if ( 'EVLA' in ant_names[ i ] ):
      evla_list.append( i + 1 )
  return evla_list

###############################################################################

def get_vla_antennas( uv ):
  ant_names = uv.antennas
  vla_list = []
  evla_list = get_evla_antennas( uv )
  for i in range( len( ant_names ) ):
    if ( ( not i + 1 in evla_list ) and ( 'VLA' in ant_names[ i ] ) ):
      vla_list.append( i + 1 )
  return vla_list

###############################################################################

def is_uv( uvim ):
  return isinstance( uvim, AIPSUVData )

###############################################################################

def is_image( uvim ):
  return isinstance( uvim, AIPSUVData )

###############################################################################

def set_header_keyword( uvim, key, value, index = None ):
  wiz_uvim = wizardry( uvim )
  if ( index != None ):
    wiz_uvim.header[ key ][ index ] = value
  else:
    wiz_uvim.header[ key ] = value
  wiz_uvim.header.update()
  return

###############################################################################

def ehex_to_decimal( ehex ):
  conversion = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  decimal = 0
  multiplier = 1
  for char in ehex.upper()[ : : -1 ]:
    decimal = decimal + conversion.index( char ) * multiplier
    multiplier = multiplier * 36
  return decimal

###############################################################################

def decimal_to_ehex( decimal ):
  conversion = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  ehex = ''
  dec = decimal
  while ( dec > 0 ):
    index = dec % 36
    ehex = conversion[ index ] + ehex
    dec = ( dec - index ) / 36
  return ehex

###############################################################################

def zero_pad_ehex( ehex, length ):
  new_ehex = ehex
  for i in range( length - len( ehex) ):
    new_ehex = '0' + new_ehex
  return new_ehex

###############################################################################

def get_aips_file_name( uvim, table = None, version = None ):
  file_path = os.getenv( 'DA' + zero_pad_ehex( decimal_to_ehex( uvim.disk ), 2 ) )
  if ( ( table != None ) and ( version != None ) ):
    tab = table[ -2 : ]
    if ( version > 0 ):
      vers = version
    elif ( version == 0 ):
      vers = uvim.table_highver( tab )
    else: # version < 0
      vers = uvim.table_highver( tab ) + 1
  else:
    vers = 1
    if is_uv( uvim ):
      tab = 'UV'
    else: # is_image( uvim )
      tab = 'MA'
  cno = zero_pad_ehex( decimal_to_ehex( find_aips_cno( uvim ) ), 3 )
  vers = zero_pad_ehex( decimal_to_ehex( vers ), 3 )
  user = zero_pad_ehex( decimal_to_ehex( get_aips_userid() ), 3 )
  file_name = file_path + '/' + tab + 'D' + cno + vers + '.' + user + ';'
  return file_name

###############################################################################

def check_aips_disk( disk, user_id = None, delete = False, print_info = False ):
  error_count = 0
  if ( user_id == None ):
    userid = get_aips_userid()
  else:
    userid = user_id
  if print_info:
    print 'checking AIPS disk %s for user %s' % ( repr( disk ), repr( userid ) )
  file_path = os.getenv( 'DA' + zero_pad_ehex( decimal_to_ehex( disk ), 2 ) )
  file_list = os.listdir( file_path )
  for name in file_list:
    if ( name == 'SPACE' ):
      continue
    if ( ( name[ 2 ] == 'D' ) and ( name[ 9 ] == '.' ) and ( name[ 13 ] == ';' ) ):
      user = ehex_to_decimal( name[ 10 : 13 ] )
      if ( user != userid ):
        continue
      if ( name[ 0 : 9 ] == 'CAD000000' ):
        continue
      tab = name[ 0 : 2 ]
      cno = ehex_to_decimal( name[ 3 : 6 ] )
      vers = ehex_to_decimal( name[ 6 : 9 ] )
      if ( tab in [ 'MA', 'UV', 'CB' ] ):
        if ( vers == 1 ):
          uvim = get_aips_cno( disk, cno )
          if ( not uvim.exists() ):
            error_count = error_count + 1
            if print_info:
              if ( tab == 'CB' ):
                print '... found orphane CB file %s' % ( name )
              else:
                print '... found uncataloged UV/MA file %s' % ( name )
            if delete:
              remove_file( file_path + '/' + name )
        else:
          error_count = error_count + 1
          if print_info:
            print '... found unknown file type %s' % ( name )
          if delete:
            remove_file( file_path + '/' + name )
      elif ( tab in [ 'AN', 'AT', 'BL', 'BP', 'CC', 'CD', 'CL', 'CQ', 'CT',
          'FG', 'FQ', 'GC', 'HI', 'IM', 'MC', 'MF', 'NI', 'NX', 'OB', 'OF',
          'OT', 'PC', 'PS', 'SN', 'SU', 'SY', 'TY', 'VL', 'VZ', 'WX',
          'CG', 'CB' ] ):
        uvim = get_aips_cno( disk, cno )
        if ( uvim == None ):
          error_count = error_count + 1
          if print_info:
            print '... found orphane table %s' % ( name )
          if delete:
            remove_file( file_path + '/' + name )
        else:
          if ( not table_exists( uvim, tab, vers ) ):
            error_count = error_count + 1
            if print_info:
              print '... found uncataloged table %s' % ( name )
            if delete:
              remove_file( file_path + '/' + name )
      else:
        error_count = error_count + 1
        if print_info:
          print '... found unknown file type %s' % ( name )
        if delete:
          remove_file( file_path + '/' + name )
  return error_count

###############################################################################

def time_average_uv_data( uv, factor ):
  
  # prepare some stuff
  time_array = array( get_time_list( uv ), dtype = float64 )
  dtime_array = time_array[ 1 : ] - time_array[ : -1 ]
  dtime_median = median( dtime_array )
  solint = ( round( factor ) ) * dtime_median * 1440.
  
  # time average data using SPLAT
  temp_uv = get_aips_file( uv.disk, uv.name, 'TEMP', -1, 'UV' )
  call_aips_task( 'SPLAT', indata = uv, outdata = temp_uv, douvcomp = 0,
      solint = solint )
  
  # get new time list and weights
  time_list = []
  weight_list = []
  wiz_uv = wizardry( temp_uv )
  for group in wiz_uv:
    time = group.time
    try:
      index = time_list.index( time )
      weight_list[ index ] = weight_list[ index ] + 1.
    except:
      time_list.append( time )
      weight_list.append( 1. )
  time_weight_list = []
  for i in range( len( time_list ) ):
    time_weight_list.append( [ time_list[ i ], weight_list[ i ] ] )
  time_weight_list.sort( cmp = lambda a, b: cmp( a[ 0 ], b[ 0 ] ) )
  time_list = [ time_weight[ 0 ] for time_weight in time_weight_list ]
  weight_list = [ time_weight[ 1 ] for time_weight in time_weight_list ]
  
  # take out small offsets
  new_time_list = [ time for time in time_list ]
  new_weight_list = [ weight for weight in weight_list ]
  dtime_min = 1.5 * dtime_median
  i = 0
  while ( i + 1 < len( new_time_list ) ):
    dtime = new_time_list[ i + 1 ] - new_time_list[ i ]
    if ( dtime < dtime_min ):
      dtime_list = [ i, i + 1 ]
      i = i + 1
      while ( i + 1 < len( new_time_list ) ):
        dtime = new_time_list[ i + 1 ] - new_time_list[ i ]
        if ( dtime >= dtime_min ):
          break
        dtime_list.append( i + 1 )
        i = i + 1
      sel_weight_list = [ new_weight_list[ i ] for i in dtime_list ]
      sel_weight_max = sel_weight_list.index( max( sel_weight_list ) )
      i_max = dtime_list[ sel_weight_max ]
      dtime_list.remove( i_max )
      for i in dtime_list:
        new_time_list[ i ] = new_time_list[ i_max ]
        new_weight_list[ i ] = new_weight_list[ i_max ]
    i = i + 1
  
  # get new time list
  sel_time_list = []
  for time in new_time_list:
    if ( not time in sel_time_list ):
      sel_time_list.append( time )
  time_array = array( sel_time_list, dtype = float64 )
  
  # adjust time grid
  dtime = ( round( factor ) ) * dtime_median
  epsilon = dtime / 1.e6
  for i in [ 1,2 ]:
    dtime_array = time_array[ i : ] - time_array[ : -i ]
    while ( True ):
      dtime_corr = amodulo( dtime_array + 0.5 * dtime, dtime ) - 0.5 * dtime
      dtime_p = dtime_corr[ i : ]
      dtime_m = dtime_corr[ : -i ]
      time_corr = ( dtime_m**2 * dtime_p - dtime_p**2 * dtime_m ) / (
          dtime_m**2 + dtime_p**2 + epsilon )
#      if alltrue( time_array[ i : -i ] == time_array[ i : -i ] + time_corr ):
      if alltrue( abs( time_corr ) < epsilon ):
        break
      time_array[ i : -i ] = time_array[ i : -i ] + time_corr
      dtime_array = time_array[ i : ] - time_array[ : -i ]
  dtime_array = time_array[ 1 : ] - time_array[ : -1 ]
  dtime_corr = amodulo( dtime_array + 0.5 * dtime, dtime ) - 0.5 * dtime
  time_array[ 0 ] = time_array[ 0 ] + dtime_corr[ 0 ]
  time_array[ -1 ] = time_array[ -1 ] - dtime_corr[ -1 ]
  time_array = time_array.tolist()
  
  # write new times
  for group in wiz_uv:
    index1 = time_list.index( group.time )
    index2 = sel_time_list.index( new_time_list[ index1 ] )
    group.time = time_array[ index2 ]
    group.update()
  
  # sort data in TB
  temp2_uv = get_aips_file( uv.disk, uv.name, 'TEMP2', -1, 'UV' )
  call_aips_task( 'UVSRT', indata = temp_uv, outdata = temp2_uv, sort = 'TB' )
  temp_uv.zap()
  
  # recheck for small offsets
  last_time = -1.e9
  wiz_uv = wizardry( temp2_uv )
  for group in wiz_uv:
    dtime = group.time - last_time
    if ( abs( dtime ) > 0. ):
      if ( abs( dtime ) < dtime_min ):
        group.time = last_time
        group.update()
      else:
        last_time = group.time
  
  # sort data in TB
  tavg_uv = get_aips_file( uv.disk, uv.name, 'TAVG', -1, 'UV' )
  call_aips_task( 'UVSRT', indata = temp2_uv, outdata = tavg_uv, sort = 'TB' )
  temp2_uv.zap()
  
  return tavg_uv

###############################################################################

