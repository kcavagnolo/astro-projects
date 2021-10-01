###############################################################################

# import Python modules
from os import *
from shutil import *

# import user modules
#from error import *
#from aips import *

###############################################################################

def file_exists( file_path ):
  return path.isfile( file_path )

###############################################################################

def directory_exists( dir_path ):
  return path.isdir( dir_path )

###############################################################################

def remove_file( path ):
  status = True
  if file_exists( path ):
    try:
      remove( path )
    except:
      status = False
  return status

###############################################################################

def move_file( src_path, dest_path, overwrite = True ):
  status = True
  if ( ( not file_exists( src_path ) ) or ( ( overwrite == False ) and ( file_exists( dest_path ) ) ) ):
    status = False
  else:
    if ( ( overwrite == True ) and ( file_exists( dest_path ) ) ):
      status = remove_file( dest_path )
  if ( status == True ):
    try:
      move( src_path, dest_path )
    except:
      status = False
  return status

###############################################################################

def copy_file( src_path, dest_path, overwrite = True ):
  status = True
  if ( ( not file_exists( src_path ) ) or ( ( overwrite == False ) and ( file_exists( dest_path ) ) ) ):
    status = False
  else:
    if ( ( overwrite == True ) and ( file_exists( dest_path ) ) ):
      status = remove_file( dest_path )
  if ( status == True ):
    try:
      copy( src_path, dest_path )
    except:
      status = False
  return status

###############################################################################

def make_directory( dir_path ):
  try:
    mkdir( dir_path )
  except OSError:
    raise error( 'creation of directory %s failed' % ( dir_path ) )
  return

###############################################################################

def remove_directory( dir_path ):
  try:
    rmdir( dir_path )
  except OSError:
    raise error( 'removal of directory %s failed' % ( dir_path ) )
  return

###############################################################################
