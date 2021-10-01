#include <stdio.h>
#include "idl_export.h"   /* IDL external definitions */


/* USAGE: dummy = CALL_EXTERNAL( 'indexed_add.so', 'indexed_add_double', $
				 vector, index, increment, num_increments )

   VECTOR (long/float/double) is an initialized array.  
   INDEX (long) and INCREMENT (long/float/double) are
   arrays of length NUM_INCREMENTS (long).  The elements of VECTOR
   indexed by INDEX are incremented by INCREMENT.
   
   NOTE: The CALLER is responsible for making sure that the values in
   the array INDEX do not violate the bounds of the array VECTOR.

   The best way to compile is probably to use the IDL utility make_dll,
   as is done in tara_install.pro.  Suggestions for manual compilation
   are below.
   
   To compile for Solaris 2.6 (32 bit):
	cc -I/usr/local/rsi/idl/external/include -K pic -c indexed_add.c
	cc -G -o indexed_add.so indexed_add.o 
   
   To compile for Solaris 2.8 (64 bit):  
	cc -xtarget=ultra -xarch=v9 -I/usr/local/rsi/idl/external/include -K pic -c indexed_add.c
	cc -xtarget=ultra -xarch=v9 -G -o indexed_add_64.so indexed_add.o 

   To compile on a 32-bit Linux system:
        gcc -I/usr/local/rsi/idl/external/include -c  -fPIC -g -Wall indexed_add.c
        ld -shared -o indexed_add.so indexed_add.o
*/

IDL_LONG indexed_add_long( int argc, void *argv[] )
{
  IDL_LONG   *vector;
  IDL_LONG   *index;
  IDL_LONG   *increment;
  IDL_LONG   *num_increments;
  int     ii;

  vector         = (IDL_LONG   *)argv[0];
  index          = (IDL_LONG   *)argv[1];
  increment      = (IDL_LONG   *)argv[2];
  num_increments = (IDL_LONG   *)argv[3];

  for (ii=0; ii < *num_increments; ii++)
  {
    vector[ index[ii] ] += increment[ii];
  }
  
  return(0);
}

IDL_LONG indexed_add_float( int argc, void *argv[] )
{
  float  *vector;
  IDL_LONG   *index;
  float  *increment;
  IDL_LONG   *num_increments;
  int     ii;

  vector         = (float  *)argv[0];
  index          = (IDL_LONG   *)argv[1];
  increment      = (float  *)argv[2];
  num_increments = (IDL_LONG   *)argv[3];

  for (ii=0; ii < *num_increments; ii++)
  {
    vector[ index[ii] ] += increment[ii];
  }
  
  return(0);
}

IDL_LONG indexed_add_double( int argc, void *argv[] )
{
  double *vector;
  IDL_LONG   *index;
  double *increment;
  IDL_LONG   *num_increments;
  int     ii;

  vector         = (double *)argv[0];
  index          = (IDL_LONG   *)argv[1];
  increment      = (double *)argv[2];
  num_increments = (IDL_LONG   *)argv[3];

  for (ii=0; ii < *num_increments; ii++)
  {
    vector[ index[ii] ] += increment[ii];
  }
  
  return(0);
}

