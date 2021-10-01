# Sample FORTRAN Makefile
#
# $Revision: 1.1.1.1 $
# 
FC = gfortran
FFLAGS = -fdefault-real-8 -g -ICUBPACK #-JCUBPACK
LDLIBS = -LCUBPACK -lcubpack
OBJS =
# FC = ifort
# MKL_PATH=/opt/intel/mkl/10.0.1.014/lib/32
# FFLAGS = -I${MKL_PATH} -L${MKL_PATH}
# FFLAGS = -r8 -ICUBPACK
# LDLIBS = -lmkl_blas95 -lmkl_lapack95 -lmkl_ia32 -lguide -lpthread

%.o : %.f90
	${FC} ${FFLAGS} -c $<

%.mod : %.f90
	${FC} ${FFLAGS} -c $<

cosmo : cosmo.f90 cosmo_def.mod cubpack
	${FC} ${FFLAGS} cosmo.f90 ${LDLIBS} -o $@

test : cosmo
	./cosmo

cubpack : 
	${MAKE} -C CUBPACK

clean : 
	rm -f *.o *.mod cosmo *~*
