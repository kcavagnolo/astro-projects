###############################################################################

# programs paths and flags

SWIG       = swig
CC         = gcc
LD         = ld
SWIGFLAGS  = -I. -I/usr/include/python2.6
CCFLAGS    = -I. -I/usr/include/python2.6 -Wall -fPIC -o3
LDFLAGS    = -lm

###############################################################################

# general make commands

all: __acalc.so __sphere.so __ionosphere.so # __mpfit.so

clean:
	rm -f __types.o __interface.o
	rm -f __acalc.o __acalc_wrap.c __acalc_wrap.o
	rm -f __sphere.o __sphere_wrap.c __sphere_wrap.o
	rm -f __mpfit.o __mpfit_wrap.c __mpfit_wrap.o
	rm -f __ionosphere.o __ionosphere_wrap.c __ionosphere_wrap.o

clean_all:
	rm -f __types.o __interface.o 
	rm -f __acalc.o __acalc_wrap.c __acalc_wrap.o _acalc.py __acalc.so
	rm -f __sphere.o __sphere_wrap.c __sphere_wrap.o _sphere.py __sphere.so
	rm -f __mpfit.o __mpfit_wrap.c __mpfit_wrap.o _mpfit.py __mpfit.so
	rm -f __ionosphere.o __ionosphere_wrap.c __ionosphere_wrap.o _ionosphere.py __ionosphere.so
	rm -f *.pyc *~

package:
	rm -rf spam
	mkdir spam
	cp makefile date.py time.py spam
	cp __types.c __types.h spam
	cp __interface.c __interface.h __interface.i spam
	cp __acalc.c __acalc.h __acalc.i spam
	cp __sphere.c __sphere.h __sphere.i spam
	cp __mpfit.c __mpfit.h __mpfit.i spam
	cp __ionosphere.c __ionosphere.h __ionosphere.i spam
	cp __init__.py files.py aips.py plot.py acalc.py sphere.py parameter.py extraction.py spam
	cp skymodel.py image.py peel.py mpfit.py ionosphere.py solutions.py error.py spam
	cp instrumental.py pointing.py unwrap.py flag.py calibrate.py spam
	tar czvf 'spam_'`python date.py`'_'`python time.py`'.tar.gz' spam
	rm -rf spam

###############################################################################

# make types

__types.o: __types.c __types.h
	$(CC) -c $(CCFLAGS) __types.c -o __types.o

###############################################################################

# make interface

__interface.o: __interface.c __interface.h __types.h
	$(CC) -c $(CCFLAGS) __interface.c -o __interface.o

###############################################################################

# make acalc

__acalc.so: __acalc.o __acalc_wrap.o __interface.o __types.o
	$(LD) -shared $(LDFLAGS) __acalc.o __acalc_wrap.o __interface.o __types.o -o __acalc.so

__acalc.o: __acalc.c __acalc.h __interface.h __types.h
	$(CC) -c $(CCFLAGS) __acalc.c -o __acalc.o

__acalc_wrap.o: __acalc_wrap.c
	$(CC) -c $(CCFLAGS) __acalc_wrap.c -o __acalc_wrap.o

__acalc_wrap.c: __acalc.h __acalc.i __interface.h __types.h
	$(SWIG) -python $(SWIGFLAGS) __acalc.i

###############################################################################

# make sphere

__sphere.so: __sphere.o __sphere_wrap.o __acalc.o __interface.o __types.o
	$(LD) -shared $(LDFLAGS) __sphere.o __sphere_wrap.o __acalc.o __interface.o __types.o -o __sphere.so

__sphere.o: __sphere.c __sphere.h __acalc.h __interface.h __types.h
	$(CC) -c $(CCFLAGS) __sphere.c -o __sphere.o

__sphere_wrap.o: __sphere_wrap.c
	$(CC) -c $(CCFLAGS) __sphere_wrap.c -o __sphere_wrap.o

__sphere_wrap.c: __sphere.h __sphere.i __acalc.h __interface.h __types.h
	$(SWIG) -python $(SWIGFLAGS) __sphere.i

###############################################################################

# make mpfit

__mpfit.so: __mpfit.o __mpfit_wrap.o __acalc.o __interface.o __types.o
	$(LD) -shared $(LDFLAGS) __mpfit.o __mpfit_wrap.o __acalc.o __interface.o __types.o -o __mpfit.so

__mpfit.o: __mpfit.c __mpfit.h __acalc.h __interface.h __types.h
	$(CC) -c $(CCFLAGS) __mpfit.c -o __mpfit.o

__mpfit_wrap.o: __mpfit_wrap.c
	$(CC) -c $(CCFLAGS) __mpfit_wrap.c -o __mpfit_wrap.o

__mpfit_wrap.c: __mpfit.h __mpfit.i __acalc.h __interface.h __types.h
	$(SWIG) -python $(SWIGFLAGS) __mpfit.i

###############################################################################

# make ionosphere

__ionosphere.so: __ionosphere.o __ionosphere_wrap.o __sphere.o __acalc.o __interface.o __types.o
	$(LD) -shared $(LDFLAGS) __ionosphere.o __ionosphere_wrap.o __sphere.o __acalc.o __interface.o __types.o -o __ionosphere.so

__ionosphere.o: __ionosphere.c __ionosphere.h __sphere.h __interface.h __types.h
	$(CC) -c $(CCFLAGS) __ionosphere.c -o __ionosphere.o

__ionosphere_wrap.o: __ionosphere_wrap.c
	$(CC) -c $(CCFLAGS) __ionosphere_wrap.c -o __ionosphere_wrap.o

__ionosphere_wrap.c: __ionosphere.h __ionosphere.i __sphere.h __acalc.h __interface.h __types.h
	$(SWIG) -python $(SWIGFLAGS) __ionosphere.i

###############################################################################
