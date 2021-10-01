CPGLIB =	-L /usr/local/pgplot -lcpgplot -lpgplot
PGLIB  = 	-L /usr/local/pgplot -lpgplot
FITSIO =        /usr/local/lib/libcfitsio.a -lm
XLIB = 		-L /usr/X11R6/lib32 -lX11
BINDIR =        ./

# Include directories:
CPGDIR =  /usr/local/pgplot

PGDIR =  /usr/local/pgplot

# Include header file
CPGHED = /usr/local/pgplot/cpgplot.h

### Compilers and flags:

# gcc
CC = gcc
CFLAGS = -O2 

# fortran
FC = f77
FFLAGS=

# Default 
default: makemap_cpg compy SZmap_binary2fits SZmap_add_binary2fits profile2d

# Clean up:
clean:
	rm -f *.o *.*~

# Targets
# CAUTION: $(PRESS2nd) $(PRESS2ndD) must precede $(CPGLIB) $(XLIB) $(CCLIB) 

makemap_cpg: makemap_cpg.c cpgmacros.c ctools.c nrutil.c dqromb.c 
	$(CC) $(CFLAGS) -c  -I$(CPGDIR) makemap_cpg.c cpgmacros.c ctools.c nrutil.c dqromb.c dtrapzd.c dpolint.c
	$(FC) -o $(BINDIR)makemap_cpg makemap_cpg.o  cpgmacros.o ctools.o nrutil.o dqromb.o dtrapzd.o dpolint.o -lm \
	$(CPGLIB) $(XLIB) $(CCLIB)

compy: compy.c
	$(CC) $(CFLAGS) -c  -I$(CPGDIR) compy.c ctools.c nrutil.c dqromb.c dtrapzd.c dpolint.c
	$(FC) -o $(BINDIR)compy compy.o  ctools.o nrutil.o dqromb.o dtrapzd.o dpolint.o -lm \
	$(CPGLIB) $(XLIB) $(CCLIB)

SZmap_binary2fits : SZmap_binary2fits.o ctools.o nrutil.c dqromb.c dtrapzd.c dpolint.c
	$(CC) $(CFLAGS) -o $(BINDIR)SZmap_binary2fits SZmap_binary2fits.o ctools.o nrutil.o dqromb.o dtrapzd.o dpolint.o -lm \
	$(FITSIO)

SZmap_add_binary2fits : SZmap_add_binary2fits.o ctools.o nrutil.c dqromb.c dtrapzd.c dpolint.c
	$(CC) $(CFLAGS) -o $(BINDIR)SZmap_add_binary2fits SZmap_add_binary2fits.o ctools.o nrutil.o dqromb.o dtrapzd.o dpolint.o -lm \
	$(FITSIO) 

profile2d: profile2d.c ctools.c nrutil.c dqromb.c dtrapzd.c dpolint.c ran1.c 
	$(CC) $(CFLAGS) -c  -I$(CPGDIR) profile2d.c ctools.c nrutil.c dqromb.c dtrapzd.c dpolint.c ran1.c
	$(FC) -o $(BINDIR)profile2d profile2d.o ctools.o nrutil.o dqromb.o dtrapzd.o dpolint.o ran1.o -lm \
	$(CPGLIB) $(XLIB) $(CCLIB) 


