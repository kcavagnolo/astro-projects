FFLAGS = -O
F77 = g77
CFLAGS = -O -I/sw/include
CC = gcc
PGPLOT = -L/Applications/scisoft/i386/lib -lpgplot -L/usr/X11R6/lib -lX11
GSLIB = -L/sw/lib -lgsl -lgslcblas
CK = check.o gfr.o
CAV = cavdec.o gfr.o
ALL = check cavedec

.f.o:
	$(F77) $(FFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

cavdec: $(CAV)
	$(F77) $(FFLAGS) $(CAV) -o $@ -lm $(GSLIB) $(PGPLOT)

check: $(CK)
	$(F77) $(FFLAGS) $(CK) -o $@ -lm $(GSLIB)

clean:
	rm -f *.o $(ALL) core*
