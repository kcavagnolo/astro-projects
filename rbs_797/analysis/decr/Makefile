FFLAGS = -O
F77 = g77
CC = gcc
#CFLAGS = -O -I/usr/local/include
#PGPLOT = -lpgplot -L/usr/local/pgplot/lib -lX11
#GSLIB = -L/usr/local/lib -lgsl -lgslcblas
CFLAGS = -O -I/sw/include
PGPLOT = -lpgplot -L/Applications/scisoft/i386/Packages/pgplot-5.2.2/lib
GSLIB = -L/sw/lib/ -lgsl -lgslcblas
CK = check.o gfr.o
CAV = cavdec.o gfr.o
ALL = check cavdec

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
