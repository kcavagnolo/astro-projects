##############################################################################
SRCDIR=src/
BINDIR=bin/
##############################################################################

##############################################################################
# slopes -- linear regressions no scatter and homoscedastic errors

slopes : slopes.o sixlin.o bootsp.o ran3.o
	f77 -o $(BINDIR)slopes slopes.o sixlin.o bootsp.o ran3.o
slopes.o : $(SRCDIR)slopes.f
	f77 -c $(SRCDIR)slopes.f
sixlin.o : $(SRCDIR)sixlin.f
	f77 -c $(SRCDIR)sixlin.f
bootsp.o : $(SRCDIR)bootsp.f
	f77 -c $(SRCDIR)bootsp.f
ran3.o : $(SRCDIR)ran3.f
	f77 -c $(SRCDIR)ran3.f

##############################################################################
# bces_regress -- linear regressions w/ scatter and heteroscedastic errors
# in x and y

bces_regress : bces_regress.o bootspbec.o  ran3.o
	f77 -o $(BINDIR)bces_regress bces_regress.o bootspbec.o ran3.o
bces_regress.o : $(SRCDIR)bces_regress.f
	f77 -c $(SRCDIR)bces_regress.f
bootspbec.o : $(SRCDIR)bootspbec.f
	f77 -c $(SRCDIR)bootspbec.f

##############################################################################
# wls_regress -- weighted linear regressions w/ scatter and heteroscedastic 
# errors in y only

wls_regress : wls_regress.o sixlin.o bootspe.o ran3.o
	f77 -o $(BINDIR)wls_regress wls_regress.o sixlin.o bootspe.o ran3.o
wls_regress.o : $(SRCDIR)wls_regress.f
	f77 -c $(SRCDIR)wls_regress.f
bootspe.o : $(SRCDIR)bootspe.f
	f77 -c $(SRCDIR)bootspe.f

##############################################################################
# COMPILE: compile all programs

compile :
	make slopes
	make bces_regress
	make wls_regress

##############################################################################
# CLEAN: purge *.o files

clean :
	rm *.o

##############################################################################
# INSTALL: compile and clean

install :
	make compile
	make clean
