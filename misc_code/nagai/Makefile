HTMLDIR=./
# Flags for HPUX
#optimize1  =  +Oall +Odataprefetch +Ocache_pad_common +FPD
#optimize2  =  +O4 +Olimit +Odataprefetch +Ocache_pad_common +FPD
#optimize3  =  +O3 -R8  
#lib =   +U77

# Flags for Solaris
#optimize1  =  -O3
#optimize2  =  -O3
#optimize3  =  -O3 -dbl  

# General default
optimize1  =  -O
optimize2  =  -O
optimize3  =  -O

#lib =  

all: PMmain PMpower PMmodels PMmodelCHDM PMhalos PMstartCDM PMstartCHDM PM_to_ASCII PMselect

clean:
	-rm *.o *.dvi *.log *.aux pmcode.ps *.tar *.DAT *.gz
	-rm PMmain PMpower PMmodels PMmodelCHDM PMhalos PMstartCDM PMstartCHDM PM_to_ASCII
	-rm InStart.dat fort.*
	-rm -rf pmcode

manual: pmcode.tex
	latex pmcode
	latex pmcode
	dvips pmcode

manual-html: pmcode.tex
	latex2html -info "0" -local_icons -no_math -html_version 3.2,math -t "PM Code" pmcode

tar:
	tar cvf PMcode.tar *.f *.h *.fit Makefile 
	gzip PMcode.tar
	tar cvf TEST32x128.tar TEST32x128
	gzip TEST32x128.tar
	tar cvXf .exclude.tar TEST128x256.tar TEST128x256
	gzip TEST128x256.tar

install: tar
	tar cvf - pmcode | (cd $(HTMLDIR) ; tar xvf -)
	mv PMcode.tar.gz $(HTMLDIR)/pmcode/
	cp pmcode.ps $(HTMLDIR)/pmcode/PMcode.ps
	gzip $(HTMLDIR)/pmcode/PMcode.ps
	-mkdir $(HTMLDIR)/pmcode/cgi-bin
	cp /home/avalon2/holtz/HTML/cgi-bin/PMregister.pl $(HTMLDIR)/pmcode/cgi-bin/

PMmain: PMmain.o PMauxiliary.o
	f77 $(lib) $(optimize1) -o PMmain PMmain.o PMauxiliary.o

PMmain.o: PMmain.f PMparameters.h PMmesh.h
	f77 $(lib) $(optimize1) -c  PMmain.f

PMauxiliary.o: PMauxiliary.f PMparameters.h PMmesh.h
	f77 $(lib) $(optimize1) -c  PMauxiliary.f

PMpower: PMpower.o PMauxiliary.o
	f77 $(lib) $(optimize1) -o PMpower PMpower.o PMauxiliary.o

PMpower.o: PMpower.f PMparameters.h PMmesh.h
	f77 $(lib) $(optimize1) -c  PMpower.f

PMmodels: PMmodels.o 
	f77 $(lib) $(optimize3) -o PMmodels PMmodels.o

PMmodels.o: PMmodels.f
	f77 $(lib) $(optimize3) -c  PMmodels.f

PMmodelCHDM: PMmodelCHDM.o 
	f77 $(lib) $(optimize3) -o PMmodelCHDM PMmodelCHDM.o

PMmodelCHDM.o: PMmodelCHDM.f
	f77 $(lib) $(optimize3) -c  PMmodelCHDM.f

PMhalos: PMhalos.o PMauxiliary.o
	f77 $(lib) $(optimize1) -o PMhalos PMhalos.o PMauxiliary.o

PMhalos.o: PMhalos.f PMparameters.h PMlists.h
	f77 $(lib) $(optimize1) -c  PMhalos.f

PMstartCDM: PMstartCDM.o PMauxiliary.o
	f77 $(lib) $(optimize2) -o PMstartCDM PMstartCDM.o PMauxiliary.o

PMstartCDM.o: PMstartCDM.f PMparameters.h 
	f77 $(lib) $(optimize2) -c  PMstartCDM.f

PMstartCHDM: PMstartCHDM.o PMauxiliary.o
	f77 $(lib) $(optimize2) -o PMstartCHDM PMstartCHDM.o PMauxiliary.o

PMstartCHDM.o: PMstartCHDM.f PMparameters.h 
	f77 $(lib) $(optimize2) -c  PMstartCHDM.f

PM_to_ASCII: PM_to_ASCII.o PMauxiliary.o
	f77 $(lib) $(optimize2) -o PM_to_ASCII PM_to_ASCII.o PMauxiliary.o

PM_to_ASCII.o: PM_to_ASCII.f PMparameters.h 
	f77 $(lib) $(optimize2) -c  PM_to_ASCII.f

PMselect: PMselect.o PMauxiliary.o
	f77 $(lib) $(optimize2) -o PMselect PMselect.o PMauxiliary.o

PMselect.o: PMselect.f PMparameters.h 
	f77 $(lib) $(optimize2) -c  PMselect.f
