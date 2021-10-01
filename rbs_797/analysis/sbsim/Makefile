#include kiamanew.stub
FFLAGS = -O
CFLAGS = -O
#PGINC = -I/Applications/scisoft/i386/Packages/pgplot-5.2.2/lib
GSINC = -I/usr/local/include/  -L/usr/local/lib/ -lgsl -lgslcblas
CXXFLAGS = -O $(PGINC) $(GSINC)
ECV = ellcav.o
CC_SCE = ellcav.cc

ALL = ellcav

# Automates dependencies - see info for GNU make
%.d: %.cc
	@$(CXX) -MM $(CXXFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@: ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

%.d: %.c
	@$(CC) -MM $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@: ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.f.o:
	$(F77) -c $(FFLAGS) $<

.c.o:
	$(CC) -c $(CFLAGS) $<

.cc.o:
	$(CXX) -c $(CXXFLAGS) $<

ellcav: $(ECV)
	$(CXX) $(CXXFLAGS) $(ECV) -o $@ $(GSLIB)

clean:
	rm -f *.o *.d core* $(ALL)


-include $(CC_SCE:.cc=.d)

#-include $(C_SCE:.c=.d)

