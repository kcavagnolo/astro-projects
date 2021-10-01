include kiamanew.stub
FFLAGS = -O
CFLAGS = -Wall -O
CXXFLAGS = -O $(PGINC) $(GSINC)
CSI = cosinfo.o cosbits.o cospar.o phyconst.o labelval.o integ.o
CC_SCE = cosbits.cc cospar.cc labelval.cc phyconst.cc integ.cc cosinfo.cc

ALL = cosinfo

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

cosinfo: $(CSI)
	$(CXX) $(CXXFLAGS) $(CSI) -o $@

clean:
	rm -f *.o *.d core* $(ALL)
	rmtilde


-include $(CC_SCE:.cc=.d)

#-include $(C_SCE:.c=.d)

