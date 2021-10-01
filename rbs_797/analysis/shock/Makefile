include kiama.stub
FFLAGS = -O
SSHCK = sshck.o wrout.o
PLFLOW = plflow.o wrout.o
PNBPROF = pubsbprof.o sensit.o wrout.o
EMWT = emwt.o wrout.o
MACH = mach.o wrout.o
PLDE = plde.o wrout.o
ET = eshock.o 
ALL = sshck plflow mach plde plsensit pubsbprof emwt eshock

.f.o:
	$(F77) $(FFLAGS) -c $<

sshck: $(SSHCK)
	$(F77) $(FFLAGS) $(SSHCK) -o $@

plflow: $(PLFLOW)
	$(F77) $(FFLAGS) $(PLFLOW) -o $@ $(PLOTLIB)

emwt: $(EMWT)
	$(F77) $(FFLAGS) $(EMWT) -o $@ $(PLOTLIB)

eshock: $(ET)
	$(F77) $(FFLAGS) $(ET) -o $@

mach: $(MACH)
	$(F77) $(FFLAGS) $(MACH) -o $@

plsensit: plsensit.o
	$(F77) $(FFLAGS) $< -o $@ $(PLOTLIB)

pubsbprof: $(PNBPROF)
	$(F77) $(FFLAGS) $(PNBPROF) -o $@ $(PLOTLIB)

clean:
	rm -f *.o $(ALL) core*
