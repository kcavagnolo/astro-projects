F77 = f77
FFLAGS = -O
COOL = newcool.o step.o

.f.o:
	$(F77) -c -m64 $(FFLAGS) $<

newcool: $(COOL)
	$(F77) -m64 $(FFLAGS) $(COOL) -o $@

