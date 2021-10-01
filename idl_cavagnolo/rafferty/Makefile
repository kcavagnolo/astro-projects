F77 = f77
FFLAGS = -O
COOL = deconv.o

.f.o:
	$(F77) -c $(FFLAGS) $<

deconv: $(COOL)
	$(F77) $(FFLAGS) $(COOL) -o $@

