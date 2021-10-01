#LIBS = -lm -lgsl -lgslcblas
LIBS = -lm -L/sw/lib -lgsl -lgslcblas
CFLAGS = -O2 -ggdb3 -Wall -pedantic 
OBJECTS = bimodality.o bimodality_data.o bimodality_kmm.o bimodality_stats.o

install : bimodality
	ln -sf `pwd`/bimodality /usr/bin/

archive : VERSION = `cvs status bimodality.c | grep Working | awk '{print($$3)}'`
archive : bimodality
	tar -zcf /tmp/bimodality-${VERSION}.tar.gz Makefile *.c *.h

bimodality : $(OBJECTS)
	gcc $(CFLAGS) -o bimodality $(OBJECTS) $(LIBS)

bimodality.o : bimodality.c bimodality.h
	gcc $(CFLAGS) -c -o bimodality.o bimodality.c

bimodality_data.o : bimodality_data.c bimodality.h
	gcc $(CFLAGS) -c -o bimodality_data.o bimodality_data.c

bimodality_kmm.o : bimodality_kmm.c bimodality.h
	gcc $(CFLAGS) -c -o bimodality_kmm.o bimodality_kmm.c

bimodality_stats.o : bimodality_stats.c bimodality.h
	gcc $(CFLAGS) -c -o bimodality_stats.o bimodality_stats.c
