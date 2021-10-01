CC = CC
CFLAGS = -c -g
CCLDR = CC
LDFLAGS = -g

# Change this to the directoty containing your gsl include and lib directories
LIB_AND_INC_DIR = /usr/local

# Include and linking paths
CINCS = -I$(LIB_AND_INC_DIR)/include/
CLIBS = -L$(LIB_AND_INC_DIR)/lib/ -lgsl -lgslcblas -lm

# Directory in which to put the compiled program
INSTALL_DIR = /usr/local/bin/

.cc.o:
	$(CC) $(CINCS) $(CFLAGS) $<

all: f-test

f-test: f-test.o
	$(CC) $(LDFLAGS) -o f-test f-test.o $(SRCS) $(CLIBS)

install:
	mv f-test $(INSTALL_DIR)

clean:
	rm -f *.o *.l

distclean:
	rm -f *.o *.l f-test
