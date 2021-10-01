# compilers (force gcc)
CXX = g++
CC = gcc

# objects
objFITS = FITS/FITSmm.a
objParammm = classes/parammm/libparammm.a

objAdaptiveBin = AdaptiveBin.o binmodule.o
objABPixelCopy = ABPixelCopy.o
objBinOnGrid = BinOnGrid.o
objFITSMaths = FITSMaths.o
objMakeMask = MakeMask.o
objAdaptiveAnnuli = AdaptiveAnnuli.o
objAdaptiveBinT = AdaptiveBinT.o binmodule.o

# c++ options
CXXFLAGS = -Wall -g -O2 -I./FITS -I./classes

# catch a make
all: AdaptiveBin AdaptiveBinT ABPixelCopy FITSMaths MakeMask AdaptiveAnnuli

# object files
AdaptiveBin.o : binmodule.hh version.hh
ABPixelCopy.o : version.hh
binmodule.o: binmodule.hh
BinOnGrid.o :
FITSMaths :

$(objFITS) :
	cd FITS && $(MAKE) FITSmm.a
$(objParammm) :
	cd classes/parammm && $(MAKE) libparammm.a

# programs
AdaptiveBin : $(objAdaptiveBin) $(objFITS) $(objParammm)
	g++ -o AdaptiveBin $(objAdaptiveBin) $(objFITS) -lm -lcfitsio \
	-L./classes/parammm -lparammm
ABPixelCopy : $(objABPixelCopy) $(objFITS) $(objParammm)
	g++ -o ABPixelCopy $(objABPixelCopy) $(objFITS) -lm -lcfitsio \
	-L./classes/parammm -lparammm
FITSMaths : $(objFITSMaths) $(objFITS)
	g++ -o FITSMaths $(objFITSMaths) $(objFITS) -lm -lcfitsio 
MakeMask : $(objMakeMask) $(objFITS)
	g++ -o MakeMask $(objMakeMask) $(objFITS) -lm -lcfitsio \
	-L./classes/parammm -lparammm
AdaptiveAnnuli : $(objAdaptiveAnnuli) $(objFITS)
	g++ -o AdaptiveAnnuli $(objAdaptiveAnnuli) $(objFITS) -lm -lcfitsio \
	-L./classes/parammm -lparammm
AdaptiveBinT : $(objAdaptiveBinT) $(objFITS) $(objParammm)
	g++ -o AdaptiveBinT $(objAdaptiveBinT) $(objFITS) -lm -lcfitsio \
	-L./classes/parammm -lparammm
