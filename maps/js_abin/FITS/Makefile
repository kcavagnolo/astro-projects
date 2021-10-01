# objects to be made
objf = FITSGeneral.o FITSImage.o FITSFile.o FITSPosn.o
libf = FITSmm.a

# dependancy headers
fitsh   = FITSGeneral.h FITSImage.h FITSFile.h FITSPosn.h

# c options to pass
CXXFLAGS = -Wall -g -O2

FITSGeneral.o : $(fitsh)
FITSFile.o : $(fitsh)
FITSPosn.o : $(fitsh)
FITSImage.o : $(fitsh)

FITSmm.a : $(objf)
	ar ruv FITSmm.a $?
	ranlib FITSmm.a
