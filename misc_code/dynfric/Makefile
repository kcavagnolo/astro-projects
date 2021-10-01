install : dynamical_friction
	ln -sf `pwd`/dynamical_friction /usr/bin/

dynamical_friction : dynamical_friction.c 
	gcc -Wall -pedantic -O2 -o $@ $< -lgsl -lgslcblas -lm

archive :  VERSION = `cvs status dynamical_friction.c | grep Working | awk '{print($$3)}'`
archive : 
	tar -hzcf dynamical_friction-${VERSION}.tar.gz --transform 's%./%./dynamical_friction/%' ./Makefile ./dynamical_friction.c

tests: dynamical_friction tests_8 tests_16 

T = 12e9
t = 1e7

tests_0.5 : 
	dynamical_friction -r 1000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R1_e0.5_M5
	dynamical_friction -r 2000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R2_e0.5_M5
	dynamical_friction -r 3000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R3_e0.5_M5
	dynamical_friction -r 4000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R4_e0.5_M5
	dynamical_friction -r 5000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R5_e0.5_M5
	dynamical_friction -r 7500 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R7_e0.5_M5
	dynamical_friction -r 10000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R10_e0.5_M5
	dynamical_friction -r 12500 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R12_e0.5_M5
	dynamical_friction -r 15000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R15_e0.5_M5
	dynamical_friction -r 17500 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R17_e0.5_M5
	dynamical_friction -r 20000 -e 0.5 -t ${t} -T ${T} -D -M 1e5 > /tmp/R20_e0.5_M5

	dynamical_friction -r 1000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R1_e0.5_M6
	dynamical_friction -r 2000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R2_e0.5_M6
	dynamical_friction -r 3000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R3_e0.5_M6
	dynamical_friction -r 4000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R4_e0.5_M6
	dynamical_friction -r 5000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R5_e0.5_M6
	dynamical_friction -r 7500 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R7_e0.5_M6
	dynamical_friction -r 10000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R10_e0.5_M6
	dynamical_friction -r 12500 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R12_e0.5_M6
	dynamical_friction -r 15000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R15_e0.5_M6
	dynamical_friction -r 17500 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R17_e0.5_M6
	dynamical_friction -r 20000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R20_e0.5_M6

	dynamical_friction -r 1000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R1_e0.5_M7
	dynamical_friction -r 2000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R2_e0.5_M7
	dynamical_friction -r 3000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R3_e0.5_M7
	dynamical_friction -r 4000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R4_e0.5_M7
	dynamical_friction -r 5000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R5_e0.5_M7
	dynamical_friction -r 7500 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R7_e0.5_M7
	dynamical_friction -r 10000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R10_e0.5_M7
	dynamical_friction -r 12500 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R12_e0.5_M7
	dynamical_friction -r 15000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R15_e0.5_M7
	dynamical_friction -r 17500 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R17_e0.5_M7
	dynamical_friction -r 20000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R20_e0.5_M7

tests_8 : 
	dynamical_friction -r 8000 -e 0.0 -t ${t} -T ${T} -D -M 1e6 > /tmp/R8_e0.0_M6
	dynamical_friction -r 8000 -e 0.0 -t ${t} -T ${T} -D -M 1e7 > /tmp/R8_e0.0_M7
	dynamical_friction -r 8000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R8_e0.5_M6
	dynamical_friction -r 8000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R8_e0.5_M7
	dynamical_friction -r 8000 -e 0.999 -t ${t} -T ${T} -D -M 1e6 > /tmp/R8_e1.0_M6
	dynamical_friction -r 8000 -e 0.999 -t ${t} -T ${T} -D -M 1e7 > /tmp/R8_e1.0_M7

tests_3 :
	 dynamical_friction -r 3000 -e 0.0 -t ${t} -T ${T} -D -M 1e6 > /tmp/R3_e0.0_M6
	dynamical_friction -r 3000 -e 0.0 -t ${t} -T ${T} -D -M 1e7 > /tmp/R3_e0.0_M7
	dynamical_friction -r 3000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R3_e0.5_M6
	dynamical_friction -r 3000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R3_e0.5_M7
	dynamical_friction -r 3000 -e 0.999 -t ${t} -T ${T} -D -M 1e6 > /tmp/R3_e1.0_M6
	dynamical_friction -r 3000 -e 0.999 -t ${t} -T ${T} -D -M 1e7 > /tmp/R3_e1.0_M7

tests_16 : 
	dynamical_friction -r 16000 -e 0.5 -t ${t} -T ${T} -D -M 1e6 > /tmp/R16_e0.5_M6
	dynamical_friction -r 16000 -e 0.5 -t ${t} -T ${T} -D -M 1e7 > /tmp/R16_e0.5_M7
	dynamical_friction -r 16000 -e 0.999 -t ${t} -T ${T} -D -M 1e6 > /tmp/R16_e1.0_M6
	dynamical_friction -r 16000 -e 0.999 -t ${t} -T ${T} -D -M 1e7 > /tmp/R16_e1.0_M7



