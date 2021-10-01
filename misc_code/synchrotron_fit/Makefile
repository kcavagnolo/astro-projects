# Makefile                                   
# $Id: Makefile,v 1.2 2009-12-05 13:57:28 cavagnolo Exp $ 
#                                                                           
# Copyright (C) 2003,2004,2005,2006 Christopher Z Waters                    
#                                                                           
# This program is free software; you can redistribute it and/or modify      
# it under the terms of the GNU General Public License as published by      
# the Free Software Foundation; either version 2 of the License, or         
# (at your option) any later version.                                       
#                                                                           
# This program is distributed in the hope that it will be useful,           
# but WITHOUT ANY WARRANTY; without even the implied warranty of            
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             
# GNU General Public License for more details.                              
#                                                                           
# You should have received a copy of the GNU General Public License         
# along with this program; if not, write to the Free Software               
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA 

# These should be changed to optimize for your given architechure.  The 
# default listed here is designed for Pentium3/Celeron D machines, and should
# work for any newer computers.
CFLAGS      = -Wall -pedantic -Wunreachable-code -O3 -march=x86-64 -fomit-frame-pointer -msse2 -mmmx -mfpmath=sse -falign-loops -falign-functions -maccumulate-outgoing-args -time -ggdb3

LIBRARIES   = -lc -lm -lgsl -lgslcblas

install : synchrotron_fit
	ln -sf `pwd`/synchrotron_fit /usr/bin/

synchrotron_fit : synchrotron_fit.o gsl_fitting.o synchrotron_functions.o read_data.o
	gcc synchrotron_fit.o gsl_fitting.o synchrotron_functions.o read_data.o -o ./synchrotron_fit $(LIBRARIES)

read_data.o : read_data.c read_data.h
	gcc $(CFLAGS) -c read_data.c

synchrotron_functions.o : synchrotron_functions.c synchrotron_functions.h
	gcc $(CFLAGS) -c synchrotron_functions.c

gsl_fitting.o : gsl_fitting.c gsl_fitting.h
	gcc $(CFLAGS) -c gsl_fitting.c

synchrotron_fit.o : synchrotron_fit.c
	gcc $(CFLAGS) -c synchrotron_fit.c

