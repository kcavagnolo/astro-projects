/*  config.h for Simulation fitting program */

#include <math.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/file.h>
#include <fcntl.h>
/*#include "/home2/jjmohr/cfa/Soltan/hfiles/image.h"*/

#define Squ(x) ((x)*(x))
#define Cube(x) ((x)*(x)*(x))
#define Quad(x) (Squ(x)*Squ(x))
#define FIT 1
#define CONSTANT 0
#define FAST 1
#define NO 0
#define YES 1
#define SLOW 0
#define RAD2DEG 57.29578
#define MEM(st) printf("%s\n",st);system("ps aux|grep sim_ensemble");fflush(stdout);
#define PI 3.141592654
