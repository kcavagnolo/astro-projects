#include "config.h"
#include "image.h"
#include "float.h"
/*#include "fitsio.h"*/

#undef H0
#undef kB
#undef G
#undef M_E

/* Basic constants */
#define M_P     1.6726e-24      /* mass of proton */
#define MUE     1.14            /* mue appropriate for 1/3 solar fully ionized*/
#define MUH     1.32            /* muH appropriate for 1/3 solar fully ionized*/
#define MU      0.592438        /* mu appropriate for 1/3 solar fully ionized*/
#define METALICITY 0.30         /* Z>2 elements at 30% solar abundances */
#define KEV_2_K 1.160485e+7     /* convert from keV to K */
#define H0 100.0                /* Hubble constant in km/s/Mpc */
#define q0 0.5                  /* deceleration parameter */
#define C 299792.47             /* speed of light in km/s */
#define PSPCPIXELS 14.947       /* PSPC pixel size in arcsec */
#define kB 1.38066e-16          /* Boltzmann constant in cgs units */
#define G 6.67e-8               /* Newton's constant in cgs units */
#define MPC_2_CM 3.086e+24      /* conversion from Mpc to cm */
#define MSOLAR 1.989e+33        /* Solar mass in cgs units */
#define LSOLAR 3.827e+33        /* Solar luminosity in cgs units */
#define M_E     0.911e-27       /* mass of electron */
#define THOMSON 6.652e-25       /* Thomson cross section */
#define T_CMB   2.736           /* Temperature of CMB in K */
#define hP      6.626075e-27    /* Planck constant in cgs units */
#define FREQUENCY 3.0e+10       /* 30GHz frequency observations */

/* Parameters for the cluster simulation */
#define NGRID  256              /* Number of grids for the cluster simulation */
#define IMSIZE 512              /* Number of grids in mock observation */
#define YNOW  0.249647          /* He abundance today */

#define XCGS 0
#define XCNTS 1
#define TM 2
#define ZM 3
#define TEW 4
#define ZEW 5
#define COMPY 6
#define KSZ 7
#define VNE 8
#define DENSITY 9
#define ENTROPY 10
#define PRESSURE 11
#define DMdensity 12
#define Chandra 13
#define PSPC 14
#define SPT 15
#define SZA 16
#define CMB 17

#define SIGMA 18
#define OVII  19
#define OTHERS 20

/* SZ map in units of Kelvin */
#define TSZ    21
#define SZTOT  22  


#define LINEAR  0
#define LOG10 1
#define LOG2  2

#define NOZOOM 0
#define ZOOM 1
#define BOX 2

/* Define global variables */
float OmegaM,OmegaL,OmegaB,h,*StoreArray;
float AEXPN, ZSIM, ZOBS, IMGSIZE, PIXSIZE, FREQ;
int   NPIX;


double  *Tlam,*Lambda;
int    Tlength;

float  CONTRA,BRIGHT,SIGN;
float  Xi,Yi,L1,L2,Angle;
int    P,Nxl,Nxr,Nyl,Nyr,Flag_zoom,Flag_box;
int    Pos,Binsize,Losbin;

/* Define global variables for xspec.c*/
int     Ebins;
float   Emin, Eincre, Norm_RS_Spec;

/*    .... global setup constants */
#define Ncellmax 5000000
float   Xcell[Ncellmax], Ycell[Ncellmax], Size[Ncellmax];
float   Var[Ncellmax];
int     Ncell;


/* Cluster properties (i.e., virial radius and masses) */
typedef struct {
  float delta[7]; /* overdensity w.r.t. mean */
  float	rvir[7];  /* virial radius */
  float	mvir[7];  /* virial masses */
  float digv[7];  /* cummulative overdensity of gas at rvir[i] */
  float dibv[7];  /* cummulative overdensity of baryons at rvir[i] */
  float dgv[7];   /* differential overdensity of gas at rvir[i] */
  float dbv[7];   /* differential overdensity of baryons at rvir[i] */
  float ddmv[7];  /* differential overdensity of DM at rvir[i] */
  float dtv[7];   /* differential overdensity of total at rvir[i] */
  float pgv[7];   /* pressure at rvir */
  float pigv[7];  /* cummulative pressure at rvir */
  float tmgv[7];  /* mass-weighted temperature at rvir */
  float tmigv[7]; /* cummulative mass-weighted temperature at rvir */

  float ysph[7];  /* integrated compton-Y enclosed within the sphere with radius rvir */
  float yint[7];  /* integrated compton-Y within rvir */
} clusters;
clusters cl;

/* Other stuffs */
#define RA(x,y,z) (15.*((x)+(y)/60.+(z)/3600.))
#define DECL(x,y,z) (fabs(x)/x*(fabs(x)+(y)/60.+(z)/3600.))
#define SKYDIST(a,x1,y1,x2,y2) (sqrt(Squ((a)*((x1)-(x2)))+Squ((y1)-(y2))))
#define DIST(x1,y1,x2,y2) (sqrt(Squ((x1)-(x2))+Squ((y1)-(y2))))

typedef struct {
	float imag,amag,rah,ram,ras,decd,decm,decs,x,y,lum,sky,xc,yc,msig;
        char    type[5],image[20];
        double  ra,dec;
	float	v,ve;
        } object;

typedef struct {
	float	rhoc;	/*central overdensity h^-3*/
	float	rc;	/*core radius h^-1*/
	float	rturn;	/*in Mpc*/
	float	alpha;	/*power law dependence*/
	float	Rhoc;	/*project central overdensity h^-2*/
	float	vbar;	/*mean radial velocity of cluster*/
	char	cluster[50];/*cluster name*/
	} model;

struct binhead {
	int	samples;	/* number of exposures recorded */
	float	frequency;	/* in GHz */
	float	fwhm[3]; 	/* dish diameters in cm */
	};

typedef struct {
	double	u,v,vreal,vimag,weight;
	short int combo;
	} integration;

typedef struct {
	float	omega0;	/*density parameter*/
	float	h;	/*Hubble parameter= h*H0 km/s/Mpc*/
	} cosmo_model;



static float maxarg1, maxarg2;
#define FMAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1) > (maxarg2) ?\
(maxarg1) : (maxarg2))

static float minarg1, minarg2;
#define FMIN(a,b) (minarg1=(a),minarg2=(b),(minarg1) < (minarg2) ?\
(minarg1) : (minarg2))

#define INVERSE_TRANSFORM -1
#define TRANSFORM 1

struct mparam {
	int	realorimag,sizefine,sizebin,sizepsf;
	double	*image,*psfimage;
	float	impars[20],psfpars[20],*binned,*psf,*fimage,*mimage;
	void	(*psfmodel)(),(*immodel)();
};

struct intpair	{
	int	x,y;
};

struct floatpair {
	float	x,y;
};

#define REALMODEL 1
#define IMAGMODEL 0





