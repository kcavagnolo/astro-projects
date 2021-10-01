/****************************************************************/
/*	Standard FOCAS 2D image header		valdes	8/27/82	*/
/****************************************************************/
/* 512 byte machine readable header describing two dimensional	*/
/* image contained in rest of file.  Image will be coded:	*/
/*								*/
/*   (bit|char|int|long) pic [pysize][pxsize] 			*/
/*								*/
/* where the type is selected from value of bitpix. 		*/
/*								*/
/* This image structure is fashioned to be related to the	*/
/* FITS flexible format as documented.  The units and display	*/
/* map types are coded and the values given below are only a	*/
/* small set which can be gradually enlarged as the need arises.*/
/****************************************************************/

#define PIXEL	float		/* Incore pixel type		*/

struct imgdef {			/* Image Header definition	*/
	short blksiz;		/* FITS storage blocksize	*/
	short bitpix;		/* Bits per pixel		*/
	short naxis;		/* Number of axis		*/
	short naxis1;		/* Pixels on first or x dim	*/
	short naxis2;		/* Pixels on second or y dim	*/
	short dflags;		/* Picture display flags	*/
	short maptyp;		/* Default display map		*/
	short bunit;		/* Brightness unit		*/
	short ctype1;		/* Coordinate 1 type		*/
	short ctype2;		/* Coordinate 2 type		*/
	PIXEL  maxpxl;		/* Maximum pixel value		*/
	PIXEL  minpxl;		/* Minimum pixel value		*/
	float bscale;		/* Pixel value scale		*/
	float bzero;		/* Pixel value zero		*/
	float crpix1;		/* Reference point on x dim	*/
	float crval1;		/* Coordinate value at ref.	*/
	float cdelt1;		/* Increment along x dim	*/
	float crota1;		/* Rotation angle of coord x	*/
	float crpix2;		/* Reference point on y dim	*/
	float crval2;		/* Coordinate value at ref.	*/
	float cdelt2;		/* Increment along y dim	*/
	float crota2;		/* Rotation angle of coord y	*/
	PIXEL  blank;		/* Value of undefined pixels	*/
	char  simple[2];	/* Boolean simple flag		*/
				/* If T then FITS standard	*/
				/* If F then floating pt. pixel	*/
	short bytepix;		/* 8-bit bytes per pixel	*/
	float dscale;		/* Display scaling		*/
	float dzero;		/* Display zero			*/
	char  dummy[12];	/* Expansion space		*/
	char  object[32];	/* Image name			*/
	char  origin[32];	/* Origin of image file		*/
	char  date[32];		/* Date of image file creation	*/
	char  observer[32];	/* Observer/Owner/Creator	*/
	char  nextpic[32];	/* Filename of next image in seq*/
	char  comment[256];	/* Comments, history, etc.	*/
	};

#define MAXPXL 2147483647	/* Maximum value of pixel	*/
#define MAXBUF  62000		/* Buffer for image i/o		*/
#define MXSZ	4096*4096	/* Maximum size of incore image	*/

				/* Display flags		*/
#define FLIPV 01		/* Display first line at bottom	*/


struct image {			/* INCORE IMAGE DESCRIPTION		*/
  int x0,y0;			/* Origin of image in scan coordinates	*/
  int dx,dy;			/* Size of image			*/
  PIXEL min,max;		/* Minimum and maximum pixel values	*/
  PIXEL *pic;			/* Pointer to beginning of image data	*/
  int	mxsz;			/* Maximum size of image data		*/
  };
