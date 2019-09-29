#include "Vista.h"
/*	Routines to do double image arithmatic between fourier and real
	images.  These routines are called by a VISTA FORTRAN routine that
	has already checked the image arrays.				*/

/* Author:	Tod R. Lauer	4/17/88					*/
/*		Princeton University Observatory			*/

/*	Filter an image by multiplying a fourier image by a real image.
	Only half of the real image is needed.				*/
ATUS(mul2four)(a,ncola,b,ncolb,nc,nr)

float	*a,		/* Destination image				*/
	*b;		/* Source image					*/

int	*ncola,		/* Number of columns in image A			*/
	*ncolb,		/* Number of columns in image B			*/
	*nc,		/* Number of columns to be added together	*/
	*nr;		/* Number of rows to be added together		*/

{
	register float *aim, *bim;
	int row, col;

	aim	= a;	/* Initialize pointers				*/
	bim	= b + *ncolb * *nr / 2 + *nc;
	for (row = 0; row < *nr / 2; row++) {
		for (col = *nc + 1; col > 0; col--) {
			*aim++	*= *bim;
			*aim++	*= *bim++;
		}
		bim	+= *ncolb - *nc - 1;
	}

	bim	= b + *nc;
	for (row = 0; row < *nr / 2; row++) {
		for (col = *nc + 1; col > 0; col--) {
			*aim++	*= *bim;
			*aim++	*= *bim++;
		}
		bim	+= *ncolb - *nc - 1;
	}

	return (0);
}

/*	Routine to divide one image by another.  As an option, rescaling
	for flatfielding can be done.  As a feature, zero pixels in the
	dividing image zero out corresponding pixels in the numerator
	image.								*/ 
ATUS(div2four)(a,ncola,b,ncolb,nc,nr)

float	*a,		/* Destination image				*/
	*b;		/* Source image					*/

int	*ncola,		/* Number of columns in image A			*/
	*ncolb,		/* Number of columns in image B			*/
	*nc,		/* Number of columns to be added together	*/
	*nr;		/* Number of rows to be added together		*/

{
	register float *aim, *bim;
	int row, col;

	aim	= a;	/* Initialize pointers				*/
	bim	= b + *ncolb * *nr / 2 + *nc;
	for (row = 0; row < *nr / 2; row++) {
		for (col = *nc + 1; col > 0; col--) {
			if (*bim != 0.0) {
				*aim++	/= *bim;
				*aim++	/= *bim++;
			} else {
				*aim++	=0.0;
				*aim++	=0.0;
				bim++;
			}
		}
		bim	+= *ncolb - *nc - 1;
	}

	bim	= b + *nc;
	for (row = 0; row < *nr / 2; row++) {
		for (col = *nc + 1; col > 0; col--) {
			if (*bim != 0.0) {
				*aim++	/= *bim;
				*aim++	/= *bim++;
			} else {
				*aim++	=0.0;
				*aim++	=0.0;
				bim++;
			}
		}
		bim	+= *ncolb - *nc - 1;
	}

	return (0);
}
