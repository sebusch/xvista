#include "Vista.h"
/*	Routine to do double image arithmatic.  These routines are called
	by a VISTA FORTRAn routine that has already taken care of finding
	the overlap and any desired offset between the two images	*/

/* Author:	Tod R. Lauer	8/17/87					*/
/*		Princeton University Observatory			*/

/*	Routine to add two images together				*/
ATUS(add2im) (a,ncola,b,ncolb,nc,nr)

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
	bim	= b;
	for (row = *nr; row > 0; row--) {
		for (col = *nc; col > 0; col--)
			*aim++	+= *bim++;
		aim	+= *ncola - *nc;
		bim	+= *ncolb - *nc;
	}

	return (0);
}

/*	Routine to subtract one image from another			*/
ATUS(sub2im) (a,ncola,b,ncolb,nc,nr)

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
	bim	= b;
	for (row = *nr; row > 0; row--) {
		for (col = *nc; col > 0; col--)
			*aim++	-= *bim++;
		aim	+= *ncola - *nc;
		bim	+= *ncolb - *nc;
	}

	return (0);
}

/*	Routine to multitply one image by another.			*/
ATUS(mul2im) (a,ncola,b,ncolb,nc,nr)

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
	bim	= b;
	for (row = *nr; row > 0; row--) {
		for (col = *nc; col > 0; col--)
			*aim++	*= *bim++;
		aim	+= *ncola - *nc;
		bim	+= *ncolb - *nc;
	}

	return (0);
}

/*	Routine to divide one image by another.  As an option, rescaling
	for flatfielding can be done.  As a feature, zero pixels in the
	dividing image zero out corresponding pixels in the numerator
	image.								*/ 
ATUS(div2im) (a,ncola,b,ncolb,nc,nr,f)

float	*a,		/* Destination image				*/
	*b,		/* Source image					*/
	*f;		/* Scale factor					*/

int	*ncola,		/* Number of columns in image A			*/
	*ncolb,		/* Number of columns in image B			*/
	*nc,		/* Number of columns to be added together	*/
	*nr;		/* Number of rows to be added together		*/

{
	register float *aim, *bim;
	int row, col;

	aim	= a;	/* Initialize pointers				*/
	bim	= b;
	if (*f == 0.0) {
		for (row = *nr; row > 0; row--) {
			for (col = *nc; col > 0; col--) {
				if (*bim == 0.0 ) 
					*aim++	= 0.0;
				else
					*aim++	/= *bim;
				bim++;
			}
			aim	+= *ncola - *nc;
			bim	+= *ncolb - *nc;
		}
	} else {
		for (row = *nr; row > 0; row--) {
			for (col = *nc; col > 0; col--) {
				if (*bim == 0.0 )
					*aim++	= 0.0;
				else
					*aim++	/= *bim / *f;
				bim++;
			}
			aim	+= *ncola - *nc;
			bim	+= *ncolb - *nc;
		}
	}

	return (0);
}
