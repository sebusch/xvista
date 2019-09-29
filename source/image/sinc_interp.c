#include "Vista.h"
/*	Routine to do Since interpolation using predefined arrays		*/

/*	Author:	Tod R. Lauer							*/
/*		NOAO 								*/
/*		9/24/97								*/

#include <math.h>

ATUS(sincinterp)(x, y, a, nx, nincr, len, sincbuf, out)

int	*nx,		/* X dimension of the image array		*/
 	*nincr,		/* Number of subpixel steps			*/
	*len;		/* Length of sinc array				*/

float	*x, *y,		/* Interpolated pixel x and y location		*/
	*a,		/* Image array pointer				*/
	*sincbuf,	/* Buffer to hold the 2D sinc arrays		*/
	*out;		/* Interpolated result				*/

{
	float	*sinc,	/* Points to sinc buffer			*/
		*pix,	/* Points to pixels in array			*/
		dx, dy,	/* Fractional pixel steps			*/
		sum;	/* Storage for running sum			*/

	int	i,j,	/* Counters					*/
		ix, iy,	/* Integer pixel location			*/
		skip;	/* Number of image pixels to skip in x		*/


/*	Calculate the fractional pixel step and locate the first pixel in the	*/
/*	patch over which to perform the convolution.				*/

	ix	= *x - 1;
	dx	= *x - ix -1;
	if (dx >= 0.5 - 0.5 / (float) *nincr) {
		ix	+= 1;
		dx	-= 1.0;
	}


	iy	= *y - 1;
	dy	= *y - iy -1;
	if (dy >= 0.5 - 0.5 / (float) *nincr) {
		iy	+= 1;
		dy	-= 1.0;
	}

	pix	= a + ix + *nx * (iy - *len / 2) - *len / 2;
	skip	= *nx - *len;

/*	Given dx and dy, find the proper location in the sinc buffer, rounding	*/
/*	dx and dy to the nearest step as required.				*/

	j	= (int) ((float) *nincr * (dx + 0.5) + 0.5);
	i	= (int) ((float) *nincr * (dy + 0.5) + 0.5);
	sinc	= sincbuf + (*nincr * *len * *len * i) + (*len * *len * j);

/*	Perform the convolution of the image and sinc function			*/

	sum	= 0.0;
	for (i = 0; i < *len; i++) {
		for (j = 0; j < *len; j++) {
			sum	+= *pix++ * *sinc++;
		}
		pix	+= skip;
	}
	*out	= sum;
}
