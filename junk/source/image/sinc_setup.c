#include "Vista.h"
/*	Routine to setup Sinc function arrays for repeated use.			*/

/*	This routine set up an effectively 4-D matrix, which consists of a 2D	*/
/*	matrix of 2D sinc arrays.  A sinc array is setup for each possible 	*/
/*	increment in dx or dy as specified by *nincr.  Once generated, the	*/
/*	arrays can be looked up as required.  The arrays are tapered down	*/
/* 	to the fractional value *tap at the edge of the arrays.			*/

/*	Author:	Tod R. Lauer							*/
/*		NOAO 								*/
/*		9/23/97								*/

#include <math.h>

ATUS(sincsetup)(nincr, len, tap, sincbuf, temp)

int	*nincr,		/* Number of subpixel steps			*/
	*len;		/* Length of sinc array				*/

float	*tap,		/* Fractional taper at array edge		*/
	*sincbuf,	/* Buffer to hold the 2D sinc arrays		*/
	*temp;		/* Scratch buffer to hole 1D arrays		*/

{
	float	*onex,	/* Points to 1-D arrays.			*/
		*oney,	/* Points to 1-D arrays.			*/
		*sinc,	/* Points to sinc buffer			*/
		beta,	/* Exponential taper				*/
		dx,	/* Fractional pixel step			*/
		x,	/* Length from center				*/
		f,	/* Value of sinc-array element			*/
		term,	/* Holds exponential kernal			*/
		taper,	/* Local taper of element			*/
		sum;	/* Storage for running sum			*/

	int	i,j,k,m,/* Counters					*/
		n2,	/* Sinc radius					*/
		sign;	/* Sinc sign					*/

/*	First calculate exponential taper factor 			*/

	beta	= sqrt ( -log ( (double) *tap)) / ((*len - 1) / 2);

/*	First calculate a 1-D array for each substep.			*/

	onex	= temp;
	n2	= *len / 2;
	for (i = 0; i < *nincr; i++) {
		sum	= 0.0;
		dx	= -0.5 + (float) i / (float) *nincr;
		x	= -n2 - dx;
		sign	= pow ( (double) -1.0, (double) n2);
		for (j = 0; j < *len; j++) {
			term	= x * beta;
			taper	= exp ( (double) -term * term);
		if (x > -0.2 / (float) *nincr && x < 0.2 / (float) *nincr)
				f	= 1.0;
		else if (dx > -0.2/ (float) *nincr && dx < 0.2/ (float) *nincr)
				f	= 0.0;
			else
				f	= sign * taper / x;

			*onex++	= f;
			sum	+= f;
			x	+= 1.0;
			sign	= -sign;
		}

/*		Normalize the new 1-D array to unit integral			*/

		onex	-= *len;
		for (j = 0; j < *len; j++) {
			*onex++	/= sum;
		}
	}

/*	Now generate 2-D sinc arrays and pack into the holding buffer		*/

	sinc	= sincbuf;
	oney	= temp;
	for (i = 0; i < *nincr; i++) {			/* dy loop		*/
		onex	= temp;
		for (j = 0; j < *nincr; j++) {		/* dx loop		*/
			for (k = 0; k < *len; k++) {
				for (m = 0; m < *len; m++)
					*sinc++	= *oney * *onex++;
				oney++;
				onex	-= *len;
			}
			onex	+= *len;
			oney	-= *len;
		}
		oney	+= *len;
	}
}
