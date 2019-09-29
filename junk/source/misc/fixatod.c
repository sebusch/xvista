#include "Vista.h"
/*	WFPC Analogue to digital converter error corrector		*/

/*	Author: Tod R. Lauer	2/16/87
		Princeton University Observatory			*/

#define MAXVAL	4095
ATUS(fixatod)(pixels, npix, table, width, errors)
float 	*pixels,	/* Input DN values to be corrected.		*/
	*table,		/* Storage space for DN translation table.	*/
	*width,		/* Space for table of DN bin widths.		*/
	*errors;	/* Input reference voltage errors.  This must be
			   a vector containing 12 values (one for each bit) */

int   	*npix;		/* Number of pixels.				*/

{
	float 	signal;
	static int volts[]	= {2048, 1024, 512, 256, 128, 64,
				   32, 16, 8, 4, 2, 1};
	int i, j, k, interr, nbin, step, dn;
	register float *input, *bin;
	float *err, *t, frac, sum, lowsum, value;
	int *reference;

/*	The first step is to calculate the translation table to convert
	the incorrect integer DN values to floating DN values that at
	least have the systematic error removed.			*/

/*	Initially, the translation table will hold the relative DN
	offset of the lower bound of the DN bin.  To start off, set
	this lower bound to its correct value of 0.0, and the bin widths
	to 1.0								*/

	input	= table;
	bin	= width;
	for (i = 0; i <= MAXVAL; i++) {
		*input++	= 0.0;
		*bin++	= 1.0;
	}

/*	Now use the A/D conversion errors in each bit to generate the
	translation table.  Start with errors in the high bits and work
	down to errors in the bits of least significance.  Find the
	bins affected by errors in any given bit.  If the error is
	positive, then increase the width of the bin below the bit
	transition by the size of the error, INCREASE the lower bound
	of the highest bin affected by the error by the fractional
	value of the error and decrease its width.  If the error is negative,
	decrease the width of the lower bin, decrease the lower edge of the
	highest bin affected and increase its width.  Zero out bins which
	should have no A/D values in WFPC image data.			*/

	reference	= volts;
	err	= errors;
	for (i = 0; i < 12; i++) {
		step	= 2.0 * *reference;
		nbin	= MAXVAL / step + 1;	/* Number of bins affected
						   by error in this bit	*/
		input	= table + *reference;   /* Point to first table */
		bin	= width + *reference++; /* entries affected */
		interr	= *err;
		frac	= *err - interr;
		for (j = 0; j < nbin; j++) {
			sum	= 0.0;
			lowsum	= 0.0;
			if (*err > 0.0) {
				t	= bin;
				if (interr >= 1) {
					for (k = 0; k < interr; k++) {
						lowsum	+= 1.0 - *t;
						sum	+= *t;
						*t++	= 0.0;
					}
				}
				if (frac > *(input + interr)) {
					sum	+= frac - *(input + interr);
					*(bin - 1)	+= sum;
					*(bin + interr)	-= frac -
							 *(input + interr);
					*(input - 1)	= lowsum;
					*(input + interr)	= frac;
				}
			} else if (*err < 0.0) {
				t	= bin;
				if (-interr >= 1) {
					for (k = 0; k > interr; k--) {
						lowsum	+= 1.0 - *t;
						sum	+= *t;
						*t--	= 0.0;
					}
				}
				if ( -frac < *(bin + interr - 1)) {
					sum	-= frac;
					*(bin + interr - 1)	+= frac;
					*input  = lowsum + frac;
				} else {
					sum	+= *(bin + interr - 1);
					*(bin + interr - 1)	= 0.0;
					*input  = lowsum + *(bin + interr - 1);
				}
				*bin	+= sum;
			}
			input   += step;
			bin	+= step;
		}
		err++;
	}

/*	Now use the bin boundries and widths that result from the A/D
	errors to make the error correction table.  The correct output
	DN value for any input DN is half of the width of the input DN
	bin plus its lower boundry.  Some input DN values should not
	occur - if they do, just set them to their bin centers.		*/

	input	= table;
	bin	= width;
	for (i = 0; i <= MAXVAL; i++) {
		if (*bin > 0.0) {
			value	= i + *input + *bin++ / 2.0;
			*input++	= value;
		} else {
			*input++	= i + 0.5;	/* Ad hoc fix	*/
			bin++;
		}
	}

/*	The translation table is complete.  Fix up the input pixel values
	by looking up their correct values in the translation table.	*/

	input	= pixels;
	bin	= table;
	for (i = 0; i < *npix; i++) {
		dn	= *input;
		*input++	= *(bin + dn);
	}
}
