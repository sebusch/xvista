#include "Vista.h"
/*	Pixel translation lookup table routine				*/

/*	Author: Tod R. Lauer	4/10/89
		Princeton University Observatory			*/

#include <stdio.h>

ATUS(lookup) (pixels, npix, table, min, max)

float 	*pixels,	/* Input DN values to be corrected.		*/
	*table,		/* Storage space for DN translation table.	*/
	*min,		/* Minimum value in the table.			*/
	*max;		/* Maximum value.				*/

int   	*npix;		/* Number of pixels.				*/

{
	int i, dn;
	register float *input, *bin;

/*	Fix up the input pixel values by looking up their correct values
	in the translation table.					*/

	input	= pixels;
	bin	= table;
	for (i = 0; i < *npix; i++) {
		dn	= *input;
		if (dn < *min)
			dn	= *min;
		else if (dn > *max)
			dn	= *max;

		*input++	= *(bin + dn);
	}
}

/*	This routine is for translating byte pixels into floating values
	by means of a lookup table.					*/

#define	MID	32768

ATUS(lookupc) (pixels, pixin, npix, table)

float 	*pixels,	/* Output pixel values				*/
	*table;		/* Storage space for DN translation table.	*/

unsigned char  	*pixin;	/* Input integer pixels				*/

int	*npix;		/* Number of pixels.				*/

{
	int i;
	register float *output, *bin;
	register unsigned char *pixbuf;

/*	Fix up the input pixel values by looking up their correct values
	in the translation table.					*/

	output	= pixels;
	bin	= table;
	pixbuf	= pixin;
	for (i = 0; i < *npix; i++) {
		*output++	= *(bin + *pixbuf++);
	}
	return (0);
}


ATUS(lookups) (pixels, pixin, npix, table)

float 	*pixels,	/* Output pixel values				*/
	*table;		/* Storage space for DN translation table.	*/

short  	*pixin;		/* Input integer pixels				*/

int	*npix;		/* Number of pixels.				*/

{
	int i;
	register float *output, *bin;
	register short *pixbuf;

/*	Fix up the input pixel values by looking up their correct values
	in the translation table.					*/

	output	= pixels;
	bin	= table + MID;
	pixbuf	= pixin;
	for (i = 0; i < *npix; i++) {
		*output++	= *(bin + *pixbuf++);
	}
	return (0);
}

/*	This routine makes a lookup table for translating scaled bytes
	into real numbers given the FITS BSCALE and BZERO.		*/

ATUS(looktabc) (table,bscale,bzero)

float	*table,		/* Output translation table.			*/
	*bscale,	/* FITS BSCALE					*/
	*bzero;		/* FITS BZERO					*/

{
	short	high, val, valsw;
	float	*tabad;

	tabad	= table;
	for (high = 0; high < 256; high++) {
		*tabad++	= high * *bscale + *bzero;
	}
	return (0);
}

/*	This routine makes a lookup table for translating DEC shorts
	into real numbers given the FITS BSCALE and BZERO.		*/

ATUS(looktabds) (table,bscale,bzero)

float	*table,		/* Output translation table.			*/
	*bscale,	/* FITS BSCALE					*/
	*bzero;		/* FITS BZERO					*/

{
	short	high, low, val, valsw;
	float	*tabad;

	for (high = 0; high < 256; high++) {
		for (low = 0; low < 256; low++) {
			val	= (high << 8 ) | (low & 0x00ff );
			valsw	= (low << 8 ) | (high & 0x00ff );
			tabad	= table + valsw + MID;
		if (tabad < table || tabad > table + 2 * MID)
			printf ("LOOKTABDS error\n");
			*tabad	= val * *bscale + *bzero;
		}
	}
	return (0);
}

/*	This routine makes a lookup table for translating shorts
	into real numbers given the FITS BSCALE and BZERO.		*/

ATUS(looktabs) (table,bscale,bzero)

float	*table,		/* Output translation table.			*/
	*bscale,	/* FITS BSCALE					*/
	*bzero;		/* FITS BZERO					*/

{
	short	high, low, val, valsw;
	float	*tabad;

	for (high = 0; high < 256; high++) {
		for (low = 0; low < 256; low++) {
			val	= (high << 8 ) | (low & 0x00ff );
			tabad	= table + val + MID;
		if (tabad < table || tabad > table + 2 * MID)
			printf ("LOOKTABDS error\n");
			*tabad	= val * *bscale + *bzero;
		}
	}
	return (0);
}
