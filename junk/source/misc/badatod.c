#include "Vista.h"
/*	WFPC Analogue to digital converter error simulator		*/

/*	This routine is designed to simulate the effects of A/D conversion
	errors in data obtained with the WFPC.  The basic algorithm is a
	direct software duplication of the A/D conversion process.  The
	pixels input are assumed to be analogue signals and must have
	a smooth and continuous distribution of floating values.  The A/D
	converter will translate these values into output integer DN by
	sucessively comparing the input signal with a decrementing power
	of 2 series of voltages.  At any stage, if the input signal is
	greater than the comparison voltage, the appropriate bit is set
	in the output DN, and the reference voltage is subtracted from
	the signal before the next stage.  The WFPC produces 12 bit DN,
	so the first bit tested will be the 2048 bit.  The 1024 bit is
	then tested and so on.

	The errors in the WFPC digitization process appear to occur
	during the reference voltage comparison, but not during the
	subsequent subtraction of the reference from the signal once
	the (possibly erroneous) bit has been set.  More specifically,
	the error model assumes that a bit-dependent error in the reference
	voltage occurs during the comparison that will cause improper
	setting of the corresponding bit in the output DN for signals
	within ``error value'' of the reference voltage.  Once the bit
	decision is made, however, the actual voltage subtracted from the
	signal for subsequent bit-tests is free from error (even though
	the decision to subtract it or not may be erroneous).

	Errors in the reference voltages appear to be constant for
	a given WFPC CCD at a given electronic bay temperature.  Errors
	may be less than one DN, therefore, for proper simulation of
	the WFPC A/D the input pixel MUST NOT BE PRESET TO INTEGER VALUES. */

/*	Author: Tod R. Lauer	2/16/87
		Princeton University Observatory			*/
ATUS(badatod)(pixels, npix, errors)

float 	*pixels,	/* Input pixel signals.  Once the A/D conversion
			   has been made, the resulting DN are converted
			   back to floating type and returned here.	*/
	*errors;	/* Input reference voltage errors.  This must be
			   a vector containing 12 values (one for each bit) */

int   	*npix;		/* Number of pixels.				*/

{
	float 	signal;
	static float volts[]	= {2048.0, 1024.0, 512.0, 256.0, 128.0, 64.0,
				   32.0, 16.0, 8.0, 4.0, 2.0, 1.0};
	static short mask[]	= {0x0800, 0x0400, 0x0200, 0x0100,
				   0x0080, 0x0040, 0x0020, 0x0010,
				   0x0008, 0x0004, 0x0002, 0x0001};
	short dn;
	int i, j;
	register float *input, *reference, *err;
	register short *bit;

/*	Go through the pixel array					*/

	input	= pixels;
	for (i = 0; i < *npix; i++) {
		signal	= *input;
		dn	= 0;		/* Clear output DN		*/
		bit	= mask;
		reference	= volts;
		err	= errors;

/*	Check to see which bits to set in the output DN.  The
	reference voltage is compared to the signal and subtracted
	off if the bit is set.  The error model adds in a bit dependent
	signal during the comparison phase.				*/

		for (j = 0; j < 12; j++) {
			if (signal >= *err++ + *reference) {
				signal	-= *reference;
				dn	|= *bit;
			}
			bit++;
			reference++;
		}

		*input++	= dn;	/* Return DN values to pixel stream */
	}
}
