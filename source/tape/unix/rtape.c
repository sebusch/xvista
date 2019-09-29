/*	Read a physical record from tape.		*/
/*	The value returned is the number of bytes read	*/
/*	if positive, 0 if EOF encountered, and -1 for	*/
/*	other errors.					*/
/*	Written by Richard Stover			*/
/*	Date:	May 31, 1982				*/

#include <sys/types.h>
#include "tm11.h"
TAPE RTAPE(imt,ibuf,nbyte)
#ifdef FORT
TAPE *imt,*nbyte,ibuf[];
#else
TAPE imt,nbyte,ibuf[];
#endif

/*	imt = channel number				*/
/*	ibuf = data buffer				*/
/*	nbyte = number of bytes to write		*/
{
	extern struct mt mt_;
	extern int lastmo[];
	register smt,nrd;
	TAPE icharc;

#ifdef FORT
	smt=(int)(*imt);
#else
	smt = imt;
#endif

/*	Check channel range				*/
	if(uchk(smt))
		return(-1);
/*	Check to see if channel is open			*/
	if(opchk(smt))
		return(-1);

/*	Read the tape					*/
#ifdef FORT
	nrd=read(mt_.mtfdr[smt],ibuf,(int)(*nbyte));
#else
	nrd=read(mt_.mtfdr[smt],ibuf,nbyte);
#endif

/*	Check for EOF					*/
	if(nrd == 0) {
		lastmo[smt]= (-1);	/* Hit EOF	*/
		mt_.file[smt]++;	/* Bump file	*/
	}
	else
		lastmo[smt]= 0;		/* didn't	*/

/*	Update tape status				*/
	SNTAPE(imt,&icharc);

	return(nrd);
}

