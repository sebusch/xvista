/*	Write a physical record to tape.		*/
/*	Return value is -1 if an error occurs or the	*/
/*	number of bytes written otherwise		*/
/*	Written by Richard Stover			*/
/*	Date:	May 31, 9182				*/

#include <sys/types.h>
#include "tm11.h"

TAPE WTAPE(imt,ibuf,nbyte)
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
	register smt,nwr;
	TAPE icharc;

#ifdef FORT
	smt=(int)(*imt);
#else
	smt = imt;
#endif

/*	check channel number				*/
	if(uchk(smt))
		return(-1);

/*	Check for channel open				*/
	if(opchk(smt))
		return(-1);

/*	Write the record				*/
#ifdef FORT
	nwr = write(mt_.mtfdr[smt],ibuf,(int)(*nbyte));
#else
	nwr = write(mt_.mtfdr[smt],ibuf,nbyte);
#endif
	lastmo[smt] = 0;		/* Not at EOF	*/
	mt_.status[smt] &= (~SEOV);	/* Not at EOV	*/

/*	Update tape status				*/
	SNTAPE(imt,&icharc);

	return(nwr);
}

