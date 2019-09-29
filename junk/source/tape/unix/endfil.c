/*	Write an EOF mark				*/
/*	Written by Richard Stover			*/
/*	Date:	June 1, 1982				*/

#include <sys/types.h>
#include "tm11.h"

TAPE ENDFIL(imt)

#ifdef FORT
TAPE *imt;
#else
TAPE imt;
#endif

/*	imt = channel number				*/
{
	extern int lastmo[];
	extern struct mt mt_;
	register smt;
	TAPE icharc;

#ifdef FORT
	smt=(int)(*imt);
#else
	smt = imt;
#endif

/*	check channel range				*/
	if(uchk(smt))
		return(0);

/*	Check if channel is open			*/
	if(opchk(smt))
		return(0);

/*	Write an EOF					*/
	docmnd(WREOF,smt,1);

/*	Clear the EOF-encountered-on-space-forward flag	*/
	lastmo[smt] = 0;

/*	Update the tape status				*/
	SNTAPE(imt,&icharc);

/*	Bump the file count by one if write was successful	*/

/* 	This next error check just always fails on the SUN 4 drive, even
	  when EOF write was successful, so I just bypass it here and
	  return -1   J.Holtzman 11/88 */

 	mt_.file[smt]++;
        return(-1);

/*	if(mt_.status[smt] & SEOF) {
		mt_.file[smt]++;
		return(-1);
	}
	else 
		return(0); */
}

