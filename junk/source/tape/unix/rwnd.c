/*	Rewind the tape associated with the specified channel	*/
/*	Written by Richard Stover				*/
/*	Date:	May 20, 1982					*/

#include <sys/types.h>
#include "tm11.h"

TAPE RWND(imt)
#ifdef FORT
TAPE *imt;
#else
TAPE imt;
#endif

/*	imt = tape channel number	*/
{
	register smt;
	extern struct mt mt_;
	extern int lastmo[];
	TAPE icharc;

#ifdef FORT
	smt=(int)(*imt);
#else
	smt = imt;
#endif

/*	Check legal range of imt	*/
	if(uchk(smt))
		return(0);
/*	Make sure the channel has been opened	*/
	if(mt_.file[smt]==-1)
		return(0);
/*	Make sure the drive is on line		*/
	if(LTAPST(imt,"OFFLINE"))
		return(0);
/*	Rewind the tape				*/
	docmnd(RWD,smt,1);

/*	Finally reset the file number to 1, reset the flag	*/
/*	which indicates an EOF on a forward tape motion, and	*/
/*	make sure the EOV bit in the status word is reset.	*/
	mt_.file[smt]=1;
	lastmo[smt]=0;
	mt_.status[smt] &= (~SEOV);

/*	Go sense the tape status to make sure BOT status is set	*/
	SNTAPE(imt,&icharc);

	return(-1);
}

