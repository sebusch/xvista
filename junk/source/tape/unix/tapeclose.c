/*			     tapeclose				*/
/*	Close a mag tape channel.				*/
/*	Return value = 1 if channel is successfully closed.	*/
/*		     = 0 if channel can not be closed for any	*/
/*			 reason.				*/
/*	Written by Richard Stover				*/
/*	Date:	   April 10, 1989				*/

#include <sys/types.h>
#include "tm11.h"

extern struct mt mt_;

#ifdef FORT

int tapeclose_(imt)
int *imt;

#else FORT

tapeclose(imt)
int imt;

#endif FORT

/*      imt = desired channel 0-3.                              */
{
	register int smt;
	register int *smtfdr;
	register int cret;

#ifdef FORT
        smt = (int)(*imt);
#else
	smt = imt;
#endif
/*      Check legal range of channel number                     */
	if(uchk(smt))
	    return(0);

/*	If the channel is open then close it.			*/
	smtfdr=(&mt_.mtfdr[smt]);
	if(*smtfdr != -1) {
	    cret = close(*smtfdr);
	    *smtfdr = (-1);
	    if(cret == 0)
		return(1);
	}
	    return(0);
}
