/*	Skip num EOF marks.  If two adjacent EOF marks are	*/
/*	encountered on a space forward, skipping will stop	*/
/*	Return:							*/
/*		0 = failure - some error occurred		*/
/*		n = current file number (which is always >0 )	*/
/*	Written by Richard Stover				*/
/*	Date:	May 30, 1982					*/

#include <sys/types.h>
#include "tm11.h"

TAPE SPACEF(imt,num)
#ifdef FORT
TAPE *imt,*num;
#else
TAPE imt,num;
#endif

/*	imt = channel number					*/
/*	num = number of files to skip				*/
/*		If num < 0 skip reverse				*/
/*		If num > 0 skip forward				*/

{
	int nskip,i;
	TAPE icharc;
	register smt,snum,sunit;
	register int *sfile;
	extern int lastmo[];
	extern struct mt mt_;

#ifdef FORT
	smt=(int)(*imt);
#else
	smt = imt;
#endif

/*	Check channel range						*/
	if(uchk(smt))
		return(0);

/*	Check to see if channel is opened				*/
	sfile=(&mt_.file[smt]);
	if(opchk(smt))
		return(0);

/*	Update tape status						*/
	SNTAPE(imt,&icharc);

/*	All is OK, so move the tape					*/
#ifdef FORT
	snum=(int)(*num);
#else
	snum = num;
#endif
	if(snum<0) {
		if(LTAPST(imt,"BOT")) {
			return(0);		/* Can't move past BOT	*/
		}
		dospc(SPRF,smt,-snum);	/* Make the move	*/
		SNTAPE(imt,&icharc);
		if(LTAPST(imt,"BOT")) {
			*sfile = 1;
		}
		else
			*sfile += snum;
		lastmo[smt] = 0;	/* Says EOF not found while	*/
					/* spacing forward		*/
		mt_.status[smt] &= (~SEOV);	/* Not at EOV		*/
	}
	else if(snum>0) {
		if(LTAPST(imt,"EOV") || LTAPST(imt,"EOT")) {
			return(0);	/* Can't move past last file	*/
		}
		nskip = dospc(SPFF,smt,snum);
		*sfile += snum-nskip;
		if(nskip == 0) {
			lastmo[smt] = 0;	/* Did not hit EOF while */
		}				/* spacing forward.	 */
		else{
			mt_.status[smt] |= SEOV;/* End of volume 	*/
			lastmo[smt] = (-1);	/* did hit EOF.		*/
			dospc(SPR,smt,1);	  /* Back up	   */
			dospc(SPR,smt,1);
			dospc(SPF,smt,1);	/* End up going fwd*/
			*sfile -= 1;
		}

	}

/*	Update tape status						*/
	SNTAPE(imt,&icharc);

/*	Success return							*/
	return(*sfile);
}

