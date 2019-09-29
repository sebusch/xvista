/*	Skip 'num' records on tape specified by 'imt'.		*/
/*	If 'num' is negative then space reverse.		*/
/*	Written by Richard Stover				*/
/*	Date:	May 25, 1982					*/

#include <sys/types.h>
#include "tm11.h"

TAPE SPACEB(imt,num)
#ifdef FORT
TAPE *imt, *num;
#else
TAPE imt,num;
#endif

/*	imt = channel number						*/
/*	num = number of records to skip					*/
{
	int nskip;
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
		dospc(SPR,smt,(-snum));		/* Make the move	*/
		if(LTAPST(imt,"EOF"))
			*sfile -= 1;		/* Decrement file num	*/
		lastmo[smt] = 0;	/* Says EOF not found while	*/
					/* spacing forward		*/
		mt_.status[smt] &= (~SEOV);	/* Not at EOV		*/
	}
	else if(snum>0) {
		if(LTAPST(imt,"EOV") || LTAPST(imt,"EOT")) {
			return(0);	/* Can't move past last file	*/
		}
		nskip=dospc(SPF,smt,snum);
		if(LTAPST(imt,"EOF")) {
			if(((snum-nskip)==1) && lastmo[smt]) {
				mt_.status[smt] |= SEOV;  /* End of volume */
				dospc(SPR,smt,1);	  /* Back up	   */
				dospc(SPR,smt,1);
				dospc(SPF,smt,1);	/* End up going fwd*/
			}
			else {
				*sfile += 1;	/* Bump file number up	  */
			}
			lastmo[smt] = (-1);	/* Says EOF found while	  */
						/* spacing forward	  */
		}
		else
			lastmo[smt] = 0;	/* Says EOF not found while*/
						/* spacing forward	   */
	}

/*	Update tape status						*/
	SNTAPE(imt,&icharc);

/*	Success return							*/
	return(*sfile);
}

