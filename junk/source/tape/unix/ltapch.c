/*	Check tape characteristics for the specified channel	*/
/*	Written by Richard Stover				*/
/*	Date:	May 20, 1982					*/

#include <sys/types.h>
#include "tm11.h"

TAPE LTAPCH(imt,statc)
register char *statc;
#ifdef FORT
TAPE *imt;
#else
TAPE imt;
#endif

/*	imt = tape channel 1-4.		*/
/*	statc = character string being tested for	*/
/*	TRUE (-1) is returned if the character string matches	*/
/*	one of the checked strings and the corresponding	*/
/*	software status bit is set.				*/
{
	register stat;
	int smt;
	extern struct mt mt_;

#ifdef FORT
	smt=(int)(*imt);
#else
	smt = imt;
#endif

/*	Check channel number for legal range	*/
	if(uchk(smt))
		return(0);

/*	Check various tape characteristics	*/
	stat=mt_.status[smt];
	if((strcmp(statc,"NORING")==0)&&(stat&SWRL))
		return(-1);
	if((strcmp(statc,"BOT")==0)&&(stat&SBOT))
		return(-1);
	if((strcmp(statc,"1600")==0)&&((stat&SDEN)==0))
		return(-1);
	if((strcmp(statc,"800")==0)&&(stat&SDEN))
		return(-1);
	if(strcmp(statc,"EVEN")==0)
		return(-1);

/*	If none of the above then return FALSE (0)	*/
	return(0);
}

