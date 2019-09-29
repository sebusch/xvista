/*	Check tape status for the specified tape channel	*/
/*	Written by Richard Stover				*/
/*	Date:	May 20, 1982					*/

#include <sys/types.h>
#include "tm11.h"

TAPE LTAPST(imt,statc)
register char *statc;
#ifdef FORT
TAPE *imt;
#else
TAPE imt;
#endif

/*	imt = tape channel number 1-4				*/
/*	statc = character string of the name of the tape status	*/
/*		that is to be checked.  If the string matches	*/
/*		one of the checked strings and the corresponding*/
/*		software status bit indicates that the condition*/
/*		is true then TRUE (-1) is returned.  Otherwise	*/
/*		FALSE (0) is returned.				*/
{
	register stat;
	extern struct mt mt_;
	int smt;

#ifdef FORT
	smt = (int)(*imt);
#else
	smt = imt;
#endif

/*	Check channel number for legal range			*/
	if(uchk(smt))
		return(0);

/*	Check the various status words				*/
	stat=mt_.status[smt];
	if((strcmp(statc,"OFFLINE")==0)&&(stat&SOFFL))
		return(-1);
	if((strcmp(statc,"NORMAL")==0)&&(stat&SNORM))
		return(-1);
	if((strcmp(statc,"OPINCOMPL")==0)&&(stat&SOPIN))
		return(-1);
	if((strcmp(statc,"EOT")==0)&&(stat&SEOT))
		return(-1);
	if((strcmp(statc,"BOT")==0)&&(stat&SBOT))
		return(-1);
	if((strcmp(statc,"CONTROL")==0)&&(stat&SHDWRE))
		return(-1);
	if((strcmp(statc,"EOF")==0)&&(stat&SEOF))
		return(-1);
	if((strcmp(statc,"EOV")==0)&&(stat&SEOV))
		return(-1);

	return(0);
}

