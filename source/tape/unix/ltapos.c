/*	Position tape to desired file.				*/
/*	Return:							*/
/*	       -1 = success					*/
/*		0 = failure - some error occurred		*/
/*	Written by Richard Stover				*/
/*	Date:	May 30, 1982					*/

#include <sys/types.h>
#include "tm11.h"

TAPE LTAPOS(imt,ifil,irec)
#ifdef FORT
TAPE *imt,*ifil,*irec;
#else
TAPE imt,ifil,irec;
#endif

/*	imt = channel number					*/
/*	ifil= number of desired file				*/
/*	irec= number of desired record (ignored)		*/
{
	extern struct mt mt_;
	extern int silent_;
	register smt,sfile;
	TAPE RWND(),SPACEF(),SPACEB(),sp;

#ifdef FORT
	smt=(int)(*imt);
	sfile=(int)(*ifil);
#else
	smt = imt;
	sfile = ifil;
#endif

/*	Check channel range					*/
	if(uchk(smt))
		return(0);

/*	Check to see if channel is open				*/
	if(opchk(smt))
		return(0);

/*	Make sure file number is legal				*/
	if(sfile < 1) {
		if(silent_ == 0)
			printf("Illegal file number\n");
		return(0);
	}
/*	See if we can rewind					*/
	if(sfile == 1) {
		return(RWND(imt));
	}
/*	Find number of files to skip				*/
	sfile=sfile-mt_.file[smt];

/*	If sfile >= 1 then we only have to skip forward.	*/
/*	Else we have to skip reverse an extra file and then	*/
/*	skip forward 1 record to position to start of the file.	*/

#ifdef FORT
	sp=(TAPE)sfile;
	if(sfile > 0) {
		sp=SPACEF(imt,&sp);
	}
	else {
/*	Must do space reverse thing.				*/
		sp= sp-1;
		sp=SPACEF(imt,&sp);
		if(sp == 0)
			return(0);
		sp=1;
		sp=SPACEB(imt,&sp);
	}
	if(sp == 0)
		return(0);
	else
		return(-1);
#else
	if(sfile > 0) {
		sfile=SPACEF(imt,sfile);
	}
	else {
/*	Must do space reverse thing.				*/
		sfile= sfile-1;
		sfile=SPACEF(imt,sfile);
		if(sfile == 0)
			return(0);
		sfile=1;
		sfile=SPACEB(imt,sfile);
	}
	if(sfile == 0)
		return(0);
	else
		return(-1);
#endif

}

