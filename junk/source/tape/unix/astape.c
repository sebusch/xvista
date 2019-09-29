/*	Assign a mag tape channel to a tape unit	*/
/*	Written by Richard Stover			*/
/*	Date:	May 25, 1982				*/

#include <sys/types.h>
#include "tm11.h"

/*	Define initial values for global variables	*/

struct mt mt_ = {
	SOFFL,SOFFL,SOFFL,SOFFL,SOFFL,	/* status	*/
	0,0,0,0,0,			/* unit		*/
	-1,-1,-1,-1,-1,			/* mtfdr	*/
	-1,-1,-1,-1,-1			/* file		*/
	};

int lastmo[5]={0,0,0,0,0};
			/* This variable controls when an EOV is	*/
			/* encountered.  It is set true whenever an	*/
			/* EOF is encountered on a forward tape motion	*/
			/* (either spacing or reading) and is reset	*/
			/* for all other tape motions.  If lastmo is	*/
			/* set and another EOF is encountered then	*/
			/* EOV becomes true.				*/

int silent_ = 0;	/* This is a flag to stop error msg output	*/

#ifdef FORT

int astape_(imt,iunit,magname,magl)
int *imt,*iunit;
char *magname;
TAPE magl;

#else

astape(imt,iunit,magname)
int imt,iunit;
char *magname;

#endif
/*	imt = desired channel 0-3.				*/
/*	iunit = desired physical mag tape unit.			*/
/*	magname = returned mag tape name, i.e. /dev/nrmts0	*/
/*	magl = length of character array magname.		*/
{
	TAPE icharc;
	register sunit,smt;
	int mode;
	register int *smtfdr;
/*	static char mtname[]="/dev/srmt12";		
	static char mtname[]="/dev/tape0";  */
	static char mtname[]="/dev/rmt0";

#ifdef FORT
	sunit = (int)(*iunit);
	smt = (int)(*imt);
#else
	sunit = iunit;
	smt = imt;
#endif

/*	Check legal range of unit				*/
/* Commented out to allow for /rmt 0,4,8,12
/*	if((sunit<0)||(sunit>3)) {
		if(silent_ == 0)
			printf("Unit out of range 0-3\n");
		return(0);
	} */

/*	Check legal range of channel number			*/
	if(uchk(smt))
		return(0);

/*	If channel was previously open then close it and get	*/
/*	get ready to open it again.				*/
	mt_.file[smt]=0;
	mt_.unit[smt]=sunit;
	smtfdr=(&mt_.mtfdr[smt]);
	if(*smtfdr != -1) {
		close(*smtfdr);
		*smtfdr = (-1);
	}
/*	Build correct file name	and return it to caller		*/
	mtname[strlen(mtname)-1]='0'+sunit;
#if defined(__alpha)
        mtname[strlen(mtname)-1]='h';
        printf("mtname: %s\n",mtname);
#endif
	strcpy(magname,mtname);

/*	Initially open the tape just for reading		*/
	if((*smtfdr=open(mtname,0)) == -1) {
		mt_.file[smt] = -1;
		return(0);
	}

/*	Check for write-ring					*/
	SNTAPE(imt,&icharc);
	if(LTAPCH(imt,"NORING"))
		mode=0;
	else
		mode=2;

/*	Close tape unit						*/
	close(*smtfdr);

/*	Open a file to the mag tape using final mode		*/
	*smtfdr=open(mtname,mode);
	if(*smtfdr == -1) {
		mt_.file[smt] = -1;
		return(0);
	}

	return(1);
}

