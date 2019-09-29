/*	This command does the actual register manipulation	*/
/*	for the spaceb routine.					*/
/*	Written by Richard Stover				*/
/*	Date:	May 31, 1982					*/

#include <sys/types.h>
#include "tm11.h"

int dospc(cmnd,smt,num)
int cmnd,smt,num;
/*	cmnd = the tape command code (SPF or SPR)		*/
/*	smt  = the desired tape channel				*/
/*	num  = the number of records to skip			*/
{
	int notskip,docmnd();
	TAPE icharc,imt;

/*	Do the actual space command.  The value returned is	*/
/*	the number of records not skipped due to an EOF.	*/
	notskip=docmnd(cmnd,smt,num);
/*	Update tape status					*/
#ifdef FORT
	imt=(TAPE)smt;
	SNTAPE(&imt,&icharc);
#else
	SNTAPE(smt,&icharc);
#endif

	return(notskip);
}

