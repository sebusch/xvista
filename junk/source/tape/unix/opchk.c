/*	Check if tape channel is open			*/
/*	Written by Richard Stover			*/
/*	Date:	May 31, 1982				*/

#include <sys/types.h>
#include "tm11.h"
int opchk(smt)
register int smt;
/*	smt = Channel number	*/
{
	extern struct mt mt_;
	extern int silent_;

	if(mt_.file[smt] < 1) {
		if(silent_ == 0)
			printf("Tape position unknown\n");
		return(-1);
	}
	return(0);
}

