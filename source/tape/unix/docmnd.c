/*	Execute the specified command on the specified tape unit	*/
/*	Written by Richard Stover					*/
/*	Date:	May 20, 1982						*/

#include <sys/types.h>
#include "tm11.h"
#include <sys/ioctl.h>
#include <sys/mtio.h>

int docmnd(cmnd,smt,num)
int cmnd,smt,num;
{
	extern struct mt mt_;
	struct mtop mtcmnd;
	struct mtget mtstat;
	int i;

/*	'num' is the number of records to skip for a space command.	*/
	mtcmnd.mt_count=num;

/*	Issue the command						*/
	mtcmnd.mt_op = cmnd;
	i=ioctl(mt_.mtfdr[smt],MTIOCTOP,(char *)&mtcmnd);

/*	Get the status							*/
	ioctl(mt_.mtfdr[smt],MTIOCGET,(char *)&mtstat);

/*	The returned status						*/
	return(mtstat.mt_resid);

}

