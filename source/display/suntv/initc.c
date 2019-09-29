/*	VISTA initializatino routines that must be called from C	*/

/*	Author:	Tod R. Lauer
		Princeton University Observatory
		12/8/87							*/

#include <sys/vadvise.h>
#include <suntool/sunview.h>
#include "tv.h"

int suninit = 0;

initc()

{
	extern int imtvnum_;
	extern struct box *raster, rasters[MAX_IM_HOLD];
	struct box *rastloc;
	int i;

/*	Tell sysytem to modify memory paging algorithm			*/

	vadvise(VA_NORM);
#ifdef NOTDEF
	imtvnum_  = -1000;
#endif
	raster	= rasters - 1;
	rastloc	= rasters;
        for (i = 0; i < MAX_IM_HOLD; i++) {
        	rastloc->bufno	= -1000;
        	rastloc->byteloc= NULL;
		rastloc->loctv = NULL;
                rastloc++;
	}
	suninit = 1;
}

releasetv_(loc)
int *loc;

{
	extern struct box *raster, rasters[MAX_IM_HOLD];
	struct box *rastloc;
	int i;

	if (suninit == 0) return(0);

	if (*loc == raster->loctv) {
	   raster->loctv = NULL;
	   lights(-2);
	}

	rastloc = rasters;
	for (i = 0; i < MAX_IM_HOLD; i++) {
	   if (*loc == rastloc->loctv) {
	      rastloc->loctv = NULL;
	      rastloc++;
	   }
	}
	return(0);
}
  
