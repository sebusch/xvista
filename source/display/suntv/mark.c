#include "Vista.h"
/*	Routine to transfer pixels marked on the display screen to VISTA  */

/*	Author:	Tod R. Lauer		4/11/86
	Princeton University Observatory				*/

#include "tv.h"
mark_(row,col,key)

int	*row,		/* Pixel row coordinate				*/
	*col;		/* Pixel column coordinate			*/

char	*key;		/* ASCII key entered				*/

{
#ifdef __SUNVIEW
	extern struct markpix pixin;
	extern int waiting_for_c,
		   from_screen;

	waiting_for_c	= 1;	/* Set flag for interupt routine	*/
	lights(1);		/* Turn on INPUT light			*/
	if (read(from_screen,key,1) > 0) {
		*row	= pixin.prow;
		*col	= pixin.pcol;
		waiting_for_c	= 0;
		lights(-1);	/* Turn off INPUT light			*/
		return (0);
	} else {
		printf ("Screen pipe failure...\n");
		waiting_for_c	= 0;
		return (-1);
	}
#endif
}

mark(row,col,key)

int	*row,		/* Pixel row coordinate				*/
	*col;		/* Pixel column coordinate			*/

char	*key;		/* ASCII key entered				*/

{
#ifdef __SUNVIEW
	extern struct markpix pixin;
	extern int waiting_for_c,
		   from_screen;

	waiting_for_c	= 1;	/* Set flag for interupt routine	*/
	lights(1);		/* Turn on INPUT light			*/
	if (read(from_screen,key,1) > 0) {
		*row	= pixin.prow;
		*col	= pixin.pcol;
		waiting_for_c	= 0;
		lights(-1);	/* Turn off INPUT light			*/
		return (0);
	} else {
		printf ("Screen pipe failure...\n");
		waiting_for_c	= 0;
		return (-1);
	}
#endif
}
