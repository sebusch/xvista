/*      SUN image display pixel value window labelling			*/

/*      Author: Tod R. Lauer    11/18/89
        Princeton University Observatory                                */
 
#include <suntool/sunview.h>
#include <suntool/panel.h>
#include "tv.h"
#define BUFLEN 20

char tvlabbuf[(BUFLEN + 1) * MAX_IM_HOLD];
 
sunpanel_(bn,text)

int     *bn;
char    *text;

{
        extern Panel_item valstring,
                          buf_panel,
                          obj_panel;
	extern struct box *raster, rasters[MAX_IM_HOLD];
	extern char tvlabbuf[(BUFLEN + 1) * MAX_IM_HOLD];
        char *s, *g, out[BUFLEN+1];
	int i;

        sprintf (out,"BUFFER %2d", *bn);
        panel_set (buf_panel,
                   PANEL_LABEL_STRING,     out,
                   0);

        s       = out;
        for (i = 0; *text != NULL && i < BUFLEN; i++)
                *s++        = *text++;

	*s	= NULL;
 
        panel_set (obj_panel,
                   PANEL_LABEL_STRING,     out,
                   0);

	g	= (BUFLEN + 1) * (raster - rasters) + tvlabbuf;
        s       = out;
        for (i = 0; i <= BUFLEN; i++)
                *g++	= *s++;

	return (0);
}
 
sunpanelb(rastloc)

struct box *rastloc;

{
        extern Panel_item valstring,
                          buf_panel,
                          obj_panel;
	extern struct box *raster, rasters[MAX_IM_HOLD];
	extern char tvlabbuf[(BUFLEN + 1) * MAX_IM_HOLD];
        char *s, out[BUFLEN+1];

	int i;
/*        sprintf (out,"BUFFER %2d", rastloc->bufno + 1);  */
        sprintf (out,"BUFFER %2d", rastloc->bufno);
        panel_set (buf_panel,
                   PANEL_LABEL_STRING,     out,
                   0);

	s	= (BUFLEN + 1) * (rastloc - rasters) + tvlabbuf;
        panel_set (obj_panel,
                   PANEL_LABEL_STRING,     s,
                   0);

	return (0);
}
