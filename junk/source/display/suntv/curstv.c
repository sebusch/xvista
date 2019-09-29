/*	CURSTV

	This package sets up and writes to a small cursoe subimage
	window that just displays a zoomed version of the portion
	of the displayed image that lies under the cursor.		*/

/*	Main routines contained here:

	tvopenc_	This does all the work of setting up the complex
			window that will hold the image display.

	tvzoom_c	This writes images prepared for display to
			the screen.

/*	This package handles several problems with using the SUN windows
	environment as an image display.  The image and associated status
	information is displayed in a frame, which can be one of several
	windows that the SUN has to worry about.  VISTA iself, for example
	lives in another window. The philosophy here is to make the
	display window act like a standard stand alone video display,
	complete with zooms, pans, cursors, and so on.  The SUN window
	manager steps in when "damage" has occurred to the display
	window and for handling input to the display.  Both types of
	events occur independent of what VISTA may be doing, so
	asynchronus event handlers must be set up to take care of this	*/

/*	Author:	Tod R. Lauer	2/9/86
	Princeton University Observatory				*/

#include	<suntool/sunview.h>	/* SUN raster utilities		*/
#include	<suntool/canvas.h>
#include	<stdio.h>
#define MAX	800
#define NRTVC	240
#define NCTVC	240
#include	"tv.h"		/* VISTA TV parameters			*/

Frame basec;
Canvas screenc;
struct pixwin 	*chanc;		/* Communication channel to frame	*/

struct rect tvrectc;
struct center lastc, lastfrez;
int freze, frezetog;

/* 	This routine opens a channel to the SUN.  Images will be displayed
	in the SUNwindows environment.  The first part of this code gets
	a new window, sets it up and installs it in the window database.*/

tvopenc_()

{
	extern Frame basec;
	extern Canvas screenc;
	extern Notify_error notify_do_dispatch();
	extern struct pixwin *chanc;	/* Channel to SUN		*/
	extern struct pixrect Vistcurs;
	extern struct rect tvrectc;
	extern struct center lastc;
	static void changec();
	struct rect local;
	static struct singlecolor  fore	= {0,0,0},
				   back	= {255,255,255};
	Event input;
	caddr_t noargs;
	char	*g;
	int	winfd,
		i;

/*	First open the large base frame, which will hold the image
	and auxillary displays						*/

	basec	= window_create(win_nametonumber (getenv("WINDOW_ME")),
			FRAME,
			FRAME_SHOW_LABEL,	TRUE,
			FRAME_LABEL,	"VISTA CURSOR SUBIMAGE",
			FRAME_FOREGROUND_COLOR,	&fore,
			FRAME_BACKGROUND_COLOR,	&back,
			WIN_WIDTH,	NCTVC+10,
			WIN_HEIGHT,	NRTVC+20,
			WIN_X,	5,
			WIN_Y,	5,
			0);

/*	Now create a canvas.  This covers most of the base frame and is
	the part of it that holds the displayed image.  This also sets
	up the special cursor used in the image display			*/

	screenc	= window_create(basec,CANVAS,
			CANVAS_RETAINED,	FALSE,
			WIN_HEIGHT,	NRTVC,
			WIN_CURSOR,	cursor_create(
					CURSOR_IMAGE,	&Vistcurs,
					CURSOR_XHOT,	7,
					CURSOR_YHOT,	7,
			CURSOR_OP,	PIX_COLOR(127) | PIX_SRC | PIX_DST,
					0),
			0);

/*	Now tell VISTA about the size of the display.			*/

	tvrectc.r_left	= 0;
	tvrectc.r_top	= 0;
	tvrectc.r_width	= NCTVC;
	tvrectc.r_height= NRTVC;
	freze	= 0;
	frezetog	= 0;

/*	Open up channels to all of the subwindows.			*/

	chanc	= canvas_pixwin(screenc);

/*	Set up a colormap segment for the main screen.  This will use only
	7 of the available 8 bits to avoid conflict with other windows
	which also need entries in the main SUN color lookup table	*/

	pw_setcmsname(chanc,"vistcolor");	/* Name the color map	*/

/*	Tell the SUN that the routine "change" can be called to fix up
	any damage that occurs to the base frame windows.		*/

	window_set(screenc,
			CANVAS_REPAINT_PROC, 	changec,
			0);

/*	Everything is set up. Display the base frame on the SUN!
	Call notify_do_dispatch to tell the SUN window manager to fix
	up any damage to the window when VISTA has IO pending.  This
	allows VISTA to execute.  Normaly at this point the window
	manager itself would want to handle all process control		*/

	window_set(basec, WIN_SHOW, TRUE, 0);
	notify_do_dispatch();

	return (0);	/* Channel is open			*/
}

/* -------------------------------------------------------------------- */

/*	This is the screen handler called by the SUN window manager when
	damage is done to the screen.  It calls tvzoom to rewrite the
	image buffer to the screen.					*/

changec(canvasdum,pixdum,areadum)

Canvas canvasdum;
Pixwin *pixdum;
Rectlist areadum;

{
	extern struct center lastc;
	extern struct pixwin *chanc;	/* Channel to SUN		*/

	if (lastc.zcn != 0) {
		pw_damaged(chanc);	/* Prepare screen for fixup	*/
		tvzoomc_(&lastc.zcn,&lastc.xcn,&lastc.ycn);
	}
}

/* --------------------------------------------------------------------- */

/*	Routine to zoom in or out of SUN images.  This routine assumes that
	an unzoomed image has been stored in the frame buffer centerred
	in a virtual frame buffer.  The zoom function isolates the area
	to be displayed and calculates the true screen coordinates with
	clipping for display.  The image is then reloaded.		*/

tvzoomc_(z,x,y)

int	*z,				/* Zoom factor			*/
	*x,				/* X center			*/
	*y;				/* Y center			*/

{
	extern Canvas screenc;
	extern struct box *raster;
	extern struct pixwin 	*chanc;
	extern struct pixrect 	pixels;
	extern struct rect	tvrectc;
	extern struct center lastc;
	extern char store[NRTVH*NCTVH];
	extern union pixline {  /* This aligns gbuf on even BYTEs       */
	        char gbuf[MAX]; /* This holds one row of bytes for      */
       		 short dummy;   /* transmission to screen.              */
	} line;

	Event input;
	caddr_t noargs;
	register char *g, *s;
	int 	ixlo, ixloz, ixhiz,
		iylo, iyloz, iyhiz,
		ncz, nrz, nc, xs, ys,
		col2, row2, mid,
		ncex, nrex,
		ictvz, irtvz, zl,
		i, j, k, jump, op;


	if ( *z > 64 || *z < 0)
		return (-1);		/* Zoom error			*/

	zl	=16;
	ixlo	= NCTV / 2 - raster->ncs / 2;
	iylo	= NRTV / 2 - raster->nrs / 2;
	ncz	= NCTVC / zl;		/* Maximum columns displayible	*/
	nrz	= NRTVC / zl;		/* Maximum rows			*/
	col2	= zl * ncz / 2;
	row2	= zl * nrz / 2;
	ixloz	= NCTVC / 2 - col2;  /* Box if display filled	*/
	iyloz	= NRTVC / 2 - row2;
	ixhiz	= zl * ncz + ixloz - 1;
	iyhiz	= zl * nrz + iyloz - 1;
	mid	= nrz - 2 * (nrz / 2);
	op	= PIX_SRC;  /* Display operation	*/

/*	The following section of code is to determine which portion
	of the entire image held in memory to write to the screen, given
	a zoom factor and central pixel coordinate.			*/

	if (*x - ixlo < ncz /2) {	/* Clip left edge		*/
		ncex	= ncz / 2 - *x + ixlo;
		ictvz	= raster->scs;
	        pw_writebackground(chanc,0,0,zl * ncex,NRTVC,op);
	} else {
		ncex	= 0;
		ictvz	= raster->scs + *x -ixlo - ncz / 2;
	}
	ixloz	= ixloz + zl * ncex;

	if (ncz /2 + *x - ixlo + mid > raster->ncs) { /* Clip right edge	*/
		ncex	= ncz / 2 + *x - ixlo - raster->ncs + mid;
	        pw_writebackground(chanc,NCTVC - zl * ncex,0,NCTVC,NRTVC,op);
	} else
		ncex	= 0;

	ixhiz	= ixhiz - zl * ncex;

	if (*y - iylo < nrz / 2) {		/* Clip top edge	*/
		nrex	= nrz / 2 - *y + iylo;
		irtvz	= raster->srs;
	        pw_writebackground(chanc,0,0,NCTVC,zl * nrex,op);
	} else {
		nrex	= 0;
		irtvz	= raster->srs + *y - iylo - nrz / 2;
	}

	iyloz	= iyloz + zl * nrex;

	if (nrz / 2 + *y -iylo + mid > raster->nrs) { /* Clip bottom edge */
		nrex	= nrz / 2 + *y - iylo - raster->nrs + mid;
	        pw_writebackground(chanc,0,NRTVC - zl * nrex,NCTVC,NRTVC,op);
	} else
		nrex	= 0;

	iyhiz	= iyhiz - zl * nrex;

	nrz	= (iyhiz - iyloz + 1)/ zl;	/* Rows in zoomed image	*/
	ncz	= (ixhiz - ixloz + 1)/ zl;	/* Cols in zoomed image	*/

/*	Now display the zoomed image.  Zoom by repeating pixels		*/

	s = raster->byteloc + (ictvz - raster->scs) + raster->ncs *
	    (irtvz - raster->srs); 
	jump	= raster->ncs - ncz;
	xs	= ixloz;	/* Corner screen positions	*/
	ys	= iyloz;
	nc	= zl * ncz;
	pw_lock(chanc,&tvrectc);	/* Lock the SUN video board	*/

/*	This is a software zoom.  Pixels are sent out one row at a time.
	Pixels are repeated in the horizontal direction here.  The SUN
	pw_rop routine will repeat the pixels vertically.		*/

/*	Do the first set if the zoom factor is 1.			*/

	if (zl <= 0) {
	for (i = 0; i < nrz; i++) {
		g	= line.gbuf;
		for (j = 0; j < ncz; j++)
			*g++	= *s++;
		pw_rop(chanc, xs, ys++, nc, 1, op, &pixels, 0, 0);
		s	+= jump;

/*	For large images, the display lock times out before the display
	is complete.  Reset the lock here.  This is an adhoc fix.	*/

		if ((i == nrz/2) && (nrz * ncz > NRTV * NCTV / 2)) {
			pw_unlock(chanc);
			pw_lock(chanc,&tvrectc)	/* Lock SUN video board	*/
		}
	}
	} else {


	for (i = 0; i < nrz; i++) {
		g	= line.gbuf;
		for (j = 0; j < ncz; j++) {
			for (k = 0; k < zl; k++)
				*g++	= *s;
			s++;
		}
		for (k = 0; k < zl; k++)
			pw_rop(chanc, xs, ys++, nc, 1, op, &pixels, 0, 0);
		s	+= jump;

/*	For large images, the display lock times out before the display
	is complete.  Reset the lock here.  This is an adhoc fix.	*/

		if ((i == nrz/2) && (nrz * ncz > NRTV * NCTV / 2)) {
			pw_unlock(chanc);
			pw_lock(chanc,&tvrectc);/* Lock SUN video board	*/
		}
	}
	}

        lastc.xcn        = *x;
        lastc.ycn        = *y;
        lastc.zcn        = zl;

/*	Draw the crosshairs						*/

        pw_vector(chanc,0,120,96,120,PIX_SRC,127);
        pw_vector(chanc,144,120,240,120,PIX_SRC,127);
        pw_vector(chanc,120,0,120,96,PIX_SRC,127);
        pw_vector(chanc,120,144,120,240,PIX_SRC,127);

	pw_unlock(chanc);
	pw_donedamaged(chanc);
	return (0);
}
