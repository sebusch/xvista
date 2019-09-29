/*	SUNTV

	This package contains routines to display images on the SUN
	by use of the `pixrect' utilities				*/

/*	Main routines contained here:

	tvopen_		This does all the work of setting up the complex
			window that will hold the image display.

	tvfload_	This prepares the floating images held in VISTA
			for transfer to the screen.

	tvzoom_		This writes images prepared for display to
			the screen.

	tvcolorld_	This sends out the colormap to the screen.	*/

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

/*	Author:	Tod R. Lauer	2/9/86	Mod 4/14/91
	Princeton University Observatory				*/

#include	<suntool/sunview.h>	/* SUN raster utilities		*/
#include	<suntool/canvas.h>
#include	<suntool/panel.h>
#include	<stdio.h>
#include	"tv.h"		/* VISTA TV parameters			*/
#define	MAX	NCTVH

char store[NRTVH*NCTVH*MAX_IM_HOLD];	/* Image frame buffer		*/

Frame base;
Canvas screen, colorbar;
struct pixwin 	*chan,		/* Communication channel to frame	*/
		*barch,		/* Color map display			*/
		*litch1,	/* Status display			*/
		*litch2,	/* Status display			*/
		*litch3,	/* Status display			*/
		*litch4;	/* Status display			*/

static short Vistcurs_dat[] = {			/* This loads in a data	*/
#include "Vistcurs"				/* file containing the	*/
};						/* screen cursor shape	*/
mpr_static(Vistcurs, 16, 16, 1, Vistcurs_dat);

struct rect tvrect;
union pixline {			/* This aligns gbuf on even BYTEs	*/
	char gbuf[MAX];		/* This holds one row of bytes for	*/
	short dummy;		/* transmission to screen.		*/
} line;
mpr_static(pixels, MAX, 1, 8, (short *)line.gbuf);	/* Memory pixrect */

struct center last;
struct box *raster,
	    rasters[MAX_IM_HOLD];
int	n_raster;

Panel pixvalwin;		/* This is a window to show pixel values */
Panel_item valstring,
	   buf_panel,
	   obj_panel;

int 	from_screen,		/* These are variables for pipe between	*/
	to_vista,		/* VISTA "terminal" and the screen	*/
	waiting_for_c;

/* 	This routine opens a channel to the SUN.  Images will be displayed
	in the SUNwindows environment.  The first part of this code gets
	a new window, sets it up and installs it in the window database.*/

tvopen_()

{
	extern Frame base;
	extern Canvas screen, colorbar;
	extern Panel pixvalwin;
	extern Panel_item valstring,
	   		  buf_panel,
	   		  obj_panel;
	extern Notify_error notify_do_dispatch();
	extern struct pixwin *chan,	/* Channel to SUN		*/
			     *barch,
			     *litch1,
			     *litch2,
			     *litch3,
			     *litch4;
	extern union pixline	line;
	extern struct pixrect pixels;	/* Channel to SUN		*/
	extern struct pixrect Vistcurs;
	extern struct rect tvrect;
	extern struct box *raster, rasters[MAX_IM_HOLD];
	extern struct center last;
	extern int 	from_screen,
		to_vista,
		waiting_for_c,
		n_raster;
	static void screen_input(),
			colorbar_input(),
			change();
	struct rect local;
	static struct singlecolor  fore	= {0,0,0},
				   back	= {255,255,255};
	Event input;
	Canvas light1, light2, light3, light4;
	Panel_item panhead;
	caddr_t noargs;
	char	*g;
	int	winfd,
		i, pipefd[2],
		bitmask;

/*	First open the large base frame, which will hold the image
	and auxillary displays						*/

	base	= window_create(win_nametonumber (getenv("WINDOW_ME")),
			FRAME,
			FRAME_SHOW_LABEL,	TRUE,
			FRAME_LABEL,	"VISTA IMAGE DISPLAY",
			FRAME_FOREGROUND_COLOR,	&fore,
			FRAME_BACKGROUND_COLOR,	&back,
			WIN_WIDTH,	NCTV+10,
			WIN_HEIGHT,	NRTV+100,
			WIN_X,	XOFF-20,
			WIN_Y,	YOFF+5,
			0);

/*	Now create a canvas.  This covers most of the base frame and is
	the part of it that holds the displayed image.  This also sets
	up the special cursor used in the image display			*/

	screen	= window_create(base,CANVAS,
			CANVAS_RETAINED,	FALSE,
			WIN_HEIGHT,	NRTV,
			WIN_CURSOR,	cursor_create(
					CURSOR_IMAGE,	&Vistcurs,
					CURSOR_XHOT,	7,
					CURSOR_YHOT,	7,
			CURSOR_OP,	PIX_COLOR(127) | PIX_SRC | PIX_DST,
					0),
			0);

/*	This lets the SUN window manager what events to expect from the
	screen.  "screeb_input" is identified as the event handler.
	Events expected are keyboard keys, mouse buttons and motion	*/

	window_set(screen,
			WIN_EVENT_PROC,	screen_input,
			WIN_CONSUME_KBD_EVENT,  WIN_ASCII_EVENTS,
			WIN_CONSUME_PICK_EVENTS, WIN_MOUSE_BUTTONS, LOC_STILL,
			0,
			0);

/*	This sets up a small area at the bottom of the base frame which
	will be used to display a running pixel value and position of
	the cursor when it is in the screen.				*/

	pixvalwin	= window_create(base, PANEL,
					WIN_BELOW,	screen,
					WIN_HEIGHT,	40,
					WIN_WIDTH,	NCTV-410,
					WIN_TOP_MARGIN,	5,
					WIN_LEFT_MARGIN,	5,
					WIN_RIGHT_MARGIN,	5,
					0);

/*	This sets up another small window at the bottom of the screen
	which will hold a colorbar showing the colormap.  It will use
	the same cursor type as the main screen, but has a separate
	event handler, "colorbar_input", which will interpret mouse
	events to change the color lookup table.			*/

	colorbar	= window_create(base, CANVAS,
					CANVAS_RETAINED,	FALSE,
					WIN_BELOW,	screen,
					WIN_RIGHT_OF,	pixvalwin,
					WIN_WIDTH,	270,
					CANVAS_WIDTH,	270,
					CANVAS_HEIGHT,	40,
					WIN_TOP_MARGIN,	5,
					WIN_LEFT_MARGIN,	5,
					WIN_RIGHT_MARGIN,	5,
					WIN_EVENT_PROC,	colorbar_input,
			WIN_CURSOR,	cursor_create(
					CURSOR_IMAGE,	&Vistcurs,
					CURSOR_XHOT,	7,
					CURSOR_YHOT,	7,
			CURSOR_OP,	PIX_COLOR(127) | PIX_SRC | PIX_DST,
					0),
			WIN_CONSUME_PICK_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
			0,
					0);

/*	Two small windows are created to display one word status info.	*/

	light3		= window_create(base, CANVAS,
					WIN_BELOW,	screen,
					WIN_RIGHT_OF,	colorbar,
					WIN_WIDTH,	65,
					WIN_HEIGHT,	17,
					CANVAS_WIDTH,	65,
					CANVAS_HEIGHT,	17,
					WIN_TOP_MARGIN,	5,
					WIN_LEFT_MARGIN,	5,
					WIN_RIGHT_MARGIN,	5,
					0);

	light4		= window_create(base, CANVAS,
					WIN_BELOW,	light3,
					WIN_RIGHT_OF,	colorbar,
					WIN_WIDTH,	65,
					CANVAS_WIDTH,	65,
					WIN_HEIGHT,	18,
					CANVAS_HEIGHT,	18,
					WIN_TOP_MARGIN,	6,
					WIN_LEFT_MARGIN,	5,
					WIN_RIGHT_MARGIN,	5,
					0);


	light1		= window_create(base, CANVAS,
					WIN_BELOW,	screen,
					WIN_RIGHT_OF,	light3,
					WIN_WIDTH,	65,
					WIN_HEIGHT,	17,
					CANVAS_WIDTH,	65,
					CANVAS_HEIGHT,	17,
					WIN_TOP_MARGIN,	5,
					WIN_LEFT_MARGIN,	5,
					WIN_RIGHT_MARGIN,	5,
					0);

	light2		= window_create(base, CANVAS,
					WIN_BELOW,	light1,
					WIN_RIGHT_OF,	light4,
					WIN_WIDTH,	65,
					CANVAS_WIDTH,	65,
					WIN_HEIGHT,	18,
					CANVAS_HEIGHT,	18,
					WIN_TOP_MARGIN,	6,
					WIN_LEFT_MARGIN,	5,
					WIN_RIGHT_MARGIN,	5,
					0);

/*	Setup two strings for displaying running pixel info in the pixel
	display window.  The first is just a header, the second, valstring
	will be update as the cursor moves arouns in the screen.	*/

	panhead	= panel_create_item(pixvalwin, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	" ROW   COL   VALUE",
				PANEL_ITEM_X,	ATTR_COL(1),
				PANEL_ITEM_Y,	5,
					0);

	valstring	= panel_create_item(pixvalwin, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	" ",
				PANEL_ITEM_X,	ATTR_COL(1),
				PANEL_ITEM_Y,	22,
					0);

	buf_panel	= panel_create_item(pixvalwin, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	" ",
				PANEL_ITEM_X,	ATTR_COL(25),
				PANEL_ITEM_Y,	5,
					0);

	obj_panel	= panel_create_item(pixvalwin, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	" ",
				PANEL_ITEM_X,	ATTR_COL(25),
				PANEL_ITEM_Y,	22,
					0);

	window_fit(base);	/* Fit all windows into the base frame	*/

/*	Now tell VISTA about the size of the display.			*/

	tvrect.r_left	= 0;
	tvrect.r_top	= 0;
	tvrect.r_width	= NCTV;
	tvrect.r_height	= NRTV;
	last.zcn	= 0;
	n_raster	= 0;
	raster		= rasters;
	for (i = 0; i < MAX_IM_HOLD; i++) {
		raster->byteloc	= NULL;
		raster++;
	}
	raster		= rasters - 1;

/*	Open up channels to all of the subwindows.			*/

	chan	= canvas_pixwin(screen);
	barch	= canvas_pixwin(colorbar);
	litch1	= canvas_pixwin(light1);
	litch2	= canvas_pixwin(light2);
	litch3	= canvas_pixwin(light3);
	litch4	= canvas_pixwin(light4);

/*	Set up a colormap segment for the main screen.  This will use only
	7 of the available 8 bits to avoid conflict with other windows
	which also need entries in the main SUN color lookup table	*/

	bitmask	= 0x007f;
#ifdef NOTDEF
	pr_putattributes(chan->pw_pixrect,&bitmask);
	pr_putattributes(barch->pw_pixrect,&bitmask);
	pr_putattributes(&pixels,&bitmask);
#endif

/*	Set up a 2 bit color map for use by the status lights alone.	*/

	bitmask	= 0x0002;
#ifdef NOTDEF
	pr_putattributes(litch1->pw_pixrect,&bitmask);
	pr_putattributes(litch2->pw_pixrect,&bitmask);
#endif
	pw_setcmsname(chan,"vistcolor");	/* Name the color map	*/
	pw_setcmsname(barch,"vistcolor");	/* segements.		*/
	pw_setcmsname(litch1,"light");
	pw_setcmsname(litch2,"light");
	pw_setcmsname(litch3,"light");
	pw_setcmsname(litch4,"light");

/*	Tell the SUN that the routine "change" can be called to fix up
	any damage that occurs to the base frame windows.		*/

	window_set(screen,
			CANVAS_REPAINT_PROC, 	change,
			0);

/*	Everything is set up. Display the base frame on the SUN!
	Call notify_do_dispatch to tell the SUN window manager to fix
	up any damage to the window when VISTA has IO pending.  This
	allows VISTA to execute.  Normaly at this point the window
	manager itself would want to handle all process control		*/

	window_set(base, WIN_SHOW, TRUE, 0);
	notify_do_dispatch();
#ifdef NOTDEF
	local.r_left	= 0;
	local.r_top	= 0;
	local.r_width	= NCTV;
	local.r_height	= NRTV;
	pw_preparesurface(chan,&local);
	local.r_width	= 300;
	local.r_height	= 40;
	pw_preparesurface(barch,&local);
	local.r_width	= 40;
	local.r_height	= 17;
	pw_preparesurface(litch1,&local);
	local.r_width	= 40;
	local.r_height	= 18;
	pw_preparesurface(litch2,&local);
#endif

	tvopenc_();

/*      Make sure all the pointers to the video memory are nulled out   */
/*       to start                                                       */
	initc();

/*	Open up a pipe to allow for asynchronus communication between
	VISTA and its interupt driven character handlers		*/

	if (pipe(pipefd))
		printf ("Could not open pipe...\n");
	else {
		from_screen	= pipefd[0];
		to_vista	= pipefd[1];
		waiting_for_c	= 0;
	}

/*	The SUN window manager seems to generate alarms it can't service.
	I avoid letting this crash VISTA by catching the alarm myself.
	(and then do nothing about it)					*/

	signal(SIGALRM, donothing());
	
	return (0);	/* Channel is open			*/
}

donothing()

{
	return(0);
}

/* -------------------------------------------------------------------- */

/*	This is the screen handler called by the SUN window manager when
	damage is done to the screen.  It calls tvzoom to rewrite the
	image buffer to the screen.					*/

change(canvasdum,pixdum,areadum)

Canvas canvasdum;
Pixwin *pixdum;
Rectlist areadum;

{
	extern struct center last;
	extern struct pixwin *chan;	/* Channel to SUN		*/

	if (last.zcn != 0) {
		pw_damaged(chan);	/* Prepare screen for fixup	*/
		tvzoom_(&last.zcn,&last.xcn,&last.ycn,1);
	}
}

/* -------------------------------------------------------------------- */

/*	This routine simply closes the channel to the SUN.  It is not used
	at the present time.						*/

tvclose_()

{
	extern struct pixwin *chan;	/* Channel to SUN		*/

	pw_close(chan);
}

/* -------------------------------------------------------------------- */

/*	Routine to load floating images into video memory.  This routine
	subtracts off a zero, and converts data to BYTE modulo a span.
	This is routine is called by VISTA to prepare an image buffer
	for display.  The packed byte array that results is held in a
	buffer and is actually written to the screen by tvzoom.		*/

tvfload_(a,nrow,ncol,nc,sr,sc,asr,asc,span,zero,flip,erase)

float 	*a,		/* Input floating image array			*/
	*span,		/* Intensity mapping span level.  This level	*/
			/* is used to scale the pixel data between 0	*/
			/* and 254 for display on the Grinnell.		*/
	*zero;		/* Intensity scale zero offest			*/

int 	*nrow,		/* Number of rows to be displayed		*/
	*ncol,		/* Number of columns to be displayed		*/
	*nc,		/* Row length in pixels of the array		*/
	*sr,		/* Row offset to first displayed row		*/
	*sc,		/* Column offset to first displayed row		*/
	*asr,		/* Row number of image array origin		*/
	*asc,		/* Column number of image array origin		*/
	*flip,		/* Flag for up-down image reflection		*/
	*erase;		/* Flag for erasing old image			*/

{
	extern struct pixwin *chan;	/* Channel to SUN		*/
	extern Frame base;
	extern struct center last;
	extern struct box *raster, rasters[MAX_IM_HOLD];
	extern char store[NRTVH*NCTVH*MAX_IM_HOLD];/* Image frame buffe	*/
	extern int tv_cnt, tx_cnt, n_raster, imtvnum_ /*,im_tv */;
	register char *g;
	register float *pix,		/* Pointer to within array	*/
			lev;
	int jump, npix, ipart, nr, i;
	int z, x, y, tvzoom_(), tvzoomc_(), ef;
	float val;
	double junk;

/*	Initialize variables						*/

	lights(-2);
	if (*flip==1) {
          pix   = a + *sc + ( *sr + *nrow - 1) * *nc;
	  jump	=  0 - *nc - *ncol;     /* Jump between rows		*/
	  yup = 1;
	}
	else {
	  pix	= a + *sc + *sr * *nc;	/* Pointer to first pixel	*/
	  jump	= *nc - *ncol;		/* Jump between rows		*/
	  yup = 0;
	}
	raster++;
	if (raster >= rasters + MAX_IM_HOLD)
		raster	= rasters;
	raster->nrs	= *nrow;
	raster->ncs	= *ncol;
	raster->srs	= *sr + *asr;
	raster->scs	= *sc + *asc;
	raster->roworg	= *asr;
	raster->colorg	= *asc;
	raster->ncim	= *nc;
/*	raster->bufno   = im_tv;  */
	raster->bufno	= imtvnum_;   
	raster->loctv	= a;
	raster->byteloc	= store + NRTVH * NCTVH * (raster - rasters);

/*	Scale each row of pixels between 0 and 254 by using the span	*/
/*	and zero level.  255 is reserved for graphics.			*/

	if (*span > 0.0) {
	g	= raster->byteloc;
	lev	= 128.0 / *span;
	for (nr = *nrow; nr > 0; nr--) {
		for (npix = *ncol; npix > 0; npix--) {
			val	= lev * ( *pix++  - *zero );
			if (val < 0.0)
				val	=0.0;
			ipart	= 0x0000007f & (int) val;
			if (ipart == 127)
				ipart	= 126;
			*g++	= ipart;
		}
		pix	+= jump;
	}
	}

/*	This block of code just clips pixels instead of letting them wrap */

	else {
	lev	= 127.0 / -*span;
	g	= raster->byteloc;
	for (nr = *nrow; nr > 0; nr--) {
		for (npix = *ncol; npix > 0; npix--) {
			val	= ( *pix++  - *zero ) * lev;
			if (val < 0.0)
				val	=0.0;
			else if (val > 126.0)
				val	= 126.0;
			*g++	= val;
		}
		pix	+= jump;
	}
	}

/*	Call tvzoom to actually display the image.  Calculate the zoom
	factor.  For images larger than the display use a negative zoom
	factor.								*/

	if (*nrow > *ncol) {
		if (*nrow <= NRTV)
			z	= NRTV / *nrow;
		else
			z	= - *nrow / NRTV;
	} else {
		if (*ncol <= NCTV)
			z	= NCTV / *ncol;
		else
			z	= - *ncol / NCTV;
	}

	if (z > 0) {
		x	= NCTV / 2;
		y	= NRTV / 2;
	} else {
		x	= (1 - z) * NCTV / 2;
		y	= (1 - z) * NRTV / 2;
	}

	if (z >= 16)
		z	= 16;
	else if (z >=8)
		z	= 8;
	else if (z >=4)
		z	= 4;
	else if (z ==3)
		z	= 2;

	last.zcn	= z;
	last.xcn	= x;
	last.ycn	= y;
	last.zi		= z;
	tv_cnt	= 0;		/* Zero vector count	*/
	tx_cnt	= 0;		/* Zero text count	*/
	if (*erase)
		ef	= 1;
	else
		ef	= 0;
	pw_damaged(chan);
	pw_exposed(chan);
	tvzoom_(&z,&x,&y,ef);
	if (z < 0)
		z	= 1;

	x	= NCTV / 2;
	y	= NRTV / 2;
	tvzoomc_(&z,&x,&y);
	lights(-1);
	lights(2);
	lights(3);
	return (0);
}

/* -------------------------------------------------------------------- */

/*	Routine to load the SUN color lookup table			*/

tvcolorld_(r,g,b,n,ad)

short	*r,				/* Pointer to red array		*/
	*g,				/* Pointer to green array	*/
	*b;				/* Pointer to blue array	*/

int	*n,				/* Number of table entries	*/
	*ad;				/* First entry adress		*/

{
	extern struct pixwin *chan, *barch, *chanc; /* SUN channel	*/
	extern union pixline line;	/* Output buffer		*/
	register char *gp;
	char *red, *green, *blue;
	int i;

	if ( *n < 0 || *ad < 0 || *n + *ad >256 )
		return (-1);		/* Illegal address		*/
	gp	= line.gbuf;

/*	Load the color maps into the buffer after converting to BYTE	*/

	red	= gp;
	*r++;
	for (i=0; i < *n ; i += 2) {
		*gp++	= ( *r++ & 0x00ff);
		*r++;
	}

	green	= gp;
	*g++;
	for (i=0; i < *n ; i += 2) {
		*gp++	= ( *g++ & 0x00ff);
		*g++;
	}

	blue	= gp;
	*b++;
	for (i=0; i < *n ; i += 2) {
		*gp++	= ( *b++ & 0x00ff);
		*b++;
	}

	i	= *n/2;
	if (pw_putcolormap(chan, *ad, i, red, green, blue))
		return (-1);

	if (pw_putcolormap(chanc, *ad, i, red, green, blue))
		return (-1);

	if (pw_putcolormap(barch, *ad, i, red, green, blue))
		return (-1);
	else
		return (0);
}

/* --------------------------------------------------------------------- */

/*	Routine to zoom in or out of SUN images.  This routine assumes that
	an unzoomed image has been stored in the frame buffer centerred
	in a virtual frame buffer.  The zoom function isolates the area
	to be displayed and calculates the true screen coordinates with
	clipping for display.  The image is then reloaded.		*/

/*	Displays larger than the screen buffer are displayed by showing
	every N columns and rows.					*/

tvzoom_(z,x,y,erase)

int	*z,				/* Zoom factor			*/
	*x,				/* X center			*/
	*y;				/* Y center			*/

int	erase;			

{
	extern Canvas screen;
	extern struct box *raster;
	extern struct pixwin 	*chan;
	extern struct pixrect 	pixels;
	extern struct rect	tvrect;
	extern union pixline	line;
	extern char store[NRTVH*NCTVH*MAX_IM_HOLD];
	extern struct center last;
	char label[7], *text;
	Event input;
	caddr_t noargs;
	register char *g, *s;
	float valm, *locpix;
	int 	ixlo, ixloz, ixhiz,
		iylo, iyloz, iyhiz,
		ncz, nrz, nc, xs, ys,
		col2, row2, mid,
		ncex, nrex,
		ictvz, irtvz, ix, iy,
		comp, zl, val,
		i, j, k, jump, op;


	if ( *z > 64 )
		return (-1);		/* Zoom error			*/
	else if ( *z > 0) {
		zl	= *z;
		comp	= 1;
	} else {
		zl	= - *z;
		comp	= 1 - *z;
	}

	if ( *z == last.zi)
		lights(4);
	else
		lights(-4);

	ixlo	= comp * NCTV / 2 - raster->ncs / 2;
	iylo	= comp * NRTV / 2 - raster->nrs / 2;
	ncz	= comp * NCTV / zl;	/* Maximum columns displayible	*/
	nrz	= comp * NRTV / zl;	/* Maximum rows			*/
	col2	= zl * ncz / 2;
	row2	= zl * nrz / 2;
	ixloz	= comp * NCTV / 2 - col2;  /* Box if display filled	*/
	iyloz	= comp * NRTV / 2 - row2;
	ixhiz	= zl * ncz + ixloz - 1;
	iyhiz	= zl * nrz + iyloz - 1;
	mid	= nrz - 2 * (nrz / 2);

/*	The following section of code is to determine which portion
	of the entire image held in memory to write to the screen, given
	a zoom factor and central pixel coordinate.			*/

	if (*x - ixlo < ncz /2) {	/* Clip left edge		*/
		ncex	= ncz / 2 - *x + ixlo;
		ictvz	= raster->scs;
	} else {
		ncex	= 0;
		ictvz	= raster->scs + *x -ixlo - ncz / 2;
	}
	ixloz	= ixloz + zl * ncex;

	if (ncz /2 + *x - ixlo + mid > raster->ncs) /* Clip right edge	*/
		ncex	= ncz / 2 + *x - ixlo - raster->ncs + mid;
	else
		ncex	= 0;

	ixhiz	= ixhiz - zl * ncex;

	if (*y - iylo < nrz / 2) {		/* Clip top edge	*/
		nrex	= nrz / 2 - *y + iylo;
		irtvz	= raster->srs;
	} else {
		nrex	= 0;
		irtvz	= raster->srs + *y - iylo - nrz / 2;
	}
	iyloz	= iyloz + zl * nrex;

	if (nrz / 2 + *y -iylo + mid > raster->nrs) /* Clip bottom edge	*/
		nrex	= nrz / 2 + *y - iylo - raster->nrs + mid;
	else
		nrex	= 0;

	iyhiz	= iyhiz - zl * nrex;

	nrz	= (iyhiz - iyloz + 1)/ zl;	/* Rows in zoomed image	*/
	ncz	= (ixhiz - ixloz + 1)/ zl;	/* Cols in zoomed image	*/

/*	Now display the zoomed image.  Zoom by repeating pixels		*/

	op	= PIX_SRC;  /* Display operation	*/
	s = raster->byteloc + (ictvz - raster->scs) + raster->ncs *
	    (irtvz - raster->srs); 
	jump	= raster->ncs - ncz;
	xs	= ixloz / comp;	/* Corner screen positions	*/
	ys	= iyloz / comp;
	nc	= zl * ncz / comp;
	pw_lock(chan,&tvrect);	/* Lock the SUN video board	*/
	raster->xlotv	= ixloz;
	raster->xhitv	= ixhiz;
	raster->ylotv	= iyloz;
	raster->yhitv	= iyhiz;
	raster->srtv	= irtvz;
	raster->sctv	= ictvz;

/*	Erase the screen before writting				*/

	if (erase)
		pw_writebackground(chan,0,0,NCTV+5,NRTV,op);

/*	This is a software zoom.  Pixels are sent out one row at a time.
	Pixels are repeated in the horizontal direction here.  The SUN
	pw_rop routine will repeat the pixels vertically.		*/

/*	Do the first set if the display is smaller than the image.	*/

	if (*z <= 0) {
	for (i = 0; i < nrz; i += comp) {
		g	= line.gbuf;
		for (j = 0; j < ncz; j += comp) {
			*g++	= *s;
			s	+= comp;
		}
		pw_rop(chan, xs, ys++, nc, 1, op, &pixels, 0, 0);
		s	+= jump + (comp - 1) * raster->ncs
			- ncz + comp * (ncz / comp);

/*	For large images, the display lock times out before the display
	is complete.  Reset the lock here.  This is an adhoc fix.	*/

		if ((i == nrz/2) && (nrz * ncz > NRTV * NCTV / 2)) {
			pw_unlock(chan);
			pw_lock(chan,&tvrect);	/* Lock SUN video board	*/
		}
	}

#ifdef NOTDEF
	if (*z <= 0) {
	for (i = 0; i < nrz; i++) {
		g	= line.gbuf;
		for (j = 0; j < ncz; j++)
			*g++	= *s++;
		pw_rop(chan, xs, ys++, nc, 1, op, &pixels, 0, 0);
		s	+= jump;

/*	For large images, the display lock times out before the display
	is complete.  Reset the lock here.  This is an adhoc fix.	*/

		if ((i == nrz/2) && (nrz * ncz > NRTV * NCTV / 2)) {
			pw_unlock(chan);
			pw_lock(chan,&tvrect);	/* Lock SUN video board	*/
		}
	}
#endif

	} else {


	for (i = 0; i < nrz; i++) {
		g	= line.gbuf;
		for (j = 0; j < ncz; j++) {
			for (k = 0; k < *z; k++)
				*g++	= *s;
			s++;
		}
		for (k = 0; k < *z; k++)
			pw_rop(chan, xs, ys++, nc, 1, op, &pixels, 0, 0);
		s	+= jump;

/*	For large images, the display lock times out before the display
	is complete.  Reset the lock here.  This is an adhoc fix.	*/

		if ((i == nrz/2) && (nrz * ncz > NRTV * NCTV / 2)) {
			pw_unlock(chan);
			pw_lock(chan,&tvrect);	/* Lock SUN video board	*/
		}
	}
	}

/*	If the zoom factor is very large, then write out the pixel numbers
	on the pixels themselves.					*/

#define ABS(x) ((x) > 0 ? (x) : -(x))

	if (*z >= 64) {
		ys	= iyloz;
		for (i = 0; i < nrz; i++) {
			xs	= ixloz;
			k	= 0;
			for (j = 0; j < ncz; j++) {
				ix      = j + raster->sctv;
				if (yup ==1)
			  		iy    = ncz - i - 1 + raster->srtv;
				else
		    			iy    = i + raster->srtv;

				locpix  = raster->loctv + raster->ncim *
					    (iy - raster->roworg) + ix
					    - raster->colorg;

				if (raster->loctv == NULL)
					valm    = 0;
			    	else
			    		valm    = *locpix;

				text	= label;
				if (ABS(val) >= 1000.0) {
					val     = valm + 0.5;
					sprintf (text,"%6d\0", val);
				} else if (ABS(valm) >= 100.0) {
					sprintf (text,"%6.1f\0", valm);
				} else if (ABS(valm) >= 10.0) {
					sprintf (text,"%6.2f\0", valm);
				} else if (ABS(valm) >= 1.0) {
					sprintf (text,"%6.3f\0", valm);
				} else {
					sprintf (text,"%6.3f\0", valm);
				}

				pw_ttext(chan, xs + 8 , ys + 24 + k,
					PIX_COLOR(127)|PIX_SRC, NULL, text);
				xs	+= *z;
				k	= 20 - k;
			}
			ys	+= *z;
		}
	}

/*	The image has been displayed, now redisplay the colorbar.	*/

	pw_damaged(barch);	/* Fix both damaged and exposed regions	*/
	pw_exposed(barch);
	g	= line.gbuf;
	*g++	= 127;
	*g++	= 127;
	for (i=0 ; i < 128; i++) {
		*g++	= i;
		*g++	= i;
	}
	for (i=0 ; i < 32; i++)
		pw_rop(barch, 5, 4+i , 258, 1, PIX_SRC, &pixels, 0, 0);

/*	Complete the fixup by playing back any graphics drawn over the
	screen.								*/

	sunvecplay();

/*	Unlock the sun video board, and let the SUN know that the window
	damage has been repaired.					*/

	pw_unlock(chan);
	pw_donedamaged(chan);
	pw_donedamaged(barch);
	return (0);
}

tvblink_(a,b,nrow,ncol,nc,sr,sc,asr,asc,span,zero,flip,ibl)

float 	*a,		/* Input floating image array			*/
        *b,             /* second floating array                        */
	*span,		/* Intensity mapping span level.  This level	*/
			/* is used to scale the pixel data between 0	*/
			/* and 254 for display on the Grinnell.		*/
	*zero;		/* Intensity scale zero offest			*/

int 	*nrow,		/* Number of rows to be displayed		*/
	*ncol,		/* Number of columns to be displayed		*/
	*nc,		/* Row length in pixels of the array		*/
	*sr,		/* Row offset to first displayed row		*/
	*sc,		/* Column offset to first displayed row		*/
	*asr,		/* Row number of image array origin		*/
	*asc,		/* Column number of image array origin		*/
	*flip,		/* Flag for up-down image reflection		*/
        *ibl;           /* Number of grey levels per image              */

{
	extern struct pixwin *chan;	/* Channel to SUN		*/
	extern Frame base;
	extern struct center last;
	extern struct box *raster, rasters[MAX_IM_HOLD];
	extern char store[NRTVH*NCTVH*MAX_IM_HOLD];/* Image frame buffer	*/
	extern int tv_cnt, tx_cnt, n_raster, imtvnum_ /*im_tv*/;
	extern union pixline line;	/* Output buffer		*/
	char *red, *green, *blue;
	int i, j, ii, iblink, ntoget, row, col;
        char *table, ch;
	register char *g;
	register float *pix,*pix2,	/* Pointer to within array	*/
			lev;
	int jump, npix, ipart, nr, iif;
	int z, x, y, tvzoom_(), tvzoomc_();
	float val,val2;
	double junk;

/*	Initialize variables						*/

	lights(-2);
	if (*flip==1) {
          pix   = a + *sc + ( *sr + *nrow - 1) * *nc;
          pix2   = b + *sc + ( *sr + *nrow - 1) * *nc;
	  jump	= 0 - *nc - *ncol;	/* Jump between rows		*/
	  yup = 1;
	}
	else {
	  pix	= a + *sc + *sr * *nc;	/* Pointer to first pixel	*/
	  pix	= b + *sc + *sr * *nc;	/* Pointer to first pixel	*/
	  jump	= *nc - *ncol;		/* Jump between rows		*/
	  yup = 0;
	}
	raster++;
	if (raster >= rasters + MAX_IM_HOLD)
	     raster = rasters;
	raster->nrs	= *nrow;
	raster->ncs	= *ncol;
	raster->srs	= *sr + *asr;
	raster->scs	= *sc + *asc;
	raster->roworg	= *asr;
	raster->colorg	= *asc;
	raster->ncim	= *nc;
/*	raster->bufno   = im_tv;  */
	raster->bufno   = imtvnum_;
	raster->loctv	= a;
	raster->byteloc = store + NRTV * NCTV * (raster - rasters);

	if (*span > 0.0) {
	g	= raster->byteloc;
	lev	= (*ibl * *ibl - 1) / *span;
	for (nr = *nrow; nr > 0; nr--) {
		for (npix = *ncol; npix > 0; npix--) {
			val	= ( *pix++  - *zero );
                        val2    = ( *pix2++ - *zero ); 
                        val = lev * (val - ((int)(val / *span) * *span));
                        val2 = lev * (val2 - ((int)(val2 / *span) * *span));
			if (val < 0.0)
				val	=0.0;
                        if (val2 < 0.0)
                                val2    = 0.0;
			ipart	= (int)((val / *ibl) + 
					(int)(val2 / *ibl) * *ibl); 
			*g++	= ipart; 
		}
		pix	+= jump; 
		pix2    += jump;
	}
	}

/*	This block of code just clips pixels instead of letting them wrap */

	else {
	iif = *ibl * *ibl - 1;
	lev	= iif / -*span;
	g	= raster->byteloc;
	for (nr = *nrow; nr > 0; nr--) {
		for (npix = *ncol; npix > 0; npix--) {
			val	= lev * ( *pix++  - *zero );
                        val2    = lev * ( *pix2++ - *zero );
			if (val < 0.0)
				val	=0.0;
			else if (val > iif)
				val	= iif;
                        if (val2 < 0.0)
                                val2    = 0.0;
                        else if (val2 > iif)
                                val2    = iif;
			*g++	= (int)((val / *ibl) + 
					(int)(val2 / *ibl)* *ibl);
		}
		pix	+= jump; 
		pix2    += jump;
	}
	}

/*	Call tvzoom to actually display the image		*/

	x	= NCTV / 2;
	y	= NRTV / 2;
	if (*nrow > *ncol)
		z	= NRTV / *nrow;
	else
		z	= NCTV / *ncol;

	if (z >= 16)
		z	= 16;
	else if (z >=8)
		z	= 8;
	else if (z >=4)
		z	= 4;
	else if (z ==3)
		z	= 2;

	last.zcn	= z;
	last.xcn	= x;
	last.ycn	= y;
	last.zi		= z;
	tv_cnt	= 0;		/* Zero vector count	*/
	tx_cnt  = 0;
	pw_damaged(chan);
	pw_exposed(chan);
	tvzoom_(&z,&x,&y,1);
	tvzoomc_(&z,&x,&y);
	lights(-1);
	lights(2);
	lights(3);

/*	Load the SUN blink tables			*/

	iblink = 0;
	ch = ' ';
	table = line.gbuf;
	printf("In TV window, enter E or Q to quit, anything else \
to blink:\n");
	ntoget = 1;
	while (ch != 'E' && ch != 'e' && ch != 'Q' && ch != 'q') {
	  if (iblink == 0) {
	    g	= line.gbuf;
            for (i=0; i<*ibl; i++) {
              for (j=0; j<*ibl; j++) {
                *g++ = j * 255. / *ibl;
              }
            }
	    if (pw_putcolormap(chan,0,*ibl**ibl,table,table,table))
		return(-1);
	    iblink = 1;
	    raster->loctv	= a;
	    mark(&row,&col,&ch);
	  }
	  else {
	    g	= line.gbuf;
            for (i=0; i<*ibl; i++) {
              for (j=0; j<*ibl; j++) {
                *g++ = i * 255. / *ibl;
              }
            }
	    if (pw_putcolormap(chan,0,*ibl**ibl,table,table,table))
		return(-1);
	    iblink = 0;
	    raster->loctv	= b;
	    mark(&row,&col,&ch);
	  }
        }

	return(0);

}
