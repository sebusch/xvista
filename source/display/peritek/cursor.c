/*
Implement Peritek cursors
JJH, CIT  1986
Version 1.5b -- JJH, CIT, 14 August 1986
	Add additional cursors
	Turn off typeahead in curkey
	Modify raster<=>cursor conversions for screen origin and zoom
Version 1.5c -- 8 Sept. 1986
   	Add overlay cursor marking
   	Fix color problem with 'P'
Version 1.6 -- 27 Sept. 1986
	Add floating point coordinate conversions
	Modify setcursor to avoid colliding cursors
	Add cursors 6 and 7 for use with drawing figures
	Add +,- commands to curkey()
Version 1.7 -- 25 Oct. 1986
   	Add cursor log file to curkey()
Version 1.8 -- 22 Jan 1987
	Fix bug with assignment of _pz_rny when zoomed.
	Add FORTRAN calls.
	Change mark from M to .
	Add nonstandard curkey definitions in subroutines.
*/

#include "fortrancompat"
#include stdio
#include iodef
#include ssdef
#include "peritek"

#define SIZE 		5		/* Square cursor size 		*/
#define MAXPOINTS	16		/* Maximum number of points set */
#define NCUR		8		/* Number of cursors implemented*/

/* Cursor colors */

#define BLACK		0
#define WHITE		1
#define BLINKING	2

char cmaps[NCUR*SIZE*SIZE] = {
	0,	0,	1,	0,	0,	/* This is cursor 0 */
	0,	0,	1,	0,	0,
	1,	1,	1,	1,	1,
	0,	0,	1,	0,	0,
	0,	0,	1,	0,	0,

	0,	0,	1,	0,	0,	/* This is cursor 1 */
	0,	1,	0,	1,	0,
	1,	0,	0,	0,	1,
	0,	1,	0,	1,	0,
	0,	0,	1,	0,	0,

	1,	1,	1,	1,	1,	/* This is cursor 2 */
	1,	0,	0,	0,	1,
	1,	0,	0,	0,	1,
	1,	0,	0,	0,	1,
	1,	1,	1,	1,	1,

	1,	0,	0,	0,	1,	/* This is cursor 3 */
	0,	1,	0,	1,	0,
	0,	0,	1,	0,	0,
	0,	1,	0,	1,	0,
	1,	0,	0,	0,	1,

	1,	1,	1,	1,	1,	/* This is cursor 4 */
	0,	1,	0,	0,	0,	/* "ZOOM" cursor    */
	0,	0,	1,	0,	0,
	0,	0,	0,	1,	0,
	1,	1,	1,	1,	1,

	1,	0,	0,	0,	0,	/* This is cursor 5 */
	1,	0,	0,	0,	0,	/* "PAN" cursor     */
	1,	1,	1,	1,	0,
	1,	0,	0,	0,	1,
	1,	1,	1,	1,	0,

	0,	1,	1,	1,	0,	/* This is cursor 6 */
	1,	0,	0,	0,	1,
	1,	0,	1,	0,	1,
	1,	0,	0,	0,	0,
	0,	1,	1,	1,	0,

	0,	1,	1,	1,	0,	/* This is cursor 7 */
	1,	0,	0,	0,	1,
	1,	0,	1,	0,	1,
	1,	0,	0,	0,	0,
	0,	1,	1,	1,	0
	};

struct {		/* Structure to hold cursor information		*/
   int on;		/* 1 if cursor is currently turned on		*/
   int xc, yc;		/* Central x, y of cursor on screen		*/
   int xs, ys;		/* Starting x, y of cursor display		*/
   int is, js;		/* Starting i, j of cur display in cmap		*/
   int in, jn;		/* Size of cursor display.  Xs-jn correct edges */
   char m[SIZE][SIZE];	/* Display values overwritten by the cursor	*/
   char *(cmap[SIZE]);	/* Map of the cursor.  See cmaps above.		*/
   struct pz_point points[MAXPOINTS];	/* Locations modifid by	cursor	*/
   int npts;		/* The number of points actually modified	*/
   int blinking;	/* If 1, cursor is currently blinking		*/
   int color;		/* Current color of the cursor			*/
   } pzcur[NCUR];

int _blink_color;	/* The current "color" of blinking cursors	*/
int timer=1;		/* Timer number for blinking cursor		*/
double deltatime;	/* Period of blinking				*/


/* Parameters for conversion of raster, screen coordinates		*/

int _pz_rx0=0, _pz_ry0=0;	/* Display Origin in raster 	*/
int _pz_sx0=0, _pz_sy0=0;	/* Display origin on screen	*/
int _pz_blocking = 1;		/* Blocking of the display 	*/
int _pz_zoom = 1;		/* Zoom factor (for blocking<0) */
int _pz_rnx=0, _pz_rny=0;	/* Raster cols, rows displayed	*/


/************************************************************************/
/* _CUR_SETUP()  -- Initialize cursor structure.  Called by Peritek	*/
/* initialization routine						*/
/************************************************************************/

_cur_setup()
{
int j, i, c;
int status;

SYS$BINTIM(strdes("0000 00:00:00.50"), &deltatime);

for(c=0; c<NCUR; c++) {
   pzcur[c].on=0;
   for(j=0; j<SIZE; j++) {
      pzcur[c].cmap[j] = cmaps + c*SIZE*SIZE + j*SIZE;
      }
/*
   pzcur[c].xc=pzcur[c].yc= 50+c*20;
*/
   pzcur[c].xc=pzcur[c].yc= 256;
   pzcur[c].blinking = 0;
   pzcur[c].color=BLINKING;
   }
}


/************************************************************************/
/* SETCURSOR() -- High level routine for use by outside world to	*/
/* control software cursors.						*/
/* n		Which cursor to affect					*/
/* sel		1 => turn cursor on;  0 => turn cursor off; <0 => same	*/
/* color	Select cursor color.  See #defines above		*/
/* x,y		Screen coordinates for the cursor			*/
/************************************************************************/

setcursor(n,sel,color,x,y)
int n, sel, color, x, y;
{
int rewrite[NCUR], i;

if(color<0) color=pzcur[n].color;
blinkoff();			/* Turn blinking off */
if(n<0 || n>=NCUR) {
   blinkon();
   return(-1);
   }
if(pzcur[n].on) pzc_off(n);	/* Begin by turning selected cursor off */
pzcur[n].on=0;
pzcur[n].blinking=0;
if(!sel) {			/* If turn off, get out now */
   blinkon();
   return(0);
   }

for(i=0; i<NCUR; i++) {		/* Turn off colliding cursors	*/
   if(i==n) {
      rewrite[i]=0;
      continue;
      }
   if(!pzcur[i].on) {
      rewrite[i]=0;
      continue;
      }
   if(abs(x-pzcur[i].xc)<SIZE && abs(y-pzcur[i].yc)<SIZE) {
      pzc_off(i);
      rewrite[i]=1;
      }
   else rewrite[i]=0;
   }

pzcur[n].on=1;
if(color>=0) pzcur[n].color=color;
if(pzcur[n].color==BLINKING) pzcur[n].blinking=1;
pzc_on(n,pzcur[n].color,x,y);

for(i=0; i<NCUR; i++) {	    /* Redraw collided cursors			*/
   if(rewrite[i]) pzpoints(pzcur[n].points,pzcur[n].npts);
   }

blinkon();		    /* Make sure cursor blink routine is active */
return(0);
}


/************************************************************************/
/* PZC_OFF() -- OVERWRITE THE CURSOR WITH DATA				*/
/************************************************************************/

pzc_off(n)
int n;
{
struct iostat ret;
struct shortint ufnc;
int start, j, is, js, in, jn;
int status;


/* Write data to the display */

ufnc.x = IO$_WRITEVBLK;
is = pzcur[n].is;
js = pzcur[n].js;
in = pzcur[n].in;
jn = pzcur[n].jn;

for(j=0; j<jn; j++) {
   start = (pzcur[n].ys+j) | (pzcur[n].xs<<16);
   status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,
   	&(pzcur[n].m[js+j][is]),in,start,0,0,0);
   if(status!=SS$_NORMAL) {
      return(-1);
      }
   }
return(0);
}


/************************************************************************/
/* PZC_ON() -- SET UP THE CURSOR AT X,Y, SAVE THE DATA FROM THE SCREEN,	*/
/* AND WRITE THE CURSOR							*/
/************************************************************************/

pzc_on(n,color,x,y)
int n, color, x, y;
{
struct iostat ret;
struct shortint ufnc;
int start, i, j, is, js, in, jn, xs, ys, ip;
unsigned char clr;
int status;
static struct pz_point *points;

pzcur[n].xc=x;
pzcur[n].yc=y;

/* Set up cursor display parameters (where cursor begins, ends) */

if(x<SIZE/2) {
   pzcur[n].xs=0;
   pzcur[n].is=SIZE/2-x;
   pzcur[n].in=SIZE-pzcur[n].is;
   }
else if (x>=512-SIZE/2) {
   pzcur[n].xs=x-SIZE/2;
   pzcur[n].is=0;
   pzcur[n].in=SIZE/2+512-x;
   }
else {
   pzcur[n].xs=x-SIZE/2;
   pzcur[n].is=0;
   pzcur[n].in=SIZE;
   }
if(y<SIZE/2) {
   pzcur[n].ys=0;
   pzcur[n].js=SIZE/2-y;
   pzcur[n].jn=SIZE-pzcur[n].js;
   }
else if (y>=512-SIZE/2) {
   pzcur[n].ys=y-SIZE/2;
   pzcur[n].js=0;
   pzcur[n].jn=SIZE/2+512-y;
   }
else {
   pzcur[n].ys=y-SIZE/2;
   pzcur[n].js=0;
   pzcur[n].jn=SIZE;
   }
   
ufnc.x = IO$_READLBLK;

is = pzcur[n].is;
js = pzcur[n].js;
in = pzcur[n].in;
jn = pzcur[n].jn;
xs = pzcur[n].xs;
ys = pzcur[n].ys;

/* Read the data from the screen and save it so that it can later	*/
/* replace the cursor							*/

for(j=0; j<jn; j++) {
   start = (pzcur[n].ys+j) | (pzcur[n].xs<<16);
   status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,
   	&(pzcur[n].m[js+j][is]),in,start,0,0,0);
   if(status!=SS$_NORMAL) {
      return(-1);
      }
   }

/* Set up a structure containing the points to be modified when the	*/
/* cursor is written							*/

ip=0;
points = pzcur[n].points;
for(j=0; j<jn; j++) {
   for(i=0; i<in; i++) {
      if(pzcur[n].cmap[js+j][is+i]) {
	 points[ip].x=xs+i;
	 points[ip].y=ys+j;
	 points[ip].mask=0xff;
	 ip++;
         }
      }
   }
pzcur[n].npts = ip;

/* Either flag the cursor as blinking or write it to display directly	*/

if (color==BLINKING) {		/* Blinking cursor */
   pzcur[n].blinking=1;
   color=_blink_color;
   }
else writecursor(n, color);
}


/************************************************************************/
/* WRITECURSOR() -- ACTUALLY WRITE CURSOR DATA TO THE DISPLAY		*/
/************************************************************************/

writecursor(n,color)
int n, color;
{
int i;
unsigned char clr;

if(color) clr=0x80;
else clr=0;

for(i=0; i<pzcur[n].npts; i++) pzcur[n].points[i].z = clr;

pzpoints(pzcur[n].points,pzcur[n].npts);
}


/************************************************************************/
/* BLINKCUR() -- IF ANY CURSORS ARE BLINKING, KEEP THEM SO		*/
/************************************************************************/

blinkcur()
{
int i, some_on;

_blink_color = !_blink_color;
some_on=0;
for(i=0; i<NCUR; i++) {
   if(pzcur[i].blinking) {
      some_on=1;
      writecursor(i, _blink_color);
      }
   }

if(!some_on) return;

SYS$SETIMR(0, &deltatime, blinkcur, timer);
}

/************************************************************************/
/* BLINKOFF() -- STOP THE TIMER DRIVING BLINKING CURSORS		*/
/************************************************************************/

blinkoff()
{
SYS$CANTIM(timer,0);
}

/************************************************************************/
/* BLINKON() -- TURN BLINKING CURSORS BACK ON				*/
/************************************************************************/

blinkon()
{
blinkcur();
}


/************************************************************************/
/* PZ_CUR_MARK() -- Mark the position of cursor n on the overlay	*/
/************************************************************************/

pz_cur_mark(n)
int n;
{
int i, j, js, is, jn, in;

if(n>=NCUR) return(-1);

in=pzcur[n].in;
jn=pzcur[n].jn;
is=pzcur[n].is;
js=pzcur[n].js;

for(j=0; j<jn; j++) {
   for(i=0; i<in; i++) {
      if(pzcur[n].cmap[js+j][is+i]) pzcur[n].m[js+j][is+i] |= 0x80;
      }
   }
return(0);
}



/************************************************************************/
/* CURKEY() -- ROUTINE TO ALLOW CONTROL OF THE CURSORS FROM THE KEY PAD	*/
/* Cur_morekeys() and cur_morehelp() allow the user to add cursor keys	*/
/* without modifying this code.						*/
/************************************************************************/

cur_noop()
{
return(0);
}

int (*cur_morekeys)() = cur_noop;
int (*cur_morehelp)() = cur_noop;

curkey(n, lastx, lasty, aflag, xsize, ysize, array, curlog)
int n;			/* Initial cursor to manipulate			*/
int *lastx, *lasty;	/* Final cursor x, y values in raster		*/
int aflag;		/* If 1, then the input array is available	*/
int xsize, ysize;	/* The size of the unbinned, unblocked array	*/
short int **array;	/* The UNBINNED input array, optional		*/
FILE *curlog;		/* If >0, marked cursors logged here		*/
{
char c;
int step=1, status, temp;
int xs, ys, rx, ry, off;
int color, i;
struct iostat ret;
struct shortint chan, ufnc;
int relx, rely;
double drx, dry;

printf("Keypad control of cursors enabled.\n");
printf("Hit '?' for instructions.\n");

setcursor(n, 1, pzcur[n].color, pzcur[n].xc, pzcur[n].yc); 

xs = pzcur[n].xc;
ys = pzcur[n].yc;

status=sys$assign(strdes("SYS$INPUT:"),&(chan.x),0,0);
if(status!=SS$_NORMAL) return(-1);

ufnc.x=IO$_READVBLK;
ufnc.x |= IO$M_NOECHO;		/* Don't echo the input 	*/
ufnc.x |= IO$M_NOFILTR;		/* Don't filter the input 	*/
ufnc.x |= IO$M_PURGE;		/* Purge the typeahead buffer	*/

while (1) {
   status=sys$qiow(0,chan,ufnc,&ret,0,0,&c,1,0,0,0,0);
   if(status!=SS$_NORMAL) return(-1);
   
   switch (c) {
      case 'f' :				/* Speed up */
      case 'F' : step = 5*step > 125 ? 125 : 5*step;
		 printf("Step = %d\n",step);
      		 break;
      case 's' :				/* Slow down */
      case 'S' : step = step/5 < 1 ? 1 : step/5;
		 printf("Step = %d\n",step);
      		 break;
      case 'n' :
      case 'N' : do {
      		    askintc("Cursor? ",1,&n);
      		    if(n<0 || n>=NCUR) printf("Illegal cursor number.\n");
      		    } while(n<0 || n>=NCUR);
      		 xs = pzcur[n].xc;
      		 ys = pzcur[n].yc;
      		 setcursor(n, 1, pzcur[n].color, xs, ys);
      		 break;

/* Commented out o,c,x,and p to avoid VISTA conflicts  

      case 'o' :
      case 'O' : askintc("Cursor to blank (-1 for all)? ",1,&off);
		 if(off== -1) {
		    for(i=0; i<NCUR; i++) setcursor(i,0,0,0,0);
		    break;
		    }
      		 if(off<0 || off>=NCUR) {
		    printf("Illegal cursor number.\n");
		    break;
      		    } 
      		 setcursor(off, 0, 0, 0, 0);
      		 break;
      case 'c' :
      case 'C' : askintc("Enter cursor color (0, 1, or 2): ",1,&color);
      		 setcursor(n, 1, color, xs, ys);
      		 break;
      case 'x' :
      case 'X' : toraster(xs, ys, &rx, &ry);
		 torasterf(xs, ys, &drx, &dry);
      		 printf("Cursor number: %d  ",n);
      		 printf("Screen x, y:  %d  %d\n",xs,ys);
      		 printf("Raster x, y:  %g  %g   ",drx,dry);
		 printf("Closest pixel:  %d  %d\n",rx,ry);
   		 if(aflag) {
   		    if(rx<0 || ry<0 || rx>=xsize || ry>=ysize)
				 printf("Data = UNDEFINED.\n");
   		    else printf("Data = %d\n",array[ry][rx]);
   		    }
      		 break;
      case 'p' : 
      case 'P' : askdbl("Enter raster coordinates for cursor: ",2,&drx,&dry);
		 toscreenf(drx,dry,&xs,&ys);
		 rx = drx+0.5;	ry = dry+0.5;
      		 setcursor(n, 1, -1, xs, ys);
		 break;

      case '.' : pz_cur_mark(n);
   		 if(curlog) pz_cur_log(n,curlog);
   		 break;
  End of commenting out for VISTA, note changed '.' for cursor color  */
      case '.' : askintc("Enter cursor color (0, 1, or 2): ",1,&color);
      		 setcursor(n, 1, color, xs, ys);
      		 break;

      case '+' : if(n==NCUR-1) break;
		 n++;
		 xs=pzcur[n].xc;
		 ys=pzcur[n].yc;
		 setcursor(n,1,-1,xs,ys);
		 printf("Cursor #%d selected.\n",n);
		 break;
      case '-' : if(n==0) break;
		 n--;
		 xs=pzcur[n].xc;
		 ys=pzcur[n].yc;
		 setcursor(n,1,-1,xs,ys);
		 printf("Cursor #%d selected.\n",n);
		 break;
      case '1' : xs -= step;
		 ys -= step;
		 xs = xs<0 ? 0 : xs; 
		 ys = ys<0 ? 0 : ys; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '2' : ys -= step;
		 ys = ys<0 ? 0 : ys; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '3' : xs += step;
		 ys -= step;
		 xs = xs>511 ? 511 : xs; 
		 ys = ys<0 ? 0 : ys; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '4' : xs -= step;
		 xs = xs<0 ? 0 : xs; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '6' : xs += step;
		 xs = xs>511 ? 511 : xs; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '7' : xs -= step;
		 ys += step;
		 xs = xs<0 ? 0 : xs; 
		 ys = ys>511 ? 511 : ys; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '8' : ys += step;
		 ys = ys>511 ? 511 : ys; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '9' : xs += step;
		 ys += step;
		 xs = xs>511 ? 511 : xs; 
		 ys = ys>511 ? 511 : ys; 
		 setcursor(n, 1, -1, xs, ys);
      		 break;
      case '?' :				/* Print instructions */
      case 'h' :
      case 'H' : cur_help();
      		 break;
      case '0' :				/* Quit */
      case 'q' :
      case 'Q' : for(i=0; i<NCUR; i++) 
      			toraster(pzcur[i].xc,pzcur[i].yc,lastx+i,lasty+i);
		 for(i=0; i<NCUR; i++)
			 setcursor(i, 0, 0, 0, 0);
      		 sys$dassgn(chan);
      		 return(0);
      		 break;
      default  : toraster(pzcur[n].xc,pzcur[n].yc,lastx+n,lasty+n);
		 if((*cur_morekeys)(c,n,lastx,lasty)) {
		    sys$dassgn(chan);
		    return(0);
		    }
		 break;
      }
   }
}


cur_help()
{
printf("Interactive control of the cursors is centered around the\n");
printf("corners and sides of the numeric keypad.\n");
printf("Additional controls are:\n");
printf("'F' Faster         'S' Slower\n");
printf("'N' New cursor\n");
/* printf("'N' New cursor     'O' Turn a cursor off\n");
   printf("'C' Color select   'X' Print cursor values\n"); */
printf("'+','-' Increment, decrement cursor number\n");
/* printf("'P' Position the cursor at some x, y\n"); 
printf("'.' Mark the position of the cursor\n"); */
printf("'.' Color select\n"); 
(*cur_morehelp)();
printf("'0' or 'Q'  Quit   '?' or 'H'  Help\n");
return;
}


/************************************************************************/
/* PZ_CUR_LOG() -- WRITE COORDINATES OF MARKED CURSOR TO A LOG FILE	*/
/************************************************************************/

pz_cur_log(n,curlog)
int n;
FILE *curlog;
{
int sx, sy, curon;
double rx, ry;

if(!curlog) return;

curxy(n,&sx,&sy,&curon);
torasterf(sx,sy,&rx,&ry);

fprintf(curlog,"%d\t%.3f\t%.3f\n",n,rx,ry);
fflush(curlog);
}
 

/************************************************************************/
/************************************************************************/
/*      ROUTINES TO CONVERT FROM RASTER TO SCREEN AND VICE VERSA	*/
/************************************************************************/
/************************************************************************/


/************************************************************************/
/* DISP_DEFINE() -- ROUTINE TO SET UP SCREEN <=> RASTER COORDINATE	*/
/* TRANSFORMATIONS							*/
/************************************************************************/

disp_define(rx0, ry0, sx0, sy0, scols, srows, b)
int rx0, ry0;			/* Raster origin of the display		*/
int sx0, sy0;			/* Screen origin of the display		*/
int scols, srows;		/* Size of the display on the screen	*/
int b;				/* Blocking of display (<0 for zoom)	*/
{
if(b==0) b=1;
_pz_rx0 = rx0;
_pz_ry0 = ry0;
_pz_sx0 = sx0;
_pz_sy0 = sy0;
_pz_blocking=b;
if(b<0) {
   _pz_zoom = -b;
   _pz_rnx = _pz_zoom * scols;
   _pz_rny = _pz_zoom * srows;
   }
else {
   _pz_zoom=1;
   _pz_rnx = scols/b;
   _pz_rny = srows/b;
   }
}


/************************************************************************/
/* TOSCREEN() -- Convert raster coordinates to screen coordinates	*/
/* If zoom is even, screen coordinates are to the upper left of the apex*/
/************************************************************************/

toscreen(rx, ry, sx, sy)
int rx, ry;
int *sx, *sy;
{
if(_pz_blocking>0) {
   *sx = (rx-_pz_rx0)/_pz_blocking + _pz_sx0;
   *sy = (ry-_pz_ry0)/_pz_blocking + _pz_sy0;
   }
else {
   *sx = (rx-_pz_rx0)*_pz_zoom + _pz_zoom/2 + _pz_sx0;
   *sy = (ry-_pz_ry0)*_pz_zoom + _pz_zoom/2 + _pz_sy0;
   }
return;
}

/************************************************************************/
/* TORASTER() -- Convert screen coordinates to raster coordinates	*/
/************************************************************************/

toraster(sx, sy, rx, ry)
int sx, sy;
int *rx, *ry;
{
if(_pz_blocking>0) {
   *rx = (sx-_pz_sx0)*_pz_blocking + _pz_rx0 + (_pz_blocking-1)/2;
   *ry = (sy-_pz_sy0)*_pz_blocking + _pz_ry0 + (_pz_blocking-1)/2;
   }
else {
   *rx = (sx-_pz_sx0)/_pz_zoom + _pz_rx0;
   *ry = (sy-_pz_sy0)/_pz_zoom + _pz_ry0;
   }
}

/************************************************************************/
/* TOSCREENF() -- Convert floating point raster coordinates to nearest	*/
/* screen coordinates.							*/
/************************************************************************/

toscreenf(rx, ry, sx, sy)
double rx, ry;
int *sx, *sy;
{
double frx0, fry0, fzoom;

if(_pz_blocking>0) toscreen((int)(rx+0.5),(int)(ry+0.5),sx,sy);

else {
   fzoom = _pz_zoom;
   frx0 = _pz_rx0 - (fzoom-1.)/(2.*fzoom);
   fry0 = _pz_ry0 - (fzoom-1.)/(2.*fzoom);
   *sx = (rx-frx0)*fzoom + _pz_sx0 + 0.5;
   *sy = (ry-fry0)*fzoom + _pz_sy0 + 0.5;
   }

return;
}


/************************************************************************/
/* TORASTERF() -- Convert integer screen coordinates to floating point	*/
/* raster coordinates							*/
/************************************************************************/

torasterf(sx, sy, rx, ry)
int sx, sy;
double *rx, *ry;
{
double frx0, fry0, fzoom;

fzoom = _pz_blocking>0 ? 1./((double)_pz_blocking) : _pz_zoom;

frx0 = _pz_rx0 - (fzoom-1.)/(2.*fzoom);
fry0 = _pz_ry0 - (fzoom-1.)/(2.*fzoom);
*rx = (sx-_pz_sx0)/fzoom + frx0;
*ry = (sy-_pz_sy0)/fzoom + fry0;

return;
}


/************************************************************************/
/* CURXY() -- Get the screen position of cursor n			*/
/************************************************************************/

curxy(n, x, y, curon)
int n;
int *x, *y;
int *curon;
{
if(n<0 || n>=NCUR) return(-1);
*x=pzcur[n].xc;
*y=pzcur[n].yc;
*curon = pzcur[n].on;
return(0);
}


/************************************************************************/
/* CURSAVE() -- Save the positions of the currently displayed cursors	*/
/* then turn them all off						*/
/************************************************************************/

int _curon[NCUR], _curcolor[NCUR], _curx[NCUR], _cury[NCUR];
int _cur_is_saved=0;

cursave()
{
int i;

if(_cur_is_saved) return;

for(i=0; i<NCUR; i++) {
   if(pzcur[i].on) {
      _curon[i]=1;
      _curcolor[i]=pzcur[i].color;
      _curx[i]=pzcur[i].xc;
      _cury[i]=pzcur[i].yc;
      setcursor(i, 0, 0, 0, 0);
      }
   else {
      _curon[i]=0;
      }
   }
_cur_is_saved=1;
}

/************************************************************************/
/* CURRESET() -- Reset the cursors as they were when last saved		*/
/************************************************************************/

curreset()
{
int i;

for(i=0; i<NCUR; i++) {
   if(_curon[i]) setcursor(i, 1, _curcolor[i], _curx[i], _cury[i]);
   }
_cur_is_saved=0;
}



/*
FORTRAN INTERFACE TO CURSOR ROUTINES
*/

fsetcursor(n, sel, color, x, y)
int *n, *sel, *color, *x, *y;
{
setcursor(*n, *sel, *color, *x, *y);
}

fcurkey(n, lastx, lasty, aflag, xsize, ysize, array)
int *n, *lastx, *lasty, *aflag, *xsize, *ysize;
short int *array;
{
char *malloc();
short int **ptr;
int x, y, i, j;

if(*aflag) {
   x= *xsize;
   y= *ysize;
   ptr = malloc(4*y);

   for(j=0; j<y; j++) ptr[j] = array+j*x;
   }

curkey(*n, lastx, lasty, *aflag, *xsize, *ysize, ptr, -1);

if(*aflag) {
free(ptr);
   }
}


fcurxy(n, x, y)
int *n, *x, *y;
{
int *curon;
curxy(*n, x, y, curon);
}

fpzc_off(n)
int *n;
{
pzc_off(*n);
}

fsetcurxform(rx, ry, sx, sy, b)
int *rx, *ry, *sx, *sy, *b;
{
int sr, sc;
sr=-1;
sc=-1;
disp_define(*rx, *ry, *sx, *sy, sr, sc, *b);
}

ftoscreen(rx, ry, sx, sy)
int *rx, *ry, *sx, *sy;
{
toscreen(*rx, *ry, sx, sy);
}

ftoraster(sx, sy, rx, ry)
int *sx, *sy, *rx, *ry;
{
toraster(*sx, *sy, rx, ry);
}

