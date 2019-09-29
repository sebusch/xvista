#include "Vista.h"
/* This file includes the interface between zimage and Vista */
/*	Main routines contained here:

	tvopen_		This does all the work of setting up the complex
			window that will hold the image display.

	tvfload_	This prepares the floating images held in VISTA
			for transfer to the screen.

	tvzoom_		This writes images prepared for display to
			the screen.

	tvcolorld_	This sends out the colormap to the screen.	*/

/*	Author:	John Tonry 6/25/87	MIT				*/

#include <stdio.h>
#include <math.h>
#include "tv.h"		/* VISTA TV parameters			*/

#include <X11/Xlib.h>

int ncolors;
int is_tv_open = 0;
extern int fd_for_X;
int xtv_refresh();

ATUS(tvopen)(nrow,ncol,nclrs)
int *nrow, *ncol, *nclrs;
{
  int zf, yup, ierr, imageinit();
  short r, g, b;
  float fcoord;
  int i, vistacoord();
  int vecclear(), inst;
  char resourcename[24], windowname[24];
  int xoff,yoff;

  inst = 0;
  if (inst == 0) {
    sprintf(resourcename,"%s","xvista");
    xoff = yoff = 20;
    sprintf(windowname,"XVISTA");
  } else if (inst == 1) {
    sprintf(resourcename,"%s","ccddisp");
    xoff = 1;
    yoff = 110;
    sprintf(windowname,"1m Data Acquisition");
  } else if (inst == 2) {
    sprintf(resourcename,"%s","gccddisp");
    xoff = 275;
    yoff = 110;
    sprintf(windowname,"1m Guider Acquisition");
  } else if (inst == 3) {
    sprintf(resourcename,"%s","disslit");
    xoff = 1;
    yoff = 110;
    sprintf(windowname,"DIS Slit Viewer");
  } else {
    sprintf(resourcename,"%s","video");
    sprintf(windowname,"1m video");
    xoff = 560;
    yoff = 110;
  }

/* Open the display if first image */
  zf = 4;
  yup = 0;
  ierr = imageinit(ncol,nrow,nclrs,zf,yup,resourcename,windowname,xoff,yoff); 
  if (ierr != 0) return(ierr); 
  ncolors = *nclrs;

  UTUV(mx11register)(&fd_for_X,xtv_refresh);


/* Install '0' - '9' to update vista variables */
  if (inst == 0) {
    for(i=0;i<10;i++) imageinstallkey('0'+i,1,vistacoord);
  }

  is_tv_open = 1;

  return(0);
}

vistacoord(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  int set, i;
  float fcoord;
  set = 1;
  i = key - '0';
  fcoord = xuser;
  ATUS(subvar)("C",&i,&fcoord,&set,1);
  fcoord = yuser;
  ATUS(subvar)("R",&i,&fcoord,&set,1);
}

ATUS(tvclose)()
{
  imageclose();
}

ATUS(tverase)()
{
  imageerase();
}

ATUS(tvupdate)(x,y,nx,ny)
int *x, *y, *nx, *ny;
{
  imageupdate(*x, *y, *nx, *ny, 1);
}


#define ABS(a) (((a) > 0) ? (a) : -(a))
extern int yup;

ATUS(cctvfload)(a,nrow,ncol,nc,sr,sc,asr,asc,span,zero,flip,erase,color)
ADDRESS *a, *span, *zero;
ADDRESS *nrow, *ncol, *nc,*sr, *sc, *asr, *asc, *flip, *erase, *color;
{
  ATUS(tvfload)(*a,nrow,ncol,nc,sr,sc,asr,asc,span,zero,flip,erase,color);
  return(0);
}

ATUS(tvfload)(a,nrow,ncol,nc,sr,sc,asr,asc,span,zero,flip,erase,color)

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
	*flip,		/* Flag for left-right image reflection		*/
	*erase,         /* Flag to erase screen                         */
        *color;
{
  float c;
  int i;

  if (*flip == 1) 
    yup = 1;
  else 
    yup = 0;

  imagemap(*zero,*span,ncolors);
  imagevnull();
  if (*erase == 1) imageerase();
  lights(4); 
  imagedisplay(*sc,*sr,*ncol,*nrow,*nc,*asc,*asr,a,*color);
  lights(-1);
  lights(2);
  lights(3);

  return (0);
}

ATUS(tvpanel)(text,n)
char *text;
int n;
{
  updatename(text,n);
}

ATUS(imagepiece)(sr,sc,nrow,ncol)
int *sr, *sc, *nrow, *ncol;
{
  imageupdate(*sc,*sr,*ncol,*nrow);
  return(0);
}


ATUS(tvcolorld)(r,g,b,n,ad)
short	*r,				/* Pointer to red array		*/
	*g,				/* Pointer to green array	*/
	*b;				/* Pointer to blue array	*/
int	*n,				/* Number of table entries	*/
	*ad;				/* First entry adress		*/

{
  short ro[4], go[4], bo[4];           /* Overlay colors               */

/*   Set the overlay color       */
  ro[0] = *(r+*n-1);
  go[0] = *(g+*n-1);
  bo[0] = *(b+*n-1);
  ro[1] = 255;
  go[1] = 0;
  bo[1] = 0;
  ro[2] = 0;
  go[2] = 255;
  bo[2] = 0;
  ro[3] = 0;
  go[3] = 0;
  bo[3] = 255;
  imagepalette(4,ro,go,bo,1);
/*   Temporarily fill in the overlay color, set the color map, then     */
/*     return to original color map                                     */
  *(r+*n-1) = *(r+*n-2);
  *(g+*n-1) = *(g+*n-2);
  *(b+*n-1) = *(b+*n-2);
  imagepalette(*n,r,g,b,0);
  *(r+*n-1) = ro[0];
  *(g+*n-1) = go[0];
  *(b+*n-1) = bo[0];
  return (0);
}

ATUS(mark)(row,col,key)

int	*row,		/* Pixel row coordinate				*/
	*col;		/* Pixel column coordinate			*/

char	*key;		/* ASCII key entered				*/

{
  imageread(col,row,key);
  return(0);
}

ATUS(immove)(ix,iy)
int *ix, *iy;
{
  imagerelocate(*ix,*iy);
  return (0); 
}

ATUS(imdraw)(ix,iy,boxcolor)
int *ix, *iy, *boxcolor;
{
  imagedraw(*ix,*iy,*boxcolor);
  return (0); 
}

ATUS(imzero)()
{
  imagevnull();
  return(0);
}

ATUS(clearvec)()
{
  vecclear();
}

ATUS(clearstore) ()
{
  storeclear();
}

ATUS(loopdisp) ()
{
  if (is_tv_open==1) xtv_refresh(0);
}
