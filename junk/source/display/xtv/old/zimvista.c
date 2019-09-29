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
#include "zimage.h"

int ncolors;
int zfreezeon = 0;
int is_tv_open = 0;

ATUS(tvopen)(nrow,ncol,nclrs)
int *nrow, *ncol, *nclrs;
{
  int nvclrs, zf, yup, ierr, imageinit();
  short r, g, b;
  float fcoord;
  int i, vistacoord(), zfreeze(), zpeak(), nextim(), lastim(), zhairs();
  int vecclear(), inst;

/* Open the display if first image */
  nvclrs = 4;
  zf = 4;
  yup = 0;
  inst = 0;
  ierr = imageinit(ncol,nrow,nclrs,&nvclrs,zf,yup,&inst); 
  if (ierr != 0) return(ierr); 
  ncolors = *nclrs;


/* Install '0' - '9' to update vista variables */
  for(i=0;i<10;i++)
    imageinstallkey('0'+i,1,vistacoord);

  imageinstallkey('f',0,zfreeze);
  imageinstallkey('F',0,zfreeze);
  imageinstallkey('p',0,zpeak);
  imageinstallkey('P',0,zpeak);
  imageinstallkey('v',0,zpeak);
  imageinstallkey('V',0,zpeak);
#ifdef __HAIRS
  imageinstallkey('h',0,zhairs);
  imageinstallkey('H',0,zhairs);
#endif
  imageinstallkey('-',0,lastim);
  imageinstallkey('+',0,nextim);
  imageinstallkey('=',0,nextim);
  imageinstallkey(']',0,vecclear);

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

/*  Routine that freezes the zoom window on one location */
zfreeze(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  zfreezeon = !zfreezeon;
  if (zfreezeon == 1) {
    lights(-3);
  }
  else {
    lights(3);
  }
}

/*  Routine to loop to the next image stored in video memory */
nextim(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  int yim;

  store = (store+1>MAXSTORE-1 ? 0 : store+1);
  while (image[store]==NULL)
    store = (store+1>MAXSTORE-1 ? 0 : store+1);
  updatestore();
  if (data == NULL) 
    lights(-2); 
  else
    lights(2);

  if (zim>0) {
    yim = winy + zim*(daty-daty-imy);
    if (yup == 1) yim = winy + zim*(imh-1 - (daty+imh-1) - imy + daty);
    writepix(zim*(datx-datx-imx)+winx, yim, zim*imw, zim*imh);
  }
  else {
    yim = winy + (daty-daty-imy)/-zim;
    if (yup == 1) yim = winy + (imh-1 - (daty+imh-1) - imy + daty)/(-zim);
    writepix((datx-datx-imx)/(-zim)+winx, yim, imw/(-zim), imh/(-zim));
  }
  if(zoomf > 0 && !zfreezeon) updatezoom(lastzoomx,lastzoomy);
/*  xtv_refresh(0);  */
}

/*  Routine to loop to the previous image stored in video memory */
lastim(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  int yim;

  store = (store-1<0 ? MAXSTORE-1 : store-1);
  while (image[store]==NULL)
    store = (store-1<0 ? MAXSTORE-1 : store-1);
  updatestore();
  if (data == NULL) 
    lights(-2); 
  else
    lights(2);
  if (zim>0) {
    yim = winy + zim*(daty-daty-imy);
    if (yup == 1) yim = winy + zim*(imh-1 - (daty+imh-1) - imy + daty);
    writepix(zim*(datx-datx-imx)+winx, yim, zim*imw, zim*imh);
  }
  else {
    yim = winy + (daty-daty-imy)/-zim;
    if (yup == 1) yim = winy + (imh-1 - (daty+imh-1) - imy + daty)/(-zim);
    writepix((datx-datx-imx)/(-zim)+winx, yim, imw/(-zim), imh/(-zim));
  }
  if(zoomf > 0 && !zfreezeon) updatezoom(lastzoomx,lastzoomy);
/*  xtv_refresh(0); */
}

/*  Routine to move cursor to local peak of data  */
zpeak(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  int xdata, ydata, xmin, ymin, xmax, ymax, ix, iy, xp, yp, xm, ym;
  float pixel, pmax, *row;

  if (data == NULL) return;

  xdata = xuser - offx;
  ydata = yuser - offy;
  xmin = ( xdata-7>datx ? xdata-7 : datx);
  ymin = ( ydata-7>daty ? ydata-7 : daty);
  xmax = ( (xdata+7)<imw+datx ? xdata+7 : imx+datx);
  ymax = ( (ydata+7)<imh+daty ? ydata+7 : imh+daty);
  pmax = *(data+ydata*datw+xdata);
  xp = xdata;
  yp = ydata;

  for (iy=ymin; iy<=ymax; iy++){
    row = data + (iy*datw);
    for (ix=xmin; ix<=xmax; ix++) {
      pixel = *(row+ix);
      if ( ((key == (int)'v' || key == (int)'V') && pixel < pmax) || 
	   ((key == (int)'p' || key == (int)'P') && pixel > pmax) ) {
        pmax = pixel;
        xp = ix;
        yp = iy;
      }
    }
  }
/*
  printf("offx: %d offy: %d datx: %d daty: %d\n",offx,offy,datx,daty); 
  printf("xp: %d yp: %d imx: %d imy: %d\n",xp,yp,imx,imy); 
  printf("datw: %d dath: %d xuser: %d yuser: %d winx: %d winy: %d\n\n",
       datw, dath, xuser, yuser, winx, winy); 
*/
  if (zim>0) {
    xm = (xp - datx - imx) * zim + winx;
    ym = (yp - daty - imy) * zim + winy;
    if (yup ==1) ym = (imh-1 - yp - imy + daty) * zim + winy;
    if (zim>1) {
      xm = xm + zim/2 - 1;
      ym = ym + zim/2 - 1;
    }
  } else {
    xm = (xp - datx - imx) / -zim + winx;
    ym = (yp - daty - imy) / -zim + winy;
    if (yup ==1) ym = (imh-1 - yp - imy + daty) / -zim + winy;
  }
/*
  printf("xm: %d ym: %d \n",xm,ym);
*/

  XWarpPointer(dpy,None,wimage,0,0,0,0,xm,ym);
  updatecoords(xm,ym,-1);
  if(zoomf > 0 && !zfreezeon) updatezoom(xm,ym);
  XFlush(dpy);
    
}

#ifdef __HAIRS
extern int usehairs;
zhairs(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  XColor curswcolor, cursbcolor;
  Pixmap csource, cmask;

  if (usehairs == 0) {
XDefineCursor(dpy,wimage,XCreateGlyphCursor(dpy, fontinfo->fid, fontinfo->fid,
    (unsigned int)' ', (unsigned int)' ',&curswcolor, &cursbcolor));
    usehairs = 1;
  }
  else {
vnohair();
curswcolor.pixel = WhitePixel(dpy,screen);
XQueryColor(dpy,defcmap,&curswcolor);
cursbcolor.pixel = BlackPixel(dpy,screen);
XQueryColor(dpy,defcmap,&cursbcolor);
csource = XCreateBitmapFromData(dpy,wimage,curs_bits,curs_width,curs_height);
cmask = XCreateBitmapFromData(dpy,wimage,curs_mask_bits,curs_width,curs_height);
curs = XCreatePixmapCursor(dpy,csource,cmask,
	 &curswcolor,&cursbcolor,curs_x_hot,curs_y_hot);
XFreePixmap(dpy,csource);
XFreePixmap(dpy,cmask);
XDefineCursor(dpy,wimage,curs);
    usehairs = 0;
  }
}
#endif

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
  imageupdate(*x, *y, *nx, *ny);
}


float breakpts[256];
#define ABS(a) (((a) > 0) ? (a) : -(a))
extern int yup;

ATUS(tvfload)(a,nrow,ncol,nc,sr,sc,asr,asc,span,zero,flip,erase)

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
	*erase;         /* Flag to erase screen                         */

{
  float c;
  int i;

/*  lights(-2); */

  if(*flip == 1) yup = 1;
  else yup = 0;

  if(*span == 0.) {
    imagemap(zero);
  } else {
    c = ABS(*span) / (ncolors-2);
    for(i=0;i<ncolors-1;i++) 
      breakpts[i] = *zero + c * i;
    imagemap(breakpts);
  }

  imagevnull();

  if (*erase == 1) imageerase();

  lights(4); 
  imagedisplay(*sc,*sr,*ncol,*nrow,*nc,*asc,*asr,a);
  lights(-1);
  lights(2);
  lights(3);

  return (0);
}

ATUS(tvpanel)(text,n)
char *text;
int n;
{
  strncpy(imname[store],text,20);
  updatecoords(-1,-1,-1);
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
/*  fprintf(stderr,"\nMARK: Turning on lights\n"); */
  lights(1);
/*  fprintf(stderr,"MARK: Waiting on imageread\n"); */
  imageread(col,row,key);
/*  fprintf(stderr,"MARK: Turning off lights\n"); */
  lights(-1);
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

imvecplay()
{
  imagevreplay();
  imagetreplay();
  return (0); 
}

static int lgtstatus=0;

lights(state)
int state;
{
  char string[10];
  if (state == -1 ) imagelight(1,"ASYNC",0);
  if (state == 1 ) imagelight(1,"INPUT",1);
  if (state == -2 ) imagelight(2,"NO DATA",0);
  if (state == 2 ) imagelight(2,"READY",1);
  if (state == -3 ) imagelight(3,"FREEZE",0);
  if (state == 3 ) imagelight(3,"UPDATE",1);
  if (state == -4 ) {
    if (zim==1)
      imagelight(4,"NORM",1);
    else if (zim>0) {
      sprintf(string,"ZOOM*%d",zim);
      imagelight(4,string,0);
    } else {
      sprintf(string,"ZOOM/%d",-zim);
      imagelight(4,string,0);
    }
  }
  if (state == 4 ) imagelight(4,"NORM",1);
}

extern int imw, imh, datx, daty, offx, offy;

ATUS(sunmove)(ix,iy)
int *ix, *iy;
{
  int x, y;
  x = *ix - 400 + imw/2 + datx + offx;
  y = *iy - 400 + imh/2 + daty + offy;
  ATUS(immove)(&x,&y);
}

ATUS(sundraw)(ix,iy)
int *ix, *iy;
{
  int x, y, boxcolor;
  x = *ix - 400 + imw/2 + datx + offx;
  y = *iy - 400 + imh/2 + daty + offy;
  boxcolor = 0;
  ATUS(imdraw)(&x,&y,&boxcolor);
}

sunvecplay()
{
  imvecplay();
}


ATUS(clearvec)()
{
  vecclear();
}

vecclear()
{
  int yim;

  if (image[store]!=NULL) {
    imagevnull();

    if (data == NULL) 
      lights(-2); 
    else
      lights(2);

    if (zim>0) {
      yim = winy + zim*(daty-daty-imy);
      if (yup == 1) yim = winy + zim*(imh-1 - (daty+imh-1) - imy + daty);
      writepix(zim*(datx-datx-imx)+winx, yim, zim*imw, zim*imh);
    } else {
      yim = winy + (daty-daty-imy)/-zim;
      if (yup == 1) yim = winy + (imh-1 - (daty+imh-1) - imy + daty)/-zim;
      writepix((datx-datx-imx)/-zim+winx, yim, imw/-zim, imh/-zim);
    }
    if(zoomf > 0 && !zfreezeon) updatezoom(lastzoomx,lastzoomy);
    xtv_refresh(0);
   }
}

ATUS(clearstore) ()
{
  int i;

  for(i=0;i<MAXSTORE;i++)
   if (i!=store) image[i]=NULL;
}

ATUS(loopdisp) ()
{
  if (is_tv_open==1) xtv_refresh(0);
}
