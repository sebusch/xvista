#include "Vista.h"
/*
   X11 Windows image routines - John Tonry 5/12/88
   Implemented in xvista with various mods - J. Holtzman 5/90-5/91, 96
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xresource.h>
#ifdef __HAIRS
#include <X11/cursorfont.h>
static Bool  hairs_on = False;
static  XSegment    crshr[4] = {
	{-1, -1, -1, -1},
	{-1, -1, -1, -1},
	{-1, -1, -1, -1},
	{-1, -1, -1, -1},
};
int usehairs = 0;
int private = 0;
#endif
#include <signal.h>

static int     fd_for_X = -1;
int waiting_for_key = 0;

#include "zimage.h"
int imagevalid;
char resourcename[24];
char windowname[80];

int blackPixel(Display *, int);
int whitePixel(Display *, int);

/* Routine to exercise refresh without signals and interrupts (for debugging) */
imagerefresh_()
{
  if(wbase != 0) {
    xtv_refresh(0);
    XFlush(dpy);
  }
return(0);
}

int tvinit = 0;
int xoff, yoff;
/* Initialize X server connection and windows */

int imageinit(winit,hinit,nclrs,nvclrs,zfinit,yupinit,inst)
int *winit, *hinit;     /* Size of window (request on input, actual returned */
int *nclrs;             /* Number of image color cells requested and actual */
int *nvclrs;            /* Number of vector color cells requested and actual */
int zfinit;             /* Initial zoom factor (0 for no zoom window) */
int yupinit;            /* flag = 0/1 for y increasing down/up */
int *inst;              /* identifies instrument for resource/windowname */
{
  int pfd[2];                     /* pipe descriptors */
  int i, ierr;
  char *option;
  XColor  curswcolor, cursbcolor;
  Pixmap csource, cmask;

  int nvisuals, depth;
  XVisualInfo *vinfo[16];
  XVisualInfo vtemplate;
  Visual *visual;
  XColor cmapcolors[256];
  XStandardColormap scmap;

  /* Initialize  necessary variable from include file: zimage.h  */
  /* These initializations were removed from the include file so that */
  /*   it can be included in separate files,  JAH 5/90  */
  dpy = NULL;
  wbase = (Window)0;
  wzoom = (Window)0;
  display = NULL;
  geometry = NULL;
  npalette = MAXCOLORS;
  vlastcolor = (-1);
  resize = 1;
  autozoomout = 0;
  zoomsample = 1;
  for (i=0; i<MAXSTORE; i++) image[i] = NULL;
  imbuf = NULL;
  zim = 0;
  zoomf = DEFZOOMFAC;
  buttondown = 0;
  store = 0;
  planes = 0;

  if (*inst == 0) {
    sprintf(resourcename,"%s","xvista");
    xoff = yoff = 20;
    sprintf(windowname,"XVISTA");
  } else if (*inst == 1) {
    sprintf(resourcename,"%s","ccddisp");
    xoff = 1;
    yoff = 110;
    sprintf(windowname,"1m Data Acquisition");
  } else if (*inst == 3) {
    sprintf(resourcename,"%s","disslit");
    xoff = 1;
    yoff = 110;
    sprintf(windowname,"DIS Slit Viewer");
  } else {
    sprintf(resourcename,"%s","gccddisp");
    sprintf(windowname,"1m Guider Acquisition");
    xoff = 275;
    yoff = 110;
  }


  /* Open the connection to the server, quit on failure */
  if( ! (dpy = XOpenDisplay(display)) ) {
    fprintf(stderr,"Can't open display %s\n",XDisplayName(display));
    return(ERR_CANT_OPEN_DISPLAY);
  }

  screen = XDefaultScreen(dpy);

  zoomf = zfinit;
  yup = yupinit;
  imagevalid = 0;

/* Do we have a PseudoColor display ?? */
  truecolor = 0;

  vtemplate.class = PseudoColor;
  *vinfo = XGetVisualInfo(dpy,VisualClassMask,&vtemplate,&nvisuals);
  if (nvisuals == 0) {
    printf("Your X server does not have a PseudoColor mode\n");
    printf("  Other visuals are currently not supported.\n");
    printf("  Try to restart your display in 8-bit PseudoColor mode.\n");
    printf("  Alternatively, run a display emulator in 8-bit mode (e.g. vncserver -cc 3\n");
    vtemplate.class = TrueColor;
    *vinfo = XGetVisualInfo(dpy,VisualClassMask,&vtemplate,&nvisuals);
    if (nvisuals == 0) {
      printf("dont have TrueColor either!\n Not ready to deal with this!\n");
      exit(0); 
    } else {
     truecolor = 1;
     rmask = vinfo[0]->red_mask;
     gmask = vinfo[0]->green_mask;
     bmask = vinfo[0]->blue_mask;
    }
  }
  visual = vinfo[0]->visual;
  depth = vinfo[0]->depth;

/* Can we allocate enough colors? If not, use a private colormap */
  defcmap = XDefaultColormap(dpy,screen);
  private = 0;
  ncolors = *nclrs;
  if (truecolor) {
    for (i=0; i < *nvclrs+4+*nclrs; i++) pixels[i] = i;
  } else {
    if (XAllocColorCells(dpy, defcmap,
  		 0, &planes, 0, pixels, *nvclrs+4+*nclrs) == 0) {
      fprintf(stderr,"Insufficient colors in default colormap: switching to private colormap\n");
      defcmap = XCreateColormap(dpy,RootWindow(dpy,screen),DefaultVisual(dpy,screen),AllocAll );
      for (i=0 ; i <*nvclrs+4+*nclrs; i++) pixels[i] = i; 
      private= 1;
    }
  } 

  /*Set the color addresses in status to the addresses returned by the server */
  for(i=0;i<*nvclrs;i++) {
    vcolor[i].pixel = pixels[i];
    vcolor[i].flags = DoRed | DoGreen | DoBlue;
  }
  nvcolors = *nvclrs;
  
  /*Set the color addresses in status to the addresses returned by the server */
  for(i=0;i<4;i++) {
    stcolor[i].pixel = pixels[i+nvcolors];
    stcolor[i].flags = DoRed | DoGreen | DoBlue;
  }
  stcolor[0].red = 256 * (255 & 0x00ff);
  stcolor[0].green = 0;
  stcolor[0].blue = 0;
  stcolor[1].red = 256 * (255 & 0x00ff);
  stcolor[1].green = 256 * (255 & 0x00ff);
  stcolor[1].blue = 256 * (255 & 0x00ff);
  stcolor[2].red = 0;
  stcolor[2].green = 256 * (255 & 0x00ff);
  stcolor[2].blue = 0;
  stcolor[3].red = 0;
  stcolor[3].green = 0;
  stcolor[3].blue = 0;
  if (!truecolor) XStoreColors(dpy, defcmap,stcolor,4);
  
  /* Ask for the nclrs colors from the server */
  /* NOTE that this must be done last because the pixels array is used later */
  /* Set the color addresses in cmap to the addresses returned by the server */
  ncolors = *nclrs;
  for(i=0;i<ncolors;i++) {
    cmap[i].pixel = pixels[i+nvcolors+4];
    cmap[i].flags = DoRed | DoGreen | DoBlue;
    pixels[i] = pixels[i+nvcolors+4];
  }

  /* Get memory for the lookup table */
  if((lookup = (short *)malloc(1<<17) ) == NULL) {
    fprintf(stderr,"Can't allocate lookup table\n");
    return(ERR_BAD_ALLOC_LOOKUP);
  }
  
  /* Get memory for the palette array */
  if((palette = (unsigned char *)malloc(MAXPALWIDTH))==NULL) {
    fprintf(stderr,"Can't allocate palette array\n");
    return(ERR_BAD_ALLOC_PALETTE);
  }
  
  /* Get memory for the display buffer; ask for X's maximum of 128k */
  if((imbuf = (unsigned char *)malloc(MAXSCREENPIX))==NULL) {
    fprintf(stderr,"Can't allocate display buffer\n");
    return(ERR_BAD_ALLOC_IMBUF);
  }
  
  /* Create the palette XImage structure */
  palimage = XCreateImage(dpy,DefaultVisual(dpy,screen),depth,ZPixmap,0,palette,
  	       MAXPALWIDTH,1,8,0);
  /* Create the data XImage structure */
  if (truecolor) {
    printf("depth: %d\n", depth);
    dataimage = XCreateImage(dpy,visual,depth,ZPixmap,0,imbuf,
               128,1024,16,0);
    dataimage->bitmap_unit = 16;
  } else {
    dataimage = XCreateImage(dpy,DefaultVisual(dpy,screen),depth,ZPixmap,0,imbuf,
  	       128,1024,8,0);
    dataimage->bitmap_unit = 8;
  }
  
  /* If requested, set parameters for the zoom window */
  if(zoomf > 0) {
    zwidth = zheight = DEFZOOMSIZE;
    lastzoomx = lastzoomy = -100000;
    zoomimage = XCreateImage(dpy,DefaultVisual(dpy,screen),8,ZPixmap,0,NULL,
  	       1,1,8,0);
  }
  
  /* Get the font to be used, and get the information about it */

  fontname1 = XGetDefault(dpy,resourcename,"FontName");
  fontname2 = DEFAULT_FONT2;
  if (fontname1 == NULL) fontname1 = DEFAULT_FONT1;
  if (  ((fontinfo = XLoadQueryFont(dpy,fontname1)) != NULL)
      ||((fontinfo = XLoadQueryFont(dpy,fontname2)) != NULL) ) {
  } else {
    fprintf(stderr,"Can't open font %s\n",fontname1);
    fprintf(stderr,"Put an appropriate font in your .Xdefaults file with index \
  xvista.FontName\n");
    fprintf(stderr,"You will need to log out before this takes effect\n");
    return(ERR_CANT_GET_FONT);
  }
  fontwidth = fontinfo->max_bounds.rbearing - fontinfo->min_bounds.lbearing;
  fontheight = fontinfo->max_bounds.ascent + fontinfo->max_bounds.descent;

  /* Initialize keyaction array */
  for(i=0;i<128;i++) {
    keyaction[i].action = NULL;
    keyaction[i].echo = 0;
  }
  keyaction[1].action = keyzoomin;
  keyaction[2].action = keyzoomout;
  keyaction[3].action = keypan;
  keyaction['@'].action = keyrecenter;
  keyaction['r'].action = keyrecenter;
  keyaction['R'].action = keyrecenter;
  keyaction['$'].action = keyzoomprint;
  
  /* Open a pipe from parent to itself to use for blocking reads from handler */
  if (pipe(pfd) == -1)
    fprintf(stderr,"ximage: Can't open a pipe for handler\n");
  
  to_program = pfd[1];
  from_display = pfd[0]; 
  
  /* Get options for autoresize of windows and/or autozoomout of large images */

  option = XGetDefault(dpy,resourcename,"resize");
  if (option != NULL) resize = atoi(option);
  option = XGetDefault(dpy,resourcename,"autozoomout");
  if (option != NULL) autozoomout = atoi(option);
  option = XGetDefault(dpy,resourcename,"zoomsample");
  if (option != NULL) zoomsample = atoi(option);

  /* Get options for default window size and maximum window size */
  option = XGetDefault(dpy,resourcename,"width");
  if (option != NULL) *winit = atoi(option);
  option = XGetDefault(dpy,resourcename,"height");
  if (option != NULL) *hinit = atoi(option);

  maxwidth=DEFMAXWIDTH;
  maxheight=DEFMAXHEIGHT;
  option = XGetDefault(dpy,resourcename,"maxwidth");
  if (option != NULL) maxwidth = atoi(option);
  option = XGetDefault(dpy,resourcename,"maxheight");
  if (option != NULL) maxheight = atoi(option);
 
  *winit=MIN(maxwidth,*winit);
  *hinit=MIN(maxheight,*hinit);

  /* Create the windows */
  newsizesubwin(*winit,*hinit);
  ierr = createwindows();
  if (ierr != 0) return(ierr);
  *winit = width;
  *hinit = height; 
  updateimage(0,0,width,height,width,NULL,0,0);
  tvinit = 1;

  return(0);
}

/* Create the base window and all its little friends */

createwindows()
{
XColor  curswcolor, cursbcolor, backcolor;
XWMHints  wmhints;
XSizeHints sizehints;

int border_width=2, old_owner, i;
Pixmap csource, cmask;
char *option;

option = XGetDefault(dpy,resourcename,"backred");
if (option != NULL) {
  i = atoi(option);
  backcolor.red = i<<8;
} else
  backcolor.red = 0;

option = XGetDefault(dpy,resourcename,"backgreen");
if (option != NULL) {
  i = atoi(option);
  backcolor.green = i<<8;
} else
  backcolor.green = 0;

option = XGetDefault(dpy,resourcename,"backblue");
if (option != NULL) {
  i = atoi(option);
  backcolor.blue = i<<8;
} else
  backcolor.blue = 0;

if (private) {
  backcolor.pixel = blackPixel(dpy,screen);
} else {
  XAllocColor(dpy,defcmap,&backcolor);
}
xswa.colormap = defcmap;
xswa.background_pixel = backcolor.pixel;
xswa.border_pixel = whitePixel(dpy,screen);
xswa.backing_store = WhenMapped;
/*xswa.override_redirect = True; */

sizehints.flags = PPosition | PSize;
sizehints.width = width;
sizehints.height = height+XYZHEIGHT; 

option = XGetDefault(dpy,resourcename,"x");
if (option != NULL) {
  xoff = atoi(option);
  if (xoff < 0) xoff = DisplayWidth(dpy,screen) + xoff - width - 2*border_width;
  sizehints.flags |= (USSize | USPosition);
  sizehints.x = xoff;
}
option = XGetDefault(dpy,resourcename,"y");
if (option != NULL) {
  yoff = atoi(option);
  if (yoff < 0) yoff = DisplayHeight(dpy,screen) + yoff - height - XYZHEIGHT - 2*border_width;
  sizehints.flags |= (USSize | USPosition);
  sizehints.y = yoff;
}

wbase = XCreateWindow(dpy,RootWindow(dpy,screen),
		xoff,yoff,width,height+XYZHEIGHT,border_width,
		0,InputOutput,CopyFromParent,
		CWBackPixel | CWBorderPixel | CWColormap, &xswa);
/* See XSetStandardProperties for defining a fancy icon */
XSetIconName(dpy, wbase, "Ximage");

if (!wbase) {
  fprintf(stderr, "XCreateWindow failed\n");
  return(ERR_CANT_CREATE_IMAGE_WINDOW);
}

/* Map the window and see what size it is (if the user has resized it) */
XSetStandardProperties(dpy, wbase, windowname, windowname,
   None, NULL, 0, &sizehints);
wmhints.input = True;
wmhints.flags = InputHint;
XSetWMHints(dpy, wbase, &wmhints);
XMapWindow(dpy,wbase);
/*
XGetGeometry(dpy,wbase,&inforoot,&infox,&infoy,&infowidth,&infoheight,
       &infoborder,&infodepth);
*/
/* Set width and height to the size of the window */
/*
width = infowidth;
height = infoheight - XYZHEIGHT;
*/

/*
* Create the subwindows
*/

/* Create the image subwindow */
wimage = XCreateSimpleWindow(dpy,wbase,       /* Parent window */
       0,0,                             /* UL location (nominal) */
       width,height,                    /* size (nominal) */
       0,                               /* border width */
       whitePixel(dpy,screen),          /* border pixmap */
       backcolor.pixel);         /* background pixmap */
XChangeWindowAttributes(dpy,wimage,CWBackingStore|CWColormap,&xswa);

/* Create the palette subwindow */
wpal = XCreateSimpleWindow(dpy,wbase,         /* Parent window */
	       palx,paly,               /* UL location */
	       palwidth,palheight,      /* size */
	       XYZBORDER,               /* border width */
       whitePixel(dpy,screen),          /* border pixmap */
       blackPixel(dpy,screen));         /* background pixmap */
XChangeWindowAttributes(dpy,wpal,CWBackingStore|CWColormap,&xswa);

/* Define the new image cursor */
curswcolor.pixel = whitePixel(dpy,screen);
cursbcolor.pixel = blackPixel(dpy,screen);
XQueryColor(dpy,defcmap,&curswcolor);
XQueryColor(dpy,defcmap,&cursbcolor);

/* If we want full screen crosshairs, define a blank cursor here */
#ifdef __HAIRS
/*XDefineCursor(dpy,wimage,XCreateGlyphCursor(dpy, fontinfo->fid, fontinfo->fid,
  (unsigned int)' ', (unsigned int)' ',&curswcolor, &cursbcolor));
  #else */
csource = XCreateBitmapFromData(dpy,wimage,curs_bits,curs_width,curs_height);
cmask = XCreateBitmapFromData(dpy,wimage,curs_mask_bits,curs_width,curs_height);
curs = XCreatePixmapCursor(dpy,csource,cmask,
       &curswcolor,&cursbcolor,curs_x_hot,curs_y_hot);
XFreePixmap(dpy,csource);
XFreePixmap(dpy,cmask);
XDefineCursor(dpy,wimage,curs);
#endif

/* Define the palette cursor */
csource = XCreateBitmapFromData(dpy,wimage,palcurs_bits,
			  palcurs_width,palcurs_height);
cmask = XCreateBitmapFromData(dpy,wimage,palcurs_mask_bits,
			palcurs_width,palcurs_height);
palcurs = XCreatePixmapCursor(dpy,csource,cmask,
       &curswcolor,&cursbcolor,
/*               whitePixel(dpy,screen),blackPixel(dpy,screen), */
       palcurs_x_hot,palcurs_y_hot);
XDefineCursor(dpy,wpal,palcurs);

XFreePixmap(dpy,csource);
XFreePixmap(dpy,cmask);

/* xyz subwindow */
wxyz = XCreateSimpleWindow(dpy,wbase,xyzx,xyzy,xyzwidth,xyzheight,XYZBORDER,
       whitePixel(dpy,screen),blackPixel(dpy,screen));
XChangeWindowAttributes(dpy,wxyz,CWBackingStore|CWColormap,&xswa);

/* light subwindows */
wlgt1 = XCreateSimpleWindow(dpy,wbase,lgtx2,xyzy,lgtwidth,lgtheight,XYZBORDER,
       whitePixel(dpy,screen),blackPixel(dpy,screen));
wlgt2 = XCreateSimpleWindow(dpy,wbase,lgtx2,lgty,lgtwidth,lgtheight,XYZBORDER,
       whitePixel(dpy,screen),blackPixel(dpy,screen));
wlgt3 = XCreateSimpleWindow(dpy,wbase,lgtx1,xyzy,lgtwidth,lgtheight,XYZBORDER,
       whitePixel(dpy,screen),blackPixel(dpy,screen));
wlgt4 = XCreateSimpleWindow(dpy,wbase,lgtx1,lgty,lgtwidth,lgtheight,XYZBORDER,
       whitePixel(dpy,screen),blackPixel(dpy,screen));
XChangeWindowAttributes(dpy,wlgt1,CWBackingStore|CWColormap,&xswa);
XChangeWindowAttributes(dpy,wlgt2,CWBackingStore|CWColormap,&xswa);
XChangeWindowAttributes(dpy,wlgt3,CWBackingStore|CWColormap,&xswa);
XChangeWindowAttributes(dpy,wlgt4,CWBackingStore|CWColormap,&xswa);

/* Create a zoom window if required              */
if(zoomf > 0) {
  sizehints.flags = PPosition | PSize;
  sizehints.width = zwidth*zoomf;
  sizehints.height = zheight*zoomf; 

  xoff = yoff = 3;
  option = XGetDefault(dpy,resourcename,"zoomx");
  if (option != NULL) {
    xoff = atoi(option);
    if (xoff < 0) xoff = DisplayWidth(dpy,screen) + xoff - zwidth - 2*border_width;
    sizehints.flags |= (USSize | USPosition);
    sizehints.x = xoff;
  }
  option = XGetDefault(dpy,resourcename,"zoomy");
  if (option != NULL) {
    yoff = atoi(option);
    if (yoff < 0) yoff = DisplayHeight(dpy,screen) + yoff - zheight - XYZHEIGHT - 2*border_width;
    sizehints.flags |= (USSize | USPosition);
    sizehints.y = yoff;
  }

  wzoom = XCreateWindow(dpy,RootWindow(dpy,screen),
		xoff,yoff,zwidth*zoomf,zheight*zoomf,1,
		0,InputOutput,CopyFromParent,
		CWBackPixel | CWBorderPixel | CWColormap, &xswa);
  XSetIconName(dpy, wzoom, "Xzoom");
  if (!wzoom) {
    fprintf(stderr, "XCreate failed to make zoom window\n");
    return(ERR_CANT_CREATE_ZOOM_WINDOW);
  }
  XSelectInput(dpy, wzoom, ButtonPressMask | ExposureMask | StructureNotifyMask);
  XSetStandardProperties(dpy, wzoom, "xvistaZoom", "xvista",
    None, NULL, 0, &sizehints);
  XMapWindow(dpy, wzoom);
}


/* Select inputs from the window */
/* Create the graphics context for images */
imagegc = XCreateGC(dpy, wbase, 0, NULL);
XSetState(dpy,imagegc,
    whitePixel(dpy,screen),blackPixel(dpy,screen),GXcopy,AllPlanes);

/* Create the graphics context for line drawing */
vectorgc = XCreateGC(dpy, wbase, 0, NULL);
XSetState(dpy,vectorgc,
    whitePixel(dpy,screen),blackPixel(dpy,screen),GXcopy,AllPlanes);
XSetForeground(dpy, vectorgc, whitePixel(dpy,screen));

textgc = XCreateGC(dpy, wbase, 0, NULL);
XSetState(dpy,textgc,
    whitePixel(dpy,screen),blackPixel(dpy,screen),GXcopy,AllPlanes);
XSetFont(dpy,textgc,fontinfo->fid);
XSetFillStyle(dpy,textgc,FillSolid);

XSelectInput(dpy,wbase, ExposureMask | LeaveWindowMask |
       ButtonPressMask | ButtonReleaseMask | StructureNotifyMask |
       PointerMotionMask | PointerMotionHintMask | KeyPressMask | ColormapChangeMask);
XSelectInput(dpy,wimage, ExposureMask);
/* Map the subwindows */
XMapSubwindows(dpy,wbase);
/* Flush this pile of X output */
XFlush(dpy);

/* Ask for non-blocking IO on the input from X */
fd_for_X = ConnectionNumber(dpy);
UTUV(mx11register)(&fd_for_X,xtv_refresh);

return(0);
}

imageclose()
{
  return(0);
}

imageerase()
{
#ifdef __HAIRS
  if (usehairs && hairs_on) vnohair(); 
#endif
  XClearWindow(dpy,wimage);
  XFlush(dpy);
}

extern int zfreezeon;
extern int imtvnum_;

imageupdate(x0,y0,nx,ny)
int x0, y0;     /* origin of area whose image to update (data coords) */
int nx, ny;     /* size of area whose image to update */
{
  int yim;
  if(data == NULL) return(ERR_NO_DATA);

  yim = y0 - daty;
  if(yup == 1) yim = imh-1 - (y0-daty);

  mapimage(x0,y0,nx,ny,datw,data,x0-datx,yim,imwidth,yup,
           image[store],lookup,branch);
  if(zim > 0) {
    yim = winy + zim*(y0-daty-imy);
    if(yup == 1) yim = winy + zim*(imh-1 - (y0+ny-1) - imy + daty);

    writepix(zim*(x0-datx-imx)+winx, yim, zim*nx, zim*ny);
  } else {
    yim = winy + (y0-daty-imy)/(-zim);
    if(yup == 1) yim = winy + (imh-1 - (y0+ny-1) - imy + daty)/(-zim);

    writepix((x0-datx-imx)/(-zim)+winx, yim, nx/(-zim), ny/(-zim));
  }
  if(zoomf > 0 && !zfreezeon) updatezoom(lastzoomx,lastzoomy);
  xtv_refresh(0);

  return(0);
}

imagedisplay(x0,y0,nx,ny,awidth,xoff,yoff,a)
float *a;               /* Input floating image array                   */
int x0, y0, nx, ny;     /* Origin and size of area to be displayed from a */
int awidth;             /* Number of pixels per row in a */
int xoff, yoff;         /* x and y coordinates to be used for pixel (0,0) */
{
 int i, yim, maxdim, mindim, maxw, maxh;

/* First test to see whether any windows have been created */
 if(wbase == 0) {
   fprintf(stderr,"Windows not yet created\n");
   return(ERR_NOT_INITIALIZED);
 }

/* Check to see that the image array is large enough for the data */

 store = store+1>MAXSTORE-1 ? 0 : store+1;

 if (nimage[store] < ROUNDUP(nx)*ny || image[store] == NULL) {
   if(image[store] != NULL) free(image[store]);
   if((image[store] = (unsigned char *)malloc(ROUNDUP(nx)*ny)) == NULL) {
     fprintf(stderr,"Can't allocate image array\n");
     return(ERR_BAD_ALLOC_IMBUF);
   }
   nimage[store] = ROUNDUP(nx)*ny;
 }

/* Update the display variables */

 updateimage(x0,y0,nx,ny,awidth,a,xoff,yoff);

 yim = 0;
 if(yup == 1) yim = imh - 1;

 mapimage(datx,daty,imw,imh,datw,data,
   0,yim,imwidth,yup,image[store],lookup,branch);

 imagevalid = 1;
 imagevnull();

/* If the image can be zoomed and fit it the display window, do it */

 maxdim = MAX(nx,ny);
 while (maxdim*2 <= MIN(width,height)) {
   updatepan(imw/2,imh/2,1);
   maxdim = maxdim*2;
 }

/* If image is too big for display, zoom out if option is set */
 if (autozoomout && (nx>width || ny>height)  ) {
    if (resize) {
      maxw = maxwidth;
      maxh = maxheight;
    } else {
      maxw = width;
      maxh = height;
    }
    while ( (nx/ABS(zim))>maxw || (ny/ABS(zim))>maxh )  {
      updatepan(imw/2,imh/2,-1);
    }
  }

/* Update the size of the display if it is too small */

 if (resize && (width < nx/ABS(zim) || height < ny)) {
   width = MIN(maxwidth,MAX(width,nx));
   height = MIN(maxheight,MAX(height,ny));
   updatepan(imw/2,imh/2,0);
   updatesize(width,height);
   if(zim > 0) {
     writepix(winx, winy, zim*imw, zim*imh);
   } else {
     writepix(winx, winy, imw/(-zim), imh/(-zim));
   }

 }

/* Otherwise damage the image window so as to get it displayed */

 else {
  if(zim > 0) {
    writepix(winx, winy, zim*imw, zim*imh);
  } else {
    writepix(winx, winy, imw/(-zim), imh/(-zim));
  }
   xtv_refresh(0);
 }
 XFlush(dpy);

 return (0);
}

/* Routine to set up the data -> image mapping */

imagemap(breakpts)
float *breakpts;        /* values of the break points */
{
  palbrkpt = breakpts;
  brancher(ncolors,pixels,breakpts,lookup,branch);
  return(0);
}

/* Routine to load the X Windows color lookup table */

imagepalette(n,r,g,b,flag)
int n;                  /* Number of table entries      */
short *r, *g, *b;       /* Pointer to red, green and blue arrays */
int flag;               /* 0/1 for image/vector color load */
{
  int i, k;
  Colormap *list;
  int num;

  if(flag == 0) {
    for(i=0;i<npalette;i++) {
      k = (i*n) / npalette;
      palcolors[i].red = 256*( r[k] & 0x00ff);
      palcolors[i].green = 256*( g[k] & 0x00ff);
      palcolors[i].blue = 256*( b[k] & 0x00ff);
    }
    newcolors(0,npalette-1);
  } else {
    for(i=0;i<MIN(nvcolors,n);i++) {
      vcolor[i].red = 256*( r[i] & 0x00ff);
      vcolor[i].green = 256*( g[i] & 0x00ff);
      vcolor[i].blue = 256*( b[i] & 0x00ff);
    }
    if (!truecolor) XStoreColors(dpy, defcmap, vcolor, nvcolors);  
  }
  return (0);
}

imageinstallkey(key,echo,function)
int key;                        /* key to cause the action */
int echo;                       /* 0/1 to echo key in x,y,z display */
int (*function)();              /* action to be taken */
			/* function(x,y,xuser,yuser,key) */
{
  keyaction[key].action = function;
  keyaction[key].echo = echo;
}

ATUS(imagetext)(xuser,yuser,text,textlen,nt)
int *xuser, *yuser, *textlen, nt;
char *text;
{
  int zad, x, y;

  if(zim > 0) {
    zad = (zim - 1) / 2;
    x = zim * (*xuser - imx - datx - offx) + winx + zad;
    if(yup == 0)
      y = winy + zim * (*yuser - imy - daty - offy) + zad;
    else
      y = winy + zim * (imh-1 - (*yuser - offy - daty) - imy) + zad;
  } else {
    x = (*xuser - imx - datx - offx)/(-zim) + winx;
    if(yup == 0)
      y = winy + (*yuser - imy - daty - offy)/(-zim);
    else
      y = winy + (imh-1 - (*yuser - offy - daty) - imy)/(-zim);
  }
  xtlast = x;
  ytlast = y;
  textlist[tcount].xt = *xuser;
  textlist[tcount].yt = *yuser;
  text[*textlen] = 0;
  sprintf(textlist[tcount].text,"%s",text);
  textlist[tcount].color = -1;
  tcount = MIN(tcount+1,MAXTEXT-1);
  XSetForeground(dpy,textgc,whitePixel(dpy,screen));
  XDrawImageString(dpy,wimage,textgc,x,y,text,*textlen);
  XFlush(dpy);
}

imagerelocate(xuser,yuser)
int xuser, yuser;               /* x,y coordinates in user's system */
{
  int x, y, zad;

  if(zim > 0) {
    zad = (zim - 1) / 2;
    x = zim * (xuser - imx - datx - offx) + winx + zad;
    if(yup == 0)
      y = winy + zim * (yuser - imy - daty - offy) + zad;
    else
      y = winy + zim * (imh-1 - (yuser - offy - daty) - imy) + zad;
  } else {
    x = (xuser - imx - datx - offx)/(-zim) + winx;
    if(yup == 0)
      y = winy + (yuser - imy - daty - offy)/(-zim);
    else
      y = winy + (imh-1 - (yuser - offy - daty) - imy)/(-zim);
  }

  xvlast = x;
  yvlast = y;
  veclist[vcount].xv = xuser;
  veclist[vcount].yv = yuser;
  veclist[vcount].color = -1;
  vcount = MIN(vcount+1,MAXVEC-1);
  return (0);
}

imagedraw(xuser,yuser,color)
int xuser, yuser;               /* x,y coordinates in user's system */
int color;
{
  int x, y, zad;
  vlastcolor=-1;
  if(zim > 0) {
    zad = (zim - 1) / 2;
    x = zim * (xuser - imx - datx - offx) + winx + zad;
    if(yup == 0)
      y = winy + zim * (yuser - imy - daty - offy) + zad;
    else
      y = winy + zim * (imh-1 - (yuser - offy - daty) - imy) + zad;
  } else {
    x = (xuser - imx - datx - offx)/(-zim) + winx;
    if(yup == 0)
      y = winy + (yuser - imy - daty - offy)/(-zim);
    else
      y = winy + (imh-1 - (yuser - offy - daty) - imy)/(-zim);
  }

  if(color != vlastcolor) {
    XSetForeground(dpy, vectorgc, vcolor[color].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 1: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
    vlastcolor = color;
  }
  XDrawLine(dpy,wimage,vectorgc,xvlast,yvlast,x,y);

  xvlast = x;
  yvlast = y;
  veclist[vcount].xv = xuser;
  veclist[vcount].yv = yuser;
  veclist[vcount].color = color;
  vcount = MIN(vcount+1,MAXVEC-1);
  XFlush(dpy);
  return (0);
}

imagedrawflush(xuser,yuser,color)
int xuser, yuser;               /* x,y coordinates in user's system */
int color;
{
  imagedraw(xuser,yuser,color);
  XFlush(dpy);
}

imagevnull()
{
  vcount = 0;
  tcount = 0;
  return(0);
}

imagetreplay()
{
  struct imagetext *textp;
  int i,zad, x, y, textlen;

  textp = textlist;
  for (i = 0; i < tcount; i++) {
   if(zim > 0) {
    zad = (zim - 1) / 2;
    x = zim * (textp->xt - imx - datx - offx) + winx + zad;
    if(yup == 0)
      y = winy + zim * (textp->yt - imy - daty - offy) + zad;
    else
      y = winy + zim * (imh-1 - (textp->yt - offy - daty) - imy) + zad;
   } else {
    x = (textp->xt - imx - datx - offx)/(-zim) + winx;
    if(yup == 0)
      y = winy + (textp->yt - imy - daty - offy)/(-zim);
    else
      y = winy + (imh-1 - (textp->yt - offy - daty) - imy)/(-zim);
   }
   textlen = strlen(textp->text);
   XSetForeground(dpy,textgc,whitePixel(dpy,screen));
   XDrawImageString(dpy,wimage,textgc,x,y,textp->text,textlen);
   XFlush(dpy);
   xtlast = x;
   ytlast = y;
   textp++;
  }
}

imagevreplay()
{
  struct imagevector *vecp;
  int i, x, y, zad;

  vecp = veclist;
  zad = (zim - 1) / 2;
  vlastcolor=-1;
  for (i = 0; i < vcount; i++) {
    if(zim > 0) {
      x = zim * (vecp->xv - imx - datx - offx) + winx + zad;
      if(yup == 0)
        y = winy + zim * (vecp->yv - imy - daty - offy) + zad;
      else
        y = winy + zim * (imh-1 - (vecp->yv - offy - daty) - imy) + zad;
    } else {
      x = (vecp->xv - imx - datx - offx)/(-zim) + winx;
      if(yup == 0)
        y = winy + (vecp->yv - imy - daty - offy)/(-zim);
      else
        y = winy + (imh-1 - (vecp->yv - offy - daty) - imy)/(-zim);
    }
    if (vecp->color >= 0) {
      if(vecp->color != vlastcolor) {
        XSetForeground(dpy, vectorgc, vcolor[vecp->color].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 2: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      vlastcolor = vecp->color;
      }
      XDrawLine(dpy,wimage,vectorgc,xvlast,yvlast,x,y);
    }

    xvlast = x;
    yvlast = y;
    vecp++;
  }
  return (0);
}

char *uppermess1=NULL, *lowermess1=NULL;
char *uppermess2=NULL, *lowermess2=NULL;

imagelight(upper,mess,color)
int upper;                      /* flag = -1 to redraw messages */
			/* flag = 1 for upper right status light */
			/* flag = 2 for lower right status light */
			/* flag = 3 for upper left status light */
			/* flag = 4 for lower left status light */
char *mess;                     /* String to be written (null terminated) */
int color;                      /* flag = 0/1 for red/green */
{
  Window *w;
  int x, y, up, in;
  static int oldcolor[3] = {0,0,0};

  if (lgtwidth<=10) return(0);

  y = fontheight + (lgtheight-fontheight)/2 - 1;

  if(upper == (-1)) {
    if(uppermess1 != NULL) {
      x = (lgtwidth-strlen(uppermess1)*fontwidth)/2;
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[0]].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 3: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XFillRectangle(dpy,wlgt1,textgc,0,0,lgtwidth,lgtheight);
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[0]+1].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 4: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XDrawString(dpy,wlgt1,textgc,x,y,uppermess1,strlen(uppermess1));
    }
    if(lowermess1 != NULL) {
      x = (lgtwidth-strlen(lowermess1)*fontwidth)/2;
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[1]].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 5: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XFillRectangle(dpy,wlgt2,textgc,0,0,lgtwidth,lgtheight);
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[1]+1].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 6: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XDrawString(dpy,wlgt2,textgc,x,y,lowermess1,strlen(lowermess1));
    }
    if(uppermess2 != NULL) {
      x = (lgtwidth-strlen(uppermess2)*fontwidth)/2;
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[2]].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 7: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XFillRectangle(dpy,wlgt3,textgc,0,0,lgtwidth,lgtheight);
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[2]+1].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 8: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XDrawString(dpy,wlgt3,textgc,x,y,uppermess2,strlen(uppermess2));
    }
    if(lowermess2 != NULL) {
      x = (lgtwidth-strlen(lowermess2)*fontwidth)/2;
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[3]].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 9: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XFillRectangle(dpy,wlgt4,textgc,0,0,lgtwidth,lgtheight);
      XSetForeground(dpy,textgc,stcolor[2*oldcolor[3]+1].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 10: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
      XDrawString(dpy,wlgt4,textgc,x,y,lowermess2,strlen(lowermess2));
    }
  XFlush(dpy);
  return(0);
  }

  oldcolor[upper-1] = color;
  x = (lgtwidth-strlen(mess)*fontwidth)/2;

  if (upper == 1) {
    w = &wlgt1;
    uppermess1 = mess;
  } else if (upper==2) {
    w = &wlgt2;
    lowermess1 = mess;
  } else if (upper==3) {
    w = &wlgt3;
    uppermess2 = mess;
  } else if (upper==4) {
    w = &wlgt4;
    lowermess2 = mess;
  }

  XSetForeground(dpy,textgc,stcolor[2*color].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 11: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
  XFillRectangle(dpy,*w,textgc,0,0,lgtwidth,lgtheight);
  
  XSetForeground(dpy,textgc,stcolor[2*color+1].pixel);
  if (_XErrorEvent.serial!=0) 
   printf("loc 12: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
  XDrawString(dpy,*w,textgc,x,y,mess,strlen(mess));

  XFlush(dpy);
}

/*
* Blocking read from the display of one character
*/
imageread(x,y,key)
int *x, *y;             /* User coordinates where the key was struck */
char *key;              /* ASCII key entered */
{
#if defined(__alpha) || defined(__solaris)
  size_t nbytes = 1;
  ssize_t tmp;
#else
  int nbytes = 1;
  int tmp;
#endif

  waiting_for_key = 1;            /* Set flag for interupt routine */

  xtv_refresh(0);
  tmp = read(from_display,key,nbytes);

  if (tmp > 0) {
    *x = lastx;
    *y = lasty;
    waiting_for_key = 0;
    return(0);
  } else {
    fprintf (stderr,"Program-display pipe failure...\n");
    waiting_for_key = 0;
    return(ERR_BAD_DISPLAY_PIPE);
  }
}

/*
* Update the image and window dimensions for a new image
*/
updateimage(x0,y0,nx,ny,awidth,a,x1,y1)
int x0, y0;                     /* location of image in data */
int nx, ny;                     /* size of image to be displayed */
int awidth;                     /* pixels/line in data */
float *a;                       /* new data array */
int x1, y1;                     /* offset of data coordinate system */
{
  data = sdata[store] = a;
  datw = sdatw[store] = awidth;
  dath = sdath[store] = ny;
  imw = simw[store] = nx;
  imwidth = simwidth[store] = ROUNDUP(imw);
  /*  fprintf(stderr,"imw = %d imwidth = %d \n",imw,imwidth); */
  imh = simh[store] = ny;
  zim = szim[store] = 1;
  offx = soffx[store] = x1;
  offy = soffy[store] = y1;
  datx = sdatx[store] = x0;
  daty = sdaty[store] = y0;
  winw = swinw[store] = MIN(imw,width);
  winh = swinh[store] = MIN(imh,height);
  imx = simx[store] = MAX(0,(imw-winw)/2);
  imy = simy[store] = MAX(0,(imh-winh)/2);
  winx = swinx[store] = MAX(0,(width-imw)/2);
  winy = swiny[store] = MAX(0,(height-imh)/2);
  imtv = simtv[store] = imtvnum_;

}

releasetv_(loc)
float **loc;

{
  int i;

  if (tvinit == 0) return(0);

  if (data == *loc) {
    data = NULL;
    lights(-2);
  }

  for (i=0; i<MAXSTORE; i++) 
    if (sdata[i] == (float *)*loc) sdata[i] = NULL;
}

updatestore()
{
  data = sdata[store];
  datw = sdatw[store];
  dath = sdath[store];
  imw = simw[store];
  imwidth = simwidth[store];
  imh = simh[store];
  offx = soffx[store];
  offy = soffy[store];
  datx = sdatx[store];
  daty = sdaty[store];
  /* zim = szim[store];
  imx = simx[store];
  imy = simy[store];
  winx = swinx[store];
  winy = swiny[store]; */
  if (zim>0) {
    winw = MIN(width-winx,zim*(imw-imx));
    winh = MIN(height-winy,zim*(imh-imy));
  } else {
    winw = MIN(width-winx,(imw-imx)/(-zim));
    winh = MIN(height-winy,(imh-imy)/(-zim));
  }
  imtv = imtvnum_ = simtv[store];
}

/*
* Update the centering of the image, center it on image (x,y) and zoom it in
* for in = 1, out for in = -1
*/
updatepan(xc,yc,in)
int xc, yc, in;
{
#define MAXZOOMFACTOR 32
  if(in == 1) {
    if(zim >= 1) zim = MIN(MAXZOOMFACTOR,zim*2);
    else if(zim == -2) zim = 1;
    else zim = zim/2;
  } else if(in == -1) {
    if(zim > 1) zim = zim/2;
    else if(zim == 1) zim = -2;
    else zim = MAX(-MAXZOOMFACTOR,zim*2);
  } else if(in == 2) {
    zim = MAXZOOMFACTOR;
  }
  if(zim > 0) {
    imx = MAX(0,xc-width/(2*zim));
    imy = MAX(0,yc-height/(2*zim));
    winx = MAX(0,zim*(width/(2*zim)-xc));
    winy = MAX(0,zim*(height/(2*zim)-yc));
    winw = MIN(width-winx,zim*(imw-imx));
    winh = MIN(height-winy,zim*(imh-imy));
  } else {
    imx = MAX(0,xc-(-zim*width)/2);
    imy = MAX(0,yc-(-zim*height)/2);
    winx = MAX(0,((-zim*width)/2-xc)/(-zim));
    winy = MAX(0,((-zim*height)/2-yc)/(-zim));
    winw = MIN(width-winx,(imw-imx)/(-zim));
    winh = MIN(height-winy,(imh-imy)/(-zim));
  }
  lights(-4);
}

/*
* Update all the windows to a new overall size
*/
updatesize(wid,hgt)
int wid, hgt;
{
/* Set width and height to the size of the window */

  wid = MAX(wid,200);
  hgt = MAX(hgt,200);

  XResizeWindow(dpy,wbase,wid,hgt+XYZHEIGHT);

  newsizesubwin(wid,hgt);
  resizesubwin();
}

newsizesubwin(wid,hgt)
int wid, hgt;
{
  int i;

  /* size of image window */
  width = wid;
  height = hgt;

  /* xyz window width */
  xyzwidth = MIN(XYZWIDTH*fontwidth,width-30-4*XYZBORDER);
  xyzheight = MIN(height,XYZHEIGHT-2*XYZBORDER);

  /* light window width */
  lgtwidth = MIN((MAXMESS+1)*fontwidth,width);

  /* palette window width */
  palwidth = MAX(30,width-xyzwidth-4*XYZBORDER-2*lgtwidth);
  for(i=0;i<MIN(MAXPALWIDTH,palwidth);i++)
    palette[i] = cmap[(i*ncolors)/palwidth].pixel;

  /* readjust lgtwidth to make things fit (if we had to use min palwidth=30)*/
  lgtwidth = (width-xyzwidth-palwidth-4*XYZBORDER)/2;
  if (lgtwidth < MAXMESS*fontwidth) {
    lgtwidth = 1;
    palwidth = MAX(30,width-xyzwidth-4*XYZBORDER-2*lgtwidth);
    for(i=0;i<MIN(MAXPALWIDTH,palwidth);i++)
      palette[i] = cmap[(i*ncolors)/palwidth].pixel;
  }


  palheight = xyzheight;
  lgtheight = xyzheight/2;
  xyzx = -1;
  xyzy = height+1;

  palx = xyzx + xyzwidth + XYZBORDER;
  paly = xyzy;

  lgtx1 = palx + palwidth + XYZBORDER;
  lgtx2 = lgtx1 + lgtwidth + XYZBORDER;
  lgty = xyzy + XYZHEIGHT/2;

  textx = fontwidth;
  texty2 = fontheight + (xyzheight-2*fontheight)/3 - 1; 
  texty = 2*fontheight + (xyzheight-2*fontheight)*2/3 - 1; 
}

resizesubwin()
{
  XMoveResizeWindow(dpy,wimage,0,0,width,height);
  XMoveResizeWindow(dpy,wxyz,xyzx,xyzy,xyzwidth,xyzheight);
  XMoveResizeWindow(dpy,wlgt1,lgtx2,xyzy,lgtwidth,lgtheight);
  XMoveResizeWindow(dpy,wlgt2,lgtx2,lgty,lgtwidth,lgtheight);
  XMoveResizeWindow(dpy,wlgt3,lgtx1,xyzy,lgtwidth,lgtheight);
  XMoveResizeWindow(dpy,wlgt4,lgtx1,lgty,lgtwidth,lgtheight);
  XMoveResizeWindow(dpy,wpal,palx,paly,palwidth,palheight);
}

updatecoords(wx,wy,key)
int wx, wy, key;
{
  float mousez;
  int x,y,imousez;

  /*  First draw lower string with x, y, value, object name           */

  /*  If cursor is out of window, just display buffer number and name */
  if(wx<winx || wx>=(winx+winw) || wy<winy || wy>= (winy+winh)) {
   if (data != NULL) 
      sprintf(xyzstring,"                    %-20.20s\0",imname[store]); 
  }
  else {

    x = USERXCOORD(wx);
    y = USERYCOORD(wy);

    if(data == NULL) {
      sprintf(xyzstring,"%4d %4d           NO DATA            ",x,y);
    } else { 
      mousez = *(data+(y-offy)*datw+(x-offx));
      if(ABS(mousez) < 1e6 && ABS(mousez) > 1e-2) {
        imousez = ABS(mousez) + 0.5;
        if(imousez >= 1000) {
          if(mousez < 0) imousez = -imousez;
          sprintf(xyzstring,
                  "%4d %4d%7d    %-20.20s\0",x,y,imousez,imname[store]);
        }
        else if(imousez >= 100)
          sprintf(xyzstring,
                  "%4d %4d%7.1f    %-20.20s\0",x,y,mousez,imname[store]);
        else if(imousez >= 10)
          sprintf(xyzstring,
                  "%4d %4d%7.2f    %-20.20s\0",x,y,mousez,imname[store]);
        else
          sprintf(xyzstring,
                  "%4d %4d%7.3f    %-20.20s\0",x,y,mousez,imname[store]);
      } else {
        sprintf(xyzstring,
                  "%4d %4d %7.2e    %-20.20s\0",x,y,mousez,imname[store]);
      }
    }

    if(key >= 0 && key <= 127 && keyaction[key].echo == 1)
    xyzstring[17] = key;
  }

  XSetForeground(dpy,textgc,whitePixel(dpy,screen));
  if (_XErrorEvent.serial!=0) 
   printf("loc 13: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
  XDrawImageString(dpy,wxyz,textgc,textx,texty,xyzstring,strlen(xyzstring));

  /*  Now draw upper label with headings and buffer number     */
  if(wx<winx || wx>=(winx+winw) || wy<winy || wy>= (winy+winh)) {
    if (data != NULL) { 
      sprintf(xyzstring,"                    BUFFER %2d\0",imtv);
    } else
      sprintf(xyzstring,"                             \0 ");
  }
  else 
    if (data != NULL) {
      sprintf(xyzstring,"  X    Y   VALUE    BUFFER %2d\0",imtv);
    } else
      sprintf(xyzstring,"                             \0 ");
  
  XSetForeground(dpy,textgc,whitePixel(dpy,screen));
  if (_XErrorEvent.serial!=0) 
   printf("loc 14: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
  XDrawImageString(dpy,wxyz,textgc,textx,texty2,xyzstring,strlen(xyzstring));
  XFlush(dpy);
  /*
  fprintf(stderr,"Mouse at %d %d, image = %d\n",x,y,*(image+y*datw+x));
  */
}

updatebrkpt(x,ibut)
int x, ibut;
{
  if(x<0 || x>=ncolors) return;
  if(x == 0) {
    strncpy(xyzstring," z<                  \0",40);
    brkwrite(xyzstring+4,palbrkpt[0]);
  } else if(x == ncolors-1) {
    strncpy(xyzstring," z>=                  \0",40);
    brkwrite(xyzstring+5,palbrkpt[ncolors-2]);
  } else {
    strncpy(xyzstring,"       <=z<         \0",40);
    brkwrite(xyzstring,palbrkpt[x-1]);
    brkwrite(xyzstring+11,palbrkpt[x]);
  }
  
  XSetForeground(dpy,textgc,whitePixel(dpy,screen));
  if (_XErrorEvent.serial!=0) 
   printf("loc 15: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
  XDrawImageString(dpy,wxyz,textgc,textx,texty,xyzstring,strlen(xyzstring));
  
  sprintf(xyzstring,"                    BUFFER %2d\0",imtv);
  XSetForeground(dpy,textgc,whitePixel(dpy,screen));
  if (_XErrorEvent.serial!=0) 
   printf("loc 16: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
  XDrawImageString(dpy,wxyz,textgc,textx,texty2,xyzstring,strlen(xyzstring));
  XFlush(dpy);
  /*
  fprintf(stderr,"Mouse at %d %d, image = %d\n",x,y,*(image+y*datw+x));
  */
}

brkwrite(s,b)
char *s;
float b;
{
  int ib;
  char buf[8];
  if(ABS(b) < 1e6 && ABS(b) > 1e-2) {
    ib = ABS(b) + 0.5;
    if(ib >= 1000) {
      if(b < 0) ib = -ib;
      sprintf(buf,"%7d",ib);
    }
    else if(ib >= 100)
      sprintf(buf,"%7.1f",b);
    else if(ib >= 10)
      sprintf(buf,"%7.2f",b);
    else
      sprintf(buf,"%7.3f",b);
  } else {
    sprintf(buf,"%7.2e",b);
  }
  strncpy(s,buf,7);
}

newcolors(n1,n2)
int n1, n2;
{
  int i, k, i1, i2;

  /*  fprintf(stderr,"newcolors n1: %d   n2: %d \n", n1, n2);  */

  n1 = (n1 + npalette) % npalette;
  n2 = (n2 + npalette) % npalette;
  i1 = (n1*ncolors)/npalette;
  i2 = (n2*ncolors)/npalette;
  if(i2 == i1) i2 = (i2+ncolors-1) % ncolors;
  if(i2 < i1) i2 += ncolors;

  for(i=MAX(0,i2+1-ncolors);i<MAX(ncolors,i2+1);i++) {
    if(i < i1) k = 0;
    else if(i > i2) k = npalette-1;
    else k = MAX(0,MIN(npalette-1,(npalette*(i-i1))/(i2-i1)));
    cmap[i%ncolors].red = palcolors[k].red;
    cmap[i%ncolors].green = palcolors[k].green;
    cmap[i%ncolors].blue = palcolors[k].blue;
  /*    fprintf(stderr,"%d %d %d %d %d %d \n",i, i%ncolors,
    cmap[i%ncolors].red,cmap[i%ncolors].green,
    cmap[i%ncolors].blue,cmap[i%ncolors].pixel);  */
  }

  /*  fprintf(stderr,"%d %d %d %d %d \n",n1,n2,i1,i2,ncolors); */

  if (!truecolor) XStoreColors(dpy, defcmap, cmap, ncolors); 
  XFlush(dpy);
  pal0 = n1;
  pal1 = n2;
}

static int whichbreak;

updatenewpal(x0,ibut)
int x0, ibut;
{

if(ibut == 2) whichbreak = x0 - pal0;
else {
if(ABS(x0-pal0) < ABS(x0-pal1)) whichbreak = 0;
else whichbreak = 1;
}

/*  fprintf(stderr,"updatepal: x = %d ibut = %d whichbreak = %d pal0 = %d pal1 = %d\n",x0,ibut,whichbreak,pal0,pal1); */

}

updatepal(x,ibut)
int x, ibut;
{
if(ibut == 2)
  newcolors(x-whichbreak,x-whichbreak+pal1-pal0);
else {
  if(whichbreak==0) {
/*      if(x>=pal1) return; */
    newcolors(x,pal1);
  } else {
/*      if(x<=pal0) return; */
  newcolors(pal0,x);
  }
}

/*  fprintf(stderr,"updatepal: x = %d ibut = %d whichbreak = %d pal0 = %d pal1 = %d\n",x,ibut,whichbreak,pal0,pal1); */

}

static int zoombytes=0;

updatezoom(xw,yw)
int xw, yw;
{
  int x, y, i, j, f, n, lc, m;
  int x0, y0, x1, y1, w, h;

  if(xw<winx || xw>=(winx+winw) || yw<winy || yw>= (winy+winh)) return;

  if(zim > 0) {
    x = (xw-winx)/zim + imx;
    y = (yw-winy)/zim + imy;
  } else {
    x = (-zim)*(xw-winx) + imx;
    y = (-zim)*(yw-winy) + imy;
  }

  f = zoomf;
  n = zoomf*zwidth;
  m = zoomf*zheight;

  if(zoombytes != n*m) {
    if(zoombytes > 0) free(zimage);
    zoombytes = n*m;
    if((zimage = (unsigned char *)malloc(zoombytes))==NULL) {
	 fprintf(stderr,"Can't allocate zoom array\n");
	 return(ERR_BAD_ALLOC_ZOOM);
       }
    lastzoomx = lastzoomy = -100000;
    zoomimage->data = (char *)zimage;
  }

  replicate(x-zwidth/2,y-zheight/2,imwidth,imh,   image[store],
	    f,
	    0,0,n,m,n,   zimage,
	    1);

/* j is the location of the top left of the central pixel */
  j = n*f*(zheight/2) + f*(zwidth/2);
  zcursor(j);

  zoomimage->width = n;
  zoomimage->height = f*zheight;
  zoomimage->bytes_per_line = n;

  XPutImage(dpy,wzoom,imagegc,zoomimage,0,0,0,0,n,f*zheight);

  lastzoomx = xw;
  lastzoomy = yw;
}

oldzcursor(j)
/* j is the location of the top left of the central pixel */
int j;
{
  int i, f, n, a1, a2, a3, a4;
  f = zoomf;
  n = zoomf*zwidth;
  a1 = j - n - 1;
  for(i=0;i<4*(f+1);i++) {
    zimage[a1] = ((i%2) == 0) ?
      blackPixel(dpy,screen) : whitePixel(dpy,screen);
    if(i<f+1) a1++;
    else if(i<2*f+2) a1 += n;
    else if(i<3*f+3) a1--;
    else a1 -= n;
  }
  a1 = j - n + f;
  a2 = a1 - f - 1;
  a3 = a2 + (f+1)*n;
  a4 = a3 + f + 1;
  for(i=0;i<8;i++) {
    zimage[a1] = zimage[a2] = zimage[a3] = zimage[a4] = ((i%2) == 0) ?
      blackPixel(dpy,screen) : whitePixel(dpy,screen);
    a1 -= (n-1);
    a2 -= (n+1);
    a3 += (n-1);
    a4 += (n+1);
  }
}


zcursor(j)
/* j is the location of the top left of the central pixel */
int j;
{
  int i, f, n, a1, a2, a3, a4;
  f = zoomf;
  n = zoomf*zwidth;
  a1 = j - n - 1;
  for(i=0;i<4*(f+1);i++) {
    zimage[a1] = ((i%2) == 0) ?
      blackPixel(dpy,screen) : whitePixel(dpy,screen);
    if(i<f+1) a1++;
    else if(i<2*f+2) a1 += n;
    else if(i<3*f+3) a1--;
    else a1 -= n;
  }
  a1 = j - n + f;
  a2 = a1 - f - 1;
  a3 = a2 + (f+1)*n;
  a4 = a3 + f + 1;
  for(i=0;i<8;i++) {
    zimage[a1] = zimage[a2] = zimage[a3] = zimage[a4] = ((i%2) == 0) ?
      blackPixel(dpy,screen) : whitePixel(dpy,screen);
    a1 -= (n-1);
    a2 -= (n+1);
    a3 += (n-1);
    a4 += (n+1);
  }
}

/*
 * writepix(x,y,wid,hgt) writes a portion of the image to the image window
 * The region displayed should start at (x,y) in the window and extend for
 * (wid,hgt)
 */

writepix(x,y,wid,hgt)
int x, y, wid, hgt;
{
  int i0, j0, x0, y0, x1, y1, w0, h0;
  int i, imageoffset;

/* First, calculate where the display should actually be */
/* We will not bother clearing the window first (for the time being) */
/* (i0,j0) are the UL pixel of image, (x0,y0) are its location in the window */
/* (x1,y1) are the LR pixel of image + 1 in each dimension */

/*
  fprintf(stderr,"writepix: x, y, w, h = %5d %5d %5d %5d\n",x,y,wid,hgt);
  fprintf(stderr,"winx, winw, winy, winh, height: = %5d %5d %5d %5d %5d\n",
winx,winw,winy,winh,height);
*/

/* Return if no data available yet */
  if(zim == 0) return;

  if(zim > 0) {
    x0 = MAX(zim*(x/zim),winx);
    y0 = MAX(zim*(y/zim),winy);
    x1 = MIN(zim*((x+wid+zim-1)/zim),winx+winw);
    y1 = MIN(height,MIN(zim*((y+hgt+zim-1)/zim),winy+winh));

    i0 = (x0-winx)/zim + imx;
    j0 = (y0-winy)/zim + imy;
  } else {
    x0 = MAX(x,winx);
    y0 = MAX(y,winy);
    x1 = MIN(x+wid,winx+winw);
    y1 = MIN(height,MIN(y+hgt,winy+winh));

    i0 = (-zim)*(x0-winx) + imx;
    j0 = (-zim)*(y0-winy) + imy;
  }

/*
  fprintf(stderr,"  x0 = %6d    y0 = %6d\n",x0,y0);
  fprintf(stderr,"  x1 = %6d    y1 = %6d\n",x1,y1);
*/
  if( x1 <= x0 || y1 <= y0) return;

/* (w0,h0) are the size of the displayed piece of image */
  w0 = (x1-x0)/zim;
  h0 = (y1-y0)/zim;

/*
  fprintf(stderr,"   x = %6d     y = %6d -----\n",x,y);
  fprintf(stderr," wid = %6d   hgt = %6d\n",wid,hgt);
  fprintf(stderr," imw = %6d   imh = %6d\n",imw,imh);
  fprintf(stderr,"winw = %6d  winh = %6d\n",winw,winh);
  fprintf(stderr," imx = %6d   imy = %6d\n",imx,imy);
  fprintf(stderr,"winx = %6d  winy = %6d\n",winx,winy);
  fprintf(stderr," zim = %6d ",zim);
  fprintf(stderr,"  x0 = %6d    y0 = %6d\n",x0,y0);
  fprintf(stderr,"  x1 = %6d    y1 = %6d\n",x1,y1);
  fprintf(stderr,"  i0 = %6d    j0 = %6d\n",i0,j0);
  fprintf(stderr,"  w0 = %6d    h0 = %6d\n",w0,h0); 
*/
  dataimage->width = imw;
  dataimage->bytes_per_line = imwidth;
/*
  if(zim == 17) {
    dataimage->data = (char *)image[store];
    dataimage->height = y1-y0;

    XPutImage(dpy,wimage,imagegc,               
	      dataimage,
	      i0,j0,     x0,y0,                 
	      ROUNDUP(x1-x0),y1-y0);            

  } else {
*/
    dataimage->width = ROUNDUP(x1-x0);
    dataimage->height = y1 - y0;
    dataimage->bytes_per_line = ROUNDUP(x1-x0);
    dataimage->data = (char *)imbuf;

    if (truecolor) {
      dataimage->bytes_per_line = ROUNDUP(x1-x0)*2;
      dataimage->bits_per_pixel = 16;
      dataimage->depth = 16;
      dataimage->format = ZPixmap;
      dataimage->red_mask = rmask;
      dataimage->green_mask = gmask;
      dataimage->blue_mask = bmask;
    }

/* Write the appropriately zoomed data into imbuf from image */
    if(zim == 1) {
      duplicate(i0, j0, imwidth, imh,  image[store], 0, 0,
                ROUNDUP(x1-x0), y1-y0,
                ROUNDUP(x1-x0),  imbuf, 1);
    } else if(zim > 1) {
      replicate(i0, j0, imwidth, imh,  image[store], zim, 0, 0,
                ROUNDUP(x1-x0), y1-y0,
                ROUNDUP(x1-x0),  imbuf, 1);
    } else if (zoomsample==1) {
      samplicate(i0, j0, imwidth, imh,  image[store], -zim, 0, 0,
                 ROUNDUP(x1-x0), y1-y0,
                 ROUNDUP(x1-x0),  imbuf, 1);
    } else {
      binlicate(i0, j0, imwidth, imh,  image[store], -zim, 0, 0,
                 ROUNDUP(x1-x0), y1-y0,
                 ROUNDUP(x1-x0),  imbuf, 1);
    }
    XPutImage(dpy,wimage,imagegc,  
	      dataimage,0,0,x0,y0,ROUNDUP(x1-x0),y1-y0); 
/*
  }
*/
  imagevreplay();
  imagetreplay();
  XFlush(dpy);

}

/*
 * Interrupt handler for X Events
 */

xtv_refresh(signo)
int signo;
{
  Window wmouse, wroot;
  XEvent event;
  XExposeEvent *expw  = (XExposeEvent *)&event;
  XButtonEvent *but = (XButtonEvent *)&event;
  XKeyEvent *key = (XKeyEvent *)&event;
  XPointerMovedEvent *pmove = (XPointerMovedEvent *)&event;
  XColormapEvent *cevent = (XColormapEvent *)&event;
  KeySym ks;
  int wmask;
#define KEYLEN 10
  char keystring[KEYLEN];
  XComposeStatus compose_status;
  int x=0, y=0, i, ix, iy, set, iii;
  static int ibut;
  unsigned char keycode;
  char keychar;
  char outbuf[32];
  float fcoord;

  whichkey = -1;


#define ALL_X_EVENTS   (~0)

while (
    /* Should we stay in this while loop indefinitely, or return to caller? */
    (waiting_for_key == 1) ?
    /* Stay in this while loop until key/button press or Config or Expose */
    (XWindowEvent(dpy,wbase,ALL_X_EVENTS,&event), 1) :
    /* Return to caller as soon as X event queue is emptied */
    XCheckWindowEvent(dpy,wbase,ALL_X_EVENTS,&event) || 
    XCheckWindowEvent(dpy,wzoom,ALL_X_EVENTS,&event)  
) {

/*    fprintf(stderr," got event w = %d, x,y,w,h = %d %d %d %d type = %d\n", 
	    expw->window,expw->x,expw->y,expw->width,expw->height,event.type);  */
/*
      switch((int)event.type) {
      case ColormapNotify:
        fprintf(stderr,"%d %d %d\n",cevent->send_event, cevent->new, cevent->state);
        XInstallColormap(dpy, defcmap);
      }
*/


/* If the event came from the zoom window, update it */
    if(zoomf != 0 && expw->window == wzoom && imagevalid) {
      switch((int)event.type) {
      case Expose:
	zoomf = ABS(zoomf);
	XGetGeometry(dpy,wzoom,&inforoot,&infox,&infoy,&infowidth,&infoheight,
		     &infoborder,&infodepth);
	zwidth = (infowidth+zoomf-1) / MAX(1,zoomf);
	zheight = (infoheight+zoomf-1) / MAX(1,zoomf);
/*      fprintf(stderr,"zoom window exposed\n"); */
	if(zoomf >= 0 && !zfreezeon) updatezoom(mousex,mousey);
	break;
      case UnmapNotify:
/*      fprintf(stderr,"zoom window unmapped\n"); */
	zoomf = -ABS(zoomf);
	break;
      case ButtonPress:
	whichkey = but->button;
	if(whichkey == Button2) zoomf = MAX(zoomf-1,1);
	if(whichkey == Button1) zoomf++;
	XGetGeometry(dpy,wzoom,&inforoot,&infox,&infoy,&infowidth,&infoheight,
		     &infoborder,&infodepth);
	zwidth = (infowidth+zoomf-1) / MAX(1,zoomf);
	zheight = (infoheight+zoomf-1) / MAX(1,zoomf);
/* fprintf(stderr,"Updating zoom... %d %d\n",mousex,mousey); */
	if(zoomf > 0 && !zfreezeon) updatezoom(lastzoomx,lastzoomy);
	XFlush(dpy);
      }

/* Otherwise the event came from the base window */
    } else {

/* Switch on the type of event */
      switch((int)event.type) {

/*
 * WINDOW EXPOSURE EVENT
 */
      case Expose:
      case ConfigureNotify:
/*      case VisibilityNotify: */
/*      XQueryWindow(wbase,&winfo); */
/* If the event came from a subwindow, dump it for now */
/*
	fprintf(stderr,"Exposure event: T = %d (E = %d, V = %d); W = %d (I = %d, B = %d);  x = %d y = %d w = %d h = %d n = %d\n",
		(int)event.type,Expose,VisibilityNotify,expw->window,wimage,wbase,
		expw->x,expw->y,expw->width,expw->height,expw->count);
*/
/*      if(expw->window != wimage) break; */

/* If the window was resized, update the subwindow sizes */
#ifdef __HAIRS
        if (usehairs && hairs_on) vnohair();
#endif
	XGetGeometry(dpy,wbase,&inforoot,&infox,&infoy,&infowidth,&infoheight,
		     &infoborder,&infodepth);
	if(infowidth != width || infoheight != height+XYZHEIGHT) {
	  newsizesubwin(infowidth,infoheight-XYZHEIGHT);
	  resizesubwin();
	  updatepan(imw/2,imh/2,0);
	  XClearWindow(dpy,wbase);
	}

/* Rewrite the required bit of image */

/*      writepix(winfo.x,winfo.y,winfo.width,winfo.height); */
	if(imagevalid) {
	  writepix(expw->x,expw->y,expw->width,expw->height);
	}

	for(i=0;i<palheight;i++) {
	  XPutImage(dpy,wpal,imagegc,palimage,0,0,0,i,palwidth,1);
	}
/*	imagelight(-1,NULL,0); */
	break;


/*
 * BUTTON PRESSED: A mouse button was pressed
 */
      case ButtonPress:
	whichkey = but->button;
/*      fprintf(stderr,"Button pressed  %d \n", whichkey); */
	ibut = 0;
	if(whichkey == Button1) ibut = 1;
	if(whichkey == Button2) ibut = 2;
	if(whichkey == Button3) ibut = 3;
	lastx = but->x;
	lasty = but->y;
	buttondown = 1;

/* The button was pressed in the image window, zoom and center */
	if(but->subwindow == wimage) {
	  if(keyaction[ibut].action != NULL) {
	    (*(keyaction[ibut].action))
	      (but->x,but->y,USERXCOORD(but->x),USERYCOORD(but->y),ibut);
	  }
	}

/* The button was pressed in the palette window */
	else if(but->subwindow == wpal) {
#ifdef __HAIRS
          if (usehairs && hairs_on) vnohair();
#endif
	  updatenewpal((npalette*(but->x-palx))/palwidth,ibut);
	}
	break;

/*
 * BUTTON RELEASED: The button was released
 */
      case ButtonRelease:
	buttondown = 0;
	break;

/*
 * KEY PRESSED: A Keyboard key was pressed
 */
      case KeyPress:

/* Only accept 'r' key from palette subwindow   */
	XLookupString(key,keystring,KEYLEN,&ks,&compose_status);
/*        ks = XLookupKeysym(key,0); */
	if (key->subwindow == wpal) {
	  if (keystring[0] == 'r' || keystring[0] == 'R')
	    newcolors(0,npalette-1);
	  keystring[0] = '\0';
	}

/* Ignore keyboard events in subwindows */
	if(key->subwindow != wimage) break;

/* Ignore modifier keys */
	if(IsModifierKey(ks)) break ;

/* If an arrow key, move the mouse */
	if(IsCursorKey(ks)) {
	  XQueryPointer(dpy,wbase,&wroot,&wmouse,
			&infox,&infoy,&mousex,&mousey,&wmask);
/*	  if(ks == XK_Left) mousex--;
	  else if(ks == XK_Right) mousex++;
	  else if(ks == XK_Down) mousey++;
	  else if(ks == XK_Up) mousey--; */
          if(ks == XK_Left) mousex -= MAX(zim,1);
          else if(ks == XK_Right) mousex += MAX(zim,1);
          else if(ks == XK_Down) mousey += MAX(zim,1);
          else if(ks == XK_Up) mousey -= MAX(zim,1);
/*	  fprintf(stderr,"Move mouse to %d %d  %d %d %d %d %d\n",mousex,mousey,
		  key->keycode,XK_Left,XK_Right,XK_Down,XK_Up); */
	  XWarpPointer(dpy,None,wimage,0,0,0,0,mousex,mousey);
	  updatecoords(mousex,mousey,-1);
	  XFlush(dpy);
	  break;
	}

/* Get the character typed */
	keychar = keystring[0];
/*      if(i == 0) break; */
	whichkey = keychar;
	if(whichkey >= 0 && whichkey <= 127 &&
	   keyaction[whichkey].action != NULL) {
	  (*(keyaction[whichkey].action))
	    (key->x,key->y,USERXCOORD(key->x),USERYCOORD(key->y),whichkey);
	}
	updatecoords(key->x,key->y,whichkey);
	keystring[0] = '\0';

/* waiting_for_key means write into pipe to program */
	if(waiting_for_key) {
	  *outbuf = keychar;
	  lastx = USERXCOORD(key->x);
	  lasty = USERYCOORD(key->y);
	  write(to_program,outbuf,1);
          waiting_for_key = 0;
	} 
	XFlush(dpy);
	break;

/*
 * MOUSE MOVED: Mouse movement
 */
      case LeaveNotify:
#ifdef __HAIRS
	if (usehairs && hairs_on) vnohair(); 
#endif
	break;

      case MotionNotify:
/* If mouse moved in image window with button down, ignore it */
	if(buttondown && pmove->subwindow == wimage) break;
	XQueryPointer(dpy,wbase,&wroot,&wmouse,
			&infox,&infoy,&mousex,&mousey,&wmask);
#ifdef __HAIRS
	if(usehairs && wmouse == wimage) {
              crshr[0] = crshr[2];
	      crshr[1] = crshr[3];
	      crshr[2].x1 = 0; 
	      crshr[2].x2 = width;
	      crshr[2].y1 = crshr[2].y2 = mousey;
	      crshr[3].y1 = 0;
	      crshr[3].y2 = height;
	      crshr[3].x1 = crshr[3].x2 = mousex;
              if (hairs_on) {
                XSetForeground(dpy, vectorgc, blackPixel(dpy,screen));
if (_XErrorEvent.serial!=0) 
 printf("loc 17: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
	        XDrawSegments(dpy,wimage,vectorgc,crshr,2);
                XSetForeground(dpy, vectorgc, vcolor[0].pixel);
if (_XErrorEvent.serial!=0) 
 printf("loc 18: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
	        writepix(crshr[0].x1,crshr[0].y1,width,1);
	        writepix(crshr[1].x1,crshr[1].y1,1,height);
	        XDrawSegments(dpy,wimage,vectorgc,crshr+2,2);
	      } else {
                XSetForeground(dpy, vectorgc, vcolor[0].pixel);
if (_XErrorEvent.serial!=0) 
 printf("loc 19: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
	        XDrawSegments(dpy,wimage,vectorgc,crshr+2,2);
	        hairs_on = True;
	      }
	}
#endif      /* HAIRS */

	if(wmouse == wpal) {
#ifdef __HAIRS
          if (usehairs && hairs_on) vnohair(); 
#endif
/* If mouse moved in palette window with button down, update palette */
	  if(buttondown) {
	    updatepal((npalette*(mousex-palx))/palwidth,ibut);
	    break;
	  } else {
/* If mouse moved in palette window tell what the breakpt is... */
	    updatebrkpt((ncolors*(mousex-palx))/palwidth,ibut);
	    break;
	  }
	}

/* Check out current position of mouse */

/* If mouse in image window, update coords and zoom */
	if(wmouse == wimage) {
/*        fprintf(stderr,"Updating coords to %d %d\n",mousex,mousey); */
	  updatecoords(mousex,mousey,-1);
/*        fprintf(stderr,"Updating zoom to %d %d\n",mousex,mousey); */
	  if(zoomf > 0 && !zfreezeon) updatezoom(mousex,mousey);
/*        fprintf(stderr,"Done with event\n"); */
	}
	break;
      } /* end switch on eventtype in base window */
    } /* end if (event came from base window) */
  } /* end of while(1) */
  return(0);
} /* end of function xtv_refresh() */

keyzoompan(x,y,inout)
int x, y, inout;
{
  int ix, iy;
  if(zim > 0) {
    ix = (x - winx)/zim + imx;
    iy = (y - winy)/zim + imy;
  } else {
    ix = (-zim)*(x - winx) + imx;
    iy = (-zim)*(y - winy) + imy;
  }
  XClearWindow(dpy,wimage);
  XWarpPointer(dpy,None,wimage,0,0,0,0,width/2,height/2);
  updatepan(ix,iy,inout);
  writepix(0,0,width,height);
  if(zoomf > 0 && !zfreezeon) updatezoom(width/2,height/2);
}

keyzoomin(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  keyzoompan(x,y,1);
}

keyzoomout(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  keyzoompan(x,y,-1);
}

keypan(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  keyzoompan(x,y,0);
}

keyrecenter(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  int maxdim, maxw, maxh;

  zim = 1;
  XClearWindow(dpy,wimage);
  XWarpPointer(dpy,None,wimage,0,0,0,0,width/2,height/2);
  updatepan(imw/2,imh/2,0);
/* If the image can be zoomed and fit it the display window, do it */
  maxdim = MAX(imw,imh);
  while (maxdim*2 <= MIN(width,height)) {
    updatepan(imw/2,imh/2,1);
    maxdim = maxdim*2;
  }
/* If image is too big for display, zoom out if option is set */
 if (autozoomout && (imw>width || imh>height)  ) {
    if (resize) {
      maxw = maxwidth;
      maxh = maxheight;
    } else {
      maxw = width;
      maxh = height;
    }
    while ( (imw/ABS(zim))>maxw || (imh/ABS(zim))>maxh )  {
      updatepan(imw/2,imh/2,-1);
    }
  }

  writepix(0,0,width,height);
  if(zoomf > 0 && !zfreezeon) updatezoom(width/2,height/2);
}

keyzoomprint(x,y,xuser,yuser,key)
int x, y, xuser, yuser, key;
{
  keyzoompan(x,y,2);
  imageprintval();
}

imageprintval()
{
  float z;
  int x0, y0, x, y, i0, i1, i, j0, j1, j, iz, printlen;
  char printstring[16];

  if(zim <= 0 || data == NULL) return;

/* Find the lower left pixel (i0,j0) */
  i0 = (-winx)/zim+imx+datx+offx;
  if(yup==0) j0 = (winh-winy)/zim+imy+daty+offy;
  else j0 = (imh-1-(winh-winy)/zim-imy)+daty+offy + 1;

/* Find the upper right pixel (i1,j1) */
  i1 = (winw-winx)/zim+imx+datx+offx;
  if(yup==0) j1 = (-winy)/zim+imy+daty+offy;
  else j1 = (imh-1-(-winy)/zim-imy)+daty+offy + 1;

/* Find the (x,y) location of the lower left pixel */
  x0 = zim * (i0 - imx - datx - offx) + winx + (zim - 1) / 2;
  if(yup == 0) y0 = winy + zim * (j0 - imy - daty - offy) + (zim - 1) / 2;
  else y0 = winy + zim * (imh-1 - (j0 - offy - daty) - imy) + (zim - 1) / 2;

/*  fprintf(stderr,"%4d %4d %4d %4d %4d %4d \n",i0,i1,j0,j1,x0,y0); */

  y = y0;
  XSetForeground(dpy,textgc,whitePixel(dpy,screen));
  for(j=j0;j<j1;j++) {
    x = x0;
    for(i=i0;i<i1;i++) {
/*
      fprintf(stderr,"%4d %4d %4d %4d %4d %4d %4d %4d %4d %4d \n",
      i,j,i0,i1,j0,j1,x,y,x0,y0);
*/
      if(i==i0 && j==j0) {
      } else if(i==i0) {
        sprintf(printstring,"* %d *",j);
      } else if(j==j0) {
        sprintf(printstring,"* %d *",i);
      } else {
        z = *(data+(j-offy)*datw+(i-offx));
        if(ABS(z) < 1e6 && ABS(z) > 1e-2) {
          iz = ABS(z) + 0.5;
/* Make it an integer no matter what 
          if(iz >= 1000) { */
          if(iz >= 000) { 
            if(z < 0) iz = -iz;
            sprintf(printstring,"%d",iz);
          }
          else if(iz >= 100)
            sprintf(printstring,"%.1f",z);
          else if(iz >= 10)
            sprintf(printstring,"%.2f",z);
          else
            sprintf(printstring,"%.3f",z);
        } else {
          sprintf(printstring,"%7.2e",z);
        }
      }
      printlen = strlen(printstring);
      XSetForeground(dpy,textgc,whitePixel(dpy,screen));
      XDrawImageString(dpy,wimage,textgc,
                       x-(fontwidth*printlen)/2,y+fontheight/2,
                       printstring,printlen);
      x = x + zim;
    }
    if(yup == 0) y = y + zim;
    else y = y - zim;
  }
}


tvblink_(a,b,nrow,ncol,nc,sr,sc,asr,asc,span,zero,flip,ibl)

float   *a,             /* Input floating image array                   */
	*b,             /* second floating array                        */
	*span,          /* Intensity mapping span level.  This level    */
			/* is used to scale the pixel data between 0    */
			/* and 254 for display on the Grinnell.         */
	*zero;          /* Intensity scale zero offest                  */

int     *nrow,          /* Number of rows to be displayed               */
	*ncol,          /* Number of columns to be displayed            */
	*nc,            /* Row length in pixels of the array            */
	*sr,            /* Row offset to first displayed row            */
	*sc,            /* Column offset to first displayed row         */
	*asr,           /* Row number of image array origin             */
	*asc,           /* Column number of image array origin          */
	*flip,          /* Flag for left-right image reflection         */
	*ibl;           /* Number of grey levels per image              */

{
	short color[128];
	int i, j, ii, iblink, ntoget, row, col;
	char *table, ch;
	register unsigned char *g;
	register float *pix,*pix2,      /* Pointer to within array      */
			lev;
	int jump, npix, ipart, nr, iif;
	int z, x, y, tvzoom_(), tvzoomc_();
	float val,val2;
	double junk;
	int nx, ny, x0, y0, awidth, xoff, yoff;

/*      Allocate the memory for the destination image                   */
	nx = *ncol;
	ny = *nrow;
	x0 = *sc;
	y0 = *sr;
	awidth = *nc;
	xoff = *asc;
	yoff = *asr;
	if (wbase == 0) {
	  fprintf(stderr,"windows not yet created\n");
	  return(ERR_NOT_INITIALIZED);
	}
	store = store+1>MAXSTORE-1 ? 0 : store+1;
	if (nimage[store] < ROUNDUP(nx)*ny || image[store] == NULL) {
	   if (image[store] != NULL) free(image[store] );
	   if ((image[store]  = (unsigned char *)malloc(ROUNDUP(nx)*ny)) == NULL) {
	     fprintf(stderr,"Can't allocate image array\n");
	     return(ERR_BAD_ALLOC_IMBUF);
	   }
	   nimage[store] = ROUNDUP(nx)*ny;
	}

/*      Initialize variables                                            */

	lights(-2);
	pix     = a + *sc + *sr * *nc;  /* Pointer to first pixel       */
	pix2    = b + *sc + *sr * *nc;  /* Pointer to first pixel       */
	jump    = *nc - *ncol;          /* Jump between rows            */

/*      Load up destination image with data from 2 images               */
	if (*span > 0.0) {
	g       = image[store] ;
	lev     = (*ibl * *ibl - 1) / *span;
	for (nr = *nrow; nr > 0; nr--) {
		for (npix = *ncol; npix > 0; npix--) {
			val     = ( *pix++  - *zero );
			val2    = ( *pix2++ - *zero );
			val = lev * (val - ((int)(val / *span) * *span));
			val2 = lev * (val2 - ((int)(val2 / *span) * *span));
			if (val < 0.0)
				val     =0.0;
			if (val2 < 0.0)
				val2    = 0.0;
			ipart   = (int)((val / *ibl) +
					(int)(val2 / *ibl) * *ibl);
			*g++    = pixels[ipart];
		}
		pix     += jump;
		pix2    += jump;
	}
	}

/*      This block of code just clips pixels instead of letting them wrap */

	else {
	iif = *ibl * *ibl - 1;
	lev     = iif / -*span;
	g       = image[store] ;
	for (nr = *nrow; nr > 0; nr--) {
		for (npix = *ncol; npix > 0; npix--) {
			val     = lev * ( *pix++  - *zero );
			val2    = lev * ( *pix2++ - *zero );
			if (val < 0.0)
				val     =0.0;
			else if (val > iif)
				val     = iif;
			if (val2 < 0.0)
				val2    = 0.0;
			else if (val2 > iif)
				val2    = iif;
			ipart   = (int)((val / *ibl) +
					(int)(val2 / *ibl) * *ibl);
			*g++    = pixels[ipart];
		}
		pix     += jump;
		pix2    += jump;
	}
	}

	updateimage(x0,y0,nx,ny,awidth,a,xoff,yoff);

	imagevalid = 1;
	imagevnull();

	if (resize && (width < nx || height < ny)) {
	  width = MIN(1024,MAX(width,nx));
	  height = MIN(865,MAX(height,ny));
	  updatepan(imw/2,imh/2,0);
	  updatesize(width,height);
	}
	else {
          if (zim>0)
	    writepix(winx, winy, zim*imw, zim*imh);
          else
	    writepix(winx, winy, imw/-zim, imh/-zim);
	  xtv_refresh(0);
	}
	XFlush(dpy);
	lights(-1);
	lights(2);
	lights(3);
	lights(4);

/*      Load the blink color tables and blink                           */

	iblink = 0;
	ch = ' ';
	printf("In TV window, enter E or Q to quit, anything else \
to blink:\n");
	ntoget = 1;
	while (ch != 'E' && ch != 'e' && ch != 'Q' && ch != 'q') {
	  if (iblink == 0) {
	    ii = 0;
	    for (i=0; i<*ibl; i++) {
	      for (j=0; j<*ibl; j++) {
		color[ii++] = (short)(j * (255. / *ibl));
	      }
	    }
	    updateimage(x0,y0,nx,ny,awidth,a,xoff,yoff);
	    imagepalette(*ibl* *ibl, color, color, color, 0);
	    iblink = 1;
	    mark_(&row,&col,&ch);
	  }
	  else {
	    ii = 0;
	    for (i=0; i<*ibl; i++) {
	      for (j=0; j<*ibl; j++) {
		color[ii++] = (short)(i * (255. / *ibl));
	      }
	    }
	    updateimage(x0,y0,nx,ny,awidth,b,xoff,yoff);
	    imagepalette(*ibl* *ibl, color, color, color, 0);
	    iblink = 0;
	    mark_(&row,&col,&ch);
	  }
	}

	return(0);

}
#ifdef __HAIRS
vnohair()
{
    crshr[0] = crshr[2];
    crshr[1] = crshr[3];
    crshr[2].x1 = crshr[2].x2 = crshr[2].y1 = crshr[2].y2 = -1;
    crshr[3].y1 = crshr[3].y2 = crshr[3].x1 = crshr[3].x2 = -1;
    XSetForeground(dpy, vectorgc, blackPixel(dpy,screen));
if (_XErrorEvent.serial!=0) 
 printf("loc 20: %d %s", _XErrorEvent.serial,_XErrorEvent.error_code);
    XDrawSegments(dpy,wimage,vectorgc,crshr,2);
    XSetForeground(dpy, vectorgc, vcolor[0].pixel);
    writepix(crshr[0].x1,crshr[0].y1,width,1);
    writepix(crshr[1].x1,crshr[1].y1,1,height);
}
#endif  /* HAIRS */
int blackPixel(Display *dpy, int screen)
{
  if (private)
    return(stcolor[3].pixel);
  else
    return(BlackPixel(dpy,screen));
}
int whitePixel(Display *dpy, int screen)
{
  if (private)
    return(stcolor[1].pixel);
  else
    return(WhitePixel(dpy,screen));
}

