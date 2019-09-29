/*
 * Include file of global static variables for zimage.c
 */
/*
 * DEFINITION OF X-WINDOWS WINDOW AND IMAGE
 *
 * (1) The base window "wbase" exists with size (width,height+XYZHEIGHT).
 *     In the portion not covered by subwindows the image is displayed.
 * (2) In the upper left is the image window "wimage" with size (width,height).
 *     This is where the images are displayed.
 * (3) At the lower left is the palette subwindow "wpal" with size 
 *     (palwidth,palheight).
 *     It displays the palette, and allows interactive palette control by
 *     dragging the mouse.
 * (4) At the lower middle are two light subwindows "wlgt1" (upper) and
 *     "wlgt2" with size (lgtwidth,lgtheigt).
 *     They display status information with text and color.
 * (5) At the lower right is the xyz display subwindow "wxyz" with size
 *     (xyzwidth,xyzheight).
 *     This displays the (x,y) coordinates of the mouse as well as the
 *     data value at that position and any marker key that has been struck.
 * (6) The zoom window is an independent window with size (zwidth,zheight).
 *     It shows a zoomed version of the location of the mouse.

       +-------------------------+      +-------+
       |                         |      |       |
       |                         |      | Zoom  |
       |                         |      |       |
       |                         |      +-------+
       |        Image            |
       |                         |
       |                         |
       |                         |
       |                         |
       |                         |
       +-------------------------+
       | Palette | L1 | x y z    |
       |         | L2 |          |
       +-------------------------+

 * 
 * All subwindows have the same size XYZHEIGHT with a border size of 
 * XYZBORDER (so their size is actually XYZHEIGHT - 2*XYZBORDER), and are 
 * placed along the bottom of the window according to the following scheme:
 * (1) XYZBORDER exists between all windows.
 * (2) xzywidth gets 20*fontwidth for (%4d %4d %6d %1d) display of x y z k
 * (4) lgtwidth gets (MAXMESS+1)*fontwidth for (%MAXMESSs) display of message
 * (5) palwidth gets the rest.
 *
 * There are a number of coordinate systems used here:
 * (1) USER COORDS: (coordinate system that is gone but not forgotten)
 * (2) DATA COORDS: Data array passed to the image routines by the user
 *     Pixel (0,0) is (offx,offy) in USER COORDS
 *     dimensions of the data array are (datw,dath)
 * (3) IMAGE COORDS: This the portion of the data which is displayed
 *     Pixel (0,0) is (datx,daty) in DATA coords
 *     dimensions of the image array are (imw,imh)
 * (4) WINDOW COORDS: This is the display screen window
 *     dimensions are (width,height)
 *     location of UL of image in window is (winx,winy)
 *     location of UL displayed pixel in IMAGE is (imx,imy)

 *     The IMAGE may be zoomed by a factor of zim.
 *     y may increase downwards (yup==0) or upwards (yup==1)
   The transformations between different coordinate systems are

   Xi = (Xw - winx) / zim + imx         Yi = (Yw - winy) / zim + imy

   Xd = Xi + datx                       Yd = Yi + daty                 (yup==0)
                                           = imh-1 - Yi +daty          (yup==1)

   Xu = Xd + offx                       Yu = Yd + offy
 *
 * The data is passed to the imaging routines as floating point with the 
 * address of the start of the array saved as "data", and is immediately 
 * converted to (byte) color addresses and saved as "image". The size of 
 * this image is (imw,imh), and the size of the (zoomed) image in the
 * window is (winw,winh). Displayed in
 * the window is a copy of this array that has been expanded by a factor of
 * "zim", or squashed by a factor of "-zim". The image may under or overfill
 * the window, and the mapping between image coordinates and window 
 * coordinates is provided by (imx,imy), which are the image coordinates of 
 * the upper left hand displayed pixel, and (winx,winy) which are the window
 * coordinates of the upper left hand displayed pixel. Note that all 
 * coordinates run from left to right and top to bottom, and that an expanded
 * display has the upper left corner of a pixel on an even multiple of 
 * window pixels.
 *
 *  +------------------------------------------------------------------------+
 *  |        |                                                               |
 *  |      offy              Original Data                                   |
 *  |        |                                                               |
 *  |        +-----------------------------------------------------+ .       |
 *  |<-offx->|        |                                            | .       |
 *  |        |      daty           Data Array                      | .       |
 *  |        |        |                                            | .       |
 *  |        |        +---------------------------------------+ .  | .       |
 *  |        |<-datx->|       |                               | .  | .       |
 *  |        |        |      imy        Image (if bigger      | .  | .       |
 *  |        |        |       |                than display)  | .  | .       |
 *  |        |        |       +--------------------------+ .  | .  | .       |
 *  |        |        |<-imx->|//////|///////////////////| .  | .  | .       |
 *  |        |        |       |////(winy)// Display /////| .  | .  | .       |
 *  |        |        |       |//////|///////////////////| .  | .  | .       |
 *  |        |        |       |(winx)+----------+////////| .  | .  | .       |
 *  |        |        |       |//////|Image (if |////////| .  |imh | .       |
 *  |        |        |       |//////| smaller  |////////|winh| .  |dath     |
 *  |        |        |       |//////|  than    |////////| .  | .  | .       |
 *  |        |        |       |//////| display) |////////| .  | .  | .       |
 *  |        |        |       |//////+----------+////////| .  | .  | .       |
 *  |        |        |       |//////////////////////////| .  | .  | .       |
 *  |        |        |       +--------------------------+ .  | .  | .       |
 *  |        |        |       ........... winw ...........    | .  | .       |
 *  |        |        +---------------------------------------+ .  | .       |
 *  |        |        .................. imw ..................    | .       |
 *  |        |                                                     | .       |
 *  |        +-----------------------------------------------------+ .       |
 *  |        ........................ datw .........................         |
 *  +________________________________________________________________________+
*/


#define XYZHEIGHT 45
#define XYZWIDTH 40
#define XYZBORDER 1
#define DEFZOOMSIZE 32
#define DEFZOOMSIZE 32

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define ABS(a) (((a) > 0) ? (a) : -(a))

#define BYTEPAD (BitmapPad(dpy)>>3)
/* #define ROUNDUP(a) ( BYTEPAD * (((a) + BYTEPAD-1) / BYTEPAD) ) */
#define ROUNDUP(a) (a)

#define PICTDEFAULT "+10+10"
#define DEFAULT_FONT1 "6x10" 
#define DEFAULT_FONT2 "fixed" 
#define DEFZOOMFAC 4 
#define MAXCOLORS 121
#define MAXPALWIDTH 1024
#define MAXVEC 90000		/* maximum number of vectors saved */
#define MAXTEXT 100		/* maximum number of text saved */
#define MAXZCURS 64		/* maximum number of zoom cursor vectors */
#define MAXMESS 7		/* maximum number of chars in message */
#define MAXSCREENPIX 1<<21      /* maximum number of chars in message */
#define DEFMAXWIDTH 1024
#define DEFMAXHEIGHT 865

#define USERXCOORD(x) ( (zim > 0) ? \
       (((x)-winx)/zim+imx+datx+offx) : ((-zim)*((x)-winx)+imx+datx+offx) )

#define USERYCOORD(y) ( ( (zim > 0) ? \
  ( (yup==0) ? (((y)-winy)/zim+imy)    : (imh-1-((y)-winy)/zim-imy) ) \
                         : \
  ( (yup==0) ? ((-zim)*((y)-winy)+imy) : (imh-1-(-zim)*((y)-winy)-imy) ) \
                         ) + daty+offy )

#ifndef __alpha
char *malloc();
#endif

/* Define the various windows */
Display *dpy;    		        /* Display pointer */
Window wbase ;           		/* Base window */
Window wimage;				/* Image window */
Window wpal, wxyz;			/* palette, xyz display windows */
Window wzoom ;				/* Zoom window */
Window wlgt1, wlgt2, wlgt3, wlgt4;      /* Status light windows */

int screen;                             /* Screen number of display */
/* Define a Window info structure */
XWindowChanges xwc;
XSetWindowAttributes xswa;
XImage *dataimage;		           /* data image */
XImage *palimage;                       /* palette strip image structure */
XImage *zoomimage;                      /* zoom image */
GC vectorgc, textgc;                 /* Graphics context */
GC imagegc;                  	   /* Image graphics context */
#ifdef __HAIRS
GC xorgc;                  	   /* Image graphics context */
#endif
Window inforoot;
int infox, infoy, infowidth, infoheight, infoborder, infodepth;

/* Define the window geometry arrays */
char *display;
char *geometry;
char filegeometry[30];
char zgeometry[30];

/* Define the image cursor and the palette cursor */
Cursor curs, palcurs;

/* Define the font used for the xyz display */
Font font;
char *fontname1, *fontname2, **fontpath;
int *nfont;
XFontStruct *fontinfo;
int fontheight, fontwidth;

int ncolors;				/* number of image colors used */
int nvcolors;				/* number of vector colors used */
#ifdef __alpha
unsigned long pixels[256];	        /* color cell addresses allocated */
unsigned long planes;			/* plane mask */
#else
int pixels[256];			/* color cell addresses allocated */
int planes;				/* plane mask */
#endif
Colormap defcmap;			/* Display color map */
XColor cmap[256];			/* color cells used */
XColor stcolor[4];			/* color status lights */
XColor palcolors[MAXCOLORS];         /* complete palette */
XColor vcolor[256];			/* vector color palette */
int npalette;				/* size of complete palette */
int pal0, pal1;				/* palette breakpoints */
int branch[3*256];			/* branch table for lookup */
int truecolor;
unsigned long rmask, gmask, bmask;

XErrorEvent _XErrorEvent;

struct imagevector {			/* Define vector structure */
 short xv;				/* x coordinate */
 short yv;				/* y coordinate */
 short color;				/* color address (-1) for relocate */
};

struct imagetext {			/* Define vector structure */
 short xt;				/* x coordinate */
 short yt;				/* y coordinate */
 char text[80];
 short color;				/* color address (-1) for relocate */
};

struct imagevector veclist[MAXVEC];	/* Save array for vectors */
int xvlast, yvlast, vcount;		/* x,y of last vector; number drawn */
int vlastcolor;				/* color of last vector */

struct imagetext textlist[MAXTEXT];	/* Save array for text */
int xtlast, ytlast, tcount;		/* x,y of last text; number drawn */
int tlastcolor;				/* color of last text */

int maxwidth, maxheight;                /* maximum image size */
int width, height;			/* last size of image part of window */
int resize;				/* flag for auto resizing of window */
int autozoomout;                        /* flag for autozoomout of large images */
int zoomsample;                         /* flag for sample/bin of large images */

int to_program, from_display;		/* display comm pipe descriptors */
int waiting_for_key;			/* flag for blocking read */
int xtv_refresh();

#define MAXSTORE 4
int store;

float *data;			/* address of data (image) array */
unsigned char *image[MAXSTORE]; /* data converted to display bytes */
int nimage[MAXSTORE];           /* number of bytes in each image */
unsigned char *imbuf;		/* 128k buffer for display */
int yup;			/* flag = 0/1 for y increasing down/up */
int datw, dath;		       	/* dimensions of data  array */
int imwidth;			/* bytes per row of image array=4*((imw+3)/4)*/
int imw, imh;		       	/* dimensions of image array */
int winw, winh;			/* dimensions of image in window */
int zim;			/* expansion factor of image */
int offx, offy;			/* original coords of UL of data array */
int datx, daty;			/* data coords of UL of image portion */
int imx, imy;			/* image coords of UL of displayed portion */
int winx, winy;			/* window coords of UL of displayed portion */
int imtv;                       /* VISTA buffer number  */

float *sdata[MAXSTORE];			
int sdatw[MAXSTORE], sdath[MAXSTORE];	
int simwidth[MAXSTORE];		
int simw[MAXSTORE], simh[MAXSTORE];
int swinw[MAXSTORE], swinh[MAXSTORE];
int szim[MAXSTORE];		
int soffx[MAXSTORE], soffy[MAXSTORE];
int sdatx[MAXSTORE], sdaty[MAXSTORE];
int simx[MAXSTORE], simy[MAXSTORE];
int swinx[MAXSTORE], swiny[MAXSTORE];
int simtv[MAXSTORE];
char imname[MAXSTORE][20];

int palheight, palwidth, palx, paly;	/* size and loc of palette subwindow */
float *palbrkpt;			/* pointer to breakpoint array */
unsigned char *palette;			/* array with palette values */
short *lookup;				/* lookup table: data -> color adr */

int lgtheight, lgtwidth, lgtx1, lgtx2, lgty;/* size and loc of light subwindows */

char xyzstring[60];
int xyzheight, xyzwidth, xyzx, xyzy;	/* size and loc of xyz subwindow */
int mousex, mousey, mousez;		/* loc and value of mouse */
int textx, texty, texty2;		/* loc of xyz text */

unsigned char *zimage;			/* zoom buffer */
int zoomf;				/* zoom factor */
int zwidth, zheight;			/* zoom window size */
int lastzoomx, lastzoomy;		/* location of last update of zoom */
struct imagevector zcursvec[MAXZCURS];	/* array of zoom cursor vectors */

int whichkey;				/* which button/key was pressed */
int buttondown;				/* true if a button is pressed */
int lastx, lasty;			/* loc where button/key went down */

struct KEYACTION {
  int (*action)();			/* functions to execute when key hit */
  int echo;				/* 0/1 to echo character in display */
};

struct KEYACTION keyaction[128];
int keyzoomin();
int keyzoomout();
int keypan();
int keyrecenter();
int keyzoomprint();


/* Bit map of image cursor */
#define curs_width 16
#define curs_height 16
#define curs_x_hot 7
#define curs_y_hot 9
#ifndef OPENCURS
/* Regular cursor */
static unsigned char curs_bits[] = {
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x40, 0x01, 0x3f, 0x7e, 0x40, 0x01, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x00, 0x00};
static unsigned char curs_mask_bits[] = {
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0x7f, 0x7f, 0x3f, 0x7e, 0x7f, 0x7f, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0x00, 0x00};
#else
/* Cursor with bigger central open hole */
static unsigned char curs_bits[] = {
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x1f, 0x7c, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x00, 0x00};
static unsigned char curs_mask_bits[] = {
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0x40, 0x01,
   0x3f, 0x7e, 0x1f, 0x7c, 0x3f, 0x7e, 0x40, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0x00, 0x00};
#endif

/* Bit map of palette cursor */
#define palcurs_width 16
#define palcurs_height 16
#define palcurs_x_hot 8
#define palcurs_y_hot 7
static unsigned char palcurs_bits[] = {
   0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01,
   0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01,
   0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00};
static unsigned char palcurs_mask_bits[] = {
   0x80, 0x03, 0x80, 0x03, 0x80, 0x03, 0x80, 0x03, 0x80, 0x03, 0x80, 0x03,
   0x80, 0x03, 0x80, 0x03, 0x80, 0x03, 0x80, 0x03, 0x80, 0x03, 0x80, 0x03,
   0x80, 0x03, 0x80, 0x03, 0x80, 0x03, 0x00, 0x00};

#define ERR_CANT_OPEN_DISPLAY			1
#define ERR_CANT_CREATE_IMAGE_WINDOW		2
#define ERR_CANT_CREATE_ZOOM_WINDOW		3
#define ERR_INSUFF_VECTOR_COLORS		4
#define ERR_INSUFF_STATUS_COLORS		5
#define ERR_INSUFF_IMAGE_COLORS			6
#define ERR_BAD_ALLOC_LOOKUP			7
#define ERR_BAD_ALLOC_PALETTE			8
#define ERR_BAD_ALLOC_IMBUF			9
#define ERR_BAD_ALLOC_ZOOM			10
#define ERR_NO_DATA				11
#define ERR_BAD_DISPLAY_PIPE			12
#define ERR_NOT_INITIALIZED			13
#define ERR_CANT_GET_FONT			14

