/*
ROUTINES TO AID IN COMMUNICATION WITH THE PERITEK

Jeff Hester, CIT 1986

Version 2.1
Add graphics routines.  26 Sept 1986  JJH
Modify clrovr for negative nx,ny  29 Sept 1986
Modify gryvlt to allow for adjustable break in overlay.  30 Sept 86
Add routines for setting overlay to a value, blink lookup table, and 
	color vlt (from J. Holtzman  22 Jan 1986).
*/

#include "fortrancompat"
#include stdio
#include "qiodef"
#include iodef
#include ssdef
#include math

/* #define MICROVAX		 Distinguish between pza0 and pta0 */

struct shortint pza_chan;

int _pzisopen=0;

/************************************************************************/
/* PZOPEN() -- Open a channel to the Peritek				*/
/************************************************************************/

pzopen()
{
char device[10];
static int first=1;

if(_pzisopen) return(0);

/*
#ifdef MICROVAX
sprintf(device,"_PZA0:");
#else
*/

sprintf(device,"DISPLAY:");

/*
#endif
*/

/*
Assign the channel.  Notes on arguments passed:
   strdes() takes a character string pointer and converts to a descriptor
   &(pza_chan.x) passes a pointer to a 16-bit integer
*/

if(sys$assign(strdes(device),&(pza_chan.x),0,0)!=SS$_NORMAL) {
   perror("PZA0:");
   printf("Cannot open device.\n");
   return(-1);
   }

_pzisopen = 1;

if(first) {
   _cur_setup();
   first=0;
   }

return(0);
}

/*
PZCLOS() -- Close the channel to the Peritek
*/

pzclos()
{
if(_pzisopen) sys$dassgn(pza_chan);
_pzisopen = 0;
return(0);
}


/************************************************************************/
/* GRYVLT() -- MAKE A GREY LEVEL VLT WITH THE TOP PLANE RESERVED 	*/
/* FOR OVERLAY								*/
/*									*/
/* Low and high select vlt entries for black and white, respectively.	*/
/* Table selects the table to use (R,G, or B) using PZDRIVER convention.*/
/* Each color has two associated tables.  The first is set up with the	*/
/* top half of the table contrasting with the bottom half (overlay on)	*/
/* and the second table is set up with the top half matching the bottom	*/
/* half (overlay off).  The break between black and white occurs at	*/
/* a lookup table value of blacktowhite.				*/
/************************************************************************/


gryvlt(low, high, blacktowhite, table)
int low, high, blacktowhite, table;
{
static unsigned char vlt[256];
int i, status, ilow, ihigh, flip, table1, table2;
float scale;
struct iostat ret;
struct shortint ufnc;
static int lastbtow=128;

if(blacktowhite<0) blacktowhite=lastbtow;
lastbtow=blacktowhite;

table1 = table & 0xffff;
table2 = (table*8) & 0xffff;

if(high==low) low=high-1;

if(high<low) flip=1;
else flip=0;

scale = ((double)(high - low))/255.;

/* Force actual range to be 1 -> 127 */

ilow  = low<high ? low : high;
ihigh = high>low ? high : low;
ilow = ilow<1 ? 1 : ilow;
ihigh = ihigh>127 ? 127 : ihigh;

vlt[0]=0;		/* 0 and 128 are always set to these values */
vlt[128]=255;

if(!flip) {
   for(i=1; i<ilow; i++) vlt[i]=0;
   for(i=ihigh+1; i<128; i++) vlt[i] = 255;
   }
else {
   for(i=1; i<low; i++) vlt[i]=255;
   for(i=ihigh+1; i<128; i++) vlt[i]=0;
   }

for(i=ilow; i<=ihigh; i++) vlt[i] = (i- low)/scale;

/* Table1 is the table with the overlay turned on */
/* Load the byte array vlt with 8-bit values between 0 and 255 */

for(i=129; i<256; i++) {
   if(vlt[i&0x7f]<blacktowhite) vlt[i]=255;
   else vlt[i]=0;
   }

ufnc.x = IO$_WRITEVBLK;

/*
Send the table to the peritek.
The QIO function code is IO$_WRITEVBLK.
Vlt points the the 256 entry table.
The value of table1 (with a low order 16 bits with a value less than 0)
specifies that a lookup table is being written and which table to write.
*/

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table1,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

/* Table 2 is the table with the overlay turned off */

for(i=129; i<256; i++) vlt[i] = vlt[i&0x7f];

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table2,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

return(0);
}


/************************************************************************/
/* PZDISP() -- DISPLAY A PICTURE ON THE PERITEK				*/
/* See documentation for description of arguments			*/
/************************************************************************/


pzdisp(pic,nc,nr,rastc0,rastr0,dispc0,dispr0,flip)
unsigned char **pic;
int nr, nc, rastc0, rastr0, dispc0, dispr0, flip;
{
int j, n, status, y;
struct iostat ret;
struct shortint ufnc;
int origin;

cursave();

ufnc.x = IO$_WRITEVBLK;

/* 
If nc is specified as -1, dump the entire picture with a single qio.
*/

if(nc== -1) {			/* Special fast mode */
   status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,pic[0],512*512,0,0,0,0);
   if(status != SS$_NORMAL) {
      return(-1);
      }
   return(0);
   }


nc = (dispc0+nc)>512 ? 512-dispc0 : nc;
nr = (dispr0+nr)>512 ? 512-dispr0 : nr;

/*
Starting row and column for a write are packed into a longword.
The starting column is given in the high order 16 bits.  The starting
row is given in the low order 16 bits.
Put the starting column in the high order part of dispc0.
*/

dispc0 = dispc0<<16;

/* Loop over rows to write, either flipping the data in y or not */

if(flip) for(j=nr-1,y=rastr0; j> -1; j--,y++) {

/*
Construct longword origin with starting row in low order 16 bits.
Nc gives the number of pixels to write.
*/

   origin = (dispr0+j) | dispc0 ;
   status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,pic[y],nc,origin,0,0,0);
   if(status != SS$_NORMAL) {
      return(-1);
      }
   }

else for(j=0,y=rastr0; j<nr; j++,y++) {
   origin = (dispr0+j) | dispc0 ;
   status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,pic[y],nc,origin,0,0,0);
   if(status != SS$_NORMAL) {
      return(-1);
      }
   }

curreset();

return(0);
}


/************************************************************************/
/* PZCLEA()  -- Clear a portion of the Peritek				*/
/*									*/
/* Unfortunately, the only way to do this is to write 0's into the	*/
/* display, which is painfully slow					*/
/************************************************************************/


pzclea(x0, y0, nx, ny)
int x0, y0, nx, ny;
{
char buf[512];
int origin, i, j, status;
struct iostat ret;
struct shortint ufnc;

for(i=0;i<512;i++) buf[i]=0;

nx = (x0+nx)>512 ? 512-x0 : nx;
ny = (y0+ny)>512 ? 512-y0 : ny;

x0 = x0<<16;

ufnc.x = IO$_WRITEVBLK;

for(j=0; j<ny; j++) {
   origin = (y0+j) | x0;
   status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,buf,nx,origin,0,0,0);
   if(status != SS$_NORMAL) {
      return(-1);
      }
   }
return(0);
}


/************************************************************************/
/* PZREAD() -- READ BACK THE PERITEK DISPLAY MEMORY			*/
/*									*/
/* Buffer must point to enough space to hold 512*512 bytes.		*/
/************************************************************************/


pzread(buffer)
char *buffer;
{
int status;
struct iostat ret;
struct shortint ufnc;

ufnc.x = IO$_READLBLK;

/* Read the Picture buffer */

/*
Arguments are just as for writing, except the function code is
IO$_READLBLK.  An origin could be specified.  Here the entire display
is read (origin at 0 and 512*512 bytes read.
*/

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,buffer,512*512,0,0,0,0);
if(status!=SS$_NORMAL) {
   return(-1);
   }
return(0);
}


/************************************************************************/
/* PZWRIT() -- QUICK WRITE OF AN IMAGE TO THE DISPLAY MEMORY		*/
/*									*/
/* Buffer must point to a 512*512 byte image.				*/
/* This can also be done by specifying nc = -1 in pzdisp.		*/
/************************************************************************/


pzwrit(buffer)
char *buffer;
{
int status;
struct iostat ret;
struct shortint ufnc;

cursave();

ufnc.x = IO$_WRITEVBLK;

/* Write the Picture buffer */

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,buffer,512*512,0,0,0,0);
if(status!=SS$_NORMAL) {
   return(-1);
   }

curreset();

return(0);
}



/************************************************************************/
/* PZRVLT() -- READ THE VIDEO LOOKUP TABLE				*/
/*									*/
/* Vlt must  point to enough space for 256 bytes.			*/
/************************************************************************/

pzrvlt(vlt, table)
unsigned char *vlt;
int table;
{
int status;
struct iostat ret;
struct shortint ufnc;

ufnc.x = IO$_READLBLK;

table &= 0xffff;

/* 
Reading the lookup table is just like reading the display memory,
with the table specified by a negative number in the low order 16 bits
of table.
*/ 

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table,0,0,0);
if(status!=SS$_NORMAL) {
   return(-1);
   }
return(0);
}


/************************************************************************/
/* PZPOINTS() -- SEND A SET OF POINTS TO THE PERITEK WITH THE IO$_ACCESS*/
/* FEATURE								*/
/*									*/
/* This is how you have to do graphics to the Peritek.  It is painful	*/
/* and slow.  The Peritek doesn't support a point to point vector mode	*/
/* in hardware, but one could be added to the driver that would speed	*/
/* this up considerably.						*/
/************************************************************************/


struct pz_point {
   short int x;
   short int y;
   short int z;
   short int mask;
   };

/* 
Each 64 bits of data passed to qio look like:

|<-x position->|<-y position->|<-data value->|<-bits to affect->|

The data (z) value determines the value to which the selected pixel is
to be set.  The mask determines which bits are to be affected.  If the
pixel contains a value of 'data' before the call, after the call it
contains  ((data AND NOT(mask)) OR z).  [ ((data&(~mask))|z) in C ]

The data are passed to pzpoints by reference.  The structure type 
pz_point is a convenient way to handle the data.
*/

pzpoints(points,n)
struct pz_point *points;
int n;
{
struct shortint ufnc;
struct iostat ret;
int status;

/*
QIO function IO$_ACCESS specifies that plot data are to follow.  N is the
number of points to plot, so n*8 bytes are passed.
*/

ufnc.x = IO$_ACCESS;

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,points,n*8,0,0,0,0);

if(status!=SS$_NORMAL) return(-1);
else return(0);
}


/************************************************************************/
/* CLROVR() -- CLEAR THE OVERLAY PLANE					*/
/*									*/
/* This has to be done by plotting zeros to all points with a mask that	*/
/* specifies that only the eigth bit is to be affected.			*/
/************************************************************************/

clrovr(x0, y0, nx, ny)
int x0, y0, nx, ny;
{
int i, j, x, y;
struct pz_point row[512];
struct shortint ufnc;
struct iostat ret;
int status;

if(nx<0) {
   x0 = x0+nx+1;
   nx = -nx;
   }
if(ny<0) {
   y0 = y0+ny+1;
   ny = -ny;
   }
if(x0<0) {
   nx += x0;
   x0 = 0;
   }
if(y0<0) {
   ny += y0;
   y0 = 0;
   }

nx = (x0+nx)>512 ? 512-x0 : nx;
ny = (y0+ny)>512 ? 512-y0 : ny;

for(i=0; i<512; i++) {
   row[i].z=0;
   row[i].mask=0x80;
   }

ufnc.x = IO$_ACCESS;

/* Do qios a row at the time */

for(j=0, y=y0; j<ny; j++, y++) {
   for(i=0, x=x0; i<nx; i++, x++) {
      row[i].x=x;
      row[i].y=y;
      }

   status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,row,nx*8,0,0,0,0);
   if(status!=SS$_NORMAL) return(-1);
   }

return(0);
}
   

/************************************************************************/
/* VLTSEL() -- SELECT WHICH VLT TO USE					*/
/*									*/
/* The Peritek has two lookup tables associated with each gun.  In the	*/
/* current software, one of these has the overlay turned "on" (table 0)	*/
/* and one of these has the overlay turned off (table 1).  Vltsel	*/
/* selects which of these is to be used to map the display memory to	*/
/* the screen.  All three colors are switched together.			*/
/************************************************************************/


vltsel(mapnum)
int mapnum;	/* If mapnum == 0, use table with overlay */
{
struct shortint ufnc;
struct iostat ret;
unsigned int mode;
int status;

/*
IO$_SETMODE function is used to select the table, with a boolean
argument "mode."
*/

ufnc.x = IO$_SETMODE;

mode = mapnum ? 1 : 0;

status = sys$qiow(0,pza_chan,ufnc,&ret,0,0,mode,0,0,0,0,0);

if(status != SS$_NORMAL) return(-1);

return(0);
}



/************************************************************************/
/* PZTEXT() -- WRITE LABELS TO THE PERITEK				*/
/* 									*/
/* JJH, Jan 1986							*/
/* This just folds a simple character generator through the plot routine*/
/************************************************************************/

pztext(str,x0,y0,mode,fill,rot,scale)
char *str;
int x0, y0;
char *mode;
int fill, rot, scale;
{
int length, x, y, dx, dy;
int i, j, n;
unsigned char **raster, mask, z;
struct pz_point point[512];

scale = scale<1 ? 1 : scale;
rot = (((rot+360)%360)/90)*90;
if(x0<0 || y0<0 || x0>511 || y0>511) return(-1);
length = strlen(str) * 8*scale;

switch (rot) {

   case 0 :	length = (x0+length)>512 ? 512-x0 : length;
   		if(y0>(512-12*scale)) return(-1);
   		break;
   case 90 :	length = (y0+length)>512 ? 512-y0 : length;
   		if(x0<(12*scale-1)) return(-1);
   		break;
   case 180 :	length = (x0-length)< -1 ? x0+1 : length;
   		if(y0<(12*scale-1)) return(-1);
   		break;
   case 270 :	length = (y0-length)< -1 ? y0+1 : length;
   		if(x0>(512-12*scale)) return(-1);
   		break;
   }

makearray(length,12*scale,1,&raster);

length = genchar(str,raster,length,scale);

if(toupper(mode[0])=='P') mask = 0xff;
else mask = 0x80;
for(i=0; i<length; i++) point[i].mask=mask;

x=x0;
y=y0;

for(j=0; j<12*scale; j++) {

   switch (rot) {

      case 0 :	y++;	dy=0;
      		x=x0;	dx=1;
      		break;
      case 90 :	x--;	dx=0;
      		y=y0;	dy=1;
      		break;
      case 180:	y--; 	dy=0;
      		x=x0;	dx= -1;
      		break;
      case 270:	x++;	dx=0;
      		y=y0;	dy= -1;
      		break;
      }

   n=0;

   for(i=0; i<length; i++) {

      if(raster[j][i]) {
	 point[n].x=x;
	 point[n].y=y;
	 point[n].z=128;
	 n++;
	 }
      else if (fill) {
	 point[n].x=x;
	 point[n].y=y;
	 point[n].z=0;
	 n++;
	 }
      x += dx;
      y += dy;
      }
   pzpoints(point,n);
   }
freearray(raster);
return(0);
}




/************************************************************************/
/* PZLINE() -- Draw a line on the Peritek display.			*/
/* (x0,y0) and (x1,y1) are endpoints in screen coordinates		*/
/* Mode is "PICTURE" or "OVERLAY"					*/
/************************************************************************/


pzline(x0, y0, x1, y1, mode)
int x0, y0, x1, y1;
char *mode;
{
int x, y, dx, dy;
int i, step;
unsigned char mask;
struct pz_point point[512];
double m, b;

dx = x1-x0;
dy = y1-y0;

if(x1==x0) m=1.e4;
else m = ((double)(y1-y0))/((double)(x1-x0));
if(m==0.) m=1.e-4;
b = y0 - m*x0;

if(toupper(mode[0])=='P') mask = 0xff;
else mask = 0x80;

if(abs(dx)>abs(dy)) {
   if(x1<x0) {
      x=x1;  x1=x0;  x0=x;
      }
   for(i=0, x=x0; x<=x1; i++, x++)  {
      y = m*x + b + 0.5;
      if(x<0||x>511||y<0||y>511) {
         i--;
         continue;
         }
      point[i].x=x;
      point[i].y=y;
      point[i].z=128;
      point[i].mask=mask;
      }
   }

else {
   if(y1<y0) {
      y=y1;  y1=y0;  y0=y;
      }
   for(i=0, y=y0; y<=y1; i++, y++) {
      x = (y-b)/m + 0.5;
      if(x<0||x>511||y<0||y>511) {
         i--;
         continue;
         }
      point[i].x=x;
      point[i].y=y;
      point[i].z=128;
      point[i].mask=mask;
      }
   }

pzpoints(point,i);

return(0);
}



/************************************************************************/
/* PZARC() -- Draw a semicicle on the display				*/
/* x0,y0	Center of the circle, screen coordinates		*/
/* radius	Radius of the circle.					*/
/* angle1,2	Beginning, ending angle of arc (degrees).		*/
/*		Counterclockwise, X axis = 0				*/
/* mode		Mode is "PICTURE" or "OVERLAY"				*/
/************************************************************************/

#define PI 3.141592654

pzarc(x0, y0, radius, angle1, angle2, mode)
double x0, y0, angle1, angle2, radius;
char *mode;
{
int x, y, i;
double t, dt, fabs();
unsigned char mask;
struct pz_point *point;
char *malloc();

if(toupper(mode[0])=='P') mask = 0xff;
else mask = 0x80;

dt = .9/(2.*PI*radius);

while(angle1<0.) angle1+=360.;
while(angle1>360.) angle1-=360.;
while(angle2<0.) angle2+=360.;
while(angle2>360.) angle2-=360.;
if(angle2<angle1) angle2+=360.;

angle1 *= PI/180;
angle2 *= PI/180;

i= fabs(angle1-angle2)/dt * 1.1;

point = malloc(i*(sizeof(struct pz_point)));

t=angle1;
for(i=0, t=angle1; t<=angle2; t+=dt, i++) {
   x = x0 + radius*cos(t) + 0.5;
   y = y0 + radius*sin(t) + 0.5;
   if(x<0||x>511||y<0||y>511) {
      i--;
      continue;
      }
   point[i].x=x;
   point[i].y=y;
   point[i].z=128;
   point[i].mask=mask;
   }

pzpoints(point,i);

free(point);

return(0);
}



/************************************************************************/
/* PZBOXR() -- Draw a box with random orientation			*/
/* cenx,ceny	Center of the box					*/
/* length,width	Box dimensions						*/
/* angle	Angle of "length" dimension, counterclockwise, degrees	*/
/*		X axis = 0						*/
/* mode		"PICUTRE" or "OVERLAY"					*/
/************************************************************************/

pzboxr(cenx, ceny, length, width, angle, mode)
double cenx, ceny, angle, length, width;
char *mode;
{
int x[4], y[4];
double dx[2], dy[2];

angle *= PI/180;
length /= 2.;
width /= 2.;

dx[0] = length*cos(angle) - width*sin(angle);
dy[0] = length*sin(angle) + width*cos(angle);
dx[1] = length*cos(angle) + width*sin(angle);
dy[1] = length*sin(angle) - width*cos(angle);

x[0] = cenx + dx[0] + 0.5;
y[0] = ceny + dy[0] + 0.5;
x[1] = cenx + dx[1] + 0.5;
y[1] = ceny + dy[1] + 0.5;
x[2] = cenx - dx[0] + 0.5;
y[2] = ceny - dy[0] + 0.5;
x[3] = cenx - dx[1] + 0.5;
y[3] = ceny - dy[1] + 0.5;

pzline(x[0],y[0],x[1],y[1],mode);
pzline(x[1],y[1],x[2],y[2],mode);
pzline(x[2],y[2],x[3],y[3],mode);
pzline(x[3],y[3],x[0],y[0],mode);

return(0);
}


/************************************************************************/
/* PZBOX() -- Draw a box with normal orientation			*/
/* x0, y0, x1, y1	Opposing corners of the box			*/
/* mode		"PICUTRE" or "OVERLAY"					*/
/************************************************************************/

pzbox(x0,y0,x1,y1,mode)
int x0, y0, x1, y1;
char *mode;
{
pzline(x0,y0,x0,y1,mode);
pzline(x0,y1,x1,y1,mode);
pzline(x1,y1,x1,y0,mode);
pzline(x1,y0,x0,y0,mode);
}


/* GRYVLTO() -- MAKE A GREY LEVEL VLT WITH THE TOP PLANE RESERVED 	*/
/* FOR OVERLAY,  OVERLAY ALL SET TO A PASSED VALUE								*/
/*									*/
/* Low and high select vlt entries for black and white, respectively.	*/
/* Table selects the table to use (R,G, or B) using PZDRIVER convention.*/
/* Each color has two associated tables.  The first is set up with the	*/
/* top half of the table contrasting with the bottom half (overlay on)	*/
/* and the second table is set up with the top half matching the bottom	*/
/* half (overlay off).							*/
/************************************************************************/


gryvlto(low, high, over, table)
int low, high, over, table;
{
static unsigned char vlt[256];
int i, status, ilow, ihigh, flip, table1, table2;
float scale;
struct iostat ret;
struct shortint ufnc;

table1 = table & 0xffff;
table2 = (table*8) & 0xffff;

if(high==low) low=high-1;

if(high<low) flip=1;
else flip=0;

scale = ((double)(high - low))/255.;

/* Force actual range to be 1 -> 127 */

ilow  = low<high ? low : high;
ihigh = high>low ? high : low;
ilow = ilow<1 ? 1 : ilow;
ihigh = ihigh>127 ? 127 : ihigh;

vlt[0]=0;		/* 0 and 128 are always set to these values */

if(!flip) {
   for(i=1; i<ilow; i++) vlt[i]=0;
   for(i=ihigh+1; i<128; i++) vlt[i] = 255;
   }
else {
   for(i=1; i<low; i++) vlt[i]=255;
   for(i=ihigh+1; i<128; i++) vlt[i]=0;
   }

for(i=ilow; i<=ihigh; i++) vlt[i] = (i- low)/scale;

/* Table1 is the table with the overlay turned on */
/* Load the byte array vlt with 8-bit values between 0 and 255 */

for(i=128; i<256; i++) {
   vlt[i]=over;
   }

ufnc.x = IO$_WRITEVBLK;

/*
Send the table to the peritek.
The QIO function code is IO$_WRITEVBLK.
Vlt points the the 256 entry table.
The value of table1 (with a low order 16 bits with a value less than 0)
specifies that a lookup table is being written and which table to write.
*/

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table1,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

/* Table 2 is the table with the overlay turned off */

for(i=129; i<256; i++) vlt[i] = vlt[i&0x7f];

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table2,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

return(0);
}

/************************************************************************/
/* FCOLVLT() -- MAKE THREE VLTS USING COLOR ARRAY PASSED		*/
/************************************************************************/

fcolvlt(r,g,b)
int *r,*g,*b;
{

static unsigned char vlt[256];
int table3,table4,table5,table6,index;
int i, over, status, ilow, ihigh, flip, table1, table2;
float scale;
struct iostat ret;
struct shortint ufnc;

table1 = -1 & 0xffff;
table2 = (-1*8) & 0xffff;
table3 = -2 & 0xffff;
table4 = (-2*8) & 0xffff;
table5 = -4 & 0xffff;
table6 = (-4*8) & 0xffff;

/*  Commented the following out, 10/91 Holtz. */
/* vlt[0]=0;   */		/* 0 and 128 are always set to these values */
/* vlt[128]=255;   */

for(i=1; i<128; i++) {
index = i * 2;
vlt[i]=r[index];
  }
for(i=129; i<255; i++) vlt[i] = r[255];

/* Table1 is the table with the overlay turned on */
/* Load the byte array vlt with 8-bit values between 0 and 255 */

ufnc.x = IO$_WRITEVBLK;

/*
Send the table to the peritek.
The QIO function code is IO$_WRITEVBLK.
Vlt points the the 256 entry table.
The value of table1 (with a low order 16 bits with a value less than 0)
specifies that a lookup table is being written and which table to write.
*/

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table1,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

/* Table 2 is the table with the overlay turned off */

for(i=129; i<256; i++) vlt[i] = vlt[i&0x7f];

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table2,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

for(i=1; i<128; i++) {
index = i * 2;
vlt[i]=g[index];
  }
for(i=129; i<255; i++) vlt[i] = g[255];

/* Table1 is the table with the overlay turned on */
/* Load the byte array vlt with 8-bit values between 0 and 255 */

ufnc.x = IO$_WRITEVBLK;

/*
Send the table to the peritek.
The QIO function code is IO$_WRITEVBLK.
Vlt points the the 256 entry table.
The value of table1 (with a low order 16 bits with a value less than 0)
specifies that a lookup table is being written and which table to write.
*/

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table3,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

/* Table 2 is the table with the overlay turned off */

for(i=129; i<256; i++) vlt[i] = vlt[i&0x7f];

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table4,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

for(i=1; i<128; i++) {
index = i * 2;
vlt[i]=b[index];
   }
for(i=129; i<255; i++) vlt[i] = b[255];

/* Table1 is the table with the overlay turned on */
/* Load the byte array vlt with 8-bit values between 0 and 255 */

ufnc.x = IO$_WRITEVBLK;

/*
Send the table to the peritek.
The QIO function code is IO$_WRITEVBLK.
Vlt points the the 256 entry table.
The value of table1 (with a low order 16 bits with a value less than 0)
specifies that a lookup table is being written and which table to write.
*/

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table5,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

/* Table 2 is the table with the overlay turned off */

for(i=129; i<256; i++) vlt[i] = vlt[i&0x7f];

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt,256,table6,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

return(0);
}

/************************************************************************/
/* BLINKVLT() -- MAKE BLINK VLTS 					*/
/************************************************************************/

blinkvlt(ibl)
int *ibl;
{

static unsigned char vlt1[256], vlt2[256];
int table3,table4,table5,table6,index;
int i, mode, status, ilow, ihigh, flip, table1, table2;
int j,ii;
char c;
float scale;
struct iostat ret;
struct shortint chan,ufnc;

table1 = -1 & 0xffff;
table2 = (-1*8) & 0xffff;
table3 = -2 & 0xffff;
table4 = (-2*8) & 0xffff;
table5 = -4 & 0xffff;
table6 = (-4*8) & 0xffff;

ii=0;
for(i=0; i<*ibl; i++) {
 for(j=0; j<*ibl; j++) {
   vlt1[ii] = j * (255 / *ibl);
   vlt2[ii] = i * (255 / *ibl);
   ii++;
   }
  }

ufnc.x = IO$_WRITEVBLK;
if (*ibl * *ibl < 128) {
  for (i=0; i<(*ibl * *ibl); i++) {
   vlt1[i+128] = 255; 
   vlt2[i+128] = 255; 
  }
}

/*
Send the table to the peritek.
The QIO function code is IO$_WRITEVBLK.
Vlt points the the 256 entry table.
The value of table1 (with a low order 16 bits with a value less than 0)
specifies that a lookup table is being written and which table to write.
*/

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt1,256,table1,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

/* Table 2 is the table with the overlay turned off */

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt2,256,table2,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

ufnc.x = IO$_WRITEVBLK;
if (*ibl * *ibl < 128) {
  for (i=0; i<(*ibl * *ibl); i++) {
   vlt1[i+128] = 0; 
   vlt2[i+128] = 0; 
  }
}

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt1,256,table3,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt2,256,table4,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

ufnc.x = IO$_WRITEVBLK;
if (*ibl * *ibl < 128) {
  for (i=0; i<(*ibl * *ibl); i++) {
   vlt1[i+128] = 255; 
   vlt2[i+128] = 255; 
  }
}

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt1,256,table5,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

status=sys$qiow(0,pza_chan,ufnc,&ret,0,0,vlt2,256,table6,0,0,0);

if(status != SS$_NORMAL) {
   perror("ERROR WRITING VLT");
   return(-status);
   }

printf("Type E or Q to exit, any other key to blink images:");

status=sys$assign(strdes("SYS$INPUT:"),&(chan.x),0,0);
if(status!=SS$_NORMAL) return(-1);

ufnc.x=IO$_READVBLK;
ufnc.x |= IO$M_NOECHO;		/* Don't echo the input */
ufnc.x |= IO$M_NOFILTR;		/* Don't filter the input */

mode = 1;

while (1) {
   status=sys$qiow(0,chan,ufnc,&ret,0,0,&c,1,0,0,0,0);
   if(status!=SS$_NORMAL) return(-1);
   
   switch (c) {
      case 'e' :
      case 'E' :
      case 'q' :
      case 'Q' :
      		 sys$dassgn(chan);
      		 return(0);
      		 break;
      default :
		 vltsel(mode);
		 if (mode == 1) 
			mode = 0;
		 else if (mode == 0)
			mode = 1;

      		 break;
      }
   }

return(0);
}
