#include "Vista.h"
/************************************************************************/
/* CTEXT() -- WRITE LABELS TO AN IMAGE                                 */
/*                                                                      */
/* JAH, 6/90 following JJH, Jan 1986                                    */
/* This just folds a simple character generator through the plot routine*/
/************************************************************************/

#ifdef VMS
ATUS(ctext) (a,ncol,nrow,str,x0,y0,fill,rot,scale)
#else
ATUS(ctext) (a,ncol,nrow,str,x0,y0,fill,rot,scale,nstr)
ADDRESS nstr;
#endif
char *str;
int *x0, *y0, *ncol, *nrow;
int *fill, *rot, *scale;
float *a;
{
int length, x, y, dx, dy;
int i, j, n, nend;
unsigned char **raster;
float *ptr;

*scale = *scale<1 ? 1 : *scale;
*rot = (((*rot+360)%360)/90)*90;
if(*x0<0 || *y0<0 || *x0 > *ncol || *y0 > *nrow) return(-1);
length = strlen(str) * 8* *scale;

switch (*rot) {

   case 0 :     length = (*x0+length)>*ncol ? *ncol-*x0 : length;
		if(*y0>(*nrow-12* *scale)) return(-1);
		break;
   case 90 :    length = (*y0+length)>*nrow ? *nrow-*y0 : length;
		if(*x0<(12* *scale-1)) return(-1);
		break;
   case 180 :   length = (*x0-length)< -1 ? *x0+1 : length;
		if(*y0<(12* *scale-1)) return(-1);
		break;
   case 270 :   length = (*y0-length)< -1 ? *y0+1 : length;
		if(*x0>(*ncol-12* *scale)) return(-1);
		break;
   }


makearray(length,12* *scale,1,&raster);

length = genchar(str,raster,length,*scale);

x= *x0;
y= *y0;
nend = 12 * *scale - 1;
for(j=0; j<12 * *scale; j++) {

   ptr=a+((*y0+nend-j) * *ncol+ *x0);
   switch (*rot) {

      case 0 :  y++;    dy=0;
		x= *x0; dx=1;
		break;
      case 90 : x--;    dx=0;
		y= *y0; dy=1;
		break;
      case 180: y--;    dy=0;
		x= *x0; dx= -1;
		break;
      case 270: x++;    dx=0;
		y= *y0; dy= -1;
		break;
      }

   n=0;

   for(i=0; i<length; i++) {

     if (raster[j][i]) {
	*(ptr++) = 32766;
	}
     else if (*fill) {
	*(ptr++) = 0;
	}
     else
	ptr++;
   }
}
freearray(raster);
return(0);
}
