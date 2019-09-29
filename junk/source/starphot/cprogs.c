#include "Vista.h"
/* FORTRAN callable C input-output routines				*/

/* This file contains all of the C routines needed for the new          */
/*   STANDALONE DAOPHOT                                                 */

char *malloc();
#ifdef VMS
#include <stdio.h>
#include descrip
#endif

#ifdef __CRAY
OPENC(onum, file, mode)
#else
ATUS(openc)(onum, file, mode)
#endif
int	*onum, *mode;
char	*file;
{
	if (*mode) {
		*onum	= creat(file, 0644);
		if (*onum == -1)
			return (-1);
	} else {
		*onum	= open(file, 0);
		if (*onum == -1)
			return (-1);
	}
	return (0);
}

#ifdef __CRAY
CLOSEC(onum)
#else
ATUS(closec)(onum)
#endif
int	*onum;
{
	close (*onum);
	return (0);
}

#ifdef __CRAY
WRITEINT(onum,ibuf,nbyte)
#else
ATUS(writeint)(onum,ibuf,nbyte)
#endif
int	*onum;		/* Input file descriptor */
short	*ibuf;		/* Output integer buffer			*/
int	*nbyte;		/* Length of integer buffer			*/
{
	if (write (*onum, ibuf, *nbyte) != *nbyte)
		return (-1);
	else
		return (0);
}

#ifdef __CRAY
READINT(onum,ibuf,nbyte)
#else
ATUS(readint)(onum,ibuf,nbyte)
#endif
int	*onum;		/* Input file descriptor */
short	*ibuf;		/* Output integer buffer			*/
int	*nbyte;		/* Length of integer buffer			*/
{
#ifdef __CRAY
   int tmpbuf[64];
   int isb = 1;
   int num = 256;
   int len = 2;
   int inc = 1;

   if (read (*onum, tmpbuf, *nbyte) != *nbyte)
      return (-1);
   else {
      VXICTC(tmpbuf,&isb,ibuf,&num,&len,&inc);
      return (0);
   }
#else
	if (read (*onum, ibuf, *nbyte) != *nbyte)
		return (-1);
	else
		return (0);
#endif
}

#ifdef __CRAY
PACKFIT(c,ibuf,nbyte)
#else
ATUS(packfit)(c,ibuf,nbyte)
#endif
short	*c;		/* Input integer buffer				*/
register short	*ibuf;	/* Output integer buffer			*/
int	*nbyte;		/* Length of integer buffer			*/
{
	int i;
	unsigned short high, low;
        short *cold, *iold;

        cold = c;
        iold = ibuf;
	for (i = *nbyte/2; i > 0; i--) {
		high	= ( *c & 0xff00);
		low	= ( *c++ & 0x00ff);
		*ibuf++	= ( ( high >> 8 ) & 0x00ff) | ( ( low << 8 ) & 0xff00 );
	}
        c = cold;
        ibuf = iold;
	return (0);
}

ATUS(packfit4) (c,ibuf,nbyte)
/*      This routine copies 4-byte integers from c to ibuf              */
/*      As it copies, it swaps bytes and shortints                      */
/*      This is the necessary conversion from VAX to Sun and back       */
/*      The copying can be done in place                                */
short   *c;             /* Input integer*4 buffer                               */
short   *ibuf;          /* Output integer*4 buffer                      */
int     *nbyte;         /* Length of integer buffer                     */
{
	int i;
	short temp;
 
	/* move the bytes most of the way to their new home, and swap   */
	vswab((char *)c, (char *)ibuf, *nbyte);
 
	/* now swap the words */
#ifdef  VMS
	/* found a bug on our old VAX/VMS C compiler and had to do this */
	for (i = 0; i < *nbyte/2; i+=2) {
		temp = ibuf[i];
		ibuf[i] = ibuf[i+1];
		ibuf[i+1] = temp;
	}
#else   /* VMS */
	for (i = 0; i < *nbyte/4; i++) {
		temp = *ibuf++;
		*(ibuf-1) = *ibuf;
		*(ibuf++) = temp;
	}
#endif  /* VMS */
	return (0);
}
 
 
vswab(from, to, nbytes)
/* This is an implementation of the Unix swab routine to swap bytes     */
/* It is permissable for the source and destination to overlap          */
/*      The following is from the SunOS man page:                       */
/*   nbytes should be even and positive.  If nbytes  is  odd  and       */
/*   positive,  swab()  uses  nbytes  -  1 instead.  If nbytes is       */
/*   negative, swab() does nothing.                                     */
char    *from, *to;
int     nbytes;
{
	int i;
	char temp;
 
	/* move the bytes most of the way to their new home */
	vbcopy(from, to, nbytes);
 
	/* now swap them */
	nbytes /= 2;
	for (i = 0; i < nbytes; i++) {
		temp = *to++;
		*(to-1) = *to;
		*(to++) = temp;
	}
}

vbcopy(b1, b2, length)
/* This is an implementation of the Unix bcopy routine to copy strings  */
/* of bytes around in memory.  It is permissable for the source and     */
/* destination to overlap.                                              */
char    *b1, *b2;
int     length;
{
	int i;
 
	if (b1 == b2) {
		return;
	} else if (b1 > b2) {
		for (i = 0; i < length; i++) {
			b2[i] = b1[i];
		}
	} else if (b1 < b2) {
		for (i = length - 1; i >= 0; i--) {
			b2[i] = b1[i];
		}
	}
}
#ifdef __CRAY
CCALLOC(nbytes,location)
#else
ATUS(ccalloc)(nbytes,location)
#endif
int *nbytes,*location;
{
  *location= malloc(*nbytes);
}

#ifdef __CRAY
CCFREE(nbytes,location)
#else
ATUS(ccfree)(nbytes,location)
#endif
int *nbytes,*location;
{
  int ii;
  ii=free(*location);
}

#ifdef __CRAY
CCGETCOL(loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one)
#else
ATUS(ccgetcol)(loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one)
#endif
int *loc,*isr,*ier,*isc,*iec,*func,*nx,*lx,*mx,*irow,*one;
{
#ifdef __CRAY
  GETCOL(*loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one);
#else
  ATUS(getcol)(*loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one);
#endif
}

#ifdef __CRAY
CCWFPCIO(loc,nrow,ncol,write)
#else
ATUS(ccwfpcio)(loc,nrow,ncol,write)
#endif
int *loc,*nrow,*ncol,*write;
{
#ifdef __CRAY
  WFPCIO(*loc,nrow,ncol,write);
#else
  ATUS(wfpcio)(*loc,nrow,ncol,write);
#endif
}

#ifdef __CRAY
CCCOPY(loc,nrow,ncol,locb)
#else
ATUS(cccopy)(loc,nrow,ncol,locb)
#endif
int *loc,*nrow,*ncol,*locb;
{
#ifdef __CRAY
  COPY(*loc,nrow,ncol,*locb);
#else
  ATUS(copy)(*loc,nrow,ncol,*locb);
#endif
}

#ifdef __NEWLIB
ATUS(cclibfill) (location,nbox,nlib,gauss,psf,npsf,maxpsf,amin,amax,app,xpsf,ypsf)
int *location,*nbox,*nlib,*gauss,*psf,*npsf,*maxpsf,*amin,*amax,*app,*xpsf,*ypsf;
{
  ATUS(libfill) (*location,nbox,nlib,gauss,psf,npsf,maxpsf,amin,amax,app,xpsf,ypsf);
}
 
ATUS(cclibget/**/_TUS(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy)
int *location,*nbox,*nlib,*ix,*iy,*x,*y,*value,*dvdx,*dvdy;
{
  ATUS(libget) (*location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy);
}
 
ATUS(ccliball)(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte)
int **location,*nbox,*nlib,*ix,*iy,*x,*y,*dvdx,*dvdy,*nall,*nbyte;
float *value;
{
  int i;

  for (i=0;i<*nall;i++) {
   ATUS(libget)(*location+i*(*nbyte/4),nbox,nlib,ix,iy,x,y,value+i,dvdx+i,dvdy+i);
  }
}
 
ATUS(cclibone)(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte,iix,iiy)
int **location,*nbox,*nlib,*ix,*iy,*x,*y,*dvdx,*dvdy,*nall,*nbyte,*iix,*iiy;
float *value;
{
  ATUS(libget)(*location+(*iiy*sqrt(*nall)+*iix)*(*nbyte/4),nbox,nlib,ix,iy,x,y,value,dvdx,dvdy);
}
 
ATUS(ccwrfits)(header,location,nrow,ncol,file,bitpix,bzero,bscale)
int *header,*location,*nrow,*ncol,*file,*bitpix,*bzero,*bscale;
{
  ATUS(wrfits) (header,*location,nrow,ncol,file,bitpix,bzero,bscale);
}
 
ATUS(ccrfits)(location,nrow,ncol,ifile,bitpix,bzero,bscale,ierr)
int *location,*nrow,*ncol,*ifile,*bitpix,*bzero,*bscale,*ierr;
{
  ATUS(rfits)(*location,nrow,ncol,ifile,bitpix,bzero,bscale,ierr);
}
 
ATUS(cclibint)(location,location2,npsflib,nlib,psfrad,amin,amax)
int *location,*location2,*npsflib,*nlib,*psfrad,*amin,*amax;
{
  ATUS(libint)(*location,*location2,npsflib,nlib,psfrad,amin,amax);
}
 
ATUS(cclibput)(location,nbox,nlib,ix,iy,x,y,value,amin,amax)
int *location,*nbox,*nlib,*ix,*iy,*x,*y,*value,*amin,*amax;
{
  ATUS(libput)(*location,nbox,nlib,ix,iy,x,y,value,amin,amax);
}
 
#endif
