#include "Vista.h"
 
#ifdef  VMS
#include <stdio.h>
#include descrip
#endif  /* VMS */
 
/************************************************************************/
 
ATUS(byte2float) (c,fbuf,numb)
/*      This routine converts an array of bytes into an array of floats */
/*      The pointers c and ibuf may point to the same place so that     */
/*      the unpacking can be done in place, or the 2 arrays may be      */
/*      totally disjoint, but they may not overlap in any other manner. */
/*      The intended use of this is for reading BITPIX=8 images.        */
char            *c;     /* Input byte buffer                            */
float           *fbuf;  /* Output float buffer                          */
int             *numb;  /* Length of character buffer                   */
{
	int i;
	/* in order to allow copying in place, this is done in reverse  */
	for (i = *numb - 1; i >= 0; i--) {
		fbuf[i] = c[i];
	}
	return (0);
}
 
/************************************************************************/
 
ATUS(short2float) (c,fbuf,numb)
/*      This routine converts an array of shorts into an array of floats*/
/*      The pointers c and ibuf may point to the same place so that     */
/*      the unpacking can be done in place, or the 2 arrays may be      */
/*      totally disjoint, but they may not overlap in any other manner. */
/*      The intended use of this is for reading BITPIX=16 images.       */
short           *c;     /* Input short buffer                           */
float           *fbuf;  /* Output float buffer                          */
int             *numb;  /* Length of character buffer                   */
{
	int i;
	/* in order to allow copying in place, this is done in reverse  */
	for (i = *numb - 1; i >= 0; i--) {
		fbuf[i] = c[i];
	}
	return (0);
}
 
/************************************************************************/
 
ATUS(int2float) (c,fbuf,numb)
/*      This routine converts an array of ints into an array of floats  */
/*      The pointers c and ibuf may point to the same place so that     */
/*      the unpacking can be done in place, or the 2 arrays may be      */
/*      totally disjoint, but they may not overlap in any other manner. */
/*      The intended use of this is for reading BITPIX=32 images.       */
int             *c;     /* Input int buffer                             */
float           *fbuf;  /* Output float buffer                          */
int             *numb;  /* Length of character buffer                   */
{
	int i;
	for (i = 0; i < *numb; i++) {
		fbuf[i] = c[i];
	}
	return (0);
}
 
/************************************************************************/
 
ATUS(unpackbyte) (c,ibuf,nbyte)
/*      This routine converts an array of bytes into an array of shorts */
/*      The pointers c and ibuf may point to the same place so that     */
/*      the unpacking can be done in place, or the 2 arrays may be      */
/*      totally disjoint, but they may not overlap in any other manner. */
unsigned char   *c;     /* Input character buffer                       */
short           *ibuf;  /* Output short integer buffer                  */
int             *nbyte; /* Length of character buffer                   */
{
	int i;
	/* in order to allow copying in place, this is done in reverse  */
	for (i = *nbyte - 1; i >= 0; i--) {
		ibuf[i] = c[i];
	}
	return (0);
}
 
/************************************************************************/
 
ATUS(unpack) (c,ibuf,nbyte)
/*      This routine copies 2-byte integers from c to ibuf              */
/*      with MC68000, MIPS, or SPARC byte ordering:  copy and byte swap */
/*      with VAX or Sun386i          byte ordering:  copy               */
/*      The copying can be done in place                                */
unsigned char   *c;     /* Input character buffer                       */
short   *ibuf;          /* Output integer buffer                        */
int     *nbyte;         /* Length of character buffer                   */
{
#ifdef  MSBFirst
	vswab((char *)c, (char *)ibuf, *nbyte);
#else   /* MSBFirst */
	vbcopy((char *)c, (char *)ibuf, *nbyte);
#endif  /* MSBFirst */
	return (0);
}
 
/************************************************************************/
 
ATUS(unpackfit) (c,ibuf,nbyte)
/*      This routine copies 2-byte integers from c to ibuf              */
/*      with MC68000, MIPS, or SPARC byte ordering:  copy               */
/*      with VAX or Sun386i          byte ordering:  copy and byte swap */
/*      The copying can be done in place                                */
unsigned char   *c;     /* Input character buffer                       */
short   *ibuf;          /* Output integer buffer                        */
int     *nbyte;         /* Length of character buffer                   */
{
#ifdef  MSBFirst
	vbcopy((char *)c, (char *)ibuf, *nbyte);
#else   /* MSBFirst */
	vswab((char *)c, (char *)ibuf, *nbyte);
#endif  /* MSBFirst */
	return (0);
}
 
/************************************************************************/
 
ATUS(packfit) (c,ibuf,nbyte)
/*      This routine copies 2-byte integers from c to ibuf              */
/*      On a machine with any byte ordering:  copy and byte swap        */
/*      The copying can be done in place                                */
short   *c;             /* Input integer buffer                         */
short   *ibuf;          /* Output integer buffer                        */
int     *nbyte;         /* Length of integer buffer                     */
{
	vswab((char *)c, (char *)ibuf, *nbyte);
	return (0);
}
 
/************************************************************************/
 
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

/************************************************************************/
 
ATUS(packfit8) (c,ibuf,nbyte)
/*      This routine copies 8-byte integers from c to ibuf              */
/*      As it copies, it swaps bytes, shortints, and longints           */
/*      This is the necessary conversion from VAX to Sun and back       */
/*      The copying can be done in place                                */
short   *c;             /* Input integer*8 buffer                               */
short   *ibuf;          /* Output integer*8 buffer                      */
int     *nbyte;         /* Length of integer buffer                     */
{
	int i;
	short temp;
        int itemp;
        int *lbuf;
 
	/* move the bytes most of the way to their new home, and swap   */
	vswab((char *)c, (char *)ibuf, *nbyte);
 
	/* now swap the words */
#ifdef  VMS
	/* found a bug on our old VAX/VMS C compiler and had to do this */
        lbuf = ibuf;
	for (i = 0; i < *nbyte/2; i+=2) {
		temp = ibuf[i];
		ibuf[i] = ibuf[i+1];
		ibuf[i+1] = temp;
        }
	for (i = 0; i < *nbyte/8; i+=4) {
		itemp = lbuf[i];
                lbuf[i] = lbuf[i+1]
                lbuf[i+i] = itemp;
	}
#else   /* VMS */
        lbuf = ibuf;
	for (i = 0; i < *nbyte/4; i++) {
		temp = *ibuf++;
		*(ibuf-1) = *ibuf;
		*(ibuf++) = temp;
        }
	for (i = 0; i < *nbyte/8; i++) {
		itemp = *lbuf++;
		*(lbuf-1) = *lbuf;
		*(lbuf++) = itemp;
	}
#endif  /* VMS */
	return (0);
}
 
/************************************************************************/
 
ATUS(unpack8) (c,ibuf,nbyte)
/*      Routine to unpack 12-bit PDP-8 integers from the input character
	buffer into 16-bit integers.  The form of the input data is:
 
		XXXXXXXX        11111111        11112222        22222222
		byte 1          byte 2          byte 3          byte 4
					pixel 1         pixel 2         */
unsigned char   *c;     /* Input character buffer                       */
short   *ibuf;          /* Output integer buffer                        */
int     *nbyte;         /* Length of character buffer                   */
{
	int i;
	unsigned high, mid, low, out;
 
	c       += 8;                   /* Skip over first 8 bytes      */
	for (i = *nbyte/4; i > 0; i--) {
		*c++;                   /* First byte is trash          */
		high    = (unsigned)*c++;
		mid     = (unsigned)*c++;
		low     = (unsigned)*c++;
		*ibuf++ = ( high << 4 ) | ( ( mid & 0x00f0 ) >> 4 );
		*ibuf++ = ( ( mid & 0x000f ) << 8 ) | low;
	}
	return (0);
}
 
/************************************************************************/
 
ATUS(unpack8h) (c,ibuf,nbyte)
/*      Routine to unpack 12-bit PDP-8 integers from the input character
	buffer into 16-bit integers.  The form of the input data is:
 
		XXXXXXXX        11111111        11112222        22222222
		byte 1          byte 2          byte 3          byte 4
					pixel 1         pixel 2         */
 
#ifdef  VMS             /* For VMS, characters are passed as descriptors */
struct dsc$descriptor_s *c;
#else   /* VMS */
char    *c;             /* Output character buffer                      */
#endif  /* VMS */
short   *ibuf;          /* Input integer buffer                 */
int     *nbyte;         /* Length of character buffer                   */
{
	int i;
	short byte, first;
	char *cold;
 
#ifdef  VMS
	cold = c->dsc$a_pointer;
#else   /* VMS */
	cold = c;
#endif  /* VMS */
	first   = 1;
	for (i = *nbyte; i > 0; i--) {
		if (first)
			byte    = ( ( *ibuf & 0x0fc0 ) >> 6 );
		else
			byte    = ( *ibuf++ & 0x003f );
 
		if (byte < 27 && byte > 0)
			byte    += 64;
		else if (byte == 0) {
			*cold   = ' ';
			return (0);
		}
		*cold++ = byte;
		first   = 1 - first;
	}
	return (0);
}
 
/************************************************************************/
 
#ifdef  __SUNOS3
 
/* Before SunOS 4.0, the FORTRAN 77 on the Suns did not have Intrinsic  */
/* Functions for doing bit manipulation.  They are implemented here.    */
 
ibset_(word, bit)
long int *word, *bit;
{
  return(*word | (0x0001 << *bit));
}
 
btest_(word, bit)
long int *word, *bit;
{
  return ( ( *word & (1 << *bit) ) ? -1 : 0 );
}
 
ibclr_(word, bit)
long int *word, *bit;
{
  return(*word & ~(1 << *bit));
}
#endif  /* SUNOS3 */
 
/************************************************************************/
 
vbcopy(b1, b2, length)
/* This is an implementation of the Unix bcopy routine to copy strings  */
/* of bytes around in memory.  It is permissable for the source and     */
/* destination to overlap.                                              */
char    *b1, *b2;
int     length;
{
	int i;
 
	if (b1 == b2) {
		return(0);
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
 
/************************************************************************/
 
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
 
/************************************************************************/
 
ATUS(vax2ieee) (from, to, nfloat)
/* This routine converts 4-byte fp numbers in VAX format to             */
/* the equivalent 4-byte fp numbers in IEEE format.                     */
/* the from array is copied to the to array, the arrays may overlap.    */
/* It is intended to work on either system, the cpp should define       */
/* MSBFirst if the byte ordering of this machine is like a Sun4 or MIPS.*/
/* MSBFirst should not be defined if this is a VAX or Sun386i.          */
/* Assumptions:  a 32-bit machine with 8-bit bytes                      */
 
unsigned long   *from;  /* yes, long, it is much easier this way */
unsigned long   *to;    /* ditto */
int     *nfloat;        /* how many floats to convert */
 
{
	int             i;
	unsigned short  *where;         /* where are we operating? */
	unsigned long   *all4;          /* for bit shifting */
	unsigned short  negative;       /* is this number negative? */
#ifndef MSBFirst
	unsigned short  vaxsux;
 
	/* move VAX bytes before other operations */
	vbcopy((char *)from, (char *)to, sizeof(float) * *nfloat);
#else   /* MSBFirst */
	/* move VAX bytes before other operations, and also swap them  */
	vswab((char *)from, (char *)to, sizeof(float) * *nfloat);
#endif  /* MSBFirst */
	all4 = (unsigned long *)to;
 
	for (i = 0; i < *nfloat; i++, all4++) {
	    where = (unsigned short *)all4;
	    /* save the sign */
	    negative = *where & 0x8000;
	    /* remove the sign */
	    *where &= 0x7fff;
	    /* do the conversion from VAX to IEEE */
	    if (*where >= 384) {
		/* Most numbers fall in this category. */
		*where -= 256;
	    } else if (*where >= 256) {
		/* These numbers are quite small. */
		*where -= 128;
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where+1);
		*++where = vaxsux;
#endif          /* MSBFirst */
		*all4 /= 2;
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where-1);
		*--where = vaxsux;
#endif          /* MSBFirst */
	    } else {
		/* These are the very smallest numbers. */
		/* Not every VAX number in this range has a corresponding */
		/* IEEE number, so we add 1 to reduce truncation error. */
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where+1);
		*++where = vaxsux;
#endif          /* MSBFirst */
		(*all4)++;
		*all4 /= 4;
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where-1);
		*--where = vaxsux;
#endif          /* MSBFirst */
	    }
	    /* restore the sign */
	    *where |= negative;
	}
 
#ifndef MSBFirst
	/* need to swap the VAX bytes after  other operations */
	vswab((char *)to, (char *)to, sizeof(float) * *nfloat);
#endif  /* MSBFirst */
	return 0;
}
 
/************************************************************************/
 
#define VAXMAX  0x7fffffff;
 
int ATUS(ieee2vax) (from, to, nfloat)
/* This routine converts 4-byte fp numbers in IEEE format to            */
/* the equivalent 4-byte fp numbers in VAX format.                      */
/* the from array is copied to the to array, the arrays may overlap.    */
/* It is intended to work on either system, the cpp should define       */
/* MSBFirst if the byte ordering of this machine is like a Sun4 or MIPS.*/
/* MSBFirst should not be defined if this is a VAX or Sun386i.          */
/* Note that IEEE can represent numbers which are too large for a VAX   */
/* to represent.  These are converted to the maximum possible value for */
/* a VAX, and the return value tells how many of them were encountered. */
/* Assumptions:  a 32-bit machine with 8-bit bytes                      */
 
unsigned long   *from;  /* yes, long, it is much easier this way */
unsigned long   *to;    /* ditto */
int     *nfloat;        /* how many floats to convert */
 
{
	int             i;
	unsigned short  *where;         /* where are we operating? */
	unsigned long   *all4;          /* for bit shifting */
	unsigned short  negative;       /* is this number negative? */
	int             numbad = 0;     /* how many infinities? */
#ifndef MSBFirst
	unsigned short  vaxsux;
 
	/* need to swap the VAX bytes before other operations */
	vswab((char *)from, (char *)to, sizeof(float) * *nfloat);
	all4 = (unsigned long *)to;
#else   /* MSBFirst */
	all4 = (unsigned long *)from;
#endif  /* MSBFirst */
 
	for (i = 0; i < *nfloat; i++, all4++) {
	    where = (unsigned short *)all4;
	    /* save the sign */
	    negative = *where & 0x8000;
	    /* remove the sign */
	    *where &= 0x7fff;
	    /* do the conversion from IEEE to VAX */
	    if (*where < 32) {
		/* zero is easy, but ... */
		/* we do not want the sign bit set */
		/* "negative zero" on a VAX is "Reserved Operand" */
		*all4 = 0;
		continue;
	    } else
#ifdef      WHY_BOTHER
	    if (*where >= 32640) {
		/* these numbers are IEEE fp exceptions */
		*all4 |= VAXMAX;
		numbad++;
	    } else
#endif      /* WHY_BOTHER */
		   if (*where >= 32512) {
		/* these numbers are Infinity to a VAX */
		*all4 |= VAXMAX;
		numbad++;
	    } else if (*where >= 128) {
		/* most numbers fall in this category */
		*where += 256;
	    } else if (*where >= 64) {
		/* these numbers are quite small */
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where+1);
		*++where = vaxsux;
#endif          /* MSBFirst */
		*all4 *= 2;
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where-1);
		*--where = vaxsux;
#endif          /* MSBFirst */
		*where += 128;
	    } else {
		/* these are the very smallest numbers */
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where+1);
		*++where = vaxsux;
#endif          /* MSBFirst */
		*all4 *= 4;
#ifndef         MSBFirst
		vaxsux = *where;
		*where = *(where-1);
		*--where = vaxsux;
#endif          /* MSBFirst */
	    }
	    /* restore the sign */
	    *where |= negative;
	}
 
#ifdef  MSBFirst
	/* need to swap the VAX bytes after  other operations */
	vswab((char *)from, (char *)to, sizeof(float) * *nfloat);
#endif  /* MSBFirst */
	return numbad;
}
 
/************************************************************************/

/************************************************************************/
#ifdef __SUN
#include <sys/ieeefp.h>
#include <stdio.h>
/*
*      Determine whether or not IEEE fp exceptions have ocurrend on a Sun.
*      Report which ones have occurred since they were last cleared.
*      Clear them in preparation for the next call of this routine.
*
*      Programmers who are debugging code may desire to call this or
*      something like it to track down errors.
*
*      Undoubtedly you really would like to give the user control over
*      which exceptions are interesting instead of hard-coding as below.
*/

#define INEXACT       (1<<fp_inexact)
#define DIVISION      (1<<fp_division)
#define UNDERFLOW     (1<<fp_underflow)
#define OVERFLOW      (1<<fp_overflow)
#define INVALID       (1<<fp_invalid)
#ifdef  i386
#define DENORMALIZED  (1<<fp_denormalized)
#endif  /* i386 */
/* only the following exceptions are interesting enough to report */
/* inexact happens all the time in routine arithmetic. */
/* underflow happens often and can usually be ignored */
#define INTERESTING (DIVISION | OVERFLOW | INVALID)

ATUS(sunieee)()
{
char out[16];   /* output string from Sun ieee_flags function */
int ieeestat;   /* return value of ieee_flags */

/* find out what exceptions have occurred since last cleared */
ieeestat = ieee_flags( "get", "exception", "all", out);

/* inform the user what exceptions have ocurred */
/* we mask out the uninteresting exceptions here */
if (ieeestat &= INTERESTING) {
  fprintf(stderr,"\n\r");
  fprintf(stderr,"IEEE exceptions have occurred\n\r");
}
if (ieeestat & INEXACT)
  fprintf(stderr,"Inexact; ");
  if (ieeestat & DIVISION)
    fprintf(stderr,"Division by 0; ");
  if (ieeestat & UNDERFLOW)
    fprintf(stderr,"Underflow; ");
  if (ieeestat & OVERFLOW)
    fprintf(stderr,"Overflow; ");
  if (ieeestat & INVALID)
    fprintf(stderr,"Invalid Operand; ");
#ifdef  i386
  if (ieeestat & DENORMALIZED)
    fprintf(stderr,"Denormalized; ");
#endif  /* i386 */
  if (ieeestat) fprintf(stderr,"\n\r");

/* clear all exceptions and return */
ieeestat = ieee_flags( "clearall", " ", " ", out);
}
#endif /* __SUN */


#include "im_compress.h"
extern int exp_hibytehi_in;

ATUS(sethibytehi) (hibytehi)

int *hibytehi;

{
  if (*hibytehi < 0) {
     exp_hibytehi_in = HIBYTEHI;
  } else
     expand_image_in_order(*hibytehi);
}
