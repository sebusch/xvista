#include "Vista.h"
/* Utility routines for zimage -- John Tonry 5/12/88 */
/* Use -DVAXFP for VAX convention fp (as opposed to IEEE) */

#include <stdio.h>

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define ABS(a) (((a) > 0) ? (a) : -(a))

/* Get the Most or Least Significant Short (16 bits) from a floating address */
#ifndef VAXFP

#define MSSHORT(p) ((*((unsigned int *)(p))) >> 16)
#define LSSHORT(p) ((*((unsigned int *)(p))) & 0x0000ffff)

#else

#define MSSHORT(p) ((*((unsigned int *)(p))) & 0x0000ffff)
#define LSSHORT(p) ((*((unsigned int *)(p))) >> 16)

#endif

#define MAXBRANCH 256	/* maximum number of results from mapping */

/* Fast (and slightly shady) routines to look up floating point numbers 
 * in a table. There are assumed to be "nbranch" results found
 * in a result array and the routines are provided with (1) a table of 
 * (nbranch-1) break points (2) an array of floating data values and (3)
 * an array of unsigned chars to receive the results corresponding to 
 * the data values. The addresses are assigned so that you get 
 * result 0           when                     data value < breakpoint[0] 
 * result k+1         when    breakpoint[k] <= data value < breakpoint[k+1]
 * result nbranch-1   when    breakpoint[nbranch-2] <= data value
 *
 * The translation from data value to result address works in two parts, both
 * treating the 4 byte floating point number as two consecutive 2 byte integers
 * Floating numbers (regarded as 2 2-byte integers) are maintained as
 *
 * On VAX ... (low address)  (L)mmmmmmme eeeeeees(H) (L)mmmmmmmm mmmmmmmm(H)
 * On IEEE (hi-byte-first)   (H)seeeeeee mmmmmmmm(L) (H)mmmmmmmm mmmmmmmm(L)
 * On IEEE (low-byte-first)  (L)mmmmmmmm mmmmmmmm(H) (L)mmmmmmme eeeeeees(H)
 *
 * so on the first two systems, the first short can be regarded as a "most 
 * significant part" and the second as "least significant part", with a 
 * little allowance for the fact that the sign is signified by a bit, 
 * not 2's complement. (First and second are interchanged for the third case.)
 * The scheme is then
 * (1) Create a lookup table of 65536 values corresponding to all possible
 *     MSB shorts. If a data value has a MSB short not shared by a break
 *     point, we are done, lookup table will hold the correct result address
 * (2) The entries in the lookup table corresponding to MSB shorts of break
 *     points, are (negative) addresses pointing to a branch table.
 *     The branch table consists of 3 integer entries for each break point:
 *     +--------------------------------------------------------------+
 *     |                    |   *** < test ***   |  *** >= test ***   |
 *     |    Test value      |  (+) result adr or |  (+) result adr or |
 *     |                    |  (-) new pointer   |  (-) new pointer   |
 *     +--------------------------------------------------------------+
 *     The LSB short is tested against the value in the branch table (which
 *     is just the LSB short of the break point) and depending on the result
 *     either the first or second following integer is used as a final result
 *     address (positive) or a new pointer into the branch table (negative).
 * The routine "brancher()" creates the lookup table and branch table
 * The routine "mapimage()" uses these tables to map the data
 */

/* brancher fills the lookup table and branch table according to the 
 * breakpoints it is given.
 */

brancher(nresult,results,breakpoints,lookup,branch)
int nresult;		/* Number of results for data to be mapped to */

//#if defined(__64BITADDRESS)
//unsigned long *results;		/* Domain of possible results */
//#else
//int *results;		/* Domain of possible results */
//#endif
ADDRESS *results;
float *breakpoints;	/* Floating dividing points between each result */
short *lookup;		/* lookup table calculated for first shorts */
int *branch;		/* branch table for ambiguous first shorts */
{
  register short *l;
  register int n, i, k, j, result;
  int i0, k1, k2;
  int si[MAXBRANCH], sj[MAXBRANCH], anc[MAXBRANCH];
  int firstshort[MAXBRANCH+1];

  if(nresult > MAXBRANCH) {
    fprintf(stderr,"brancher: insufficient scratch space\n");
    exit(1);
  }

  l = lookup;

/*
 * Set firstshort[] to the first short values of the floating breakpoints.
 * Use [1] - [nresult-1] for breakpoints, [0] and [nresult] for -/+infinity, 
 */
  firstshort[0] = 65535;
  firstshort[nresult] = 32767;

/*
 * Fill the firstshort[] table with result addresses, not worrying about cases
 * where there is ambiguity because of a result transition.
 */
  j = firstshort[0];
  for(k=0; k<nresult; k++) {
    i = j;
    if(k < nresult-1) firstshort[k+1] = MSSHORT(breakpoints);
    j = firstshort[k+1];
    result = results[k];
/* 
 * If floating number is less than zero, (firstshort > 32767)
 * increasing float => decreasing first short.
 * float:       -infinity  ->    -0    ->  +0  ->  +infinity
 * first short:   65535    ->   32768  ->   0  ->     32767
 */
    if(j > 32767) {
      for(n=i; n>j; n--) *(l+n) = result;
    } else {
/* 
 * Floating number is greater than zero, (firstshort < 32768)
 */
      if(i > 32767) {
/* 
 * Fill in the remaining entries in the lookup decreasing to 32768 
 * and increasing from 0.
 */
	for(n=i; n>=32768; n--) *(l+n) = result;
	for(n=0; n<=j; n++) *(l+n) = result;
      } else {
	for(n=i; n<=j; n++) *(l+n) = result;
      }
    }
/* 
 * Set s to point the the second short of the floating breakpoints
 * and fill the branch table with the second short of floating breakpoints.
 */
    if(k < nresult-1) branch[3*(k+1)] = LSSHORT(breakpoints++);
  }

/*
 * Now replace the ambiguous entries in firstshort[] with (negative) addresses
 * in branch[], and fill the remaining entries of branch[] with result
 * addresses or further (negative) addresses into branch[].
 */

/* Compute the range k1-k2 of breakpoints that share an entry in firstshort */
  for(k1=1; k1<nresult; k1++) {
    for(i=k1; firstshort[i]==firstshort[k1]; i++) k2 = i;

/* For starters, compute just a linear search through the possibilities */
/*
    lookup[firstshort[k1]] = -3*k1;
    for(i=k1;i<=k2;i++) {
      branch[3*i+1] = results[i-1];
      branch[3*i+2] = -3*(i+1);
    }
    branch[3*k2+2] = results[k2];
*/

/* Binary search tree through the possibilities... */
/* i - j are the result indices, divided by k1 - k2 breakpoint indices */
/* Push the initial interval to be subdivided on the stack. Set anc(estor) to 0
  in the branch table, where we will eventually find the initial entry point */
    n = 0;
    si[n] = k1-1;   sj[n] = k2;   anc[n++] = 0;

/* Now loop until the stack is empty */
    while( n > 0 ) {

      i = si[--n];   j = sj[n];   k = anc[n];

/* Is the interval only one result index wide? */
/* Yes, "result" is used for the resulting "branch" 
 * (the final result address) */
      if( i == j)
	result = results[i];

/* No, we need to subdivide again and push the new intervals on the stack */
      else {
	i0 = (i + j + 1) / 2;
/* Push GE branch on the stack */
	si[n] = i0;   sj[n] = j;   anc[n++] = i0;
/* Push LT branch on the stack */
	si[n] = i;   sj[n] = i0-1;   anc[n++] = i0;
/* "result" is used as the branch point for this subdivision */
	result = -3*i0;
      }

/* Now inform the ancestor of this interval where it was subdivided */
      if( i < k ) branch[3*k+1] = result;	/* interval LT ancestor */
      else	  branch[3*k+2] = result;	/* interval GE ancestor */
    }

/* Retrieve the initial entry into the branch table */
    lookup[firstshort[k1]] = branch[2];

/* If the breakpoint is negative swap entries in branch table...
 * Remember that if firstshort[] > 32768, we must reverse the order of the
 * result addresses, because larger second short => smaller number
 * second short: ** f < 0 **  => (-) 65535 -> 0 (+)
 *               ** f > 0 **  => (-) 0 -> 65535 (+)
 */
    if(firstshort[k1] > 32767) {
      for(i=k1;i<=k2;i++) {
	j = branch[3*i+1];
	branch[3*i+1] = branch[3*i+2];
	branch[3*i+2] = j;
      }
    }
    k1 = k2;
  }
}

/* mapimage uses the lookup table from brancher to convert floating point
 * data to displayable image data
 */
mapimage(x0,y0,nx,ny,datw,data,imx0,imy0,imw,yinv,image,lookup,branch)
int x0, y0, nx, ny;	/* origin and dimensions of area to be mapped */
int datw;		/* number of data pixels per row */
float *data;		/* array of data to be mapped */
int imx0, imy0;		/* origin of area in image array */
int imw;		/* number of image pixels per row */
int yinv;		/* flag to invert y order of data and image */
unsigned char *image;	/* array to receive mapped data */
short *lookup;		/* lookup table from brancher() of first shorts */
int *branch;		/* fall back branch table for ambiguous cases */
{
  register short *l;
  register int n, i, *b;
  register unsigned char *im;
  int k, iminc;
/*
  float tarray[2], time, dtime_();
  time = dtime_(tarray);
*/

  data += datw*y0 + x0;
  l = lookup;
  im = image + imw*imy0 + imx0;
  iminc = imw - nx;
  if (yinv == 1) iminc -= 2*imw;

  for(k=0;k<ny;k++) {
    n = nx;
    while (n--) {
/* 
 * look up the first short of the floating data in the lookup table
 * If non-negative it is a color address, increment s by 2 to point at
 * the first short of the next float, and set image = color address
 */
      i = *(l+MSSHORT(data));
      if (i >= 0) {
	data++;
	*im++ = i;
/* 
 * Negative result means a (negative) address into the branch table
 * Increment s to point at the second short of the float, and then loop
 * through the branch table, using negative results as further branching
 * addresses, and quitting with a positive color address.
 */
      } else {
	do {
	  b = branch - i;
	  if ( LSSHORT(data) >= *b++ ) b++;
	  i = *b;
	} while (i < 0);
	data++;
	*im++ = i;
      }
    }
    im += iminc;
    data += datw - nx;
  }

/*
  time = dtime_(tarray) * (1000000./(nx*ny));
  fprintf(stderr,"Mapping time = %10.2f usec/pixel\n",time);
*/
}

#define LONG int

mapimage2(x0,y0,nx,ny,datw,data,imx0,imy0,imw,yinv,image,breakpts,nbreak,pixels,bits_per_pixel,cmask)
int x0, y0, nx, ny;	/* origin and dimensions of area to be mapped */
int datw;		/* number of data pixels per row */
float *data;		/* array of data to be mapped */
int imx0, imy0;		/* origin of area in image array */
int imw;		/* number of image pixels per row */
int yinv;		/* flag to invert y order of data and image */
unsigned LONG *image;	/* array to receive mapped data */
float *breakpts;
//unsigned LONG nbreak;		
int nbreak;
unsigned long *pixels;
int bits_per_pixel;
unsigned long cmask;
{
  register short *l;
  register int n, *b;
  unsigned LONG i;
  register unsigned char *cim;
  register unsigned short *sim;
  register unsigned LONG *lim;
  int k, iminc;
  void locate(),hunt();
/*
  float tarray[2], time, dtime_();
  time = dtime_(tarray);
  fprintf(stderr,"Mapping time = %10.2f %10.2f %10.2f\n",time,tarray[0],tarray[1]);
*/
  data += datw*y0 + x0;
  iminc = imw - nx;
  if (yinv == 1) iminc -= 2*imw;

  i = nbreak/2;
  if (bits_per_pixel == 8) {
   cim = (unsigned char *)image + imw*imy0 + imx0;
   for (k=0;k<ny;k++) {
    n = nx;
    while (n--) {
      hunt(breakpts-1,nbreak-1,*data,&i);
      if (i<1) i=1;
      *cim++ = pixels[i-1] & 0xff;
      data++;
    }
    cim += iminc;
    data += datw - nx;
   }
  } else if (bits_per_pixel == 16) {
   sim = (unsigned short *)image + imw*imy0 + imx0;
   for (k=0;k<ny;k++) {
    n = nx;
    while (n--) {
      hunt(breakpts-1,nbreak-1,*data,&i);
      if (i<1) i=1;
      *sim++ = pixels[i-1] & 0xffff;
      data++;
    }
    sim += iminc;
    data += datw - nx;
   }
  } else if (bits_per_pixel == 32) {
   lim = (unsigned LONG *)image + imw*imy0 + imx0;
   if (cmask==0) {
     for (k=0;k<ny;k++) {
      n = nx;
      while (n--) {
        hunt(breakpts-1,nbreak-1,*data,&i);
        if (i<1) i=1;
        *lim++ = pixels[i-1];
        data++;
      }
      lim += iminc;
      data += datw - nx;
     }
   } else {
     for (k=0;k<ny;k++) {
      n = nx;
      while (n--) {
        hunt(breakpts-1,nbreak-1,*data,&i);
        if (i<1) i=1;
        *lim = ((*lim&~cmask) + (pixels[i-1]&cmask)) ;
        lim++;
        data++;
      }
      lim += iminc;
      data += datw - nx;
     }
     
   }
  }
/*
  time = dtime_(tarray);
  fprintf(stderr,"Mapping time = %10.2f %10.2f  %10.2f \n",time,tarray[0],tarray[1]);
*/
} 

/* replicate, samplicate, and duplicate are fast (I hope) routines to
 * filling one array from another.
 */
/* replicate fills the destination array with an arbitrary (square) repetition
 * of each source pixel into NxN pixels in the destination array
 */

replicate(x0,y0,w0,h0,from,f,x,y,w,h,wto,to,fill,bits_per_pixel)
/* Fill array "to" with a piece of "from", each pixel mapping to fxf pixels */
/* If fill != 0, zero any uncovered piece of destination array */
unsigned char *from, *to;	/* Source array, destination array */
int x0,y0,w0,h0;		/* Source origin, dimensions */
int x,y,w,h;			/* Destination origin, dimensions */
int wto;			/* Destination dimension */
int f;				/* Replication factor */
int fill;			/* flag to zero rest of "to" array */
int bits_per_pixel;
{
  register unsigned char *s, *d;
  register int k, n, i;
  int i0,i1,j0,j1,j;
  int nbytes;

  nbytes = bits_per_pixel/8;
  j0 = MAX(0,y0);
  j1 = MIN(h0,y0+h/f);
  i0 = MAX(0,x0);
  i1 = MIN(w0,x0+w/f);
  for (j=j0;j<j1;j++) {
    s = from + nbytes*(w0*j + i0);
    d = to + nbytes*(wto*(f*(j-y0)+y) + x + f*(i0-x0));
    i = i1 - i0;
    n = f;
    while (i--) {
      k = n;
      /*while (k--) *d++ = *s;*/
      while (k--) {
        bcopy(s,d,nbytes);
        d+=nbytes;
      }
      s+=nbytes;
    }
    i = f-1;
    n = nbytes*f*(i1-i0);
    s = d - nbytes*(f*(i1-i0));
    d = s + nbytes*wto;
    while (i--) {
      bcopy(s,d,n);
      d += wto*nbytes;
    }
  }

  if (fill != 0) zeroborder(f*x0, f*y0, f*w0, f*h0, w, h, wto, to, nbytes);
}

/* samplicate fills the destination array with an arbitrary (square) sampling
 * sampling of each NxN source pixels into the destination array
 */

samplicate(x0,y0,w0,h0,from,f,x,y,w,h,wto,to,fill,bits_per_pixel)
/* Fill array "to" with a piece of "from", fxf pixels mapping to a pixel */
/* If fill != 0, zero any uncovered piece of destination array */
unsigned char *from, *to;	/* Source array, destination array */
int x0,y0,w0,h0;		/* Source origin, dimensions */
int x,y,w,h;			/* Destination origin, dimensions */
int wto;			/* Destination dimension */
int f;				/* Replication factor */
int fill;			/* flag to zero rest of "to" array */
int bits_per_pixel;
{
  register unsigned char *s, *d;
  register int k, n, i;
  int i0,i1,j0,j1,j;
  int nbytes;
  nbytes = bits_per_pixel/8;
  j0 = MAX(0,y0);
  j1 = MIN(h0,y0+f*h);
  i0 = MAX(0,x0);
  i1 = MIN(w0,x0+f*w);
  for (j=j0; j<j1; j+=f) {
    s = from + nbytes*(w0*j + i0);
    d = to + nbytes*(wto*((j-y0)/f+y) + x + (i0-x0)/f);
    for (i=i0; i<i1; i+=f) {
      bcopy(s,d,nbytes);
      d+=nbytes;
      s+=f*nbytes;
    }
  }

  if (fill != 0) zeroborder(x0/f, y0/f, w0/f, h0/f, w, h, wto, to, nbytes);
}

/* duplicate is the same as replicate with no expansion or compression */

duplicate(x0,y0,w0,h0,from,x,y,w,h,wto,to,fill,bits_per_pixel)
/* Fill array "to" with a piece of "from"
 * If fill != 0, zero any uncovered piece of destination array
 */
unsigned char *from, *to;	/* Source array, destination array */
int x0,y0,w0,h0;		/* Source origin, dimensions */
int x,y,w,h;			/* Destination origin, dimensions */
int wto;			/* Destination array width */
int fill;			/* flag to zero rest of "to" array */
int bits_per_pixel;
{
  int i0,i1,j0,j1,j,k,l,nbytes;
//  unsigned LONG *val;
  j0 = MAX(0,y0);
  j1 = MIN(h0,y0+h);
  i0 = MAX(0,x0);
  i1 = MIN(w0,x0+w);
  nbytes=bits_per_pixel/8;
  for (j=j0; j<j1; j++) {
     bcopy(from + nbytes*(w0*j + i0),   
           to + nbytes*(wto*(j-y0+y) + x + i0-x0), 
           nbytes*(i1 - i0));
  }
  if (fill != 0) zeroborder(x0, y0, w0, h0, w, h, wto, to, nbytes);
}

zeroborder(x0, y0, w0, h0, w, h, wto, to, nbytes)
unsigned char *to;
int x0, y0, w0, h0, w, h, wto, nbytes;
{
  register int i;
  if (y0 < 0) {
    for(i=0; i<(-y0); i++) bzero(to+nbytes*(i*wto), w*nbytes);
  }
  if (h > h0-y0) {
    for(i=h0-y0; i<h; i++) bzero(to+nbytes*(i*wto), w*nbytes);
  }
  if (x0 < 0) {
    for(i=0; i<h; i++) bzero(to+nbytes*(i*wto), -x0*nbytes);
  }
  if (x0+w > w0) {
    for(i=0; i<h; i++) bzero(to+nbytes*(i*wto+w0-x0), (x0+w-w0)*nbytes);
  }
}

void locate(xx, n, x, j)
float xx[];
unsigned LONG n;
float x;
unsigned LONG *j;
{
        unsigned LONG ju,jm,jl;
        int ascnd;
        jl=0;
        ju=n+1;
        ascnd=(xx[n] >= xx[1]);
        while (ju-jl > 1) {
                jm=(ju+jl) >> 1;
                if (x >= xx[jm] == ascnd)
                        jl=jm;
                else
                        ju=jm;
        }
        if (x == xx[1]) *j=1;
        else if(x == xx[n]) *j=n-1;
        else *j=jl;
}

void hunt(xx, n, x, jlo)
float xx[];
unsigned LONG n;
float x;
unsigned LONG *jlo;
{
	unsigned LONG jm,jhi,inc;
	int ascnd;

	ascnd=(xx[n] >= xx[1]);
	if (*jlo <= 0 || *jlo > n) {
		*jlo=0;
		jhi=n+1;
	} else {
		inc=1;
		if (x >= xx[*jlo] == ascnd) {
			if (*jlo == n) return;
			jhi=(*jlo)+1;
			while (x >= xx[jhi] == ascnd) {
				*jlo=jhi;
				inc += inc;
				jhi=(*jlo)+inc;
				if (jhi > n) {
					jhi=n+1;
					break;
				}
			}
		} else {
			if (*jlo == 1) {
				*jlo=0;
				return;
			}
			jhi=(*jlo)--;
			while (x < xx[*jlo] == ascnd) {
				jhi=(*jlo);
				inc <<= 1;
				if (inc >= jhi) {
					*jlo=0;
					break;
				}
				else *jlo=jhi-inc;
			}
		}
	}
	while (jhi-(*jlo) != 1) {
		jm=(jhi+(*jlo)) >> 1;
		if (x >= xx[jm] == ascnd)
			*jlo=jm;
		else
			jhi=jm;
	}
	if (x == xx[n]) *jlo=n-1;
	if (x == xx[1]) *jlo=1;
}
