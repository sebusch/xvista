#include "Vista.h"
/*  This file contains C routine calls that handle passing                 */
/*   memory addresses and string pointers and descriptors (for VMS)        */
/*   back and forth between FORTRAN routines. The conventions is that      */
/*   each of these routines begins with cc, then the name of the FORTRAN   */
/*   subroutine for which it is doing the pointer-address-descriptor-etc   */
/*   translation.                                                          */

/*  NOTE:  The routines here can either have or not have a trailing         */
/*         underscore as needed by some compilers for the Fortran / C       */
/*         interface.  The existence of a trailing character for the        */
/*         functions is set by the C-preprocessor variable TUS,  which     */
/*         should be defined for an underscore or undefine to have no       */
/*         underscore. This variable is set in the Vista.(machine) file     */
/*         in the lickvista/source subdirectory                             */

/****************************************************************************/
 
#include <stdio.h>
//#include <stdlib.h>
#define MAXFITSCARD 10
#define NFITSBYTES   2880
 
#ifdef VMS
#include descrip
#endif  /* VMS */

#ifdef __alpha
void *malloc();
#else 
char *malloc();
#endif
 
/*                 ***  SECTION 1  ***                                */
 
/* The top two routines ( ccalloc and ccfree ) do the virtual memory  */
/*         allocation and deallocation.                               */
 
#ifdef __UNIX
 
ATUS(ccalloc) (nbytes,location)
ADDRESS *location;
int *nbytes;
{
  *location= (ADDRESS)malloc(*nbytes);
  if (*location == NULL) fprintf(stderr,"ERROR allocating memory\n");
}
 
ATUS(ccfree) (nbytes,location)
int *nbytes;
ADDRESS *location;
{
  int ii;
  free(*location);
  *location = NULL;
}
 
/*  These are the routines which need to extend the virtual memory   */
/*  of FITS header cards if possible.                                */
 
ATUS(ccvstradd) (new,old,nold)
ADDRESS *new, *old;
//int *new, *old;
ADDRESS nold;
{
  int ii, len, nbytes;
  len = ATUS(numchar) (old,nold);
/* Determine number of bytes needed                                 */
  nbytes = ((len/2880)+1) * 2880;
  if ( *(new+1) != nbytes) {
    free(*new);
    *new = (ADDRESS)malloc(nbytes);
    *(new+1) = nbytes;
  }
  ATUS(vstrcpy) (*new,old,*(new+1),nold);
}
 
ATUS(cccheadset) (card,val,loc,ncard,nval)
ADDRESS *loc,*card,*val;
ADDRESS ncard, nval;
{
  hextend(loc);
  ATUS(cheadset) (card,val,*loc,ncard,nval,*(loc+1));
}
 
ATUS(ccfheadset) (card,val,loc,ncard)
ADDRESS *card,*val,*loc;
ADDRESS ncard;
{
  hextend(loc);
  ATUS(fheadset) (card,val,*loc,ncard,*(loc+1));
}
 
ATUS(ccinheadset) (card,val,loc,ncard)
ADDRESS *card,*val,*loc;
ADDRESS ncard;
{
  hextend(loc);
  ATUS(inheadset) (card,val,*loc,ncard,*(loc+1));
}
 
ATUS(ccvstrcpy) (old,loc,nchar)
ADDRESS *loc,*old;
ADDRESS nchar;
{
  ATUS(vstrcpy) (old,*loc,nchar,*(loc+1));
}
#endif  /* UNIX */
 
hextend(loc)
ADDRESS *loc;
{
  char *temp;
  int len;
 
#ifdef VMS
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  len = numchar(&s);
#else   /* VMS */
  len = ATUS(numchar) (*loc,*(loc+1));
#endif  /* VMS */
  if (len+80 > *(loc+1) && *(loc+1) < MAXFITSCARD*NFITSBYTES ) {
    temp = malloc(*(loc+1));
    strncpy(temp,*loc,*(loc+1));
    free(*loc);
    *loc = (ADDRESS)malloc(*(loc+1)+2880);
    *(loc+1) = *(loc+1)+2880;
    strinit(*loc,*(loc+1));
    strncpy(*loc,temp,*(loc+1)-2880);
    free(temp);
  }
}
 
strinit(string,n)
char *string;
ADDRESS n;
{
  int i;
  for (i=0; i<n; i++)
    *(string+i) = '\0';
}
 
/*  Here are the VMS versions of the routines above                  */
#ifdef VMS
 
ccalloc(nbytes,location)
int *nbytes,*location;
{
  vmsalloc(nbytes,location);
/*  *location= malloc(*nbytes); */
}
 
ccfree(nbytes,location)
int *nbytes,*location;
{
/*  free(*location); */
    vmsfree(nbytes,location);
}
 
/*  These are the routines which need to extend the virtual memory   */
/*  of FITS header cards if possible.                                */
 
ccvstradd(new,old)
int *new;
struct dsc$descriptor_s *old;
{
  int len, nbytes;
  struct dsc$descriptor_s s;
 
  len = numchar(old);
/* Determine number of bytes needed                                 */
  nbytes = ((len/2880)+1) * 2880;
  if ( *(new+1) != nbytes) {
/*    free(*new);
    *new = malloc(nbytes);  */
    vmsfree((new+1),new);
    vmsalloc(&nbytes,new);
    *(new+1) = nbytes;
  }
  s.dsc$w_length = *(new+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *new;
  vstrcpy(&s,old);
}
 
cccheadset(card,val,loc)
int *card,*val,*loc;
{
  struct dsc$descriptor_s s;
 
  hextend(loc);
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  cheadset(card,val,&s);
}
 
ccfheadset(card,val,loc)
int *card,*val,*loc;
{
  struct dsc$descriptor_s s;
 
  hextend(loc);
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  fheadset(card,val,&s);
}
 
ccinheadset(card,val,loc)
int *card,*val,*loc;
{
  struct dsc$descriptor_s s;
 
  hextend(loc);
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  inheadset(card,val,&s);
}
 
ccvstrcpy(old,loc)
int *loc,*old;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  vstrcpy(old,&s);
}
#endif  /* VMS */
 
ATUS(ccwrfits)(header,location,nrow,ncol,file,bitpix,bzero,bscale,haveblank,blank,fblank,ierr,nc,nc2)
ADDRESS *header,*location,*nrow,*ncol,*file,*bitpix,*bzero,*bscale,*ierr;
ADDRESS *haveblank,*blank,*fblank;
ADDRESS nc,nc2;
{
  ATUS(wrfits) (header,*location,nrow,ncol,file,bitpix,bzero,bscale,haveblank,blank,fblank,ierr,nc,nc2);
}

ATUS(ccwrcwfpc4)(pix4,a,nrow,ncol,ifile,bitpix,bzero,bscale,haveblank,blank,fblank,ierr)
ADDRESS *pix4,*a,*nrow,*ncol,*ifile,*bitpix,*bzero,*bscale,*haveblank;
ADDRESS *blank,*fblank,*ierr;
{
  ATUS(wrcwfpc4)(*pix4,a,nrow,ncol,ifile,bitpix,bzero,bscale,haveblank,blank,fblank,ierr);
}

ATUS(ccwrcwfpc2)(pix2,a,nrow,ncol,ifile,bitpix,bzero,bscale,haveblank,blank,fblank,ierr)
ADDRESS *pix2,*a,*nrow,*ncol,*ifile,*bitpix,*bzero,*bscale,*haveblank;
ADDRESS *blank,*fblank,*ierr;
{
  ATUS(wrcwfpc2)(*pix2,a,nrow,ncol,ifile,bitpix,bzero,bscale,haveblank,blank,fblank,ierr);
}

ATUS(ccwrwfpc)(header,location,nrow,ncol,file,bitpix,bzero,bscale,haveblank,blank,fblank,ierr,comprsn,nc,nc2)
ADDRESS *header,*location,*nrow,*ncol,*file,*bitpix,*bzero,*bscale,*ierr;
ADDRESS *haveblank,*blank,*fblank,*comprsn;
ADDRESS nc,nc2;
{
  ATUS(wrwfpc) (header,*location,nrow,ncol,file,bitpix,bzero,bscale,haveblank,blank,fblank,ierr,comprsn,nc,nc2);
}

ATUS(ccwrvist)(header,location,nrow,ncol,file,bitpix,bzero,bscale,ierr,nc,nc2)
ADDRESS *header,*location,*nrow,*ncol,*file,*bitpix,*bzero,*bscale,*ierr;
ADDRESS nc,nc2;
{
  ATUS(wrvist) (header,*location,nrow,ncol,file,bitpix,bzero,bscale,ierr,nc,nc2);
}

#ifdef __USEIRAF 
ATUS(ccwriraf)(header,location,nrow,ncol,file,bitpix,bzero,bscale,ierr,nc,nc2)
ADDRESS *header,*location,*nrow,*ncol,*file,*bitpix,*bzero,*bscale,*ierr;
ADDRESS nc,nc2;
{
  ATUS(wriraf) (header,*location,nrow,ncol,file,bitpix,bzero,bscale,ierr,nc,nc2);
}
#endif

#ifdef __HAVEDST 
ATUS(ccwrdst)(header,location,nrow,ncol,file,bitpix,bzero,bscale,ierr,nc,nc2)
ADDRESS *header,*location,*nrow,*ncol,*file,*bitpix,*bzero,*bscale,*ierr;
ADDRESS nc,nc2;
{
  ATUS(wrdst) (header,*location,nrow,ncol,file,bitpix,bzero,bscale,ierr,nc,nc2);
}
#endif
 
ATUS(ccrdfits)(location,nrow,ncol,ifile,header,ierr,nc)
ADDRESS *location,*nrow,*ncol,*ifile,*header,*ierr;
ADDRESS nc;
{
  ATUS(rdfits)(*location,nrow,ncol,ifile,header,ierr,nc);
}
 
ATUS(ccrdvist)(location,nrow,ncol,ifile,header,ierr,nc)
ADDRESS *location,*nrow,*ncol,*ifile,*header,*ierr;
ADDRESS nc;
{
  ATUS(rdvist)(*location,nrow,ncol,ifile,header,ierr,nc);
}

ATUS(ccrddat)(location,nrow,ncol,tfile,header,nskip,ierr,nc,nc2)
ADDRESS *location,*nrow,*ncol,*tfile,*header,*ierr,*nskip;
ADDRESS nc, nc2;
{
  ATUS(rddat)(*location,nrow,ncol,tfile,header,nskip,ierr,nc,nc2);
}


#ifdef __USEWFPC 
ATUS(ccrdwfpc)(location,nrow,ncol,ifile,header,ierr,swap,nc)
ADDRESS *location,*nrow,*ncol,*ifile,*header,*ierr,*swap;
ADDRESS nc;
{
  ATUS(rdwfpc)(*location,nrow,ncol,ifile,header,ierr,swap,nc);
}
#endif

#ifdef __HAVEDST 
ATUS(ccrddst)(location,nrow,ncol,tfile,header,ierr,nc,nc2)
ADDRESS *location,*nrow,*ncol,*tfile,*header,*ierr;
ADDRESS nc,nc2;
{
  ATUS(rddst)(*location,nrow,ncol,tfile,header,ierr,nc);
}
#endif

#ifdef __USEIRAF 
ATUS(ccrdiraf)(location,nrow,ncol,ifile,header,imptr,ierr,nc)
ADDRESS *location,*nrow,*ncol,*ifile,*header,*imptr,*ierr;
ADDRESS nc;
{
  ATUS(rdiraf)(*location,nrow,ncol,ifile,header,imptr,ierr,nc);
}
#endif
 
ATUS(ccrddaopsf)(location,nrow,ncol,ifile,header,ierr,nc)
ADDRESS *location,*nrow,*ncol,*ifile,*header,*ierr;
ADDRESS nc;
{
  ATUS(rddaopsf)(*location,nrow,ncol,ifile,header,ierr,nc);
}

#ifdef __USECRI 
ATUS(ccrdcri)(location,nrow,ncol,ifile,header,ierr,nc)
ADDRESS *location,*nrow,*ncol,*ifile,*header,*ierr;
ADDRESS nc;
{
  ATUS(rdcri)(*location,nrow,ncol,ifile,header,ierr,nc);
}
#endif
 
ATUS(cccompressfileshort)(ifile,npix,a)
ADDRESS *ifile,*npix,*a;
{
  ATUS(compressfileshort)(*ifile,*npix,a);
}

ATUS(cccomfileintunshortbase)(ifile,npix,a)
ADDRESS *ifile,*npix,*a;
{
  ATUS(compressfileintunshortbase)(*ifile,*npix,a);
}

ATUS(ccexpandfileshort)(ifile,npix,a)
ADDRESS *ifile,*npix,*a;
{
  ATUS(expandfileshort)(*ifile,*npix,a);
}

ATUS(ccexpandfileintunshortbase)(ifile,npix,a)
ADDRESS *ifile,*npix,*a;
{
  ATUS(expandfileintunshortbase)(*ifile,*npix,a);
}
 

ATUS(ccinhead) (inchar,loc,card,nchar)
ADDRESS *inchar,*loc;
int *card;
ADDRESS nchar;
{
  int ATUS(inhead)();
  *card = ATUS(inhead) (inchar,*loc,nchar,*(loc+1));
}

ATUS(ccchead) (inchar,loc,card,nchar,ncard)
ADDRESS *inchar,*loc;
ADDRESS *card;
ADDRESS nchar,ncard;
{
  ATUS(chead) (inchar,*loc,card,nchar,*(loc+1),ncard);
}

ATUS(ccfhead) (inchar,loc,card,nchar)
ADDRESS *inchar,*loc;
double *card;
ADDRESS nchar;
{
  double ATUS(fhead) ();
  *card = ATUS(fhead) (inchar,*loc,nchar,*(loc+1));
}
ATUS(ccpop) (location,nsr,ner,nsc,nec,irow,icol,value)
ADDRESS *location,*nsr,*ner,*nsc,*nec,*irow,*icol,*value;
{
  ATUS(pop) (*location,nsr,ner,nsc,nec,irow,icol,value);
}

ATUS(ccpush) (location,nsr,ner,nsc,nec,irow,icol,value)
ADDRESS *location,*nsr,*ner,*nsc,*nec,*irow,*icol,*value;
{
  ATUS(push) (*location,nsr,ner,nsc,nec,irow,icol,value);
}

ATUS(ccgethjd) (loc,xerr)
ADDRESS *loc,*xerr;
{
  ATUS(gethjd) (*loc,xerr,*(loc+1));
}


ATUS(ccwrascii)(a,nrow,ncol,file,nf)
float **a;
int *nrow, *ncol;
char *file;
ADDRESS nf;
{
  FILE *fp;
  int i, j, k;

  fp = fopen(file,"w");
  printf("ncol: %d nrow: %d\n",*ncol,*nrow);
  printf("fp: %d file: %s\n",fp, file);
  fprintf(fp,"%d %d\n",*ncol,*nrow);
  k=0;
  for (i=0 ; i< *nrow ; i++) {
    for (j=0 ; j< *ncol; j++) {
     fprintf(fp,"%f ",**a++);
     if ((k++)%10 == 0) fprintf(fp,"\n");
    }
  }
  fclose(fp);
}

ATUS(ccmaskset) (location,iwd,ibt,iset,masked)
ADDRESS *location,*iwd,*ibt,*iset,*masked;
{
  ATUS(maskset) (*location,iwd,ibt,iset,masked);
}

ATUS(cczeromask)(loc,nwords)
ADDRESS *loc, *nwords;
{
  ATUS(zeromask)(*loc,nwords);
}

/*
#undef _POSIX_SOURCE
#include <math.h>
ATUS(casinh)(val,out)
double *val,*out;
{
  *out=asinh(*val);
}
*/
