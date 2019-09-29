#include "Vista.h"
/* FORTRAN callable C input-output routines                             */
#include <stdio.h>
#include <fcntl.h>

/* This routine opens a file for reading/writing of raw bytes           */ 
#ifdef  __CRAY
OPENC(onum, file, mode, nc)
#else
ATUS(openc) (onum, file, mode, nc)
#endif  /* CRAY */
 
int     *onum, *mode;
char    *file;
ADDRESS     nc;
 
{
/* Note that incoming file name MUST be null terminated! */
	if (*mode == 1) {
		*onum   = creat(file, 0644);
		if (*onum == -1)
			return (-1);
	} else if (*mode == 2) {
		*onum   = open(file, O_APPEND | O_WRONLY);
		if (*onum == -1) {
		  *onum   = creat(file, 0644);
		  if (*onum == -1) 
			return (-1);
                }
	} else {
		*onum   = open(file, 0);
		if (*onum == -1)
			return (-1);
	}

	return (0);
}
 
/* This routine closes a file                                           */ 
#ifdef  __CRAY
CLOSEC(onum)
#else
ATUS(closec) (onum)
#endif  /* CRAY */
 
int     *onum;
 
{
	close (*onum);
	return (0);
}


/*  This routine writes raw bytes to a file                            */ 
#ifdef  __CRAY
WRITEINT(onum,ibuf,nbyte)
#else
ATUS(writeint) (onum,ibuf,nbyte)
#endif  /* CRAY */
 
int     *onum;          /* Input file descriptor */
short   *ibuf;          /* Output integer buffer                        */
int     *nbyte;         /* Length of integer buffer                     */
 
{
	int numtot, numput;
 
	numtot = 0;
	while (numtot < *nbyte) {
	  numput = write (*onum, ibuf+numtot, *nbyte-numtot);
	  if (numput <= 0) break;
	  numtot += numput;
	}
	if (numtot != *nbyte)
		return (-1);
	else
		return (0);
}
 
/*  This routine reads raw bytes from a file                            */ 
#ifdef  __CRAY
READINT(onum,ibuf,nbyte)
#else
ATUS(readint) (onum,ibuf,nbyte)
#endif  /* CRAY */
 
int     *onum;          /* Input file descriptor */
short   *ibuf;          /* Output integer buffer                        */
int     *nbyte;         /* Length of integer buffer                     */
 
{
	int numtot, numgot;
 
	numtot = 0;
	while (numtot < *nbyte) {
	  numgot = read (*onum, ibuf+numtot, *nbyte-numtot);
	  if (numgot <= 0) break;
	  numtot += numgot;
	}
	if (numtot != *nbyte)
		return (-1);
	else
		return (0);
}

ATUS(removec) (file, l, nc)
char    *file;
int     *l,nc;
{ 
  *(file+*l) = 0;
  remove(file);
}

ATUS(readc)(s,l)
char s[];
int *l;
{
  int ret,i;
  char *n;

  fgets(s,*l,stdin);
  for (i=0;i<*l;i++) if (s[i]=='\n'|s[i]=='\0') s[i]=' ';
/*  for (i=0;i<*l;i++) printf("%d %c",i,s[i]);   */
  return(*l);
}

#ifdef __DECSTA
dectime_()
{
    return(time((long *)0));
}
#endif
/************************ NUMBER ************************************/
int getnumb_( str, otype, inum, fnum )
			/* str is a C string, n a pointer to a normal
			for the return value; V_DPL is set to the decimal
			point location if float (contains . and optionally
			a following e or an embedded + or -; e-scientific
			notation no work if base >15; sorry.*/
    char *str;
    int *otype, *inum;
    double *fnum;
{
    int len ;
    int eflag = 0;
    int eaccum = 0;
    char c;
    int d;
    int isminus = 0;
    int esign = 1;
    char *eptr;
    unsigned int accum = 0;
    double faccum = 0.;
    double ftemp = 0.;
    double fac;
    int expon;
    int fflag = 0;
    int V_DPL = -100;
    int base = 10;
 
    len = strlen(str);
    *inum = 0;
    *fnum = 0.;
    *otype = 0;
    if ( *str == '-' ) {
	isminus = 1;
	len--;
	++str;
    } else if( *str == '+' ) {   /* correctly interpret gratuitous leading + */
	len--;
	++str;
    }
    for( ; len > 0; len-- ) {
	c = *str++;
	if ( (c == '.') && fflag == 0 && eflag == 0){
	    V_DPL = 0;
	    fflag = 1;
	    faccum = (double)accum;
	} else if ( !eflag &&
	    (( c == 'e' || c == 'E') || ( c == 'd' || c == 'D') )){
	    eflag = 1;
	    eptr = str ;  /* one greater than pointer to e */
	} else if (eflag && (c == '-') && ((str - eptr) == 1)){
	    esign = -1;       /* negative exponent */
            if (fflag==0) {   /* modified to get exponents on numbers without */
              fflag = 1;      /* decimal point, 7/93 holtz. */
              V_DPL = 0;
            }
	} else if (eflag && (c == '+') && ((str - eptr) == 1)){
	    esign = 1;       /* negative exponent */
	} else{
	    if( -1 == (d = digit( base, c )) ){
		fflag = 0;
		break;      /* no good */
	    }
	    if(!eflag){
		++V_DPL;
		if (fflag) faccum = faccum * (double)base + (double)d;
		else accum = accum * base + d;
		/* this nonsense is to accomodate float numbers with more
		'precision' than can be represented by a scaled integer. Bah. */
	    } else{
		eaccum = eaccum * base + d;
	    }
	}
    }
    if (len) {
      *otype = 3;
      return ( 0 );     /* not a legal number */
    }
 
    if (V_DPL < 0){      /* int */
	V_DPL = -1;
	*otype = 1;
         /* added code to get integers with exponents, 7/93 holtz. */
        if (eflag) {
	  expon = esign * eaccum;
	  fac = (double)base;
	  *inum = accum;
	  for (d=0; d<expon; d++)  *inum *= fac ;
	  *inum = isminus ? -(*inum) : *inum;
        } else
        *inum = isminus ? -accum : accum ;
    } else {   /* float */
	expon = -V_DPL + esign * eaccum;
	fac = (double)base;
	if (expon <0 ){
	    fac = 1./fac ;
	    expon = -expon;
	}
	*otype = 2;
	ftemp = faccum;
	for (d=0; d<expon; d++)  ftemp *= fac ;
	*fnum = isminus ? -ftemp : ftemp;
    }
    return ( 1 );
}
/************************ DIGIT() *******************************/
/*
 * Converts the character c into a digit in base 'base'.
 * Returns the digit or -1 if not a valid digit.
 * Accepts either lower or upper case letters for bases larger than ten.
 * Only works for ASCII.
 */
int 
digit ( base, c )
    int base;
    char c;
{
    register int ival = c;
 
    ival -= '0';
    if( ival < 0 )
     return(-1);
    if( ival > 9 ) {
    ival -= ('A' - '0' - 10);
    if( ival < 10 )
	return(-1);
    if( ival > ('Z' - 'A' + 11) ) {
	ival -= ('a' - 'A');
	if( ival < 10 )
	return(-1);
    }
    }
    return( ival >= base ? -1 : ival );
}
