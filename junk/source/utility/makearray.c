#include "Vista.h"
/*
MAKEARRAY() -- Generate space for a contiguous array (which FORTRAN can
understand) and map into it an array of pointers (which C can more easily
deal with).

Jeff Hester, CIT

Some bits in makearray() having to do with isize and iptr commented out as
they cause compiler complaints, esp. on fully ANSI compliant C compilers,
but have no identifiable function.  Vestiges of earlier code? 
[rwp/osu; 96Dec01]
*/

#include <stdio.h>

char *makearray(xsize,ysize,elsize,array)
int xsize,ysize,elsize;
char ***array;
{
  char *malloc();
  char *ptr;
  int j;

/* int *iptr, isize; */

  (*array) = (char **)malloc(4*ysize);
  if(*array==NULL) {
    fprintf(stderr,
	    "MAKEARRAY: Memory allocation failed for pointers. %d bytes\n"
	    ,4*ysize);
    perror("malloc");
    return(0);
  }
  ptr=malloc(elsize*xsize*ysize);
  if(ptr==NULL) {
    fprintf(stderr,"MAKEARRAY: Memory allocation failed for data. %d bytes\n",
	    elsize*xsize*ysize);
    perror("malloc");
    return(0);
  }

/*
  isize=(elsize*xsize*ysize+3)/4;
  iptr=ptr;
  for(j=0; j<isize; j++) *iptr++ =0;
*/

  for(j=0; j<ysize; j++) {
    (*array)[j]=ptr+xsize*elsize*j;
  }
  return(ptr);
}


/*
FREEARRAY() -- FREE THE SPACE ALLOCATED WITH A CALL TO MAKEARRAY
*/

freearray(array)
char **array;
{
  int bad=0;

#ifdef VMS
  if(free(array[0])) {
    fprintf(stderr,"Error freeing array (main space)\n");
    perror("free");
    bad= -1;
  }
  if(free((char *)array)) {
    fprintf(stderr,"Error freeing array (pointer space)\n");
    perror("free");
    bad= -1;
  }
#else
  free(array[0]);
  free((char *)array);
#endif
  return(bad);
}


/*Subroutine to do malloc with test for valid return.*/

char *tstalloc(length)
unsigned length;
{
  char *tptr;
  char *malloc();

  tptr=malloc(length);
  if(tptr==0) {
    printf("tstalloc:  Unsuccessful allocation of %d bytes. Must exit.\n",
	   length);
    exit(0);
  }
  return(tptr);
}
