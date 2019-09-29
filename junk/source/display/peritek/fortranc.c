/*
Routines to facilitate mixed FORTRAN / C code.
These routines deal with FORTRAN string descriptors of the type passed by
FORTRAN for CHARACTER*(*) objects.

Jeff Hester, CIT, 1986
*/

#define FORTRANCOMPAT
#include "fortrancompat"


/*
Construct a FORTRAN string descriptor for a C character string.
*/

struct dsc$descriptor_s *strdes(string)
char *string;
{
char *malloc();
struct dsc$descriptor_s *s;

s=malloc(sizeof(struct dsc$descriptor_s));
s->dsc$w_length=strlen(string);
s->dsc$b_dtype=DSC$K_DTYPE_T;
s->dsc$b_class=DSC$K_CLASS_S;
s->dsc$a_pointer=string;
return(s);
}


/*
Construct a FORTRAN string descriptor for a C character string of length n.
*/

struct dsc$descriptor_s *strndes(string,n)
char *string;
int n;
{
char *malloc();
struct dsc$descriptor_s *s;

s=malloc(sizeof(struct dsc$descriptor_s));
s->dsc$w_length=n-1;
s->dsc$b_dtype=DSC$K_DTYPE_T;
s->dsc$b_class=DSC$K_CLASS_S;
s->dsc$a_pointer=string;
return(s);
}


/*
Convert from a FORTRAN string descriptor to a C pointer.
*/

char *strptr(descriptor)
struct dsc$descriptor_s *descriptor;
{
return(descriptor->dsc$a_pointer);
}

