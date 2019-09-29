/*
Include file to facilitate C/FORTRAN compatibility

Jeff Hester, CIT
WARNING:  THIS INCLUDE FILE MUST COME AFTER STDIO
*/

unsigned char _fortran_true = 0xff;
unsigned char _fortran_false = 0x00;

#define TRUE (&_fortran_true)
#define FALSE (&_fortran_false)

#include descrip

#ifndef FCOMPAT

struct dsc$descriptor_s *strdes();
struct dsc$descriptor_s *strndes();
char *strptr();

#endif
