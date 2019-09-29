/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyright (c) 1991 Steven L. Allen
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"
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

void ATUS(sunieee)(ieeestat)
int *ieeestat;  /* return value of ieee_flags("get"...) */
{
	char out[16];   /* output string from Sun ieee_flags function */
	int i;          /* return val of ieee_flags("clearall"...) */

	/* find out what exceptions have occurred since last cleared */
	*ieeestat = ieee_flags( "get", "exception", "all", out);

	/* inform the user what exceptions have ocurred */
	/* we mask out the uninteresting exceptions here */
	if (*ieeestat &= INTERESTING) {
		fprintf(stderr,"\n\r");
		fprintf(stderr,"IEEE exceptions have occurred\n\r");
	}
	if (*ieeestat & INEXACT)
		fprintf(stderr,"Inexact; ");
	if (*ieeestat & DIVISION)
		fprintf(stderr,"Division by 0; ");
	if (*ieeestat & UNDERFLOW)
		fprintf(stderr,"Underflow; ");
	if (*ieeestat & OVERFLOW)
		fprintf(stderr,"Overflow; ");
	if (*ieeestat & INVALID)
		fprintf(stderr,"Invalid Operand; ");
#ifdef  i386
	if (*ieeestat & DENORMALIZED)
		fprintf(stderr,"Denormalized; ");
#endif  /* i386 */
	if (*ieeestat) fprintf(stderr,"\n\r");

	/* clear all exceptions and return */
	i = ieee_flags( "clearall", " ", " ", out);
}
