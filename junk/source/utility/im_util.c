/*
 *						im_util.c
 *						Sepetember 28, 1993
 */


#include <stdio.h>

/* A list of key words which are handled by im_get() and im_put().
 * (NAXIS1, NAXIS2, etc. keywords are not listed here but are handled.)
 * Other key words can be assigned from the outside.
 */
#define TAB_SIZE 15
char *im_handled_cards[TAB_SIZE] = {
"SIMPLE", "BITPIX", "NAXIS", "HIBYTEHI", "HIIEEEHI", "DECORDER",
"BSCALE", "BZERO", "COMPRSN", "COMP_VER", "END", NULL
};


