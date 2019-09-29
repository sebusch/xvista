#include "Vista.h"
/*	Routines to do image scalar arithmatic.				*/

/* Author:	Tod R. Lauer	8/17/87					*/
/*		Princeton University Observatory			*/
/* Modified by J Holtzman 3/89 to get addresses from FORTRAN by reference */
/*	Routine to add a constant to an image				*/
ATUS(addcon)(a,sr,sc,nrow,ncol,ncola,f)
float   **a;
float	*f;		/* Constant to add				*/

int	*nrow, *ncol,	/* Number of rows and columns to add		*/
        *ncola;          /* Number of compiled columns                   */
int     *sr, *sc;
{
	register float *aim, c;
	int irow, icol;

	aim	= *a + *sr * (*ncola) + *sc;	/* Initialize pointer 	*/
	c	= *f;
        for (irow = 1; irow <= *nrow; irow++) {
          for (icol = 1; icol <= *ncol; icol++)
                *aim++  += c;
          aim += *ncola - *ncol;
        }

	return (0);
}
/*	Routine to multiply an image by a constant			*/
ATUS(mulcon)(a,sr,sc,nrow,ncol,ncola,f)
float	**a,		/* Destination image				*/
	*f;		/* Constant to multiply by			*/

int	*nrow,*ncol,	/* Number of pixels in image  			*/
        *ncola;         /* Number of compiled columns                   */
int     *sr, *sc;
{
	register float *aim, c;
	int irow, icol;

	aim	= *a + *sr * (*ncola) + *sc;	/* Initialize pointer 	*/
	c	= *f;
        for (irow = 1; irow <= *nrow; irow++) {
          for (icol = 1; icol <= *ncol; icol++)
                *aim++  *= c;
          aim += *ncola - *ncol;
        }

	return (0);
}
#define NINT(x) ((int)(x+0.49))

/*	Routine to AND an image by a constant			*/
ATUS(andcon)(a,sr,sc,nrow,ncol,ncola,ic)
float	**a;		/* Destination image				*/
int	*ic;		/* Constant to multiply by			*/

int	*nrow,*ncol,	/* Number of pixels in image  			*/
        *ncola;         /* Number of compiled columns                   */
int     *sr, *sc;
{
	register float *aim, c;
	int irow, icol;

	aim	= *a + *sr * (*ncola) + *sc;	/* Initialize pointer 	*/
        for (irow = 1; irow <= *nrow; irow++) {
          for (icol = 1; icol <= *ncol; icol++)
                *aim++ = (float)(NINT(*aim) & *ic);
          aim += *ncola - *ncol;
        }

	return (0);
}
/*	Routine to OR an image by a constant			*/
ATUS(orcon)(a,sr,sc,nrow,ncol,ncola,ic)
float	**a;		/* Destination image				*/
int	*ic;		/* Constant to multiply by			*/

int	*nrow,*ncol,	/* Number of pixels in image  			*/
        *ncola;         /* Number of compiled columns                   */
int     *sr, *sc;
{
	register float *aim, c;
	int irow, icol;

	aim	= *a + *sr * (*ncola) + *sc;	/* Initialize pointer 	*/
        for (irow = 1; irow <= *nrow; irow++) {
          for (icol = 1; icol <= *ncol; icol++)
                *aim++ = (float)(NINT(*aim) | *ic);
          aim += *ncola - *ncol;
        }

	return (0);
}

