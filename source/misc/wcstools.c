#include <stdio.h>
#include "wcs.h"

struct WorldCoor *wcs;

fwcskinit_(int *naxis1, int *naxis2, double *crpix1, double *crpix2, double *crval1, double *crval2, double *cd1_1, double *cd1_2, double *cd2_1, double *cd2_2)
{
  double cd[2][2];

  cd[0][0] = *cd1_1;
  cd[0][1] = *cd1_2;
  cd[1][0] = *cd2_1;
  cd[1][1] = *cd2_2;

fprintf(stderr,"calling wcskinit\n");
  wcs = wcskinit(naxis1, naxis2, "RA--TAN", "DEC--TAN", crpix1, crpix2, crval1, crval2, &cd, 0, 0, 0, 2000, 0.); 
}

fwcs2pix_(double *ra, double *dec, double *x, double *y)
{
  int offscl;

  wcs2pix(wcs, *ra, *dec, x, y, &offscl);
}
