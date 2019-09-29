#include <stdio.h>

typedef struct {
  int n;
  double dx;
  double fwhmpix;
  int nlib;
  float *data;
} PSF;

PSF spsf;
double psf(PSF *psf, double x, double y, double *, double *);

void initpsf_(float **psfbuf, int *psfn, double *psfdx, double *psffwhm)
{
  spsf.data = *psfbuf; 
  spsf.dx = *psfdx; 
  spsf.fwhmpix = *psffwhm; 
  //spsf.nlib = (int)rint(1./spsf.dx);
  spsf.nlib = (int)(1./spsf.dx+0.001);
  spsf.n = *psfn;
}

void getpsfbuf_(double *dx, double *dy, double *val)
{
  double dpdx, dpdy;
  *val = psf(&spsf,*dx,*dy,&dpdx,&dpdy);
}
