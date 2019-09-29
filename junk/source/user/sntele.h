#ifndef havesnphot
  #include "snphot.h"
#endif
#define NTELE 10
#define MAXDIST 10

typedef struct {
  int ndist;
  double xdist[MAXDIST][MAXFILT];
  double ydist[MAXDIST][MAXFILT];
  double trans[MAXFILT];
  double transerr[MAXFILT];
  int tfilt[MAXFILT];
  int tfit[MAXFILT];
  double zeropt[MAXFILT];
  double f0[MAXFILT];
  double ext[MAXFILT];
  float fitrad;  /*fitting radius in degrees */
  float psfrad;  /*psf radius in degrees */
} TELESCOPE;

TELESCOPE teles[NTELE];

