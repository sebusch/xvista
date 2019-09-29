#ifndef havesnphot
  #include "snphot.h"
#endif
#define NTELE 10
#define NDIST 0

typedef struct {
  double xdist[NDIST][MAXFILT];
  double ydist[NDIST][MAXFILT];
  double trans[MAXFILT];
  int tfilt[MAXFILT];
  int tfit[MAXFILT];
  double zeropt[MAXFILT];
  double scale[MAXFILT];
  float fitrad;  /*fitting radius in degrees */
} TELESCOPE;

TELESCOPE teles[NTELE];

