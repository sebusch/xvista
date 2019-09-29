#include <stdio.h>
#include <math.h>
#include "sntele.h"

void telinit() 
{
  int ifilt, i, itele;
  double f=10.;

  /* SDSS 2.5m */
  itele = 0;
  /* rough zeropoints to go from calibrated mags to estimated counts */
  teles[itele].zeropt[0] = 0.84;
  teles[itele].zeropt[1] = 3.32;
  teles[itele].zeropt[2] = 3.14;
  teles[itele].zeropt[3] = 2.81;
  teles[itele].zeropt[4] = 1.13;
  for (ifilt=0;ifilt<MAXFILT;ifilt++) {
    for (i=0;i<=NDIST;i++){
      teles[itele].xdist[i][ifilt] = 0;
      teles[itele].ydist[i][ifilt] = 0;
    }
    teles[itele].tfit[ifilt] = 0;
    teles[itele].tfilt[ifilt] = ifilt;
    teles[itele].trans[ifilt] = 0.;
    teles[itele].fitrad = 2/3600.;
    teles[itele].scale[ifilt] = pow(f,0.4*teles[itele].zeropt[ifilt]); 
fprintf(stderr,"teles ifilt %d %f %f\n",ifilt,teles[itele].zeropt[ifilt],teles[itele].scale[ifilt]);
  }

  /* MDM 2.4m */
  itele = 4;
  /* rough zeropoints to go from calibrated mags to estimated counts */
  teles[itele].zeropt[0] = 0.84;
  teles[itele].zeropt[1] = 3.32;
  teles[itele].zeropt[2] = 3.14;
  teles[itele].zeropt[3] = 2.81;
  teles[itele].zeropt[4] = 1.13;
  for (ifilt=0;ifilt<MAXFILT;ifilt++) {
    for (i=0;i<=NDIST;i++){
      teles[itele].xdist[i][ifilt] = 0;
      teles[itele].ydist[i][ifilt] = 0;
    }
    teles[itele].tfit[ifilt] = 1;
    if (ifilt<3) 
      teles[itele].tfilt[ifilt] = ifilt+1;
    else
      teles[itele].tfilt[ifilt] = ifilt-1;
    teles[itele].trans[ifilt] = 0.;
    teles[itele].fitrad = 2/3600.;
    teles[itele].scale[ifilt] = pow(f,0.4*teles[itele].zeropt[ifilt]); 
fprintf(stderr,"teles ifilt %d %f %f\n",ifilt,teles[itele].zeropt[ifilt],teles[itele].scale[ifilt]);
  }
}

