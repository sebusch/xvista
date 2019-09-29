#include <stdio.h>
#include <math.h>
#include "sntele.h"

void telinit() 
{
  int ifilt, i, itele;
  double f=10.;

  /* SDSS 2.5m */
  itele = 0;
  /* rough zeropoints to go from calibrated mags to zeropt counts */
  // Zeropoints and extinction taken from arbitrary SDSS run, exptime from www.sdss.org
  //teles[itele].zeropt[0] = -23.96+0.46*1.2-2.5*log10(53.907456);
  //teles[itele].zeropt[1] = -24.55+0.15*1.2-2.5*log10(53.907456);
  //teles[itele].zeropt[2] = -24.04+0.076*1.2-2.5*log10(53.907456);
  //teles[itele].zeropt[3] = -23.65+0.033*1.2-2.5*log10(53.907456);
  //teles[itele].zeropt[4] = -22.00+0.026*1.2-2.5*log10(53.907456);
  // Add in gain factor to work in electrons to allow for different gains
  //  in different camcols
  teles[itele].zeropt[0] = -23.96-2.5*log10(1.62);
  teles[itele].zeropt[1] = -24.55-2.5*log10(3.32);
  teles[itele].zeropt[2] = -24.04-2.5*log10(4.71);
  teles[itele].zeropt[3] = -23.65-2.5*log10(5.17);
  teles[itele].zeropt[4] = -22.00-2.5*log10(4.75);
  teles[itele].ext[0] = 0.5;
  teles[itele].ext[1] = 0.2;
  teles[itele].ext[2] = 0.1;
  teles[itele].ext[3] = 0.05;
  teles[itele].ext[4] = 0.03;
  for (ifilt=0;ifilt<MAXFILT;ifilt++) {
    teles[itele].ndist = 0;
    for (i=0;i<=MAXDIST;i++){
      teles[itele].xdist[i][ifilt] = 0;
      teles[itele].ydist[i][ifilt] = 0;
    }
    teles[itele].tfit[ifilt] = 0;
    teles[itele].tfilt[ifilt] = ifilt;
    teles[itele].trans[ifilt] = 0.;
    teles[itele].fitrad = 1/3600.;
    teles[itele].psfrad = 4/3600.;
    //teles[itele].f0[ifilt] = 1.e10*pow(f,0.4*(teles[itele].zeropt[ifilt])); 
    teles[itele].f0[ifilt] = pow(f,-0.4*(teles[itele].zeropt[ifilt])); 
  }

  /* MDM 2.4m */
  itele = 4;
  /* rough zeropoints to go from calibrated mags to zeropt counts */
  teles[itele].zeropt[0] = -24.;
  teles[itele].zeropt[1] = -24.8;
  teles[itele].zeropt[2] = -24.8;
  teles[itele].zeropt[3] = -24.;
  teles[itele].zeropt[4] = -23.;
  teles[itele].ext[0] = 0.5;
  teles[itele].ext[1] = 0.2;
  teles[itele].ext[2] = 0.1;
  teles[itele].ext[3] = 0.05;
  teles[itele].ext[4] = 0.03;
  for (ifilt=0;ifilt<MAXFILT;ifilt++) {
    teles[itele].ndist = 0;
    for (i=0;i<=MAXDIST;i++){
      teles[itele].xdist[i][ifilt] = 0;
      teles[itele].ydist[i][ifilt] = 0;
    }
    teles[itele].tfit[ifilt] = 0;
    if (ifilt==1)
      teles[itele].trans[ifilt] = -0.1;
    else if (ifilt==2)
      teles[itele].trans[ifilt] = -0.05;
    else if (ifilt==3)
      teles[itele].trans[ifilt] = 0.08;
    else 
      teles[itele].trans[ifilt] = 0.;
    if (ifilt<3) 
      teles[itele].tfilt[ifilt] = ifilt+1;
    else
      teles[itele].tfilt[ifilt] = ifilt-1;
    teles[itele].fitrad = 1/3600.;
    teles[itele].psfrad = 4/3600.;
    teles[itele].f0[ifilt] = pow(f,-0.4*(teles[itele].zeropt[ifilt])); 
//fprintf(stderr,"teles ifilt %d %f %f\n",ifilt,teles[itele].zeropt[ifilt],teles[itele].f0[ifilt]);
  }

  /* UH 88in */
  itele = 5;
  /* rough zeropoints to go from calibrated mags to zeropt counts */
  teles[itele].zeropt[0] = -25.4;
  teles[itele].zeropt[1] = -26.2;
  teles[itele].zeropt[2] = -26.2;
  teles[itele].zeropt[3] = -25.4;
  teles[itele].zeropt[4] = -24.4;
  teles[itele].ext[0] = 0.5;
  teles[itele].ext[1] = 0.2;
  teles[itele].ext[2] = 0.1;
  teles[itele].ext[3] = 0.05;
  teles[itele].ext[4] = 0.03;
  for (ifilt=0;ifilt<MAXFILT;ifilt++) {
    teles[itele].ndist = 0;
    for (i=0;i<=MAXDIST;i++){
      teles[itele].xdist[i][ifilt] = 0;
      teles[itele].ydist[i][ifilt] = 0;
    }
    teles[itele].tfit[ifilt] = 0;
    if (ifilt<3) 
      teles[itele].tfilt[ifilt] = ifilt+1;
    else
      teles[itele].tfilt[ifilt] = ifilt-1;
    //teles[itele].trans[ifilt] = 0.;
    if (ifilt==1)
      teles[itele].trans[ifilt] = -0.09;
    else if (ifilt==2)
      teles[itele].trans[ifilt] = -0.01;
    else if (ifilt==3)
      teles[itele].trans[ifilt] = 0.04;
    else if (ifilt==4)
      teles[itele].trans[ifilt] = -0.04;
    else 
      teles[itele].trans[ifilt] = 0.;
    teles[itele].fitrad = 1/3600.;
    teles[itele].psfrad = 4/3600.;
    teles[itele].f0[ifilt] = pow(f,-0.4*(teles[itele].zeropt[ifilt])); 
//fprintf(stderr,"teles ifilt %d %f %f\n",ifilt,teles[itele].zeropt[ifilt],teles[itele].f0[ifilt]);
  }

  /* FLWO */
  itele = 8;
  /* rough zeropoints to go from calibrated mags to zeropt counts */
  teles[itele].zeropt[0] = -21.0;
  teles[itele].zeropt[1] = -21.8;
  teles[itele].zeropt[2] = -21.8;
  teles[itele].zeropt[3] = -21.6;
  teles[itele].zeropt[4] = -20.6;
  teles[itele].ext[0] = 0.5;
  teles[itele].ext[1] = 0.2;
  teles[itele].ext[2] = 0.1;
  teles[itele].ext[3] = 0.05;
  teles[itele].ext[4] = 0.03;
  for (ifilt=0;ifilt<MAXFILT;ifilt++) {
    teles[itele].ndist = 0;
    for (i=0;i<=MAXDIST;i++){
      teles[itele].xdist[i][ifilt] = 0;
      teles[itele].ydist[i][ifilt] = 0;
    }
    teles[itele].tfit[ifilt] = 1;
    if (ifilt<3) 
      teles[itele].tfilt[ifilt] = ifilt+1;
    else
      teles[itele].tfilt[ifilt] = ifilt-1;
    teles[itele].trans[ifilt] = 0.;
    teles[itele].fitrad = 1/3600.;
    teles[itele].psfrad = 4/3600.;
    teles[itele].f0[ifilt] = pow(f,-0.4*(teles[itele].zeropt[ifilt])); 
//fprintf(stderr,"teles ifilt %d %f %f\n",ifilt,teles[itele].zeropt[ifilt],teles[itele].f0[ifilt]);
  }
}

