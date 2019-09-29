#include "Vista.h"
#include "snphot.h"
#include "sntele.h"
#include "nrutil.h"
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define D2R 3.14159/180.
#define MAXITER 1
#define MAXSTAR 500
#define MAXGAL 1
#define MAXSTRIP 2
#undef STRIPINTEN  
#define STRIPPM
#define MAXVAR 100
#define MAXPAR 2+MAXFRAME*(NBACK+3)-3+NTELE*MAXFILT*(2*MAXDIST+1)+ \
               MAXSTAR*(4+MAXFILT)+MAXGAL*MAXSIZE*MAXSIZE*MAXFILT+MAXSTRIP*5
#define TRUE 1
#define FALSE 0
#define LOCKSTARINTEN 0x1
#define LOCKSTARPOS 0x2
#define FITSTARPM 0x4
#define LOCKGAL 0x8
#define LOCKFRAME 0x10
#define LOCKFRAMEINTEN 0x20
#define LOCKFRAMEPOS 0x40
#define LOCKFRAMEASTROM 0x80
#define LOCKVPOS 0x100
#define LOCKVINTEN 0x200
#define LOCKSTRIPPM 0x400
#define DISTORT

#define FLAG_BRIGHTSKY  1
#define FLAG_PSF        2
#define FLAG_CLOUDY     4
#define FLAG_VARSKY     8
#define FLAG_VARSKYALL 16
#define FLAG_BRIGHTGAL 32
#define FLAG_FEWSTARS  64
#define FLAG_STARCHI  128
#define FLAG_CONVERGE 256
#define FLAG_NOSTARS  512
#define FLAG_BAD     1024

void skytoxy(FRAME frame, double dec, double ra, double *x, double *y);
void xytosky(FRAME frame, double x, double y, double *dec, double *ra);
void dsolve(FRAME frame, double dec, double ra, double *dx, double *dy);
void getsncoord(FRAME frame, VAR *var);
void getscoord(FRAME frame, STAR *star);
void getgcoord(FRAME frame, GAL *gal, double, double);
double psf(PSF *psf, double x, double y, double *, double *);
void psfint(FRAME *, int smooth);
double pixdist2(FRAME, double, double, double , double, float fitrad);
float back(FRAME *frame, int ix, int iy, int verbose);
int setpar(int set, PAR par[MAXPAR], 
           int *ncombo, int *nfilt, int filt[],
           FRAME *frame, int nframe, 
           STAR *star, int nstar, GAL *gal, int ngal, VAR *var, int nvar, int *converge);
double update_par(PAR *,double,double);
double update_pos(PAR *, double,double);
double update_inten(PAR *, double,int);
double mag(double);
double magerr(double,double);
double asinhmag(double,double,int);
double asinhmagerr(double,double,int);
double asinhinv(double,int);
double asinhinverr(double,double,int);
double gexp(double);
double glog(double);
double dg(double);
void telinit();
int snphot(PAR par[], int oldpar, FRAME frame[], int nframe, 
            STAR star[], int nstar, int maxstar,
            GAL gal[], int ngal, VAR *var, int nvar, int, int, int, int, double *);
STRIP strip[MAXSTRIP];
int nstrip;

void dosnphot_(int *nim, float **locframe, char **lochead, float **locpsf, int *psfn, double *psfdx, double *psffwhm, int *maxiter, int *maxvar, int *galsize, float *dxgal, int *maxstar, float *dmax, int *fitsky, int *sub, int *lock, float *djdmin, float *djdmax, float *critstarerr, float *critstarchi, float *critskyrat, float *critskymed, float *critscale, float *critfwhm, int *skipgal, char *starfile, char *varfile, char *galfile, char *parfile, int ns, int nsn, int ng, int np)
{
  FRAME frame[MAXFRAME];
  STAR star[MAXSTAR];
  GAL gal[MAXGAL];
  VAR var[MAXVAR];
  PAR par[MAXPAR];
  FILE *fp, *fpp, *fsum;
#define MAXLINE 300
  char line[MAXLINE];
  char out[20], card[9];
  int id, i, j, l, itele, ifilt, ivar, istar, nstar;
  int igx, igy, ngal, nvar, igal, niter, ival, ix, iy, jgx, jgy;
  double pi=3.14159, sig2, dist2;
  double gra, gdec, gtot, gerr, neff, gerr0, serr, p, dpdx, dpdy;
  int oldstrip, istrip, jframe, new;
  size_t nline;
  int good, iframe, nframe, iback, smooth, ifile, oldpar, novar[MAXFILT], yesvar[MAXFILT];
  float x, y, ddec, dra;
  double scale, jdmax, cback, err, framechi, f0, merr, merrg, merrs, criterr, starchi, emax[MAXFRAME];
  double chi2final;
  float sky[MAXFILT][MAXFRAME], skyrat[MAXFILT][MAXFRAME];
  int nsky[MAXFILT];
  float medsky[MAXFILT], medskyrat[MAXFILT], perc=0.5;
  PSF *fpsf;
  double psfrad, fwhm, pixscale;

  *dmax /= 3600.;

  // Load up variable & galaxy information
  nvar=ngal=0;
  if (varfile[0] != 0) {
    fp = fopen(varfile,"r");
    if (fp==NULL) {
      fprintf(stderr,"Error opening varfile: %s\n",varfile);
    }
    while (fgets(line,MAXLINE,fp) != NULL && nvar<MAXVAR) {
      sscanf(line,"%d%lf%lf%lf",&var[nvar].id,&var[nvar].ra,&var[nvar].dec,&jdmax);
      if (nvar==0&&var[0].ra>270) var[nvar].ra -= 360;
      if (nvar>0&&fabs(var[0].ra)<90&&var[nvar].ra>180) var[nvar].ra -= 360;
      if (jdmax>nvar) {
        var[nvar].jdmin=jdmax+*djdmin;
        var[nvar].jdmax=jdmax+*djdmax;
      } else {
        var[nvar].jdmin=-1;
        var[nvar].jdmax=1.e10;
      }
      gal[nvar].ra=var[nvar].ra; 
      gal[nvar].dec=var[nvar].dec; 
      gal[nvar].nsize=*galsize;
      gal[nvar].dx=*dxgal/3600.;
      ddec=var[nvar].dec-var[0].dec;
      if (fabs(ddec) < *dmax) {
        dra=(var[nvar].ra-var[0].ra)*cos(var[0].dec*D2R);
        if (fabs(dra) < *dmax) {
          if ((dra*dra+ddec*ddec)<((*dmax)*(*dmax))) nvar++;
        }
      }
    }
    fclose(fp);
    fprintf(stderr,"SN: %f %f %f %f\n",var[0].ra,var[0].dec,var[0].jdmin,var[0].jdmax);
    if (*galsize>0)
      ngal = nvar;
    else
      ngal = 0;
  }

  // initialize telescope information 
  telinit();

  // Load up frame and psf information
  nframe = *nim;
  for (i=0;i<MAXFILT;i++) {
    novar[i] = 0;
    yesvar[i] = 0;
    nsky[i] = 0;
  }
  for (i=0;i<nframe;i++) {
   frame[i].data = *(locframe+i);
   ATUS(ccinhead)("NAXIS1  ",lochead+i*2,&frame[i].nx,8);
   ATUS(ccinhead)("NAXIS2  ",lochead+i*2,&frame[i].ny,8);
   ATUS(ccfhead)("CRVAL1  ",lochead+i*2,&frame[i].crval1,8);
   if (fabs(var[0].ra)<90&&frame[i].crval1>180) frame[i].crval1 -= 360;
   ATUS(ccfhead)("CRVAL2  ",lochead+i*2,&frame[i].crval2,8);
   ATUS(ccfhead)("CRPIX1  ",lochead+i*2,&frame[i].crpix1,8);
   ATUS(ccfhead)("CRPIX2  ",lochead+i*2,&frame[i].crpix2,8);
   ATUS(ccinhead)("CNPIX1  ",lochead+i*2,&frame[i].cnpix1,8);
   ATUS(ccinhead)("CNPIX2  ",lochead+i*2,&frame[i].cnpix2,8);
   ATUS(ccfhead)("CD1_1   ",lochead+i*2,&frame[i].cd1_1,8);
   ATUS(ccfhead)("CD1_2   ",lochead+i*2,&frame[i].cd1_2,8);
   ATUS(ccfhead)("CD2_1   ",lochead+i*2,&frame[i].cd2_1,8);
   ATUS(ccfhead)("CD2_2   ",lochead+i*2,&frame[i].cd2_2,8);
   for (j=0;j<5;j++) frame[i].apcor[j] = 0;
   ATUS(ccfhead)("SNPAPCOR",lochead+i*2,&frame[i].apcor[1],8);
   if (frame[i].apcor[1] < 0.001) frame[i].apcor[1] = 1.;
   ATUS(ccfhead)("SNPSCALE",lochead+i*2,&frame[i].zeropt,8);
   if (frame[i].zeropt < 0.001) frame[i].zeropt = 1.;
   ATUS(ccinhead)("TELE    ",lochead+i*2,&frame[i].tele,8);
   ATUS(ccinhead)("CAMCOL  ",lochead+i*2,&frame[i].camcol,8);
   ATUS(ccinhead)("FIELD   ",lochead+i*2,&frame[i].field,8);
   ATUS(ccinhead)("FILT    ",lochead+i*2,&frame[i].filt,8);
   ATUS(ccchead)("STRIP   ",lochead+i*2,card,8,9);
   frame[i].strip = 0;
   if (strncmp(card,"N",1) == 0) frame[i].strip = 1;
   if (strncmp(card,"S",1) == 0) frame[i].strip = 2;
   ATUS(ccfhead)("MJD     ",lochead+i*2,&frame[i].jd,8);
   ATUS(ccfhead)("EXPTIME ",lochead+i*2,&frame[i].exp,8);
   if (frame[i].zeropt < 0.001) frame[i].exp = 1.;
   ATUS(ccfhead)("AM      ",lochead+i*2,&frame[i].am,8);
   if (frame[i].am < 0.001) frame[i].am = 1.2;
   ATUS(ccfhead)("GAIN    ",lochead+i*2,&frame[i].gain,8);
   ATUS(ccfhead)("RN      ",lochead+i*2,&frame[i].rn,8);
   ATUS(ccfhead)("LOWBAD  ",lochead+i*2,&frame[i].lowbad,8);
   ATUS(ccfhead)("HIGHBAD ",lochead+i*2,&frame[i].highbad,8);
   ATUS(ccinhead)("DISTORT ",lochead+i*2,&l,8);
   if (l) {
     for (j=0;j<10;j++) {
       if (j+1<10)
         sprintf(card,"X(%1d,1)  ",j+1);
       else
         sprintf(card,"X(%2d,1) ",j+1);
       ATUS(ccfhead)(card,lochead+i*2,&frame[i].xdist[j],8);
       if (j+1<10)
         sprintf(card,"X(%1d,2)  ",j+1);
       else
         sprintf(card,"X(%2d,2) ",j+1);
       ATUS(ccfhead)(card,lochead+i*2,&frame[i].ydist[j],8);
     }
   } else {
     itele = frame[i].tele;
     ifilt = frame[i].filt;
     for (j=0;j<10;j++) {
       frame[i].xdist[j] = teles[itele].xdist[j][ifilt];
       frame[i].ydist[j] = teles[itele].ydist[j][ifilt];
     }
   }
   ATUS(ccfhead)("BACK    ",lochead+i*2,&frame[i].back[0],8);
   ATUS(ccfhead)("BACKC   ",lochead+i*2,&frame[i].back[1],8);
   ATUS(ccfhead)("BACKR   ",lochead+i*2,&frame[i].back[2],8);
   ATUS(ccfhead)("BACKC2  ",lochead+i*2,&frame[i].back[3],8);
   ATUS(ccfhead)("BACKR2  ",lochead+i*2,&frame[i].back[4],8);
   ATUS(ccfhead)("BACKRC  ",lochead+i*2,&frame[i].back[5],8);
   ATUS(ccfhead)("BACKC0  ",lochead+i*2,&frame[i].backx0,8);
   ATUS(ccfhead)("BACKR0  ",lochead+i*2,&frame[i].backy0,8);
   ATUS(ccfhead)("SKYRAT  ",lochead+i*2,&frame[i].skyrat,8);
   ATUS(ccfhead)("SKYSIG  ",lochead+i*2,&frame[i].skysig,8);
   ATUS(ccinhead)("SKIPGAL ",lochead+i*2,&frame[i].skipgal,8);
   ATUS(ccinhead)("SNPNCAL ",lochead+i*2,&j,8);
   if (j>0) {
     ATUS(ccfhead)("SNPSERR ",lochead+i*2,&frame[i].starerr,8);
     ATUS(ccfhead)("SNPEMAX ",lochead+i*2,&emax[i],8);
     ATUS(ccfhead)("SNPCRIT ",lochead+i*2,&criterr,8);
     ATUS(ccfhead)("STARCHI ",lochead+i*2,&frame[i].starchi,8);
   }
   frame[i].psf = (PSF *)malloc(sizeof(PSF));
   fpsf = frame[i].psf;
   fpsf->data = *(locpsf+i);
   fpsf->dx = *(psfdx+i);
   fpsf->fwhmpix = *(psffwhm+i);
   fpsf->nlib = (int)rint(1./fpsf->dx);
   fpsf->n = *(psfn+i);

   // Only use flag value of FLAG_CONVERGE from previous runs, redetermine others here
   //ATUS(ccinhead)("SNPFLAG ",lochead+i*2,&frame[i].flag,8);
   //frame[i].flag &= FLAG_CONVERGE;

   // Determine frame flags from previous solution here
   frame[i].flag = 0;
   if (j<0) {
     frame[i].flag |= FLAG_NOSTARS;
     frame[i].flag |= FLAG_BAD;
   }
   else if (j>0&&j<3)
     frame[i].flag |= FLAG_FEWSTARS;
   if (frame[i].starchi > *critstarchi ) frame[i].flag |= FLAG_STARCHI;
   if (frame[i].starchi > *(critstarchi+1) ) frame[i].flag |= FLAG_BAD;
   if (frame[i].starerr > *critstarerr ) frame[i].flag |= FLAG_STARCHI;
   if (frame[i].starerr > *(critstarerr+1) ) frame[i].flag |= FLAG_BAD;
   if (frame[i].skyrat > *critskyrat ) frame[i].flag |= FLAG_VARSKYALL;
   if (frame[i].zeropt < *critscale ) frame[i].flag |= FLAG_CLOUDY;
   if (frame[i].zeropt < *(critscale+1) ) frame[i].flag |= FLAG_BAD;
   scale = 3600.*sqrt(frame[i].cd1_1*frame[i].cd1_1+frame[i].cd1_2*frame[i].cd1_2);
   if (frame[i].psf->fwhmpix*scale > *critfwhm ) frame[i].flag |= FLAG_PSF;
   ifilt = frame[i].filt;
   sky[ifilt][nsky[ifilt]] = frame[i].back[0];
   skyrat[ifilt][nsky[ifilt]] = frame[i].skyrat;
   nsky[ifilt]++;

  }

  // Calculate median sky values
  for (ifilt=0;ifilt<MAXFILT;ifilt++) {
    if (nsky[ifilt]>0) {
      ATUS(median)(&sky[ifilt][0],&nsky[ifilt],&medsky[ifilt],&perc);
      ATUS(median)(&skyrat[ifilt][0],&nsky[ifilt],&medskyrat[ifilt],&perc);
    }
    fprintf(stderr,"ifilt: %d  sky median: %f median sky rat: %f\n",ifilt,medsky[ifilt], medskyrat[ifilt]);
  }
  // Flag any frames that are required and count number of non-variable frames
  for (i=strlen(varfile)-1;i>=0;i--) if (varfile[i] == '.') l=i;
  strncpy(out,varfile,l);
  out[l] = 0;
  fp=fopen(strcat(out,".flag"),"w");
  for (i=0;i<nframe;i++) {
    ifilt = frame[i].filt;
    if (frame[i].back[0] > *critskymed * medsky[ifilt]) frame[i].flag |= FLAG_BRIGHTSKY;
    if (frame[i].skyrat > 2. * medskyrat[ifilt]) frame[i].flag |= FLAG_VARSKY;
    if (frame[i].skyrat > 10. * medskyrat[ifilt]) frame[i].flag |= FLAG_BAD;
    // Skip galaxy fitting for all objects where flag matches skipgal mask
    if (frame[i].flag & *skipgal) frame[i].skipgal = 1;

    scale = 3600.*sqrt(frame[i].cd1_1*frame[i].cd1_1+frame[i].cd1_2*frame[i].cd1_2);
    fprintf(stderr,"%d %d %f %f %f %f %f %f %f %f %f %d %d %d %d %f %f %f %f %f %f %f %f\n", 
      frame[i].skipgal, frame[i].flag,
      frame[i].jd, frame[i].crval1, frame[i].crval2, frame[i].cd1_1, frame[i].cd1_2,
      frame[i].cd2_1, frame[i].cd2_2, frame[i].crpix1, frame[i].crpix2,
      frame[i].cnpix1, frame[i].cnpix2, frame[i].tele, frame[i].filt, frame[i].zeropt,
      frame[i].back[0], frame[i].skyrat, frame[i].psf->fwhmpix*scale, frame[i].starchi, frame[i].starerr,
      medsky[ifilt], medskyrat[ifilt]);
    fprintf(fp,"%d %d %f %f %f %f %f %f %f %f %f %d %d %d %d %f %f %f %f %f %f %f %f\n", 
      frame[i].skipgal, frame[i].flag,
      frame[i].jd, frame[i].crval1, frame[i].crval2, frame[i].cd1_1, frame[i].cd1_2,
      frame[i].cd2_1, frame[i].cd2_2, frame[i].crpix1, frame[i].crpix2,
      frame[i].cnpix1, frame[i].cnpix2, frame[i].tele, frame[i].filt, frame[i].zeropt,
      frame[i].back[0], frame[i].skyrat, frame[i].psf->fwhmpix*scale, frame[i].starchi, frame[i].starerr,
      medsky[ifilt], medskyrat[ifilt]);

     if ((frame[i].jd < var[0].jdmin || frame[i].jd > var[0].jdmax) && 
         frame[i].skipgal == 0) 
       novar[frame[i].filt]++;
     else
       yesvar[frame[i].filt]++;
  }
  fclose(fp);

  // Load up strip information
  oldstrip = 0;
  nstrip = 0;
  for (iframe=0; iframe<nframe; iframe++) {
    if (frame[iframe].strip>0) {
      new = TRUE;
      for (jframe=0;jframe<iframe;jframe++) {
        if (frame[iframe].strip == frame[jframe].strip) {
          new = FALSE;
          frame[iframe].nstrip = frame[jframe].nstrip;
        }
      } 
      if (new) {
        frame[iframe].nstrip = nstrip;
        nstrip++;
      }
    } else {
      frame[iframe].nstrip = 0;
    }
  }
  for (istrip=0; istrip < nstrip; istrip++) {
    strip[istrip].dra = 0;
    strip[istrip].ddec = 0;
    strip[istrip].pmra = 0;
    strip[istrip].pmdec = 0;
    for (ifilt=0; ifilt<MAXFILT; ifilt++) strip[istrip].scale[ifilt] = 1.;
  }

  if (*maxvar>=0) nvar=*maxvar;

  // If we have a variable and are fitting for underlying galaxy, 
  //   make sure we have epochs without the variable. If not, remove
  //   frames in that filter. If no good filters quit
  if (nvar>0&&ngal>0) {
    good = 0;
    for (i=0;i<MAXFILT;i++) {
      if (novar[i]>0) good = 1; 
      if (yesvar[i]>0 && novar[i] == 0) {
        fprintf(stderr,"No epochs without SN are available for filter %d %d %d!!\n",i,novar[i],yesvar[i]);
        for (iframe=0;iframe<nframe;iframe++) {
           if (frame[iframe].filt==i) frame[iframe].zeropt=0.;
        }
      }
    }
    if (!good) {
      fprintf(stderr,"No epochs without SN are available for any filters!!\n");       
      return;
    }
  }

  // Integrate up the PSF at the desired galaxy spacing for galaxy model
  fprintf(stderr,"PSF...\n");
  for (i=0;i<nframe;i++) {
    fpsf = frame[i].psf;
    scale = sqrt(frame[i].cd1_1*frame[i].cd1_1+frame[i].cd1_2*frame[i].cd1_2);
    smooth = (int)rint(fpsf->nlib*gal[0].dx/scale);
    smooth = (smooth%2==1) ? smooth : smooth+1;
smooth=0;
    psfint(&frame[i],smooth);
  }

  // Load up stars information
  fprintf(stderr,"Loading stars...\n");
  nstar=0;
  if (starfile[0] != 0) {
    fp = fopen(starfile,"r");
    if (fp==NULL) {
      fprintf(stderr,"Error opening file: %s\n",starfile);
    }
    while (fgets(line,MAXLINE,fp) != NULL && nstar<MAXSTAR ) {
      sscanf(line,"%d%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf",
         &star[nstar].id,&star[nstar].ra,&star[nstar].dec,
         &star[nstar].pmra, &star[nstar].pmdec,
         &star[nstar].mag[0],&star[nstar].err[0],
         &star[nstar].mag[1],&star[nstar].err[1],
         &star[nstar].mag[2],&star[nstar].err[2],
         &star[nstar].mag[3],&star[nstar].err[3],
         &star[nstar].mag[4],&star[nstar].err[4]);
      if (fabs(var[0].ra)<90&&star[nstar].ra>180) star[nstar].ra -= 360;
      ddec=star[nstar].dec-var[0].dec;
      if (fabs(ddec) < *dmax) {
        dra=(star[nstar].ra-var[0].ra)*cos(var[0].dec*D2R);
        if (fabs(dra) < *dmax) {
          if ((dra*dra+ddec*ddec)<((*dmax)*(*dmax))) {
            //fprintf(stderr,"nstar: %d %f %f %f %f %f\n",nstar, dra, ddec, star[nstar].pmra, star[nstar].pmdec,star[nstar].mag[2]);
            nstar++;
          }
        }
      }
    }
    fclose(fp);
  }

  if (galfile[0] != 0) {
    ngal=0;
    fp = fopen(galfile,"r");
    if (fp==NULL) {
      fprintf(stderr,"Error opening file: %s\n",galfile);
    }
    while (fgets(line,MAXLINE,fp) != NULL && ngal<MAXGAL ) {
      sscanf(line,"%lf%lf%d%lf",&gal[ngal].ra,&gal[ngal].dec,&gal[ngal].nsize,&gal[ngal].dx);
      if (fabs(var[0].ra)<90&&gal[ngal].ra>180) gal[ngal].ra -= 360;
      for (igx=0;igx<gal[ngal].nsize;igx++) {
        for (igy=0;igy<gal[ngal].nsize;igy++) {
          fgets(line,MAXLINE,fp);
          sscanf(line,"%lf%lf%lf%lf%lf",  &gal[ngal].inten[igx][igy][0],
           &gal[ngal].inten[igx][igy][1], &gal[ngal].inten[igx][igy][2],
           &gal[ngal].inten[igx][igy][3], &gal[ngal].inten[igx][igy][4]);
        }
      }
      ngal++;
    }
    fclose(fp);
  }

  oldpar = 0;
  if (parfile[0] != 0) {
    ifile = open(parfile,O_RDONLY);
    if (ifile>0) {
      read(ifile,&par,MAXPAR*sizeof(PAR));
      close(ifile);
      oldpar = 1;
  for (i=0;i<100;i++) fprintf(stderr,"%d %f %f %f %f %d\n",i,par[i].val,par[i].oldval,par[i].err,par[i].del,par[i].converge);
    } else
     fprintf(stderr,"Error opening parfile: %s\n",parfile);
  }

  // Do the fit!!
  niter = snphot(par,oldpar,frame,nframe,star,nstar,*maxstar,gal,ngal,var,nvar,*maxiter,*fitsky,*sub,*lock,&chi2final);

  if (*sub>0) return;

  for (i=strlen(varfile)-1;i>=0;i--) if (varfile[i] == '.') l=i;
  strncpy(out,varfile,l);
  out[l] = 0;

  // Output stars
  fp=fopen(strcat(out,".stars"),"w");
  for (istar=0;istar<nstar;istar++) {
    if (star[istar].used) {
      fprintf(fp,"%6d %12.6f %12.6f %12.6f %12.6f",star[istar].id,
              star[istar].ra,star[istar].dec,star[istar].pmra,star[istar].pmdec);
      for (ifilt=0;ifilt<MAXFILT;ifilt++) {
        if (!(*lock&LOCKSTARINTEN))
          star[istar].err[ifilt] = asinhmagerr(star[istar].inten[ifilt],star[istar].intenerr[ifilt],ifilt);
        fprintf(fp," %7.3f %7.3f %7.3f %7.3f",
              asinhmag(star[istar].inten[ifilt],star[istar].intenerr[ifilt],ifilt),
              star[istar].err[ifilt],
              star[istar].chi[ifilt],star[istar].sumwt[ifilt]);
      }
      fprintf(fp,"\n");
    }
  }
  fclose(fp);

  // Output galaxy
  strncpy(out,varfile,l);
  out[l] = 0;
  fp=fopen(strcat(out,".gal"),"w");
  fpp=fopen(strcat(out,".galerr"),"w");
  for (igal=0;igal<ngal;igal++) {
    sig2=0.5*0.5/(gal[0].dx*gal[0].dx*3600*3600);
    for (igx=0;igx<gal[igal].nsize;igx++) {
      for (igy=0;igy<gal[igal].nsize;igy++) {
        for (ifilt=0;ifilt<MAXFILT;ifilt++) {
          gal[igal].intgal[igx][igy][ifilt] = 0.;
            for (jgx=0;jgx<gal[igal].nsize;jgx++) {
              for (jgy=0;jgy<gal[igal].nsize;jgy++) {
                dist2=(jgx-igx)*(jgx-igx)+(jgy-igy)*(jgy-igy); 
                p = 1/(2*pi*sig2)*exp(-dist2/(2.*sig2));
                gal[igal].intgal[igx][igy][ifilt] += p*gal[igal].inten[jgx][jgy][ifilt];
              }
            }
        }
      }
    }
    fprintf(fp,"%12.6f %12.6f %6d %12.3e\n",gal[igal].ra,gal[igal].dec,gal[igal].nsize,gal[igal].dx);
    for (igx=0;igx<gal[igal].nsize;igx++) {
      for (igy=0;igy<gal[igal].nsize;igy++) {
        for (ifilt=0;ifilt<MAXFILT;ifilt++) {
          fprintf(fp," %12.3e",gal[igal].inten[igx][igy][ifilt]);
          fprintf(fp," %12.3e",gal[igal].intgal[igx][igy][ifilt]);
          fprintf(fpp," %12.3e",gal[igal].intenerr[igx][igy][ifilt]);
          fprintf(fpp," %12.3e",sqrt(gal[igal].interr[igx][igy][ifilt]));
        }
        fprintf(fp,"\n");
        fprintf(fpp,"\n");
      }
    }
  }
  fclose(fp);
  fclose(fpp);

  // Output frame info
  strncpy(out,varfile,l);
  out[l] = 0;
  fp=fopen(strcat(out,".frame"),"w");
  fprintf(fp,"# MJD     FILT TELE   CAMCOL FIELD FWHM   NITER ZEROPT   ZERR    CD1_1    CD1_2    CD2_1    CD2_2    CRPIX1   CRPIX2    SKY   SKYCOUNTS FRAMECHI   NPIX    NSTAR   TRANS TRANSERR  Z_M     Z_RMS   APCOR     Z_N Z_M     Z_RMS   APCOR     Z_N Z_M     Z_RMS   APCOR     Z_N Z_M     Z_RMS   APCOR     Z_N Z_M     Z_RMS   APCOR     Z_N  STARCHI  CHI2FINAL\n");
  for (iframe=0;iframe<nframe;iframe++) {
    itele=frame[iframe].tele>1000 ? 0 : frame[iframe].tele;
    ifilt = frame[iframe].filt;
    framechi = (frame[iframe].npix>0?frame[iframe].chi2/frame[iframe].npix:0);

    ival = 1;
    ATUS(ccinheadset)("SNPHOT  ",&ival,lochead+iframe*2,8);
    ATUS(ccfheadset)("SNPSCALE",&frame[iframe].zeropt,lochead+iframe*2,8);
    ATUS(ccfheadset)("SNPAPCOR",&frame[iframe].apcor[1],lochead+iframe*2,8);
    ATUS(ccfheadset)("CD1_1   ",&frame[iframe].cd1_1,lochead+iframe*2,8);
    ATUS(ccfheadset)("CD1_2   ",&frame[iframe].cd1_2,lochead+iframe*2,8);
    ATUS(ccfheadset)("CD2_1   ",&frame[iframe].cd2_1,lochead+iframe*2,8);
    ATUS(ccfheadset)("CD2_2   ",&frame[iframe].cd2_2,lochead+iframe*2,8);
    ATUS(ccfheadset)("CRPIX1  ",&frame[iframe].crpix1,lochead+iframe*2,8);
    ATUS(ccfheadset)("CRPIX2  ",&frame[iframe].crpix2,lochead+iframe*2,8);
    if (nstar>0) {
      for (i=0;i<5;i++) {
        sprintf(card,"MEAN%1d   ",i+1);
        ATUS(ccfheadset)(card,&frame[iframe].mean[i],lochead+iframe*2,8);
        sprintf(card,"SIG%1d    ",i+1);
        ATUS(ccfheadset)(card,&frame[iframe].sig[i],lochead+iframe*2,8);
        sprintf(card,"CHI2%1d   ",i+1);
        ATUS(ccfheadset)(card,&frame[iframe].starchi2[i],lochead+iframe*2,8);
      }
      j=-1;
      for (i=4;i>=0;i--) if (frame[iframe].nsig[i] >= 5) j = i+1;
      if (j == -1) {
        for (i=4;i>=0;i--) if (frame[iframe].nsig[i] >= 3) j = i+1;
      }
      if (j==-1) {
        frame[iframe].starerr = 0.;
        emax[iframe] = 0.;
        criterr = 0.;
        i = -1;
       ATUS(ccinheadset)("SNPNCAL ",&i,lochead+iframe*2,8);
      } else {
        emax[iframe] = ((j-1)<4 ? 0.01*(j-1) : 0.02*(j-1) );
        ATUS(ccfheadset)("SNPEMAX ",&emax[iframe],lochead+iframe*2,8);
        ATUS(ccinheadset)("SNPNCAL ",&frame[iframe].nsig[j-1],lochead+iframe*2,8);
      }
      ATUS(ccfheadset)("SNPSERR ",&frame[iframe].starerr,lochead+iframe*2,8);
      ATUS(ccfheadset)("STARCHI ",&frame[iframe].starchi,lochead+iframe*2,8);
    }
    if (niter>*maxiter || niter<0 || framechi>10 || framechi < 0.5 ) {
      frame[iframe].flag |= FLAG_CONVERGE;
      if (framechi>50 || framechi < 0.2) frame[iframe].flag |= FLAG_BAD;
      if ((niter>*maxiter || niter<0)  && chi2final > 1.2) frame[iframe].flag |= FLAG_BAD;
    }
    ATUS(ccinheadset)("SNPFLAG ",&frame[iframe].flag,lochead+iframe*2,8);

    f0=teles[itele].f0[ifilt]*frame[iframe].exp*pow(10,-0.4*(frame[iframe].am*teles[itele].ext[ifilt]))/frame[iframe].gain;
    scale = 3600.*sqrt(frame[iframe].cd1_1*frame[iframe].cd1_1+frame[iframe].cd1_2*frame[iframe].cd1_2);
    if (frame[iframe].zeropt > 0) {
      cback = asinhmag(frame[iframe].back[0]/frame[iframe].zeropt/f0/scale/scale,1.,frame[iframe].filt);
    } else
      cback = 99.999;
    fprintf(fp,"%12.6f %1d %6d %3d %8d %8.2f %5d %8.3f %8.3f %12.3e %12.3e %12.3e %12.3e %8.2f %8.2f %8.2f %8.2f %8.2f %8d %4d %8.3f %8.3f",
        frame[iframe].jd,frame[iframe].filt,frame[iframe].tele,
        frame[iframe].camcol, frame[iframe].field,
        frame[iframe].psf->fwhmpix*scale, niter,
        frame[iframe].zeropt,frame[iframe].zeropterr,
        frame[iframe].cd1_1,frame[iframe].cd1_2,frame[iframe].cd2_1,frame[iframe].cd2_2,
        frame[iframe].crpix1+frame[iframe].cnpix1,frame[iframe].crpix2+frame[iframe].cnpix2,
        cback, frame[iframe].skyrat,
        framechi, frame[iframe].npix,
        frame[iframe].nstar,teles[itele].trans[ifilt],teles[itele].transerr[ifilt]);
    for (i=0;i<5;i++)  {
      fprintf(fp," %8.3f %8.3f %8.3f %3d",frame[iframe].mean[i],frame[iframe].sig[i],
               frame[iframe].apcor[i],frame[iframe].nsig[i]);
    }
    fprintf(fp," %8.3f %8.3f %5d", frame[iframe].starchi,chi2final,frame[iframe].flag);
    fprintf(fp,"\n");

  }
  fclose(fp);

  // Strip output
  strncpy(out,varfile,l);
  out[l] = 0;
  fp=fopen(strcat(out,".strip"),"w");
  for (istrip=0; istrip<nstrip; istrip++) {
    fprintf(fp," %12.6e %12.6e %12.6e %12.6e %12.6e %12.6e %12.6e %12.6e", 
          strip[istrip].dra, strip[istrip].ddec,
          strip[istrip].pmra, strip[istrip].pmdec,
          strip[istrip].draerr, strip[istrip].ddecerr,
          strip[istrip].pmraerr, strip[istrip].pmdecerr);
    for (ifilt=0;ifilt<MAXFILT;ifilt++)
     fprintf(fp, "%8.3f", strip[istrip].scale[ifilt]);
  fprintf(fp,"\n");
  }
  fclose(fp);

  // Output variable(s)
  for (ivar=0;ivar<nvar;ivar++) {
    strncpy(out,varfile,l);
    out[l] = 0;
    if (ivar==0) {
      fp=fopen(strcat(out,".phot"),"w");
      fsum=fopen(strcat(out,".sum"),"w");
      fprintf(fsum,"# %6d %12.6f %12.6f\n",var[ivar].id,var[ivar].ra,var[ivar].dec);
      fprintf(fsum,"#    FLAG MJD     FILT MAG       ERR     RUN   N(NOVAR)\n");
      fprintf(fp,"# %6d %12.6f %12.6f\n",var[ivar].id,var[ivar].ra,var[ivar].dec);
      fprintf(fp,"#    MJD     FILT TELE    MAG       ERR     CHI    SUMWT     COUNTS  COUNTSERR    APMAG  FRAMECHI    CHI    N(NOVAR)\n");
//      fprintf(fp,"#    MJD       FILT TELE    MAG      ERR        CHI   COUNTS  COUNTSERR      SCALE SCALEERR     SKY   SKYCOUNTS  FRAMECHI  STARSIG  NSTAR  APMAG  FWHM CAMCOL FIELD CHI NOVAR\n");
      strncpy(out,varfile,l);
      out[l] = 0;
      fpp=fopen(strcat(out,".var"),"w");
    }
    for (iframe=0;iframe<nframe;iframe++) {
      fprintf(fpp," %5d %12.6f %12.6f",var[ivar].id,var[ivar].ra,var[ivar].dec);
      ifilt = frame[iframe].filt;
      itele = frame[iframe].tele > 1000 ? 0 : frame[iframe].tele;
      f0=teles[itele].f0[ifilt]*frame[iframe].exp*pow(10,-0.4*(frame[iframe].am*teles[itele].ext[ifilt]))/frame[iframe].gain;
      scale = 3600.*sqrt(frame[iframe].cd1_1*frame[iframe].cd1_1+frame[iframe].cd1_2*frame[iframe].cd1_2);

    // Get galaxy brightness estimate for this frame
      getsncoord(frame[iframe],&var[ivar]);
      gtot = 0;
      gerr = 0;
      for (igal=0;igal<1;igal++) {
       for (igx=0;igx<gal[igal].nsize;igx++) {
        gra=gal[igal].ra+(igx-gal[igal].nsize/2)*
            gal[igal].dx/frame[iframe].cosdec;
        for (igy=0;igy<gal[igal].nsize;igy++) {
           gdec=gal[igal].dec+(igy-gal[igal].nsize/2)*gal[igal].dx;
           gtot += p*gal[igal].inten[igx][igy][ifilt];
           getgcoord(frame[iframe],&gal[igal],gra,gdec);
           p = psf(frame[iframe].psf,gal[igal].x-var[ivar].x,gal[igal].y-var[ivar].y,&dpdx,&dpdy);
           gerr += pow(p*gal[igal].intenerr[igx][igy][ifilt],2);
 if (igx==gal[igal].nsize/2 && igx==gal[igal].nsize/2) gerr0=sqrt(gal[igal].interr[igx][igy][ifilt]);
        }
       }
       gerr = sqrt(gerr)/4.;
      }

      itele=frame[iframe].tele>1000 ? 0 : frame[iframe].tele;
      pixscale = sqrt(frame[iframe].cd1_1*frame[iframe].cd1_1+
                      frame[iframe].cd1_2*frame[iframe].cd1_2);
      fwhm = frame[iframe].psf->fwhmpix * pixscale;
      psfrad = 3*fwhm > teles[itele].psfrad ? 3*fwhm : teles[itele].psfrad;
      neff = 0;
      for (ix=-psfrad/pixscale; ix<psfrad/pixscale; ix++) {
        for (iy=-psfrad/pixscale; iy<psfrad/pixscale; iy++) {
           p = psf(frame[iframe].psf,(double)ix,(double)iy,&dpdx,&dpdy);
           neff += p*p;
        }
      }
      neff=1/neff;

      if (frame[iframe].zeropt > 0) {
        cback = asinhmag(frame[iframe].back[0]/frame[iframe].zeropt/f0/scale/scale,1.,frame[iframe].filt);
        serr = neff*frame[iframe].skysig/frame[iframe].zeropt/f0;
      } else {
        cback = 99.999;
        serr = -1.;
      }
      framechi = (frame[iframe].npix>0?frame[iframe].chi2/frame[iframe].npix:0);
      if (var[ivar].sumwt[iframe] <=3) {
        var[ivar].inten[iframe] = 0;
        var[ivar].intenerr[iframe] = -1.;
      }
      merr = asinhmagerr(var[ivar].inten[iframe],var[ivar].intenerr[iframe],frame[iframe].filt);
      if (merr > 0) merr = sqrt(merr*merr+frame[iframe].starerr*frame[iframe].starerr);
      merrg=0;
      if (ivar==0) {
          if (merr > 50.5 ) {
            frame[iframe].flag |= FLAG_CONVERGE;
            frame[iframe].flag |= FLAG_BAD;
          }
          p = psf(frame[iframe].psf,0.,0.,&dpdx,&dpdy);
          if (var[ivar].inten[iframe]>0&&
              asinhmag(p*var[ivar].inten[iframe],0.,frame[iframe].filt) >
              asinhmag(gtot,0.,frame[iframe].filt))
            frame[iframe].flag |= FLAG_BRIGHTGAL;
          //merrg = asinhmagerr(p*var[ivar].inten[iframe],gerr,frame[iframe].filt);
          merrg = asinhmagerr(var[ivar].inten[iframe],gerr0,frame[iframe].filt);
          merrs = asinhmagerr(var[ivar].inten[iframe],serr,frame[iframe].filt);

          fprintf(fp," %12.6f %1d %6d %8.3f %8.3f %8.3f %8.3f %8.3f %8.2f %8.2f %10.2f %10.2f %8.3f %8.3f %8.3f %5d %8.2f %8.2f\n",
              frame[iframe].jd,frame[iframe].filt,frame[iframe].tele,
              asinhmag(var[ivar].inten[iframe],var[ivar].intenerr[iframe],frame[iframe].filt),
              asinhmagerr(var[ivar].inten[iframe],var[ivar].intenerr[iframe],frame[iframe].filt),
              merr,merrg,merrs,
              var[ivar].chi[iframe],var[ivar].sumwt[iframe],
              var[ivar].inten[iframe]*frame[iframe].zeropt*f0,
              (var[ivar].intenerr[iframe]>0 ? var[ivar].intenerr[iframe]*frame[iframe].zeropt*f0 : 0.),
              asinhmag(var[ivar].apinten[iframe],1.,frame[iframe].filt),
              framechi, frame[iframe].chi, novar[ifilt], var[ivar].x+frame[iframe].cnpix1, var[ivar].y+frame[iframe].cnpix2
              );
          fprintf(fsum," %5d %12.6f %1d %8.3f %8.3f %8.3f %8.3f %6d %8.3f %12.3e %12.3e %12.3e %12.3e %8.2f\n",
              frame[iframe].flag, frame[iframe].jd, frame[iframe].filt,
              asinhmag(var[ivar].inten[iframe],var[ivar].intenerr[iframe],frame[iframe].filt), 
              merr, merrs, merrg, frame[iframe].tele, asinhmag(gtot,0.,frame[iframe].filt),var[ivar].intenerr[iframe],serr,gerr,gerr0,neff);
      }
      fprintf(fpp," %5d %12.6f %1d %6d %8.3f %8.3f %8.3f %8.3f %8.1e %8.3f %8.1e %8.2f %8.2f %10.2f %10.2f %8.3f %8.3f %8.3f %5d\n",
              frame[iframe].flag, frame[iframe].jd,frame[iframe].filt,frame[iframe].tele,
              asinhmag(var[ivar].inten[iframe],var[ivar].intenerr[iframe],frame[iframe].filt),
              asinhmagerr(var[ivar].inten[iframe],var[ivar].intenerr[iframe],frame[iframe].filt),
              merr,merrs,serr,merrg,gerr0,
              var[ivar].chi[iframe],var[ivar].sumwt[iframe],
              var[ivar].inten[iframe]*frame[iframe].zeropt*f0,
              (var[ivar].intenerr[iframe]>0 ? var[ivar].intenerr[iframe]*frame[iframe].zeropt*f0 : 0.),
              asinhmag(var[ivar].apinten[iframe],1.,frame[iframe].filt),
              framechi, frame[iframe].chi, novar[ifilt]
              );
    }
    if (ivar==0) {
      fclose(fp);
      fclose(fsum);
    }
  }
  if (nvar>0) fclose(fpp);

  strncpy(out,varfile,l);
  out[l] = 0;
  ifile = creat(strcat(out,".par"),0644);
  write(ifile,&par,MAXPAR*sizeof(PAR));
  close(ifile);

  return;
}

int snphot(PAR par[], int oldpar, FRAME frame[], int nframe, 
            STAR star[], int nstar, int maxstar,
            GAL gal[], int ngal, 
            VAR var[], int nvar, 
            int maxiter, int fitsky, int sub, int lock, double *chi2final)
{
  int npar, redo=1, used, objpix, galpix;
  int niter=0, ncombo, ipar, jpar, nfit, nskipped;
  int iframe, jframe, ix, iy, istar, jstar, kstar, igal, ivar;
  int i0, ig0, n2, dummy, istar1; 
  int ifilt, jfilt, kfilt, lfilt, filts[MAXFILT], nfilt;
  int iback, itele, istat, converge=0;
  float **alpha,**alphared,**alpha0,*beta0,*err,*betared,*betatmp,det;
  int *indx, tfit, itfilt, tfilt, tframe, icombo, ndist, cframe, subfinal;
  float beta[MAXPAR], dpar[MAXPAR], spar[MAXPAR];
  float data;
  double mod, modvar, modstar, modgal, modsky;
//  double pkerr=0.0002;
  double pkerr=0.001;
  double p, p1, p2, tot, fitrad, psfrad, pixscale, f0, dpdx, dpdy;
  double diff, pwt, wt, sig2, x, y, tmp;
  double dist2, rsq, dra, ddec, fwhm, erad, fitrad2, psfrad2, relerr;
  double trans, chi2, chi2old, chicrit, lambda, lastlambda, chimax, definten;
  double dxdd, dxdr, dydd, dydr, dx[4], dy[4];
  double cd1_1, cd1_2, cd2_1, cd2_2, den, crval1, crval2;
  double ra, dec, pra, pdec, gra, gdec, rng, gain;
  double m1, m2, m3, m4, e2, emax, sum, sum2, sumap;
  double wsum, wsum2, w, wmax, wrawmax;
  int crpix1, crpix2, nsum, wnsum, ngood, istrip, nin, nx, ny;
  int i, j, ipix, npix, igx, igy, skip[MAXSTAR], skipvar[MAXFRAME];
  int clip=FALSE, weightexpo=0;
  int jgx,jgy,kgx,kgy;
  double pi=3.14159;
  FILE *fp;

  extern int con_[5];

  if (sub<0) {
    subfinal = abs(sub);
    sub = 0;
  } else {
    subfinal = 15;
  }
 
  fprintf(stderr,"SNPHOT nframe: %d nvar: %d  ngal: %d nstar: %d\n",nframe,nvar,ngal,nstar);

  /* initialize frame zeropt based on telescope information */
  for (iframe=0; iframe<nframe; iframe++) {
    ifilt=frame[iframe].filt;
    frame[iframe].cosdec = cos(frame[iframe].crval2*D2R);
    // Take initial guess zeropt from image headers if available
    // frame[iframe].zeropt = 1.;
    if (frame[iframe].skipgal)
      frame[iframe].first = 0;
    else {
      frame[iframe].first = 1;
      for (jframe=0;jframe<iframe;jframe++)
        if (frame[jframe].filt == frame[iframe].filt && frame[jframe].first==1) 
            frame[iframe].first = 0;
    }
    for (i=0;i<5;i++) {
      frame[iframe].mean[i] = 0;
      frame[iframe].sig[i] = 0;
      frame[iframe].nsig[i] = 0;
      frame[iframe].starchi2[i] = 0;
    }
    frame[iframe].starchi = 0;
  }

  fprintf(stderr,"initialize star brightnesses, %d\n",nstar);
  /* initialize star brightnesses based on input magnitudes */
  for (istar=0; istar<nstar; istar++) {
    for (ifilt=0; ifilt<MAXFILT; ifilt++) {
      iframe=0;
      itele=frame[iframe].tele>1000 ? 0 : frame[iframe].tele;
      f0=teles[itele].f0[ifilt]*frame[iframe].exp*pow(10,-0.4*(frame[iframe].am*teles[itele].ext[ifilt]))/frame[iframe].gain;
      star[istar].used = 0;
      if (star[istar].mag[ifilt] < 30 && star[istar].mag[ifilt] > 5 ) {
        star[istar].inten[ifilt] = asinhinv(star[istar].mag[ifilt],ifilt);
        star[istar].intenerr[ifilt] = 0.;
      } else {
        if (lock&LOCKSTARINTEN) {
          star[istar].inten[ifilt] = -1.;
          star[istar].intenerr[ifilt] = -1;
        } else {
          star[istar].inten[ifilt] = 1./f0;
          star[istar].intenerr[ifilt] = 0.;
        }
      }
    }
    if (lock&FITSTARPM) {
//   If we are doing proper motion, only take stars on most frames
      nin = 0;
      for (iframe=0;iframe<nframe;iframe++) {
        getscoord(frame[iframe],&star[istar]);
        pixscale = sqrt(frame[iframe].cd1_1*frame[iframe].cd1_1+
                        frame[iframe].cd1_2*frame[iframe].cd1_2);
        fwhm = frame[iframe].psf->fwhmpix * pixscale;
        itele=frame[iframe].tele>1000 ? 0 : frame[iframe].tele;
        psfrad = 3*fwhm > teles[itele].psfrad ? 3*fwhm : teles[itele].psfrad;
        erad = fwhm > teles[itele].fitrad ? fwhm : teles[itele].fitrad;
        star[istar].ixmin = (int)floor(star[istar].x-psfrad/pixscale-1);
        star[istar].ixmax = (int)ceil(star[istar].x+psfrad/pixscale+1);
        star[istar].iymin = (int)floor(star[istar].y-psfrad/pixscale-1);
        star[istar].iymax = (int)ceil(star[istar].y+psfrad/pixscale+1);
        if (star[istar].ixmin > 1 && star[istar].ixmax < frame[iframe].nx &&
            star[istar].iymin > 1 && star[istar].iymax < frame[iframe].ny) nin++;
      }
      if (nin<nframe/2) star[istar].id = -1 * abs(star[istar].id);
    }
  }

  fprintf(stderr,"initialize galaxy brightnesses\n");
  /* initialize galaxy brightness */
  if (!(lock&LOCKGAL)) {
   for (igal=0; igal<ngal; igal++) 
     for (igx=0;igx<gal[igal].nsize;igx++) 
       for (igy=0;igy<gal[igal].nsize;igy++) 
         for (ifilt=0;ifilt<MAXFILT;ifilt++) {
            gal[igal].inten[igx][igy][ifilt] = glog(0.);
            gal[igal].intenerr[igx][igy][ifilt] = 0.;
            gal[igal].interr[igx][igy][ifilt] = 0.;
         }
   for (iframe=nframe-1; iframe>=0; iframe--) {
    if ((frame[iframe].jd<var[0].jdmin||frame[iframe].jd>var[0].jdmax) && frame[iframe].zeropt>0) {
      ifilt=frame[iframe].filt;
      itele=frame[iframe].tele>1000 ? 0 : frame[iframe].tele;
      f0=teles[itele].f0[ifilt]*frame[iframe].exp*pow(10,-0.4*(frame[iframe].am*teles[itele].ext[ifilt]))/frame[iframe].gain;
      pixscale = sqrt(frame[iframe].cd1_1*frame[iframe].cd1_1+
                      frame[iframe].cd1_2*frame[iframe].cd1_2);
      for (igal=0; igal<ngal; igal++) {
        for (igx=0;igx<gal[igal].nsize;igx++) {
          gra=gal[igal].ra+(igx-gal[igal].nsize/2)*
                          gal[igal].dx/frame[iframe].cosdec;
          for (igy=0;igy<gal[igal].nsize;igy++) {
            gdec=gal[igal].dec+(igy-gal[igal].nsize/2)*gal[igal].dx;
            getgcoord(frame[iframe],&gal[igal],gra,gdec);
            ix = (int)rint(gal[igal].x);
            ix = ix<1 ? 1 : ix;
            ix = ix>frame[iframe].nx ? frame[iframe].nx : ix;
            iy = (int)rint(gal[igal].y);
            iy = iy<1 ? 1 : iy;
            iy = iy>frame[iframe].ny ? frame[iframe].ny : iy;
            data =
             (*(frame[iframe].data+iy*frame[iframe].nx+ix)-frame[iframe].back[0]);
            data = (data>0) ? data : 1;
            gal[igal].inten[igx][igy][ifilt] =
               data/frame[iframe].zeropt/f0*
               gal[igal].dx*gal[igal].dx/(pixscale*pixscale);
            gal[igal].inten[igx][igy][ifilt] =  
                   (gal[igal].inten[igx][igy][ifilt] > 0) ? 
                   glog(gal[igal].inten[igx][igy][ifilt]) : glog(0.) ; 
          }
        }
      }
    }
   }
  }
  fprintf(stderr,"initialize variable brightnesses\n");
  // Initialize variable brightness
  for (iframe=nframe-1; iframe>=0; iframe--) {
    ifilt=frame[iframe].filt;
    itele = frame[iframe].tele > 1000 ? 0 : frame[iframe].tele;
    f0=teles[itele].f0[ifilt]*frame[iframe].exp*pow(10,-0.4*(frame[iframe].am*teles[itele].ext[ifilt]))/frame[iframe].gain;
    for (ivar=0; ivar<nvar; ivar++) {
      getsncoord(frame[iframe],&var[ivar]);
      ix = (int)rint(var[ivar].x);
      iy = (int)rint(var[ivar].y);
      if (ix>1&&iy>1&&ix<frame[iframe].nx&&iy<frame[iframe].ny) {
        data = (*(frame[iframe].data+iy*frame[iframe].nx+ix)-frame[iframe].back[0]);
        if (ivar==0&&ngal>0) 
          data -= gexp(gal[0].inten[gal[0].nsize/2][gal[0].nsize/2][ifilt])*frame[iframe].zeropt*f0;
      } else
          data = 0.;
      p = psf(frame[iframe].psf,ix-var[ivar].x,iy-var[ivar].y,&dpdx,&dpdy);
      if (frame[iframe].jd >= var[ivar].jdmin&&frame[iframe].jd <= var[ivar].jdmax) {
        var[ivar].inten[iframe] =  data/p/frame[iframe].zeropt/f0;
        var[ivar].intenerr[iframe] =  0.;
      } else {
        var[ivar].inten[iframe] = 0.;
        var[ivar].intenerr[iframe] =  0.;
      }
      var[ivar].apinten[iframe] = 0.;
      if (frame[iframe].zeropt <= 0) {
        var[ivar].inten[iframe] = 0.;
        var[ivar].intenerr[iframe] = -1.;
      }
    }
  }

  /* If we have are starting from a previous solution, set variables
       from that solution here -- redoing the above initializations */
  if (oldpar) npar = setpar(0, par, &ncombo, &nfilt, filts,
                      frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
  /* Determine number of parameters and set par array */
  npar = setpar(1, par, &ncombo, &nfilt, filts,
                frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
  fprintf(stderr,
   "npar: %d nframe: %d nstar: %d ngal: %d nfilt: %d nvar: %d ncombo: %d\n",
    npar, nframe, nstar, ngal,nfilt,nvar, ncombo);

  /* Allocate matrix space */
  alpha=matrix(0,npar,0,npar);
  alphared=matrix(1,npar,1,npar);
  alpha0=matrix(1,npar,1,npar);
  err=vector(1,npar);
  betared=vector(1,npar);
  beta0=vector(1,npar);
  betatmp=vector(1,npar);
  indx=ivector(1,npar);
  lambda = 1.e-3;
  chi2old = 0.;

  /* Main iteration loop starts here */
  while (redo) {

    // Initialize least squares arrays
    for (ipar=0;ipar<npar;ipar++) {
      beta[ipar] = 0;
      if (converge) beta0[ipar+1] = 0;
      for (jpar=0;jpar<npar;jpar++) {
        alpha[ipar][jpar] = 0;
        if (converge) alpha0[ipar+1][jpar+1] = 0;
      }
    }

    // Initialize stellar fit quality estimators
    // If <10 valid pixels in all filters, flag as bad
    for (istar=0; istar<nstar; istar++) {
      if (niter>0) {
        ngood = 0;
        for (i=0;i<MAXFILT;i++) {
          if (star[istar].sumwt[i] >= 10) ngood=1;
        }
        if (ngood==0) {
          star[istar].id = -1 * abs(star[istar].id);
          for (i=0;i<MAXFILT;i++) star[istar].inten[i] = 0;
        }
      }
      for (i=0;i<MAXFILT;i++) {
        star[istar].chi[i] = 0.;
        star[istar].sumwt[i] = 0.;
      }
      // Remove closely separated stars
      for (jstar=0;jstar<istar;jstar++) {
        ddec = star[istar].dec-star[jstar].dec;
        if (fabs(ddec) < 0.5/3600.) {
          dra = (star[istar].ra-star[jstar].ra)*frame[0].cosdec;
          if (fabs(dra) < 0.5/3600.) {
            rsq = dra*dra+ddec*ddec;
            if (rsq < 1.92901e-8) {
              kstar = star[istar].inten[0] > star[jstar].inten[0] ? jstar : istar;
              star[kstar].id = -1 * abs(star[kstar].id);
              for (i=0;i<MAXFILT;i++) star[kstar].inten[i] = 0;
              i0=nvar*2+nvar*nframe+nframe*(7+NBACK)-1;
              i0+=kstar*(4+nfilt);
              for (i=0;i<nfilt;i++) {
                par[i0+5+i].val = 0;
                par[i0+5+i].del = 0;
                par[i0+5+i].err = 0;
              }
            }
          }
        }
      }
    }
    // Initialize variable fit quality estimators
    for (ivar=0; ivar<nvar; ivar++) {
      for (iframe=0; iframe<nframe; iframe++) {
        var[ivar].chi[iframe] = 0;
        var[ivar].sumwt[iframe] = 0;
      }
    }

    chi2 = 0;
    if (niter==1) chi2old = 0;
    npix = 0;
/* Loop over all frames, all pixels, and load up data-model and derivatives */
    for (iframe=0; iframe<nframe; iframe++) {
     frame[iframe].chi = 0;
     frame[iframe].chi2 = 0;
     frame[iframe].npix = 0;
     frame[iframe].nstar = 0;
     if (frame[iframe].zeropt > 0) {

      // Return if CTRL-C hit (in XVISTA installation)
      if (con_[2]) return(niter);

      // Initialize some shorthand variables for this frame
      //     jfilt is index in full 5 filter list, ifilt is
      //              index in list of filters under consideration
      jfilt = frame[iframe].filt;
      for (i=0;i<nfilt;i++) if (filts[i] == jfilt) ifilt=i;
      itele = frame[iframe].tele > 1000 ? 0 : frame[iframe].tele;
      cd1_1 = frame[iframe].cd1_1;
      cd1_2 = frame[iframe].cd1_2;
      cd2_1 = frame[iframe].cd2_1;
      cd2_2 = frame[iframe].cd2_2;
      den=cd1_1*cd2_2-cd1_2*cd2_1;
      crval1 = frame[iframe].crval1;
      crval2 = frame[iframe].crval2;
      crpix1 = frame[iframe].crpix1;
      crpix2 = frame[iframe].crpix2;
      istrip = frame[iframe].nstrip;
      nx = frame[iframe].nx;
      ny = frame[iframe].ny;
      frame[iframe].stripdra = strip[istrip].dra;
      frame[iframe].stripddec = strip[istrip].ddec;
      gain = frame[iframe].gain;
      rng = frame[iframe].rn*frame[iframe].rn/gain/gain;
      pixscale = sqrt(cd1_1*cd1_1+cd1_2*cd1_2);
      fwhm = frame[iframe].psf->fwhmpix * pixscale;
      psfrad = 3*fwhm > teles[itele].psfrad ? 3*fwhm : teles[itele].psfrad;
      psfrad2 = psfrad*psfrad;
      // First iteration use big fitting radius to make sure to find stars
      if (niter==0) {
        fitrad = psfrad;
        fitrad2 = psfrad2;
      } else {
        fitrad = fwhm > teles[itele].fitrad ? fwhm : teles[itele].fitrad;
        fitrad2 = fitrad*fitrad;
      }
      f0=teles[itele].f0[jfilt]*frame[iframe].exp*pow(10,-0.4*(frame[iframe].am*teles[itele].ext[jfilt]))/frame[iframe].gain;
      ndist = teles[itele].ndist;
      tfit = teles[itele].tfit[jfilt];
      trans = teles[itele].trans[jfilt];
      tfilt = teles[itele].tfilt[jfilt];
      itfilt = -1;
      for (i=0;i<nfilt;i++) if (filts[i] == tfilt) itfilt=i;
      icombo = frame[iframe].combo;
      tframe=-1;
      for (i=0;i<nframe;i++)
        if (fabs(frame[iframe].jd-frame[i].jd)<0.1 && frame[i].filt==jfilt) tframe = i;

     // get current positions of variables in this frame, and pixel limits for consideration
      for (ivar=0; ivar<nvar; ivar++) {
        getsncoord(frame[iframe],&var[ivar]);
        var[ivar].ixmin = (int)floor(var[ivar].x-psfrad/pixscale-1);
        var[ivar].ixmax = (int)ceil(var[ivar].x+psfrad/pixscale+1);
        var[ivar].iymin = (int)floor(var[ivar].y-psfrad/pixscale-1);
        var[ivar].iymax = (int)ceil(var[ivar].y+psfrad/pixscale+1);
 
        var[ivar].ixmin = (var[ivar].ixmin < 1) ? (int)floor(var[ivar].x-fitrad/pixscale-1) : var[ivar].ixmin;
        var[ivar].ixmax = (var[ivar].ixmax > nx) ? (int)ceil(var[ivar].x+fitrad/pixscale+1) : var[ivar].ixmax;
        var[ivar].iymin = (var[ivar].iymin < 1) ? (int)floor(var[ivar].y-fitrad/pixscale-1) : var[ivar].iymin;
        var[ivar].iymax = (var[ivar].iymax > nx) ? (int)ceil(var[ivar].y+fitrad/pixscale+1) : var[ivar].iymax;

        var[ivar].ixmin = var[ivar].ixmin < 1 ? nx+1 : var[ivar].ixmin;
        var[ivar].iymin = var[ivar].iymin < 1 ? ny+1 : var[ivar].iymin;
        var[ivar].ixmax = var[ivar].ixmax > nx ? 0 : var[ivar].ixmax;
        var[ivar].iymax = var[ivar].iymax > ny ? 0 : var[ivar].iymax;

        if (var[ivar].ixmin>var[ivar].ixmax || var[ivar].iymin>var[ivar].iymax) {
          var[ivar].inten[iframe] = 0.;
          var[ivar].intenerr[iframe] = -1.;
        }
/*  Block for color terms for variable is more complicated since need to find matching frame
        of transformation color. Not implemented yet.
        if (var[ivar].inten[tfilt]>0) {
          var[ivar].color = var[ivar].inten[jfilt]/var[ivar].inten[tfilt];
          if (var[ivar].color>0)
            var[ivar].pow = pow(var[ivar].color,trans);
          else {
            var[ivar].color = 0.;
            var[ivar].pow = 1.;
          }
        } else {
          var[ivar].color = 0.;
          var[ivar].pow = 1.;
        }
*/
      }
     // get current positions of stars in this frame, and pixel limits for consideration
      istar1 = -1;
      for (istar=0; istar<nstar; istar++) {
        // Set pixel limits using psfrad, but if that falls off edge of frame, still
        //  include stars if fitrad is within frame
        getscoord(frame[iframe],&star[istar]);
        star[istar].ixmin = (int)floor(star[istar].x-psfrad/pixscale-1);
        star[istar].ixmax = (int)ceil(star[istar].x+psfrad/pixscale+1);
        star[istar].iymin = (int)floor(star[istar].y-psfrad/pixscale-1);
        star[istar].iymax = (int)ceil(star[istar].y+psfrad/pixscale+1);
 
        star[istar].ixmin = (star[istar].ixmin < 1) ? (int)floor(star[istar].x-fitrad/pixscale-1) : star[istar].ixmin;
        star[istar].ixmax = (star[istar].ixmax > nx) ? (int)ceil(star[istar].x+fitrad/pixscale+1) : star[istar].ixmax;
        star[istar].iymin = (star[istar].iymin < 1) ? (int)floor(star[istar].y-fitrad/pixscale-1) : star[istar].iymin;
        star[istar].iymax = (star[istar].iymax > nx) ? (int)ceil(star[istar].y+fitrad/pixscale+1) : star[istar].iymax;

        star[istar].ixmin = star[istar].ixmin < 1 ? nx+1 : star[istar].ixmin;
        star[istar].iymin = star[istar].iymin < 1 ? ny+1 : star[istar].iymin;
        star[istar].ixmax = star[istar].ixmax > nx ? 0 : star[istar].ixmax;
        star[istar].iymax = star[istar].iymax > ny ? 0 : star[istar].iymax;

        if (star[istar].id >=0 && star[istar].ixmin < star[istar].ixmax && star[istar].iymin < star[istar].iymax &&
            star[istar].ixmin >= 1 && star[istar].ixmax <= frame[iframe].nx &&
            star[istar].iymin >= 1 && star[istar].iymax <= frame[iframe].ny ) {
          frame[iframe].nstar += 1;
          star[istar].used = 1;
          if (istar1<0) istar1 = istar;
          if (maxstar>=0 && frame[iframe].nstar > maxstar) {
            frame[iframe].nstar -= 1;
            star[istar].ixmin = frame[iframe].nx+1;
            star[istar].iymin = frame[iframe].ny+1;
            star[istar].ixmax = 0;
            star[istar].ixmax = 0;
          }
        }
        star[istar].good = 0;
        star[istar].apinten[jfilt] = 0;
        star[istar].bigapinten[jfilt] = 0;

        // set parameters for transformation terms to minimize computations at each pixel
        if (star[istar].inten[tfilt]>0) {
          star[istar].color = star[istar].inten[jfilt]/star[istar].inten[tfilt];
          if (star[istar].color>0)
            star[istar].pow = pow(star[istar].color,trans);
          else {
            star[istar].color = 0.;
            star[istar].pow = 1.;
          }
        } else {
          star[istar].color = 0.;
          star[istar].pow = 1.;
        }
      }

      // get current positions of galaxy in this frame, and pixel limits for consideration
      for (igal=0; igal<ngal; igal++) {
        gal[igal].ixmin = 32767;
        gal[igal].iymin = 32767;
        gal[igal].ixmax = -32767;
        gal[igal].iymax = -32767;
        i=0;
         for (igx=0;igx<=gal[igal].nsize;igx+=gal[igal].nsize) {
          gra=gal[igal].ra+(igx-gal[igal].nsize/2)*
                          gal[igal].dx/frame[iframe].cosdec+
                          i*psfrad;
          for (igy=0;igy<=gal[igal].nsize;igy+=gal[igal].nsize) {
            gdec=gal[igal].dec+(igy-gal[igal].nsize/2)*gal[igal].dx+
                          i*psfrad;
            getgcoord(frame[iframe],&gal[igal],gra,gdec);
            gal[igal].ixmin = 
               gal[igal].x<gal[igal].ixmin ? gal[igal].x : gal[igal].ixmin;
            gal[igal].ixmax = 
               gal[igal].x>gal[igal].ixmax ? gal[igal].x : gal[igal].ixmax;
            gal[igal].iymin = 
               gal[igal].y<gal[igal].iymin ? gal[igal].y : gal[igal].iymin;
            gal[igal].iymax = 
               gal[igal].y>gal[igal].iymax ? gal[igal].y : gal[igal].iymax;
          }
         }
         // If galaxy is too close to edge of frame, don't allow frame to influence galaxy
         if (gal[igal].ixmin<0 || gal[igal].ixmax>frame[iframe].nx-1 ||
             gal[igal].iymin<0 || gal[igal].iymax>frame[iframe].ny-1 )
           frame[iframe].skipgal = 1;
        // Set up quantities for color terms
        for (igx=0;igx<gal[igal].nsize;igx++) {
          for (igy=0;igy<gal[igal].nsize;igy++) {
            if (gexp(gal[igal].inten[igx][igy][tfilt])>0) {
              gal[igal].color[igx][igy] = 
              gexp(gal[igal].inten[igx][igy][jfilt])/gexp(gal[igal].inten[igx][igy][tfilt]);
              if (gal[igal].color[igx][igy]>0)
                gal[igal].pow[igx][igy] = pow(gal[igal].color[igx][igy],trans);
              else {
                gal[igal].color[igx][igy] = 0.;
                gal[igal].pow[igx][igy] = 1.;
              }
            } else {
              gal[igal].color[igx][igy] = 0.;
              gal[igal].pow[igx][igy] = 1.;
            }
          }
        }
      }

      /* Loop over all pixels */
      /* model = sum (star*psf) + sum (gal*psfint) + sn*psf + back */
      nskipped = 0;
      //fprintf(stderr,"niter: %d iframe: %d icombo: %d ifilt: %d jfilt: %d tfilt: %d itfilt: %d tfit: %d trans: %f fitsky: %d nlib: %d ix: ",niter,iframe,icombo,ifilt,jfilt,tfilt,itfilt,tfit,trans,fitsky,frame[iframe].psf->nlib);
      for (ix=0; ix<frame[iframe].nx; ix++) {
        //if (ix%100==0) fprintf(stderr," %d ",ix);
        for (iy=0; iy<frame[iframe].ny; iy++) {

         data = *(frame[iframe].data+iy*frame[iframe].nx+ix);
         if (data>frame[iframe].lowbad&&data<frame[iframe].highbad) {

          // Initialize derivatives wrt each parameter 
          for (ipar=0;ipar<npar;ipar++) dpar[ipar] = 0;
          if (converge) for (ipar=0;ipar<npar;ipar++) spar[ipar] = 0;
          used = FALSE;
          objpix = FALSE;
          galpix = FALSE;
          wt = mod = modvar = modgal = modstar = modsky = 0;
          dxdd = -cd1_2/den;
          dxdr = cd2_2/den*frame[iframe].cosdec;
          dydd = cd1_1/den;
          dydr = -cd2_1/den*frame[iframe].cosdec;
          xytosky(frame[iframe], (double) ix, (double) iy, &pdec, &pra);

          // Variables
          for (ivar=0; ivar<nvar; ivar++) {
           skipvar[ivar] = 1;
           if (frame[iframe].jd >= var[ivar].jdmin&&frame[iframe].jd <= var[ivar].jdmax) {
            if (ix>=var[ivar].ixmin&&ix<=var[ivar].ixmax&&
                iy>=var[ivar].iymin&&iy<=var[ivar].iymax) {
              ra = var[ivar].ra+strip[istrip].pmra*(frame[iframe].jd-51545.292)/365.25/3600.;
              dec = var[ivar].dec+strip[istrip].pmdec*(frame[iframe].jd-51545.292)/365.25/3600.;
              if ((dist2=pixdist2(frame[iframe],ra,dec,pra,pdec,psfrad))<psfrad2) {
               skipvar[ivar] = 0;
               x=var[ivar].x;
               y=var[ivar].y;
               objpix = TRUE;
               p = psf(frame[iframe].psf,ix-var[ivar].x,iy-var[ivar].y,&dpdx,&dpdy);
               tot = var[ivar].inten[iframe] * frame[iframe].zeropt * f0 * strip[istrip].scale[ifilt];
               mod+= tot*p;
               modvar+= tot*p;
               if (dist2<fitrad2) {
                rsq = ((x-ix)*(x-ix) + (y-iy)*(y-iy)) * pixscale * pixscale / fitrad2;
                pwt = 5. / (5 + rsq/(1.-rsq) );
                wt = wt>pwt ? wt : pwt;
                used = TRUE;
                /* Derivs wrt variable position */
                if (!frame[iframe].skipgal&&!(lock&LOCKVPOS)) {
                  dpar[ivar*2] += tot*(dpdx*dxdr+dpdy*dydr);
                  dpar[ivar*2+1] += tot*(dpdx*dxdd+dpdy*dydd);
                }
                if (!(lock&LOCKVINTEN)) {
                  /* Derivs wrt variable intensity */
                  dpar[nvar*2+ivar*nframe+iframe] += frame[iframe].zeropt*p*f0*strip[istrip].scale[ifilt];
                }
  
                if (!(lock&LOCKFRAMEINTEN)) {
                  if (!frame[iframe].first||(frame[iframe].nstar>0&&(lock&LOCKSTARINTEN))) {
                    /* Deriv wrt frame zeropt */
                    dpar[nvar*2+nvar*nframe+iframe] += var[ivar].inten[iframe] * p * f0;
                  }
                }
                if (lock&LOCKFRAMEPOS) {
                    i0=nvar*2 + nvar*nframe + nframe*(7+NBACK) + nstar*(4+nfilt) + 
                       ncombo + ncombo*2*MAXDIST + ngal*gal[0].nsize*gal[0].nsize*nfilt;
                    i0 += istrip*(4+nfilt);
                    if (istrip>0) {
                      dpar[i0] += tot*(dpdx*dxdr+dpdy*dydr);
                      dpar[i0+1] += tot*(dpdx*dxdd+dpdy*dydd);
#ifdef STRIPINTEN
                      dpar[i0+4+ifilt] += var[ivar].inten[iframe] * p * f0 * frame[iframe].zeropt;
#endif
                    }
#ifdef STRIPPMVAR
                    // Strip reference proper motion
                    dpar[i0+2] += tot*(dpdx*dxdr+dpdy*dydr)*(frame[iframe].jd-51545.292)/365.25/3600.;
                    dpar[i0+3] += tot*(dpdx*dxdd+dpdy*dydd)*(frame[iframe].jd-51545.292)/365.25/3600.;   
#endif
                }
                if (iframe>0||(frame[iframe].nstar>0&&(lock&LOCKSTARPOS))) {
                  if (!(lock&LOCKFRAMEPOS) && (frame[iframe].nstar>0 || !frame[iframe].first)) {
                    /* Derivs wrt frame position (crpix)*/
                    i0=nvar*2+nvar*nframe+nframe+iframe*2-1;
                    dpar[i0+1] += tot*dpdx;
                    dpar[i0+2] += tot*dpdy;
                  }
                  if (!(lock&LOCKFRAMEASTROM)) {
                    /* Derivs wrt linear astrometric parameters */
                    dsolve(frame[iframe], dec, ra, dx, dy);
                    i0=nvar*2+nvar*nframe+nframe+nframe*2+iframe*4-1;
                    dpar[i0+1] += tot*(dpdx*dx[0]+dpdy*dy[0]);
                    dpar[i0+2] += tot*(dpdx*dx[1]+dpdy*dy[1]);
                    dpar[i0+3] += tot*(dpdx*dx[2]+dpdy*dy[2]);
                    dpar[i0+4] += tot*(dpdx*dx[3]+dpdy*dy[3]);
                  }
                }
               }
              }
            }
           }
          }

          // Stars
          for (istar=0; istar<nstar; istar++) {
           skip[istar] = 1;
           if (star[istar].id>=0 && ix>=star[istar].ixmin&&ix<=star[istar].ixmax&&
               iy>=star[istar].iymin&&iy<=star[istar].iymax) {
            // If within a PSF radius, include pixel in model
            dec=star[istar].dec+star[istar].pmdec*(frame[iframe].jd-51545.292)/365.25/3600.;
            ra=star[istar].ra+star[istar].pmra*(frame[iframe].jd-51545.292)/365.25/3600.;
            if ((dist2=pixdist2(frame[iframe],ra,dec,pra,pdec,psfrad))<psfrad2 &&
                    star[istar].inten[jfilt]>0) {
             skip[istar] = 0;
             x=star[istar].x;
             y=star[istar].y;
             objpix = TRUE;
             p = psf(frame[iframe].psf,ix-star[istar].x,iy-star[istar].y,&dpdx,&dpdy);
             tot = star[istar].inten[jfilt]*frame[iframe].zeropt*f0;
             if (tot>100) star[istar].good = 1;
             if (trans!=0) tot *= star[istar].pow;
             mod+= tot*p;
             modstar+= tot*p;
             // If within a fitting radius, include pixel in parameter derivative calculation
             if (dist2<fitrad2) {
              // Lower weight near fitting radius to prevent solution from oscillating
              rsq = ((x-ix)*(x-ix) + (y-iy)*(y-iy)) * pixscale * pixscale / fitrad2;
              pwt = 5. / (5 + rsq/(1.-rsq) );
              wt = wt>pwt ? wt : pwt;
              used = TRUE;
              if (!(lock&LOCKFRAMEINTEN)) {
                if (!frame[iframe].first||(lock&LOCKSTARINTEN)) {
                  // Deriv wrt frame zeropt
                  dpar[nvar*2+nvar*nframe+iframe] += star[istar].inten[jfilt] * p * f0;
                }
              }
              if (lock&LOCKFRAMEPOS && istrip>0) {
                  i0=nvar*2 + nvar*nframe + nframe*(7+NBACK) + nstar*(4+nfilt) + 
                     ncombo + ncombo*2*MAXDIST + ngal*gal[0].nsize*gal[0].nsize*nfilt;
                  i0 += istrip*(4+nfilt);
                  dpar[i0] += tot*(dpdx*dxdr+dpdy*dydr);
                  dpar[i0+1] += tot*(dpdx*dxdd+dpdy*dydd);
              }
              if (iframe>0||(frame[iframe].nstar>0&&(lock&LOCKSTARPOS))) {
                if (!(lock&LOCKFRAMEPOS) && (frame[iframe].nstar>0 || !frame[iframe].first)) {
                  i0=nvar*2+nvar*nframe+nframe+iframe*2-1;
                  // Derivs wrt frame position
                  dpar[i0+1] += tot*dpdx;
                  dpar[i0+2] += tot*dpdy;
                }
                if (!(lock&LOCKFRAMEASTROM)) {
                  // Derivs wrt linear astrometric parameters
                  dsolve(frame[iframe], dec, ra, dx, dy);
                  i0=nvar*2+nvar*nframe+nframe+nframe*2+iframe*4-1;
                  dpar[i0+1] += tot*(dpdx*dx[0]+dpdy*dy[0]);
                  dpar[i0+2] += tot*(dpdx*dx[1]+dpdy*dy[1]);
                  dpar[i0+3] += tot*(dpdx*dx[2]+dpdy*dy[2]);
                  dpar[i0+4] += tot*(dpdx*dx[3]+dpdy*dy[3]);
                }
                if (icombo>1) {
                  // Derivatives with respect to distortion terms
                  i0=nvar*2+nvar*nframe+nframe*(7+NBACK)+nstar*(4+nfilt)+
                       ncombo+(icombo-1)*(2+MAXDIST);
  /*
                  dpar[i0]    += tot*dpdx*x*x;
                  dpar[i0+1]  += tot*dpdy*x*x;
                  dpar[i0+2]  += tot*dpdx*x*y;
                  dpar[i0+3]  += tot*dpdy*x*y;
                  dpar[i0+4]  += tot*dpdx*y*y;
                  dpar[i0+5]  += tot*dpdy*y*y;
                  dpar[i0+6]  += tot*dpdx*x*x*x;
                  dpar[i0+7]  += tot*dpdy*x*x*x;
                  dpar[i0+8]  += tot*dpdx*x*x*y;
                  dpar[i0+9]  += tot*dpdy*x*x*y;
                  dpar[i0+10] += tot*dpdx*x*y*y;
                  dpar[i0+11] += tot*dpdy*x*y*y;
                  dpar[i0+12] += tot*dpdx*y*y*y;
                  dpar[i0+13] += tot*dpdy*y*y*y;
  */
                }
              }
              // Derivs wrt star position and brightness
              i0=nvar*2+nvar*nframe+nframe*(7+NBACK)-1;
              i0+=istar*(4+nfilt);
              if (!(lock&LOCKSTARPOS)) {
                dpar[i0+1] += tot*(dpdx*dxdr+dpdy*dydr);
                dpar[i0+2] += tot*(dpdx*dxdd+dpdy*dydd);
              }
              if ((lock&FITSTARPM)&&(istar!=istar1)) {
                dpar[i0+3] += tot*(dpdx*dxdr+dpdy*dydr)*(frame[iframe].jd-51545.292)/365.25/3600.;
                dpar[i0+4] += tot*(dpdx*dxdd+dpdy*dydd)*(frame[iframe].jd-51545.292)/365.25/3600.;
              }
              if (!(lock&LOCKSTARINTEN)) {
                if (trans == 0)
                  dpar[i0+5+ifilt] += frame[iframe].zeropt*p*f0;
                else {
                  dpar[i0+5+ifilt] += frame[iframe].zeropt*p*f0*
                     (1+trans)*star[istar].pow;
                  if (itfilt>=0) dpar[i0+5+itfilt] += -1* frame[iframe].zeropt*p*f0*
                      star[istar].pow*star[istar].color;
                }
              }
              if (converge) {
                if (trans == 0)
                  spar[i0+5+ifilt] += frame[iframe].zeropt*p*f0;
                else {
                  spar[i0+5+ifilt] += frame[iframe].zeropt*p*f0*
                     (1+trans)*star[istar].pow;
                  if (itfilt>=0) spar[i0+5+itfilt] += -1* frame[iframe].zeropt*p*f0*
                      star[istar].pow*star[istar].color;
                }
              }
              if (tfit) {
                i0=nvar*2+nvar*nframe+nframe*(7+NBACK)+nstar*(4+nfilt)-1;
                if (star[istar].color>0) 
                    dpar[i0+icombo] += tot*p*log(star[istar].color);
              }
             }
            }
           }
          }

          // Galaxy 
          for (igal=0; igal<ngal; igal++) {
           if (ix>=gal[igal].ixmin&&ix<=gal[igal].ixmax&&
               iy>=gal[igal].iymin&&iy<=gal[igal].iymax) {
            ipix=0;
            for (igx=0;igx<gal[igal].nsize;igx++) {
              gra=gal[igal].ra+strip[istrip].pmra*(frame[iframe].jd-51545.292)/365.25/3600. +
                  (igx-gal[igal].nsize/2)*gal[igal].dx/frame[iframe].cosdec;
              for (igy=0;igy<gal[igal].nsize;igy++) {
                gdec=gal[igal].dec+strip[istrip].pmdec*(frame[iframe].jd-51545.292)/365.25/3600. +
                  (igy-gal[igal].nsize/2)*gal[igal].dx;
                if ((dist2=pixdist2(frame[iframe],gra,gdec,pra,pdec,psfrad))<psfrad2) {
                 objpix = TRUE;
                 getgcoord(frame[iframe],&gal[igal],gra,gdec);
                 p=psf(frame[iframe].psfint,ix-gal[igal].x,iy-gal[igal].y,&dpdx,&dpdy);
                 tot= gexp(gal[igal].inten[igx][igy][jfilt])*frame[iframe].zeropt*f0*strip[istrip].scale[ifilt];
                 if (trans!=0) tot *= gal[igal].pow[igx][igy];
                 mod+= tot*p;
                 modgal+= tot*p;
                 if (dist2<psfrad2) {
                  wt= 1.;
/*
          // Lower weight near galaxy edges to prevent solution from oscillating
              x = var[0].x;
              y = var[0].y;
              rsq = ((x-ix)*(x-ix) + (y-iy)*(y-iy)) * pixscale * pixscale / 
                    pow(gal[igal].nsize/2*gal[igal].dx,2);
              pwt = rsq<1 ? 5. / (5 + rsq/(1.-rsq) ) : 0;
              wt = wt>pwt ? wt : pwt;
*/
                  used = TRUE;
                  if (igx==0&&igy==0) galpix = TRUE;
                  if (!(lock&LOCKFRAMEINTEN)) {
                    if (!frame[iframe].first||(frame[iframe].nstar>0&&(lock&LOCKSTARINTEN))) {
                      // Deriv wrt frame zeropt
                      dpar[nvar*2+nvar*nframe+iframe] += 
                              gexp(gal[igal].inten[igx][igy][jfilt]) * p * f0;
                    }
                  }
                  if (lock&LOCKFRAMEPOS) {
                    i0=nvar*2 + nvar*nframe + nframe*(7+NBACK) + nstar*(4+nfilt) + 
                       ncombo + ncombo*2*MAXDIST + ngal*gal[0].nsize*gal[0].nsize*nfilt;
                    i0 += istrip*(4+nfilt);
                    if (istrip>0) {
                      dpar[i0] += tot*(dpdx*dxdr+dpdy*dydr);
                      dpar[i0+1] += tot*(dpdx*dxdd+dpdy*dydd);
#ifdef STRIPINTEN
                      dpar[i0+4+ifilt] += gexp(gal[igal].inten[igx][igy][jfilt]) * p * f0 * frame[iframe].zeropt;
#endif
                    }
#ifdef STRIPPM
                    if (!(lock&LOCKSTRIPPM)) {
                      // Strip reference proper motion
                      dpar[i0+2] += tot*(dpdx*dxdr+dpdy*dydr)*(frame[iframe].jd-51545.292)/365.25/3600.;
                      dpar[i0+3] += tot*(dpdx*dxdd+dpdy*dydd)*(frame[iframe].jd-51545.292)/365.25/3600.;   
                    }
#endif
                  }
                  if (iframe>0||(frame[iframe].nstar>0&&(lock&LOCKSTARPOS))) {
                    if (!(lock&LOCKFRAMEPOS) && (frame[iframe].nstar>0 || !frame[iframe].first)) {
                      i0=nvar*2+nvar*nframe+nframe+iframe*2-1;
                      // Derivs wrt frame position
                      dpar[i0+1] += tot*dpdx;
                      dpar[i0+2] += tot*dpdy;
                    }
                    if (!(lock&LOCKFRAMEASTROM)) {
                      x=gal[igal].x;
                      y=gal[igal].y;
                      dec=gdec;
                      ra=gra;
                      dsolve(frame[iframe], dec, ra, dx, dy);
                      i0=nvar*2+nvar*nframe+nframe+nframe*2+iframe*4-1;
                      dpar[i0+1] += tot*(dpdx*dx[0]+dpdy*dy[0]);
                      dpar[i0+2] += tot*(dpdx*dx[1]+dpdy*dy[1]);
                      dpar[i0+3] += tot*(dpdx*dx[2]+dpdy*dy[2]);
                      dpar[i0+4] += tot*(dpdx*dx[3]+dpdy*dy[3]);
                    }
                  }
                  if (!frame[iframe].skipgal&&!(lock&LOCKGAL)) {
                    i0=nvar*2 + nvar*nframe + nframe*(7+NBACK) + nstar*(4+nfilt) + 
                       ncombo + ncombo*2*MAXDIST;
                    for (i=0;i<igal;i++) i0 += gal[i].nsize*gal[i].nsize*nfilt;

                    if (trans == 0)
                      dpar[i0+ipix*nfilt+ifilt] += dg(gal[igal].inten[igx][igy][jfilt])*frame[iframe].zeropt*p*f0*strip[istrip].scale[ifilt];
                    else {
                      dpar[i0+ipix*nfilt+ifilt] += dg(gal[igal].inten[igx][igy][jfilt])*frame[iframe].zeropt*p*f0*strip[istrip].scale[ifilt]*
                         (1+trans)*gal[igal].pow[igx][igy];
                      if (itfilt>=0) dpar[i0+ipix*nfilt+itfilt] += -1* frame[iframe].zeropt*p*f0*strip[istrip].scale[ifilt]*
                          gal[igal].pow[igx][igy]*gal[igal].color[igx][igy];
                    }
                  }
/*
                  Don't allow galaxy to affect color term
                  if (tfit) {
                    i0=nvar*2+nvar*nframe+nframe*(7+NBACK)+nstar*(4+nfilt)-1;
                    if (gal[igal].color[igx][igy]>0) 
                        dpar[i0+icombo] += tot* log(gal[igal].color[igx][igy]);
                  }
*/
                }
               }
               ipix++;
              }
            }
           }
          }
          // Background
          wt = (wt>0 ? wt : 1.);
          mod += back(&frame[iframe],ix,iy,0);
          modsky += back(&frame[iframe],ix,iy,0);
          //if (fitsky>0&&!objpix&&ix%fitsky==0&&iy%fitsky==0) {
          if ( fitsky>0&&ix%fitsky==0&&iy%fitsky==0)  {
            used = TRUE;
            i0=nvar*2+nvar*nframe+nframe*7;
            for (iback=0; iback<NBACK; iback++) {
              dpar[i0+iframe*NBACK+iback] += frame[iframe].dback[iback];
            }
          } else if (fitsky<0&&galpix) {
            i0=nvar*2+nvar*nframe+nframe*7;
            dpar[i0+iframe*NBACK] += frame[iframe].dback[0];
          }
         
          // On final iteration, use unit weights (for errors). Do aperture photometry
          if (converge) {
  //          wt = 1.;
  //          weightexpo = 0;
            ivar=0;
            if (frame[iframe].jd >= var[ivar].jdmin&&frame[iframe].jd <= var[ivar].jdmax) {
              if (ix>=var[ivar].ixmin&&ix<=var[ivar].ixmax&&
                  iy>=var[ivar].iymin&&iy<=var[ivar].iymax) {
                if ((dist2=pixdist2(frame[iframe],var[ivar].ra,var[ivar].dec,pra,pdec,psfrad))<fitrad2) {
                  var[ivar].apinten[iframe] += (data-modstar-modgal-modsky);
                }
                if ((dist2=pixdist2(frame[iframe],var[ivar].ra,var[ivar].dec,pra,pdec,psfrad))<psfrad2) {
                  var[ivar].bigapinten[iframe] += (data-modstar-modgal-modsky);
                }
              }
            }
            for (istar=0; istar<nstar; istar++) {
               if (ix>=star[istar].ixmin&&ix<=star[istar].ixmax&&
                   iy>=star[istar].iymin&&iy<=star[istar].iymax) {
                 if ((dist2=pixdist2(frame[iframe],star[istar].ra,star[istar].dec,pra,pdec,psfrad))<fitrad2 &&
                    star[istar].inten[jfilt]>0) {
                   star[istar].apinten[jfilt] += (data-modvar-modgal-modsky);
                 }
                 if ((dist2=pixdist2(frame[iframe],star[istar].ra,star[istar].dec,pra,pdec,psfrad))<psfrad2 &&
                    star[istar].inten[jfilt]>0) {
                   star[istar].bigapinten[jfilt] += (data-modvar-modgal-modsky);
                 }
               }
            }
          }
          // On final iteration or with sub option, replace data with data-model
          if (niter==maxiter||sub) {
            if (niter==maxiter||sub&1) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modvar;
            if (niter==maxiter||sub&2) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modstar;
            if (niter==maxiter||sub&4) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modgal;
            if (niter==maxiter||sub&8) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modsky;
            if (niter==maxiter||sub&16) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) = 0;
          }

          // Load this pixel into the solution matrices
          if (used) {
            diff = data - mod;
            //sig2 = (data/gain + rng + pkerr*(modstar+modvar+modgal));
            //sig2 = ((mod>0?mod/gain:0) + rng + pkerr*modstar);
            sig2 = ((mod>0?mod/gain:0) + rng);
            if (weightexpo) {
              relerr = fabs(diff) / sqrt(sig2);
              if (relerr>2.5) wt = wt / (1.+pow(0.4*relerr,weightexpo));
            }
            sig2 /=  wt;
            if (!frame[iframe].skipgal || gal[0].nsize==0) {
              chi2 += diff*diff/sig2;
	      npix++;
            }
            if (niter==maxiter||sub&16) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) = diff*diff/sig2;
            frame[iframe].chi += fabs(diff)/sqrt(sig2+pkerr*modstar*modstar);
            frame[iframe].chi2 += diff*diff/sig2;
            frame[iframe].npix += 1;
            if (converge || !sub) {
              if (objpix) {
                for (ipar=0; ipar<npar; ipar++) {
                  if (dpar[ipar] != 0) {
                    tmp = dpar[ipar] / sig2;
                    beta[ipar] += diff * tmp ;
                    for (jpar=0; jpar<=ipar; jpar++) {
                      if (dpar[jpar] != 0) {
                        alpha[ipar][jpar] += tmp*dpar[jpar];
                      }
                    }
                  }
                }
                if (converge) {
                  for (ipar=0; ipar<npar; ipar++) {
                    if (spar[ipar] != 0) {
                      tmp = spar[ipar] / sig2;
                      beta0[ipar+1] += diff*tmp;
                      for (jpar=0; jpar<=ipar; jpar++) {
                        if (spar[jpar] != 0) {
                          if (converge) alpha0[ipar+1][jpar+1] += tmp*spar[jpar];
                        }
                      }
                    }
                  }
                }
              } else {
                // If this pixel was just used for sky, don't have to loop over all parameters
                i0=nvar*2+nvar*nframe+nframe*7;
                for (ipar=i0; ipar<i0+NBACK; ipar++) {
                  if (dpar[ipar] != 0) {
                    tmp = dpar[ipar] / sig2;
                    beta[ipar] += diff * tmp ;
                    //if (converge) beta0[ipar+1] += diff*tmp;
                    for (jpar=i0; jpar<=ipar; jpar++) {
                      if (dpar[jpar] != 0) {
                        alpha[ipar][jpar] += tmp*dpar[jpar];
                        //if (converge) alpha0[ipar+1][jpar+1] += tmp*dpar[jpar];
                      }
                    }
                  }
                }
              }
            }
            // Add into goodness-of-fit measurement for each star/var
            for (istar=0;istar<nstar;istar++) {
              if (!skip[istar]) {
                star[istar].chi[jfilt] += fabs(diff)/sqrt(sig2+pkerr*modstar*modstar);
                star[istar].sumwt[jfilt] += 1.;
              }
            }
            for (ivar=0;ivar<nvar;ivar++) {
              if (!skipvar[ivar]) {
                var[ivar].chi[iframe] += fabs(diff)/sqrt(sig2+pkerr*modvar*modvar);
                var[ivar].sumwt[iframe] += 1.;
              }
            }
          } else
            nskipped++;
         } else { // if good pixel
           nskipped++;
           //fprintf(stderr,"bad pixel: %d %d %f %f %f\n",ix,iy,data,frame[iframe].lowbad,frame[iframe].highbad);
         }


        } // end of loop over rows 
      } // end of loop over cols
      // fprintf(stderr," skipped: %d of %d\n",nskipped,frame[iframe].nx*frame[iframe].ny);
     } // end if frame.zeropt>0

     // How many "good" stars on frame? If not enough, remove frame!
     ngood = 0;
     for (istar=0;istar<nstar;istar++) {
       if (star[istar].good) ngood++;
     }
     if (lock&LOCKFRAMEASTROM)
       frame[iframe].ngood = -1;
     else
       frame[iframe].ngood = ngood;
#define MINGOOD 4
     if (!(lock&LOCKFRAMEASTROM)&&ngood<MINGOOD) {
      fprintf(stderr,"iframe: %d ngood: %d  lock: %d\n",iframe,ngood,lock);
      fprintf(stderr,"fixing scale\n");
      if (ngood>1) {
       i0=nvar*2+nvar*nframe+nframe+nframe*2+iframe*4-1;
       beta[i0+1] -= beta[i0+4];
       beta[i0+4] = 0;
       beta[i0+2] += beta[i0+3];
       beta[i0+3] = 0;
       alpha[i0+1][i0+1] += alpha[i0+4][i0+4]-2*alpha[i0+1][i0+4];
       alpha[i0+1][i0+2] += 
         alpha[i0+1][i0+3]-alpha[i0+4][i0+2]-alpha[i0+4][i0+3];
       alpha[i0+2][i0+1] += 
         alpha[i0+1][i0+3]-alpha[i0+4][i0+2]-alpha[i0+4][i0+3];
       alpha[i0+2][i0+2] += alpha[i0+3][i0+3]+2*alpha[i0+2][i0+3];
       alpha[i0+3][i0+3] = 0;
       alpha[i0+3][i0+4] = 0;
       alpha[i0+4][i0+3] = 0;
       alpha[i0+4][i0+4] = 0;
      } else {
       frame[iframe].zeropt = 0;
       frame[iframe].zeropterr = -3;
       // Derivs wrt variable intensity for this frame
       beta[nvar*2+ivar*nframe+iframe] = 0.;
       // Derivs wrt frame zeropt
       beta[nvar*2+nvar*nframe+iframe] = 0.;
       par[nvar*2+nvar*nframe+iframe].val = 0.;
       par[nvar*2+nvar*nframe+iframe].oldval = 0.;
       // Derivs wrt frame position
       i0=nvar*2+nvar*nframe+nframe+iframe*2-1;
       beta[i0+1] = beta[i0+2] = 0.;
       i0=nvar*2+nvar*nframe+nframe+nframe*2+iframe*4-1;
       beta[i0+1] = beta[i0+2] = beta[i0+3] = beta[i0+4] = 0.;
      }
     }

     // On final iteration, we will do a solution for stellar intensities on EACH FRAME
     //   separately, to solve for the standard deviation of measured vs input mags of
     //   stars on this frame
     // Also compute aperture corrections and aperture photometry for var[0]
     if (converge) {

      i0=nvar*2+nvar*nframe+nframe*(7+NBACK)-1;
      nfit=0;
      for (istar=0;istar<nstar;istar++) {
        ipar=i0+istar*(4+nfilt)+5+ifilt;
        par[ipar].fit = 0;
        if (beta0[ipar+1] !=0) {
          par[ipar].fit = 1;
          betared[nfit+1] = beta0[ipar+1];
          n2=1;
          for (jstar=0;jstar<=istar;jstar++) {
            jpar=i0+jstar*(4+nfilt)+5+ifilt;
            if (beta0[jpar+1] !=0) {
              alphared[nfit+1][n2] = alpha0[ipar+1][jpar+1];
              alphared[n2][nfit+1] = alpha0[ipar+1][jpar+1];
              n2++;
            }
          }
          nfit++;
        }
      }
      if (nfit>0) {
       for (i=1;i<=nfit;i++) {
         beta0[i] = betared[i];
         for (j=1;j<=nfit;j++) alpha0[i][j] = alphared[i][j]; 
       }
       // Solve equations to get current correction vector
       ludcmp(alphared,nfit,indx,&det);
       lubksb(alphared,nfit,indx,betared);
       mprove(alpha0,alphared,nfit,indx,beta0,betared);
       mprove(alpha0,alphared,nfit,indx,beta0,betared);
       // Invert matrix to get parameter errors -- diagonal elements only 
       for (j=1;j<=nfit;j++) {
         for (i=1;i<=nfit;i++) betatmp[i] = 0.;
         betatmp[j] = 1.;
         lubksb(alphared,nfit,indx,betatmp);
         err[j] = sqrt(betatmp[j]);
       }
       for (istar=0; istar<nstar; istar++) {
         if (frame[iframe].zeropt>0) {
           star[istar].apinten[jfilt] /= (frame[iframe].zeropt*f0);
           star[istar].bigapinten[jfilt] /= (frame[iframe].zeropt*f0);
         } else {
           star[istar].apinten[jfilt] = 0.;
           star[istar].bigapinten[jfilt] = 0.;
         }
       }
       // Determine stellar offsets and sigma for 5 different samples depending
       //  on estimated error
       fp=fopen("stars.out","a");
       for (i=4; i>=0; i--) {

        double wraw, wraw2, efloor, e1, starerr= 0.;
        frame[iframe].starchi2[i] = 100.;
        while (frame[iframe].starchi2[i] > 1 && starerr < 0.1) {
         starerr += 0.001;
         nfit=1;
         sum = sum2 = sumap = wsum = wsum2 = wraw2 = w =0;
         nsum = wnsum = 0;
         wmax = wrawmax = 0.;
         for (istar=0;istar<nstar;istar++) {
           ipar=i0+istar*(4+nfilt)+5+ifilt;
           if (par[ipar].fit) {
             m1 = asinhmag(star[istar].inten[jfilt],1.,jfilt);
             e1 = star[istar].err[jfilt];
             m2 = asinhmag(star[istar].inten[jfilt]+betared[nfit],1.,jfilt);
             m3 = asinhmag(star[istar].apinten[jfilt],1.,jfilt);
             m4 = asinhmag(star[istar].bigapinten[jfilt],1.,jfilt);
             e2 = asinhmagerr(star[istar].inten[jfilt]+betared[nfit],err[nfit],jfilt);
             // Set error floor for starchi calculation to 0.02 to allow for errors in
             //   catalog mags, flat fielding, PSF errors
             efloor = (e2>0.02 ? e2 : 0.02);
             if (i==0 && starerr < 0.0015) fprintf(fp,"%d %d %d %8.3f %8.3f %8.3f %8.2f %8.2f %d %d %d %d %8.2f %8.2f\n",iframe,star[istar].id,frame[iframe].filt,m1,m2,e2,star[istar].x,star[istar].y,frame[iframe].camcol,frame[iframe].cnpix1,frame[iframe].cnpix2,frame[iframe].tele,m3,m4);
             emax = (i<4 ? 0.01*(i+1) : 0.02*(i+1) );
             if (e2 < emax) {
               sum += (m1-m2);
               sum2 += (m1-m2)*(m1-m2);
               w = (m1-m2)*(m1-m2)/(e1*e1+e2*e2+starerr*starerr);
               wsum2 += w;
               wraw = (m1-m2)*(m1-m2)/(efloor*efloor);
               //wmax = (wraw > wmax ? w : wmax);
               if (wraw > wmax) {
                 wmax = w;
                 wrawmax = wraw;
               }      
               wraw2 += wraw;
               nsum++;
               if (frame[iframe].zeropt>0) sumap += star[istar].inten[jfilt]/star[istar].apinten[jfilt];
             }
             nfit++;
           }
         }
         frame[iframe].mean[i] = 0;
         frame[iframe].sig[i] = 0;
         frame[iframe].nsig[i] = nsum;
         if (nsum>0) {
           frame[iframe].mean[i] = sum/nsum;
           frame[iframe].apcor[i] = sumap/nsum;
         }
         if (nsum>1) {
           frame[iframe].sig[i] = sqrt((sum2-sum*sum/nsum)/(nsum-1));
           // Will throw out one 5 sigma measurement
           if (wmax > 5 ) {
             if (nsum>2) frame[iframe].starchi2[i] = (wsum2-wmax)/(nsum-2.);
           } else {
             frame[iframe].starchi2[i] = wsum2/(nsum-1.);
           }
         } else 
           frame[iframe].starchi2[i] = 0.;
         // fprintf(stderr,"i: %d starerr: %f  chi2: %f\n",i,starerr,frame[iframe].starchi2[i]);
        }
   
        // Adopt starerr and starchi from best bin with more than 3 stars
        // Starchi computed without any additional starerr term
        if (nsum>3) {
          if (wmax > 5 )
            frame[iframe].starchi = (wraw2-wrawmax)/(nsum-2.);
          else 
            frame[iframe].starchi = wraw2/(nsum-1.);
          frame[iframe].starerr = starerr-0.001;
        }
       }
       fclose(fp);
       // Aperture correction aperture magnitude for var[0]
       if (frame[iframe].zeropt>0) 
         var[0].apinten[iframe] *= (frame[iframe].apcor[1]/(f0*frame[iframe].zeropt));
       else
         var[0].apinten[iframe] = 0.;

       for (ipar=0;ipar<npar;ipar++) {
         beta0[ipar+1] = 0;
         for (jpar=0;jpar<npar;jpar++) alpha0[ipar+1][jpar+1] = 0.;
       }
      }
     }
     if (frame[iframe].npix>3) 
          frame[iframe].chi *= 1.253 *
            sqrt(1./(frame[iframe].npix*(frame[iframe].npix-3)));
    }  // end of loop over frames
  
    // Calculate goodness of fit parameter for each star for this iteration
    for (istar=0;istar<nstar;istar++) {
      for (i=0;i<nfilt;i++) {
        j = filts[i];
        if (star[istar].sumwt[j]>3) 
          star[istar].chi[j] *= 1.253 *
            sqrt(1./(star[istar].sumwt[j]*(star[istar].sumwt[j]-3)));
      }
    }
    // Calculate goodness of fit parameter for each variable for this iteration
    for (ivar=0;ivar<nvar;ivar++) {
      for (iframe=0;iframe<nframe;iframe++) {
        if (var[ivar].sumwt[iframe]>3) 
          var[ivar].chi[iframe] *= 1.253 *
            sqrt(1./(var[ivar].sumwt[iframe]*(var[ivar].sumwt[iframe]-3)));
      }
    }

    clip = FALSE;
    chi2 = chi2/npix;
    cframe = -1;
  // increase weightexpo if having trouble converging
    if (niter>5) 
      chicrit=2;
    else if (niter>3)
      chicrit=3;
    else if (niter>1)
      chicrit=5;

    if (!converge && !sub && niter!=maxiter && chi2old>0 && 
        ( (niter>4 && chi2>1.001*chi2old && chi2>1) || 
          (niter>1 && niter%2==0 && chi2>chicrit) ) ) {
      // If chi2 has gone up, back up, increment weightexpo to increase downweighting
      //   of pixels with relerr>2
      // clip = TRUE;
      weightexpo++;
      // Set par back to old values and reload into named equivalents
      for (ipar=0;ipar<npar;ipar++) {
        par[ipar].del = 0;
        par[ipar].err = 0;
        par[ipar].val = par[ipar].oldval;
      }
      npar = setpar(0, par, &ncombo, &nfilt, filts,
                  frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
      npar = setpar(1, par, &ncombo, &nfilt, filts,
                  frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
    }

    fprintf(stderr,"niter: %d npar: %d nfit: %d  chi2*npix: %e chi2: %e npix: %d chi2old: %e clip: %d cframe: %d weightexpo: %d\n",
            niter,npar,nfit,chi2*npix,chi2,npix,chi2old,clip,cframe,weightexpo);
    fp = fopen("fit.out","a");
    fprintf(fp,"niter: %d npar: %d nfit: %d  chi2: %e npix: %d chi2old: %e clip: %d cframe: %d weightexpo: %d\n",
            niter,npar,nfit,chi2,npix,chi2old,clip,cframe,weightexpo);
    fclose(fp);

    // Go on to do the least squares solution if called for
    if (converge||(!clip&&!sub&&niter!=maxiter)) {
      // If chi^2 has gone up, and we haven't hit the CLIP criteria above, increase lambda 
      if (!converge && chi2old > 0 && chi2 > 1.001*chi2old) {
        for (i=1;i<=nfit;i++) {
          betared[i] = beta0[i];
          for (j=1;j<=nfit;j++) alphared[i][j] = alpha0[i][j];
        }
        for (i=1;i<=nfit;i++) alphared[i][i] /= (1.+lambda);
        lambda *= 10;
        for (i=1;i<=nfit;i++) alphared[i][i] *= (1.+lambda);
        //niter--;
        for (ipar=0; ipar<npar; ipar++) {
          par[ipar].oldval = par[ipar].savedval;
          par[ipar].val = par[ipar].oldval;
        }
        chi2=chi2old;
        clip = TRUE;
      } else {
//        lambda = (lambda / 10.) < 1.e-3 ? 1.e-3 : (lambda / 10.);
//        if (converge) lambda = 1.e-6;
        if (converge) {
          lastlambda = lambda;
          lambda = 1.e-6;
          lambda = 0.;
        }
        // Pack all parameters that are being fit into arrays and fill in symmetric part of matrix 
        nfit=0;
        i0=nvar*2 + nvar*nframe + nframe*(7+NBACK) + nstar*(4+nfilt) + 
             ncombo + ncombo*2*MAXDIST;
        ig0=-1;
        for (ipar=0; ipar<npar; ipar++) { 
          par[ipar].fit = 0;
          if (beta[ipar]!=0) {
            par[ipar].fit = 1;
            betared[nfit+1] = beta[ipar];
            if (ipar==i0) ig0 = nfit+1;
            n2=1;
            for (jpar=0; jpar<=ipar; jpar++) { 
              if (par[jpar].fit) alphared[nfit+1][n2++] = alpha[ipar][jpar];
            }
            nfit++;
          }
        }
        // Multiply diagonal terms by lambda, reflect non-diagonal terms
        for (ipar=1; ipar<=nfit; ipar++) { 
          alphared[ipar][ipar] *= (1.+lambda);
          for (jpar=ipar+1; jpar<=nfit; jpar++)
            alphared[ipar][jpar] = alphared[jpar][ipar];
        }
      }
      fprintf(stderr,"inverting, lambda: %e clip: %d\n",lambda,clip);
#undef PRINT
#ifdef PRINT
      fprintf(stderr,"betared: \n");
      for (ipar=1; ipar<=nfit; ipar++) fprintf(stderr,"%8.2e %e\n",betared[ipar],alphared[ipar][ipar]);
      fprintf(stderr,"alphared: \n");
      for (ipar=1; ipar<=nfit; ipar++) {
         for (jpar=1; jpar<=nfit; jpar++) fprintf(stderr,"%8.2e ",alphared[ipar][jpar]);
         fprintf(stderr,"\n");
      }
      printf("enter number to continue: ");
      scanf("%d",&ipar);
#endif
  
      // copy into beta0, alpha0 for use later to refine solution
      for (i=1;i<=nfit;i++) {
        beta0[i] = betared[i];
        for (j=1;j<=nfit;j++) alpha0[i][j] = alphared[i][j];
      }
  
      // Solve equations to get current correction vector

for (i=1;i<=nfit;i++) {
  for (j=1;j<=nfit;j++) {
     if (alphared[i][j] != alphared[j][i]) fprintf(stderr,"matrix not symmetric: %d %d\n",i,j);
  }
}
      ludcmp(alphared,nfit,indx,&det);
      lubksb(alphared,nfit,indx,betared);
      mprove(alpha0,alphared,nfit,indx,beta0,betared);
      mprove(alpha0,alphared,nfit,indx,beta0,betared);
  
#ifdef PRINT
      fprintf(stderr,"betared: \n");
      for (ipar=1; ipar<=nfit; ipar++) fprintf(stderr,"%14.6e\n",betared[ipar]);
      fprintf(stderr,"alphared: \n");
      for (ipar=1; ipar<=nfit; ipar++) {
  //      for (jpar=1; jpar<=nfit; jpar++) fprintf(stderr,"%14.6e ",alphared[ipar][jpar]);
  //      fprintf(stderr,"\n");
      }
      for (i=1;i<=nfit;i++) {
        tmp=0;
        for (j=1;j<=nfit;j++) tmp+= alpha0[i][j]*betared[j];
        fprintf(stderr,"check: %d %e %e %e\n",i,beta0[i],tmp,(beta0[i]-tmp)/beta0[i]);
      }
#endif
  
      // Invert matrix to get parameter errors -- diagonal elements only 
      for (j=1;j<=nfit;j++) {
        for (i=1;i<=nfit;i++) betatmp[i] = 0.;
        betatmp[j] = 1.;
        lubksb(alphared,nfit,indx,betatmp);
        if (betatmp[j] > 0)
          err[j] = sqrt(betatmp[j]);
        else
          err[j] = -1;
      }

      // On final iteration do galaxy pixels, determining total error 
      //  integrating over 1.2 arcsec PSF, including correlated errors
      if (converge&&ig0>0) {
       //fp = fopen("cov.dat","w");
       j=ig0;
       for (igx=0; igx<gal[0].nsize; igx++) {
        for (igy=0; igy<gal[0].nsize; igy++) {
          for (ifilt=0;ifilt<nfilt; ifilt++) {
            jfilt=filts[ifilt];
            for (i=1;i<=nfit;i++) betatmp[i] = 0.;
            betatmp[j] = 1.;
            lubksb(alphared,nfit,indx,betatmp);
            // betatmp now has covariances for this galaxy pixel 
            //  (j = igx,igy,ifilt )
            sig2=0.5*0.5/(gal[0].dx*gal[0].dx*3600*3600);
            i=ig0;
            // Loop over each covariance (i = jgx,jgy,kfilt)
            for (jgx=0;jgx<gal[0].nsize;jgx++) {
              for (jgy=0;jgy<gal[0].nsize;jgy++) {
               for (lfilt=0;lfilt<nfilt;lfilt++) {
                kfilt=filts[lfilt];
                // Diagonal element contributes to all locations
                if (jfilt==kfilt) {

                  // Contribution to galaxy sum at each location
                  for (kgx=0;kgx<gal[0].nsize;kgx++) {
                    for (kgy=0;kgy<gal[0].nsize;kgy++) {
                      dist2=(kgx-igx)*(kgx-igx)+(kgy-igy)*(kgy-igy); 
                      p1 = 1/(2*pi*sig2)*exp(-dist2/(2.*sig2));
                      dist2=(kgx-jgx)*(kgx-jgx)+(kgy-jgy)*(kgy-jgy); 
                      p2 = 1/(2*pi*sig2)*exp(-dist2/(2.*sig2));
                      gal[0].interr[kgx][kgy][jfilt]+=p1*p2*betatmp[i];
   //if (kgx==gal[0].nsize/2&&kgy==gal[0].nsize/2) 
    //fprintf(fp,"%d %d %d %d %d %f %f %d %d %e\n",jfilt,igx,igy,jgx,jgy,p1,p2,j,i,betatmp[i]);
                    }
                  }

/*
                 if (i==j) {
                  for (kgx=0;kgx<gal[0].nsize;kgx++) {
                    for (kgy=0;kgy<gal[0].nsize;kgy++) {
                      dist2=(kgx-igx)*(kgx-igx)+(kgy-igy)*(kgy-igy); 
                      p = 1/(2*pi*sig2)*exp(-dist2/(2.*sig2));
                      gal[0].interr[kgx][kgy][jfilt]+=p*p*betatmp[i];
   if (kgx==gal[0].nsize/2&&kgy==gal[0].nsize/2) 
    fprintf(fp,"%d %d %d %d %d %f %f %d %d %e\n",jfilt,igx,igy,jgx,jgy,dist2,p,j,i,betatmp[i]);
                    }
                  }
                 } else {
                  dist2=(jgx-igx)*(jgx-igx)+(jgy-igy)*(jgy-igy); 
                  p = 1/(2*pi*sig2)*exp(-dist2/(2.*sig2));
                  gal[0].interr[igx][igy][jfilt]+=p*p*betatmp[i];
   if (igx==gal[0].nsize/2&&igy==gal[0].nsize/2) 
    fprintf(fp,"%d %d %d %d %d %f %f %d %d %e\n",jfilt,igx,igy,jgx,jgy,dist2,p,j,i,betatmp[i]);
                  gal[0].interr[jgx][jgy][jfilt]+=p*p*betatmp[i];
   if (jgx==gal[0].nsize/2&&jgy==gal[0].nsize/2) 
    fprintf(fp,"%d %d %d %d %d %f %f %d %d %e\n",jfilt,igx,igy,jgx,jgy,dist2,p,j,i,betatmp[i]);
                 }
*/
                }
                i++;
               }
              }
            }
            j++;
          }
        }
       }
    //fclose(fp);
      }
 
      // Unpack parameters from fitting array into full array 
      nfit=0;
      for (ipar=0; ipar<npar; ipar++) { 
        par[ipar].del = 0;
        par[ipar].err = 0;
        if (par[ipar].fit) {
          if (!converge) par[ipar].del = betared[nfit+1];
          par[ipar].err = err[nfit+1];
          nfit++;
        }
        if (niter==0)
          par[ipar].savedval = par[ipar].val;
        else
          par[ipar].savedval = par[ipar].oldval;
        par[ipar].oldval = par[ipar].val;
      }

     
      if (converge) { 
        // Need to load error estimates (set .del to zero above to preserve params)
        npar = setpar(0, par, &ncombo, &nfilt, filts,
                    frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
      } else {
        // Correct parameters, transfer into named counterparts, and check for convergence
        npar = setpar(0, par, &ncombo, &nfilt, filts,
                    frame, nframe, star, nstar, gal, ngal, var, nvar, &converge);
        npar = setpar(1, par, &ncombo, &nfilt, filts,
                    frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
      }
/*
        // Output parameters and corrections for this iteration to par.out
        fp = fopen("par.out","a");
        fprintf(fp," niter ipar old         delta       new\n");
        for (ipar=0; ipar<npar; ipar++) { 
          if (ipar==0) fprintf(fp,"SN posn\n");
          if (ipar==nvar*2) fprintf(fp,"SN inten\n");
          if (ipar==nvar*2+nvar*nframe) fprintf(fp," nframe zeropt\n");
          if (ipar==nvar*2+nvar*nframe+nframe) fprintf(fp," nframe central posns\n");
          if (ipar==nvar*2+nvar*nframe+nframe*3) fprintf(fp," nframe 4 linear params\n");
          if (ipar==nvar*2+nvar*nframe+nframe*7) fprintf(fp," nframe backgrounds\n");
          if (ipar==nvar*2+nvar*nframe+nframe*(7+NBACK)) fprintf(fp," star params\n");
          if (ipar==nvar*2+nvar*nframe+nframe*(7+NBACK)+nstar*(nfilt+4)) fprintf(fp," transform\n");
          if (ipar==nvar*2+nvar*nframe+nframe*(7+NBACK)+nstar*(nfilt+4)+ncombo) fprintf(fp," distortion\n");
          if (ipar==nvar*2+nvar*nframe+nframe*(7+NBACK)+nstar*(nfilt+4)+ncombo+ncombo*2*MAXDIST) fprintf(fp," galaxy:\n");
          fprintf(fp," %d %d %e %e %e %e %d\n",niter,ipar, 
                 par[ipar].oldval, par[ipar].del, par[ipar].val, par[ipar].err, par[ipar].converge);
          if (ipar<npar&&niter>10&&par[ipar].converge==0) 
             fprintf(stderr," %d %e %e %e %e %d\n",ipar, 
               par[ipar].oldval, par[ipar].del, par[ipar].val, par[ipar].err, par[ipar].converge);
        }
        fclose(fp);
*/
    } else {
      // if !clip
      chi2 = chi2old;
    }
    niter++;
    if (!clip && fabs(chi2-chi2old)/chi2 < 0.001) converge = TRUE;
    if ((niter>maxiter)||sub) redo=FALSE;
    if (niter==maxiter||converge) sub = subfinal;
    if (converge&&redo) *chi2final = chi2;
    chi2old = chi2;
    // Check to see if we need to iterate again, or do a final subtraction
    fprintf(stderr,"niter: %d maxiter: %d redo:%d converge: %d sub: %d\n",niter,maxiter,redo,converge,sub);
  }

/*
  npar = setpar(0, par, &ncombo, &nfilt, filts,
                frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
*/
  // Deallocate matrices
  free_matrix(alpha,0,npar,0,npar);
  free_matrix(alphared,1,npar,1,npar);
  free_matrix(alpha0,1,npar,1,npar);
  free_vector(betared,1,npar);
  free_vector(err,1,npar);
  free_vector(beta0,1,npar);
  free_vector(betatmp,1,npar);
  free_ivector(indx,1,npar);

/* Star output
  for (istar=0; istar<nstar; istar++){
    fprintf(stderr,"star: %d  ",istar);
    getscoord(frame[0], &star[istar]);
    for (ifilt=0;ifilt<nfilt;ifilt++) {
      jfilt = filts[ifilt];
      fprintf(stderr,"  %f %f %f %f %f\n",star[istar].x, star[istar].y, 
                                    star[istar].mag[jfilt],
                                    mag(star[istar].inten[jfilt]),
                                    mag(star[istar].inten[jfilt]) - 
                                    mag(star[1].inten[jfilt]) -
                                    (star[istar].mag[jfilt]-star[1].mag[ifilt]));
    }
  }
*/
  if (lastlambda>1) niter=-1*niter;
  return(niter);
}

int setpar(int set, PAR par[MAXPAR], 
           int *ncombo, int *nfilt, int filts[], FRAME *frame, int nframe, 
           STAR *star, int nstar, GAL *gal, int ngal, VAR *var, int nvar, 
           int *converge)
{
  int npar, itele, jtele, ntele=0, ivar, i, nmean[MAXFRAME], cframe;
  double mean[MAXFRAME], maxpm=0.04;
  int iframe, jframe, ifilt, jfilt, istar, iback, idist, ix, iy, igal, new, istrip;
  static int combotel[NTELE*MAXFILT+1];
  static int combofilt[NTELE*MAXFILT+1];

/* Determine number of parameters and initialize them 
  2 SN posns
  NFRAME*nvar  variable intensities
  NFRAME frame zeropt
  NFRAME*2 frame central positions
  NFRAME*4 linear parameters
  NFRAME*NBACK backgrounds;
  NSTAR *
    2 star posns + 2 star proper motions
    nfilt star intensities
  NGAL *
    nsize*nsize*nfilt intensities
  NCOMBO *  (NCOMBO is unique number of tele/filter combinations)
    MAXDIST xdistortions
    MAXDIST ydistortions
    photometric xform coef
*/
  npar=0;

  // variable positions
  for (ivar=0; ivar<nvar; ivar++) {
    if (set) {
      par[npar++].val = var[ivar].ra;
      par[npar++].val = var[ivar].dec;
    } else {
      var[ivar].ra = update_pos(&par[npar],1./3600.,0.1/3600.);
      npar++;
      var[ivar].dec = update_pos(&par[npar],1./3600.,0.1/3600.);
      npar++;
    }
  }

  // variable intensities
  for (ivar=0; ivar<nvar; ivar++) {
    for (iframe=0; iframe<nframe; iframe++) {
      if (set) {
        par[npar].val = var[ivar].inten[iframe];
        par[npar].err = var[ivar].intenerr[iframe];
        npar++;
      } else {
        var[ivar].inten[iframe] = update_inten(&par[npar],0.005,0);
        var[ivar].intenerr[iframe] = par[npar].err;
        if (frame[iframe].zeropt <=0) var[ivar].intenerr[iframe] = -1.;
        npar++;
      }
    }
  }

  for (iframe=0; iframe<nframe; iframe++) {
    if (set)
      par[npar++].val = frame[iframe].zeropt;
    else {
      frame[iframe].zeropt = update_inten(&par[npar],0.005,1);
      frame[iframe].zeropterr = par[npar].err;
      npar++;
    }
  }

  for (iframe=0; iframe<nframe; iframe++) {
    if (set) {
      par[npar++].val = frame[iframe].crpix1;
      par[npar++].val = frame[iframe].crpix2;
    } else {
      frame[iframe].crpix1 = update_par(&par[npar],0.05,0.);
      npar++;
      frame[iframe].crpix2= update_par(&par[npar],0.05,0.);
      npar++;
    }
  }

  *nfilt = 0;
  for (iframe=0; iframe<nframe; iframe++) {
    /* Linear xform parameters for this frame */
    if (set) {
      par[npar++].val = frame[iframe].cd1_1;
      par[npar++].val = frame[iframe].cd1_2;
      par[npar++].val = frame[iframe].cd2_1;
      par[npar++].val = frame[iframe].cd2_2;
    } else {
      frame[iframe].cd1_1 = update_par(&par[npar],1.e-7,0.01);
      npar++;
      frame[iframe].cd1_2 = update_par(&par[npar],1.e-7,0.01);
      npar++;
      frame[iframe].cd2_1 = update_par(&par[npar],1.e-7,0.01);
      npar++;
      frame[iframe].cd2_2 = update_par(&par[npar],1.e-7,0.01);
      npar++;
      if (frame[iframe].ngood < MINGOOD && frame[iframe].ngood > 0) {
        frame[iframe].cd2_1 = frame[iframe].cd1_2;
        frame[iframe].cd2_2 = -frame[iframe].cd1_1;
      }
    }
  }
    /* background parameters for this frame */
  for (iframe=0; iframe<nframe; iframe++) {
    for (iback=0; iback<NBACK; iback++) {
      if (set) 
        par[npar++].val = frame[iframe].back[iback];
      else {
        frame[iframe].back[iback] = update_par(&par[npar],0.,0.01);
        npar++;
      }
    }
    /* See if this frame adds a new filter to be solved for */
    new = TRUE;
    for (ifilt=0; ifilt<*nfilt; ifilt++) {
      if (filts[ifilt] == frame[iframe].filt) new = FALSE;
    }
    if (new) {
      filts[*nfilt] = frame[iframe].filt;
      *nfilt += 1;
    }
  }

  /* Stellar position and intensities */
  for (istar=0; istar<nstar; istar++) {
    if (set) {
      par[npar++].val = star[istar].ra;
      par[npar++].val = star[istar].dec;
      par[npar++].val = star[istar].pmra;
      par[npar++].val = star[istar].pmdec;
    } else {
      star[istar].ra = update_pos(&par[npar],1.0/3600.,0.1/3600.);
      npar++;
      star[istar].dec = update_pos(&par[npar],1.0/3600.,0.1/3600.);
      npar++;
      star[istar].pmra = update_par(&par[npar],0.01/3600.,0.);
      npar++;
      star[istar].pmdec = update_par(&par[npar],0.01/3600.,0.);
      npar++;
    }
    for (ifilt=0; ifilt<*nfilt; ifilt++) {
      jfilt = filts[ifilt];
      if (set) {
        par[npar++].val = star[istar].inten[jfilt];
      } else {
        star[istar].inten[jfilt] = update_inten(&par[npar],0.005,0);
        star[istar].intenerr[jfilt] = par[npar].err;
        npar++;
      }
    }
  }

  *ncombo = 0;
  for (iframe=0; iframe<nframe; iframe++) {
    /* See if this frame adds a new telescope/filter combination 
       to be solved for */
    new = TRUE;
    ifilt = frame[iframe].filt;
    itele = frame[iframe].tele > 1000 ? 0 : frame[iframe].tele;
    for (jframe=0; jframe<iframe; jframe++) {
      jtele = frame[jframe].tele > 1000 ? 0 : frame[jframe].tele;
      if (ifilt == frame[jframe].filt &&
           itele == jtele) {
         new = FALSE;
         frame[iframe].combo = frame[jframe].combo;
      }
    }
    /* Add parameters for this telescope/filter combination */
    if (new) {
      *ncombo += 1;
      combotel[*ncombo] = itele;
      combofilt[*ncombo] = ifilt;
      if (set) 
        par[npar++].val = teles[itele].trans[ifilt];
      else {
        teles[itele].trans[ifilt] = update_par(&par[npar],0.005,0.01);
        teles[itele].transerr[ifilt] = par[npar].err;
        npar++;
      }
      frame[iframe].combo = *ncombo;
    }
  }

  for (i=1; i<=*ncombo; i++) {
    itele=combotel[i];
    ifilt=combofilt[i];
    for (idist=0; idist<MAXDIST; idist++) {
      if (set) {
        par[npar++].val = teles[itele].xdist[idist][ifilt];
        par[npar++].val = teles[itele].ydist[idist][ifilt];
      } else {
        teles[itele].xdist[idist][ifilt] = update_par(&par[npar],0.,0.01);
        npar++;
        teles[itele].ydist[idist][ifilt] = update_par(&par[npar],0.,0.01);
        npar++;
      }
    }
    mean[i] = 0;
    nmean[i] = 0;
  }

#ifdef NOTDEF
  // If any frame has scale factor less than 0.5 of the mean of all other 
  //    observations in this combo set it to 0
  // NOTDEF: just let these go and flag them later as more uncertain. FOr
  //    some (bright) SN, these may provide decent measurements
  if (!set) {
    for (iframe=0; iframe<nframe; iframe++) {
      if (frame[iframe].zeropt > 0 ) {
        i = frame[iframe].combo;
        mean[i] += frame[iframe].zeropt;
        nmean[i]++;
      }
    }
    for (iframe=0; iframe<nframe; iframe++) {
      i = frame[iframe].combo;
      if (nmean[i]>1&&frame[iframe].zeropt*(nmean[i]-1)/(mean[i]-frame[iframe].zeropt) < 0.5 ) {
        frame[iframe].zeropt = 0;
        if (frame[iframe].zeropterr > 0) frame[iframe].zeropterr = -1;
        for (ivar=0; ivar<nvar; ivar++) var[ivar].inten[iframe] = -1.;
        for (ivar=0; ivar<nvar; ivar++) var[ivar].intenerr[iframe] = -1.;
      }
    }
  }
#endif

  /* Galaxy intensities */
  //gal[0].ra = var[0].ra;
  //gal[0].dec = var[0].dec;
  for (igal=0; igal<ngal; igal++) {
    for (ix=0; ix<gal[igal].nsize; ix++) {
      for (iy=0; iy<gal[igal].nsize; iy++) {
        for (ifilt=0; ifilt<*nfilt; ifilt++) {
          jfilt = filts[ifilt];
          if (set) 
            par[npar++].val = gal[igal].inten[ix][iy][jfilt];
          else {
            //par[npar].del = par[npar].del > 0.5 ? 0.5 : par[npar].del;
            //par[npar].del = par[npar].del < -0.5 ? -0.5 : par[npar].del;
            gal[igal].inten[ix][iy][jfilt] = update_inten(&par[npar],0.02,0);
            gal[igal].intenerr[ix][iy][jfilt] = par[npar].err;
            //if (gal[igal].inten[ix][iy][jfilt] < 2*par[npar].err)
/*
            if (gal[igal].inten[ix][iy][jfilt] < 0)
              gal[igal].inten[ix][iy][jfilt] = 0.;
*/
            npar++;
          }
        }
      }
    }
  }

  for (istrip=0; istrip < nstrip; istrip++) {
    if (set) {
      par[npar++].val = strip[istrip].dra;
      par[npar++].val = strip[istrip].ddec;
      par[npar++].val = strip[istrip].pmra;
      par[npar++].val = strip[istrip].pmdec;
      for (ifilt=0; ifilt<*nfilt; ifilt++)
        par[npar++].val = strip[istrip].scale[ifilt];
     
    } else {
      strip[istrip].dra = update_pos(&par[npar],1.0/3600.,0.1/3600.);
      strip[istrip].draerr = par[npar].err;
      npar++;
      strip[istrip].ddec = update_pos(&par[npar],1.0/3600.,0.1/3600.);
      strip[istrip].ddecerr = par[npar].err;
      npar++;
      strip[istrip].pmra = update_par(&par[npar],0.01/3600.,0.);
      strip[istrip].pmraerr = par[npar].err;
      npar++;
      strip[istrip].pmdec = update_par(&par[npar],0.01/3600.,0.);
      strip[istrip].pmdecerr = par[npar].err;
      npar++;
      strip[istrip].pmra = ( strip[istrip].pmra>maxpm ? maxpm : strip[istrip].pmra );
      strip[istrip].pmdec = ( strip[istrip].pmdec>maxpm? maxpm : strip[istrip].pmdec );
      strip[istrip].pmra = ( strip[istrip].pmra<-maxpm ? -maxpm : strip[istrip].pmra );
      strip[istrip].pmdec = ( strip[istrip].pmdec<-maxpm ? -maxpm : strip[istrip].pmdec );
fprintf(stderr,"strip: %d %e %e %e %e",istrip,strip[istrip].dra,strip[istrip].ddec,strip[istrip].pmra,strip[istrip].pmdec);
      for (ifilt=0; ifilt<*nfilt; ifilt++) {
        strip[istrip].scale[ifilt] = update_inten(&par[npar],0.005,1);
fprintf(stderr," %e ",strip[istrip].scale[ifilt]);
        npar++;
fprintf(stderr,"\n");
      }
    }
  }

  *converge = 1;
  for (i=0; i<npar; i++) *converge *= par[i].converge;

  return(npar);
}
double update_par(PAR *x, double dmin, double fmin)
{
  if (x->del==0) {
    x->converge = 1;
    return(x->val);
  }
  if (fabs(x->del) < x->err || fabs(x->del/x->val) < fmin || fabs(x->del)<dmin)
    x->converge = 1;
  else
    x->converge = 0;
  return (x->val+x->del);
}

double update_pos(PAR *x, double dmax, double dmin)
{
  double newpos;

  if (x->del==0) {
    x->converge = 1;
    return(x->val);
  }
  x->converge = 0;

  newpos = fabs(x->del)>dmax ? x->val+dmax*(x->del/fabs(x->del)) : x->val+x->del;
  //newpos = x->val+x->del;
  if (fabs(newpos-x->val) < x->err ||
      fabs(newpos-x->val) < dmin ) x->converge = 1;
  return(newpos);
}

double update_inten(PAR *x, double rmax, int pos)
{
  double dxmax;

  if (x->del==0) {
    x->converge = 1;
    return(x->val);
  }
  x->converge = 0;
  if ((x->val+x->del)/x->val < (1.+rmax) && 
        (x->val+x->del)/x->val > (1.-rmax)) x->converge = 1;
  if (fabs(x->del) < x->err/2) x->converge = 1;
  if (pos || x->val>10*x->err) {
    if ( (x->val+x->del)/x->val > 5) {
      return (5*x->val);
    } else if ( (x->val+x->del)/x->val < 0.2) {
      return (x->val/5.);
    }
  } else if (x->val>5*x->err) {
    if ((x->val+x->del)/x->val < (1.+2*rmax) && 
        (x->val+x->del)/x->val > (1.-2*rmax)) x->converge = 1;
  } else {
    if (fabs(x->del) < x->err/2) x->converge = 1;
  }
  return (x->val+x->del);
}


void skytoxy(FRAME frame, double dec, double ra, double *x, double *y) 
{
  double a,b,c,d,den,xt,yt,xpix,ypix, xd, yd;
  int istat, maxpar=2, npar=2, i, j, iter;

  frame.invert[0][0] = a = frame.cd1_1;
  frame.invert[0][1] = b = frame.cd1_2;
  frame.invert[1][0] = c = frame.cd2_1;
  frame.invert[1][1] = d = frame.cd2_2;
  den=a*d-b*c;
  xt = frame.crpix1 + 
       d/den*(ra-frame.crval1+frame.stripdra)*frame.cosdec-b/den*(dec-frame.crval2+frame.stripddec);
  yt = frame.crpix2 - 
       c/den*(ra-frame.crval1+frame.stripdra)*frame.cosdec+a/den*(dec-frame.crval2+frame.stripddec);

  *x = xt;
  *y = yt;

#ifdef DISTORT
/* Distortion terms */
  i = frame.tele > 1000 ? 0 : frame.tele;
  j = frame.filt;

  xpix = xt;
  ypix = yt;
  for (iter=0; iter<3; iter++) {
    xd = xpix-frame.crpix1;
    yd = ypix-frame.crpix2;
    *x = xt - 
        (frame.xdist[0] + 
        frame.xdist[1]*xd + 
        frame.xdist[2]*yd + 
        frame.xdist[3]*xd*xd + 
        frame.xdist[4]*xd*yd + 
        frame.xdist[5]*yd*yd +
        frame.xdist[6]*xd*xd*xd +
        frame.xdist[7]*xd*xd*yd +
        frame.xdist[8]*xd*yd*yd +
        frame.xdist[9]*yd*yd*yd);
    *y = yt -
        (frame.ydist[0] + 
        frame.ydist[1]*xd + 
        frame.ydist[2]*yd + 
        frame.ydist[3]*xd*xd + 
        frame.ydist[4]*xd*yd + 
        frame.ydist[5]*yd*yd +
        frame.ydist[6]*xd*xd*xd +
        frame.ydist[7]*xd*xd*yd +
        frame.ydist[8]*xd*yd*yd +
        frame.ydist[9]*yd*yd*yd);
    xpix = *x;
    ypix = *y;
  }
#endif
}

void xytosky(FRAME frame, double xpix, double ypix, double *dec, double *ra)
{
  double x, y, xd, yd;
  int i, j;

  i = frame.tele > 1000 ? 0 : frame.tele;
  j = frame.filt;

#ifdef DISTORT
  xd = xpix - frame.crpix1;
  yd = ypix - frame.crpix2;
  x = xpix + 
      frame.xdist[0] + 
      frame.xdist[1]*xd + 
      frame.xdist[2]*yd + 
      frame.xdist[3]*xd*xd + 
      frame.xdist[4]*xd*yd + 
      frame.xdist[5]*yd*yd +
      frame.xdist[6]*xd*xd*xd +
      frame.xdist[7]*xd*xd*yd +
      frame.xdist[8]*xd*yd*yd +
      frame.xdist[9]*yd*yd*yd;
  y = ypix +
      frame.ydist[0] + 
      frame.ydist[1]*xd + 
      frame.ydist[2]*yd + 
      frame.ydist[3]*xd*xd + 
      frame.ydist[4]*xd*yd + 
      frame.ydist[5]*yd*yd +
      frame.ydist[6]*xd*xd*xd +
      frame.ydist[7]*xd*xd*yd +
      frame.ydist[8]*xd*yd*yd +
      frame.ydist[9]*yd*yd*yd;
#else
  x = xpix;
  y = ypix;
#endif
  *ra = frame.crval1 - frame.stripdra + 
         ((x-frame.crpix1)*frame.cd1_1 + (y-frame.crpix2)*frame.cd1_2)/frame.cosdec;
  *dec = frame.crval2 - frame.stripddec + 
         (x-frame.crpix1)*frame.cd2_1 + (y-frame.crpix2)*frame.cd2_2;
}
void dsolve(FRAME frame, double dec, double ra, double *dx, double *dy)
{
  double a,b,c,d,den,den2,dra,ddec;
  double x,y,x2,y2;

  a=frame.cd1_1;
  b=frame.cd1_2;
  c=frame.cd2_1;
  d=frame.cd2_2;
  den=a*d-b*c;
  den2=den*den;
  ddec=(dec-frame.crval2);
  dra=(ra-frame.crval1)*frame.cosdec;
  //inv[0][0] = d / den;
  //inv[0][1] = -b / den;
  //inv[1][0] = -c / den;
  //inv[1][1] = a / den;
  //fprintf(stderr,"x: %f\n",inv[0][0] * (dec-frame.crval2) + inv[0][1] * (ra-frame.crval1));
  *(dx+0) = dra*(-d*d/den2) + ddec*(b*d/den2);
  *(dx+1) = dra*(d*c/den2) + ddec*(-1./den - b*c/den2);
  *(dx+2) = dra*(d*b/den2) + ddec*(-b*b/den2);
  *(dx+3) = dra*(1./den - d*a/den2) + ddec*(b*a/den2);
  //fprintf(stderr,"y: %f\n",inv[1][0] * (dec-frame.crval2) + inv[1][1] * (ra-frame.crval1));
  *(dy+0) = dra*(c*d/den2) + ddec*(1./den-a*d/den2);
  *(dy+1) = dra*(-c*c/den2) + ddec*(a*c/den2);
  *(dy+2) = dra*(-1./den-b*c/den2) + ddec*(a*b/den2);
  *(dy+3) = dra*(c*a/den2) + ddec*(-a*a/den2);
}

void getsncoord(FRAME frame, VAR *var)
{
  skytoxy(frame,var->dec,var->ra,&var->x,&var->y); 
  // double pra, pdec;
  //xytosky(frame, var->x, var->y, &pdec, &pra);
  // fprintf(stderr,"getsncoord: %f %f %f %f\n",var->dec, var->ra, pdec, pra);
}
void getscoord(FRAME frame, STAR *star)
{
  double ra, dec;
  dec=star->dec+star->pmdec*(frame.jd-51545.292)/365.25/3600.;
  ra=star->ra+star->pmra*(frame.jd-51545.292)/365.25/3600.;
  skytoxy(frame,dec,ra,&star->x,&star->y);
}
void getgcoord(FRAME frame, GAL *gal, double gra, double gdec)
{
  skytoxy(frame,gdec,gra,&gal->x,&gal->y);
}

void psfint(FRAME *frame, int smooth)
{
  int i, j, ii, jj, n, nsum;
  double dx;
  PSF *psf;
  PSF *spsf;

  psf=frame->psf;
  n=psf->n;
  dx=psf->dx;
  frame->psfint = (PSF *)(malloc(sizeof(PSF)));
  spsf=frame->psfint;
  spsf->n=n;
  spsf->dx=dx;
  spsf->nlib=psf->nlib;
  spsf->data = (float *)malloc(n*n*sizeof(float));
  for (i=0;i<n;i++) {
    for (j=0;j<n;j++) {
      *(spsf->data+j*n+i) = 0.;
      nsum = 0;
      for (ii=i-smooth/2;ii<=i+smooth/2;ii++) {
        if (ii>=0&&ii<n) {
          for (jj=j-smooth/2;jj<=j+smooth/2;jj++) {
            if (jj>=0&&jj<n) {
              *(spsf->data+j*n+i) += *(psf->data+jj*n+ii);
              nsum++;
            }
          }
        }
      }
      *(spsf->data+j*n+i) /= nsum;
    }
  }

}
double psf(PSF *psf, double x, double y, double *dpdx, double *dpdy)
{
  int ix, iy, n, nx;
  float *data;
  double xpix, ypix, y1x1, y1x2, y2x1, y2x2, y1, y2;
  double dx, dy, dpdy1,dpdy2;
  data = psf->data;
  n = psf->n;

/*
  xpix = x*psf->nlib + psf->n/2;
  ypix = y*psf->nlib + psf->n/2;
  ix = (int) floor(xpix);
  if (ix<0||ix>(psf->n-2)) return(0.);
  iy = (int) floor(ypix);
  if (iy<0||iy>(psf->n-2)) return(0.);
  dx = xpix - ix;
  dy = ypix - iy;
*/
//fprintf(stderr,"nlib: %d psf->dx: %f x: %f y:%f xpix: %f ypix: %f\n",psf->nlib,psf->dx,x,y,xpix,ypix);
//fprintf(stderr,"ix: %d iy: %d dx: %f dy: %f\n",ix,iy,dx,dy);

  nx = (int) rint(1./psf->dx);
  ix = (int) floor(x/psf->dx) + psf->n/2;
  dx = (x/psf->dx) + psf->n/2 - ix;
  iy = (int) floor(y/psf->dx) + psf->n/2;
  dy = (y/psf->dx) + psf->n/2 - iy;
  if (ix<0||iy<0||ix>(psf->n-nx-1)||iy>(psf->n-nx-1)) return(0.);
//fprintf(stderr,"ix: %d iy: %d dx: %f dy: %f\n",ix,iy,dx,dy);

  /* Bilinear interpolation */
  y1x1 = *(data+iy*n+ix);
  y1x2 = *(data+iy*n+ix+1);
  y2x1 = *(data+(iy+1)*n+ix);
  y2x2 = *(data+(iy+1)*n+ix+1);
  y1 = y1x1+dx*(y1x2-y1x1);
  y2 = y2x1+dx*(y2x2-y2x1);

  //dpdy1 = (y1x2-y1x1);
  //dpdy2 = (y2x2-y2x1);
  //*dpdy = -1.*((y2-y1));
  // To get derivatives wrt object center, we want negative of derivative
  //    wrt x, so flip difference here
  dpdy1 = (y1x1-y1x2)*psf->nlib;
  dpdy2 = (y2x1-y2x2)*psf->nlib;
  *dpdx = dpdy1 + dy*(dpdy2-dpdy1);
  *dpdy = (y1-y2)*psf->nlib;
//if (fabs(x)<2&&fabs(y)<2)
//fprintf(stderr,"psf: %f %f %d %d %d %f %f %f %f %f %f %f %f %f %f\n",
//                  x,y,ix,iy,n,y1x1,y1x2,y2x1,y2x2,dx,dy,y1,y2,*dpdx,*dpdy);

  return(y1+dy*(y2-y1));
}
double pixdist2(FRAME frame, double ra, double dec, double pra, double pdec, float maxrad) 
{
  double ddec,dra,dist2;
  ddec=fabs(dec-pdec);
  if (ddec>maxrad) return(1.e10);
  dra=fabs(ra-pra)*frame.cosdec;
  if (dra>maxrad) return(1.e10);
  dist2 =ddec*ddec+dra*dra;
  if (dist2 > maxrad*maxrad) return(1.e10);
  return(dist2);
}

float back(FRAME *frame, int ix, int iy, int verbose)
{
  float t,x0,y0;

  t = frame->back[0];
  frame->dback[0] = 1;
  if (NBACK>1) {
    //x0 = frame->nx/2;
    //y0 = frame->ny/2;
    x0 = frame->backx0;
    y0 = frame->backy0;
    t+= frame->back[1]*(ix-x0);
    t+= frame->back[2]*(iy-y0);
    frame->dback[1] = ix-x0;
    frame->dback[2] = iy-y0;
  }
  if (NBACK>3) {
    t+= frame->back[3]*(ix-x0)*(ix-x0);
    t+= frame->back[4]*(iy-y0)*(iy-y0);
    t+= frame->back[5]*(ix-x0)*(iy-y0);
    frame->dback[3] = (ix-x0)*(ix-x0);
    frame->dback[4] = (iy-y0)*(iy-y0);
    frame->dback[5] = (ix-x0)*(iy-y0);
  }
  return(t);
}
double mag(double inten)
{
  if (inten>0)
    return(-2.5*log10(inten)+25);
  else
    return(99.999);
}
double magerr(double inten, double intenerr)
{
  if (inten>0)
    return(1.086*(intenerr/inten));
  else
    return(9.999);
}

#ifdef LOGGAL
double gexp(double g)
{
  return(exp(g));
}

double dg(double g)
{
  return(exp(g));
}

double glog(double g)
{
  if (g>0)
    return(log(g));
  else
    return(-50.);
}
#else
double gexp(double g)
{
  return(g);
}

double dg(double g)
{
  return(1.);
}

double glog(double g)
{
  return(g);
}
#endif
