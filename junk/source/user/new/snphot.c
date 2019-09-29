#include "snphot.h"
#include "sntele.h"
#include "nrutil.h"
#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define D2R 3.14159/180.
#define MAXITER 1
#define MAXSTAR 500
#define MAXGAL 1
#define MAXVAR 1
#define MAXPAR 2+MAXFRAME*(NBACK+3)-3+NTELE*MAXFILT*(2*NDIST+1)+ \
               MAXSTAR*(2+MAXFILT)+MAXGAL*MAXSIZE*MAXSIZE*MAXFILT
#define TRUE 1
#define FALSE 0
void skytoxy(FRAME frame, double dec, double ra, double *x, double *y);
void xytosky(FRAME frame, double x, double y, double *dec, double *ra);
void dsolve(FRAME frame, double dec, double ra, double *dx, double *dy);
void getsncoord(FRAME frame, VAR *var);
void getscoord(FRAME frame, STAR *star);
void getgcoord(FRAME frame, GAL *gal, double, double);
double psf(PSF *psf, double x, double y, double *, double *);
void psfint(FRAME *, int smooth);
int usepix(FRAME, double, double, double , double, float fitrad);
float back(FRAME *frame, int ix, int iy);
int setpar(int set, PAR par[MAXPAR], 
           int *ncombo, int *nfilt, int filt[],
           FRAME *frame, int nframe, 
           STAR *star, int nstar, GAL *gal, int ngal, VAR *var, int nvar, int *converge);
float update_par(PAR *,double);
float update_pos(PAR *, double,double);
float update_inten(PAR *);
double mag(double);
double magerr(double,double);
double asinhmag(double,int);
double asinhmagerr(double,double,int);
void snphot(PAR par[], int oldpar, FRAME frame[], int nframe, 
            STAR star[], int nstar, 
            GAL gal[], int ngal, VAR *var, int nvar, int, int, int);

#undef MAIN
#ifdef MAIN
main()
{
  FRAME frame[1];
  double x, y,x2,y2;
  double ra, dec;
  double cd1_1, cd1_2, cd2_1, cd2_2, crval1, crval2;
  int i0,iframe, crpix1, crpix2;
  double dx[8],dy[8];

  frame[0].crpix1 = 1.02450000000000E+03; // Column Pixel Coordinate of Ref. Pixel   
  frame[0].crpix2 = 7.44500000000000E+02; // Row Pixel Coordinate of Ref. Pixel
  frame[0].crval1 = 4.66272516700000E+01; // RA at Reference Pixel              
  frame[0].crval2 = 1.15385290000000E+00; // DEC at Reference Pixel              
  frame[0].cd1_1 = 1.23021924141151E-09; // RA  degrees per column pixel          
  frame[0].cd1_2 = 1.10002179209069E-04; // RA  degrees per row pixel              
  frame[0].cd2_1 = 1.09989824218750E-04; // DEC degrees per column pixel            
  frame[0].cd2_2 = -4.1512096774093E-08; // DEC degrees per row pixel              

  frame[0].cd1_1 = 1.e-12; // RA  degrees per column pixel          
  frame[0].cd2_2 = 1.e-12; // DEC degrees per row pixel              
  frame[0].crval2 = 0.; // DEC degrees per row pixel              

  dec=1.1538; 
  ra=46.627;

  fprintf(stderr,"calling skytoxy\n");
  skytoxy (frame[0], dec, ra, &x, &y);
  fprintf(stderr,"x: %f  y:%f\n",x,y);

  iframe=0;
  cd1_1 = frame[iframe].cd1_1;
  cd1_2 = frame[iframe].cd1_2;
  cd2_1 = frame[iframe].cd2_1;
  cd2_2 = frame[iframe].cd2_2;
  crval1 = frame[iframe].crval1;
  crval2 = frame[iframe].crval2;
  crpix1 = frame[iframe].crpix1;
  crpix2 = frame[iframe].crpix2;

  dsolve(frame[0], dec, ra, dx, dy);
  fprintf(stderr,"analytical\n");
  fprintf(stderr," %f %f %f %f\n",dx[0],dx[1],dx[2],dx[3]);
  fprintf(stderr," %f %f %f %f\n",dy[0],dy[1],dy[2],dy[3]);

  frame[0].cd1_1*=1.01;
  skytoxy (frame[0], dec, ra, &x2, &y2);
  fprintf(stderr,"numerical:\n %f %f %f %f\n",x2,y2,(x2-x)/0.01/cd1_1,(y2-y)/0.01/cd1_1);
  frame[0].cd1_1/=1.01;
  frame[0].cd1_2*=1.01;
  skytoxy (frame[0], dec, ra, &x2, &y2);
  fprintf(stderr," %f %f %f %f\n",x2,y2,(x2-x)/0.01/cd1_2,(y2-y)/0.01/cd1_2);
  frame[0].cd1_2/=1.01;
  frame[0].cd2_1*=1.01;
  skytoxy (frame[0], dec, ra, &x2, &y2);
  fprintf(stderr," %f %f %f %f\n",x2,y2,(x2-x)/0.01/cd2_1,(y2-y)/0.01/cd2_1);
  frame[0].cd2_1/=1.01;
  frame[0].cd2_2*=1.01;
  skytoxy (frame[0], dec, ra, &x2, &y2);
  fprintf(stderr," %f %f %f %f\n",x2,y2,(x2-x)/0.01/cd2_2,(y2-y)/0.01/cd2_2);
  frame[0].cd2_2/=1.01;

fprintf(stderr,"%f %f\n",1./cd1_2, 1./cd2_1);
}
#endif

void dosnphot_(int *nim, float **locframe, int *nx, int *ny, double *crval1, double *crval2, double *cd1_1, double *cd1_2, double *cd2_1, double *cd2_2, int *crpix1, int *crpix2, int *cnpix1, int *cnpix2, int *tele, int *filt, double *back, double *jd, double *gain, double *rn, double *lowbad, double *highbad, float **locpsf, int *psfn, double *psfdx, int *niter, int *maxvar, int *maxgal, int *maxstar, float *dmax, int *backgrid, int *sub, char *starfile, char *snfile, char *parfile, int ns, int nsn, int np)
{
  FRAME frame[MAXFRAME];
  STAR star[MAXSTAR];
  GAL gal[MAXGAL];
  VAR var[MAXVAR];
  PAR par[MAXPAR];
  FILE *fp;
#define MAXLINE 200
  char *line = NULL;
  char out[20];
  int id, i, l, ifilt, ivar, istar, nstar, nline, igx, igy, ngal, nvar;
  int iframe, nframe, iback, smooth, ifile, oldpar;
  float x, y, ddec, dra;
  double scale, jdmax;
  PSF *psf;

  nframe = *nim;
  for (i=0;i<nframe;i++) {
   frame[i].data = *(locframe+i);
   frame[i].nx = *(nx+i);
   frame[i].ny = *(ny+i);
   frame[i].crval1 = *(crval1+i);
   frame[i].crval2 = *(crval2+i);
   frame[i].cd1_1 = *(cd1_1+i);
   frame[i].cd1_2 = *(cd1_2+i);
   frame[i].cd2_1 = *(cd2_1+i);
   frame[i].cd2_2 = *(cd2_2+i);
   frame[i].crpix1 = *(crpix1+i);
   frame[i].crpix2 = *(crpix2+i);
   frame[i].cnpix1 = *(cnpix1+i);
   frame[i].cnpix2 = *(cnpix2+i);
   frame[i].tele = *(tele+i);
   frame[i].filt = *(filt+i);
   frame[i].back[0] = *(back+i);
   for (iback=1;iback<NBACK;iback++) frame[i].back[iback] = 0.;
   frame[i].jd = *(jd+i);
   frame[i].gain = *(gain+i);
   frame[i].rn = *(rn+i);
   frame[i].lowbad = *(lowbad+i);
   frame[i].highbad = *(highbad+i);
   fprintf(stderr,"%f %f %f %f %f %f %f %f %d %d %d %d %f %f\n",
      frame[i].crval1, frame[i].crval2, frame[i].cd1_1, frame[i].cd1_2,
      frame[i].cd2_1, frame[i].cd2_2, frame[i].crpix1, frame[i].crpix2,
      frame[i].cnpix1, frame[i].cnpix2, frame[i].tele, frame[i].filt,
      frame[i].back[0], frame[i].jd);
   frame[i].psf = (PSF *)malloc(sizeof(PSF));
   psf = frame[i].psf;
   psf->data = *(locpsf+i);
   psf->dx = *(psfdx+i);
   psf->nlib = (int)rint(1./psf->dx);
   psf->n = *(psfn+i);

   scale = sqrt(frame[i].cd1_1*frame[i].cd1_1+frame[i].cd1_2*frame[i].cd1_2);
   smooth = (int)rint(1.5/(scale*3600.));
   smooth = (smooth%2==1) ? smooth : smooth+1;
   psfint(&frame[i],smooth);
  }

  if (snfile[0] != 0) {
    fp = fopen(snfile,"r");
    if (fp==NULL) {
      fprintf(stderr,"Error opening snfile: %s\n",snfile);
    }
    getline(&line,&nline,fp);
    sscanf(line,"%d%lf%lf%lf",&var[0].id,&var[0].ra,&var[0].dec,&jdmax);
    var[0].jdmin=jdmax-20;
    var[0].jdmax=jdmax+50;
    fclose(fp);
    nvar = 1;
    fprintf(stderr,"SN: %f %f %f %f\n",var[0].ra,var[0].dec,var[0].jdmin,var[0].jdmax);
    gal[0].ra=var[0].ra; 
    gal[0].dec=var[0].dec; 
    gal[0].nsize=5;
    gal[0].dx=1./3600.;
    ngal = 1;
  }

  nstar=0;
  *dmax /= 3600.;
  if (starfile[0] != 0) {
    fp = fopen(starfile,"r");
    if (fp==NULL) {
      fprintf(stderr,"Error opening file: %s\n",starfile);
    }
    while (getline(&line,&nline,fp) != -1 && nstar<MAXSTAR ) {
      sscanf(line,"%d%lf%lf%lf%lf%lf%lf%lf",&star[nstar].id,&star[nstar].ra,&star[nstar].dec,&star[nstar].mag[0],&star[nstar].mag[1],&star[nstar].mag[2],&star[nstar].mag[3],&star[nstar].mag[4]);
      ddec=star[nstar].dec-var[0].dec;
      if (fabs(ddec) < *dmax) {
        dra=(star[nstar].ra-var[0].ra)*cos(var[0].dec*D2R);
        if (fabs(dra) < *dmax) {
          if ((dra*dra+ddec*ddec)<((*dmax)*(*dmax))&&nstar<*maxstar) {
    fprintf(stderr,"nstar: %d %f %f\n",nstar, dra, ddec);
            nstar++;
          }
        }
      }
    }
    fclose(fp);
  }

  nvar=*maxvar;
  ngal=*maxgal;
  
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

fprintf(stderr,"*sub: %d\n",*sub);
  snphot(par,oldpar,frame,nframe,star,nstar,gal,ngal,var,nvar,*niter,*backgrid,*sub);

fprintf(stderr,"*sub: %d\n",*sub);
  if (*sub) return;

  for (i=strlen(snfile)-1;i>=0;i--) if (snfile[i] == '.') l=i;
  strncpy(out,snfile,l);
  out[l] = 0;


  fp=fopen(strcat(out,".stars"),"w");
  for (istar=0;istar<nstar;istar++) {
    fprintf(fp,"%6d %12.6f %12.6f",star[istar].id,star[istar].ra,star[istar].dec);
    for (ifilt=0;ifilt<MAXFILT;ifilt++) {
      fprintf(fp," %8.3f %8.3f %8.3f %8.3f %8.3f",star[istar].mag[ifilt],
              mag(star[istar].inten[ifilt]),
              magerr(star[istar].inten[ifilt],star[istar].intenerr[ifilt]),
              star[istar].chi[ifilt],star[istar].sumwt[ifilt]);
    }
    fprintf(fp,"\n");
  }
  fclose(fp);

  strncpy(out,snfile,l);
  out[l] = 0;
  fp=fopen(strcat(out,".phot"),"w");
  for (ivar=0;ivar<nvar;ivar++) {
    fprintf(fp,"%6d %12.6f %12.6f\n",var[ivar].id,var[ivar].ra,var[ivar].dec);
    for (iframe=0;iframe<nframe;iframe++) {
      fprintf(fp," %8.2f %1d %1d %8.3f %8.2f %8.3f %8.3f %8.2f %10.2f %10.2f\n",
              frame[iframe].jd,frame[iframe].filt,
              frame[iframe].tele,frame[iframe].zeropt,frame[iframe].back[0],
              mag(var[ivar].inten[iframe]),
              magerr(var[ivar].inten[iframe],var[ivar].intenerr[iframe]),
              var[ivar].chi[iframe],
              var[ivar].inten[iframe],
              var[ivar].intenerr[iframe]
              );
    }
    fprintf(fp,"\n");
  }
  fclose(fp);

  strncpy(out,snfile,l);
  out[l] = 0;
  fp=fopen(strcat(out,".frame"),"w");
  for (iframe=0;iframe<nframe;iframe++) {
    fprintf(fp,"%8.2f %1d %1d %8.3f %8.2f %8.2f %8.2f \n",frame[iframe].jd,frame[iframe].filt,frame[iframe].tele,frame[iframe].zeropt,frame[iframe].back[0],frame[iframe].crpix1,frame[iframe].crpix2);
  }
  fclose(fp);

  strncpy(out,snfile,l);
  out[l] = 0;
  ifile = creat(strcat(out,".par"),0644);
  write(ifile,&par,MAXPAR*sizeof(PAR));
  close(ifile);

  return;
}

void snphot(PAR par[], int oldpar, FRAME frame[], int nframe, 
            STAR star[], int nstar, 
            GAL gal[], int ngal, 
            VAR var[], int nvar, 
            int maxiter, int backgrid, int sub)
{
  int npar, redo=1, used, objpix, niter=0, ncombo, ipar, jpar, nfit, nskipped;
  int iframe, jframe, ix, iy, istar, igal, ivar, i0, n2, dummy; 
  int ifilt, jfilt, filts[MAXFILT], nfilt, iback, itele, istat, lockinten, converge;
  float **alpha,**alphared,**alpha0,*beta0,*err,*betared,*betatmp,det;
  int *indx, tfit, itfilt, tfilt, tframe, icombo;
  float beta[MAXPAR], dpar[MAXPAR];
  float data;
  double mod, modvar, modstar, modgal, modsky;
  double p, p2, tot, scale, dpdx, dpdy, diff, sig2, x, y, tmp;
  double trans;
  double dxdd, dxdr, dydd, dydr, dx[4], dy[4];
  double cd1_1, cd1_2, cd2_1, cd2_2, den, crval1, crval2;
  double ra, dec, pra, pdec, gra, gdec, rng, gain;
  int crpix1, crpix2;
  int i, j, ipix, igx, igy, skip[MAXSTAR], skipvar[MAXFRAME];

  extern int con_[5];

  fprintf(stderr,"SNPHOT nframe: %d nvar: %d  ngal: %d nstar: %d\n",nframe,nvar,ngal,nstar);
  /* initialize telescope information */
  telinit();

  /* initialize frame zeropt based on telescope information */
  for (iframe=0; iframe<nframe; iframe++) {
    itele=frame[iframe].tele; 
    ifilt=frame[iframe].filt; 
    frame[iframe].cosdec = cos(frame[iframe].crval2*D2R);
    frame[iframe].zeropt = 1.;
    frame[iframe].first = 1;
    for (jframe=0;jframe<iframe;jframe++) 
      if (frame[jframe].filt == frame[iframe].filt) frame[iframe].first = 0;
    for (ivar=0; ivar<nvar; ivar++) {
      if (frame[iframe].jd >= var[ivar].jdmin&&frame[iframe].jd <= var[ivar].jdmax) 
        var[ivar].inten[iframe] = 100.*frame[0].zeropt;
      else
        var[ivar].inten[iframe] = 0.;
    }
  }

  /* initialize star brightnesses based on input magnitudes */
  for (istar=0; istar<nstar; istar++) {
    for (ifilt=0; ifilt<MAXFILT; ifilt++) {
       star[istar].inten[ifilt] = 
        star[istar].mag[ifilt] < 30 ? 
          pow(10.,-0.4*(star[istar].mag[ifilt]-25.)) : 1.;
    }
  }
  lockinten = 0;

  /* initialize galaxy brightness */
  for (igal=0; igal<ngal; igal++) {
    for (igx=0;igx<gal[igal].nsize;igx++)
      for (igy=0;igy<gal[igal].nsize;igy++)
        for (i=0;i<MAXFILT;i++) gal[igal].inten[igx][igy][i] = 30.*frame[0].zeropt;
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

  /* Main iteration loop starts here */
  while (redo) {

    for (ipar=0;ipar<npar;ipar++) {
      beta[ipar] = 0;
      for (jpar=0;jpar<npar;jpar++) alpha[ipar][jpar] = 0;
    }
    for (istar=0; istar<nstar; istar++) {
      for (i=0;i<MAXFILT;i++) {
        star[istar].chi[i] = 0.;
        star[istar].sumwt[i] = 0.;
      }
    }
    for (ivar=0; ivar<nvar; ivar++) {
      for (iframe=0; iframe<nframe; iframe++) {
        var[ivar].chi[iframe] = 0;
        var[ivar].sumwt[iframe] = 0;
      }
    }

/* Load up data-model and derivatives */
    for (iframe=0; iframe<nframe; iframe++) {

      if (con_[2]) return;

      jfilt = frame[iframe].filt;
      for (i=0;i<nfilt;i++) if (filts[i] == jfilt) ifilt=i;
      itele = frame[iframe].tele;
      cd1_1 = frame[iframe].cd1_1;
      cd1_2 = frame[iframe].cd1_2;
      cd2_1 = frame[iframe].cd2_1;
      cd2_2 = frame[iframe].cd2_2;
      den=cd1_1*cd2_2-cd1_2*cd2_1;
      crval1 = frame[iframe].crval1;
      crval2 = frame[iframe].crval2;
      crpix1 = frame[iframe].crpix1;
      crpix2 = frame[iframe].crpix2;
      gain = frame[iframe].gain;
      rng = frame[iframe].rn*frame[iframe].rn/gain/gain;
      scale = teles[itele].scale[jfilt];
      tfit = teles[itele].tfit[jfilt];
      trans = teles[itele].trans[jfilt];
      tfilt = teles[itele].tfilt[jfilt];
      for (i=0;i<nfilt;i++) if (filts[i] == tfilt) itfilt=i;
      icombo = frame[iframe].combo;
      tframe=-1;
      for (i=0;i<nframe;i++)
        if (fabs(frame[iframe].jd-frame[i].jd)<0.1 && frame[i].filt==jfilt) tframe = i;

     /* get current positions of objects in frame */
      for (ivar=0; ivar<nstar; ivar++) {
        getsncoord(frame[iframe],&var[ivar]);
/*
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
      for (istar=0; istar<nstar; istar++) {
        getscoord(frame[iframe],&star[istar]);
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
        //fprintf(stderr,"star: %d %d %f %f %f\n",istar, iframe, 
        //          star[istar].x, star[istar].y, star[istar].mag[2]);
      }
      for (igal=0; igal<ngal; igal++) {
        // Determine upper and lower pixel values for this object
        gal[igal].ixmin = 32767;
        gal[igal].iymin = 32767;
        gal[igal].ixmax = -32767;
        gal[igal].iymax = -32767;
        for (i=-1;i<=1;i+=2) 
        for (igx=0;igx<=gal[igal].nsize;igx+=gal[igal].nsize) {
          gra=gal[igal].ra+(igx-gal[igal].nsize/2)*
                          gal[igal].dx/frame[iframe].cosdec+
                          i*teles[itele].fitrad/3600.;
          for (igy=0;igy<=gal[igal].nsize;igy+=gal[igal].nsize) {
            gdec=gal[igal].dec+(igy-gal[igal].nsize/2)*gal[igal].dx+
                          i*teles[itele].fitrad/3600.;
fprintf(stderr,"gra: %f  gdec: %f %d %d %d\n",gra,gdec,i,igx,igy);
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
fprintf(stderr,"galaxy corners: %d %d %d %d\n",
      gal[igal].ixmin, gal[igal].ixmax, gal[igal].iymin,gal[igal].iymax);
//fprintf(stderr,"enter number...");
//scanf("%d",&i);
        // Set up quantities for color terms
        for (igx=0;igx<gal[igal].nsize;igx++) {
          for (igy=0;igy<gal[igal].nsize;igy++) {
            if (gal[igal].inten[igx][igy][tfilt]>0) {
              gal[igal].color[igx][igy] = 
              gal[igal].inten[igx][igy][jfilt]/gal[igal].inten[igx][igy][tfilt];
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
      fprintf(stderr,"niter: %d iframe: %d icombo: %d ifilt: %d jfilt: %d tfilt: %d itfilt: %d tfit: %d trans: %f backgrid: %d ix: ",niter,iframe,icombo,ifilt,jfilt,tfilt,itfilt,tfit,trans,backgrid);
      for (ix=0; ix<frame[iframe].nx; ix++) {
        if (ix%100==0) fprintf(stderr," %d ",ix);
        for (iy=0; iy<frame[iframe].ny; iy++) {

         data = *(frame[iframe].data+iy*frame[iframe].nx+ix);
         if (data>frame[iframe].lowbad&&data<frame[iframe].highbad) {

          for (ipar=0;ipar<npar;ipar++) dpar[ipar] = 0;
          used = FALSE;
          objpix = FALSE;
          mod = modvar = modgal = modstar = modsky = 0;
          dxdd = -cd1_2/den;
          dxdr = cd2_2/den*frame[iframe].cosdec;
          dydd = cd1_1/den;
          dydr = -cd2_1/den*frame[iframe].cosdec;
          xytosky(frame[iframe], (double) ix, (double) iy, &pdec, &pra);

          /* SN */
          for (ivar=0; ivar<nvar; ivar++) {
           skipvar[ivar] = 1;
           if (frame[iframe].jd >= var[ivar].jdmin&&frame[iframe].jd <= var[ivar].jdmax) {
            if (usepix(frame[iframe], var[ivar].ra,var[ivar].dec,pra,pdec,teles[itele].fitrad)) {
              skipvar[ivar] = 0;
              used = TRUE;
              objpix = TRUE;
              p = psf(frame[iframe].psf,ix-var[ivar].x,iy-var[ivar].y,&dpdx,&dpdy);
              tot = var[ivar].inten[iframe] * frame[iframe].zeropt * scale;
              mod+= tot*p;
              modvar+= tot*p;
              /* Derivs wrt SN position */
              dpar[0] += tot*(dpdx*dxdr+dpdy*dydr);
              dpar[1] += tot*(dpdx*dxdd+dpdy*dydd);
              /* Derivs wrt SN intensity */
              dpar[2+ivar*nvar+iframe] += frame[iframe].zeropt*p*scale;

              if (!frame[iframe].first) {
                /* Deriv wrt frame zeropt */
                dpar[2+nvar*nframe+iframe] += var[ivar].inten[iframe] * p * scale;
              }
              if (iframe>0) {
                /* Derivs wrt frame position (crpix)*/
                i0 = 1+nvar*nframe+nframe+iframe*2;
                dpar[i0+1] += tot*dpdx;
                dpar[i0+2] += tot*dpdy;
                /* Derivs wrt linear astrometric parameters */
                x=var[ivar].x;
                y=var[ivar].y;
                dec=var[ivar].dec;
                ra=var[ivar].ra;
                //dsolve(frame[iframe], dec, ra, dx, dy);
                //i0=1+nvar*nframe+nframe+nframe*2+iframe*4;
                //dpar[i0+1] += tot*(dpdx*dx[0]+dpdy*dy[0]);
                //dpar[i0+2] += tot*(dpdx*dx[1]+dpdy*dy[1]);
                //dpar[i0+3] += tot*(dpdx*dx[2]+dpdy*dy[2]);
                //dpar[i0+4] += tot*(dpdx*dx[3]+dpdy*dy[3]);
              }
            }
           }
          }

          /* Stars */
          for (istar=0; istar<nstar; istar++) {
            skip[istar] = 1;
            if (usepix(frame[iframe],star[istar].ra,star[istar].dec,pra,pdec,teles[itele].fitrad)) {
              used = TRUE;
              skip[istar] = 0;
              objpix = TRUE;
              p = psf(frame[iframe].psf,ix-star[istar].x,iy-star[istar].y,&dpdx,&dpdy);
              tot = star[istar].inten[jfilt]*frame[iframe].zeropt*scale;
              if (trans!=0) tot *= star[istar].pow;
              mod+= tot*p;
              modstar+= tot*p;
              if (!frame[iframe].first||lockinten) {
                /* Deriv wrt frame zeropt */
                dpar[2+nvar*nframe+iframe] += star[istar].inten[jfilt] * p * scale;
              }
              if (iframe>0) {
                i0=1+nvar*nframe+nframe+iframe*2;
                /* Derivs wrt frame position */
                dpar[i0+1] += tot*dpdx;
                dpar[i0+2] += tot*dpdy;
                /* Derivs wrt linear astrometric parameters */
                //x=star[istar].x;
                //y=star[istar].y;
                dec=star[istar].dec;
                ra=star[istar].ra;
                dsolve(frame[iframe], dec, ra, dx, dy);
                i0=1+nvar*nframe+nframe+nframe*2+iframe*4;
                dpar[i0+1] += tot*(dpdx*dx[0]+dpdy*dy[0]);
                dpar[i0+2] += tot*(dpdx*dx[1]+dpdy*dy[1]);
                dpar[i0+3] += tot*(dpdx*dx[2]+dpdy*dy[2]);
                dpar[i0+4] += tot*(dpdx*dx[3]+dpdy*dy[3]);
              }
              /* Derivs wrt star position and brightness*/
              i0=1+nvar*nframe+nframe*(7+NBACK);
              i0+=istar*(2+nfilt);
              dpar[i0+1] += tot*(dpdx*dxdr+dpdy*dydr);
              dpar[i0+2] += tot*(dpdx*dxdd+dpdy*dydd);
              if (!lockinten) {
                if (trans == 0)
                  dpar[i0+3+ifilt] += frame[iframe].zeropt*p*scale;
                else {
                  dpar[i0+3+ifilt] += frame[iframe].zeropt*p*scale*
                     (1+trans)*star[istar].pow;
                  dpar[i0+3+itfilt] += -1* frame[iframe].zeropt*p*scale*
                      star[istar].pow*star[istar].color;
                }
                if (tfit) {
                  i0 = 1+nvar*nframe+nframe*(7+NBACK)+nstar*(2+nfilt);
                  if (star[istar].color>0) 
                      dpar[i0+icombo] += tot* log(star[istar].color);
                }

              }
            }
          }

          /* Galaxy  */
          for (igal=0; igal<ngal; igal++) {
           if (ix>=gal[igal].ixmin&&ix<=gal[igal].ixmax&&
               iy>=gal[igal].iymin&&iy<=gal[igal].iymax) {
            ipix=0;
            for (igx=0;igx<gal[igal].nsize;igx++) {
              gra=gal[igal].ra+(igx-gal[igal].nsize/2)*
                                gal[igal].dx/frame[iframe].cosdec;
              for (igy=0;igy<gal[igal].nsize;igy++) {
                gdec=gal[igal].dec+(igy-gal[igal].nsize/2)*gal[igal].dx;
                if (usepix(frame[iframe],gra,gdec,pra,pdec,teles[itele].fitrad)&&
                    gal[igal].inten[igx][igy][jfilt]>0) {
                  used = TRUE;
                  objpix = TRUE;
                  getgcoord(frame[iframe],&gal[igal],gra,gdec);
                  p=psf(frame[iframe].psfint,ix-gal[igal].x,iy-gal[igal].y,&dpdx,&dpdy);
                  tot= gal[igal].inten[igx][igy][jfilt]*frame[iframe].zeropt*scale;
                  if (trans!=0) tot *= gal[igal].pow[igx][igy];
                  mod+= tot*p;
                  modgal+= tot*p;
                  if (!frame[iframe].first) {
                    /* Deriv wrt frame zeropt */
                    dpar[2+nvar*nframe+iframe] += 
                            gal[igal].inten[igx][igy][jfilt] * p * scale;
                  }
                  if (iframe>0) {
                    i0=1+nvar*nframe+nframe+iframe*2;
                    /* Derivs wrt frame position */
                    dpar[i0+1] += tot*dpdx;
                    dpar[i0+2] += tot*dpdy;
                    //x=gal[igal].x;
                    //y=gal[igal].y;
                    //dec=gdec;
                    //ra=gra;
                    //dsolve(frame[iframe], dec, ra, dx, dy);
                    //i0=1+nvar*nframe+nframe+nframe*2+iframe*4;
                    //dpar[i0+1] += tot*(dpdx*dx[0]+dpdy*dy[0]);
                    //dpar[i0+2] += tot*(dpdx*dx[1]+dpdy*dy[1]);
                    //dpar[i0+3] += tot*(dpdx*dx[2]+dpdy*dy[2]);
                    //dpar[i0+4] += tot*(dpdx*dx[3]+dpdy*dy[3]);
                  }
                  i0 = 2 + nvar*nframe + nframe*(7+NBACK) + nstar*(2+nfilt) + ncombo;
                  for (i=0;i<igal;i++) i0 += gal[i].nsize*gal[i].nsize*nfilt;

                  if (trans == 0)
                    dpar[i0+ipix*nfilt+ifilt] += frame[iframe].zeropt*p*scale;
                  else {
                    dpar[i0+ipix*nfilt+ifilt] += frame[iframe].zeropt*p*scale*
                       (1+trans)*gal[igal].pow[igx][igy];
                    dpar[i0+ipix*nfilt+itfilt] += -1* frame[iframe].zeropt*p*scale*
                        gal[igal].pow[igx][igy]*gal[igal].color[igx][igy];
                  }
                  if (tfit) {
                    i0 = 1+nvar*nframe+nframe*(7+NBACK)+nstar*(2+nfilt);
                    if (gal[igal].color[igx][igy]>0) 
                        dpar[i0+icombo] += tot* log(gal[igal].color[igx][igy]);
                  }
                }
                ipix++;
              }
            }
           }
          }

          /* Background */
          mod += back(&frame[iframe],ix,iy);
          modsky += back(&frame[iframe],ix,iy);
          if (!objpix&&ix%backgrid==0&&iy%backgrid==0) {
            used = TRUE;
            i0 = 2+nvar*nframe+nframe*7;
            for (iback=0; iback<NBACK; iback++) {
              dpar[i0+iframe*NBACK+iback] += frame[iframe].dback[iback];
            }
          }

          // On final iteration, replace data with data-model
          if (niter==maxiter||sub) {
            if (niter==maxiter||sub&1) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modvar;
            if (niter==maxiter||sub&2) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modstar;
            if (niter==maxiter||sub&4) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modgal;
            if (niter==maxiter||sub&8) 
              *(frame[iframe].data+iy*frame[iframe].nx+ix) -= modsky;
          }
 
          // Load this pixel into the solution matrices
          if (used) {
            diff = data - mod;
            sig2 = data/gain + rng;
            if (!sub) {
              for (ipar=0; ipar<npar; ipar++) {
                if (dpar[ipar] != 0) {
                  tmp = dpar[ipar] / sig2;
                  beta[ipar] += diff * tmp ;
                  for (jpar=0; jpar<=ipar; jpar++) {
                    if (dpar[jpar] != 0)
                      alpha[ipar][jpar] += tmp*dpar[jpar];
                  }
                }
              }
            }
            // Add into goodness-of-fit measurement for each star/var
            for (istar=0;istar<nstar;istar++) {
              if (!skip[istar]) {
                star[istar].chi[jfilt] += fabs(diff)/sqrt(sig2);
                star[istar].sumwt[jfilt] += 1.;
              }
            }
            for (ivar=0;ivar<nvar;ivar++) {
              if (!skipvar[ivar]) {
                var[ivar].chi[iframe] += fabs(diff)/sqrt(sig2);
                var[ivar].sumwt[iframe] += 1.;
              }
            }
          } else
            nskipped++;
         } else  /* if good pixel */
           nskipped++;
        } /* end of loop over rows */
      } /* end of loop over cols */      
      fprintf(stderr," skipped: %d of %d\n",nskipped,frame[iframe].nx*frame[iframe].ny);
    }  /* end of loop over frames */
  
    /* Calculate goodness of fit parameter for each star for this iteration*/
    for (istar=0;istar<nstar;istar++) {
      for (i=0;i<nfilt;i++) {
        j = filts[i];
        if (star[istar].sumwt[j]>3) 
          star[istar].chi[j] *= 1.253 *
            sqrt(1./(star[istar].sumwt[j]*(star[istar].sumwt[j]-3)));
      }
    }
    for (ivar=0;ivar<nvar;ivar++) {
      for (iframe=0;iframe<nframe;iframe++) {
        if (var[ivar].sumwt[iframe]>3) 
          var[ivar].chi[iframe] *= 1.253 *
            sqrt(1./(var[ivar].sumwt[iframe]*(var[ivar].sumwt[iframe]-3)));
      }
    }

    if (!sub&&niter!=maxiter) {
      /* Fill in symmetric part of matrix */
      nfit=0;
      for (ipar=0; ipar<npar; ipar++) { 
        if (beta[ipar]!=0) {
          betared[nfit+1] = beta[ipar];
          n2=1;
          for (jpar=0; jpar<=ipar; jpar++) { 
            if (beta[jpar]!=0) 
              alphared[nfit+1][n2++] = alpha[ipar][jpar];
          }
          nfit++;
        }
      }
      fprintf(stderr,"npar: %d nfit: %d\n",npar,nfit);
      for (ipar=1; ipar<=nfit; ipar++) { 
        for (jpar=ipar+1; jpar<=nfit; jpar++) 
          alphared[ipar][jpar] = alphared[jpar][ipar];
      }
  
  /* Invert matrix and get parameter corrections */
  
  #undef PRINT
  #ifdef PRINT
      fprintf(stderr,"betared: \n");
      for (ipar=1; ipar<=nfit; ipar++) fprintf(stderr,"%8.2e\n",betared[ipar]);
      fprintf(stderr,"alphared: \n");
      for (ipar=1; ipar<=nfit; ipar++) {
  //      for (jpar=1; jpar<=nfit; jpar++) fprintf(stderr,"%8.2e ",alphared[ipar][jpar]);
  //      fprintf(stderr,"\n");
      }
     printf("enter number to continue: ");
     scanf("%d",&ipar);
      //pause();
  #endif
  
      /* copy into beta0, alpha0 for use later to refine solution */
      for (i=1;i<=nfit;i++) {
        beta0[i] = betared[i];
        for (j=1;j<=nfit;j++) alpha0[i][j] = alphared[i][j];
      }
  
      /* Solve equations to get current solution */
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
  
  /* Invert matrix to get parameter errors -- diagonal elements only */
     fprintf(stderr,"inverse matrix determination: %d\n",nfit);
     for (j=1;j<=nfit;j++) {
       for (i=1;i<=nfit;i++) betatmp[i] = 0.;
       betatmp[j] = 1.;
       lubksb(alphared,nfit,indx,betatmp);
       err[j] = sqrt(betatmp[j]);
     }
  
      nfit=0;
      for (ipar=0; ipar<npar; ipar++) { 
        par[ipar].del = 0;
        if (beta[ipar]!=0) {
          par[ipar].del = betared[nfit+1];
          par[ipar].err = err[nfit+1];
          nfit++;
        }
        par[ipar].oldval = par[ipar].val;
      }
  
  /* Correct parameters and check for convergence */
      npar = setpar(0, par, &ncombo, &nfilt, filts,
                  frame, nframe, star, nstar, gal, ngal, var, nvar, &converge);
      npar = setpar(1, par, &ncombo, &nfilt, filts,
                  frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);
  
      fprintf(stderr," ipar old         delta       new\n");
      for (ipar=0; ipar<npar; ipar++) { 
        if (ipar==0) fprintf(stderr,"SN posn\n");
        if (ipar==2) fprintf(stderr,"SN inten\n");
        if (ipar==2+nvar*nframe) fprintf(stderr," nframe zeropt\n");
        if (ipar==2+nvar*nframe+nframe) fprintf(stderr," nframe central posns\n");
        if (ipar==2+nvar*nframe+nframe*3) fprintf(stderr," nframe 4 linear params\n");
        if (ipar==2+nvar*nframe+nframe*7) fprintf(stderr," nframe backgrounds\n");
        if (ipar==2+nvar*nframe+nframe*(7+NBACK)) fprintf(stderr," star params\n");
        if (ipar==2+nvar*nframe+nframe*(7+NBACK)+nstar*(nfilt+2)) fprintf(stderr," transform\n");
        if (ipar<500) fprintf(stderr," %d %e %e %e %e %d\n",ipar, par[ipar].oldval, par[ipar].del, par[ipar].val, par[ipar].err, par[ipar].converge);
      }
  
      niter++;
    }
    if ((niter>=maxiter)||sub) redo=FALSE;
    if (converge) sub = 15;
    fprintf(stderr,"niter: %d maxiter: %d redo:%d converge: %d sub: %d\n",niter,maxiter,redo,converge,sub);
  }

  npar = setpar(0, par, &ncombo, &nfilt, filts,
                frame, nframe, star, nstar, gal, ngal, var, nvar, &dummy);

  free_matrix(alpha,0,npar,0,npar);
  free_matrix(alphared,1,npar,1,npar);
  free_vector(betared,1,npar);
  free_vector(err,1,npar);
  free_ivector(indx,1,npar);

  for (istar=0; istar<nstar; istar++){
    fprintf(stderr,"star: %d\n",istar);
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
}

int setpar(int set, PAR par[MAXPAR], 
           int *ncombo, int *nfilt, int filts[], FRAME *frame, int nframe, 
           STAR *star, int nstar, GAL *gal, int ngal, VAR *var, int nvar, 
           int *converge)
{
  int npar, itele, ntele=0, ivar, i;
  int iframe, jframe, ifilt, jfilt, istar, iback, idist, ix, iy, igal, new;
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
    2 star posns
    nfilt star intensities
  NGAL *
    nsize*nsize*nfilt intensities
  NCOMBO *  (NCOMBO is unique number of tele/filter combinations)
    NDIST xdistortions
    NDIST ydistortions
    photometric xform coef
*/
  npar=0;
  /* SN position and intensities */

  if (set) {
    par[npar++].val = var[0].ra;
    par[npar++].val = var[0].dec;
  } else {
    var[0].ra = update_pos(&par[npar],0.5/3600.,0.1/3600.);
    npar++;
    var[0].dec = update_pos(&par[npar],0.5/3600.,0.1/3600.);
    npar++;
  }

  for (ivar=0; ivar<nvar; ivar++) {
    for (iframe=0; iframe<nframe; iframe++) {
      if (set)
        par[npar++].val = var[ivar].inten[iframe];
      else {
        var[ivar].inten[iframe] = update_inten(&par[npar]);
        var[ivar].intenerr[iframe] = par[npar].err;
        npar++;
      }
    }
  }

  for (iframe=0; iframe<nframe; iframe++) {
    if (set)
      par[npar++].val = frame[iframe].zeropt;
    else {
      frame[iframe].zeropt = update_inten(&par[npar]);
      npar++;
    }
  }
  for (iframe=0; iframe<nframe; iframe++) {
    if (set) {
      par[npar++].val = frame[iframe].crpix1;
      par[npar++].val = frame[iframe].crpix2;
    } else {
      //frame[iframe].crpix1 = update_pos(par[npar],dpar[npar],0.5,0.1,converge);
      frame[iframe].crpix1 = update_par(&par[npar],0.05);
      npar++;
      //frame[iframe].crpix2 = update_pos(par[npar],dpar[npar],0.5,0.1,converge);
      frame[iframe].crpix2= update_par(&par[npar],0.05);
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
      frame[iframe].cd1_1 = update_par(&par[npar],1.e-7);
      npar++;
      frame[iframe].cd1_2 = update_par(&par[npar],1.e-7);
      npar++;
      frame[iframe].cd2_1 = update_par(&par[npar],1.e-7);
      npar++;
      frame[iframe].cd2_2 = update_par(&par[npar],1.e-7);
      npar++;
    }
  }
    /* background parameters for this frame */
  for (iframe=0; iframe<nframe; iframe++) {
    for (iback=0; iback<NBACK; iback++) {
      if (set) 
        par[npar++].val = frame[iframe].back[iback];
      else {
        frame[iframe].back[iback] = update_par(&par[npar],0.);
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
    } else {
      star[istar].ra = update_pos(&par[npar],0.5/3600.,0.1/3600.);
      npar++;
      star[istar].dec = update_pos(&par[npar],0.5/3600.,0.1/3600.);
      npar++;
    }
    for (ifilt=0; ifilt<*nfilt; ifilt++) {
      jfilt = filts[ifilt];
      if (set) {
        par[npar++].val = star[istar].inten[jfilt];
      } else {
        star[istar].inten[jfilt] = update_inten(&par[npar]);
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
    itele = frame[iframe].tele;
    for (jframe=0; jframe<iframe; jframe++) {
       if (ifilt == frame[jframe].filt &&
           itele == frame[jframe].tele) {
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
        teles[itele].trans[ifilt] = update_par(&par[npar],0.005);
        npar++;
      }
      frame[iframe].combo = *ncombo;
    }
  }

  for (i=1; i<=*ncombo; i++) {
    itele=combotel[i];
    ifilt=combofilt[i];
    for (idist=0; idist<NDIST; idist++) 
      if (set)
        par[npar++].val = teles[itele].xdist[idist][ifilt];
      else {
        teles[itele].xdist[idist][ifilt] = update_par(&par[npar],0.);
        npar++;
      }
  }

  /* Galaxy intensities */
  for (igal=0; igal<ngal; igal++) {
    for (ix=0; ix<gal[igal].nsize; ix++) {
      for (iy=0; iy<gal[igal].nsize; iy++) {
        for (ifilt=0; ifilt<*nfilt; ifilt++) {
          jfilt = filts[ifilt];
          if (set) 
            par[npar++].val = gal[igal].inten[ix][iy][jfilt];
          else {
            gal[igal].inten[ix][iy][jfilt] = update_inten(&par[npar]);
            if (gal[igal].inten[ix][iy][jfilt] < 2*par[npar].err)
              gal[igal].inten[ix][iy][jfilt] = 0.;
            npar++;
          }
        }
      }
    }
  }


  *converge = 1;
  for (i=0; i<npar; i++) *converge *= par[i].converge;

  return(npar);
}
float update_par(PAR *x, double dmin)
{
  if (x->del==0) {
    x->converge = 1;
    return(x->val);
  }
  if (fabs(x->del) < x->err || fabs(x->del/x->val) < 0.01 || fabs(x->del)<dmin)
    x->converge = 1;
  else
    x->converge = 0;
  return (x->val+x->del);
}

float update_pos(PAR *x, double dmax, double dmin)
{
  float newpos;

  if (x->del==0) {
    x->converge = 1;
    return(x->val);
  }
  x->converge = 0;
  newpos = fabs(x->del)>dmax ? x->val+dmax*(x->del/fabs(x->del)) : x->val+x->del;
  if (fabs(newpos-x->val) < x->err ||
      fabs(newpos-x->val) < dmin ) x->converge = 1;
  return(newpos);
}

float update_inten(PAR *x)
{
  float dxmax;

  if (x->del==0) {
    x->converge = 1;
    return(x->val);
  }
  x->converge = 0;
  if (x->val>2*x->err) {
    if ( (x->val+x->del)/x->val > 5) {
      return (5*x->val);
    }
    if ( (x->val+x->del)/x->val < 0.2) {
      return (x->val/5.);
    }
    if ((x->val+x->del)/x->val < 1.005 && 
        (x->val+x->del)/x->val > 0.995) x->converge = 1;
    if (fabs(x->del) < x->err) x->converge=1;
    return (x->val+x->del);
  } else {
    if (fabs(x->del) < 2*x->err) x->converge = 1;
    return(x->val+x->del);
  }
}


void skytoxy(FRAME frame, double dec, double ra, double *x, double *y) 
{
  double a,b,c,d,den;
  int istat, maxpar=2, npar=2;

  frame.invert[0][0] = a = frame.cd1_1;
  frame.invert[0][1] = b = frame.cd1_2;
  frame.invert[1][0] = c = frame.cd2_1;
  frame.invert[1][1] = d = frame.cd2_2;
  den=a*d-b*c;
  *x = frame.crpix1 + 
       d/den*(ra-frame.crval1)*frame.cosdec-b/den*(dec-frame.crval2);
  *y = frame.crpix2 - 
       c/den*(ra-frame.crval1)*frame.cosdec+a/den*(dec-frame.crval2);
}
void xytosky(FRAME frame, double x, double y, double *dec, double *ra)
{
  *ra = frame.crval1 + 
         ((x-frame.crpix1)*frame.cd1_1 + (y-frame.crpix2)*frame.cd1_2)/frame.cosdec;
  *dec = frame.crval2 + 
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
/*
  skytoxy (frame, dec, ra, &x, &y);
  frame.cd1_1*=1.01;
  skytoxy (frame, dec, ra, &x2, &y2);
//  *(dx+0) = (x2-x)/0.01/frame.cd1_1;
//  *(dy+0) = (y2-y)/0.01/frame.cd1_1;
  fprintf(stderr,"numerical:\n %f %f %f %f %f %f\n",x2,y2,(x2-x)/0.01/frame.cd1_1,(y2-y)/0.01/frame.cd1_1,*(dx+0),*(dy+0));
  frame.cd1_1/=1.01;
  frame.cd1_2*=1.01;
  skytoxy (frame, dec, ra, &x2, &y2);
  fprintf(stderr," %f %f %f %f %f %f\n",x2,y2,(x2-x)/0.01/frame.cd1_2,(y2-y)/0.01/frame.cd1_2,*(dx+1),*(dy+1));
//  *(dx+1) = (x2-x)/0.01/frame.cd1_1;
//  *(dy+1) = (y2-y)/0.01/frame.cd1_1;
  frame.cd1_2/=1.01;
  frame.cd2_1*=1.01;
  skytoxy (frame, dec, ra, &x2, &y2);
  fprintf(stderr," %f %f %f %f %f %f\n",x2,y2,(x2-x)/0.01/frame.cd2_1,(y2-y)/0.01/frame.cd2_1,*(dx+2),*(dy+2));
//  *(dx+2) = (x2-x)/0.01/frame.cd1_1;
//  *(dy+2) = (y2-y)/0.01/frame.cd1_1;
  frame.cd2_1/=1.01;
  frame.cd2_2*=1.01;
  skytoxy (frame, dec, ra, &x2, &y2);
  fprintf(stderr," %f %f %f %f %f %f\n",x2,y2,(x2-x)/0.01/frame.cd2_2,(y2-y)/0.01/frame.cd2_2,*(dx+3),*(dy+3));
//  *(dx+3) = (x2-x)/0.01/frame.cd1_1;
//  *(dy+3) = (y2-y)/0.01/frame.cd1_1;
  frame.cd2_2/=1.01;
*/
}

void getsncoord(FRAME frame, VAR *var)
{
  skytoxy(frame,var->dec,var->ra,&var->x,&var->y); 
}
void getscoord(FRAME frame, STAR *star)
{
  skytoxy(frame,star->dec,star->ra,&star->x,&star->y);
}
void getgcoord(FRAME frame, GAL *gal, double gra, double gdec)
{
  skytoxy(frame,gdec,gra,&gal->x,&gal->y);
}

void psfint(FRAME *frame, int smooth)
{
  int i, j, ii, jj, n;
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
  spsf->data = (float *)malloc(n*n*sizeof(float));
  for (i=0;i<n;i++) {
    for (j=0;j<n;j++) {
      *(spsf->data+j*n+i) = 0.;
      for (ii=i-smooth/2;ii<=i+smooth/2;ii++) {
        if (ii>=0&&ii<n) {
          for (jj=j-smooth/2;jj<=j+smooth/2;jj++) {
            if (jj>=0&&jj<n) {
              *(spsf->data+j*n+i) += *(psf->data+jj*n+ii);
            }
          }
        }
      }
    }
  }

}
double psf(PSF *psf, double x, double y, double *dpdx, double *dpdy)
{
  int ix, iy, n;
  float *data;
  double xpix, ypix, y1x1, y1x2, y2x1, y2x2, y1, y2;
  double dx, dy, dpdy1,dpdy2;

  data = psf->data;
  n = psf->n;
  xpix = x*psf->nlib + psf->n/2;
  ypix = y*psf->nlib + psf->n/2;
  ix = (int) floor(xpix);
  if (ix<0||ix>(psf->n-2)) return(0.);
  iy = (int) floor(ypix);
  if (iy<0||iy>(psf->n-2)) return(0.);
  dx = xpix - ix;
  dy = ypix - iy;

  //nx = (int) rint(1./psf->dx);
  //ix = (int) floor(x/psf->dx) + psf->n/2;
  //dx = (x/psf->dx) + psf->n/2 - ix;
  //iy = (int) floor(y/psf->dx) + psf->n/2;
  //dy = (y/psf->dx) + psf->n/2 - iy;
  //if (ix<0||iy<0||ix>(psf->n-nx-1)||iy>(psf->n-nx-1)) return(0.);

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
int usepix(FRAME frame, double ra, double dec, double pra, double pdec, float fitrad) 
{
  double ddec,dra;
  ddec=fabs(dec-pdec);
  if (ddec>fitrad) return(0);
  dra=fabs(ra-pra)*frame.cosdec;
  if (dra>fitrad) return(0);
  if (ddec*ddec+dra*dra > fitrad*fitrad) return(0);
  return(1);
}

float back(FRAME *frame, int ix, int iy)
{
  float t;

  t = frame->back[0];
  frame->dback[0] = 1;
  if (NBACK>1) {
    t+= frame->back[1]*(ix-frame->crpix1);
    t+= frame->back[2]*(iy-frame->crpix2);
    frame->dback[1] = ix-frame->crpix1;
    frame->dback[2] = iy-frame->crpix2;
  }
  if (NBACK>3) {
    t+= frame->back[3]*(ix-frame->crpix1)*(ix-frame->crpix1);
    t+= frame->back[4]*(iy-frame->crpix2)*(iy-frame->crpix2);
    t+= frame->back[5]*(ix-frame->crpix1)*(iy-frame->crpix2);
    frame->dback[3] = (ix-frame->crpix1)*(ix-frame->crpix1);
    frame->dback[4] = (iy-frame->crpix2)*(iy-frame->crpix2);
    frame->dback[5] = (ix-frame->crpix1)*(iy-frame->crpix2);
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
double asinhmag(double f, int ifilt)
{
  double f0;
  static double b[5]={1.4e-10,0.9e-10,1.2e-10,1.8e-10,7.4e-10};
  f0=1.;
  return(-(2.5/log(10.))*(asinh((f/f0)/(2*b[ifilt]))+log(b[ifilt])));
}
double asinhmagerr(double f, double ferr, int ifilt)
{
  double f0;
  static double b[5]={1.4e-10,0.9e-10,1.2e-10,1.8e-10,7.4e-10};
  f0=1.;

  return( 2.5/log(10) * ferr / (2.*b[ifilt]) / sqrt(pow(1+(f/(2*b[ifilt])),2)));
}
