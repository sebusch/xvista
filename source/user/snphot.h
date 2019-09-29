#define havesnphot

#define NBACK 6
//#define NBACK 1
#define MAXFILT 5
#define MAXFRAME 900
#define MAXSIZE 30

typedef struct {
  double val;
  double oldval;
  double savedval;
  double err;
  double del;
  int fit;
  int converge;
} PAR;

typedef struct {
  int n;
  double dx;
  double fwhmpix;
  int nlib;
  float *data;
} PSF;

typedef struct {
  char *filename;
  int nx;
  int ny;
  double crval1;
  double crval2;
  double cd1_1;
  double cd1_2;
  double cd2_1;
  double cd2_2;
  double crpix1;
  double crpix2;
  double cosdec;
  int cnpix1;
  int cnpix2;
  double xdist[10];
  double ydist[10];
  double invert[2][2];
  int tele;
  int camcol;
  int field;
  int filt;
  int strip;
  int nstrip;
  double stripdra;
  double stripddec;
  double zeropt;
  double zeropterr;
  double backx0;
  double backy0;
  double back[NBACK];
  double dback[NBACK];
  double skyrat;
  double skysig;
  double jd;
  double exp;
  double am;
  double gain;
  double rn;
  double lowbad;
  double highbad;
  int first;
  int combo;
  int flag;
  int skipgal;
  float *data;
  double chi;
  double chi2;
  int nsig[5];
  double sig[5];
  double mean[5];
  double apcor[5];
  double starchi2[5];
  double starchi;
  double starerr;
  int npix;
  int nstar;
  int ngood;
  PSF *psf;
  PSF *psfint;
} FRAME;

typedef struct {
  int id;
  double mag[MAXFILT];
  double err[MAXFILT];
  double inten[MAXFILT];
  double intenerr[MAXFILT];
  double apinten[MAXFILT];
  double bigapinten[MAXFILT];
  double chi[MAXFILT];
  double sumwt[MAXFILT];
  double dec;
  double ra;
  double pmdec;
  double pmra;
  double x;
  double y;
  double color;
  double pow;
  int ixmin;
  int ixmax;
  int iymin;
  int iymax;
  int good;
  int used;
} STAR;

typedef struct {
  int id;
  double mag[MAXFRAME];
  double inten[MAXFRAME];
  double intenerr[MAXFRAME];
  double apinten[MAXFRAME];
  double bigapinten[MAXFRAME];
  double chi[MAXFRAME];
  double sumwt[MAXFRAME];
  double dec;
  double ra;
  double jdmin;
  double jdmax;
  double x;
  double y;
  double color;
  double pow;
  int ixmin;
  int ixmax;
  int iymin;
  int iymax;
} VAR;

typedef struct {
  double inten[MAXSIZE][MAXSIZE][MAXFILT];
  double intenerr[MAXSIZE][MAXSIZE][MAXFILT];
  double intgal[MAXSIZE][MAXSIZE][MAXFILT];
  double interr[MAXSIZE][MAXSIZE][MAXFILT];
  double dec;
  double ra;
  int nsize; /*number of 1D pixels for galaxy */
  double dx; /*separation of pixels in DEGREES */
  double x;
  double y;
  double color[MAXSIZE][MAXSIZE];
  double pow[MAXSIZE][MAXSIZE];
  int ixmin;
  int ixmax;
  int iymin;
  int iymax;
} GAL;

typedef struct {
  double dra;
  double draerr;
  double ddec;
  double ddecerr;
  double pmra;
  double pmraerr;
  double pmdec;
  double pmdecerr;
  double scale[MAXFILT];
} STRIP;
