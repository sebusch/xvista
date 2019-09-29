#define havesnphot

#define NBACK 6
#define MAXFILT 5
#define MAXFRAME 200
#define MAXSIZE 30

typedef struct {
  double val;
  double oldval;
  double err;
  double del;
  int converge;
} PAR;

typedef struct {
  int n;
  double dx;
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
  int cnpix1;
  int cnpix2;
  double invert[2][2];
  int tele;
  int filt;
  double zeropt;
  double back[NBACK];
  double dback[NBACK];
  double jd;
  double gain;
  double rn;
  double lowbad;
  double highbad;
  int first;
  int combo;
  float *data;
  PSF *psf;
  PSF *psfint;
} FRAME;

typedef struct {
  int id;
  double mag[MAXFILT];
  double inten[MAXFILT];
  double intenerr[MAXFILT];
  double chi[MAXFILT];
  double sumwt[MAXFILT];
  double dec;
  double ra;
  double x;
  double y;
  double color;
  double pow;
} STAR;

typedef struct {
  int id;
  double mag[MAXFRAME];
  double inten[MAXFRAME];
  double intenerr[MAXFRAME];
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
} VAR;

typedef struct {
  double inten[MAXSIZE][MAXSIZE][MAXFILT];
  double dec;
  double ra;
  int nsize; /*number of 1D pixels for galaxy */
  double dx; /*separation of pixels in DEGREES */
  double x;
  double y;
  double color[MAXSIZE][MAXSIZE];
  double pow[MAXSIZE][MAXSIZE];
} GAL;

