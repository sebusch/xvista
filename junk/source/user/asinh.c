#include <math.h>
#include <stdio.h>

double asinhmag_(double *, double *, int*);
double asinhmag(double, double, int);
double asinhmagerr_(double *, double *, int*);
double asinhmagerr(double, double, int);
double asinhinv_(double *, int*);
double asinhinv(double, int);
double asinhinverr_(double *, double *, int *);
double asinhinverr(double, double, int);

double asinhmag_(double *f, double *ferr, int *ifilt)
{
  return(asinhmag(*f,*ferr,*ifilt));
}

double asinhmag(double f,  double ferr, int ifilt)
{
  double f0;
  static double b[5]={1.4e-10,0.9e-10,1.2e-10,1.8e-10,7.4e-10};
  f0=1.;
  if (ferr<0.)
    return(99.999);
  else
    return(-(2.5/log(10.))*(asinh((f/f0)/(2*b[ifilt]))+log(b[ifilt])));
}

double asinhmagerr_(double *f, double *ferr, int *ifilt)
{
  return(asinhmagerr(*f,*ferr,*ifilt));
}
double asinhmagerr(double f, double ferr, int ifilt)
{
  double f0;
  static double b[5]={1.4e-10,0.9e-10,1.2e-10,1.8e-10,7.4e-10};
  f0=1.;
  if (ferr<0)
    return(9.999);
  else
    return( 2.5/log(10) * ferr / (2.*b[ifilt]) / sqrt(1+pow(f/(2*b[ifilt]),2)) );
    //return( 2.5/log(10) * ferr / (2.*b[ifilt]) / sqrt(pow(1+(f/(2*b[ifilt])),2)));
}

double asinhinv_(double *mag, int *ifilt)
{
  return(asinhinv(*mag,*ifilt));
}
double asinhinv(double mag, int ifilt)
{
  static double b[5]={1.4e-10,0.9e-10,1.2e-10,1.8e-10,7.4e-10};
  return(2*b[ifilt]*sinh(mag / (-(2.5/log(10.))) - log(b[ifilt])) );
}

double asinhinverr_(double *magerr, double *f, int *ifilt)
{
  return(asinhinverr(*magerr,*f,*ifilt));
}
double asinhinverr(double magerr, double f, int ifilt)
{
  static double b[5]={1.4e-10,0.9e-10,1.2e-10,1.8e-10,7.4e-10};
  return(magerr * log(10)/2.5 * (2.*b[ifilt]) * sqrt(1+pow(f/(2*b[ifilt]),2)));
}

