#include <math.h>
casinh_(val,out)
double *val,*out;
{
  double v=1;
  float f=1;
  long double l=1;
  *out=asinh(*val);
  printf("asinh: %f %f %f\n",asinh(v), asinf(f), asinl(l));
}

