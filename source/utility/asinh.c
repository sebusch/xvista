#include "Vista.h"
#undef _POSIX_SOURCE
#include <math.h>
ATUS(casinh)(val,out)
double *val,*out;
{
  *out=asinh(*val);
}

