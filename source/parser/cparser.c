#include "Vista.h"

ATUS(ccvirset) (loc,val)
ADDRESS *loc, *val;
{
  ATUS(virset) (*loc,val);
}

ATUS(ccgetvir) (loc)
ADDRESS *loc;
{
  ATUS(getvir) (*loc);
}


