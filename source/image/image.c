#include "Vista.h"

ATUS(cchistload)(locim,isr,ier,isc,iec,out,maxout,ncol,ntot)
ADDRESS *locim, *isr, *ier, *isc, *iec, *out, *ntot, *ncol, *maxout;
{
  ATUS(histload)(*locim,isr,ier,isc,iec,out,maxout,ncol,ntot);
}

ATUS(cchistlist)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(histlist)(*locim,imsr,imer,imsc,imec);
}

ATUS(cc1expio)(locnew,newrol,newcol,locim,nrow,ncol,nexp,idelt,jdelt)
ADDRESS *locnew,*newrol,*newcol,*locim,*nrow,*ncol,*nexp,*idelt,*jdelt;
{
  ATUS(expio)(*locnew,newrol,newcol,*locim,nrow,ncol,nexp,idelt,jdelt);
}

ATUS(ccphase)(locim,nrow,ncol)
ADDRESS *locim, *nrow, *ncol;
{
  ATUS(phase)(*locim,nrow,ncol);
}

ATUS(cccrossoff)(locim,isr,ier,isc,iec,locnew,jsr,jer,jsc,jec)
ADDRESS *locim, *isr, *ier, *isc, *iec, *locnew, *jsr, *jer, *jsc, *jec;
{
  ATUS(crossoff)(*locim,isr,ier,isc,iec,*locnew,jsr,jer,jsc,jec);
}
