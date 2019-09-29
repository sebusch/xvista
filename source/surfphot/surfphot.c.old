#include "Vista.h"

ATUS(ccloadmodel)(model,derv,see,work,j1,j2,i1,i2,xcen,ycen,rmin,rmax,par,lock,maxpar,fwhm,ndim)
ADDRESS *model,*derv,*work,*see,*j1,*j2,*i1,*i2,*xcen,*ycen,*rmin,*rmax;
ADDRESS *par,*lock,*maxpar,*fwhm,*ndim;
{
  ATUS(loadmodel)(*model,*derv,*see,*work,j1,j2,i1,i2,xcen,ycen,rmin,rmax,par,lock,maxpar,fwhm,ndim);
}

ATUS(ccaccum)(locmod,locderv,j1,j2,i1,i2,a,isrow,ierow,iscol,iecol,locerr,mask,xcen,ycen,rmin,rmax,gain,skyerr,sky,maxpar,npar,lock,chi2,npix,alp,beta,ndim)
ADDRESS *locmod,*locderv,*j1,*j2,*i1,*i2,*a,*isrow,*ierow,*iscol,*iecol,*ndim,*mask;
ADDRESS *rmin,*rmax,*gain,*skyerr,*sky,*maxpar,*npar,*lock,*chi2,*npix,*alp,*beta;
ADDRESS *xcen,*ycen,*locerr;
{
  ATUS(accum)(*locmod,*locderv,j1,j2,i1,i2,a,isrow,ierow,iscol,iecol,*locerr,mask,xcen,ycen,rmin,rmax,gain,skyerr,sky,maxpar,npar,lock,chi2,npix,alp,beta,ndim);
}

ATUS(ccmkgauss)(locsee,ndim,fwhm)
ADDRESS *locsee,*ndim,*fwhm;
{
  ATUS(mkgauss)(*locsee,ndim,fwhm);
}

ATUS(ccmkpsf)(locpsf,isr,ier,isc,iec,locsee,ndim,fwhm)
ADDRESS *locsee,*ndim,*fwhm,*locpsf,*isr,*ier,*isc,*iec;
{
  ATUS(mkpsf)(*locpsf,isr,ier,isc,iec,*locsee,ndim,fwhm);
}

ATUS(ccsubmodel)(locmod,j1,j2,i1,i2,a,isrow,ierow,iscol,iecol,locerr,sub,xcen,ycen,rmin,rmax,totobs,totmod, sky, ndim, gain, skyerr)
ADDRESS *locmod,*j1,*j2,*i1,*i2,*a,*isrow,*ierow,*iscol,*iecol,*sub,*totobs,*totmod;
ADDRESS *xcen, *ycen,*rmin, *rmax, *sky, *ndim, *locerr, *gain, *skyerr;
{
  ATUS(submodel)(*locmod,j1,j2,i1,i2,a,isrow,ierow,iscol,iecol,*locerr,sub,xcen,ycen,rmin,rmax,totobs,totmod,sky,ndim,gain,skyerr);
} 

