#include "Vista.h"

/*  Here is a sample subroutine which takes pointers which are passed
   from the VISTA command tree and passes them on to your subroutine,
   simply translating the pointer to the pointer to the data to just
   the pointer to the data  */

//#define ADDRESS long int
#ifdef __alpha
void *malloc();
#else
char *malloc();
#endif


#ifdef RODDIER
ATUS(cchubbleg1)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(hubbleg1)(*locim,imsr,imer,imsc,imec);
}

ATUS(ccbfourt41)(locim,imsr,imer,imsc,imec,locb,jmsr,jmer,jmsc,jmec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
ADDRESS *locb, *jmsr, *jmer, *jmsc, *jmec;
{
  ATUS(bfourt41)(*locim,imsr,imer,imsc,imec,*locb,jmsr,jmer,jmsc,jmec);
}

ATUS(ccbfourt41a)(pupil1,imsr,imer,imsc,imec,ro0,jmsr,jmer,jmsc,jmec,locc,kmsr,kmer,kmsc,kmec,locd,lmsr,lmer,lmsc,lmec,intbuf,stabuf)
ADDRESS *pupil1, *imsr, *imer, *imsc, *imec;
ADDRESS *ro0, *jmsr, *jmer, *jmsc, *jmec;
ADDRESS *locc, *kmsr, *kmer, *kmsc, *kmec;
ADDRESS *locd, *lmsr, *lmer, *lmsc, *lmec;
ADDRESS *intbuf, *stabuf;
{
  ATUS(bfourt41a)(pupil1,imsr,imer,imsc,imec,ro0,jmsr,jmer,jmsc,jmec,*locc,kmsr,kmer,kmsc,kmec,*locd,lmsr,lmer,lmsc,lmec,intbuf,stabuf);
}

ATUS(ccbfourt70)(locim,imsr,imer,imsc,imec,locb,jmsr,jmer,jmsc,jmec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
ADDRESS *locb, *jmsr, *jmer, *jmsc, *jmec;
{
  ATUS(bfourt70)(*locim,imsr,imer,imsc,imec,*locb,jmsr,jmer,jmsc,jmec);
}

ATUS(ccunwrapit)(locim,imsr,imer,imsc,imec,locb,jmsr,jmer,jmsc,jmec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
ADDRESS *locb, *jmsr, *jmer, *jmsc, *jmec;
{
  ATUS(unwrapit)(*locim,imsr,imer,imsc,imec,*locb,jmsr,jmer,jmsc,jmec);
}

ATUS(ccbfourt70a)(pupil1,imsr,imer,imsc,imec,ro0,jmsr,jmer,jmsc,jmec,locc,kmsr,kmer,kmsc,kmec,locd,lmsr,lmer,lmsc,lmec,loce,msr,mer,msc,mec,intbuf,phabuf,stabuf)
ADDRESS *pupil1, *imsr, *imer, *imsc, *imec;
ADDRESS *ro0, *jmsr, *jmer, *jmsc, *jmec;
ADDRESS *locc, *kmsr, *kmer, *kmsc, *kmec;
ADDRESS *locd, *lmsr, *lmer, *lmsc, *lmec;
ADDRESS *loce, *msr, *mer, *msc, *mec;
ADDRESS *intbuf, *phabuf, *stabuf;
{
  ATUS(bfourt70a)(pupil1,imsr,imer,imsc,imec,ro0,jmsr,jmer,jmsc,jmec,*locc,kmsr,kmer,kmsc,kmec,*locd,lmsr,lmer,lmsc,lmec,*loce,msr,mer,msc,mec,intbuf,phabuf,stabuf);
}

/*ATUS(ccwfigure)(locim,nrow,ncol)
ADDRESS *locim, *nrow, *ncol;
{
  ATUS(wfigure)(*locim,nrow,ncol);
} */

/*
ATUS(cclibget2)(location,npsflib,nlib,ix,iy,xc,yc,qval,dvdx,dvdy,x,ipsfmode,mag)
ADDRESS *location,*npsflib,*nlib,*ix,*iy,*xc,*yc,*qval,*dvdx,*dvdy;
ADDRESS *x,*ipsfmode,*mag;
{
  ATUS(libget2)(*location,npsflib,nlib,ix,iy,xc,yc,qval,dvdx,dvdy,x,ipsfmode,mag); 
} */

ATUS(ccfourtit)(locim,nr,nc,locb,nrb,ncb)
ADDRESS *locim, *locb, *nr, *nc, *nrb, *ncb;
{
  ATUS(fourtit)(*locim,nr,nc,*locb,nrb,ncb);
}

ATUS(ccdofourt)(loc,inten,phase,nrow,ncol,inv)
ADDRESS *loc, *inten, *phase, *nrow, *ncol, *inv;
{
  ATUS(dofourt)(*loc,inten,phase,nrow,ncol,inv);
}

ATUS(ccwrapit)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(wrapit)(*locim,imsr,imer,imsc,imec);
}

ATUS(ccfixpole)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(fixpole)(*locim,imsr,imer,imsc,imec);
}
ATUS(ccvalley)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(valley)(*locim,imsr,imer,imsc,imec);
}

#endif

ATUS(ccpsfphase)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(psfphase)(*locim,imsr,imer,imsc,imec);
}

ATUS(ccphaseret)(a,nc,nr,nf,dx,dy,dz2f,dz3f,exp,wave,weight,nwave,x,y,nstar,sky,icam,fitrad,iwrite,nsamp,loc,isr,ier,isc,iec,outbuf,loc1,loc2,locs,loc3,loc4,n1,n2,n3,n4,n5,n6)
ADDRESS *a,*nc,*nr,*nf,*wave,*weight,*nwave,*x,*y,*sky,*icam,*fitrad,*iwrite,*outbuf;
ADDRESS *loc,*isr,*ier,*isc,*iec,*nstar,*nsamp,*loc1,*loc2,*loc3,*loc4,*locs;
ADDRESS *n1,*n2,*n3,*n4,*n5,*n6,*dx,*dy,*exp,*dz2f,*dz3f;
{
  ATUS(phaseret)(a,nc,nr,nf,dx,dy,dz2f,dz3f,exp,wave,weight,nwave,x,y,nstar,sky,icam,fitrad,iwrite,nsamp,*loc,isr,ier,isc,iec,outbuf,*loc1,*loc2,*locs,*loc3,*loc4,n1,n2,n3,n4,n5,n6);
}

ATUS(ccloadjit)(locim,ksr,ker,ksc,kec,wj,is,ie)
ADDRESS *locim,*ksr,*ker,*ksc,*kec,*wj,*is,*ie;
{
  ATUS(loadjit)(*locim,ksr,ker,ksc,kec,wj,is,ie);
}


#ifdef NOTDEF
ATUS(cccrcount)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(crcount)(*locim,imsr,imer,imsc,imec);
}

ATUS(cccte)(locim,nrow,ncol)
ADDRESS *locim, *nrow, *ncol;
{
  ADDRESS location;
  int ii;
  location = (ADDRESS)malloc(*nrow * *ncol * 4);
  ATUS(cte_fix_sub)(*locim,nrow,ncol,location,nrow,ncol);
  ii=free(location);
}

ATUS(cccteput)(locim,nrow,ncol)
ADDRESS *locim, *nrow, *ncol;
{
  ADDRESS location;
  int ii;
  location = (ADDRESS)malloc(*nrow * *ncol * 4);
  ATUS(cte_put_sub)(*locim,nrow,ncol,location,nrow,ncol);
  ii=free(location);
}

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

ATUS(ccbdfit2d)(locim,imsr,imer,imsc,imec)
ADDRESS *locim, *imsr, *imer, *imsc, *imec;
{
  ATUS(bdfit2d)(*locim,imsr,imer,imsc,imec);
}
#endif

ATUS(ccmakepupil)(locinten,ndim,icam,xc,yc,radius,radx,rady,nsamp,pupslope,pupxc,pupyc,pupsize)
ADDRESS *locinten,*ndim,*icam,*xc,*yc,*radius,*radx,*rady,*nsamp;
ADDRESS *pupslope,*pupxc,*pupyc,*pupsize;
{
  ATUS(makepupil)(*locinten,ndim,icam,xc,yc,radius,radx,rady,nsamp,pupslope,pupxc,pupyc,pupsize);
}

ATUS(ccmakephase)(locinten,locphase,ndim,z,radius,radx,rady,wave,icam)
ADDRESS *locinten,*locphase,*ndim,*z,*radius,*radx,*rady,*wave,*icam;
{
  ATUS(makephase)(*locinten,*locphase,ndim,z,radius,radx,rady,wave,icam);
}

ATUS(ccdofft)(locfft,locinten,locphase,ndim,resolve,rgauss,smoothsize,a,b,ngw)
ADDRESS *locfft,*locinten,*locphase,*ndim,*resolve,*rgauss,*smoothsize,*a,*b;
ADDRESS *ngw;
{
  ATUS(dofft)(*locfft,*locinten,*locphase,ndim,resolve,rgauss,smoothsize,a,b,ngw);
}

