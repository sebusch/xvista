#include "Vista.h"

ATUS(ccdofind)(loc1,loc2,maxbox,ncola)
ADDRESS *loc1,*loc2,*maxbox,*ncola;
{
  ATUS(dofind)(*loc1,*loc2,maxbox,ncola);
}

ATUS(ccdomultistar)(
rsig,xc,yc,mag,skyparam,dxpos,dypos,dxposorig,dyposorig,
xcclamp,ycclamp,magclamp,skyclamp,dxposclamp,dyposclamp,
chi,sumwt,numer,denom,sharp,rpixsq,skybar,sky,
chiold,magerr,xcorig,ycorig,id,npix,
group,groupold,nout,ngroup,ind2,skip,done,skipall,
tempdata,c,v,sumv,x,c2,c8,indpar,ixpsf,iypsf,wpsf,
itmp,indtmp,maxstr,maxfrm,maxcol,maxfit,maxsky,maxunk)
ADDRESS *rsig,*xc,*yc,*mag,*skyparam,*dxpos,*dypos,*dxposorig,*dyposorig;
ADDRESS *xcclamp,*ycclamp,*magclamp,*skyclamp,*dxposclamp,*dyposclamp;
ADDRESS *chi,*sumwt,*numer,*denom,*sharp,*rpixsq,*skybar,*sky;
ADDRESS *chiold,*magerr,*xcorig,*ycorig,*id,*npix;
ADDRESS *group,*groupold,*nout,*ngroup,*ind2,*skip,*done,*skipall;
ADDRESS *tempdata,*c,*v,*sumv,*x,*c2,*c8,*indpar,*ixpsf,*iypsf,*wpsf;
ADDRESS *itmp,*indtmp,*maxstr,*maxfrm,*maxcol,*maxfit,*maxsky,*maxunk;
{
  ATUS(domultistar)(
  *rsig,*xc,*yc,*mag,*skyparam,*dxpos,*dypos,*dxposorig,*dyposorig,
  *xcclamp,*ycclamp,*magclamp,*skyclamp,*dxposclamp,*dyposclamp,
  *chi,*sumwt,*numer,*denom,*sharp,*rpixsq,*skybar,*sky,
  *chiold,*magerr,*xcorig,*ycorig,*id,*npix,
  *group,*groupold,*nout,*ngroup,*ind2,*skip,*done,*skipall,
  *tempdata,*c,*v,*sumv,*x,*c2,*c8,*indpar,*ixpsf,*iypsf,*wpsf,
  *itmp,*indtmp,maxstr,maxfrm,maxcol,maxfit,maxsky,maxunk);
}

ATUS(ccphotsub)(subarr,maxrad)
ADDRESS *subarr,*maxrad;
{
  ATUS(photsub)(*subarr,maxrad);
}

ATUS(ccsorter)(line1,line2,maxstr)
ADDRESS *line1,*line2,*maxstr;
{
  ATUS(sorter)(*line1,*line2,maxstr);
}

ATUS(ccdonmag)(id,x,y,mag,sky,isize,number,maxstr)
ADDRESS *id, *x, *y, *mag, *sky, *isize, *number, *maxstr;
{
  ATUS(donmag)(*id,*x,*y,*mag,*sky,*isize,*number,maxstr);
}
ATUS(ccdooldgroup)(groupmember,x,y,mag,sky,id,isize,number,removed,take,maxgroup,maxstr)
ADDRESS *groupmember,*x,*y,*mag,*sky,*id,*isize,*number,*removed,*take,*maxgroup,*maxstr;
{
  ATUS(dooldgroup)(*groupmember,*x,*y,*mag,*sky,*id,*isize,*number,*removed,*take,maxgroup,maxstr);
}

ATUS(ccdogroup)(id,x,y,mag,sky,isize,number,maxstr)
ADDRESS *id,*x,*y,*mag,*sky,*isize,*number,*maxstr;
{
  ATUS(dogroup)(*id,*x,*y,*mag,*sky,*isize,*number,maxstr);
}

ATUS(ccdoselect)(id,x,y,mag,sky,maxstr)
ADDRESS *id,*x,*y,*mag,*sky,*maxstr;
{
  ATUS(doselect)(*id,*x,*y,*mag,*sky,maxstr);
}

ATUS(ccdogetpsf)(psf,f,maxbox,maxpsf,maxgau)
ADDRESS *psf,*f,*maxbox,*maxpsf,*maxgau;
{
  ATUS(dogetpsf)(*psf,*f,maxbox,maxpsf,maxgau);
}

ATUS(ccdoaddstar)(psf,maxpsf)
ADDRESS *psf,*maxpsf;
{
  ATUS(doaddstar)(*psf,maxpsf);
}

ATUS(ccdomagaver)(out,ind,maxstr,nout,n1,n2)
ADDRESS *out,*ind,*maxstr,*nout,*n1,*n2;
{
  ATUS(domagaver)(*out,*ind,maxstr,nout,n1,n2);
}

ATUS(ccdocombine)(refdata,refpos,chi,err,am,xplot,yplot,mstr,mobs)
ADDRESS *refdata,*refpos,*chi,*err,*am,*xplot,*yplot,*mstr,*mobs;
{
  ATUS(docombine)(*refdata,*refpos,*chi,*err,*am,*xplot,*yplot,mstr,mobs);
}

ATUS(ccpsffit) (location,isr,ier,isc,iec,psfrad,rad,watch)
ADDRESS *location,*isr,*ier,*isc,*iec,*psfrad,*rad,*watch;
{
  ATUS(psffit) (*location,isr,ier,isc,iec,psfrad,rad,watch);
}

ATUS(ccstarplot) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(starplot) (*location,isr,ier,isc,iec);
}

ATUS(ccdomergemag) (location,i1,i2,i3)
ADDRESS *location,*i1,*i2,*i3;
{
  ATUS(domergemag) (*location,i1,i2,i3);
}


ATUS(ccaperturesky)(location,nx,ny,xc,yc,ntotstr,rin,rout,have3sig,lowbad,highbad,skyparam,maxsky,maxstr)
ADDRESS *location,*nx,*ny,*xc,*yc,*ntotstr,*rin,*rout,*have3sig,*lowbad,*highbad;
ADDRESS *skyparam,*maxsky,*maxstr;
{
  ATUS(aperturesky)(*location,nx,ny,xc,yc,ntotstr,rin,rout,have3sig,lowbad,highbad,skyparam,maxsky,maxstr);
}

ATUS(ccaddone)(location,isrow,ierow,iscol,iecol,ix,iy)
ADDRESS *location, *isrow,*ierow,*iscol,*iecol,*ix,*iy;
{
  ATUS(addone)(*location,isrow,ierow,iscol,iecol,ix,iy);
}

ATUS(ccdoweed)(data,line2,maxstr0,maxcol0)
ADDRESS *data, *line2, *maxstr0, *maxcol0;
{
  ATUS(doweed)(*data,*line2,maxstr0,maxcol0);
}

ATUS(ccrload)(data,ksr,ker,ksc,kec,ix,iy,a,ireset)
ADDRESS *data, *ksr, *ker, *ksc, *kec, *ix, *iy, *a, *ireset;
{
  ATUS(rload)(*data,ksr,ker,ksc,kec,ix,iy,a,ireset);
}

ATUS(ccrdiv)(data,ksr,ker,ksc,kec,ix,iy,rtot)
ADDRESS *data, *ksr, *ker, *ksc, *kec, *ix, *iy, *rtot ;
{
  ATUS(rdiv)(*data,ksr,ker,ksc,kec,ix,iy,*rtot);
}
ATUS(cccmderr) (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
ADDRESS *location,*isr,*ier,*isc,*iec;
ADDRESS *loc2,*isr2,*ier2,*isc2,*iec2;
{
  ATUS(cmderr) (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}


