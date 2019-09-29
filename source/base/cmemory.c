#include "Vista.h"
/*  This file contains C routine calls that handle passing               */
/*   memory addresses and string pointers and descriptors (for VMS)        */
/*   back and forth between FORTRAN routines. The conventions is that      */
/*   each of these routines begins with cc, then the name of the FORTRAN   */
/*   subroutine for which it is doing the pointer-address-descriptor-etc   */
/*   translation.                                                        */

/*  NOTE:  The routines here can either have or not have a trailing         */
/*         underscore as needed by some compilers for the Fortran / C       */
/*         interface.  The existence of a trailing character for the        */
/*         functions is set by the C-preprocessor variable TUS,  which     */
/*         should be defined for an underscore or undefine to have no       */
/*         underscore. This variable is set in the Vista.(machine) file     */
/*         in the lickvista/source subdirectory                             */

/****************************************************************************/
 
#include <stdio.h>
 
#ifdef VMS
#include descrip
#endif  /* VMS */

ATUS(cctapewrite) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(tapewrite) (*location,isr,ier,isc,iec);
}
 
ATUS(ccaverage) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(average) (*location,isr,ier,isc,iec);
}
 
ATUS(ccoldsky) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(oldsky) (*location,isr,ier,isc,iec);
}
 
ATUS(ccsky) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(sky) (*location,nr,nc);
}
 
ATUS(ccwsky) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(wsky) (*location,nr,nc);
}
 
ATUS(ccanalbox) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(analbox) (*location,isr,ier,isc,iec);
}
 
ATUS(ccaxes) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(axes) (*location,isr,ier,isc,iec);
}
 
ATUS(cconeimagemath) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(oneimagemath) (*location,isr,ier,isc,iec);
}
 
ATUS(ccdoatan2) (location,isr,ier,isc,iec,locb,jsr,jer,jsc,jec)
ADDRESS *location,*isr,*ier,*isc,*iec;
ADDRESS *locb,*jsr,*jer,*jsc,*jec;
{
  ATUS(doatan2) (*location,isr,ier,isc,iec,*locb,jsr,jer,jsc,jec);
}
 
ATUS(ccbaseline) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(baseline) (*location,nr,nc);
}
 
ATUS(ccclip) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(clip) (*location,isr,ier,isc,iec);
}
 
ATUS(ccrotate) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(rotate) (*location,isr,ier,isc,iec);
}
 
ATUS(ccflip) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(flip) (*location,isr,ier,isc,iec);
}
 
ATUS(ccfitspline) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(fitspline) (*location,isr,ier,isc,iec);
}
 
ATUS(ccispline) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(ispline) (*location,isr,ier,isc,iec);
}

ATUS(ccradplot) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(radplot) (*location,isr,ier,isc,iec);
}
 
ATUS(ccoverlay)(location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(overlay)(*location,isr,ier,isc,iec);
}
 
ATUS(ccmakehistogram) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(makehistogram) (*location,isr,ier,isc,iec);
}
 
ATUS(ccthreed) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(threed) (*location,isr,ier,isc,iec);
}
 
ATUS(ccvideo) (location,isr,ier,isc,iec,locb,isrb,ierb,iscb,iecb)
ADDRESS *location,*isr,*ier,*isc,*iec,*locb,*isrb,*ierb,*iscb,*iecb;
{
  ATUS(video) (*location,isr,ier,isc,iec,*locb,isrb,ierb,iscb,iecb);
}
 
ATUS(ccimpost) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(impost) (*location,isr,ier,isc,iec);
}
 
ATUS(ccbinanimage) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(binanimage) (*location,isr,ier,isc,iec);
}
 
ATUS(ccsurface) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(surface) (*location,isr,ier,isc,iec);
}
 
ATUS(ccshift) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(shift) (*location,isr,ier,isc,iec);
}
 
ATUS(ccpradial) (location,nrow,ncol)
ADDRESS *location,*nrow,*ncol;
{
  ATUS(pradial) (*location,nrow,ncol);
}

ATUS(ccdopradial)(a,nrow,ncol,sb,nn,maxwid,maxlen)
ADDRESS *a,*nrow,*ncol,*sb,*nn,*maxwid,*maxlen;
{
  ATUS(dopradial)(a,nrow,ncol,*sb,*nn,maxwid,maxlen);
}
 
ATUS(cctemplate) (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
ADDRESS *location,*isr,*ier,*isc,*iec;
ADDRESS *loc2,*isr2,*ier2,*isc2,*iec2;
{
  ATUS(template) (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
 
ATUS(ccrecon) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(recon) (*location,isr,ier,isc,iec);
}
 
ATUS(ccsmooth) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(smooth) (*location,nr,nc);
}
 
ATUS(cczap) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(zap) (*location,isr,ier,isc,iec);
}
 
ATUS(ccaperture) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(aperture) (*location,nr,nc);
}
 
ATUS(ccprofile) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(profile) (*location,nr,nc);
}
 
ATUS(ccannulus) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(annulus) (*location,isr,ier,isc,iec);
}
 
ATUS(cccrosscor) (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
ADDRESS *location,*isr,*ier,*isc,*iec;
ADDRESS *loc2,*isr2,*ier2,*isc2,*iec2;
{
  ATUS(crosscor) (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
 
ATUS(cccolfix) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(colfix) (*location,isr,ier,isc,iec);
}
 
ATUS(ccinterp) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(interp) (*location,isr,ier,isc,iec);
}
 
ATUS(ccmash) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(mash) (*location,isr,ier,isc,iec);
}
 
ATUS(ccextract) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(extract) (*location,isr,ier,isc,iec);
}
 
ATUS(ccstretch) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(stretch) (*location,isr,ier,isc,iec);
}
 
ATUS(ccspectroid) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(spectroid) (*location,isr,ier,isc,iec);
}

#ifdef HAVELINER
ATUS(ccliner) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(liner) (*location,isr,ier,isc,iec);
}
#endif
 
ATUS(cclineid) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(lineid) (*location,isr,ier,isc,iec);
}
 
ATUS(ccwavescale) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(wavescale) (*location,isr,ier,isc,iec);
}
 
ATUS(ccewave) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(ewave) (*location,isr,ier,isc,iec);
}
 
ATUS(cccopyw) (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
ADDRESS *location,*isr,*ier,*isc,*iec;
ADDRESS *loc2,*isr2,*ier2,*isc2,*iec2;
{
  ATUS(copyw) (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
 
ATUS(ccalign) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(align) (*location,isr,ier,isc,iec);
}
 
ATUS(ccextinct) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(extinct) (*location,isr,ier,isc,iec);
}
ATUS(ccrefract) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(refract) (*location,isr,ier,isc,iec);
}
 
ATUS(ccfluxcal) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(fluxcal) (*location,isr,ier,isc,iec);
}
 
ATUS(ccskyline) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(skyline) (*location,isr,ier,isc,iec);
}
 
ATUS(ccpolyfit) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(polyfit) (*location,isr,ier,isc,iec);
}
 
ATUS(ccextspec) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(extspec) (*location,isr,ier,isc,iec);
}
 
ATUS(ccfindpeak) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(findpeak) (*location,isr,ier,isc,iec);
}
 
ATUS(ccrowfit) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(rowfit) (*location,isr,ier,isc,iec);
}
 
ATUS(ccgfit) (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
ADDRESS *location,*isr,*ier,*isc,*iec;
ADDRESS *loc2,*isr2,*ier2,*isc2,*iec2;
{
  ATUS(gfit) (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
 
ATUS(ccfindshift) (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
ADDRESS *location,*isr,*ier,*isc,*iec;
ADDRESS *loc2,*isr2,*ier2,*isc2,*iec2;
{
  ATUS(findshift) (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
ATUS(ccloaderr)(location,b,isr,ier,isc,iec,sr,sc,ec)
ADDRESS *location, *b, *isr,*ier,*isc,*iec,*sr,*sc,*ec;
{
  ATUS(loaderr)(*location,b,isr,ier,isc,iec,sr,sc,ec);
}

/* BBSPEC routine, called by BLACKBODY command */

ATUS(ccbbspec)(locim,npix,T,lam0,disp,ierr)
int *locim, *npix, *T, *lam0, *disp, *ierr;
{
  ATUS(bbspec)(*locim,npix,T,lam0,disp,ierr);
}
 
ATUS(ccmarkstar) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(markstar) (*location,isr,ier,isc,iec);
}
 
#if defined(__SUNVIEW) || defined(__X11)
ATUS(ccarea) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  printf("calling area\n");
  ATUS(areag) (*location,isr,ier,isc,iec);
}
#endif /* SUNVIEW || X11 */
 
ATUS(ccphotons) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(photons) (*location,isr,ier,isc,iec);
}
 
ATUS(ccautomark) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(automark) (*location,isr,ier,isc,iec);
}
 
ATUS(ccaperstar) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(aperstar) (*location,isr,ier,isc,iec);
}
 
ATUS(ccaedit) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(aedit) (*location,isr,ier,isc,iec);
}
 
ATUS(ccemag) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(emag) (*location,nr,nc);
}
 
ATUS(ccgetwfpcad) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(getwfpcad) (*location,isr,ier,isc,iec);
}
 
ATUS(ccatodsim) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(atodsim) (*location,isr,ier,isc,iec);
}
 
ATUS(ccatodfix) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(atodfix) (*location,isr,ier,isc,iec);
}
 
ATUS(ccoverscan) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(overscan) (*location,isr,ier,isc,iec);
}
 
ATUS(ccfilter) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(filter) (*location,nr,nc);
}

ATUS(cchistnuc) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(histnuc) (*a,nr,nc);
}

ATUS(ccsimulnuc) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(simulnuc) (*location,isr,ier,isc,iec);
}

ATUS(ccbdfit) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(bdfit) (*location,isr,ier,isc,iec);
}
 
 
ATUS(ccrenuc) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(renuc) (*location,isr,ier,isc,iec);
}
 
ATUS(ccpolargrid) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(polargrid) (*location,isr,ier,isc,iec);
}
 
ATUS(ccrancr) (location,nr,nc,locb,nrowb,ncolb)
ADDRESS *location,*nr,*nc,*locb,*nrowb,*ncolb;
{
  ATUS(rancr) (*location,nr,nc,*locb,nrowb,ncolb);
}
 
ATUS(ccstreak) (location,nr,nc,locb,nrowb,ncolb)
ADDRESS *location,*nr,*nc,*locb,*nrowb,*ncolb;
{
  ATUS(streak) (*location,nr,nc,*locb,nrowb,ncolb);
}
 
ATUS(cctabtran) (location,nr,nc)
ADDRESS *location,*nr,*nc;
{
  ATUS(tabtran) (*location,nr,nc);
}
 
ATUS(ccdevauc) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(devauc) (*location,isr,ier,isc,iec);
}
 
ATUS(cctext) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(text) (*location,isr,ier,isc,iec);
}

ATUS(ccregistar) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(registar) (*location,isr,ier,isc,iec);
}

ATUS(ccspindex) (a,isra,iera,isca,ieca)
ADDRESS *a,*isra,*iera,*isca,*ieca;
{
  ATUS(spindex) (*a,isra,iera,isca,ieca);
}

ATUS(cclincomb) (a,isra,iera,isca,ieca)
ADDRESS *a,*isra,*iera,*isca,*ieca;
{
  ATUS(lincomb) (*a,isra,iera,isca,ieca);
}

ATUS(ccfquo) (a,isra,iera,isca,ieca,b,isrb,ierb,iscb,iecb)
ADDRESS *a,*isra,*iera,*isca,*ieca,*b,*isrb,*ierb,*iscb,*iecb;
{
  ATUS(fquo) (*a,isra,iera,isca,ieca,*b,isrb,ierb,iscb,iecb);
}

ATUS(ccrebin) (b,a,isr,ier,isc,iec,nbins,bin)
ADDRESS *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin;
{
  ATUS(rebin) (*b,a,isr,ier,isc,iec,nbins,bin);
}

ATUS(ccrebinlc) (b,a,isr,ier,isc,iec,nbins,bin,mod,isrm,ierm,iscm,iecm)
ADDRESS *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin,*mod,*isrm,*ierm,*iscm,*iecm;
{
  ATUS(rebinlc) (*b,a,isr,ier,isc,iec,nbins,bin,*mod,isrm,ierm,iscm,iecm);
}

ATUS(ccreal2dfft) (a,nra,nca,b,nrb,ncb,isign,work)
ADDRESS *a,*nra,*nca,*b,*nrb,*ncb,*isign,*work;
{
  ATUS(real2dfft) (*a,nra,nca,*b,nrb,ncb,isign,work);
}

ATUS(ccpows) (a,nra,nca,b,nrb,ncb)
ADDRESS *a,*nra,*nca,*b,*nrb,*ncb;
{
  ATUS(pows) (*a,nra,nca,*b,nrb,ncb);
}

ATUS(cccabs) (a,b,nr,nc)
ADDRESS *a,*b,*nr,*nc;
{
  ATUS(cabs) (*a,*b,nr,nc);
}

ATUS(ccconj) (a,b,nr,nc)
ADDRESS *a,*b,*nr,*nc;
{
  ATUS(conj) (*a,*b,nr,nc);
}

ATUS(ccreal) (a,b,nr,nc)
ADDRESS *a,*b,*nr,*nc;
{
  ATUS(real) (*a,*b,nr,nc);
}

ATUS(ccimag) (a,b,nr,nc)
ADDRESS *a,*b,*nr,*nc;
{
  ATUS(imag) (*a,*b,nr,nc);
}

ATUS(cccmplx) (a,b,c,nr,nc)
ADDRESS *a,*b,*c,*nr,*nc;
{
  ATUS(cmplx) (*a,*b,*c,nr,nc);
}

ATUS(cccmul) (a,isra,iera,isca,ieca,b,isrb,ierb,iscb,iecb)
ADDRESS *a,*isra,*iera,*isca,*ieca,*b,*isrb,*ierb,*iscb,*iecb;
{
  ATUS(cmul) (*a,isra,iera,isca,ieca,*b,isrb,ierb,iscb,iecb);
}

ATUS(cccdiv) (a,isra,iera,isca,ieca,b,isrb,ierb,iscb,iecb)
ADDRESS *a,*isra,*iera,*isca,*ieca,*b,*isrb,*ierb,*iscb,*iecb;
{
  ATUS(cdiv) (*a,isra,iera,isca,ieca,*b,isrb,ierb,iscb,iecb);
}

ATUS(ccautocen) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(autocen) (*a,nr,nc);
}

ATUS(ccsector) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(sector) (*a,nr,nc);
}

ATUS(ccegal) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(egal) (*a,nr,nc);
}

ATUS(ccpsfmodel) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(psfmodel) (*a,nr,nc);
}

ATUS(ccpsftrans) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(psftrans) (*a,nr,nc);
}

ATUS(ccellmag) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(ellmag) (*a,nr,nc);
}

ATUS(ccsurfbright) (a,nr,nc)
ADDRESS *a, *nr, *nc;
{
  ATUS(surfbright) (*a,nr,nc);
}


/*                         ***  SECTION 3  ***                          */
 
/* Now we come to the utility routines that pass things with virtual    */
/*          memory adresses                                             */
 
 
#ifdef __NEWLIB
ATUS(cclibfill) (location,nbox,nlib,gauss,psf,npsf,maxpsf,amin,amax,app,xpsf,ypsf)
ADDRESS *location,*nbox,*nlib,*gauss,*psf,*npsf,*maxpsf,*amin,*amax,*app,*xpsf,*ypsf;
{
  ATUS(libfill)(*location,nbox,nlib,gauss,psf,npsf,maxpsf,amin,amax,app,xpsf,ypsf);
}
 
ATUS(cclibget)(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy)
ADDRESS *location,*nbox,*nlib,*ix,*iy,*x,*y,*value,*dvdx,*dvdy;
{
  ATUS(libget)(*location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy);
}
 
ATUS(cclibmul)(location,nbox,fact)
ADDRESS *location,*nbox,*fact;
{
  ATUS(libmul)(*location,nbox,fact);
}
 
ATUS(ccliball)(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte)
ADDRESS *location;
int *nbox,*nlib,*ix,*iy,*x,*y,*nall,*nbyte;
float *value,*dvdx,*dvdy;
{
  int i;

/*  for (i=0;i<*nall;i++) {
    printf("%d %d %d\n",i,location,*location+i*(*nbyte));
    printf("%d %d %d\n",i,value,value+i);
  } */
  for (i=0;i<*nall;i++) {
   ATUS(libget)(*location+i*(*nbyte),nbox,nlib,ix,iy,x,y,value+i,dvdx+i,dvdy+i);
  }
}

ATUS(cclibone)(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte,nsamp,offset,nskip)
int **location,*nbox,*nlib,*ix,*iy,*nall,*nbyte,*offset,*nsamp,*nskip;
float *value,*x,*y,*dvdx,*dvdy;
{
  int nside, ixfrac, iyfrac, ixpix, iypix;
  float spacing;

  *nskip = 0;
  if (*nsamp>1) {
    nside = *nbox / *nlib ;
    spacing = 1. / *nlib ;
    ixfrac = (*x - (int)*x) / spacing ;
    ixpix = (*ix - (int)*x + nside/2) * (*nlib) + (*nlib - ixfrac);
    if ( ixpix<2 || ixpix>(*nbox) ) {
      *nskip = 1;
    } else {
      iyfrac = (*y - (int)*y) / spacing ;
      iypix = (*iy - (int)*y + nside/2) * (*nlib) + (*nlib - iyfrac);
      if ( iypix<2 || iypix>(*nbox) ) 
        *nskip = 1;
    }
  } 
  ATUS(libget)(*location+*offset*(*nbyte/4)+(*nskip)*(*nbyte/4)*(*nall),nbox,nlib+(*nskip),ix,iy,x,y,value,dvdx,dvdy); 
}

ATUS(cclibnew)(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte,nsamp,ixpsf,iypsf,nskip,deriv,error)
int *nbox,*nlib,*ix,*iy,*nall,*nbyte,*nsamp,*nskip,*deriv,*error;
short *ixpsf, *iypsf;
float **location,*value,*x,*y,*dvdx,*dvdy;
{
/*  Function to retrieve 4 PSF values from big array which has integrated
     values for multiple centerings (1st two dimensions), multiple field
     angle (3rd dimension), and multiple samplings (4th dimension) */

  int nside, ixfrac, iyfrac, ixpix, iypix, iypix2;
  int iix, iiy, iii, offset, o1, o2, o3, o4, o5, o6, nb4, tmp;
  float spacing,xfrac,yfrac,val1,val2,*ltmp,*ltmp2,*ltmp4;
  double sqrt();

  *nskip = 0;
  nb4 = *nbyte / 4;
  *error = 0;

/*  First get the proper pixel centering indices, ixpix and iypix */
  spacing = 1. / *nlib ;
  nside = *nbox * spacing ;
  nside = nside / 2 ;
  xfrac = (*x - (int)*x) * (*nlib) ;
  ixfrac = (int) xfrac ;
  ixpix = (*ix - (int)*x + nside) * (*nlib) + (*nlib - ixfrac);

  /* If this is larger than nbox, we'll use the lower sampling */
  if ( ixpix<2 || ixpix>(*nbox) ) {
    *nskip = 1;
  } else {
    /* Now go on and do iypix */
    yfrac = (*y - (int)*y) * (*nlib) ;
    iyfrac = (int) yfrac;
    iypix = (*iy - (int)*y + nside) * (*nlib) + (*nlib - iyfrac);
    if ( iypix<2 || iypix>(*nbox) ) 
      *nskip = 1;
  }

/*  If were using the lower sampling, get the proper indices for this case */
  if (*nskip == 1) {
    spacing = 1. / *(nlib+1) ;
    nside = *nbox * spacing ;
    nside = nside / 2 ;
    xfrac = (*x - (int)*x) * *(nlib+1) ;
    ixfrac = (int) xfrac ;
    ixpix = (*ix - (int)*x + nside) * (*(nlib+1)) + (*(nlib+1) - ixfrac);
    if ( (ixpix<2) || (ixpix>(*nbox)) ) {
        printf("%f %d %d %d %d %d %d\n",
               *x,*(nlib+1),*ix,nside,ixfrac,ixpix,*nbox);
        printf("x Pixel out of library range\n");
        *error = 1;
        for (iii=0;iii<4;iii++) {
          *(value+iii) = 0.;
          *(dvdx+iii) = 0.;
          *(dvdy+iii) = 0.;
        }
        return(-1);
    } else {
      /* if ixpix is within range, compute iypix */
      yfrac = (*y - (int)*y) * *(nlib+1) ;
      iyfrac = (int) yfrac;
      iypix = (*iy - (int)*y + nside) * (*(nlib+1)) + (*(nlib+1) - iyfrac);
      if ( (iypix<2) || (iypix>(*nbox)) ) { 
        printf("y Pixel out of library range\n");
	printf("%f %d %d %d %d %d %d\n",
               *y,*(nlib+1),*iy,nside,iyfrac,iypix,*nbox);
        *error = 1;
        for (iii=0;iii<4;iii++) {
          *(value+iii) = 0.;
          *(dvdx+iii) = 0.;
          *(dvdy+iii) = 0.;
        }
        return(-1);
      }
    }
/*    printf("%f %f %d %d %d %d %d %d %d %d %d\n",
     *x,*y,*(nlib+1),*ix,*iy,nside,ixfrac,iyfrac,ixpix,iypix,*nbox); */
  }

/*  Now compute the addresses of the four surrounding pixels relative to
    the start of the individual PSF */
    o1 = (iypix-1)*(*nbox)+(ixpix-2) ;
    o2 = o1 + 1 ;
    o3 = o1 - *nbox;
    o4 = o3 + 1 ;

/*  Offset gives the address of the beginning of the individual PSF */
    iii = 0;
    tmp = (int)sqrt((float)*nall);
    offset = (*nskip)*nb4*(*nall);
    offset += (*ixpsf + *iypsf*tmp ) * nb4;
    iypix2 = iypix ;
    if ((yfrac - iyfrac) >= 0.5) iypix2--;
    if (*deriv == 1) {
      o5 = (iypix2-1)*(*nbox)+(ixpix-2) ;
      o6 = o5 + 1;
    }

/*  xfrac, yfrac give the relative weights for different pixel centerings */
    xfrac = xfrac - ixfrac ;
    yfrac = yfrac - iyfrac ;

/*  Now go and get the four surrounding values. Do the pixel centering
     intepolation, but return four values for 4 different surrounding
     field locations (since the interpolating values for these dont need
     to be redetermined for every pixel */
    for (iiy=0;iiy<2;iiy++) {
       for (iix=0;iix<2;iix++) {
/*          offset = *ixpsf+iix + (*iypsf + iiy)*(int)sqrt((float)*nall); */
/*          offset = offset*nb4+(*nskip)*nb4*(*nall);    */
          ltmp = *location + offset;
          ltmp2 = ltmp + o2;
          ltmp4 = ltmp + o4;
          val1 = *(ltmp+o1) - *ltmp2;
          val1 = val1*xfrac + *ltmp2;
          val2 = *(ltmp+o3) - *ltmp4; 
          val2 = val2*xfrac + *ltmp4;
          *(value+iii) = (val2 - val1)*yfrac + val1;
          if (*deriv == 1) {
            *(dvdy+iii) = val2 - val1 ;
            *(dvdx+iii) = *(ltmp+o5) - *(ltmp+o6);
          }
          iii++;
          offset += nb4;
       }
/* Now increment iiy, but decrement iix (that's the -2) */
       offset += (tmp-2)*nb4 ;
     }
}

#ifdef PSFMODE10
ATUS(ccfilllib)(locpsf,locfft,smcent,smadj,nlib,wj,nwx,nwy)
ADDRESS *locpsf,*locfft,*smcent,*smadj,*nlib,*wj,*nwx,*nwy;
{
  ATUS(filllib)(*locpsf,*locfft,smcent,smadj,nlib,wj,nwx,nwy);
}
#endif

ATUS(ccgetdata)(location,ix,iy,nx,ny,data)
int *ix,*iy,*nx,*ny;
float **location;
float *data;
{
   *data = *(*location+(*iy-1)*(*nx)+(*ix-1));
}

ATUS(ccputdata)(location,ix,iy,nx,ny,data)
int *ix,*iy,*nx,*ny;
float **location;
float *data;
{
   *(*location+(*iy-1)*(*nx)+(*ix-1)) = *data;
}

ATUS(cclibint)(location,location2,npsflib,nlib,psfrad,amin,amax)
ADDRESS *location,*location2,*npsflib,*nlib,*psfrad,*amin,*amax;
{
  ATUS(libint)(*location,*location2,npsflib,nlib,psfrad,amin,amax);
}
 
ATUS(cclibput)(location,nbox,nlib,ix,iy,x,y,value,amin,amax)
ADDRESS *location,*nbox,*nlib,*ix,*iy,*x,*y,*value,*amin,*amax;
{
  ATUS(libput)(*location,nbox,nlib,ix,iy,x,y,value,amin,amax);
}

#else
ATUS(ccmakepsflib) (location,isr,ier,isc,iec)
ADDRESS *location,*isr,*ier,*isc,*iec;
{
  ATUS(makepsflib) (*location,isr,ier,isc,iec);
}

ATUS(ccimfrpsf) (loc,isr,ier,isc,iec)
ADDRESS *loc,*isr,*ier,*isc,*iec;
{
  ATUS(imfrpsf) (*loc,isr,ier,isc,iec);
}
 
ATUS(ccimtopsf) (loc,isr,ier,isc,iec)
ADDRESS *loc,*isr,*ier,*isc,*iec;
{
  ATUS(imtopsf) (*loc,isr,ier,isc,iec);
}
#endif
 
ATUS(cceqwindex) (b,a,nra,nca,bands,nb,zp1,dp)
ADDRESS *b,*a,*nra,*nca,*bands,*nb,*zp1,*dp;
{
  ATUS(eqwindex) (*b,a,nra,nca,bands,nb,zp1,dp);
}

ATUS(cceqwindexv) (b,a,nra,nca,bands,nb,zp1,dp,v)
ADDRESS *b,*a,*nra,*nca,*bands,*nb,*zp1,*dp,*v;
{
  ATUS(eqwindexv) (*b,a,nra,nca,bands,nb,zp1,dp,*v);
}

/* end of the new ones from Rick */
 
#ifdef  __OLD_STUFF
ATUS(ccrdtapedata) (a,nunit,fitshead,byteswap,error,nchar)
ADDRESS *a,*nunit,*fitshead,*byteswap,*error;
ADDRESS nchar;
{
  ATUS(rdtapedata) (*a,nunit,fitshead,byteswap,error,nchar);
}
#endif  /* OLD_STUFF */
 
ATUS(cccopio) (location,nr2,nc2,spa,nr,nc,ii,jj)
ADDRESS *location,*nr2,*nc2,*nc,*nr,*ii,*jj,*spa;
{
  ATUS(copio) (*location,nr2,nc2,spa,nr,nc,ii,jj);
}
 
ATUS(cccopio2) (location,nr2,nc2,spa,nr,nc,ii,jj)
ADDRESS *location,*nr2,*nc2,*nc,*nr,*ii,*jj,*spa;
{
  ATUS(copio2) (*location,nr2,nc2,spa,nr,nc,ii,jj);
}
 
ATUS(cc1copio) (location,nr2,nc2,loc2,nr,nc,ii,jj)
ADDRESS *location,*nr2,*nc2,*nc,*nr,*loc2,*ii,*jj;
{
  ATUS(copio) (*location,nr2,nc2,*loc2,nr,nc,ii,jj);
}
 
ATUS(cc2copio) (location,nr2,nc2,loc2,nr,nc,ii,jj)
ADDRESS *location,*nr2,*nc2,*nc,*nr,*loc2,*ii,*jj;
{
  ATUS(copio) (location,nr2,nc2,*loc2,nr,nc,ii,jj);
}
 
ATUS(ccbinit) (a,isr,ier,isc,iec,bsr,bxc,loc,nr,nc,dr,dc,normal,sample)
ADDRESS *a,*isr,*ier,*isc,*iec,*bsr,*bxc,*loc,*nr,*nc,*dr,*dc,*normal,*sample;
{
  ATUS(binit) (a,isr,ier,isc,iec,bsr,bxc,*loc,nr,nc,dr,dc,normal,sample);
}
 
ATUS(ccrepit) (a,isr,ier,isc,iec,bsr,bxc,loc,nr,nc,dr,dc,normal,sample)
ADDRESS *a,*isr,*ier,*isc,*iec,*bsr,*bxc,*loc,*nr,*nc,*dr,*dc,*normal,*sample;
{
  ATUS(repit) (a,isr,ier,isc,iec,bsr,bxc,*loc,nr,nc,dr,dc,normal,sample);
}

ATUS(ccdocross) (a,asr,aer,asc,aec,b,bsr,ber,bsc,bec,loc,isr,ier,isc,iec,isrow,ierow,
iscol,iecol)
ADDRESS *a,*asr,*aer,*asc,*aec,*b,*bsr,*ber,*bsc,*bec,*loc,*isr,*ier,*isc,*iec;
ADDRESS *isrow,*ierow,*iscol,*iecol;
{
  ATUS(docross) (a,asr,aer,asc,aec,b,bsr,ber,bsc,bec,*loc,isr,ier,isc,iec,isrow,ierow,
	       iscol,iecol);
}
 
ATUS(ccfindlimits) (loc,isr,ier,isc,iec,pmin,pmax,haveblank,fblank)
ADDRESS *loc,*isr,*ier,*isc,*iec,*pmin,*pmax,*haveblank,*fblank;
{
  ATUS(findlimits) (*loc,isr,ier,isc,iec,pmin,pmax,haveblank,fblank);
}
 
ATUS(ccextractsky) (a,isr,ier,isc,iec,sr,er,ns,skyord,loc)
ADDRESS *a,*isr,*ier,*isc,*iec,*sr,*er,*ns,*skyord,*loc;
{
  ATUS(extractsky) (a,isr,ier,isc,iec,sr,er,ns,skyord,*loc);
}
 
ATUS(ccoptimal) (a,isr,ier,isc,iec,locsky,locpro,locv,locspec,locsvar,locmask,
   sr,er,pro)
ADDRESS *a,*isr,*ier,*isc,*iec,*locsky,*locpro,*locv,*locspec,*locsvar,*locmask;
ADDRESS *sr, *er, *pro;
{
  ATUS(optimal) (a,isr,ier,isc,iec,*locsky,*locpro,*locv,*locspec,*locsvar,*locmask,
   sr,er,pro);
}
 
ATUS(ccsubsky) (a,isr,ier,isc,iec,loc)
ADDRESS *a, *isr,*ier,*isc,*iec,*loc;
{
  ATUS(subsky) (a,isr,ier,isc,iec,*loc);
}
 
ATUS(ccflipinnrows) (a,loc,isr,ier,isc,iec)
ADDRESS *a,*loc,*isr,*ier,*isc,*iec;
{
  ATUS(flipinnrows) (a,*loc,isr,ier,isc,iec);
}
 
ATUS(ccflipinncols) (a,loc,isr,ier,isc,iec)
ADDRESS *a,*loc,*isr,*ier,*isc,*iec;
{
  ATUS(flipinncols) (a,*loc,isr,ier,isc,iec);
}
 
ATUS(ccfillfrommask) (loc,sr,er,sc,ec,zero,fill)
ADDRESS *loc,*sr,*er,*sc,*ec,*zero,*fill;
{
  ATUS(fillfrommask) (*loc,sr,er,sc,ec,zero,fill);
}

ATUS(ccloadmask)(iunit,loc,nwords,fortran)
ADDRESS *loc, *nwords, *iunit, *fortran;
{
  int nbytes;
  if (*fortran ==1)
    ATUS(loadmask)(iunit,*loc,nwords);
  else {
    nbytes = 4 * *nwords;
    ATUS(readint)(iunit,*loc,&nbytes);
  }
}
 
ATUS(ccwritemask)(ifile,loc,nbytes)
ADDRESS *loc, *nbytes, *ifile;
{
   ATUS(writeint)(ifile,*loc,nbytes);
}
 
ATUS(ccmeanofimage) (loc,nr,nc)
ADDRESS *loc, *nr, *nc;
{
  ATUS(meanofimage) (*loc,nr,nc);
}
 
ATUS(ccdomerge) (locm,locw,nr,nc,next,sr,er,sc,ec,nozero,i)
ADDRESS *locm, *locw, *nr, *nc, *next, *sr, *er, *sc, *ec, *nozero, *i;
{
  ATUS(domerge) (*locm,*locw,nr,nc,*next,sr,er,sc,ec,nozero,i);
}

ATUS(ccmedian) (loc,npts,fmedia,perc)
ADDRESS *loc, *npts, *fmedia, *perc;
{
  ATUS(median)(*loc,npts,fmedia,perc);
}
 
ATUS(cclibread) (loc,nside,maxlib,npsf,ipsf)
ADDRESS *loc, *nside,*maxlib,*npsf,*ipsf;
{
  ATUS(libread) (*loc,nside,maxlib,npsf,ipsf);
}
 
ATUS(ccdpsfall) (loc,nside,maxlib,npsf,col,row,ixpix,iypix,val,dvdx,dvdy)
ADDRESS *loc,*nside,*maxlib,*npsf,*col,*row,*ixpix,*iypix,*val,*dvdx,*dvdy;
{
  ATUS(dpsfall) (*loc,nside,maxlib,npsf,col,row,ixpix,iypix,val,dvdx,dvdy);
}
 
ATUS(ccloadvector) (loc,isr,ier,isc,iec,yplot,nplot,ir1,ir2,ic1,ic2,rowmash,norm)
ADDRESS *loc,*isr,*ier,*isc,*iec,*yplot,*nplot,*ir1,*ir2,*ic1,*ic2,*rowmash,*norm;
{
  ATUS(loadvector) (*loc,isr,ier,isc,iec,yplot,nplot,ir1,ir2,ic1,ic2,rowmash,norm);
}
 
ATUS(ccprintimage) (loc,isr,ier,isc,iec,ncmx,nsig,user,haveform,formstr,nc)
ADDRESS *loc,*isr,*ier,*isc,*iec,*ncmx,*nsig,*user,*haveform,*formstr;
ADDRESS nc;
{
  ATUS(printimage) (*loc,isr,ier,isc,iec,ncmx,nsig,user,haveform,formstr,nc);
}
 
ATUS(ccrot90) (a,loc,nr,nc)
ADDRESS *a, *loc, *nr, *nc;
{
  ATUS(rot90) (a,*loc,nr,nc);
}
 
ATUS(ccrot180) (a,loc,nr,nc)
ADDRESS *a, *loc, *nr, *nc;
{
  ATUS(rot180) (a,*loc,nr,nc);
}
 
ATUS(ccrot270) (a,loc,nr,nc)
ADDRESS *a, *loc, *nr, *nc;
{
  ATUS(rot270) (a,*loc,nr,nc);
}
 
ATUS(cctranspose) (a,loc,nr,nc)
ADDRESS *a, *loc, *nr, *nc;
{
  ATUS(transpose) (a,*loc,nr,nc);
}
 
ATUS(ccrotateit) (a,isr,ier,isc,iec,loc,jsr,jer,jsc,jec,pa,interp,row0,col0)
ADDRESS *a,*isr,*ier,*isc,*iec,*loc,*jsr,*jer,*jsc,*jec,*pa,*interp,*row0,*col0;
{
  ATUS(rotateit) (a,isr,ier,isc,iec,*loc,jsr,jer,jsc,jec,pa,interp,row0,col0);
}
 
ATUS(ccunloadfit1) (a,isr,ier,isc,iec,lpf,isx,iex,isy,iey,ncoef,oper,loc,jsy,jey)
ADDRESS *a,*isr,*ier,*isc,*iec,*lpf,*isx,*iex,*isy,*iey,*ncoef,*oper,*loc;
ADDRESS *jsy,*jey;
{
  ATUS(unloadfit1) (a,isr,ier,isc,iec,*lpf,isx,iex,isy,iey,ncoef,oper,*loc,jsy,jey);
}
 
ATUS(ccloadfit1) (a,isr,ier,isc,iec,lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,load,
  y,loc,clip,fclip)
ADDRESS *a,*isr,*ier,*isc,*iec,*lpf,*isx,*iex,*isy,*iey,*ncoef,*oper,*loc;
ADDRESS *lim,*nlim,*load,*y,*clip,*fclip;
{
  ATUS(loadfit1) (a,isr,ier,isc,iec,*lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,load,
    y,*loc,clip,fclip);
}
 
ATUS(ccloadfit2)(a,isr,ier,isc,iec,lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,
  load,y,loc,clip,fclip,w,isw,iew)
ADDRESS *a,*isr,*ier,*isc,*iec,*lpf,*isx,*iex,*isy,*iey,*ncoef,*oper,*loc;
ADDRESS *lim,*nlim,*load,*y,*clip,*fclip,*w,*isw,*iew;
{
  ATUS(loadfit2) (a,isr,ier,isc,iec,*lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,
      load,y,*loc,clip,fclip,*w,isw,iew);
}

ATUS(ccpfunc)(lpf,x,ncoef,nx,i,j,ifunc)
ADDRESS *lpf,*x,*ncoef,*nx,*i,*j,*ifunc;
{
  ATUS(pfunc) (*lpf,x,ncoef,nx,i,j,ifunc);
}
 
ATUS(ccshiftitbymod) (a,loc,nr,nc,rshift,cshift,rmodel,cmodel,mode)
ADDRESS *a,*loc,*nr,*nc,*rshift,*cshift,*rmodel,*cmodel,*mode;
{
  ATUS(shiftitbymod) (a,*loc,nr,nc,rshift,cshift,rmodel,cmodel,mode);
}
 
ATUS(ccshiftit) (a,loc,nr,nc,dr,dc,mode)
ADDRESS *a,*loc,*nr,*nc,*dr,*dc,*mode;
{
  ATUS(shiftit) (a,*loc,nr,nc,dr,dc,mode);
}
 
ATUS(ccbshiftit)(a,loc,nr,nc,dr,dc,blanking,blank)
ADDRESS *a,*loc,*nr,*nc,*dr,*dc,*blanking,*blank;
{
  ATUS(bshiftit)(a,*loc,nr,nc,dr,dc,blanking,blank);
}
 
ATUS(ccstretchit) (sp,nspec,loc,nr,nc,vert)
ADDRESS *sp,*nspec,*loc,*nr,*nc,*vert;
{
  ATUS(stretchit) (sp,nspec,*loc,nr,nc,vert);
}
 
ATUS(ccfill) (loc,nc,nr,a,isr,ier,isc,iec,sr,er,sc,ec)
ADDRESS *loc,*nc,*nr,*a,*isr,*ier,*isc,*iec,*sr,*er,*sc,*ec;
{
  ATUS(fill) (*loc,nc,nr,a,isr,ier,isc,iec,sr,er,sc,ec);
}
 
/*
ATUS(ccfourier_TUS (loc,nr,nc,locold,nrold,ncold)
ADDRESS *loc,*nr,*nc,*locold,*nrold,*ncold;
{
  ATUS(fourier_TUS (*loc,nr,nc,*locold,nrold,ncold);
}
*/
 
ATUS(ccgetthepixel) (loc,nc,nr,col,row,pix)
ADDRESS *loc,*nc,*nr,*col,*row,*pix;
{
  ATUS(getthepixel) (*loc,nc,nr,col,row,pix);
}
 
ATUS(ccgetcol) (loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one)
ADDRESS *loc,*isr,*ier,*isc,*iec,*func,*nx,*lx,*mx,*irow,*one;
{
  ATUS(getcol) (*loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one);
}
 
ATUS(cczapthatsucker) (a,isr,ier,isc,iec,loc,sr,er,sc,ec,rsize,csize,sig,perc,tty,noisemod,gain,rn,positive)
ADDRESS *a,*isr,*ier,*isc,*iec,*loc,*sr,*er,*sc,*ec,*rsize,*csize,*sig,*perc,*tty,*noisemod,*gain,*rn,*positive;
{
  ATUS(zapthatsucker) (a,isr,ier,isc,iec,*loc,sr,er,sc,ec,rsize,csize,sig,perc,tty,noisemod,gain,rn,positive);
}
 
ATUS(ccsaturate) (loc,isc,iec,isr,ier,col,row,satur,nbox,sat,maskrad)
ADDRESS *loc,*isc,*iec,*isr,*ier,*col,*row,*satur,*nbox,*sat,*maskrad;
{
  ATUS(saturate) (*loc,isc,iec,isr,ier,col,row,satur,nbox,sat,maskrad);
}
 
ATUS(ccround) (a,loc,nsr,ner,nsc,nec,ellip,pa,xc,yc,invert)
ADDRESS *a,*loc,*nsr,*ner,*nsc,*nec,*ellip,*pa,*xc,*yc,*invert;
{
  ATUS(round) (a,*loc,nsr,ner,nsc,nec,ellip,pa,xc,yc,invert);
}
 
ATUS(ccresam) (b,a,isr,ier,isc,iec,nbins,bin)
ADDRESS *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin;
{
  ATUS(resam) (*b,a,isr,ier,isc,iec,nbins,bin);
}
 
ATUS(ccresamlc) (b,a,isr,ier,isc,iec,nbins,bin,mod,isrm,ierm,iscm,iecm)
ADDRESS *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin,*mod,*isrm,*ierm,*iscm,*iecm;
{
  ATUS(resamlc) (*b,a,isr,ier,isc,iec,nbins,bin,*mod,isrm,ierm,iscm,iecm);
}

ATUS(ccloadhistbuf) (loc,x,y,n)
ADDRESS *loc, *x, *y, *n;
{
  ATUS(loadhistbuf) (*loc,x,y,n);
}

ATUS(ccdobigmed)(a,isr,ier,isc,iec,ifile,havebias,mean,bitpix,bzero,bscale,nfiles,sig,wfpc,havelowbad,lowbad,havehighbad,highbad)
ADDRESS *a,*isr,*ier,*isc,*iec,*ifile,*havebias,*mean,*bitpix,*bzero,*bscale;
ADDRESS *nfiles,*sig,*wfpc;
ADDRESS *havelowbad,*lowbad,*havehighbad,*highbad;
{
  ATUS(dobigmed) (*a,isr,ier,isc,iec,ifile,havebias,mean,bitpix,bzero,bscale,nfiles,sig,wfpc,havelowbad,lowbad,havehighbad,highbad);
}

ATUS(ccdopiccrs)(locim,loc1,loc2,imsr,imer,imsc,imec,ifile,ofile,havebias,mean,
bitpix,bzero,bscale,nfiles,fits,loca,locb,locc,locd,flag,neigh,bad,pflag,nts,
lines,n1,n2,n3)
ADDRESS *locim,*loc1,*loc2,*imsr,*imer,*imsc,*imec,*ifile,*ofile,*havebias,*mean;
ADDRESS *bitpix, *bzero, *bscale, *nfiles, *fits;
ADDRESS *loca,*locb,*locc,*locd,*n1,*n2,*n3;
ADDRESS *flag,*neigh,*bad,*pflag,*nts,*lines;
{
  ATUS(dopiccrs)(*locim,*loc1,*loc2,imsr,imer,imsc,imec,ifile,ofile,
havebias,mean, bitpix,bzero,bscale,nfiles,fits,
*loca,*locb,*locc,*locd,*flag,*neigh,*bad,*pflag,*nts,*lines,n1,n2,n3);
}

ATUS(ccwfpc2un)(locim,isr,ier,isc,iec,c,d,loc1,loc2,loc3,loc4)
ADDRESS *locim, *isr, *ier, *isc, *iec, *c, *d, *loc1, *loc2, *loc3, *loc4;
{
  ATUS(wfpc2un)(*locim,isr,ier,isc,iec,c,d,*loc1,*loc2,*loc3,*loc4);
}

ATUS(ccconvolveit)(locnew,jsr,jer,jsc,jec,locim,isr,ier,isc,iec,a,nrow,ncol)
ADDRESS *locnew,*jsr,*jer,*jsc,*jec,*locim,*isr,*ier,*isc,*iec,*a,*nrow,*ncol;
{
ATUS(convolveit)(*locnew,jsr,jer,jsc,jec,*locim,isr,ier,isc,iec,a,nrow,ncol);
}

ATUS(ccsumimage) (location,isr,ier,isc,iec,sr,er,sc,ec,sum,maxpix)
ADDRESS *location,*isr,*ier,*isc,*iec,*sr,*er,*sc,*ec,*sum,*maxpix;
{
  ATUS(sumimage)(*location,isr,ier,isc,iec,sr,er,sc,ec,sum,maxpix);
}

ATUS(ccmpgcont) (location,nc,nr,psc,pec,psr,pper,alev,is,tr)
ADDRESS *location,*nc,*nr,*psc,*pec,*psr,*pper,*alev,*is,*tr;
{
  ATUS(mpgcont) (*location,nc,nr,psc,pec,psr,pper,alev,is,tr);
}

ATUS(ccmpgcons) (location,nc,nr,psc,pec,psr,pper,alev,is,tr)
ADDRESS *location,*nc,*nr,*psc,*pec,*psr,*pper,*alev,*is,*tr;
{
  ATUS(mpgcons) (*location,nc,nr,psc,pec,psr,pper,alev,is,tr);
}

 
/*                   ***   SECTION 4   ***                               */
 
/* The following routines have character strings among their arguments.  */
/* These have to be handled differently for UNIX and for VMS.            */
 
/* On the Sun, a pointer to the location of the character string is passed */
/* in the FORTRAN slot in the call. In addition, the length of the character */
/* string is passed BY VALUE as the last argument in the parameter list. */
/* If there are more than one character strings, then the lengths are passed */
/* in the same order at the end of the parameter list as the order of the */
/* pointers to the addresses in the call statement                        */
 
/* On VMS, FORTRAN passes an address of a string descriptor to the subroutines*/
/* Since this is just an address, it can generally just be passes right back */
/* to the FORTRAN subroutine.                                             */
 
#ifdef __UNIX
ATUS(ccmathimagecon) (location,isr,ier,isc,iec,loc2,jsr,jer,jsc,jec,com,constant,nc)
ADDRESS *location,*isr,*ier,*isc,*iec,*loc2,*jsr,*jer,*jsc,*jec,*com;
ADDRESS nc;
float *constant;
{
  ATUS(mathimagecon) (*location,isr,ier,isc,iec,*loc2,jsr,jer,jsc,jec,
		  com,constant,nc);
}
 
ATUS(ccmathwithimag) (location,isr,ier,isc,iec,loc2,jsr,jer,jsc,jec,com,nc)
ADDRESS *location,*isr,*ier,*isc,*iec,*loc2,*jsr,*jer,*jsc,*jec,*com;
long nc;
{
  ATUS(mathwithimag) (*location,isr,ier,isc,iec,*loc2,jsr,jer,jsc,jec,
		  com,nc);
}
 
ATUS(ccmathwithcon) (location,isr,ier,isc,iec,com,constant,nc)
ADDRESS *location,*isr,*ier,*isc,*iec,*com;
float *constant;
ADDRESS nc;
{
  ATUS(mathwithcon) (*location,isr,ier,isc,iec,com,constant,nc);
}
 
#ifdef  __OLD_STUFF
ATUS(ccdiskio) (loc,isr,ier,isc,iec,ints,headnew,bzero,bscale,nc)
ADDRESS *loc,*isr,*ier,*isc,*iec,*ints,*headnew,*bzero,*bscale;
ADDRESS nc;
{
  ATUS(diskio) (*loc,isr,ier,isc,iec,ints,headnew,bzero,bscale,nc);
}
#endif  /* OLD_STUFF */
 
ATUS(ccrdtape) (nunit,imtape,zero,loc,nr,nc,headnew,xerr,nchar)
ADDRESS *nunit,*imtape,*zero,*loc,*nr,*nc,*headnew,*xerr;
ADDRESS nchar;
{
  ATUS(rdtape) (nunit,imtape,zero,*loc,nr,nc,headnew,xerr,nchar);
}

/* 
ATUS(ccdaophot1) (loc,max,msg,locim,isr,ier,isc,iec,locb,isrb,ierb,iscb,iecb,nc)
ADDRESS *loc,*max,*msg,*locim,*isr,*ier,*isc,*iec,*locb,*isrb,*ierb,*iscb,*iecb;
ADDRESS nc;
{
  ATUS(daophot1) (*loc,max,msg,locim,isr,ier,isc,iec,locb,isrb,ierb,iscb,iecb,nc);
}
*/

ATUS(ccsubstarv) (locim,isr,ier,isc,iec)
ADDRESS *locim,*isr,*ier,*isc,*iec;
{
  ATUS(substarv) (*locim,isr,ier,isc,iec);
}

ATUS(ccsubphot) (locb,jsr,jer,jsc,jec,locim,isr,ier,isc,iec)
ADDRESS *locim,*isr,*ier,*isc,*iec;
ADDRESS *locb,*jsr,*jer,*jsc,*jec;
{
  ATUS(subphot) (*locb,jsr,jer,jsc,jec,*locim,isr,ier,isc,iec);
}

ATUS(ccpkfit)(f,nbox,nbox2,maxbox,scale,x,y,sky,radius,lowbad,highbad,gauss,psf,npsf,errmag,chi,sharp,niter,location,npsflib,nlib)
ADDRESS *f,*nbox,*nbox2,*maxbox,*scale,*x,*y,*sky,*radius,*lowbad,*highbad;
ADDRESS *gauss,*psf,*npsf,*errmag,*chi,*sharp,*niter,*location;
ADDRESS *npsflib,*nlib;
{
  ATUS(pkfit)(f,nbox,nbox2,maxbox,scale,x,y,sky,radius,lowbad,highbad,gauss,psf,npsf,errmag,chi,sharp,niter,*location,npsflib,nlib);
 }


ATUS(ccunfit) (inchar,loc,nchar)
ADDRESS *inchar, *loc;
ADDRESS nchar;
{
  ATUS(unfit) (inchar,*loc,nchar,*(loc+1));
}
 
ATUS(ccheader) (inchar,loc,index,nchar)
ADDRESS *inchar,*loc,*index;
ADDRESS nchar;
{
  ATUS(header) (inchar,*loc,index,nchar,*(loc+1));
}
 
ATUS(ccprfits) (loc,inchar,full,nchar)
ADDRESS *loc,*inchar,*full;
ADDRESS nchar;
{
  ATUS(prfits) (*loc,inchar,full,*(loc+1),nchar);
}
 
ATUS(ccsetpoly) (loc,a,b,c,d)
ADDRESS *loc,*a,*b,*c,*d;
{
  ATUS(setpoly) (*loc,a,b,c,d,*(loc+1));
}
 
ATUS(ccesetpoly) (loc,a,b,c,d,e)
ADDRESS *loc,*a,*b,*c,*d,*e;
{
  ATUS(esetpoly) (*loc,a,b,c,d,e,*(loc+1));
}
 
ATUS(ccegetpoly) (loc,a,b,c,d)
ADDRESS *loc,*a,*b,*c,*d;
{
  ATUS(egetpoly) (*loc,a,b,c,d,*(loc+1));
}

#ifdef __READLINE
ATUS(ccaddhistory) (string,l,ls)
char *string;
int *l;
ADDRESS ls;
{
  *(string+*l) = 0;
  add_history(string);
  *(string+*l) = ' ';
}
#endif
 
#endif  /* UNIX */
 
/*  Now we do the VMS versions. These come in two classes: the routines    */
/*   that pass an address of a character descriptor and just want this    */
/*   address returned, which is fairly trivial, and those routines which  */
/*   pass a pointer to the address of the string and want a string        */
/*   descriptor returned. We do the first type first.                     */
 
#ifdef VMS
 
ccmathimagecon(location,isr,ier,isc,iec,loc2,jsr,jer,jsc,jec,com,constant)
int *location,*isr,*ier,*isc,*iec,*loc2,*jsr,*jer,*jsc,*jec,*com;
float *constant;
{
  mathimagecon(*location,isr,ier,isc,iec,*loc2,jsr,jer,jsc,jec,
		  com,constant);
}
 
ccmathwithimag(location,isr,ier,isc,iec,loc2,jsr,jer,jsc,jec,com)
int *location,*isr,*ier,*isc,*iec,*loc2,*jsr,*jer,*jsc,*jec,*com;
{
  mathwithimag(*location,isr,ier,isc,iec,*loc2,jsr,jer,jsc,jec,com);
}
 
ccmathwithcon(location,isr,ier,isc,iec,com,constant)
int *location,*isr,*ier,*isc,*iec,*com;
float *constant;
{
  mathwithcon(*location,isr,ier,isc,iec,com,constant);
}
 
#ifdef  __OLD_STUFF
ccdiskio(loc,isr,ier,isc,iec,ints,headnew,bzero,bscale)
int *loc,*isr,*ier,*isc,*iec,*ints,*headnew,*bzero,*bscale;
{
  diskio(*loc,isr,ier,isc,iec,ints,headnew,bzero,bscale);
}
#endif  /* OLD_STUFF */
 
ccrdtape(nunit,imtape,zero,loc,nr,nc,headnew,xerr)
int *nunit,*imtape,*zero,*loc,*nr,*nc,*headnew,*xerr;
{
  rdtape(nunit,imtape,zero,*loc,nr,nc,headnew,xerr);
}
 
ccdaophot1(loc,max,msg,locim,isr,ier,isc,iec,locb,isrb,ierb,iscb,iecb)
int *loc,*max,*msg,*locim,*isr,*ier,*isc,*iec,*locb,*isrb,*ierb,*iscb,*iecb;
{
  daophot1(*loc,max,msg,locim,isr,ier,isc,iec,locb,isrb,ierb,iscb,iecb);
}
 
/* These guys have the location referring to a character string, so   */
/* we have to change them to VMS descriptors                          */
 
ccinhead(inchar,loc,card)
int *inchar,*loc,*card;
{
  int inhead();
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  *card = inhead(inchar,&s);
}
 
ccchead(inchar,loc,card)
int *inchar,*loc;
char *card;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  chead(inchar,&s,card);
}
 
ccfhead(inchar,loc,card)
int *inchar,*loc;
double *card;
{
  double fhead();
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  *card = fhead(inchar,&s);
}
 
ccunfit(inchar,loc)
int *inchar, *loc;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  unfit(inchar,&s);
}
 
 
ccheader(inchar,loc,index)
int *inchar,*loc,*index;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  header(inchar,&s,index);
}
 
ccprfits(loc,inchar,full)
int *loc,*inchar,*full;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  prfits(&s,inchar,full);
}
 
ccgethjd(loc)
int *loc;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  gethjd(&s);
}
 
ccsetpoly(loc,a,b,c,d)
int *loc,*a,*b,*c,*d;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  setpoly(&s,a,b,c,d);
}
 
ccesetpoly(loc,a,b,c,d,e)
int *loc,*a,*b,*c,*d,*e;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  esetpoly(&s,a,b,c,d,e);
}
 
ccegetpoly(loc,a,b,c,d)
int *loc,*a,*b,*c,*d;
{
  struct dsc$descriptor_s s;
 
  s.dsc$w_length = *(loc+1);
  s.dsc$b_dtype = DSC$K_DTYPE_T;
  s.dsc$b_class = DSC$K_CLASS_S;
  s.dsc$a_pointer = *loc;
  egetpoly(&s,a,b,c,d);
}
 
#endif  /* VMS */



