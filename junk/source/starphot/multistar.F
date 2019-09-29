#include "Vista.h"

#define __MAXFRM 30
#define __MAXCOL 5
#define __MAXFIT 150

#undef __HAVEBYTE
#undef __SHARP
#define __SMOOTHSIZE 30
#define __MAXWAVE 9
#undef PSFMODE10


      SUBROUTINE MULTISTAR

C  MAXSTR = maximum number of input stars (total)
C  MAXFRM = maximum number of frames to fit simultaneously
C  MAXCOL = maximum number of total colors to fit
C  MITFIT = maximum number of stars which can be fit simultaneously
C  MAXSPAR = maximum number of parameters per star
C  MAXSKY = maximum number of sky parameters
      PARAMETER (MAXSTR=__MAXSTR, MAXFRM=__MAXFRM, MAXCOL= __MAXCOL)
      PARAMETER (MAXFIT=__MAXFIT, MAXSPAR=MAXCOL+2, MAXSKY=2)
C  Maxmimum number of parameters: star params, sky params, + frame shifts
C    MAXFIT*MAXSPAR:  star params
C    2*MAXFRM  : dx,dy(frame)
C    (1-10)*MAXFRM    : sky (optional)
      PARAMETER (MAXUNK=MAXFIT*MAXSPAR+(MAXSKY+2)*MAXFRM)

#ifdef __64BITADDRESS
      INTEGER*8 RSIG,XC,YC,MAG,SKYPARAM,DXPOS,DYPOS,DXPOSORIG,DYPOSORIG
      INTEGER*8 XCCLAMP,YCCLAMP,MAGCLAMP,SKYCLAMP,DXPOSCLAMP,DYPOSCLAMP
      INTEGER*8 CHI,SUMWT,NUMER,DENOM,SHARP,RPIXSQ,SKYBAR,SKY
      INTEGER*8 CHIOLD,MAGERR,XCORIG,YCORIG,ID,NPIX
      INTEGER*8 GROUP,GROUPOLD,NOUT,NGROUP,IND2,SKIP,DONE,SKIPALL
      INTEGER*8 TEMPDATA,C,V,SUMV,X,C2,C8,INDPAR,IXPSF,IYPSF,WPSF
      INTEGER*8 ITMP, INDTMP
#else
      INTEGER RSIG,XC,YC,MAG,SKYPARAM,DXPOS,DYPOS,DXPOSORIG,DYPOSORIG
      INTEGER XCCLAMP,YCCLAMP,MAGCLAMP,SKYCLAMP,DXPOSCLAMP,DYPOSCLAMP
      INTEGER CHI,SUMWT,NUMER,DENOM,SHARP,RPIXSQ,SKYBAR,SKY
      INTEGER CHIOLD,MAGERR,XCORIG,YCORIG,ID,NPIX
      INTEGER GROUP,GROUPOLD,NOUT,NGROUP,IND2,SKIP,DONE,SKIPALL
      INTEGER TEMPDATA,C,V,SUMV,X,C2,C8,INDPAR,IXPSF,IYPSF,WPSF
      INTEGER ITMP, INDTMP
#endif

C  This front end routine simply allocates a lot of memory and passes
C    the allocated addresses to the real routine DOMULTISTAR below

C  Star parameters
      CALL CCALLOC(MAXSTR*4,RSIG)
      CALL CCALLOC(MAXSTR*4,XC)
      CALL CCALLOC(MAXSTR*4,YC)
      CALL CCALLOC(MAXSTR*MAXCOL*4,MAG)
      CALL CCALLOC(4*(MAXSKY+1)*MAXSTR*MAXFRM, SKYPARAM)
      CALL CCALLOC(4*MAXSTR*MAXFRM,DXPOS)
      CALL CCALLOC(4*MAXSTR*MAXFRM,DYPOS)
      CALL CCALLOC(4*MAXSTR*MAXFRM,DXPOSORIG)
      CALL CCALLOC(4*MAXSTR*MAXFRM,DYPOSORIG)
#ifdef __HAVEBYTE
      CALL CCALLOC(1*MAXSTR*2,XCCLAMP)
      CALL CCALLOC(1*MAXSTR*2,YCCLAMP)
      CALL CCALLOC(1*MAXSTR*MAXCOL*2,MAGCLAMP)
      CALL CCALLOC(1*MAXSTR*MAXSKY*MAXFRM*2,SKYCLAMP)
      CALL CCALLOC(1*MAXSTR*MAXFRM*2,DXPOSCLAMP)
      CALL CCALLOC(1*MAXSTR*MAXFRM*2,DYPOSCLAMP)
#else
      CALL CCALLOC(4*MAXSTR*2,XCCLAMP)
      CALL CCALLOC(4*MAXSTR*2,YCCLAMP)
      CALL CCALLOC(4*MAXSTR*MAXCOL*2,MAGCLAMP)
      CALL CCALLOC(4*MAXSTR*MAXSKY*MAXFRM*2,SKYCLAMP)
      CALL CCALLOC(4*MAXSTR*MAXFRM*2,DXPOSCLAMP)
      CALL CCALLOC(4*MAXSTR*MAXFRM*2,DYPOSCLAMP)
#endif
      CALL CCALLOC(4*MAXSTR*MAXCOL,CHI)
      CALL CCALLOC(4*MAXSTR*MAXCOL,SUMWT)
      CALL CCALLOC(4*MAXSTR*MAXCOL,NUMER)
      CALL CCALLOC(4*MAXSTR*MAXCOL,DENOM)
      CALL CCALLOC(4*MAXSTR*MAXCOL,SHARP)
      CALL CCALLOC(4*MAXSTR,RPIXSQ)
      CALL CCALLOC(4*MAXSTR,SKYBAR)
      CALL CCALLOC(4*MAXSTR*MAXCOL,SKY)
      CALL CCALLOC(4*MAXSTR*MAXCOL,CHIOLD)
      CALL CCALLOC(4*MAXSTR*MAXCOL,MAGERR)
      CALL CCALLOC(4*MAXSTR,XCORIG)
      CALL CCALLOC(4*MAXSTR,YCORIG)
      CALL CCALLOC(4*MAXSTR*MAXCOL,ID)
      CALL CCALLOC(4*MAXSTR,NPIX)
      CALL CCALLOC(4*MAXSTR,GROUP)
      CALL CCALLOC(4*MAXSTR,GROUPOLD)
      CALL CCALLOC(4*MAXSTR,NOUT)
      CALL CCALLOC(4*MAXSTR,NGROUP)
      CALL CCALLOC(4*MAXSTR,IND2)
      CALL CCALLOC(1*MAXSTR,SKIP)
      CALL CCALLOC(1*MAXSTR,DONE)
      CALL CCALLOC(1*MAXSTR,SKIPALL)
      CALL CCALLOC(4*MAXSTR*MAXFIT,TEMPDATA)

      CALL CCALLOC(4*MAXUNK*MAXUNK,C)
      CALL CCALLOC(4*MAXUNK,V)
      CALL CCALLOC(4*MAXUNK,SUMV)
      CALL CCALLOC(4*MAXUNK,X)
      CALL CCALLOC(4*MAXUNK*MAXUNK,C2)
      CALL CCALLOC(8*MAXUNK*MAXUNK,C8)
      CALL CCALLOC(4*MAXUNK,INDPAR)

      CALL CCALLOC(2*MAXSTR*MAXFRM,IXPSF)
      CALL CCALLOC(2*MAXSTR*MAXFRM,IYPSF)
      CALL CCALLOC(4*MAXSTR*MAXFRM*10,WPSF)

      CALL CCALLOC(4*MAXSTR,ITMP)
      CALL CCALLOC(4*MAXSTR*2,INDTMP)

C	print *, rsig, xc, yc, mag, skyparam, expos, dypos, dxposorig,
C     &   dyposorig, xcclamp, yccalmp, magclampl, skyclamp, dxposclamp,
C     &   cyposclamp, chi, sumwt, numer, denom, sharp, rpixsq, skybar, sky,
C     &   group, groupold, nout, ngroup, ind2, skkyp, done, skipall,
C     &   tempdata, c,v,sumv, x, c2, c8, indapr, ixpsf, iypsf, wpsf, itmp,
C     &   indtmp, maxstr, maxfrm, maxcol, maxfit, maxsky, maxunk
      CALL CCDOMULTISTAR(RSIG,XC,YC,MAG,SKYPARAM,
     &      DXPOS,DYPOS,DXPOSORIG,DYPOSORIG,
     &      XCCLAMP,YCCLAMP,MAGCLAMP,SKYCLAMP,DXPOSCLAMP,DYPOSCLAMP,
     &      CHI,SUMWT,NUMER,DENOM,SHARP,RPIXSQ,SKYBAR,SKY,
     &      CHIOLD,MAGERR,XCORIG,YCORIG,ID,NPIX,
     &      GROUP,GROUPOLD,NOUT,NGROUP,IND2,SKIP,DONE,SKIPALL,
     &      TEMPDATA,C,V,SUMV,X,C2,C8,INDPAR,IXPSF,IYPSF,WPSF,
     &      ITMP, INDTMP,
     &      MAXSTR,MAXFRM,MAXCOL,MAXFIT,MAXSKY,MAXUNK)

C  Free up the memory when we are done
      CALL CCFREE(MAXSTR*4,RSIG)
      CALL CCFREE(MAXSTR*4,XC)
      CALL CCFREE(MAXSTR*4,YC)
      CALL CCFREE(MAXSTR*MAXCOL*4,MAG)
      CALL CCFREE(4*(MAXSKY+1)*MAXSTR*MAXFRM, SKYPARAM)
      CALL CCFREE(4*MAXSTR*MAXFRM,DXPOS)
      CALL CCFREE(4*MAXSTR*MAXFRM,DYPOS)
      CALL CCFREE(4*MAXSTR*MAXFRM,DXPOSORIG)
      CALL CCFREE(4*MAXSTR*MAXFRM,DYPOSORIG)
#ifdef __HAVEBYTE
      CALL CCFREE(1*MAXSTR*2,XCCLAMP)
      CALL CCFREE(1*MAXSTR*2,YCCLAMP)
      CALL CCFREE(1*MAXSTR*MAXCOL*2,MAGCLAMP)
      CALL CCFREE(1*MAXSTR*MAXSKY*MAXFRM*2,SKYCLAMP)
      CALL CCFREE(1*MAXSTR*MAXFRM*2,DXPOSCLAMP)
      CALL CCFREE(1*MAXSTR*MAXFRM*2,DYPOSCLAMP)
#else
      CALL CCFREE(4*MAXSTR*2,XCCLAMP)
      CALL CCFREE(4*MAXSTR*2,YCCLAMP)
      CALL CCFREE(4*MAXSTR*MAXCOL*2,MAGCLAMP)
      CALL CCFREE(4*MAXSTR*MAXSKY*MAXFRM*2,SKYCLAMP)
      CALL CCFREE(4*MAXSTR*MAXFRM*2,DXPOSCLAMP)
      CALL CCFREE(4*MAXSTR*MAXFRM*2,DYPOSCLAMP)
#endif
      CALL CCFREE(4*MAXSTR*MAXCOL,CHI)
      CALL CCFREE(4*MAXSTR*MAXCOL,SUMWT)
      CALL CCFREE(4*MAXSTR*MAXCOL,NUMER)
      CALL CCFREE(4*MAXSTR*MAXCOL,DENOM)
      CALL CCFREE(4*MAXSTR*MAXCOL,SHARP)
      CALL CCFREE(4*MAXSTR,RPIXSQ)
      CALL CCFREE(4*MAXSTR,SKYBAR)
      CALL CCFREE(4*MAXSTR*MAXCOL,SKY)
      CALL CCFREE(4*MAXSTR*MAXCOL,CHIOLD)
      CALL CCFREE(4*MAXSTR*MAXCOL,MAGERR)
      CALL CCFREE(4*MAXSTR,XCORIG)
      CALL CCFREE(4*MAXSTR,YCORIG)
      CALL CCFREE(4*MAXSTR*MAXCOL,ID)
      CALL CCFREE(4*MAXSTR,NPIX)
      CALL CCFREE(4*MAXSTR,GROUP)
      CALL CCFREE(4*MAXSTR,GROUPOLD)
      CALL CCFREE(4*MAXSTR,NOUT)
      CALL CCFREE(4*MAXSTR,NGROUP)
      CALL CCFREE(4*MAXSTR,IND2)
      CALL CCFREE(1*MAXSTR,SKIP)
      CALL CCFREE(1*MAXSTR,DONE)
      CALL CCFREE(1*MAXSTR,SKIPALL)
      CALL CCFREE(4*MAXSTR*MAXFIT,TEMPDATA)

      CALL CCFREE(4*MAXUNK*MAXUNK,C)
      CALL CCFREE(4*MAXUNK,V)
      CALL CCFREE(4*MAXUNK,SUMV)
      CALL CCFREE(4*MAXUNK,X)
      CALL CCFREE(4*MAXUNK*MAXUNK,C2)
      CALL CCFREE(8*MAXUNK*MAXUNK,C8)
      CALL CCFREE(4*MAXUNK,INDPAR)

      CALL CCFREE(2*MAXSTR*MAXFRM,IXPSF)
      CALL CCFREE(2*MAXSTR*MAXFRM,IYPSF)
      CALL CCFREE(4*MAXSTR*MAXFRM,WPSF)

      CALL CCFREE(4*MAXSTR,ITMP)
      CALL CCFREE(4*MAXSTR*2,INDTMP)

      RETURN
      END

      SUBROUTINE DOMULTISTAR(RSIG,XC,YC,MAG,SKYPARAM,
     &      DXPOS,DYPOS,DXPOSORIG,DYPOSORIG,
     &      XCCLAMP,YCCLAMP,MAGCLAMP,SKYCLAMP,DXPOSCLAMP,DYPOSCLAMP,
     &      CHI,SUMWT,NUMER,DENOM,SHARP,RPIXSQ,SKYBAR,SKY,
     &      CHIOLD,MAGERR,XCORIG,YCORIG,ID,NPIX,
     &      GROUP,GROUPOLD,NOUT,NGROUP,IND2,SKIP,DONE,SKIPALL,
     &      TEMPDATA,C,V,SUMV,X,C2,C8,INDPAR,IXPSF,IYPSF,WPSF,
     &      ITMP, INDTMP,
     &      MAXSTR,MAXFRM,MAXCOL,MAXFIT,MAXSKY,MAXUNK)
C
C      SUBROUTINE  DOMULTISTAR (TEMPDATA, RADIUS, PSFRAD, WATCH)
C
C=======================================================================
C
C Photometry for many stars by simultaneous multiple PSF fits of multiple frames
C Based on original NSTAR routine in DAOPHOT, but significantly modified
C
C Jon Holtzman, last rev spring 1999
C
C=======================================================================
C
      PARAMETER(MAXFRM0 = __MAXFRM, MAXCOL0 = __MAXCOL, MAXFIT0 = __MAXFIT)

C  Variables, character
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE(MAXCOL0), SWITCH
      CHARACTER*132 TMPSTR1, TMPSTR2, FSTRCAT
      CHARACTER*132 TEMPSTRING
      CHARACTER FORMSTR*132, NAME*80, PARM*8

C  For matrix inversion:
      REAL*4 C(MAXUNK,MAXUNK), V(MAXUNK), SUMV(MAXUNK), X(MAXUNK)
      REAL*4 C2(MAXUNK,MAXUNK)
      REAL*8 C8(MAXUNK,MAXUNK)
      INTEGER INDPAR(MAXUNK)

C  For PSF
      REAL*4 GAUSS(10)
      REAL*4 TEMPDATA(1)

#ifdef PSFMODE10
      REAL*4 Z(11,0:6,MAXFRM0), SLOPE(5)
      INTEGER NWX(MAXFRM0), NWY(MAXFRM0), SMOOTHSIZE
      PARAMETER (SMOOTHSIZE = __SMOOTHSIZE, MAXWAVE=__MAXWAVE)
      REAL*4 WJ(2*SMOOTHSIZE+1,2*SMOOTHSIZE+1,MAXFRM0)
      REAL*4 WAVE(MAXWAVE), WEIGHT(MAXWAVE)
C      REAL*4 INTEN(512,512), PHASE(512,512), FFT(2,512,512)
      REAL*4 RGAUSS(-SMOOTHSIZE:SMOOTHSIZE,-SMOOTHSIZE:SMOOTHSIZE,2)
      REAL*4 GTOT(2)
#endif

C  Star parameters
      REAL*4 RSIG(MAXSTR,1)
      REAL*4 XC(MAXSTR), YC(MAXSTR), MAG(MAXSTR,MAXCOL)
C  Sky parameters
      REAL*4 SKYPARAM(0:MAXSKY,MAXSTR,MAXFRM)
C  Frame parameters
      REAL*4 DXPOS(MAXSTR,MAXFRM), DYPOS(MAXSTR,MAXFRM)
      REAL*4 DXPOSORIG(MAXSTR,MAXFRM), DYPOSORIG(MAXSTR,MAXFRM)
#ifdef __HAVEBYTE
      INTEGER*1 XCCLAMP(MAXSTR,2), YCCLAMP(MAXSTR,2), MAGCLAMP(MAXSTR,MAXCOL,2)
      INTEGER*1 SKYCLAMP(MAXSKY,MAXSTR,MAXFRM,2)
      INTEGER*1 DXPOSCLAMP(MAXSTR,MAXFRM,2), DYPOSCLAMP(MAXSTR,MAXFRM,2)
#else
      REAL*4 XCCLAMP(MAXSTR,2), YCCLAMP(MAXSTR,2), MAGCLAMP(MAXSTR,MAXCOL,2)
      REAL*4 SKYCLAMP(MAXSKY,MAXSTR,MAXFRM,2)
      REAL*4 DXPOSCLAMP(MAXSTR,MAXFRM,2), DYPOSCLAMP(MAXSTR,MAXFRM,2)
#endif

      REAL*4 CHI(MAXSTR,MAXCOL), SUMWT(MAXSTR,MAXCOL)
      REAL*4 NUMER(MAXSTR,MAXCOL),DENOM(MAXSTR,MAXCOL),SHARP(MAXSTR,MAXCOL)
      REAL*4 RPIXSQ(MAXSTR)
      REAL*4 SKYBAR(MAXSTR), SUMSKY(MAXFRM0), SKY(MAXSTR,MAXCOL)
      REAL*4 CHIOLD(MAXSTR,MAXCOL), MAGERR(MAXSTR,MAXCOL)
      REAL*4 SUMRES(MAXCOL0), GRPWT(MAXCOL0)
      REAL*4 XCORIG(MAXSTR), YCORIG(MAXSTR)
      REAL*4 XOUT(MAXFRM0), YOUT(MAXFRM0)
      INTEGER*4 ID(MAXSTR,MAXCOL), NPIX(MAXSTR)

      INTEGER*4 GETNPAR, CLOSEC, SKYTERM, ICONVERGE

      INTEGER*4 GROUP(MAXSTR), GROUPOLD(MAXSTR)
      INTEGER*4 NOUT(MAXSTR), IND(MAXFIT0), NGROUP(MAXSTR), IND2(MAXSTR)

      LOGICAL*1 SKIP(MAXSTR), DONE(MAXSTR), SKIPALL(MAXSTR)
      LOGICAL RESOLVE, OMIT, REDO, NOGROUP
      REAL*4 LOWBAD

      INTEGER NPHOT(MAXCOL0), NLOCK(MAXCOL0)
      REAL SCALEPHOT(MAXFRM0), VSCALE(MAXFRM0), XSCALE(MAXFRM0)
      REAL CSCALE(MAXFRM0,MAXFRM0)
      LOGICAL LOCKPHOT(MAXFRM0), REDOSCALE

C  Commons
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      LOGICAL NOFILES, HAVEMERGE, BAD, HEADER
      LOGICAL LOCKPOS, LOCKDPOS, WRITE, SAMEGROUP
      COMMON /DAOASK/ NOFILES
      COMMON /SIZE/ NCOL, NROW

C     Include stuff for PSFLIB
#ifdef VMS
      include 'VINCLUDE:customize.inc'
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
#else
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C  PSF library variables
      REAL ALLVAL(10),ALLDVDX(10),ALLDVDY(10)
      integer x0psf, y0psf, dxpsf, dypsf
      real psfval(0:1,0:1), psfdvdx(0:1,0:1), psfdvdy(0:1,0:1)
      real*8 fhead

      REAL PSFMAG(MAXFRM0), EXPDATA(MAXFRM0), PSFTOT(MAXFRM0)
      INTEGER PHOTDATA(MAXFRM0)
      REAL TMP(MAXFRM0), GAIN(MAXFRM0), RN(MAXFRM0)
      INTEGER*2 IXPSF(MAXSTR,MAXFRM), IYPSF(MAXSTR,MAXFRM)
      REAL WPSF(MAXSTR,MAXFRM,10)
      INTEGER NX(MAXFRM0), NY(MAXFRM0), COLORS(MAXFRM0)
      INTEGER NALL(MAXFRM0), NLIB(2,MAXFRM0), NPSFLIB(MAXFRM0), NBYTES(MAXFRM0)
      INTEGER SQNALL(MAXFRM0), SKYBUF(MAXFRM0)
      CHARACTER CARD*8, LINE3*132

      INTEGER ITMP(MAXSTR)
      REAL INDTMP(MAXSTR,2), SKYRAD(2)
      LOGICAL HAVESKYRAD, DYNAMICGROUP, HAVE3SIG, POSFIT, HAVECTE
      INTEGER GETDATE
      CHARACTER DATESTR*32
      REAL DATE(MAXFRM0)

C  Include common with new options
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)

      logical computeerrors, keycheck, lastiter, keep, wkeep, saturate
      external msolve
      external mtsolv
#ifdef CHAR_NOT_CHAR
      character char*1
#endif

C   Memory adresses for real time memory allocation
#ifdef __64BITADDRESS
      INTEGER*8 LOCDATA(MAXFRM0), LOCPSF(MAXFRM0), OFFSET, LOCSKYB(MAXFRM0)
      INTEGER*8 LOCINTEN, LOCPHASE, LOCFFT
#else
      INTEGER LOCDATA(MAXFRM0), LOCPSF(MAXFRM0), OFFSET, LOCSKYB(MAXFRM0)
      INTEGER LOCINTEN, LOCPHASE, LOCFFT
#endif
      SAVE

C  Initialization of values
      CALL GETOPT(OPT)
      RADIUS = OPT(2)
      PSFRAD = OPT(3)
      WATCH = OPT(4)
      PKRATIO = OPT(10)
      WEIGHTEXPO = OPT(11)
      NOWEIGHT = NINT(OPT(12))
      SEPRATIO = OPT(13)
      WRATIO = OPT(14)
      ISKY = NINT(OPT(16))
      IFIT = NINT(OPT(18))
      DIFFM = OPT(24)
      IMODE = NINT(OPT(26))
      CHIRATIO = OPT(27)

      NCLIP = 5
      HAVEMERGE = .FALSE.
      KEEP = .FALSE.
      WKEEP = .FALSE.
      SATURATE = .FALSE.
      DRIFT = 2048.**2
      NFRAMES = 0
      NPHOTDATA = 0
      NCOLOR = 1
      NGAIN = 0
      NRN = 0
      HEADER = .FALSE.
      LOCKDPOS = .FALSE.
      DPOSMAX = 2048.**2
      LOCKPOS = .FALSE.
      WRITE = .FALSE.
      GAIN0 = -1.
      EXP0 = 1.
      AIR0 = -1.
      FLATERR = 0.0075
      PSFERR = 0.00
      FWHM = 0.
      RESOLVE = .FALSE.
      CRIT = 0.
      MINDPOS = 2
      ERRMAX = 0.05
      CHIMAX = 5
      NOGROUP = .FALSE.
      REJECT = 20.
      FAINTEST = 1.E-5
      CRITDEL = -1
      ICONVERGE = 0
      HAVESKYRAD = .FALSE.
      HAVE3SIG = .FALSE.
      HAVECTE = .FALSE.
      NSKY = 0

C  Setup VISTA keywords
      CALL KEYINIT
      CALL KEYDEF('FI=')
      CALL KEYDEF('PS=')
      CALL KEYDEF('WA=')
      CALL KEYDEF('PK=')
      CALL KEYDEF('WE=')
      CALL KEYDEF('NO=')
      CALL KEYDEF('SE=')
      CALL KEYDEF('WR=')
      CALL KEYDEF('IS=')
      CALL KEYDEF('IF=')
      CALL KEYDEF('DI=')
      CALL KEYDEF('CH=')
      CALL KEYDEF('NCLIP=')
      CALL KEYDEF('MERGE')
      CALL KEYDEF('KEEP')
      CALL KEYDEF('WKEEP')
      CALL KEYDEF('DRIFT=')
      CALL KEYDEF('SAT')
      CALL KEYDEF('EXP=')
      CALL KEYDEF('PHOT=')
      CALL KEYDEF('EXP0=')
      CALL KEYDEF('AIR0=')
      CALL KEYDEF('COL=')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('GAIN0=')
      CALL KEYDEF('FLATERR=')
      CALL KEYDEF('PSFERR=')
      CALL KEYDEF('HEADER')
      CALL KEYDEF('LOCKDPOS')
      CALL KEYDEF('DPOSMAX=')
      CALL KEYDEF('LOCKPOS')
      CALL KEYDEF('WRITE')
      CALL KEYDEF('FWHM=')
      CALL KEYDEF('RESOLVE')
      CALL KEYDEF('CRIT=')
      CALL KEYDEF('MINDPOS=')
      CALL KEYDEF('ERRMAX=')
      CALL KEYDEF('CHIMAX=')
      CALL KEYDEF('NOGROUP')
      CALL KEYDEF('WATCH=')
      CALL KEYDEF('REJECT=')
      CALL KEYDEF('FAINT=')
      CALL KEYDEF('CRITDEL=')
      CALL KEYDEF('CONVERG=')
      CALL KEYDEF('SKY=')
      CALL KEYDEF('SKYRAD=')
      CALL KEYDEF('3SIG')
      CALL KEYDEF('CTE')

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

C  Get keyword values
      DO 6706 I=1,NCON
         IF (WORD(I)(1:3) .EQ. 'FI=') THEN
            CALL ASSIGN(WORD(I),RADIUS,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'PS=') THEN
            CALL ASSIGN(WORD(I),PSFRAD,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'WA=') THEN
            CALL ASSIGN(WORD(I),WATCH,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'PK=') THEN
            CALL ASSIGN(WORD(I),PKRATIO,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'WE=') THEN
            CALL ASSIGN(WORD(I),WEIGHTEXPO,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'NO=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            NOWEIGHT = NINT(TEMP)
         ELSE IF (WORD(I)(1:3) .EQ. 'SE=') THEN
            CALL ASSIGN(WORD(I),SEPRATIO,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'WR=') THEN
            CALL ASSIGN(WORD(I),WRATIO,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'IS=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            ISKY = NINT(TEMP)
         ELSE IF (WORD(I)(1:3) .EQ. 'IF=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            IFIT = NINT(TEMP)
         ELSE IF (WORD(I)(1:3) .EQ. 'DI=') THEN
            CALL ASSIGN(WORD(I),DIFFM,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'IM=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            IMODE = NINT(TEMP)
         ELSE IF (WORD(I)(1:3) .EQ. 'CH=') THEN
            CALL ASSIGN(WORD(I),CHIRATIO,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:6) .EQ. 'NCLIP=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            NCLIP = NINT(TEMP)
         ELSE IF (WORD(I) .EQ. 'MERGE') THEN
            HAVEMERGE = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'KEEP') THEN
	    KEEP = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'WKEEP') THEN
	    WKEEP = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'SAT') THEN
	    SATURATE = .TRUE.
         ELSE IF (WORD(I)(1:6) .EQ. 'DRIFT=') THEN
            CALL ASSIGN(WORD(I),DRIFT,PARM)
            IF (XERR) RETURN
	    DRIFT = DRIFT**2
         ELSE IF (WORD(I)(1:5) .EQ. 'EXP0=') THEN
            CALL ASSIGN(WORD(I),EXP0,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:5) .EQ. 'AIR0=') THEN
            CALL ASSIGN(WORD(I),AIR0,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:4) .EQ. 'EXP=') THEN
            CALL ASSIGNV(WORD(I),MAXFRM,EXPDATA,NFRAMES,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:5) .EQ. 'PHOT=') THEN
            CALL ASSIGNV(WORD(I),MAXFRM,TMP,NPHOTDATA,PARM)
            IF (XERR) RETURN
            DO IFRM = 1, NPHOTDATA
              PHOTDATA(IFRM) = NINT(TMP(IFRM))
            END DO
         ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
            CALL ASSIGNV(WORD(I),MAXFRM,GAIN,NGAIN,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'RN=') THEN
            CALL ASSIGNV(WORD(I),MAXFRM,RN,NRN,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:6) .EQ. 'GAIN0=') THEN
            CALL ASSIGN(WORD(I),GAIN0,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:8) .EQ. 'FLATERR=') THEN
            CALL ASSIGN(WORD(I),FLATERR,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:7) .EQ. 'PSFERR=') THEN
            CALL ASSIGN(WORD(I),PSFERR,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:5) .EQ. 'FWHM=') THEN
            CALL ASSIGN(WORD(I),FWHM,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:5) .EQ. 'CRIT=') THEN
            CALL ASSIGN(WORD(I),CRIT,PARM)
            IF (XERR) RETURN
            CRIT = CRIT**2
         ELSE IF (WORD(I)(1:4) .EQ. 'COL=') THEN
            CALL ASSIGNV(WORD(I),MAXFRM,TMP,NCOLOR,PARM)
	    DO ICOL = 1, NCOLOR
	      COLORS(ICOL) = NINT(TMP(ICOL))
	    END DO
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:8) .EQ. 'MINDPOS=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            MINDPOS = NINT(TMP(1))
         ELSE IF (WORD(I)(1:7) .EQ. 'ERRMAX=') THEN
            CALL ASSIGN(WORD(I),ERRMAX,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:7) .EQ. 'CHIMAX=') THEN
            CALL ASSIGN(WORD(I),CHIMAX,PARM)
            IF (XERR) RETURN
	 ELSE IF (WORD(I) .EQ. 'HEADER') THEN
	    HEADER = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'LOCKPOS') THEN
	    LOCKPOS = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'LOCKDPOS') THEN
	    LOCKDPOS = .TRUE.
         ELSE IF (WORD(I)(1:8) .EQ. 'DPOSMAX=') THEN
            CALL ASSIGN(WORD(I),DPOSMAX,PARM)
            IF (XERR) RETURN
	 ELSE IF (WORD(I) .EQ. 'WRITE') THEN
	    WRITE = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'RESOLVE') THEN
	    RESOLVE = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'NOGROUP') THEN
	    NOGROUP = .TRUE.
         ELSE IF (WORD(I)(1:6) .EQ. 'WATCH=') THEN
            CALL ASSIGN(WORD(I),WATCH,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:7) .EQ. 'REJECT=') THEN
            CALL ASSIGN(WORD(I),REJECT,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:6) .EQ. 'FAINT=') THEN
            CALL ASSIGN(WORD(I),FAINTEST,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:8) .EQ. 'CRITDEL=') THEN
            CALL ASSIGN(WORD(I),CRITDEL,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:8) .EQ. 'CONVERG=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            ICONVERGE = NINT(TMP(1))
            IF (XERR) RETURN
        ELSE IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
            CALL ASSIGNV(WORD(I),MAXFRM,TMP,NSKY,PARM)
	    DO ICOL = 1, NSKY
	      SKYBUF(ICOL) = NINT(TMP(ICOL))
	    END DO
            IF (XERR) RETURN
        ELSE IF (WORD(I)(1:7) .EQ. 'SKYRAD=') THEN
            HAVESKYRAD = .TRUE.
            CALL ASSIGNV(WORD(I),2,SKYRAD,NSKYRAD,PARM)
            IF (XERR) RETURN
        ELSE IF (WORD(I) .EQ. '3SIG') THEN
            HAVE3SIG = .TRUE.
        ELSE IF (WORD(I) .EQ. 'CTE') THEN
            HAVECTE = .TRUE.
        END IF
 6706 CONTINUE

C  Get number of input frames
      IF (NFRAMES .GT. 0 .AND. NINTS .NE. NFRAMES) THEN
        PRINT *, 'Wrong number of exptimes specified'
        XERR = .TRUE.
        RETURN
      ELSE IF (NFRAMES .EQ. 0) THEN
        CALL ASKDATA('Enter exposure times:',EXPDATA,NINTS)
      END IF
      NFRAMES = NINTS

C  Check correct number of colors given
      IF (NCOLOR .GT. 1 .AND. NCOLOR .NE. NFRAMES) THEN
	PRINT *, 'Wrong number of colors specified'
	XERR = .TRUE.
	RETURN
      ELSE IF (NCOLOR .EQ. 1) THEN
        DO I = 1, NFRAMES
          COLORS(I) = 1
        END DO
      END IF

C  Check that correct number of gains/readout noises given
      IF (NGAIN .GT. 0 .AND. NGAIN .NE. NFRAMES) THEN
        PRINT *, 'Wrong number of gains specified', NGAIN, NFRAMES
        XERR = .TRUE.
        RETURN
      END IF 
      IF (NRN .GT. 0 .AND. NRN .NE. NFRAMES) THEN
        PRINT *, 'Wrong number of readout noises specified', NRN, NFRAMES
        XERR = .TRUE.
        RETURN
      END IF 

C  Check that correct number of entries for PHOT=
      IF (NPHOTDATA .GT. 0) THEN
        IF (NPHOTDATA .NE. NFRAMES) THEN
          PRINT *, 'Wrong number of entries in PHOT='
          XERR = .TRUE.
          RETURN
        END IF
      ELSE
        DO I = 1, NFRAMES
          PHOTDATA(I) = 1
        END DO
      END IF

C  Get critical grouping separation if we need it
      IF (IGROUP .GE. 4 .AND. CRIT .LE. 0) THEN
C        CALL ASKDATA('Enter critical separation:',CRIT,1)
        CRIT = (2.*RADIUS)**2
      END IF
 
      IF (IGROUP .EQ. 4 .OR. IGROUP .EQ. 5) THEN
        DYNAMICGROUP = .TRUE.
      ELSE
        DYNAMICGROUP = .FALSE.
      END IF


C  Get number of independent colors we have to fit for
      NN = 1
      TMP(NN) = COLORS(1)
      DO I= 1, NFRAMES
	DO J = 1, NN
	  IF (NINT(TMP(J)) .EQ. COLORS(I)) GOTO 4099
	END DO
	NN = NN + 1
	TMP(NN) = COLORS(I)
4099    CONTINUE
      END DO
      NCOLOR = NN

      IF (ISKY .GT. 1) THEN
        PRINT *, 'Code must be set with larger MAXSKY for this ISKY!'
        XERR = .TRUE.
        RETURN
      ELSE IF (ISKY .EQ. 0 .AND. .NOT. HAVESKYRAD) THEN
        PRINT *, 'You must give a sky annulus with SKYRAD= for ISKY=0!'
        XERR = .TRUE.
        RETURN
      END IF
      IF (NSKY .GT. 0 .AND. NSKY .NE. NFRAMES) THEN
        PRINT *, 
     &    'You must specify the same number of sky buffers as data frames'
        XERR = .TRUE.
        RETURN
      END IF

      IF (HAVECTE) THEN
       DO IFRAME= 1, NFRAMES
        CALL CCCHEAD('DATE-OBS',HEADBUF(1,IBUF(IFRAME)),DATESTR)
        IERR=GETDATE(DATESTR,IYY,MM,IDD,IUTH,IUTM,SS)
        DATE(IFRAME) = IYY+(MM-1)/12.+(IDD-1)/30./12.
	print *, iframe, DATESTR, DATE(IFRAME), MM, IDD, IUTH, IUTM, SS
       END DO
      END IF

C-----------------------------------------------------------------------

C SECTION 1

C Get ready, get set, . . .
      CALL TBLANK

C Read the point-spread functions into memory.

      L = INDEX(PSFFILE,'.') 
      L = L + INDEX(PSFFILE(L+1:),'.')
      L = L + INDEX(PSFFILE(L+1:),'.')
      L = MAX(L,1)

      MAXNLIB = 0

C  Separate PSF for each frame
      DO 5602 IFRAME=1, NFRAMES

C   Load image data information for each frame and make sure all
C     frames are the same size
        LOCDATA(IFRAME) = IMLOC(IBUF(IFRAME))
        IF (NSKY .EQ. 0) THEN
          LOCSKYB(IFRAME) = IMLOC(IBUF(IFRAME))
        ELSE
          LOCSKYB(IFRAME) = IMLOC(SKYBUF(IFRAME))
        END IF
        NX(IFRAME) = ICOORD(NNCOL,IBUF(IFRAME))
        NY(IFRAME) = ICOORD(NNROW,IBUF(IFRAME))
        NROW = NY(IFRAME)
        NCOL = NX(IFRAME)

        IF (NX(IFRAME) .NE. NX(1) .OR. NY(IFRAME) .NE. NY(1)) THEN
          PRINT *, 'All frames must be the same size'
          XERR = .TRUE.
          RETURN
        END IF

C   Get the PSF
        TMPSTR1 = PSFFILE
        IF (NFRAMES .GT. 1) WRITE(TMPSTR1(L:),117) IFRAME
117     FORMAT(I2.2,'.lib')

C  Types of PSFs
C    PSF libraries:
C      ipsfmode = 0:  single PSF in a library
C      ipsfmode = -1: PSF library, with basis functions and 
C         quadratic interpolation formula
C      ipsfmode = -2: PSF library grid, tabulated on rectangular grid in 
C         image array
C    Computed PSFs:
C      ipsfmode = 1: Gaussian aligned with pixels + residuals
C      ipsfmode = 2: Two gaussian aligned with pixels + residuals
C      ipsfmode = 3: 
C      ipsfmode = 4: 
C      ipsfmode = 5: 
C      ipsfmode = 10: Calculatd with full diffraction, requires input library
C                     file with fully loaded header giving all necessary
C                     parameters

        if (ipsfmode .le. 0 .or. ipsfmode .eq. 10) then
          CALL ASKFILE ('File with the PSF:', TMPSTR1)
          IF (TMPSTR1 .EQ. 'END OF FILE') RETURN
          call filedef(TMPSTR1,name,psfdir,'.lib')
C     Read the library header and decipher necessary header cards
          ifile = -1
	  call rdfitshead(temphead,name,ifile,ierr,.true.)
	  if (ierr .ne. 0) goto 9300
          npsflib(iframe) = inhead('NAXIS1',temphead) 
	  nlib(1,iframe) = inhead('NLIB',temphead)
          maxnlib = max(maxnlib,nlib(1,iframe))
	  nside = npsflib(iframe) / nlib(1,iframe)
	  psftot(iframe) = sngl(fhead('PSFTOT',temphead))
	  nall(iframe) = 1
          nsamp = 1
          if (ipsfmode .lt. 0) then
            nall(iframe) = inhead('NAXIS3',temphead) 
            x0psf = inhead('X0',temphead) 
            y0psf = inhead('Y0',temphead) 
	    if (nlib(1,iframe) .le. 0 .or. psftot(iframe) .le. 0) goto 9300
            if (ipsfmode .lt. -1) then
              if (abs(sqrt(float(nall(iframe)))-nint(sqrt(float(nall(iframe))))) 
     &            .gt. 1.e-5) then
                print *, 'ERROR: not a PSF input grid'
                xerr = .true.
                return
              end if
 	      sqnall(iframe) = nint(sqrt(float(nall(iframe))))
              x0psf = inhead('X0',temphead)
              y0psf = inhead('Y0',temphead)
              dxpsf = inhead('DX',temphead)
              dypsf = inhead('DY',temphead)
              nsamp = inhead('NAXIS4',temphead)
              if (nsamp .le. 0) nsamp = 1
              if (nsamp .gt. 1) then
                do i=1,nsamp
                  write(card,145) i    
145               format('NLIB',i1)
                  nlib(i,iframe) = inhead(card,temphead)
                end do
              end if
            end if
#ifdef PSFMODE10
          else if (ipsfmode .eq. 10) then
            nsamp = 2
            nlib(1,iframe) = 4
            nlib(2,iframe) = 1
#endif
	  end if
	  nbytes(iframe) = 4*npsflib(iframe)*npsflib(iframe)

C    Allocate memory for the PSF and read it in
	  call ccalloc(nbytes(iframe)*nall(iframe)*nsamp,locpsf(iframe))
          if (ipsfmode .le. 0) then
	    call ccrdfits(locpsf(iframe),npsflib(iframe)*nall(iframe)*nsamp,
     &                  npsflib(iframe),ifile,temphead,ierr)
	    if (ierr .ne. 0) then
              print *, iframe, locpsf(iframe), npsflib(iframe), nall(iframe),
     &             nsmap, ifile, temphead, ierr
              goto 575
            end if
#ifdef PSFMODE10
          else
C    Get all of the header parameters needed to compute PSF
            ndim = inhead('NAXIS1',temphead)
            call ccalloc(4*ndim,locinten)
            call ccalloc(4*ndim,locphase)
            call ccalloc(8*ndim,locfft)
            icam = inhead('CAMERA',temphead)
            nsamp = inhead('NSAMP',temphead)
            pupslope = sngl(fhead('PUPSLOPE',temphead))
            pupsize = sngl(fhead('PUPSIZE',temphead))
            pupxc = sngl(fhead('PUPXC',temphead))
            pupyc = sngl(fhead('PUPYC',temphead))
            smcent = sngl(fhead('SMCENT',temphead))
            smadj = sngl(fhead('SMADJ',temphead))
            nwave = inhead('NWAVE',temphead)
            do i=1,nwave
              write(card,79) i
79            format('WAVE',i2.2)
              wave(i) = sngl(fhead(card,temphead))
              write(card,80) i
80            format('WEIGHT',i2.2)
              weight(i) = sngl(fhead(card,temphead))
            end do
            w0 = wave(nwave/2+1)
            maxzer = inhead('MAXZER',temphead)
            do izer=1,maxzer
              write(card,78) izer
78            format('Z',i2.2)
              z(izer,0,iframe) = sngl(fhead(card,temphead))
            end do
            z(2,0,iframe) = 0.
            z(3,0,iframe) = 0.
            do izer=4,8
              do i=1,6
                write(card,81) izer, i
81              format('Z',i1,'_',i1)
                z(izer,i,iframe) = sngl(fhead(card,temphead))
              end do
            end do
#endif
          end if
#ifdef VMS
  	  close(ifile)
#else
	  ierr = closec(ifile)
#endif
#ifdef PSFMODE10
C   Get jitter files and load up jitter array
          if (ipsfmode .eq. 10) then
            tmpstr1 = switch(name,'.jitter')
            call readjitter(tmpstr1,icam,nsamp,
     &          wj(1,1,iframe),smoothsize,.true.,nwx(iframe),nwy(iframe))
          end if
#endif

          nrowb = nside
          ncolb = nside
          npar = 10
C  Figure out some values to use for gaussian sigmas from library stars
	    call getgauss(locpsf(iframe),radius,gauss(4),gauss(5),
     &                    npsflib(iframe),nside)
	    gauss(6) = 0.
	    call cclibget(locpsf(iframe),npsflib(iframe),nlib(1,iframe),
     &         nside/2+1,nside/2+1, nside/2.+1.,nside/2.+1.,qval,dvdx,dvdy)
	    if (watch .gt. 1.5) then
	      print *, gauss(4), gauss(5), qval
	    end if
            gauss(1) = qval
          psfmag(iframe) = -2.5*alog10(psftot(iframe)) + 25
          if (abs(psfmag(iframe)-psfmag(1)) .gt. 0.001 ) then
            print *, 'WARNING: all values of PSFTOT are NOT the same!!'
C            pause
          end if
C     Set psfmag(iframe) to be 25, because we will normalize PSFs by PSFTOT
C         below whenever getting PSF values. This way input PSFTOTs will not
C         be required to be the same
          psfmag(iframe) = 25
          npsf = 1000

        else

C  Computed PSF options
  	  PRINT *, 'This PSFMODE not yet supported in MULTISTAR'
	  XERR = .TRUE.
          RETURN

        END IF

 5602 CONTINUE

C Stars will be checked for merger if they are separated by less than
C 1 FWHM of the image core.
C     Crit. sep. = 2.355*sigma, where
C          sigma = SQRT [ (sigma(X)**2 + sigma(Y)**2)/2 ]

      if (ipsfmode .eq. 1 .or. ipsfmode .eq. 5) then
        peak = gauss(1)
        sig = gauss(4)**2 + gauss(5)**2
        prod = gauss(4)*gauss(5)
        xsharp = gauss(4)
        ysharp = gauss(5)
      else if (ipsfmode .eq. 2  .or. ipsfmode .le. 0) then
        peak = gauss(1) + gauss(6)*gauss(1)
        sig = gauss(4)**2 + gauss(5)**2
        prod = gauss(4)*gauss(5)
        xsharp = gauss(4)
        ysharp = gauss(5)
      else if (ipsfmode .le. 4) then
        sig = gauss(4)**2
        prod = sig
        peak = gauss(1)
        xsharp = gauss(4)
        ysharp = gauss(4)
      end if

C SEPMIN contains the square of the critical separation.
      IF (FWHM .GT. 0) THEN
        SEPMIN = FWHM**2
      ELSE
        SEPMIN=2.773*SIG
      END IF
      if (watch .gt. 1.5) print *, 'PSF sig, sepmin: ',sig, sepmin

      PKERR=0.027*PKRATIO/PROD**2
      IF (PSFERR .GT. 0) PKERR = PSFERR
      if (watch .gt. 1.5) print *, 'PKERR : ', pkerr

      NBOX=MIN(2*NINT(PSFRAD)+1, (NPSF-7)/2)
      PSFRSQ=(0.5*(NBOX-1))**2

      LX=1
      LY=1

C Ascertain the names of the files with the stellar groups, and open them.
C At this point, also open the global output files, one per color, for
C   the final mags with exposure time EXP0.

      DO 5598 ICOL = 1, NCOLOR

        CALL ASKFILE ('File with stellar groups:', GRPFILE(ICOL))
        IF (GRPFILE(ICOL) .EQ. 'END OF FILE') goto 9999
        CALL FILEDEF(GRPFILE(ICOL),NAME,DAODIR,'.grp')
        CALL INFILE (50+ICOL, GRPFILE(ICOL), IFLAG)
C      Error opening file?
        IF (IFLAG .LT. 0) GO TO 9300

        CALL RDHEAD (50+ICOL, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     &     EXPOSE, HJD, AP1, PHPADU, RONOIS, DUM, LINE3)
C      Not a group file?
        IF (NL .NE. 3) GO TO 9200

C  Put proper airmass in output file if specified
	IF (AIR0 .GE. 0) AIR=AIR0

C  Master output file(s)
        TMPSTR1 = SWITCH(GRPFILE(ICOL),'.nst')
	CALL OUTFILE(10+ICOL, TMPSTR1)
        CALL WRHEAD (10+ICOL, 1, NCOL, NROW, 10, LOWBAD, HIGHBAD, 
     &       THRESH, AIR, EXP0, HJD, AP1, GAIN0, RONOIS, RADIUS, LINE3)

C    OK, here is the GROUPMERGE option file handling. This just looks at
C       an old GROUP and NSTAR file if you''ve done a previous pass. It
C       compares the old GROUP file with the new one: if there are groups
C       which have not changed, then just extract the old NSTAR results
C       rather than recomputing that group.
	IF (HAVEMERGE) THEN
C        Open the old group file
          L = NUMCHAR(GRPFILE(ICOL))
          TMPSTR1 = FSTRCAT(GRPFILE(ICOL)(1:L),'.old')
          CALL INFILE (10+MAXCOL+MAXFRM+ICOL, TMPSTR1, IFLAG)
          IF (IFLAG .LT. 0) GO TO 9300

C        Open the old profile file
          TEMPSTRING = SWITCH(GRPFILE(ICOL),'.nst')
          L = NUMCHAR(TEMPSTRING)
          TMPSTR1 = FSTRCAT(TEMPSTRING(1:L),'.old')
          CALL INFILE (10+2*MAXCOL+MAXFRM+ICOL, TMPSTR1, IFLAG)
          IF (IFLAG .LT. 0) GO TO 9300

C        Open the new temporary group file for writing
          L = NUMCHAR(GRPFILE(ICOL))
          TMPSTR1 = FSTRCAT(GRPFILE(ICOL)(1:L),'.new')
          CALL OUTFILE (10+3*MAXCOL+MAXFRM+ICOL, TMPSTR1)
          IF (IFLAG .LT. 0) GO TO 9300
          CALL WRHEAD (10+3*MAXCOL+MAXFRM+ICOL, 3, IDUM, IDUM, 9, LOWBAD, 
     &     HIGHBAD, THRESH, AIR, EXPOSE, HJD, AP1, PHPADU, RONOIS, DUM, LINE3)

C        Do the work of extracting the old relevant results        
          CALL GROUPMER(TEMPDATA,ICOL,10+ICOL,
     &         10+MAXCOL+MAXFRM+ICOL,10+2*MAXCOL+MAXFRM+ICOL,
     &         10+3*MAXCOL+MAXFRM+ICOL)

C        Open the new temporary group file for reading
          CALL CLFILE(ICOL)
          TMPSTR1 = FSTRCAT(GRPFILE(ICOL)(1:L),'.new')
          CALL INFILE (50+ICOL, TMPSTR1, IFLAG)
          IF (IFLAG .LT. 0) GO TO 9300
          CALL RDHEAD (50+ICOL, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     &      EXPOSE, HJD, AP1, PHPADU, RONOIS, DUM, LINE3)
          IF (NL .NE. 3) GO TO 9200
        END IF


5598  CONTINUE

C Inquire the name of the individual frame output file(s), and open it (them).
      ICOL = 1
      IF (NOFILES) THEN
        L = INDEX(GRPFILE(ICOL),'.') 
        L = L + INDEX(GRPFILE(ICOL)(L+1:),'.')
        L = L + INDEX(GRPFILE(ICOL)(L+1:),'.')
      ELSE
        L = INDEX(PROFILE,'.') 
        L = L + INDEX(PROFILE(L+1:),'.')
        L = L + INDEX(PROFILE(L+1:),'.')
      END IF

C  Open file for delta position information (frame-to-frame pointing shifts)
      IF (NOFILES) THEN
          TMPSTR1 = GRPFILE(ICOL)
          WRITE(TMPSTR1(L:),115) 
          PROFILE=SWITCH(TMPSTR1, '.dpos')
      ELSE
          WRITE(PROFILE(L:),115) 
      END IF
115   FORMAT('.dpos')
      CALL OUTFILE (10, PROFILE)
      WRITE(10,*) NFRAMES

C  Open file for frame-to-frame scales
      IF (NOFILES) THEN
          TMPSTR1 = GRPFILE(ICOL)
          WRITE(TMPSTR1(L:),118) 
          PROFILE=SWITCH(TMPSTR1, '.frm')
      ELSE
          WRITE(PROFILE(L:),118) 
      END IF
118   FORMAT('.frm')
      CALL OUTFILE (9, PROFILE)
      WRITE(9,*) NFRAMES

C  Load up gain and readnoise for each frame.
C  Open file for each individual profile results with the WRITE option
      DO IFRAME = 1, NFRAMES

        IF (NGAIN .EQ. 0) GAIN(IFRAME) = PHPADU
        IF (NRN .EQ. 0) THEN
            RN(IFRAME) = RONOIS
        ELSE
            RN(IFRAME) = RN(IFRAME) / GAIN(IFRAME)
            RONOIS = RN(IFRAME)
        END IF
        RN(IFRAME) = MAX(RN(IFRAME),0.001)
        RN(IFRAME)=RN(IFRAME)**2

        IF (NOFILES) THEN
            TMPSTR1 = GRPFILE(ICOL)
            WRITE(TMPSTR1(L:),116) IFRAME
            PROFILE=SWITCH(TMPSTR1, '.nst')
        ELSE
            WRITE(PROFILE(L:),116) IFRAME
        END IF
116     FORMAT(I2.2,'.nst')

        IF (WRITE) THEN
          CALL ASKFILE ('File for results:', PROFILE)
          IF (PROFILE .EQ. 'END OF FILE') GO TO 9010

          CALL OUTFILE (10+MAXCOL+IFRAME, PROFILE)
          CALL WRHEAD (10+MAXCOL+IFRAME, 1, NCOL, NROW, 10, LOWBAD, HIGHBAD, 
     &       THRESH, AIR, EXP0*ABS(EXPDATA(IFRAME)), HJD, AP1, 
     &       GAIN(IFRAME), RONOIS, RADIUS, LINE3)
        END IF
      END DO

      IF (GAIN0 .LT. 0) GAIN0 = GAIN(1)

C Get ready to go.

C   Type a blank line
      CALL TBLANK
      IF (WATCH .GT. 0.5) WRITE (6,610,ERR=902)
  610 FORMAT (/' It = number of iterations for current group',
     &       /,/' n* = number of stars in current group',
     &       /,/' N* = number of stars up through current group'
     &      /,/,/'    It   n*   N*  ')
      RADSQ=RADIUS**2
      CUTOFF=0.999998*RADSQ
      NTOT=0
C-----------------------------------------------------------------------

C SECTION 2

C Loop over stellar groups. If igroup=4 or 5, read in ALL stars

C Type a blank line
 2000 IF (WATCH .GT. 0.5) CALL TBLANK
      I=0
      DO 5599 IFRM = 1, NFRAMES
        SUMSKY(IFRM)=0.
5599  CONTINUE

C Read in the next group of stars.

 2010 I=I+1

C Need multiple group files ...
      DO 5597 ICOL = NCOLOR, 1, -1
        CALL RDSTAR (50+ICOL, 3, ID(I,ICOL), 
     &     XC(I), YC(I), MAG(I,ICOL), DUM, SKY(I,ICOL)) 
        IF (ID(I,ICOL) .NE. ID(I,NCOLOR)) THEN
          PRINT *, 'Input group files dont match'
          XERR = .TRUE.
          RETURN
        END IF
5597  CONTINUE

C  End-of-file was encountered?
      IF (ID(I,1) .LT. 0) GO TO 2100
C  A blank line was encountered?
      IF (ID(I,1) .EQ. 0) THEN
        IF (IGROUP .LT. 4) THEN
          GO TO 2110
        ELSE
          I = I - 1
          GOTO 2010
        END IF
      END IF

C  Too many stars in the group?
      IF (I .GT. MAXSTR-1) GO TO 2020

C Store original position for DRIFT option
      XCORIG(I) = XC(I)
      YCORIG(I) = YC(I)

      DO 6601 ICOL = 1, NCOLOR

C A single sky brightness value, equal to the arithmetic mean of the
C skies determined for the individual stars, will be used for the
C group as a whole to start.
        SUMSKY(ICOL)=SUMSKY(ICOL)+SKY(I,ICOL)

C Convert magnitude to brightness, scaled relative to the PSF, per second
        MAG(I,ICOL)=10.**(-0.4*(MAG(I,ICOL)-PSFMAG(1)))

C If PHOTOMETRY was unable to obtain a magnitude for the star (Mag. =
C 99.999), NSTAR will give it the old university attempt anyway.
        IF (MAG(I,ICOL) .LE. 1.E-4) THEN
          MAG(I,ICOL)=0.01
          MAG(I,ICOL) = 10.**(-0.4*(20.-PSFMAG(1)))
        END IF
        MAGERR(I,ICOL)=0.0
        SHARP(I,ICOL)=0.0
6601  CONTINUE

      GO TO 2010

C The group is too large.  Type out a message, keep reading until
C a blank line is encountered, and then go back and start a completely
C new group.

 2020 WRITE (6,620,ERR=902) BELL, MAXSTR-1
  620 FORMAT (' Group with more than ', A1, I6, ' stars.')
 2030 CALL RDSTAR (52, 3, I, DUM, DUM, DUM, DUM, DUM)
      IF (I .LT. 0) GO TO 9000
      IF (I .NE. 0) GO TO 2030
      GO TO 2000

C Either a blank line or the EOF has been encountered.  If at least one
C real star has been read in since the last blank line, reduce the
C group.  If it is a blank line and no star has been read in, go back
C and read in a new group (in case in editing the file, the user has
C clumsily left several blank lines in a row).  If it is the EOF and
C no star has been read in, return.

 2100 IF (I .EQ. 1) GO TO 9000
 2110 IF (I .EQ. 1) GO TO 2000

C    Number of stars in the group
      NSTR=I-1
      NTOTSTR = NSTR
      NFIT = NSTR
C    Number of stars reduced to date
      NTOT=NTOT+NSTR
C    Mean sky value for group
      DO 6602 IFRM = 1, NFRAMES
        ICOL = COLORS(IFRM)
        SKYBAR(IFRM)=SUMSKY(ICOL)/NSTR
6602  CONTINUE

C Start reducing the group. Total number of terms to fit:
      NTERM = GETTERM(NFIT,NCOLOR,NFRAMES,ISKY,LOCKPOS,LOCKDPOS,RESOLVE,
     &         NSTARTERM,NSPAR)

C Terms for position offsets for each frames. With HEADER option,
C   read in previously determined coefficients from image header.
C   With LOCKDPOS option, lock the shifts at these values, otherwise
C   they will be allowed to float
      do iframe = 1, nframes
        if (header) then
          call getdpos(xc,yc,dxpos(1,iframe),dypos(1,iframe),nstr,
     &                 headbuf(1,ibuf(iframe)))
        else
          do i=1,nstr
            dxpos(i,iframe) = 0.
            dypos(i,iframe) = 0.
          end do
        end if
        do i=1,nstr
          dxposorig(i,iframe) = dxpos(i,iframe)
          dyposorig(i,iframe) = dypos(i,iframe)
        end do
      end do

      redoscale = .false.
      if (dynamicgroup) then
C     Initialize terms for scale offsets between frames if non-photometric
        do icol=1,ncolor
          nphot(icol) = 0
          nlock(icol) = 0
        end do
        do iframe = 1, nframes
          icol = colors(iframe)
          if (photdata(iframe) .gt. 0) nphot(icol) = nphot(icol) + 1
        end do
        nscalefit = 0
        do icol=1,ncolor
          do iframe=1,nframes
            if (icol .eq. colors(iframe)) then
              if (photdata(iframe) .gt. 0 .or. 
     &            (nphot(icol) .eq. 0 .and. nlock(icol) .eq. 0) ) then
                nlock(icol) = nlock(icol) + 1
                lockphot(iframe) = .true.
              else
                lockphot(iframe) = .false.
                nscalefit = nscalefit + 1
              end if
              scalephot(iframe) = 1.
            end if
          end do
        end do
      else
        nscalefit = 0
        do iframe = 1, nframes
          scalephot(iframe) = 1.
        end do
      end if
      if (nscalefit .gt. 0) redoscale = .true.

C Initialize sky terms. If we are using aperture sky values (isky=0), we need
C   to measure the sky on each individual FRAME; remember we have just
C   read in "average" sky values for each COLOR. If we are fitting for sky,
C   we will just use the "average" value as the starting guess. For isky=0,
C   we want the raw sky values, for other isky, we want sky values normalized
C   to the default exptime-gain
      if (isky .eq. 0) then
        do iframe = 1, nframes
          call ccaperturesky(locskyb(iframe),nx,ny,xc,yc,ntotstr,
     &           skyrad(1), skyrad(2), HAVE3sig,
     &           lowbad,highbad, skyparam(0,1,iframe),maxsky,maxstr)
        end do
        if (watch .gt. 1.5) then
	  do istr=1,ntotstr
	    print 1009, istr,xc(istr),yc(istr),
     &         (skyparam(0,istr,iframe),iframe=1,nframes)
1009	   format(i6,2f8.2,8f10.3)
          end do
        end if
      else
        jterm = skyterm(isky)
        do istr = 1, ntotstr
          do iframe = 1, nframes
	if (istr .lt. 1 .or. istr .gt. maxstr) print *, 'sky', 1, istr
	if (iframe .lt. 1 .or. iframe .gt. maxfrm) print *, 'sky', 1, iframe
            skyparam(0,istr,iframe) = skybar(iframe)
            do 6599 ii=2,skyterm(isky)
	if (istr .lt. 1 .or. istr .gt. maxstr) print *, 'sky', 1, istr
	if (iframe .lt. 1 .or. iframe .gt. maxfrm) print *, 'sky', 1, iframe
	if (ii .lt. 0 .or. ii .gt. maxsky) print *, 'sky', 1, ii
              skyparam(ii,istr,iframe) = 0.
6599        continue
          end do
        end do
      end if

C Initialize accumulators and constraints on parameter corrections.
      DO 6614 ISTR = 1, NTOTSTR
        DONE(ISTR) = .FALSE.
        NOUT(ISTR) = 0
        CALL RESETCLAMP(ISTR,XCCLAMP,YCCLAMP,MAGCLAMP,DXPOSCLAMP,DYPOSCLAMP,
     &      SKYCLAMP,MAXSTR,MAXCOL,MAXFRM,MAXSKY)
6614  CONTINUE
     
      NITER=0
      COMPUTEERRORS = .FALSE.
      LASTITER = .FALSE.

C Update information on screen.

      IF (WATCH .GT. 0.5) WRITE (6,621,ERR=902) NITER, NSTR, NTOT
      if (watch .gt. 2.5) then
        print *, 'at 1: ', colors(1),expdata(2),gain(1), gain0
        print *, ind(1), radius, ncol, nrow,xmin,ymin,xmax,ymax
      end if

  621 FORMAT (1X, 3I5, 2X)

      oldwatch = watch
C Begin to iterate solution here.

 2200 NITER=NITER+1

      if (watch .lt. 0 .and. niter .ge. abs(watch)) then
        watch = 2
      else
        watch = oldwatch
      end if

      if (watch .gt. 1.5) print *, 'top of iteration loop'

      if (ipsfmode .le. -1) then
C     Load up which interpolation values for field dependence of PSF for
C      each star. These are the same for every pixel.
        do 2322 iframe=1,nframes
         do 2321 i = 1, ntotstr
          if (ipsfmode .eq. -1) then
            xxx = xc(i) + dxpos(i,iframe)
            yyy = yc(i) + dypos(i,iframe)
            wpsf(i,iframe,1) = 1.
            wpsf(i,iframe,2) = xxx-x0psf
            wpsf(i,iframe,3) = yyy-y0psf
            wpsf(i,iframe,4) = (xxx-x0psf)**2
            wpsf(i,iframe,5) = (xxx-x0psf)*(yyy-y0psf)
            wpsf(i,iframe,6) = (yyy-y0psf)**2
            wpsf(i,iframe,7) = (xxx-x0psf)**3
            wpsf(i,iframe,8) = (xxx-x0psf)**2*(yyy-y0psf)
            wpsf(i,iframe,9) = (xxx-x0psf)*(yyy-y0psf)**2
            wpsf(i,iframe,10) = (yyy-y0psf)**3
          else
            ixpsf(i,iframe) = int((xc(i)+dxpos(i,iframe)-x0psf)/dxpsf)
            iypsf(i,iframe) = int((yc(i)+dypos(i,iframe)-y0psf)/dypsf)
            if (ixpsf(i,iframe) .lt. 0 .or. iypsf(i,iframe) .lt. 0 .or.
     &            ixpsf(i,iframe)+1 .ge. sqnall(iframe) .or. 
     &            iypsf(i,iframe)+1 .ge. sqnall(iframe)) then
                print *, 'PSF out of interpolated bound!!!'
                print *, xc(i), yc(i), dxpos(i,iframe), dypos(i,iframe)
	        print *, x0psf, y0psf, dxpsf, dypsf, ixpsf(i,iframe), iypsf(i,iframe)
	        print *, 'deleting star: ', id(i,1)
                nfit = nfit - 1
                ntot = ntot - 1
C                IF (NFIT .LE. 0) GO TO 2001
                id(i,1) = -1*abs(id(i,1))
            end if
            xxx = (xc(i)+dxpos(i,iframe) - (x0psf+ixpsf(i,iframe)*dxpsf))/dxpsf
            yyy = (yc(i)+dypos(i,iframe) - (y0psf+iypsf(i,iframe)*dypsf))/dypsf
            ix = int(xxx)
            iy = int(yyy)
            ww1 = iy+1.-yyy
            ww2 = ix+1.-xxx
            ww3 = yyy-iy
            ww4 = xxx-ix
            wpsf(i,iframe,1) = ww1 * ww2
            wpsf(i,iframe,2) = ww3 * ww2
            wpsf(i,iframe,3) = ww3 * ww4
            wpsf(i,iframe,4) = ww1 * ww4
	    if (watch .gt. 2.5) then
              print *, 'Iframe: ', iframe, colors(iframe),expdata(iframe),
     &          gain(iframe), gain0
              print *, ind(1), radius, ncol, nrow,xmin,ymin,xmax,ymax
            end if
          end if
2321     continue
2322    continue
      end if

C Set up critical error for star rejection.

      IF (NOGO) GOTO 9010
#if defined(__SUNVIEW) || defined(__X11)
      CALL LOOPDISP
#endif
      WCRIT=400.*WRATIO
      IF (NITER .GE. 4) WCRIT=1.0*WRATIO
      IF (NITER .GE. 8) WCRIT=0.4444444*WRATIO
      IF (NITER .GE. 12) WCRIT=0.25*WRATIO
      IF (WRATIO .EQ. 0.) WCRIT = 1.E38

C  Delete most uncertain star if CHI>CHICRIT (and not the brightest star)
      CHICRIT = 10.*CHIRATIO
      IF (NITER .GE. 12) CHICRIT = 5*CHIRATIO
      IF (NITER .GE. 24) CHICRIT = 3*CHIRATIO
      IF (CHIRATIO .EQ. 0.) CHICRIT = 1.E38

C  If we''re dynamically grouping, get the group ID numbers for each star by
C    doing the grouping. Ignore previously deleted stars
      if (dynamicgroup) then
	if (watch .gt. 1.5) print *, 'calling groupit ', niter, nogroup
        if (niter .eq. 1 .or. .not. nogroup)
     &    call groupit(id,xc,yc,group,ntotstr,ngrp,ngroup,done,igroup,crit,
     &                 itmp,maxstr)
	if (watch .gt. 1.5) print *, 'back groupit ', ngrp
        ntot = 0
        do igrp=1,ngrp
          ntot = ntot + ngroup(igrp)
        end do
	write(6,622,err=902) niter, ntotstr, ngrp, ntot

C  Rezero the frame scale matrices
        do i=1,nscalefit
          vscale(i) = 0.
          do j=1,nscalefit
            cscale(j,i) = 0.
          end do
        end do

      else
        ngrp = 1
        ngroup(1) = nstr
      end if
     
C  Loop over each group and advance one iteration. For igroup<4, there
C   is only one group at a time.
      do 2002 igrp = 1, ngrp
	if (niter .eq. 7 .and. igrp .eq. 501 .and. watch .gt. 0.5) watch=3

       IF (WATCH .GT. 0.5) WRITE (6,622,ERR=902) NITER, IGRP, NGRP, NGROUP(IGRP)
        if (ngroup(igrp) .gt. maxfit) then
          print *, 'too many stars in group: ', igrp, ngroup(igrp)
          goto 2002
        end if

C  With dynamic grouping, find all the stars in the current group and
C    load up their IDs into the index array.
C  nstr will be the number of stars to fit in this group
C  ntotstr is the TOTAL number of stars 
C  NOTE that neither of these numbers changes even when stars get deleted:
C    instead, deleted stars are flagged by setting the ID number negative
C  nfit gives the number of stars in this group currently being fit

        if (dynamicgroup) then
          nstr = 0
          omit = .true.
C     Get all of the stars in this group.
          do i=1,ntotstr
            if (group(i) .eq. igrp .and. id(i,1) .gt. 0) then
              nstr = nstr + 1
              ind(nstr) = i
              if (.not. done(i)) omit = .false.
  	  if (watch .gt. 1.5) print *, igrp, i, ind(nstr), xc(ind(nstr)), yc(ind(nstr))
            end if
          end do
  	  if (watch .gt. 1.5) print *, igrp, nstr, ngroup(igroup)
          nfit = nstr
          if (omit) goto 2001
          NTERM = GETTERM(NFIT,NCOLOR,NFRAMES,ISKY,LOCKPOS,LOCKDPOS,RESOLVE,
     &         NSTARTERM,NSPAR)
          IF (WATCH .GT. 1.5) print *, NFIT, ntotstr, nterm, nspar, nstarterm
	  if (watch .gt. 2.5) 
     &         print *, 'at 2: ', 1, colors(1),expdata(1),gain(1), gain0


        else
          do i=1,nstr
            ind(i) = i
            group(i) = igrp
          end do
        end if

C  Starting guess for frame offsets and sky value. Do this for first iteration
C    regardless. If we are dynamically grouping, recalculate these if the
C    group differs from what is was last iteration; if this is the case,
C    also reset clamps.
        if (niter .eq. 1 .or.
     &     (dynamicgroup .and. .not. nogroup .and. .not.
     &      samegroup(group,groupold,ntotstr,igrp,itmp,indtmp,maxstr))
     &      ) then
C         Calculate offset positions and sky values for the group
          do ifrm=1,nframes
            sumsky(ifrm) = 0.
            do j=1,nstr
              istr=ind(j)
              dxpos(istr,ifrm) = dxpos(ind(1),ifrm)
              dypos(istr,ifrm) = dypos(ind(1),ifrm)
              sumsky(ifrm) = sumsky(ifrm) + skyparam(0,istr,ifrm)
            end do
            do j=1,nstr
              istr=ind(j)
	if (istr .lt. 1 .or. istr .gt. maxstr) print *, 'sky', 2, istr
	if (ifrm .lt. 1 .or. ifrm .gt. maxfrm) print *, 'sky', 2, ifrm
              skyparam(1,istr,ifrm) = sumsky(ifrm)/nstr
            end do
          end do
C       Reset clamps (for dynamic grouping)
          if (niter .gt. 1) then
            do j=1,nstr
                istr=ind(j)
                call resetclamp(istr,xcclamp,ycclamp,magclamp,
     &             dxposclamp,dyposclamp,skyclamp,maxstr,maxcol,maxfrm,maxsky)
            end do
          end if
        end if
      
C  Remember the current groups for next iteration 
        if (dynamicgroup) then
          do i=1,ntotstr
            groupold(i) = group(i)
          end do
        end if

C If we have to make diffraction PSFs, do it here:
#ifdef PSFMODE10
        if (ipsfmode .eq. 10) then
C        Load up smoothing array
          if (resolve) then
            ngpar = 1
            do k=1,ngpar+1
              gtot(k) =0
            end do
            do j=-smoothsize,smoothsize
              do i=-smoothsize,smoothsize
                r=i**2+j**2
                rgauss(i,j,1)= exp(-r/2/rsig(ind(1),1)**2)
                rgauss(i,j,2)= exp(-r/2/(rsig(ind(1),1)+0.5)**2)
                do k=1,ngpar+1
                  gtot(k) = gtot(k) + rgauss(i,j,k)
                end do
              end do
            end do
            do j=-smoothsize,smoothsize
              do i=-smoothsize,smoothsize
                do k=1,ngpar+1
                  rgauss(i,j,k)= rgauss(i,j,k)/gtot(k)
                end do
              end do
            end do
            ngw=min(smoothsize,nint(5*(rsig(ind(1),1)+0.5)))
          end if
          do iframe = 1, nframes
            slope(1) = xc(ind(1))+dxpos(ind(1),iframe)-400
            slope(2) = yc(ind(1))+dypos(ind(1),iframe)-400
            slope(3) = (xc(ind(1))+dxpos(ind(1),iframe)-400)**2
            slope(4) = (yc(ind(1))+dypos(ind(1),iframe)-400)**2
            slope(5) = (xc(ind(1))+dxpos(ind(1),iframe)-400)*
     &                 (yc(ind(1))+dypos(ind(1),iframe)-400)
            do izer=4,8
              z(izer,0,iframe) = z(izer,1,iframe)
              do i=1,5
                z(izer,0,iframe) = z(izer,0,iframe) + 
     &            slope(i)*z(izer,i+1,iframe)
              end do
            end do

            do iwave = 1, nwave
              ww = weight(iwave) * (wave(iwave)/w0)
              call ccmakepupil(locinten,ndim,icam,xc(ind(1)),yc(ind(1)),radius,
     &           radx, rady,nsamp,pupslope,pupxc,pupyc,pupsize)
              call ccmakephase(locinten,locphase,ndim,z(1,0,iframe),
     &           radius,radx,rady,wave(iwave),icam)
C         Do two resoltutions for derivative
              offset=0
              do ires=1,2
C         Do two samplings
                do isamp=1,2
C         Do the FFT and smooth it
                  call ccdofft(locfft,locinten,locphase,ndim,resolve,
     &              rgauss(-smoothsize,-smoothsize,ires),smoothsize,0,1,ngw)
C         Integrate over pixels, include smearing, load into library!
	          call ccfilllib(locpsf(iframe)+offset,locfft,smcent,smadj,
     &              nlib(isamp,iframe),wj(1,1,iframe),nwx(iframe),nwy(iframe))
                  offset = offset + nbytes(iframe)
                end do
              end do
            end do
          end do

        end if
#endif

C We return to here to redo an iteration on any particular single group
C Update iteration display with group number
 2210   IF (WATCH .GT. 0.5) 
     &      WRITE (6,622,ERR=902) NITER, IGRP, NGRP, NGROUP(IGRP)
  622   FORMAT ('+', 4I5)

        if (watch .gt. 2.5) then
          print *, 'npar: ', nFIT, nstarterm, nframes, ncolor
	  print *, 'at 3: ', 1, colors(1),expdata(1),gain(1), gain0
          print *, ind(1), radius, ncol, nrow,xmin,ymin,xmax,ymax
        end if

C  If we're using IGROUP=2,3 and we've deleted all of the stars that
C    we were interested in, stop here and go to next group
        if (igroup .eq. 2 .or. igroup .eq. 3) then
          omit = .true.
          do 5701 i=1,nstr
            if (id(i,1) .lt. 50000 .and. id(i,1) .gt. 0) omit = .false.
 5701     continue
          if (omit) goto 2000
        end if

C If there is more than one star, check to see whether any two stars
C have merged.  Meanwhile, determine the upper and lower limits in x
C and y of a rectangular box containing the centroids of all stars, and
C initialize a couple of accumulators.

      XMIN=NCOL
      XMAX=1.
      YMIN=NROW
      YMAX=1.

      DO 2230 I=1,NSTR
        IF (ID(IND(I),1) .LE. 0) GOTO 2230
        DO 6603 ICOL = 1, NCOLOR
          CHI(IND(I),ICOL)=0.
          SUMWT(IND(I),ICOL)=0.
#ifdef __SHARP
          NUMER(IND(I),ICOL)=0.
          DENOM(IND(I),ICOL)=0.
#endif
6603    CONTINUE
        XMIN=MIN(XMIN,XC(IND(I)))
        XMAX=MAX(XMAX,XC(IND(I)))
        YMIN=MIN(YMIN,YC(IND(I)))
        YMAX=MAX(YMAX,YC(IND(I)))
	if (watch .gt. 1.5) print *, i, id(ind(i),1),xc(ind(i)),yc(ind(i)),xmin,xmax,ymin,ymax
        IF (NSTR .EQ. 1) GO TO 2230

        DO 2220 J=1,I-1
          IF (ID(IND(J),1) .LE. 0) GOTO 2220
          SEP=(XC(IND(I))-XC(IND(J)))**2+(YC(IND(I))-YC(IND(J)))**2
          K=J
          IF (MAG(IND(I),1) .LT. MAG(IND(J),1)) K=I

#ifdef NOTDEF
C If we''ve had at least 4 iterations, delete the fainter star in a pair
C    if it''s less than (DELTA MAG)*FWHM pixels away
          DO 2221 ICOL = 1, NCOLOR
	if (watch .gt. 2.5) print *, i, j, mag(ind(i),1), mag(ind(j),1)
            DIFF = -2.5*ALOG10(MAG(IND(I),1)/MAG(IND(J),1))
	    DIFF = DIFF**2
            IF (NITER .GT. 4 .AND. SEP .LT. DIFF*SEPRATIO*SEPMIN) GOTO 2240
2221      CONTINUE
#endif
	if (watch .gt. 2.5)
     &      print *, sep, sepratio, sepmin,  k, magerr(ind(k),1), wcrit
C Consider deleting stars closer than SEPRATIO*SEPMIN
          IF (SEP .GT. SEPRATIO*SEPMIN .OR. KEEP) GO TO 2220

C Two stars are overlapping (SEP .LT. 0.14*SEPRATIO*SEPMIN).  
C Identify the fainter of the two, and delete it.
          IF ((SEP .LT. 0.14*SEPRATIO*SEPMIN)) GOTO 2240

C Intermediate separation: delete if magnitude is uncertain.
          BAD = .TRUE.
          DO 6604 ICOL = 1, NCOLOR
            IF (MAGERR(IND(K),ICOL) .LT. WCRIT) 
     &          BAD = .FALSE.
6604      CONTINUE

          IF (BAD) GO TO 2240

 2220   CONTINUE

 2230 CONTINUE

C No two stars have merged.
      GO TO 2260

 2240 CONTINUE

C Now eliminate the fainter of the two, or the "fake" star for igroup=2.
      IF (MAG(IND(I),1) .LT. MAG(IND(J),1)) I=J

	if (watch .gt. 2.5) print *, i, k, ind(i), ind(k),
     &    id(ind(i),1), id(ind(k),1)

C The K-th star is now the fainter of the two, the I-th, the brighter.
      if (watch .gt. 1.5) then
        PRINT *, IND(I), IND(K), NSTR, NFIT, NTOT, 'at 1'
        PRINT *, ID(IND(I),1)
        PRINT *, ID(IND(K),1)
      end if
      NI=ALOG10(ID(IND(I),1)+0.5)+2
      NK=ALOG10(ID(IND(K),1)+0.5)+2
      FORMSTR = ' '
      WRITE(FORMSTR,623,ERR=901) NK, NI
  623 FORMAT ('(''+'', 3I5, 5X, ''Star'', I',I2.2,
     &  ','' merged with star'', I',I2.2,
     &  ','', so its been deleted.'')')
      IF (WATCH .GT. 0.5) WRITE (6,FORMSTR,ERR=902) 
     &        NITER, NSTR, NTOT, ID(IND(K),1), ID(IND(I),1)
    
C Now replace the centroid of the I-th star with the weighted mean of
C the most recent estimates of the centroids of the I-th and K-th
C stars, and the brightness of the I-th with the sum of the brightnesses
C of the I-th and K-th.
      XC(IND(I))=XC(IND(I))*MAG(IND(I),1)+XC(IND(K))*MAG(IND(K),1)
      YC(IND(I))=YC(IND(I))*MAG(IND(I),1)+YC(IND(K))*MAG(IND(K),1)
      DO 6605 ICOL = 1, NCOLOR
        MAG(IND(I),ICOL)=MAG(IND(I),ICOL)+MAG(IND(K),ICOL)
6605  CONTINUE
      XC(IND(I))=XC(IND(I))/MAG(IND(I),1)
      YC(IND(I))=YC(IND(I))/MAG(IND(I),1)

C Remove the K-th star from the group by flagging it with a negative ID
      ID(IND(K),1) = -1 * ABS(ID(IND(K),1))
      NFIT = NFIT - 1
      NTOT=NTOT-1
      IF (NFIT .LE. 0) GO TO 2001
C    Update display
      IF (WATCH .GT. 0.5)
     &     WRITE (6,622,ERR=902) NITER-1, NSTR, NTOT

C Reset the clamps for all the stars in this group and go try another iteration
      DO 2250 I = 1, NSTR
        ISTR = IND(I)
        CALL RESETCLAMP(ISTR,XCCLAMP,YCCLAMP,MAGCLAMP,
     &             DXPOSCLAMP,DYPOSCLAMP,SKYCLAMP,MAXSTR,MAXCOL,MAXFRM,MAXSKY)
2250  CONTINUE
      GO TO 2210

 2260 CONTINUE

C Now... on with the iteration.

C Zero the normal matrix and the vector of residuals.

      NTERM = GETTERM(NFIT,NCOLOR,NFRAMES,ISKY,LOCKPOS,LOCKDPOS,RESOLVE,
     &         NSTARTERM,NSPAR)
      DO 2275 J=1,NTERM
        V(J)=0.0
        X(J)=0.0
        SUMV(J) = 0.0
        DO 2270 I=J,NTERM
          C(I,J)=0.0
 2270   CONTINUE
 2275 CONTINUE

      DO 2280 I=1,NSTR
        NPIX(IND(I))=0
 2280 CONTINUE

C Now deal with the pixels one by one.

      SUMRES2=0.
      DO 6616 ICOL = 1, NCOLOR
        SUMRES(ICOL)=0.
        GRPWT(ICOL)=0.
6616  CONTINUE

C  Loop over each input frame
      ISCALEFIT = 0
      DO 9901 IFRAME = 1, NFRAMES
        if (dynamicgroup .and. .not. lockphot(iframe)) iscalefit=iscalefit+1

	if (watch .gt. 1.5) then
          print *, 'Iframe: ', iframe, colors(iframe),expdata(iframe),
     &             gain(iframe), gain0
          print *, ind(1), radius, ncol, nrow,xmin,ymin,xmax,ymax
        end if

      ICOL = COLORS(IFRAME)
      SCALE = 1./ABS(EXPDATA(IFRAME))*GAIN(IFRAME)/GAIN0

      iskystart = nstarterm + (iframe-1)*jterm
      iframestart = nstarterm + nframes*skyterm(isky) + (iframe-2)*2

      IXMIN=MAX(1, INT(XMIN+DXPOS(IND(1),IFRAME)-RADIUS)+1)
      IXMAX=MIN(NCOL, INT(XMAX+DXPOS(IND(1),IFRAME)+RADIUS))
      IYMIN=MAX(1, INT(YMIN+DYPOS(IND(1),IFRAME)-RADIUS)+1)
      IYMAX=MIN(NROW, INT(YMAX+DYPOS(IND(1),IFRAME)+RADIUS))

C IXMIN, IXMAX, IYMIN, and IYMAX are now the limits of a rectangular
C containing all pixels within one fitting radius of any star in the
C group. Loop over each pixel.

C Get first cut at which stars to skip: skip if they''re more than a PSFRAD
C   from corner of the pixel subsection we''ll loop over
      IF (IFRAME .EQ. 1) THEN
        NLOOP = 0
        IXMIN = IXMIN - PSFRAD
        IXMAX = IXMAX + PSFRAD
        IYMIN = IYMIN - PSFRAD
        IYMAX = IYMAX + PSFRAD
        DO 2391 I=1,NTOTSTR
          XX = XC(I) + DXPOS(I,IFRAME)
          IF (XX .LT. IXMIN) THEN
            SKIPALL(I) = .TRUE.
          ELSE IF (XX .GT. IXMAX) THEN
            SKIPALL(I) = .TRUE.
          ELSE
            YY = YC(I) + DYPOS(I,IFRAME)
            IF (YY .LT. IYMIN) THEN
              SKIPALL(I) = .TRUE.
            ELSE IF (YY .GT. IYMAX) THEN
              SKIPALL(I) = .TRUE.
            ELSE IF (ID(I,1) .LT. 0) THEN
              SKIPALL(I) = .TRUE.
            ELSE
              SKIPALL(I) = .FALSE.
              NLOOP = NLOOP + 1
              IND2(NLOOP) = I
C	if (watch .gt. 2.5) print 7000, nloop, id(i,1), mag(i,1), 
C     &           xc(i), yc(i), igrp, group(i)
7000	format(2i6,3f12.3,2i6)
            END IF
          END IF
2391    CONTINUE
        IXMIN = IXMIN + PSFRAD
        IXMAX = IXMAX - PSFRAD
        IYMIN = IYMIN + PSFRAD
        IYMAX = IYMAX - PSFRAD
      END IF

C Adjust the positions for this frame for the relevant stars
      DO 2392 J=1,NLOOP
        I = IND2(J)
C	if (xc(i) .lt. 0 .or. xc(i) .gt. 1000) then
C	  print *, niter, igrp, iframe
C          print *,  j, i, xc(i), yc(i), dxpos(i,iframe), dypos(i,iframe)
C	  pause
C        end if
        if (watch .gt. 2.5) print *, i, iframe, xc(i), yc(i), dxpos(i,iframe), dypos(i,iframe)
        XC(I) = XC(I) + DXPOS(I,IFRAME)
        YC(I) = YC(I) + DYPOS(I,IFRAME)
 2392 CONTINUE

      DO 2390 IY=IYMIN,IYMAX
        DO 2380 IX=IXMIN,IXMAX

C If this pixel is within one fitting radius of at least one star,
C include it in the solution.  Otherwise, skip it.  While figuring
C this out, compute the squared distance of this pixel from the
C centroid of each star in the group; make sure that every star
C has at least four valid pixels within one fitting radius.

	if (watch .gt. 2.5) print *, iframe, ix, iy

        OMIT=.TRUE.
        DO 2310 ILOOP=1,NLOOP
          I = IND2(ILOOP)
          XSQ = FLOAT(IX)-XC(I)
          IF (XSQ .GT. PSFRAD) THEN
            RPIXSQ(I) = PSFRSQ +1
          ELSE
            YSQ = FLOAT(IY)-YC(I)
            IF (YSQ .GT. PSFRAD) THEN
              RPIXSQ(I) = PSFRSQ +1
            ELSE
              RPIXSQ(I)= XSQ**2 + YSQ**2
            END IF
          END IF
C          XSQ = (FLOAT(IX)-XC(I))**2
C          RPIXSQ(I)= XSQ + (FLOAT(IY)-YC(I))**2
          SKIP(I)=.TRUE.
 2310   CONTINUE
        DO 2311 J=1,NSTR
          I = IND(J)
          IF (ID(I,1) .LE. 0) GOTO 2311
          IF (RPIXSQ(I) .GT. CUTOFF) GO TO 2311
          SKIP(I)=.FALSE.
          NPIX(I)=NPIX(I)+1
          OMIT=.FALSE.
 2311   CONTINUE
        IF (OMIT) GO TO 2380

C  Get the observed data point for this pixel in this frame
        CALL CCGETDATA(LOCDATA(IFRAME),IX,IY,NX,NY,DATA)

C  If we have the SATURATE option and a bad pixel, goto next group
        IF ((DATA .GT. HIGHBAD) .AND. SATURATE) GOTO 2002

C  Without the saturate option, continue, but skip this pixel
        IF ((DATA .LT. LOWBAD) .OR. (DATA .GT. HIGHBAD)) GOTO 2380

C  Normalize the data to unit exposure time, gain0
	DATA = DATA*SCALE

C  Get the current sky value for this pixel
        SKYBAR(ICOL) = 
     &     GETSKY(NCOL/2-IX,NROW/2-IY,SKYPARAM(1,IND(1),IFRAME),SKYTERM(ISKY))
        IF (WATCH .GT. 2.5) THEN
           IF (ISKY .GT. 1 .OR. (IX .EQ. IXMIN .AND. IY .EQ. IYMIN))
     &       PRINT *, 'SKYBAR: ', SKYBAR(ICOL)
        END IF

C  If we are using aperture sky values, subtract them _before_ scaling
        IF (ISKY .EQ. 0) THEN
          D=DATA-SKYBAR(ICOL)*SCALE
        ELSE
          D=DATA-SKYBAR(ICOL)
        END IF
        WT=0.

C  Do the sky values that are dependent on pixel location if requested
        IF (ISKY .GE. 1) X(ISKYSTART+1) = -1.0
        if (isky .ge. 2) then
          x(iskystart+2) = -1.*(ncol/2-ix)
          x(iskystart+3) = -1.*(nrow/2-iy)
        end if
        if (isky .ge. 3) then
          x(iskystart+4) = -1.*(ncol/2-ix)**2
          x(iskystart+5) = -1.*(ncol/2-ix)*(nrow/2-iy)
          x(iskystart+6) = -1.*(nrow/2-iy)**2
        end if
        if (isky .ge. 4) then
          x(iskystart+7) = -1.*(ncol/2-ix)**3
          x(iskystart+8) = -1.*(ncol/2-ix)**2*(nrow/2-iy)
          x(iskystart+9) = -1.*(ncol/2-ix)*(nrow/2-iy)**2
          x(iskystart+10) = -1.*(nrow/2-iy)**3
        end if
        if (iframe .gt. 1) then
	  X(IFRAMESTART+1) = 0.
	  X(IFRAMESTART+2) = 0.
        end if

C Now loop over the stars, one by one.

        ISTAR = 0
        DO 2320 ILOOP=1,NLOOP
          I = IND2(ILOOP)
C Keep track of the number of stars that we''re actually fitting
          IF (GROUP(I) .EQ. IGRP) THEN
            ISTAR = ISTAR + 1
C Initialize derivatives for this star
            K = (ISTAR-1)*NSPAR
            DO III=1,NCOLOR
              X(K+III) = 0
            END DO
            IF (LOCKPOS) THEN
              IDERIV = 0
            ELSE
              IDERIV = 1
            END IF
          ELSE
            IDERIV = 0
          END IF

	if (watch .gt. 2.5) print *, iloop,i,rpixsq(i),psfrsq

C If this pixel is within one PSF radius of this star''s center, compute
C the scaled value of the PSF at this point and subtract it.
          IF (RPIXSQ(I) .GT. PSFRSQ) GO TO 2320

C Get the integrated value of the PSF at this pixel using the appropriate
C   PSF method
          IF (IPSFMODE .LE. 0 .or. ipsfmode .eq. 10) THEN
	    if (ipsfmode .eq. 0) then
	      call cclibget(locpsf(iframe),npsflib(iframe),nlib(1,iframe),
     &          ix,iy, xc(i),yc(i),qval, dvdx, dvdy)
              qval=qval/psftot(iframe)
              if (ideriv .eq. 1) then
                dvdx=dvdx/psftot(iframe)
                dvdy=dvdy/psftot(iframe)
              end if
	    else if (ipsfmode .eq. -1) then
	      call ccliball(locpsf(iframe),npsflib(iframe),nlib(1,iframe),
     &          ix,iy,xc(i),yc(i),
     &          allval,alldvdx,alldvdy,nall(iframe),nbytes(iframe))
              qval = allval(1)*wpsf(i,iframe,1)
              do ii=2,nall(iframe)
                qval = qval + allval(ii)*wpsf(i,iframe,ii)
              end do
              qval=qval/psftot(iframe)
              if (ideriv .eq. 1) then
                dvdx = alldvdx(1)*wpsf(i,iframe,1)
                dvdy = alldvdy(1)*wpsf(i,iframe,1)
                do ii=2,nall(iframe)
                  dvdx = dvdx + alldvdx(ii)*wpsf(i,iframe,ii)
                  dvdy = dvdy + alldvdy(ii)*wpsf(i,iframe,ii)
                end do
                dvdx=dvdx/psftot(iframe)
                dvdy=dvdy/psftot(iframe)
              end if
#ifdef NOTDEF
              call gpcoords(yc(i),xc(i),xxx,yyy)
              call gpsfval(xxx,yyy,allval,qval)
              if (group(i) .eq. igrp) then
                call gpsfval(xxx,yyy,alldvdx,dvdx)
                call gpsfval(xxx,yyy,alldvdy,dvdy)
              end if
#endif
	    else 
C            Get the four PSF values for this pixel and interpolate for
C             proper field location
              if (ideriv .eq. 1 .and. resolve .and. ipsfmode .eq. 10) then
                offset = 2*nbytes(iframe)
                call cclibnew(locpsf(iframe)+offset,
     &             npsflib(iframe),nlib(1,iframe),
     &             ix,iy,xc(i),yc(i),
     &             psfval,psfdvdx,psfdvdy,
     &             nall(iframe),nbytes(iframe),nsamp,
     &             ixpsf(i,iframe),iypsf(i,iframe),nskip,ideriv,ierr)
                qval2 = psfval(0,0)*wpsf(i,iframe,1) + psfval(0,1)*wpsf(i,iframe,2) +
     &               psfval(1,1)*wpsf(i,iframe,3) + psfval(1,0)*wpsf(i,iframe,4) 
              end if
              call cclibnew(locpsf(iframe),npsflib(iframe),nlib(1,iframe),
     &             ix,iy,xc(i),yc(i),
     &             psfval,psfdvdx,psfdvdy,
     &             nall(iframe),nbytes(iframe),nsamp,
     &             ixpsf(i,iframe),iypsf(i,iframe),nskip,ideriv,ierr)
	if (ierr .ne. 0) then
	  print *, niter, igrp, iframe, ix, iy, iloop
          print *, locpsf(iframe), npsflib(iframe), nlib(1,iframe),ix,iy
          print *,  j, i, xc(i), yc(i), dxpos(i,iframe), dypos(i,iframe)
	  pause
          watch=3
        end if
              qval=psfval(0,0)*wpsf(i,iframe,1) + psfval(0,1)*wpsf(i,iframe,2) +
     &             psfval(1,1)*wpsf(i,iframe,3) + psfval(1,0)*wpsf(i,iframe,4) 
              qval = qval / psftot(iframe)
              if (ideriv .eq. 1) then
                dvdx = psfdvdx(0,0)*wpsf(i,iframe,1) + psfdvdx(0,1)*wpsf(i,iframe,2) +
     &               psfdvdx(1,1)*wpsf(i,iframe,3) + psfdvdx(1,0)*wpsf(i,iframe,4) 
                dvdy = psfdvdy(0,0)*wpsf(i,iframe,1) + psfdvdy(0,1)*wpsf(i,iframe,2) +
     &               psfdvdy(1,1)*wpsf(i,iframe,3) + psfdvdy(1,0)*wpsf(i,iframe,4) 
	        dvdx = dvdx / psftot(iframe)
	        dvdy = dvdy / psftot(iframe)
                if (resolve .and. ipsfmode .eq. 10) dvdsig = (qval2-qval)/0.5
              end if
	    end if
            if (ideriv .eq. 1) then
	      dvdx = -1. * float(nlib(nskip+1,iframe)) * dvdx
	      dvdy = -1. * float(nlib(nskip+1,iframe)) * dvdy
            end if
          END IF
          if (watch .gt. 2.5 .and. ix .eq. nint(xc(i)) .and. iy .eq.
     &       nint(yc(i))) then
	    print *, ix,iy, xc(i), yc(i), qval, dvdx,dvdy,mag(i,icol)*qval,d,
     &         data, scale
C            pause
          end if
          IF (HAVECTE) THEN
            CTECORR=CTE(XC(I),YC(I),MAG(I,ICOL)*GAIN(IFRAME)/SCALE,
     &                  SKYBAR(ICOL)*GAIN(IFRAME),DATE(IFRAME))
C	write(44,*) xc(i), yc(i), mag(i,icol), scale, skybar(icol), date, ctecorr
            D=D-MAG(I,ICOL)*QVAL*SCALEPHOT(IFRAME)*CTECORR
          ELSE
            D=D-MAG(I,ICOL)*QVAL*SCALEPHOT(IFRAME)
          END IF
          if (ideriv .eq. 1) then
            DVDX = DVDX*MAG(I,ICOL)*SCALEPHOT(IFRAME)
            DVDY = DVDY*MAG(I,ICOL)*SCALEPHOT(IFRAME)
            IF (HAVECTE) THEN
              DVDX = DVDX*CTECORR
              DVDY = DVDY*CTECORR
            END IF
          end if

C The condition equation for pixel (IX,IY) is of the form

C data(IX,IY)-sky-summation{scale*psf(IX-Xcenter,IY-Ycenter)}=residual

C Then we will jigger the scale''s, Xcenter''s, and Ycenter''s such that
C
C                Summation{weight * residual**2}
C
C is minimized.  'weight' will be a function (1) of the distance of this
C pixel from the center of the nearest star, (2) of the model-predicted
C brightness of the pixel (taking into consideration the readout noise,
C the photons/ADU, and the interpolation error of the PSF), and (3) of
C the size of the residual itself.  (1) is necessary to prevent the
C non-linear least-squares solution from oscillating:  oft-times it will
C come to pass that if you include a pixel in the solution, then the
C predicted shift of the centroid will cause that pixel to be excluded
C in the next iteration, and the new predicted shift of the centroid
C will cause that pixel to be included again.  This could go on ad
C infinitum.  The cure is to have the weight of a pixel go
C asymptotically to zero as its distance from the stellar centroid
C approaches the fitting radius.  In a case like that just described,
C the solution can then find a real minimum of the sum of the
C weighted squared residuals with that pixel at some low-weight position
C just inside the fitting radius.  (2) is just sensible weighting.
C (3) is just a crude attempt at making the solution more robust against
C bad pixels.

C If we''re not fitting for this star, move on
          IF (GROUP(I) .NE. IGRP) THEN
C	    WRITE(41,41) NITER, IFRAME, ICOL, ID(I,1),sqrt(RPIXSQ(I)),
C     &       mag(i,icol),MAG(I,ICOL)*QVAL,DATA,SKYBAR(ICOL),DATA-SKYBAR(ICOL)
41	    FORMAT(4I5,6F10.3)
            GOTO 2320
          END IF
            
C  Load up the derivative for this star

          RSQ=RPIXSQ(I)/RADSQ
          IF (SKIP(I)) GO TO 2320
          WT=MAX(WT, 5./(5.+RSQ/(1.-RSQ)))
          SKIP(I)=.FALSE.

C    magnitude:
          x(k+icol) = -qval
C    position
          iextra = 0
          if (.not. lockpos) then
C            if (magerr(i,icol) .lt. wcrit) then   
C    note multiplication of dvdx by mag(i) is done above to save repeating it
              x(k+ncolor+1) = dvdx
              x(k+ncolor+2) = dvdy
C            end if
            iextra = iextra + 2
          end if
C    resolution
          if (resolve) then
            x(k+ncolor+iextra+1) = mag(i,icol)*dvdsig*scalephot(iframe)
            iextra = iextra + 1
          end if
C    frame shifts
          IF (IFRAME .GT. 1 .AND. .NOT. LOCKDPOS) THEN
C            if (magerr(i,icol) .lt. wcrit) then   
C    note multiplication of dvdx by mag(i) is done above to save repeating it
              X(IFRAMESTART+1) = X(IFRAMESTART+1) + DVDX
              X(IFRAMESTART+2) = X(IFRAMESTART+2) + DVDY
C            end if
          END IF
 2320   CONTINUE

C At this point, the vector X contains the first derivative of
C the condition equation for pixel (IX,IY) with respect to each of
C the fitting parameters for all of the stars.
C
C Now these derivatives will be added into the normal matrix and the
C vector of residuals.
C
C The expected random error in the pixel is the quadratic sum of
C the Poisson statistics, plus the readout noise, plus an estimated
C error of 0.75% of the total brightness for the difficulty of flat-
C fielding and bias-correcting the chip, plus an estimated error of
C some fraction of the fourth derivative at the peak of the profile,
C to account for the difficulty of accurately interpolating within the
C point-spread function.  The fourth derivative of the PSF is
C proportional to H/sigma**4 (sigma is the Gaussian width parameter for
C the stellar core); using the geometric mean of sigma(x) and sigma(y),
C this becomes H/[sigma(x)*sigma(y)]**2.  The ratio of the fitting
C error to this quantity is estimated from a good-seeing CTIO frame to
C be approximately 0.027 (see definition of PKERR above.)

          DPOS=MAX(0., DATA-D)/SCALE

C DPOS = raw data minus residual = model-predicted value of the
C intensity at this point (which presumably is non-negative).
          SIGSQ=DPOS/GAIN(IFRAME)+RN(IFRAME)+(FLATERR*DPOS)**2+
     &          (PKERR*(DPOS-SKYBAR(ICOL)))**2
          SIGSQ=SIGSQ*SCALE**2

	if (watch .gt. 2.5) print *, 'at sigsq', dpos, gain(iframe), rn(iframe), flaterr, pkerr, scale
	if (watch .gt. 2.5) print *, niter, d, dpos, wt, sigsq, sumres2
	if (watch .gt. 2.5) print *, noweight, niter, nclip, relerr,
     &     chiold(i,icol)

C   Absolute relative error
          RELERR=ABS(D)/SQRT(SIGSQ)

C If this pixel has a twenty-sigma error, reject it out of hand (after
C the first iteration).
          IF (NITER .GT. NCLIP .AND. 
     &          (RELERR .GT. REJECT*CHIOLD(IND(1),ICOL))) GO TO 2380

C Add this residual into the weighted sum of the absolute relative residuals.
          SUMRES(ICOL)=SUMRES(ICOL)+RELERR*WT
          GRPWT(ICOL)=GRPWT(ICOL)+WT

C Add into the accumulating sums of the weighted absolute relative
C residuals and of the image sharpness parameter for each of the stars.
          DO 2330 J=1,NSTR
            I = IND(J)
            IF (ID(I,1) .LE. 0) GOTO 2330
            IF (SKIP(I)) GO TO 2330
            CHI(I,ICOL)=CHI(I,ICOL)+RELERR*WT
            SUMWT(I,ICOL)=SUMWT(I,ICOL)+WT
#ifdef __SHARP
            RHOSQ=((XC(I)-FLOAT(IX))/XSHARP)**2
            IF (RHOSQ .GT. 36.) GO TO 2330
            RHOSQ=RHOSQ+
     &            ((YC(I)-FLOAT(IY))/YSHARP)**2

C Include in the sharpness index only those pixels within six
C sigma of the centroid of the object.  (This saves time and
C floating underflows by excluding pixels which contribute less than
C about one part in a million to the index.)
            IF (RHOSQ .GT. 36.) GO TO 2330
            RHOSQ=0.5*RHOSQ
            DFDSIG=EXP(-RHOSQ)*(RHOSQ-1.)
            DPOS=(MAX(0., DATA-SKYBAR(ICOL))+SKYBAR(ICOL))/SCALE

C DPOS-SKYBAR = raw data minus sky = model-predicted value of the
C intensity at this point (which presumably is non-negative).
            SIG=DPOS/GAIN(IFRAME)+RN(IFRAME)+(0.0075*DPOS)**2+
     &          (PKERR*(DPOS-SKYBAR(ICOL)))**2
            NUMER(I,ICOL)=NUMER(I,ICOL)+DFDSIG*D/SIG
            DENOM(I,ICOL)=DENOM(I,ICOL)+DFDSIG**2/SIG
#endif
 2330     CONTINUE

C After the first iteration, reduce the weight of a bad pixel.  A pixel
C having a residual of 2.5 sigma gets reduced to half weight; a pixel
C having a residual of 5. sigma gets weight 1/257.
          IF (NOWEIGHT .LE. 1) WT=WT/SIGSQ
          IF (NOWEIGHT .LE. 0 .OR. NOWEIGHT .EQ. 3) THEN
            IF (NITER .GT. NCLIP) 
     &        WT=WT/(1.+(0.4*RELERR/CHIOLD(IND(1),ICOL))**WEIGHTEXPO)
          END IF
          DWT=D*WT
          SUMRES2 = SUMRES2 + D**2*WT


C Now work this pixel into the normal matrix.

C    Sky parameters
          if (isky .ge. 1) then
            do 6511 ii = iskystart+1,iskystart+jterm
              xwt = wt*x(ii)
              do 6512 jj = iskystart+1,iskystart+jterm
        if (watch .gt. 1.5 .and. igrp .eq. 8358) print *, ix, iy, wt, 
     &        x(ii), ii, jj, x(jj)
                c(jj,ii) = c(jj,ii) + xwt*x(jj)
6512          continue
              v(ii) = v(ii) + dwt*x(ii)
6511        continue
          end if

C    Frame shift parameters
          if (iframe .gt. 1 .and. .not. lockdpos) then
            do 6521 ii = iframestart+1,iframestart+2
              xwt = wt*x(ii)
              do 6522 jj = iframestart+1,iframestart+2
                C(JJ,II) = C(JJ,II) + xwt*x(jj)
6522          continue
              v(ii) = v(ii) + dwt*x(ii)
              do 6523 jj = iskystart+1,iskystart+jterm
                c(ii,jj) = c(ii,jj) + xwt*x(jj)
6523          continue
6521        continue
          end if

C    Star parameters
          ISTAR = 0
          DO 2370 I=1,NSTR
            IF (ID(IND(I),1) .LE. 0) GOTO 2370
            ISTAR = ISTAR+1
            IF (SKIP(IND(I))) GO TO 2370
            I1 = (ISTAR-1)*NSPAR+1
            I2 = ISTAR*NSPAR
C          frame scale if needed
            if (dynamicgroup .and. .not. lockphot(iframe)) then
              vscale(iscalefit) = vscale(iscalefit) +
     &          dwt*x(i1+icol-1)*mag(ind(i),icol)
              cscale(iscalefit,iscalefit) = cscale(iscalefit,iscalefit) + 
     &          wt*x(i1+icol-1)*mag(ind(i),icol)*x(i1+icol-1)*mag(ind(i),icol)
            end if
            DO 2340 K=I1,I2
              xwt = wt*x(k)
              if (isky .ge. 1) then
                do 6515 ii=iskystart+1,iskystart+jterm
                  c(ii,k) = c(ii,k) + xwt*x(ii)
6515            continue
              end if
              if (iframe .gt. 1 .and. .not. lockdpos) then
                do 6517 ii = iframestart+1,iframestart+2
                  c(ii,k) = c(ii,k) + xwt*x(ii)
6517            continue
              end if

C	      if (imode .eq. 0) then
                V(K)=V(K)+X(K)*DWT
C	      else if (imode .eq. 1) then
C	        v(k) = v(k) + dwt
C	        sumv(k) = sumv(k) + x(k)
C	      end if
 2340       CONTINUE 
            JSTAR=0
            DO 2360 J=1,I
              IF (ID(IND(J),1) .LE. 0) GOTO 2360
              JSTAR=JSTAR+1
              IF (SKIP(IND(J))) GO TO 2360
              J1 = (JSTAR-1)*NSPAR+1
              DO 2355 K=I1,I2
                J2 = MIN(K,JSTAR*NSPAR)
                XWT = WT*X(K)
                DO 2350 L=J1,J2
C	          IF (IMODE .EQ. 0) THEN
                    C(K,L)=C(K,L)+XWT*X(L)
C	IF (K .EQ. L) PRINT *, K, IFRAME, IX, IY, XWT, (X(LL),LL=1,NTERM)
C	          ELSE
C	            C(K,L)=C(K,L)+WT
C	          END IF
 2350           CONTINUE
 2355         CONTINUE
 2360       CONTINUE
 2370     CONTINUE

	if (watch .gt. 2.5) print *, ix, iy, c(1,1), x(1), wt

 2380   CONTINUE
 2390 CONTINUE

      DO 2393 J=1,NLOOP
        I = IND2(J)
        XC(I) = XC(I) - DXPOS(I,IFRAME)
        YC(I) = YC(I) - DYPOS(I,IFRAME)
 2393 CONTINUE

C  If we are missing all stars on first frame, then we cant fit for 
C    positions. Find out if this is the case.
      IF (IFRAME .EQ. 1) THEN
        POSFIT = .FALSE.
        ISTAR = 0
        DO 2395 I=1,NSTR
          IF (ID(IND(I),1) .LE. 0) GOTO 2395
          ISTAR = ISTAR+1
          K = (ISTAR-1)*NSPAR
          IF (C(K+NCOLOR+1,K+NCOLOR+1) .GT. 0) POSFIT = .TRUE.
          IF (C(K+NCOLOR+2,K+NCOLOR+2) .GT. 0) POSFIT = .TRUE.
 2395   CONTINUE

      END IF

 9901 CONTINUE

C  If we cant fit for positions, zero out those terms not
      IF (.NOT. POSFIT) THEN
        ISTAR = 0
        DO 2396 I=1,NSTR
          IF (ID(IND(I),1) .LE. 0) GOTO 2396
          ISTAR = ISTAR+1
          K = (ISTAR-1)*NSPAR
          C(K+NCOLOR+1,K+NCOLOR+1) = 0.
          C(K+NCOLOR+2,K+NCOLOR+2) = 0.
 2396   CONTINUE
      END IF

C  Alternate fitting mode
C      if (imode .eq. 1) then
C	do 2395 j=1,nterm
C          v(j) = v(j)*sumv(j)
C	  do 2397 k=1,j
C	    c(j,k) = c(j,k)*sumv(j)*sumv(k)
C2397      continue
C2395    continue
C      end if

C Make sure that every star in the group has at least four valid pixels
C within one fitting radius.
      REDO=.FALSE.
      DO 2400 J=1,NSTR
         I = IND(J)
         IF (ID(I,1) .LE. 0) GOTO 2400
         IF (NPIX(I) .GE. 4) GO TO 2400
            REDO=.TRUE.
            if (watch .gt. 1.5) then
	    PRINT *, I, 'at 2'
      	    PRINT *, ID(I,1)
            end if
            NI=INT(ALOG10(ID(I,1)+0.5))+2
            FORMSTR = ' '
            IF (WATCH .GT. 0.5) THEN
	print *, 'star didnt have enough pixels: ', i, id(i,1), npix(i)
               WRITE(FORMSTR,624,ERR=901) NI
               WRITE (6,FORMSTR,ERR=902) NITER, NSTR, NTOT, ID(I,1)
            ELSE
               WRITE(FORMSTR,625,ERR=901) NI
               IF (WATCH .LT. 0.5) WRITE (6,FORMSTR,ERR=902) ID(I,1)
            END IF
            ID(I,1) = -1 * ABS(ID(I,1))
            NFIT = NFIT-1
C            CALL REMOVEIT(I,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR,MAXCOL)
            NTOT=NTOT-1
            IF (NFIT .LE. 0) GO TO 2001
            IF (WATCH .GT. 0.5) CALL TBLANK
            NTERM = GETTERM(NFIT,NCOLOR,NFRAMES,ISKY,LOCKPOS,LOCKDPOS,RESOLVE,
     &         NSTARTERM,NSPAR)
 2400 CONTINUE
      IF (REDO) THEN
         GO TO 2210
      END IF

C Compute the robust estimate of the standard deviation of the
C residuals for the group as a whole, and for each star.  This
C estimate is SQRT(PI/2) * Weighted mean absolute relative residual
C (Do you like that "absolute relative" stuff?):
C
C          CHI = 1.2533 * SUM(weight*resid)/(no. of pixels)
C
C This gets corrected for bias by being multiplied by
C
C              SQRT[(no. of pixels)/(no. of pixels - 3)].
C

	if (watch .gt. 2.5) print *, 'computing chiold', ncolor, nstr
      DO 6610 ICOL = 1, NCOLOR
        DO 2420 J=1,NSTR
          I = IND(J)
          CHIOLD(I,ICOL) = 1.
	if (watch .gt. 2.5) print *, j, i, id(i,1)
          IF (ID(I,1) .LE. 0) GOTO 2420
          IF (J .EQ. 1) THEN
            IF (GRPWT(ICOL) .GT. 3) THEN
              CHIOLD(I,ICOL)=
     &          1.2533*SUMRES(ICOL)*SQRT(1./(GRPWT(ICOL)*(GRPWT(ICOL)-3.)))
C But then I drive the value toward unity, depending on exactly how
C many pixels were involved:  if CHIOLD is based on exactly a total
C weight of 3, then it is extremely poorly determined, and we just
C want to keep CHIOLD = 1.  The larger GRPWT is, the better determined
C CHIOLD is, and the less we want to force it toward unity.  So,
C just take the weighted average of CHIOLD and unity, with weights
C GRPWT-3 and 1, respectively.
              CHIOLD(I,ICOL)=((GRPWT(ICOL)-3.)*CHIOLD(I,ICOL)+3.)/GRPWT(ICOL)
	if (watch .gt. 2.5) print *, 'chiold: ', chiold(i,icol),grpwt(icol)
            ELSE
              CHIOLD(I,ICOL)=1.
	if (watch .gt. 2.5) print *, 'chiold: ', chiold(i,icol),grpwt(icol)
            END IF
          ELSE
            CHIOLD(I,ICOL)=CHIOLD(IND(1),ICOL)
          END IF

C CHIOLD has been pulled toward its expected value of unity to keep the
C statistics of a small number of pixels from compeletely dominating
C the error analysis.  Similarly, the photometric errors for the
C individual stars will be pulled toward unity now.  Later on, if the
C number of stars in the group is greater than one, CHI will be nudged
C toward the group average.  In order to work optimally, of
C course, this requires that PHPADU, RONOIS, and the other noise
C contributors which I have postulated properly represent the true
C errors expected in each pixel.

          IF (SUMWT(I,ICOL) .GT. 3.) CHI(I,ICOL)=
     &     1.2533*CHI(I,ICOL)*SQRT(1./((SUMWT(I,ICOL)-3.)*SUMWT(I,ICOL)))
          IF (SUMWT(I,ICOL) .GT. 3.) 
     &     CHI(I,ICOL)=((SUMWT(I,ICOL)-3.)*CHI(I,ICOL)+3.)/SUMWT(I,ICOL)
          IF (SUMWT(I,ICOL) .LE. 3.) CHI(I,ICOL)=CHIOLD(I,ICOL)
 2420   CONTINUE

6610  CONTINUE

C Reflect the normal matrix across the diagonal.
      DO 2410 L=2,NTERM
        DO 2410 K=1,L-1
 2410     C(K,L)=C(L,K)

C Are we missing data needed for any of the parameters (from bad pixels, etc.)?
      LL = 0
      DO L=1,NTERM
        IF (C(L,L) .GT. 0) THEN
          LL = LL + 1
          INDPAR(LL) = L
        ELSE IF (WATCH .GT. 1.5) THEN
          PRINT *, 'Error with parameter: ', L, C(L,L)
	  do i=1,nterm
            print *, i, c(i,i)
          end do
	  print *, 'group: ', (id(ind2(j),1),j=1,nloop)
          print *, 'stars: ', istar
	  do i=1,nstr
            j=ind(i)
            if (id(j,1) .gt. 0) print *, id(j,1), skip(j), skipall(j),
     &          group(j), igrp, npix(j), mag(j,1), xc(j), yc(j),ixmin,
     &          ixmax,iymin,iymax
          end do
        END IF
      END DO

      IF (LL .EQ. 0) THEN
        IF (WATCH .GT. 0.5) PRINT *, 'No good parameters for this group!!'
        GOTO 2001
      END IF

C Load up matrix to invert, sans bad parameters
      DO L=1,LL
        DO K = 1, LL
          C2(K,L) = C(INDPAR(K),INDPAR(L))
        END DO
      END DO

C  Get the corrections and uncertainties by using the appropriate method
      if (ifit .eq. 1 .and. mod(niter,4) .ne. 0 .and.
     &          .not. computeerrors) then
        do 8702 i=1,nterm
          x(i) = 0.
 8702   continue
        itol = 1
        tol = 1.e-4
        itmax = 2*nterm
        call bcgms(nterm,v,x,msolve,mtsolv,itol,tol,itmax,iter,err,0,0)
        do 8703 i=1,nterm
          c(i,i) = 0.
 8703   continue
      else
        if (watch .gt. 1.5) then
	  do i=1,nterm
	    print *, 'Term: ', i
            print *, (c(j,i),j=1,nterm)
          end do
	  print *, 'v: '
          print *, (v(j),j=1,nterm)
        end if
C        CALL INVERS (C, MAXUNK, NTERM, ISTAT)
        CALL INVERS (C2, MAXUNK, LL, ISTAT)
        do i=1,ll
          if (c2(i,i) .le. 0.) istat = 1
        end do
	if (istat .ne. 0) then
          if (watch .gt. 0.5)
     &          print *, 'trying double precision inversion ...', nterm, ll
          DO L=1,LL
            DO K = 1, LL
              C8(K,L) = C(INDPAR(K),INDPAR(L))
            END DO
            if (watch .gt. 1.5) print *, (c8(k,l),k=1,ll)	
          END DO
          CALL INVERS8 (C8, MAXUNK, LL, ISTAT)
          do l=1,ll
              if (watch .gt. 1.5) print *, (c8(k,l),k=1,ll)	
              do k=1,ll
                c2(k,l) = c8(k,l)
              end do
          end do
        end if

        do i=1,ll
          if (c2(i,i) .le. 0.) istat = 1
        end do
	if (istat .ne. 0) then
          if (watch .gt. 0.5) print *, 'error inverting matrix ...', nterm, ll
          if (watch .gt. 1.5) then
	    do i=1,nterm
              print *, i, c(i,i), c2(i,i), c8(i,i)
            end do
            print *, 'stars: ', istar
	    do i=1,nstr
              j=ind(i)
              if (id(j,1) .gt. 0) print *, id(j,1), skip(j), skipall(j),
     &            group(j), igrp, npix(j), mag(j,1), xc(j), yc(j),ixmin,
     &            ixmax,iymin,iymax
            end do
          end if
          if (watch .gt. 2.5) pause
          goto 2001
        end if
        DO L=1,NTERM
          DO K = 1, NTERM
            C(K,L) = 0.
          END DO
        END DO
        DO L=1,LL
          DO K = 1, LL
            C(INDPAR(K),INDPAR(L)) = C2(K,L)
          END DO
        END DO
        CALL VMUL (C, MAXUNK, NTERM, V, X)
        if (watch .gt. 1.5) then
	  do i=1,nterm
	    print *, 'Term: ', i
            print *, (c(j,i),j=1,nterm)
          end do
	  print *, 'x: '
          print *, (x(j),j=1,nterm)
        end if
      end if

      REDO=.FALSE.
      IF (NITER .LE. 2) REDO=.TRUE.
      IF (REDOSCALE) REDO = .TRUE.

C   Now correct the frame parameters:  sky and frame offsets
      do 9902 iframe=1, nframes
        icol = colors(iframe)
        if (isky .eq. 1) then
          j = nstarterm+(iframe-1)+1
C      If derivative has changed sign, reduce clamp value
          skyold = skyclamp(1,ind(1),iframe,2)
          istar=0
          ii=0
          do k=1,nstr
            i = ind(k) 
	if (watch .gt. 1.5) print *, iframe, k, i, id(i,1)
            if (id(i,1) .gt. 0 .and. x(j) .ne. 0) then
              istar=istar+1
              if (istar .eq. 1) then
                ii = i
                if (skyold*x(j) .lt. -1.e-36) then
#ifdef __HAVEBYTE
                  skyclamp(1,i,iframe,1) = 0.5*(skyclamp(1,i,iframe,1)+129)-129
                  skyclamp(1,i,iframe,1) = max(-127,skyclamp(1,i,iframe,1))
#else
                  skyclamp(1,i,iframe,1) = 0.5*skyclamp(1,i,iframe,1)
                  skyclamp(1,i,iframe,1) = max(0.002,skyclamp(1,i,iframe,1))
#endif
                end if
#ifdef __HAVEBYTE
                skyclamp(1,i,iframe,2) = x(j)
#else
                skyclamp(1,i,iframe,2) = x(j)/abs(x(j))
#endif
              else if (ii .gt. 0) then
                skyclamp(1,i,iframe,1) = skyclamp(1,ii,iframe,1)
                skyclamp(1,i,iframe,2) = skyclamp(1,ii,iframe,2)
              end if
            end if
          end do

C       Adjust sky value for all stars in group
#ifdef __HAVEBYTE
          delsky = x(j)/(1.+abs(x(j))/(5.*((129+skyclamp(1,ind(1),iframe,1))/256)))
#else
          delsky = x(j)/(1.+abs(x(j))/(5.*skyclamp(1,ind(1),iframe,1)))
#endif
          if (abs(delsky) .gt. 0.01) redo = .true.
          do k=1,nstr
            i = ind(k) 
            if (id(i,1) .gt. 0) then
	if (i .lt. 1 .or. i .gt. maxstr) print *, 'sky', 3, i
	if (iframe .lt. 1 .or. iframe .gt. maxfrm) print *, 'sky', 3, iframe
              skyparam(1,i,iframe) = skyparam(1,i,iframe) - delsky
            end if
          end do
	  if (watch .gt. 1.5) print *, 'iframe, sky: ', iframe, 
     &      skyparam(1,ind(1),iframe), delsky, skyclamp(1,ind(1),iframe,1)
        else if (isky .gt. 1) then
          if (watch .gt. 1.5)
     &     print *, 'old skyparam: ', (skyparam(ii,ind(1),iframe),ii=1,jterm)
          do k=1,nstr
            i = ind(k) 
            if (id(i,1) .gt. 0) then
              do 6516 ii = 1, jterm
                skyparam(ii,i,iframe) = skyparam(ii,i,iframe) - 
     &             x(nstarterm+(iframe-1)*jterm+ii)
6516          continue
              if (watch .gt. 1.5) then
                print *, 'new skyparam: ', (skyparam(ii,i,iframe),ii=1,jterm)
                print *, 'x: ', (x(nstarterm+(iframe-1)*jterm+ii),ii=1,jterm)
              end if
            end if
          end do
        end if

C   Now correct the frame offsets. Only allow 0.5 pixels per iteration max.
        IF (IFRAME .GT. 1 .AND. .NOT. LOCKDPOS) THEN
          if (watch .gt. 2.5) 
     &      print *, 'x: ', (x(nstarterm+nframes*jterm+ii),ii=1,2)
          K = NSTARTERM+NFRAMES*SKYTERM(ISKY)+(IFRAME-2)*2+1
          L = K+1
          ISTAR = 0
          DO 6518 J=1,NSTR
            I = IND(J)
            IF (ID(I,1) .LE. 0) GOTO 6518
            ISTAR = ISTAR + 1
            IF (ISTAR .EQ. 1) THEN
              II = I
#ifdef __HAVEBYTE
              IF (C(K,K) .GT. 0) THEN
                DXPOS(I,IFRAME) = DXPOS(I,IFRAME) - 
     &            X(K)/(1.+ABS(X(K))/(0.5*((129+DXPOSCLAMP(I,IFRAME,1))/256.)))
                IF (DXPOSCLAMP(I,IFRAME,2)*X(K) .LT. -1.E-36) 
     &            DXPOSCLAMP(I,IFRAME,1)=0.5*(DXPOSCLAMP(I,IFRAME,1)+129)-129
                DXPOSCLAMP(I,IFRAME,1) = MAX(DXPOSCLAMP(I,IFRAME,1),-127)
                DXPOSCLAMP(I,IFRAME,2) = X(K)/ABS(X(K))
              END IF
              IF (C(L,L) .GT. 0) THEN
                DYPOS(I,IFRAME) = DYPOS(I,IFRAME) - 
     &            X(L)/(1.+ABS(X(L))/(0.5*((129+DYPOSCLAMP(I,IFRAME,1))/256.)))
                IF (DYPOSCLAMP(I,IFRAME,2)*X(L) .LT. -1.E-36) 
     &            DYPOSCLAMP(I,IFRAME,1)=0.5*(DYPOSCLAMP(I,IFRAME,1)+129)-129
                DYPOSCLAMP(I,IFRAME,1) = MAX(DYPOSCLAMP(I,IFRAME,1),-127)
                DYPOSCLAMP(I,IFRAME,2) = X(L)/ABS(X(L))
              END IF
#else
              IF (C(K,K) .GT. 0) THEN
                DXPOS(I,IFRAME) = DXPOS(I,IFRAME) - 
     &            X(K)/(1.+ABS(X(K))/(0.5*DXPOSCLAMP(I,IFRAME,1)))
                IF (DXPOSCLAMP(I,IFRAME,2)*X(K) .LT. -1.E-36) 
     &            DXPOSCLAMP(I,IFRAME,1)=0.5*DXPOSCLAMP(I,IFRAME,1)
                DXPOSCLAMP(I,IFRAME,1) = MAX(DXPOSCLAMP(I,IFRAME,1),0.001)
                DXPOSCLAMP(I,IFRAME,2) = X(K)
	if (watch .gt. 2.5 .and. 
     &      abs(dxpos(i,iframe)-dxposorig(i,iframe)) .gt. 20) then
            print *, i, iframe, dxpos(i,iframe), dxposorig(i,iframe),dposmax
            pause
        end if
                IF ( ABS(DXPOS(I,IFRAME)-DXPOSORIG(I,IFRAME)) .GT. DPOSMAX )
     &            DXPOS(I,IFRAME) = DXPOSORIG(I,IFRAME) + 
     &            DPOSMAX*(DXPOS(I,IFRAME)-DXPOSORIG(I,IFRAME))/
     &            ABS(DXPOS(I,IFRAME)-DXPOSORIG(I,IFRAME))
              END IF
              IF (C(L,L) .GT. 0) THEN
                DYPOS(I,IFRAME) = DYPOS(I,IFRAME) - 
     &            X(L)/(1.+ABS(X(L))/(0.5*DYPOSCLAMP(I,IFRAME,1)))
                IF (DYPOSCLAMP(I,IFRAME,2)*X(L) .LT. -1.E-36) 
     &            DYPOSCLAMP(I,IFRAME,1)=0.5*DYPOSCLAMP(I,IFRAME,1)
                DYPOSCLAMP(I,IFRAME,1) = MAX(DYPOSCLAMP(I,IFRAME,1),0.001)
                DYPOSCLAMP(I,IFRAME,2) = X(L)
	if (watch .gt. 2.5 .and. 
     &      abs(dypos(i,iframe)-dyposorig(i,iframe)) .gt. 20) then
            print *, i, iframe, dypos(i,iframe), dyposorig(i,iframe),dposmax
            pause
        end if
                IF ( ABS(DYPOS(I,IFRAME)-DYPOSORIG(I,IFRAME)) .GT. DPOSMAX )
     &            DYPOS(I,IFRAME) = DYPOSORIG(I,IFRAME) + 
     &            DPOSMAX*(DYPOS(I,IFRAME)-DYPOSORIG(I,IFRAME))/
     &            ABS(DYPOS(I,IFRAME)-DYPOSORIG(I,IFRAME))
              END IF
#endif
              if (watch .gt. 1.5)
     &          print *, 'iframes, dpos: ', iframe, dxpos(i,iframe), dypos(i,iframe)
            ELSE
              DXPOS(I,IFRAME) = DXPOS(II,IFRAME)
              DYPOS(I,IFRAME) = DYPOS(II,IFRAME)
              DXPOSCLAMP(I,IFRAME,1) = DXPOSCLAMP(II,IFRAME,1)
              DYPOSCLAMP(I,IFRAME,1) = DYPOSCLAMP(II,IFRAME,1)
              DXPOSCLAMP(I,IFRAME,2) = DXPOSCLAMP(II,IFRAME,2)
              DYPOSCLAMP(I,IFRAME,2) = DYPOSCLAMP(II,IFRAME,2)
            END IF
6518      CONTINUE
        END IF

9902  CONTINUE

C In the beginning, the brightness of each star will be permitted to
C change by no more than two magnitudes per iteration, and the x,y
C coordinates of each centroid will be permitted to change by no more
C than 0.4 pixel per iteration.  Any time that the parameter
C correction changes sign from one iteration to the next, the maximum
C permissible change will be reduced by a factor of two.  These
C clamps are released any time a star disappears.
      ISTAR = 0
      DO 2520 II=1,NSTR
        I = IND(II)
        IF (ID(I,1) .LE. 0) GOTO 2520
        ISTAR = ISTAR + 1
C  Position change indices
        K = (ISTAR-1)*NSPAR + NCOLOR

#ifdef __SHARP
        DO 6611 ICOL = 1, NCOLOR
          IF (DENOM(I,ICOL)*PEAK*MAG(I,ICOL) .NE. 0) THEN
            SHARP(I,ICOL)=2.*PROD*NUMER(I,ICOL)/(MAG(I,ICOL)*PEAK*DENOM(I,ICOL))
          ELSE
            SHARP(I,ICOL) = 99.999
          END IF
6611    CONTINUE
#endif

C If you already know that the solution hasn't converged, don't bother
C to keep checking.
        IF (REDO .OR. (LOCKPOS .AND. IGROUP .LT. 4)) GO TO 2510

C For IGROUP=2 option, only need the first star to converge
        IF ((IGROUP .EQ. 2 .OR. IGROUP .EQ. 3) .AND. 
     &      ID(I,1) .GT. 50000) goto 2510

C Check for convergence in stellar magnitudes 
        PRAT = 0.01
        IF (IPSFMODE .LE. 0) PRAT = 1./MAXNLIB/2.
        DO 6617 ICOL = 1, NCOLOR
           J = (ISTAR-1)*NSPAR+ICOL
           IF (ICONVERGE .EQ. 0) THEN
             CONVERGE = MAX( 0.05*CHI(I,ICOL)*SQRT(MAX(0.001,C(J,J))), 
     &                       0.001*MAG(I,ICOL) ) 
C  7/97 - Get rid of MAX(0.001... because it converges too fast for very faint objects
             IF (C(J,J) .GT. 0) THEN
               CONVERGE = MAX( 0.05*CHI(I,ICOL)*SQRT(C(J,J)), 
     &                       0.001*MAG(I,ICOL) ) 
             ELSE
               CONVERGE = 0.001*MAG(I,ICOL) 
             END IF
           ELSE
             CONVERGE = 0.001*MAG(I,ICOL) 
           ENDIF
           IF (ABS(X(J)) .GT. CONVERGE) THEN
             REDO=.TRUE.
C     If we have set MAG to faintest, dont redo for this star
             IF (MAG(I,ICOL) .EQ. FAINTEST) REDO = .FALSE.
           END IF
6617    CONTINUE

C Check for convergence in stellar position
        IEXTRA = 0
	IF (.NOT. LOCKPOS) THEN
          IF (MAX( ABS(X(K+1)), ABS(X(K+2)) ) .GT. PRAT) REDO=.TRUE.
          IEXTRA = 2
	END IF
        IF (RESOLVE) THEN
          IF (ABS(X(K+IEXTRA+1)) .GT. 0.05) REDO=.TRUE.
        END IF

 2510 CONTINUE

      if (watch .gt. 1.5) then
	print *, 'sumres2: ', sumres2
	print *, 'old x, y, mag: '
	print *, xc(i),yc(i), (mag(i,icol),icol=1,ncolor)
	print *, '  parameter changes to be applied: '
        do 6699 icol = 1, ncolor
          j = (ISTAR-1)*nspar+icol
	  print *, x(j),x(k+1),x(k+2)
6699    continue
	print *, 'convergence parameters: '
        do 6618 icol = 1, ncolor
          j = (ISTAR-1)*nspar+icol
C	  print *, chi(i,icol),0.05*sqrt(max(0.001,c(j,j)))*chi(i,icol),
          if (c(j,j) .gt. 0) then
	    print *, chi(i,icol),0.05*sqrt(c(j,j))*chi(i,icol),
     &       0.001*mag(i,icol),c(j,j),converge,redo
          else
	    print *, chi(i,icol),0.0,
     &       0.001*mag(i,icol),c(j,j),converge,redo
          end if
6618    continue
	print *
      end if

C If any correction has changed sign since the last iteration, reduce
C the maximum permissible change by a factor of 2.
      DO 6619 ICOL = 1, NCOLOR
        J = (ISTAR-1)*NSPAR+ICOL
#ifdef __HAVEBYTE
        IF (MAGCLAMP(I,ICOL,2)*X(J)/MAG(I,ICOL)**2 .LT. -1.E-36) 
     &      MAGCLAMP(I,ICOL,1)=0.5*(MAGCLAMP(I,ICOL,1)+129)-129
        MAGCLAMP(I,ICOL,1) = MAX(MAGCLAMP(I,ICOL,1),-128)
#else
        IF (MAGCLAMP(I,ICOL,2)*X(J)/MAG(I,ICOL)**2 .LT. -1.E-36) 
     &      MAGCLAMP(I,ICOL,1)=0.5*MAGCLAMP(I,ICOL,1)
        MAGCLAMP(I,ICOL,1) = MAX(MAGCLAMP(I,ICOL,1),0.0001)
#endif
6619  CONTINUE
      IEXTRA = 0
      K = (ISTAR-1)*NSPAR + NCOLOR
      IF (.NOT. LOCKPOS) THEN
        IEXTRA = IEXTRA + 2
#ifdef __HAVEBYTE
        IF (XCCLAMP(I,2)*X(K+1) .LT. -1.E-36) 
     &      XCCLAMP(I,1)=0.5*(XCCLAMP(I,1)-129) + 129
        IF (YCCLAMP(I,2)*X(K+2) .LT. -1.E-36) 
     &      YCCLAMP(I,1)=0.5*(YCCLAMP(I,1)-129) + 129
        XCCLAMP(I,1) = MAX(XCCLAMP(I,1),-127)
        YCCLAMP(I,1) = MAX(YCCLAMP(I,1),-127)
#else
        IF (XCCLAMP(I,2)*X(K+1) .LT. -1.E-36) XCCLAMP(I,1)=0.5*XCCLAMP(I,1)
        IF (YCCLAMP(I,2)*X(K+2) .LT. -1.E-36) YCCLAMP(I,1)=0.5*YCCLAMP(I,1)
        XCCLAMP(I,1) = MAX(XCCLAMP(I,1),0.001)
        YCCLAMP(I,1) = MAX(YCCLAMP(I,1),0.001)
#endif
      END IF

C Note that the sign of the correction is such that it must be
C SUBTRACTED from the current value of the parameter to get the
C improved parameter value.  This being the case, if the correction
C to the brightness is negative (the least-squares thinks that the
C star should be brighter) a change of 1 magnitude is a change of a
C factor of 2.5; if the brightness correction is positive (the star
C should be fainter) a change of 1 magnitude is a change of 60%.

      IF (LOCKPOS .AND. IGROUP .LE. 3 .AND. .NOT. RESOLVE) THEN
        DO 6620 ICOL = 1, NCOLOR
          J = (ISTAR-1)*NSPAR+ICOL
	  MAG(I,ICOL) = MAG(I,ICOL) - X(J)
6620    CONTINUE
      ELSE
        DO 6621 ICOL = 1, NCOLOR
          J = (ISTAR-1)*NSPAR+ICOL
#ifdef __HAVEBYTE
          MAG(I,ICOL)=MAG(I,ICOL)-X(J)/
     &      (1.+MAX( X(J)/(0.8415*MAG(I,ICOL)) , -X(J)/(5.309*MAG(I,ICOL)) )/
     &       ((129+MAGCLAMP(I,ICOL,1))/256.))
          MAGCLAMP(I,ICOL,2) = X(J)/ABS(X(J))
#else
          MAG(I,ICOL)=MAG(I,ICOL)-X(J)/
     &      (1.+MAX( X(J)/(0.8415*MAG(I,ICOL)) , -X(J)/(5.309*MAG(I,ICOL)) )/
     &       MAGCLAMP(I,ICOL,1))
          MAGCLAMP(I,ICOL,2) = X(J)
#endif
	if (watch .gt. 2.5) print *, icol, j, mag(i,icol), x(j)
6621    CONTINUE
        IEXTRA = 0
        IF (.NOT. LOCKPOS) THEN
          IEXTRA = IEXTRA + 2
#ifdef __HAVEBYTE
          XCCLAMP(I,2) = X(K+1)/ABS(X(K+1))
          YCCLAMP(I,2) = X(K+2)/ABS(X(K+2))
          XC(I)=XC(I)-X(K+1)/(1.+ABS(X(K+1))/(((XCCLAMP(I,1)+129)/256)*0.4))
          YC(I)=YC(I)-X(K+2)/(1.+ABS(X(K+2))/(((YCCLAMP(I,1)+129)/256)*0.4))
#else
          XCCLAMP(I,2) = X(K+1)
          YCCLAMP(I,2) = X(K+2)
          XC(I)=XC(I)-X(K+1)/(1.+ABS(X(K+1))/(XCCLAMP(I,1)*0.4))
          YC(I)=YC(I)-X(K+2)/(1.+ABS(X(K+2))/(YCCLAMP(I,1)*0.4))
#endif
        END IF
        IF (RESOLVE) THEN
          RSIG(I,1) = RSIG(I,1) - X(K+IEXTRA+1)
        END IF
	if (watch .gt. 2.5) print *, i, xc(i), yc(i), x(k+1), x(k+2)
      END IF

C      if (watch .gt. 2.5) pause

C  Don''t let stars move more than DRIFT. With igroup=2, if this is an edge
C    star, however, let it drift, in case there''s a bright star just outside
C    of the grouping radius, which we would rather include than some faint
C    star just inside of it.
      IF ((IGROUP .EQ. 2 .OR. IGROUP .EQ. 3) .AND.
     &    ID(I,1) .GT. 50000) GOTO 627

      IF ((XC(I)-XCORIG(I))**2+(YC(I)-YCORIG(I))**2 .GT. DRIFT) THEN
	K = I
	if (watch .gt. 2.5) print *, 'drift: ', id(k,1)
        if (watch .gt. 1.5) then
	print *, k, 'at 3'
	print *, id(k,1)
 	end if
        NK=ALOG10(ID(K,1)+0.5)+2
        FORMSTR = ' '
        WRITE(FORMSTR,626,ERR=901) NK
  626   FORMAT ('(''+'', 3I5, 5X, ''Star'', I',I2.2,
     &  ','' moved too much, setting it back to original position.'')')
        IF (WATCH .GT. 0.5) WRITE (6,FORMSTR,ERR=902) 
     &        NITER, NSTR, NTOT, ID(K,1)
        XC(I) = XCORIG(I)
        YC(I) = YCORIG(I)
      END IF

627   CONTINUE

C Drive the magnitude error toward the expected value, as above.

      DO 6622 ICOL = 1, NCOLOR
        J = (ISTAR-1)*NSPAR+ICOL
	if (watch .gt. 1.5) then
         print *, icol, j, magerr(i, icol), c(j,j), chi(i,icol),chiold(i,icol),nstr
	  print *, chiold(i,icol)**2
        end if
        MAGERR(I,ICOL)=C(J,J)*
     &     (NSTR*CHI(I,ICOL)**2+(NSTR-1)*CHIOLD(I,ICOL)**2)/(2.*NSTR-1.)

C Store in MAGERR the square of the fractional error, since this is all we use
        MAGERR(I,ICOL) = MAGERR(I,ICOL) / MAG(I,ICOL)**2

	if (watch .gt. 1.5) 
     &    print *, icol, j, magerr(i, icol), c(j,j), chi(i,icol),chiold(i,icol)
6622  CONTINUE
 2520 CONTINUE

      if (lockpos .and. igroup .lt. 4) goto 2909

C Check whether the estimated centroid of any star has moved so far out
C of the limits of the picture that it has fewer than four or five
C pixels within one fitting radius.

      J=0
 2525 J=J+1
      IF (J .GT. NSTR) GO TO 2528
      I=IND(J)
      DX=MAX( 1.-XC(I), XC(I)-NCOL, 0.)
      DY=MAX( 1.-YC(I), YC(I)-NROW, 0.)

C If the centroid of the star is outside the picture in x or y, then
C DX or DY is its distance from the center of the edge pixel; otherwise
C DX and DY are zero.

      IF ((DX .LE. 0.001) .AND. (DY .LE. 0.001)) GO TO 2525
      IF ( (DX+1.)**2+(DY+1.)**2 .LT. RADSQ) GO TO 2525
      if (watch .gt. 1.5) then
	print *, i, 'at 4'
	print *, id(i,1)
      end if
      NI=INT(ALOG10(ID(I,1)+0.5))+2
      FORMSTR = ' '
	print *, 'star outside picture: ', id(i,1)
      IF (WATCH .GT. 0.5) THEN
        WRITE(FORMSTR,624,ERR=901) NI
        WRITE (6,FORMSTR,ERR=902) NITER, NSTR, NTOT, ID(I,1)
      ELSE
        WRITE(FORMSTR,625,ERR=901) NI
        WRITE (6,FORMSTR,ERR=902) ID(I,1)
      END IF
      ID(I,1) = -1 * ABS(ID(I,1))
      NFIT = NFIT-1
C      CALL REMOVEIT(I,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR,MAXCOL)
      NTOT=NTOT-1
C    No stars left in group?
      IF (NFIT .LE. 0) GO TO 2001
      REDO=.TRUE.

C Update display on terminal.

      IF (WATCH .GT. 0.5) CALL TBLANK
      IF (WATCH .GT. 0.5) WRITE (6,622,ERR=902) NITER, NSTR, NTOT
      IF (I .LT. NSTR) GO TO 2525
C                               ^<-------------------------------------+
C                                                                      |
C End of loop to check that centroids aren''t too far from the edge     |
C frame. --------------------------------------------------------------+

C Update matrix dimensions.

 2528 CONTINUE
      NTERM = GETTERM(NFIT,NCOLOR,NFRAMES,ISKY,LOCKPOS,LOCKDPOS,RESOLVE,
     &         NSTARTERM,NSPAR)

C Now check whether any of the stars is too faint (more than 12.5
C magnitudes fainter than the PSF star).  If several stars are
C too faint, delete the faintest one, and set the brightnesses of
C the other faint ones exactly to 12.5 mag below the PSF star.
C That way on the next iteration we will see whether these stars
C want to grow or to disappear.

      FAINT=1.0
      IFAINT=0

      DO 2540 J=1,NSTR
        I = IND(J)
        IF (ID(I,1) .LE. 0) GOTO 2540
        DO 2541 ICOL=1,NCOLOR
          if (watch .gt. 1.5) print *, i, id(i,icol), icol, 
     &           magerr(i,icol), mag(i,icol), 1
          IF (MAG(I,ICOL) .GT. FAINTEST) GO TO 2541
          IF (MAG(I,ICOL) .GT. FAINT) GO TO 2530
          FAINT=MAG(I,ICOL)
          IFAINT=I
 2530     MAG(I,ICOL)=FAINTEST
 2541   CONTINUE
 2540 CONTINUE

C If at least one star is more than 12.5 mag. fainter than the
C PSF, then  I  is the index of the faintest of them, and FAINT
C is the relative brightness of the faintest of them.

C    If we have a very faint star, see if it is very faint in ALL colors.
C    If not, keep it
      IF (IFAINT .GT. 0) THEN
        JFAINT = IFAINT
        DO 2542 ICOL=1,NCOLOR
          IF (MAG(JFAINT,ICOL) .GT. FAINTEST) IFAINT = 0
2542    CONTINUE
      END IF

C    No star that faint
      IF (IFAINT .GT. 0 .AND. .NOT. KEEP .AND. .NOT. WKEEP) GO TO 2560

C If the solution has not converged and if the number of iterations
C is less than 4, perform another iteration no questions asked.

      IF (REDO .AND. (NITER .LT. 4)) GO TO 2001
      if (ifit .eq. 1) then
        if (.not. computeerrors .and. mod(niter,4) .ne. 0) then
          if (redo) then
            goto 2001
          else
            computeerrors = .true.
            goto 2001
          end if
        end if
      end if

C If the solution doesn''t think it has converged, after the fourth
C iteration delete the least certain star if it is less than a one-sigma
C detection; after the eighth iteration delete the least certain star if
C it is less than a 1.50 sigma detection; after the twelfth iteration
C OR if the solution thinks it has converged, delete the least certain
C star if it is less than a two-sigma detection.

C Don''t let the brightest star in a group be deleted by the CHI criterion
      BRIGHT=0.0
      IBRIGHT=0
      DO 2548 J=1,NSTR 
        I = IND(J)
        IF (ID(I,1) .LE. 0) GOTO 2548
        IF ((IGROUP .EQ. 2 .OR. IGROUP .EQ. 3) .AND. 
     &      ID(I,1) .GT. 50000) GOTO 2548
        DO 2549 ICOL = 1, NCOLOR
          if (watch .gt. 1.5) print *, i, id(i,icol), icol, 
     &           magerr(i,icol), mag(i,icol), 2
          IF (MAG(I,ICOL) .GT. BRIGHT) THEN
            BRIGHT = MAG(I,ICOL)
            IBRIGHT = I
          END IF
 2549   CONTINUE
 2548 CONTINUE

C  Find the most uncertain star
      FAINT=0.0
      IFAINT=0

      DO 2550 J=1,NSTR
        I = IND(J)
        IF (ID(I,1) .LE. 0) GOTO 2550
        DO 2551 ICOL = 1, NCOLOR
          if (watch .gt. 1.5) print *, i, id(i,icol), icol, 
     &           magerr(i,icol), mag(i,icol), 3
          IF (MAGERR(I,ICOL) .LT. FAINT) GO TO 2551
          FAINT=MAGERR(I,ICOL)
          IFAINT=I
 2551   CONTINUE
 2550 CONTINUE

C  Delete the most uncertain star if it fails the WCRIT or CHICRIT test
C    in all colors
      BAD = .TRUE.
      FAINT = 0.
      DO 6623 ICOL = 1, NCOLOR
	if (watch .gt. 1.5) print *, ifaint, id(ifaint,icol), icol, 
     &         magerr(i,icol), mag(i,icol),chi(ifaint,icol),chicrit
        IF (MAGERR(IFAINT,ICOL) .LT. WCRIT .AND. 
     & (CHI(IFAINT,ICOL) .LT. CHICRIT .OR. IFAINT .EQ. IBRIGHT) ) BAD = .FALSE.
        FAINT = MAX(FAINT,MAGERR(IFAINT,ICOL))
6623  CONTINUE

      if (watch .gt. 1.5) print *, redo, bad, keep, wkeep, niter

C  If we have an uncertain star, delete it
      IF (REDO .AND. BAD .AND. .NOT. KEEP .AND. .NOT. WKEEP) GOTO 2560

C    Do another iteration if called for
      IF (REDO .AND. (NITER .LT. 50) ) GOTO 2001
C     .AND. 
C     &    (.NOT. BAD .OR. KEEP .OR. WKEEP))
C     &     GO TO 2200

C  If we have converged and the least certain star is less uncertain then
C    two sigma, we''re done. Otherwise delete the least certain star
      IF (CRITDEL .LT. 0 .OR. 
     &    FAINT .LT. CRITDEL*WRATIO .OR. KEEP .OR. WKEEP) GO TO 2900

C Either (a) the solution thinks it has not converged, and the faintest
C star more uncertain than SQRT(WCRIT), or (b) the solution thinks it
C has converged and the faintest star is more uncertain than two sigma.

 2560 CONTINUE
      if (watch .gt. 1.5) then
        PRINT *, IFAINT, NSTR, NFIT, NTOT, 'at 5'
	print *, ID(IFAINT,1)
      end if
      NI=INT(ALOG10(ID(IFAINT,1)+0.5))+2
      FORMSTR = ' '
      IF (WATCH .GT. 0.5) THEN
	print *, 'deleting star: ', id(ifaint,1), faint, wcrit, wratio
        WRITE(FORMSTR,624,ERR=901) NI
  624   FORMAT ('(''+'', 3I5, 5X, ''Star'', I',I8.8,
     &   ', '' has disappeared.'')')
        WRITE (6,FORMSTR,ERR=902) NITER, NSTR, NTOT, ID(IFAINT,1)
C      ELSE
C        WRITE(FORMSTR,625,ERR=901) NI
C        WRITE (6,FORMSTR,ERR=902) ID(IFAINT,1)
  625   FORMAT('(1X, ''Star'', I',I8.8,', '' has disappeared.'')')
      END IF
      ID(IFAINT,1) = -1 * ABS(ID(IFAINT,1))
      NFIT = NFIT-1
C      CALL REMOVEIT(IFAINT,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR,MAXCOL)
      NTOT=NTOT-1
      IF (NFIT .LE. 0) GO TO 2001
      IF (WATCH .GT. 0.5) CALL TBLANK
C    Update display
      IF (WATCH .GT. 0.5)
     &     WRITE (6,622,ERR=902) NITER, NSTR, NTOT
      NTERM = GETTERM(NFIT,NCOLOR,NFRAMES,ISKY,LOCKPOS,LOCKDPOS,RESOLVE,
     &         NSTARTERM,NSPAR)

C After deleting a star, release all the clamps, back the iteration
C counter up by one, and do the next iteration without incrementing
C the counter.  That way the second most uncertain star will have
C two chances to get its act together before it comes up for tenure
C review.
      DO J=1,NSTR
        I=IND(J)
        IF (ID(I,1) .GT. 0) THEN
          CALL RESETCLAMP(I,XCCLAMP,YCCLAMP,MAGCLAMP,
     &      DXPOSCLAMP,DYPOSCLAMP,SKYCLAMP,MAXSTR,MAXCOL,MAXFRM,MAXSKY)
        END If
      END DO
      GO TO 2210

 2900 CONTINUE

C  If we''ve converged on this group, set flags for these stars
      DO J=1,NSTR
        I=IND(J)
        IF (ID(I,1) .GT. 0) THEN
          DONE(I) = .TRUE.
          NOUT(I) = NITER
          GROUP(I) = -NGROUP(IGRP)
        END IF
      END DO

 2001 CONTINUE

	if (watch .gt. 1.5) print *, 'igrp, nfit: ', igrp, nfit

      NGROUP(IGRP) = NFIT

      IF (NFIT .LE. 0) REDO = .FALSE.

 2002 CONTINUE

C  Update frame scale parameters
      if (nscalefit .gt. 0 .and. niter .gt. 8) then
	do i=1,nscalefit
	  print *, cscale(i,i), vscale(i)
	end do
        call invers(cscale,maxfrm,nscalefit,istat)
        if (istat .ne. 0) then
           print *, 'error invering frame scale matrix '
           do i=1,nscalefit
             print *, cscale(i,i)
           end do
        end if
	call vmul(cscale,maxfrm,nscalefit,vscale,xscale)
        iscalefit = 0
        do iframe=1,nframes
          if (.not. lockphot(iframe)) then
            iscalefit = iscalefit + 1
            scalephot(iframe) = scalephot(iframe) - 
     & xscale(iscalefit)/ (1. + abs(xscale(iscalefit)/(0.05*scalephot(iframe))))
	print *, iframe, scalephot(iframe), xscale(iscalefit)
          end if
        end do
      end if

	if (watch .gt. 1.5) print *, 'done groups: ', redo, igroup, niter

      IF ((REDO .OR. IGROUP .GT. 3) .AND. NITER .LT. 50) GOTO 2200

C Solution has either converged or gone to 50 iterations.

C  Here is a feature to try to delete stars on the last iteration

      IF (DIFFM .GT. 0. .AND. .NOT. LASTITER .AND. NITER .LT. 12) THEN
	LASTITER = .TRUE.
	GOTO 2200
      END IF

2909  CONTINUE

C For IGROUP=2 option, just print out first star

      if (watch .gt. 1.5) print *, 'master output: '

C  Master output
      DO ICOL = 1, NCOLOR
C    Output the sky value from one of the correct color frames - remember it
C      doesn''t apply to all frames of this color!
       DO IFRAME = 1, NFRAMES
          IF (COLORS(IFRAME) .EQ. ICOL) ISKYFRAME = IFRAME
       END DO
       DO 2810 I=1,NTOTSTR
        IF (ID(I,1) .LE. 0) GOTO 2810
        if ((igroup .eq. 2 .or. igroup .eq. 3) .and. 
     &      id(i,1) .gt. 50000) goto 2810
C        if (isky .eq. 1) sky(i,icol) = skyparam(1,i,ISKYFRAME)
        sky(i,icol) = skyparam(1,i,ISKYFRAME)
        if (mag(i,icol) .le. 0. .or. magerr(i,icol) .le. 0) then
  	  OUTMAG = 99.999
	  ERR = 9.999
        ELSE
          ERR=MIN(9.999,1.085736*SQRT(MAGERR(I,ICOL)))
          OUTMAG=MIN(99.999,PSFMAG(1)-1.085736*ALOG(MAG(I,ICOL)))
        END IF
        CHI(I,ICOL)=MIN(999.9,CHI(I,ICOL))
#ifdef __SHARP
        SHARP(I,ICOL)=MIN(99.999,MAX(SHARP(I,ICOL),-99.999))
#else
        SHARP(I,ICOL)=0.
#endif
        WRITE(10+ICOL,321,ERR=903) ID(I,1), XC(I)+DXPOS(I,1), 
     &     YC(I)+DYPOS(I,1), OUTMAG,
     &     ERR, SKY(I,ICOL), FLOAT(NOUT(I)), CHI(I,ICOL), SHARP(I,ICOL)
 2810  CONTINUE
  321  FORMAT (I6, 2F9.2, 3F9.3, F9.0, F9.2, F9.3)

C    Write a blank line
       WRITE (10+ICOL,321,ERR=903)
      END DO

C With WRITE option, output separate file for each input frrame
      IF (WRITE) THEN
        DO IFRAME = 1, NFRAMES
         ICOL = COLORS(IFRAME)
         DO 2910 I=1,NTOTSTR
          IF (ID(I,1) .LE. 0) GOTO 2910
          if ((igroup .eq. 2 .or. igroup .eq. 3) .and. 
     &         id(i,1) .gt. 50000) goto 2910
          IF (ISKY .EQ. 1) 
     &       SKY(I,ICOL)=SKYPARAM(1,I,IFRAME)*
     &           ABS(EXPDATA(IFRAME))*GAIN0/GAIN(IFRAME)/SCALEPHOT(IFRAME)
          if (mag(i,icol) .le. 0. .or. magerr(i,icol) .le. 0) then
  	    OUTMAG = 99.999
	    ERR = 9.999
          ELSE
            ERR=MIN(9.999,1.085736*SQRT(MAGERR(I,ICOL)))
            OUTMAG=MIN(99.999,PSFMAG(IFRAME)-1.085736*ALOG(MAG(I,ICOL)))
          END IF
          CHI(I,ICOL)=MIN(999.9,CHI(I,ICOL))
#ifdef __SHARP
          SHARP(I,ICOL)=MIN(99.999,MAX(SHARP(I,ICOL),-99.999))
#else
          SHARP(I,ICOL)=0.
#endif
          WRITE(10+MAXCOL+IFRAME,321,ERR=903) ID(I,1), XC(I)+DXPOS(I,IFRAME), 
     &     YC(I)+DYPOS(I,IFRAME), 
     &     OUTMAG-2.5*LOG10(ABS(EXPDATA(IFRAME))*GAIN0/GAIN(IFRAME)/SCALEPHOT(IFRAME)), 
     &     ERR, SKY(I,ICOL), FLOAT(NOUT(I)), CHI(I,ICOL), SHARP(I,ICOL)
 2910    CONTINUE

C    Write a blank line
         WRITE (10+MAXCOL+IFRAME,321,ERR=903)
        END DO
      END IF

C    Write out positional shift information for all groups larger than
C      MINDPOS stars
      DO 5001 I=1,NTOTSTR
        IF (ID(I,1) .GT. 0 .AND. -GROUP(I) .GE. MINDPOS) THEN
          DO IFRAME=1,NFRAMES
            ICOL = COLORS(IFRAME)
            IF (MAG(I,ICOL) .LE. 0. .OR. MAGERR(I,ICOL) .LE. 0) THEN
              ERR = 9.999
            ELSE
              ERR=MIN(9.999,1.085736*SQRT(MAGERR(I,ICOL)))
            END IF
            IF (ERR .LT. ERRMAX .AND. CHI(I,ICOL) .LT. CHIMAX) THEN
              XOUT(IFRAME) = DXPOS(I,IFRAME)
              YOUT(IFRAME) = DYPOS(I,IFRAME)
            ELSE
              XOUT(IFRAME) = 9999.99
              YOUT(IFRAME) = 9999.99
            END IF
          END DO
C          WRITE(10,21) XC(I),YC(I),
C     &       (DXPOS(I,IFRAME),DYPOS(I,IFRAME),IFRAME=1,NFRAMES)
          WRITE(10,21) XC(I),YC(I),
     &       (XOUT(IFRAME),YOUT(IFRAME),IFRAME=1,NFRAMES)
21        FORMAT(52F8.2)
          GOTO 5001
        END IF
5001  CONTINUE
      do iframe=1,nframes
        write(9,*) iframe, scalephot(iframe)
      end do
      GO TO 2000

C-----------------------------------------------------------------------

C Normal return.

 9000 CONTINUE
      WRITE (6,690,ERR=902) BELL
  690 FORMAT (/'    Done.  ', A1/)
 9010 CONTINUE
 9999 IF (IPSFMODE .LE. 0) THEN
      do 9011 iframe = 1, nframes
	call ccfree(nbytes(iframe)*nall(iframe)*nsamp,locpsf(iframe))
        call clfile(10+maxcol+iframe)
 9011 continue
      do 9012 icol=1,ncolor
        call clfile(50+icol)
        call clfile(10+icol)
	if (havemerge) then
          call clfile(10+maxcol+maxfrm+icol)
          call clfile(10+2*maxcol+maxfrm+icol)
          call clfile(10+3*maxcol+maxfrm+icol)
        end if
 9012 continue
      CALL CLFILE(10)
      CALL CLFILE(9)
        
      END IF
      RETURN

C-----------------------------------------------------------------------

C Irrecoverable errors.

 9200 WRITE (6,692,ERR=902) BELL
  692 FORMAT (/' Not a group file.', A1/)
      goto 9999

 9300 WRITE (6,693,ERR=902) BELL
  693 FORMAT (/' Error opening file.', A1/)
      goto 9999

575   PRINT *, 'Error reading PSFLIB file ', iframe
      GOTO 9999

901   PRINT *, 'Error with internal write '
      GOTO 9999
902   PRINT *, 'Error with write to terminal (unit 6) '
      GOTO 9999
903   PRINT *, 'Error with write to file (unit 1)'
      GOTO 9999

      END

C  Function to determine number of fitting terms given various options
      FUNCTION GETTERM(NSTR,NCOLOR,NFRAMES,ISKY,LOCKPOS,LOCKDPOS,RESOLVE,
     &         NSTARTERM,NSPAR)
	
      LOGICAL LOCKDPOS, LOCKPOS, RESOLVE
      INTEGER SKYTERM

      NSPAR = NCOLOR
      IF (.NOT. LOCKPOS) NSPAR = NSPAR+2
      IF (RESOLVE) NSPAR = NSPAR+1
      GETTERM = NSTR*NSPAR
      NSTARTERM = GETTERM
      IF (ISKY .GE. 1) GETTERM = GETTERM + SKYTERM(ISKY)*NFRAMES
      IF (.NOT. LOCKDPOS) GETTERM = GETTERM + 2*(NFRAMES-1)

      RETURN
      END

C  Subroutine to reset all clamp values for a given star
      SUBROUTINE RESETCLAMP(ISTR,XCCLAMP,YCCLAMP,MAGCLAMP,
     &      DXPOSCLAMP,DYPOSCLAMP,SKYCLAMP,MAXSTR,MAXCOL,MAXFRM,MAXSKY)

      REAL*4 XCCLAMP(MAXSTR,2), YCCLAMP(MAXSTR,2)
      REAL*4 MAGCLAMP(MAXSTR,MAXCOL,2)
      REAL*4 DXPOSCLAMP(MAXSTR,MAXFRM,2),DYPOSCLAMP(MAXSTR,MAXFRM,2)
      REAL*4 SKYCLAMP(MAXSKY,MAXSTR,MAXFRM,2)

      XCCLAMP(ISTR,1) = 1.0
      YCCLAMP(ISTR,1) = 1.0
      XCCLAMP(ISTR,2) = 0.0
      YCCLAMP(ISTR,2) = 0.0
      DO 6615 ICOL = 1, MAXCOL
        MAGCLAMP(ISTR,ICOL,1) = 1.0
        MAGCLAMP(ISTR,ICOL,2) = 0.0
6615  CONTINUE
      DO 6613 IFRM = 1, MAXFRM
        DXPOSCLAMP(ISTR,IFRM,1) = 1.0
        DYPOSCLAMP(ISTR,IFRM,1) = 1.0
        DXPOSCLAMP(ISTR,IFRM,2) = 0.0
        DYPOSCLAMP(ISTR,IFRM,2) = 0.0
        DO 6612 I=1,MAXSKY
          SKYCLAMP(I,ISTR,IFRM,1) = 1.0
          SKYCLAMP(I,ISTR,IFRM,2) = 0.0
6612    CONTINUE
6613  CONTINUE
       
      RETURN
      END

C  Function to compare whether a group has changed
      LOGICAL FUNCTION SAMEGROUP(GROUP,GROUPOLD,NTOT,IGRP,ITMP,IND,MAXSTR)
      INTEGER*4 GROUP(NTOT), GROUPOLD(NTOT)
      INTEGER ITMP(MAXSTR)
      REAL IND(MAXSTR,2)

      SAMEGROUP = .FALSE.

C  Load up the ID numbers of all the stars in the current group
      I1 = 0
      DO I = 1, NTOT
        IF (GROUP(I) .EQ. IGRP) THEN
          I1 = I1 + 1 
          IND(I1,1) = I
        END IF
      END DO

C  IOLD gives the old group number of the first star in the new group
      IOLD = GROUPOLD(INT(IND(1,1)))
      IF (IOLD .EQ. 0) RETURN

C  Now find all of the stars in this old group
      I2 = 0
      DO I = 1, NTOT
        IF (GROUPOLD(I) .EQ. IOLD) THEN
          I2 = I2 + 1 
          IND(I2,2) = I
        END IF
      END DO

C  If we don't have the same number of stars as before, it's not the same group
      IF (I1 .NE. I2) THEN
        RETURN
      ELSE
C  If we do have the same number, sort the ID numbers and see if they''re
C    really the same stars
        CALL QUICK(IND(1,1),I1,ITMP)
        CALL QUICK(IND(1,2),I2,ITMP)
        DO I = 1, I1
          IF (NINT(IND(I,1)) .NE. NINT(IND(I,2))) RETURN
        END DO
      END IF

      SAMEGROUP = .TRUE.

      RETURN
      END 

C  Subroutine to dynamically group stars with critical separation CRIT

      SUBROUTINE GROUPIT(ID,X,Y,GROUP,NTOT,NGRP,NGROUP,DONE,IGROUP,CRIT,
     &                   IND,MAXSTR)

      REAL*4 X(NTOT), Y(NTOT), CRIT
      INTEGER*4 GROUP(NTOT), NGROUP(NTOT)
      INTEGER IGROUP, ID(NTOT), IND(MAXSTR)
      LOGICAL*1 DONE(NTOT)

      DO I = 1, NTOT
        IF (.NOT. DONE(I)) GROUP(I) = 0
      END DO

      IGRP = 0
C  Loop over every star
      DO I = 1, NTOT
C      If it''s valid and not in a group already, start a new group
        IF (ID(I) .GT. 0 .AND. GROUP(I) .EQ. 0 .AND. .NOT. DONE(I)) THEN
          IGRP = IGRP + 1
          GROUP(I) = IGRP
          N = 1
          NOLD = 0
          IND(N) = I

C  This next stuff is a horribly slow way to go but it''s simple. Loop
C     over the list of stars looking for neighbors of anyone in the group.
C     We keep increasing the group as we go. However, we don''t go backwards
C     in the list to check for new neighbors of new members. So keep
C     repeating the pass through the list until the group size doesn''t
C     change
101       CONTINUE
          IF (N .NE. NOLD) THEN
            NOLD = N
            DO J = 1, NTOT
              IF (ID(J) .GT. 0 .AND. GROUP(J) .EQ. 0 .AND. .NOT. DONE(I)) THEN
C    Compute distance from each member of the current group
                DO K = 1, N
                  XD = (X(IND(K))-X(J))**2
                  IF (XD .LT. CRIT) THEN
                    YD = (Y(IND(K))-Y(J))**2
                    IF (XD+YD .LT. CRIT) THEN
C    If we''re within critical distance, add this star to the group
                      N = N + 1
                      IND(N) = J
                      GROUP(J) = IGRP
                      GOTO 201
                    END IF
                  END IF
                END DO
201             CONTINUE

              END IF
            END DO
            GOTO 101
          END IF
          NGROUP(IGRP) = N

        END IF
      END DO

      NGRP = IGRP
     
      RETURN
      END 

#ifdef PSFMODE10
      subroutine filllib(psf,fft,smcent,smadj,nsamp,wj,nwx,nwy,npsflib)

      real*4 psf(npsflib,npsflib)

      do iy=1,npsflib
        dy = iy-npsflib/2
        do ix=1,npsflib
          dx = ix-npsflib/2
          psf(ix,iy) = sumpsf(fft,ndim,dx,dy,smcent,smadj,nsamp,wj,nwx,nwy)
        end do 
      end do
   
      return
      end 
#endif

	subroutine getdpos(xc,yc,dxpos,dypos,nstr,headbuf)

	implicit none
	integer i, nstr, maxpar, j, npar
	real xc(nstr), yc(nstr), dxpos(nstr), dypos(nstr)
	real*8 ax, bx, cx, ay, by, cy
#ifdef __64BITADDRESS
        INTEGER*8 HEADBUF
#else
        INTEGER HEADBUF
#endif
	character fitscard*8
        PARAMETER (MAXPAR = 20)
        real*8 DPAR(MAXPAR,2), PAR(MAXPAR/2,2), XNEW, YNEW

#ifdef OLD
        call ccfhead('REF_AX',headbuf,ax)
        call ccfhead('REF_BX',headbuf,bx)
        call ccfhead('REF_CX',headbuf,cx)
        call ccfhead('REF_AY',headbuf,ay)
        call ccfhead('REF_BY',headbuf,by)
        call ccfhead('REF_CY',headbuf,cy)
        do i=1,nstr
            dxpos(i) = -1 * (ax*xc(i) + bx*yc(i) + cx)
            dypos(i) = -1 * (ay*xc(i) + by*yc(i) + cy)
        end do
#else
        DO J=1,10
            WRITE(FITSCARD,111) J
111         FORMAT('X(',i2.2,',1)')
            CALL CCFHEAD(FITSCARD,HEADBUF,PAR(J,1))
            WRITE(FITSCARD,112) J
112         FORMAT('X(',i2.2,',2)')
            CALL CCFHEAD(FITSCARD,HEADBUF,PAR(J,2))
        END DO
        DO I=1,NSTR
            XNEW = XC(I)
            YNEW = YC(I)
            CALL GETXFUNC(XNEW,YNEW,DPAR,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)
            DXPOS(I) = 0.
            DYPOS(I) = 0.
            DO J=1,MAXPAR/2
              DXPOS(I) = DXPOS(I) + PAR(J,1)*DPAR(J,1)
              DYPOS(I) = DYPOS(I) + PAR(J,2)*DPAR(J+NPAR/2,2)
            END DO
        END DO
#endif
	return
	end

        subroutine aperturesky(a,nx,ny,xc,yc,ntotstr,rin,rout,HAVE3sig,
     &                         lowbad,highbad,skyparam,maxsky,maxstr)

	real a(nx,ny)
	real skyparam(0:maxsky,maxstr)
	real xc(ntotstr), yc(ntotstr)
	real lowbad, highbad
	parameter (maxskypix = 50000, minsky=20)
	real sky(maxskypix)
        logical HAVE3sig
        integer index(maxskypix)
	common /work/ sky, index

	rinsq = rin*rin
	routsq = rout*rout
	do i=1, ntotstr
          iymin = int(yc(i)-rout-1)
          iymax = int(yc(i)+rout+1)
          ixmin = int(xc(i)-rout-1)
          ixmax = int(xc(i)+rout+1)
          iymin = max(1,iymin)
          iymax = min(ny,iymax)
          ixmin = max(1,ixmin)
          ixmax = min(nx,ixmax)
	  nsky = 0
          do iy=iymin,iymax
            ydist = (iy-yc(i))**2
            do ix=ixmin,ixmax
              dist = ydist + (ix-xc(i))**2
              if (dist .ge. rinsq .and. dist .le. routsq .and.
     &            a(ix,iy) .gt. lowbad .and. a(ix,iy) .lt. highbad 
     &            .and. nsky .lt. maxskypix) then
                nsky = nsky+1
                sky(nsky) = a(ix,iy)
              end if
            end do
          end do

          if (nsky .lt. minsky) then
            skymode = 0
          else 
	    call quick(sky,nsky,index)
            if (HAVE3sig) then
              call mmm3(sky, nsky, highbad, skymode, skysig, skyskew)
            else
              call mmm(sky, nsky, highbad, skymode, skysig, skyskew)
            end if
	  end if
          skyparam(0,i) = skymode
	end do
	return
	end
