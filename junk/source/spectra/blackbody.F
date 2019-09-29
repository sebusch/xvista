#include "Vista.h"

      SUBROUTINE BLACKBODY

C
C  BLACKBODY - Make a blackbody spectrum
C
C  Creates a black-body spectrum (linearized, f-lambda units) of
C  a given temperature.  
C  If the buffer already contains a spectrum, BB replaces it, using the 
C  wavelength scale of the spectrum.
C  If the buffer is empty, it needs the starting wavelength, dispersion,
C  and number of pixels in addition to the temperature.
C
C  Command Syntax:
C
C     BLACKBODY spbuf T=temp [LAM0=lambda] [DISP=disp] [NPIX=n]
C
C  where:
C
C     spbuf        Image buffer containing spectrum
C     T=temp       Blackbody temperature in degrees Kelvin
C     LAM0=lambda  Wavelength of the first pixel in angstrom
C     DISP=disp    Linear dispersion in angstroms/pixel
C     NPIX=n       Number of pixels in the final spectrum
C
C  Author:
C     R.W. Pogge
C     OSU Astronomy Dept.
C     1995 August 10
C
C  Modification History:
C
C---------------------------------------------------------------------------

C  Include files.

#ifdef VMS
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C  Other declarations.

      real*4 T
      real*4 disp, lam0
      integer npix
      integer imbuf

      real*4 arg

      LOGICAL KEYCHECK, MAKESPEC
      LOGICAL HAVETEMP, HAVEDISP, HAVELAM0, HAVENPIX

      CHARACTER PARAM*80
      character object*64

C  Yow!

      imbuf = IBUF(1)

C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('T=')
      CALL KEYDEF('LAM0=')
      CALL KEYDEF('DISP=')
      CALL KEYDEF('NPIX=')

C  Exit if we encounter keywords we don''t know about.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Some defaults.  These are to mark that the data are missing

      havetemp = .false.
      havedisp = .false.
      havelam0 = .false.
      havenpix = .false.
      makespec = .true.

C  Check the image buffer status
C    If the buffer is attached, check to see if it contains a
C       linearized spectrum, otherwise terminate immediately
C    If the buffer is empty, we will need to get memory for it, 
C       requiring us to pick the LAM0=, DISP=, and NPIX= keywords off
C       the command line.

      if (BUFF(IMBUF)) then
         if (.not. ISSPECTRUM(IMBUF)) then
            print *, 'Buffer does not contain a SPECTRUM'
            xerr = .true.
            return
         end if
         if (LAMTYPE(IMBUF) .ne. LAMBDALINEAR) then
            print *,'Spectrum does not have a linear wavelength scale'
            xerr = .true.
            return
         end if
         isploc = IMLOC(IMBUF)
C  get the dispersion and lam0 from the header.  These should be in units 
C  of meters, not angstroms.  But it *could* be in Angstroms.         
         disp = sngl(CDELT1(IMBUF))
         if (log10(disp) .lt. -5.0) disp=disp*1.0E10
         lam0 = sngl(CRVAL1(IMBUF))
         if (log10(lam0) .lt. -3.0) lam0 = lam0*1.0E10
         npix = ICOORD(NNCOL,IMBUF)
         ispcol = ICOORD(IXSC,IMBUF)
         iepcol = ICOORD(IXEC,IMBUF)
         isprow = ICOORD(IYSR,IMBUF)
         ieprow = ICOORD(IYER,IMBUF)
         makespec = .false.
         havedisp = .true.
         havelam0 = .true.
         havenpix = .true.
      else
         makespec = .true.
      end if

C  Process the command line

      do 8701 i = 1, NCON
         IF (WORD(I)(1:2) .EQ. 'T=' ) THEN
            call assign (WORD(i), T, param)
            IF (T .le. 0.0) then
               print *,'Temperature must be >0'
               xerr = .true.
               return
            end if
            havetemp = .true.

         ELSE IF (makespec .and. WORD(I)(1:5) .EQ. 'DISP=') THEN
            call assign (WORD(i), arg, param)
            disp = arg
            if (disp .eq. 0.0) then
               print *,'Dispersion must be nonzero'
               xerr = .true.
               return
            end if
            havedisp = .true.

         ELSE IF (makespec .and. WORD(I)(1:5) .EQ. 'LAM0=') THEN
            call assign (WORD(i), arg, param)
            lam0 = arg
            havelam0 = .true.

         ELSE IF (makespec .and. WORD(I)(1:5) .EQ. 'NPIX=') THEN
            call assign (WORD(i), arg, param)
            npix = nint(arg)
            if (npix .le. 0) then
               print *,'Number of pixels must be >0'
               xerr = .true.
               return
            end if
            havenpix = .true.

         END IF

 8701 continue

C  Is anything missing?  Prompt for it

      if (.not. havetemp) then
 10      write(*,'(1x,''Blackbody Temperature (K): '',$)')
         read(*,*,err=10) T
         if (NOGO) return
         if (T .le. 0.0) then
            print *,'Temperature must be >0'
            go to 10
         end if
         havetemp = .true.
      end if

      if (makespec) then
         if (.not. havelam0) then
 11         write(*,'(1x,''Starting Wavelength (Angstroms): '',$)')
            read(*,*,err=11) lam0
            if (NOGO) return
            if (lam0 .le. 0.0) then
               print *,'Starting Wavelength must be >0'
               go to 11
            end if
            havelam0 = .true.
         end if

         if (.not. havedisp) then
 12         write(*,'(1x,''Linear Dispersion (Ang/pixel): '',$)')
            read(*,*,err=12) disp
            if (NOGO) return
            if (disp .eq. 0.0) then
               print *,'Dispersion must be nonzero'
               go to 12
            end if
            havedisp = .true.
         end if

         if (.not. havenpix) then
 13         write(*,'(1x,''Number of Pixels: '',$)')
            read(*,*,err=13) npix
            if (NOGO) return
            if (npix .le. 0) then
               print *,'Number of pixels must be nonzero'
               go to 13
            end if
            havenpix = .true.
         end if
      end if

C  If we need to make the spectrum, do it now

      if (makespec) then
         TEMPHEAD = ' '
         TEMPHEAD(1:80) = 'SIMPLE  =                    T'
         TEMPHEAD(81: ) = 'END '
         call INHEADSET('BITPIX',16,TEMPHEAD)
         call INHEADSET('NAXIS',2,TEMPHEAD)
         call INHEADSET('NAXIS1',npix,TEMPHEAD)
         call INHEADSET('NAXIS2',   1,TEMPHEAD)
         call FHEADSET('CRVAL1',DBLE(LAM0),TEMPHEAD)
         call FHEADSET('CDELT1',DBLE(DISP),TEMPHEAD)
         call INHEADSET('CRPIX1',1,TEMPHEAD)
         call CHEADSET('CTYPE1','LAMBDA',TEMPHEAD)
         call FHEADSET('CRVAL2',0.0D0,TEMPHEAD)
         call FHEADSET('CDELT2',1.0D0,TEMPHEAD)
         call INHEADSET('CRPIX2',1,TEMPHEAD)
         call CHEADSET('STATUS','SPECTRUM',TEMPHEAD)

         call CREATEIM(isploc,isprow,ieprow,ispcol,iepcol,
     &                 1,TEMPHEAD,.TRUE.)
         if (XERR) then
            print *,'Error creating spectrum buffer'
            return
         end if
      end if

C  Now that we have the stuff, do it to it.  The BB function is
C  loaded by the BBSPEC subroutine, which in turn is wrapped in
C  a C-routine to handle VM

      call ccbbspec(isploc,npix,T,lam0,disp,ierr)
      if (ierr .ne. 0) then
         print *,'Error computing blackbody curve'
         XERR = .true.
         return
      end if

C  Change the header

      write(object,'(''Blackbody, T='',i6,'' K'')') int(T)
      call CCCHEADSET('OBJECT',object,HEADBUF(1,IMBUF))

C  All done!

      return
      end

C---------------------------------------------------------------------------

      subroutine bbspec(rvec,npix,T,lam0,disp,ierr)
  
C
C  Load a vector with a blackbody spectrum for T=0 in F-lambda units
C
C  Arguments:
C     rvec (i/o, r*4): vector to hold the spectrum
C     npix (input, i*4): number of pixels
C     T (input, r*4): blackbody temperature in degees K
C     lam0 (input, r*4): starting wavelength in Angstroms
C     disp (input, r*4): linear dispersion in Angstrom/pixel
C     ierr (output, i*4): error code.  0 on success, non-zero if failure
C
C  Computes:
C
C     \pi B_\lambda = (C1/\lambda^5)*(1/[exp(C2/(lambda*T)) -1])
C
C  in units of ergs/sec/cm^2/Angstrom
C
C  where:
C     lambda is in centimeters
C     T is in kelvins
C
C     C1 = 2\pi h c^2 = 3.74185E-15 erg cm^3 s^-1 Angstroms^-1
C     C2 = hc/k = 1.43883 cm K
C
C  R. Pogge
C  OSU Astronomy Dept
C  1995 August 10
C
C  Modification History:
C
C---------------------------------------------------------------------------

C  External Variables

      real*4 rvec(npix)
      real*4 T
      real*4 lam0
      real*4 disp
      integer*4 ierr

C  Internal Variables & Parameters

C  Radiation constants:
C
C     C1 = 2*pi*h*c^2 = 3.74185x10^-5 erg cm^2 s^-1  (lambda in cm)
C     C2 = hc/k = 1.43883 cm K  (lambda in cm)
C

      real*4 C1, C2
      parameter (C1 = 3.74185E-5)
      parameter (C2 = 1.43883)

C  This number converts Angstroms to centimeters

      real*4 CMperANG
      parameter (CMperANG = 1.0E-8)

C  Working Variables

      integer*4 ipix
      real*4 lambda
      real*4 arg1
      real*4 arg2
      real*4 piBlambda

C  Yow!

      do 100 ipix = 1, npix
         lambda = CMperANG*(lam0 + disp*floaT(ipix-1))
         if (lambda .le. 0.0) then
            print *,'ERROR: (bbspec) lambda<=0'
            ierr = -1
            return
         end if
         arg1 = C1/(lambda**5)
         arg2 = C2/(lambda*T)
         piBlambda = arg1/(exp(arg2)-1.0)
         rvec(ipix) = piBlambda*CMperANG
 100  continue
      ierr = 0
      return
      end


