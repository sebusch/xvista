#include "Vista.h"
      
      subroutine dsscoord

C
C  DSSCOORD - Convert (C,R) to (RA,Dec) for Digitized Sky Survey images.  
C
C  This program uses the astrometric coefficicent in the decompressed
C  and extracted DSS image FITS headers to compute the Right Ascension
C  and Declination corresponding to a given pixel location within the
C  image.  The polynomial coefficients and conversion procedures are
C  as described in the Volume 1 DSS documentation (section 5).  However,
C  it was found that the formula for deriving the declination from 
C  standard coordinates (xi,eta) as printed in the booklet is wrong.  It
C  should read:
C
C   \delta=\arctan{[(\eta+\tan\delta_c)\cos(\alpha-\alpha_c)] 
C          / [1-\eta\tan\delta_c]}
C
C  where the booklet has \cos\alpha_c instead of \cos(\alpha-\alpha_c).
C
C  Output positions are given in J2000 equatorial coordinates.  The
C  origin convention adopted by the DSS puts the pixel coordinate in
C  the lower lefthand corner of the pixel, not the pixel center, so
C  VISTA pixel coordinates must be offset (+0.5,+0.5).  Note that the
C  intrinsic display "flip" of VISTA (whereby the image origin is in 
C  the upper left instead of lower left) is irrelevant.
C  
C  Command Syntax:
C
C     DSSCOORD imbuf [P=(c,r)] [V=n] [TTY] [PHOT] [STANDARD] [>file] [>>file]
C
C  where:
C     imbuf     buffer holding the DSS image
C     P=(c,r)   Pixel coordinates in the image to be converted
C     V=n       Vista TV cursor variables (Cn,Rn), n=[0:9], with the pixel
C                 coordinates to convert to (RA,Dec)
C     TTY       Enter a keyboard entry loop where the program asks
C                 for pixel coordinates to enter
C     PHOT      Get pixel coordinates from the stellar photometry 
C                 common block (e.g., MARKSTAR)
C     STANDARD  Also print standard coordinates (xi,eta) in arcsec
C
C  output may be redirected into an external ascii file
C
C  Modification History:
C     1994 Jul 15: new program [rwp/osu]
C     1994 Dec 11: added PHOT and STANDARD keywords, and fixed output
C                  format bugs [rwp/osu]
C
C---------------------------------------------------------------------------

C  VISTA include statements

#ifdef VMS
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
      include 'VINCLUDE:customize.inc'
      include 'VINCLUDE:photfiles.inc'
#else   /* VMS */
      include 'vistadisk/source/include/vistalink.inc'
      include 'vistadisk/source/include/imagelink.inc'
      include 'vistadisk/source/include/customize.inc'
      include 'vistadisk/source/include/photfiles.inc'
#endif  /* VMS */

C  Parser Variables

      character*80 parm, tword
      character*8  tstr
      real*4 tmp
      real*4 vtmp(2)
      integer itmp
      integer upper
      logical keycheck

C  Photometry common block working variables

      real*4 phot(NUMDATA)
      character*80 label

C  Internal Working Variables

      integer col, row
      real*8 xpix, ypix
      real*8 xc, yc
      real*8 px, py
      real*8 x, y
      real*8 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13
      real*8 b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13
      real*8 xi, eta
      real*8 pra, prah, pram, pras
      real*8 pdec, pdecd, pdecm, pdecs
      character pdecsn*1
      real*8 dra, ras
      integer rah, ram
      real*8 ddec, decs
      integer decd, decm
      character decsn*1
      logical havecoords, needcoords, usephot, stdcoord

      real*8 xx, yy, xy, rr
      real*8 arg1, arg2

C  Conversion Factors

      real*8 ArcsPerRad
      parameter (ArcsPerRad = 2.062648063D+05)

      real*8 DegPerRad
      parameter (DegPerRad = 5.729577951D+01)

      real*8 TwoPI
      parameter (TwoPI = 6.283185308D+00)

C  Yow!

      call keyinit
      call keydef('V=')
      call keydef('P=')
      call keydef('TTY')
      call keydef('PHOT')
      call keydef('STANDARD')
      if (.not. keycheck()) then
         xerr = .true.
         return
      end if

C  Make sure we''ve been given an image on the command line

      if (nints .eq. 0) then
         print *,'No image specified on the command line'
         xerr = .true.
         return
      end if

      imbuf = ibuf(1)
      if (.not. buff(imbuf)) then
         print '(a,i2,a)','Image buffer ',imbuf,' is empty!'
         xerr = .true.
         return
      end if

C  Parse the keywords list

      havecoords = .false.
      needcoords = .false.
      usephot = .false.
      stdcoord = .false.

      do 8701 i = 1, NCON
         tword = word(i)
         n = upper(tword)
         if (tword(1:2) .eq. 'V=') then
            call assign(tword,tmp,parm)
            if (xerr) return
            n = nint(tmp)
            if (n .lt. 0 .or. n .gt. 9) then
               print *,'V=n keyword restricted to n=0-9 inclusive'
               xerr = .true.
               return
            end if
            tstr = 'C'
            write(tstr(2:2),'(i1)') n
            call variable(tstr,tmp,.false.)
            col = nint(tmp)
            tstr = 'R'
            write(tstr(2:2),'(i1)') n
            call variable(tstr,tmp,.false.)
            row = nint(tmp)
            xpix = dble(col)
            ypix = dble(row)
            havecoords = .true.
            needcoords = .false.

         else if (tword(1:2) .eq. 'P=') then
            call assignv(tword,2,vtmp,n,parm)
            if (xerr) return
            if (n .ne. 2) then
               print *,'P=(c,r) requires 2 arguments'
               xerr = .true.
               return
            end if
            xpix = dble(vtmp(1))
            ypix = dble(vtmp(2))
            havecoords = .true.
            needcoords = .false.

         else if (tword .eq. 'PHOT') then
            needcoords = .false.
            havecoords = .true.
            usephot = .true.

         else if (tword .eq. 'STANDARD') then
            stdcoord = .true.

         else if (tword .eq. 'TTY') then
            needcoords = .true.

         end if

 8701 continue

C  Check for incompatible keywords:

      if (havecoords .and. needcoords) then
         print *,'Cannot use TTY keyword with either V= or P='
         xerr = .true.
         return
      end if

C  If using the photometry common data, make sure there is a
C  photometry record connected

      if (usephot) then
         if (nstars .eq. 0) then
            print *,'No photometry file is currently active...'
            xerr = .true.
            return
         end if
      end if

C  Get the pixel scale and center keywords from the image header.  These
C  are the bellwether cards.  If absent, the image did not come from the
C  DSS.

      px = 0.d0
      py = 0.d0
      call ccfhead('PPO3',headbuf(1,imbuf),xc)
      call ccfhead('PPO6',headbuf(1,imbuf),yc)
      call ccfhead('XPIXELSZ',headbuf(1,imbuf),px)
      call ccfhead('YPIXELSZ',headbuf(1,imbuf),py)

      if (px .eq. 0.d0) then 
         print *,'Cannot find XPIXELSZ header keyword...'
         print *,'Is the image from the DSS?'
         xerr = .true.
         return
      else if (py .eq. 0.d0) then
         print *,'Cannot find YPIXELSZ header keyword...'
         print *,'Is the image from the DSS?'
         xerr = .true.
         return
      end if

C  Get the xi coordinate plate coefficients

      call ccfhead('AMDX1',headbuf(1,imbuf),a1)
      call ccfhead('AMDX2',headbuf(1,imbuf),a2)
      call ccfhead('AMDX3',headbuf(1,imbuf),a3)
      call ccfhead('AMDX4',headbuf(1,imbuf),a4)
      call ccfhead('AMDX5',headbuf(1,imbuf),a5)
      call ccfhead('AMDX6',headbuf(1,imbuf),a6)
      call ccfhead('AMDX7',headbuf(1,imbuf),a7)
      call ccfhead('AMDX8',headbuf(1,imbuf),a8)
      call ccfhead('AMDX9',headbuf(1,imbuf),a9)
      call ccfhead('AMDX10',headbuf(1,imbuf),a10)
      call ccfhead('AMDX11',headbuf(1,imbuf),a11)
      call ccfhead('AMDX12',headbuf(1,imbuf),a12)
      call ccfhead('AMDX13',headbuf(1,imbuf),a13)

C  Get the eta coordinate plate coefficients

      call ccfhead('AMDY1',headbuf(1,imbuf),b1)
      call ccfhead('AMDY2',headbuf(1,imbuf),b2)
      call ccfhead('AMDY3',headbuf(1,imbuf),b3)
      call ccfhead('AMDY4',headbuf(1,imbuf),b4)
      call ccfhead('AMDY5',headbuf(1,imbuf),b5)
      call ccfhead('AMDY6',headbuf(1,imbuf),b6)
      call ccfhead('AMDY7',headbuf(1,imbuf),b7)
      call ccfhead('AMDY8',headbuf(1,imbuf),b8)
      call ccfhead('AMDY9',headbuf(1,imbuf),b9)
      call ccfhead('AMDY10',headbuf(1,imbuf),b10)
      call ccfhead('AMDY11',headbuf(1,imbuf),b11)
      call ccfhead('AMDY12',headbuf(1,imbuf),b12)
      call ccfhead('AMDY13',headbuf(1,imbuf),b13)

C  Get the plate center in J2000 RA and Dec, converting to decimal radians

      call ccinhead('PLTRAH',headbuf(1,imbuf),itmp)
      prah = dble(itmp)
      call ccinhead('PLTRAM',headbuf(1,imbuf),itmp)
      pram = dble(itmp)
      call ccfhead('PLTRAS',headbuf(1,imbuf),pras)

      pra = 15.d0*(prah + (pram + (pras/60.d0))/60.d0)
      pra = pra/DegPerRad

      call ccinhead('PLTDECD',headbuf(1,imbuf),itmp)
      pdecd = dble(itmp)
      call ccinhead('PLTDECM',headbuf(1,imbuf),itmp)
      pdecm = dble(itmp)
      call ccfhead('PLTDECS',headbuf(1,imbuf),pdecs)
      call ccchead('PLTDECSN',headbuf(1,imbuf),pdecsn)

      pdec = pdecd + (pdecm + (pdecs/60.d0))/60.d0
      if (pdecsn .eq. '-') pdec = -pdec
      pdec = pdec/DegPerRad

C  If no pixel coordinates were given, prompt the user for them on the
C  tty

      istar = 0
 777  if (needcoords) then
         call askdata('Please enter pixel coords (Col,Row):',vtmp,2)
         xpix = dble(vtmp(1))
         ypix = dble(vtmp(2))
      else if (usephot) then
         istar = istar + 1
         if (istar .gt. nstars) return
         call getphotrec(label,phot,istar)
         xpix = dble(phot(colloc))
         ypix = dble(phot(rowloc))
         if (istar .eq. 1) then
            if (stdcoord) then
               write(olu, 201)
201            format(3x,'X(pix)',5x,'Y(pix)',4x, 
     &              'RA(2000)',5x,'Dec(2000)',7x,'Xi',8x,'Eta')
            else
               write(olu,202)
202            format(3x,'X(pix)',5x,'Y(pix)',4x,'RA(2000)',5x,'Dec(2000)')
            end if
         end if
      end if

C  Apply the offset to put the pixel coords in DSS convention for the
C  pixel origin

      xpix = xpix + 0.5d0
      ypix = ypix + 0.5d0

C  Convert pixel coordinates (XPIX,YPIX) to millimeters from plate center

      x = (xc - px*xpix)/1000.d0
      y = (py*ypix - yc)/1000.d0

C  Convert (x,y) to standard coordinates (xi,eta) in arcseconds
      
      xx = x*x
      yy = y*y
      xy = x*y
      rr = xx + yy
      
      xi = a1*x + a2*y + a3 + a4*xx + a5*xy + a6*yy
     &          + a7*rr + a8*x*xx + a9*xx*y + a10*x*yy
     &          + a11*y*yy + a12*x*rr + a13*x*rr*rr

      eta = b1*y + b2*x + b3 + b4*yy + b5*xy + b6*xx
     &           + b7*rr + b8*y*yy + b9*x*yy + b10*xx*y
     &           + b11*x*xx + b12*y*rr + b13*y*rr*rr

C  Convert (xi,eta) in arcseconds to radians

      xi = xi/ArcsPerRad
      eta = eta/ArcsPerRad

C  Compute (RA,Dec) corresponding to (X,Y) in radians

      arg1 = 1.d0 - eta*dtan(pdec)
      arg2 = xi/dcos(pdec)
      
      dra = pra + datan2(arg2,arg1)
      if (dra .lt. 0.d0) dra = dra + TwoPI

      arg2 = dcos(pra-dra)*(eta + dtan(pdec))
      
      ddec = datan2(arg2,arg1)

C  Conver RA to hours and break into (h,m,s) parts

      dra = DegPerRad*dra/15.d0
      rah = dint(dra)
      ram = dint(60.d0*(dra-dble(rah)))
      ras = 3600.d0*(dra-(dble(rah)+(dble(ram)/60.d0)))

C  Convert Dec to degrees and break into (d,arcmin,arcsec) parts

      ddec = DegPerRad*ddec
      decsn = '+'
      if (ddec .lt. 0.d0) decsn = '-'
      ddec = dabs(ddec)
      decd = dint(ddec)
      decm = dint(60.d0*(ddec-dble(decd)))
      decs = 3600.d0*(ddec-(dble(decd)+(dble(decm)/60.d0)))

C  Print it:
      
      if (.not. usephot) then
         if (stdcoord) then
            write(olu,203)
203         format(3x,'X(pix)',5x,'Y(pix)',4x,
     &          'RA(2000)',5x,'Dec(2000)',7x,'Xi',8x,'Eta')
         else
            write(olu,204)
204         format(3x,'X(pix)',5x,'Y(pix)',4x,
     &           'RA(2000)',5x,'Dec(2000)')
         end if
      end if
      if (stdcoord) then
         xi = xi*ArcsPerRad
         eta = eta*ArcsPerRad
         xpix = xpix - 0.5d0
         ypix = ypix - 0.5d0
         write(olu,667) xpix,ypix,rah,ram,ras,decsn,decd,decm,decs,
     &                  xi,eta
      else
         xpix = xpix - 0.5d0
         ypix = ypix - 0.5d0
         write(olu,666) xpix,ypix,rah,ram,ras,decsn,decd,decm,decs
      end if

 666  format(1x,f9.2,2x,f9.2,2x,i2.2,':',i2.2,':',
     &       f5.2,2x,a1,i2.2,':',i2.2,':',f4.1)

 667  format(1x,f9.2,2x,f9.2,2x,i2.2,':',i2.2,':',
     &       f5.2,2x,a1,i2.2,':',i2.2,':',f4.1,2(2x,f9.2))

C  Do it again?

      if (needcoords) then
         write (*,'(1x,''Do More <y|n> ? '',$)') 
         read (*,'(a)',iostat=ierr) tstr
         n = upper(tstr)
         if (tstr(1:1) .eq. 'Y') go to 777
      else if (usephot) then
         go to 777
      end if

      return
      end
