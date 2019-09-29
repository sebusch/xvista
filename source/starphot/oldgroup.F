#include "Vista.h"

#define __MAXGROUP 60

C  Called by driver subroutine in group.F

      SUBROUTINE DOOLDGROUP(GROUPMEMBER,X,Y,MAG,SKY,ID,ISIZE,
     &               NUMBER,REMOVED,TAKE,MAXGROUP,MAXSTR)
C
C=======================================================================
C
C This subroutine accepts a file containing stellar coordinates, and
C associates the stars into natural groups based on a critical
C separation:  stars within one critical separation of each other are
C put into the same group; no star is within one critical separation of
C any star outside its group.  Groups are written out into the disk file
C in order of increasing size.  (This is done to minimize the length of
C time the array processor must be attached to the program in NSTAR).
C
C             OFFICIAL DAO VERSION:  1985 August 15
C
C======================================================================
C
C      PARAMETER  (MAXPSF=145)
      PARAMETER  (MAXPSF=301)
C
C Parameter
C
C MAXSTR is the largest number of stars that may be contained in a data
C        file.
C
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH
      CHARACTER*132 FORMSTR, NAME, LINE3
      CHARACTER*4 PLSTR, PLGRP
      REAL*4 X(MAXSTR+1), Y(MAXSTR+1), MAG(MAXSTR+1), SKY(MAXSTR+1), ERR(MAXSTR+1)
      INTEGER*4 ID(MAXSTR+1), GETNPAR
      INTEGER*2 ISIZE(MAXSTR), NUMBER(MAXSTR)
      LOGICAL TAKE(MAXGROUP),REMOVED(MAXSTR)
      INTEGER GROUPMEMBER(MAXGROUP,MAXSTR)
      REAL ALLVAL(10), ALLDVDX(10), ALLDVDY(10), WPSF(10)
      integer x0psf, y0psf, dxpsf, dypsf, nlib(2)
      real psfval(0:1,0:1), psfdvdx(0:1,0:1), psfdvdy(0:1,0:1)
      character card*80

      REAL*4 LOWBAD, GAUSS(50), PSF(MAXPSF,MAXPSF), STARS(30)
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      LOGICAL NOFILES
      COMMON /DAOASK/ NOFILES

      COMMON /WORK/ PSF

      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)
      REAL*8 FHEAD
      INTEGER CLOSEC

#ifdef VMS
      INCLUDE 'VINCLUDE:customize.inc'
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      LOGICAL KEYCHECK, HAVECRIT, HAVEGAIN, HAVERN
      CHARACTER PARM*8

      CALL GETOPT(OPT)
      FITRAD = OPT(2)
      PSFRAD = OPT(3)

      CALL KEYINIT
      CALL KEYDEF('FI=')
      CALL KEYDEF('PS=')
      CALL KEYDEF('CRIT=')
      CALL KEYDEF('STARS=')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('SCALE=')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      HAVECRIT = .FALSE.
      HAVEGAIN = .FALSE.
      HAVERN = .FALSE.
      NSTARS = -1
      SCALE = 1.
      DO 4701 I = 1, NCON
         IF (WORD(I)(1:3) .EQ. 'FI=') THEN
            CALL ASSIGN(WORD(I),FITRAD,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'PS=') THEN
            CALL ASSIGN(WORD(I),PSFRAD,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:5) .EQ. 'CRIT=') THEN
            CALL ASSIGN(WORD(I),CRIT,PARM)
            IF (XERR) RETURN
            HAVECRIT = .TRUE.
            IF (CRIT .LT. 0) CRIT = 2*PSFRAD
         ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
            CALL ASSIGN(WORD(I),GAIN,PARM)
            IF (XERR) RETURN
            HAVEGAIN = .TRUE.
         ELSE IF (WORD(I)(1:3) .EQ. 'RN=') THEN
            CALL ASSIGN(WORD(I),RN,PARM)
            IF (XERR) RETURN
            HAVERN = .TRUE.
         ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
            CALL ASSIGN(WORD(I),SCALE,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:6) .EQ. 'STARS=') THEN
            CALL ASSIGNV(WORD(I),30,STARS,NSTARS,PARM)
            IF (XERR) RETURN
         END IF
 4701 CONTINUE

C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get set up.
C
C Open input photometry file.
C
      CALL TBLANK
C Type a blank line
      CALL ASKFILE ('File with photometry:', MAGFILE)
      IF (MAGFILE .EQ. 'END OF FILE') RETURN
      call filedef(magfile,name,daodir,'.ap')
      CALL INFILE (2, NAME, IFLAG)
C   Error opening file ?
      IF (IFLAG .LT. 0)GO TO 9400
      REWIND(2)
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)

      IF (HAVEGAIN) PHPADU = GAIN
      IF (HAVERN) READNS = RN / PHPADU

      IF (NL .LE. 0) NL=1

C  If IGROUP = 3 then we need to get the PSF and the critical overlap value.
C  Otherwise, just get the critical separation.

      IF (IGROUP .EQ. 3) THEN
        
        IF (NOFILES) THEN
           PSFFILE=SWITCH(MAGFILE, '.psf')
        END IF
        CALL ASKFILE('File with the PSF:', PSFFILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN
        
        IF (IPSFMODE .LE. 0) THEN
          call filedef(psffile,name,psfdir,'.lib')
          ifile = -1
	  call rdfitshead(temphead,name,ifile,ierr,.true.)
	  if (ierr .ne. 0) goto 9400
	  npsflib = inhead('NAXIS1',temphead)
	  nlib(1) = inhead('NLIB',temphead)
	  nside = npsflib / nlib(1)
	  psftot = sngl(fhead('PSFTOT',temphead))
          nall = 1
          nsamp = 1

          if (ipsfmode .lt. 0) then
            if (ipsfmode .eq. -1) then
             print *, 'WARNING: oldgroup needs to be properly programmed for IP=-1'
             xerr = .TRUE.
             RETURN
            end if
            nall = inhead('NAXIS3',temphead)
            x0psf = inhead('X0',temphead)
            y0psf = inhead('Y0',temphead)
            if (nlib(1) .le. 0 .or. psftot .le. 0) goto 9400
            if (ipsfmode .lt. -1) then
              if (abs(sqrt(float(nall))-nint(sqrt(float(nall))))
     &            .gt. 1.e-5) then
                print *, 'ERROR: not a PSF input grid'
                xerr = .true.
                return
              end if
              sqnall = nint(sqrt(float(nall)))
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
                  nlib(i) = inhead(card,temphead)
                end do
              end if
            end if
          end if
          nbytes = 4*npsflib*npsflib

	  call ccalloc(nbytes*nall*nsamp,location)
	  call ccrdfits(location,npsflib*nall*nsamp,npsflib,ifile,temphead,ierr)
	  if (ierr .ne. 0) goto 575
#ifdef VMS
	  close(ifile)
#else
	  ierr = closec(ifile)
#endif
          psfmag = -2.5*alog10(psftot) + 25
          npar = 10
          npsf = maxpsf
        ELSE
          CALL INFILE(3, PSFFILE, IFLAG)
          IF (IFLAG .LT. 0) GOTO 9400
          NPAR = GETNPAR(IPSFMODE)
          FORMSTR = ' '
          WRITE(FORMSTR,210) NPAR-1
  210     FORMAT ('(2I3, 2F10.3,',I12.12,'(F10.6), 2F7.2)')
          READ (3,FORMSTR) NPSF, NPSF, PSFMAG, (GAUSS(ii),ii=1,npar),
     &        XPSF, YPSF
          READ (3,211) ((PSF(I,J), I=1,NPSF), J=1,NPSF)
  211     FORMAT (10E13.6)
          CALL CLFILE (3)
        END IF

        IF (.NOT. HAVECRIT) THEN
        CALL ASKDATA ('Critical overlap:',CRIT,1)
        IF (CRIT .LT. -1.e36) GOTO 9090
        END IF
        CRIT = CRIT**2

      ELSE
C Obtain numerical value for critical separation.
        IF (.NOT. HAVECRIT) THEN
        CALL ASKDATA ('Critical separation:', CRIT, 1)
        IF (CRIT .LT. -1.E36) GO TO 9090
        END IF
        RSQ=CRIT**2
      END IF
C
C Open output group file.
C
      IF (NOFILES) THEN
      GRPFILE=SWITCH(MAGFILE, '.grp')
      END IF
      CALL ASKFILE ('File for stellar groups:', GRPFILE)
C  CTRL-Z was entered ?
      IF (GRPFILE .EQ. 'END OF FILE') GO TO 9090
      CALL OUTFILE (3, GRPFILE)
      CALL WRHEAD (3, 3, NCOL, NROW, 9, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      READNS = READNS**2
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Associate stars into natural groupings.
C
C Read in and count the stars.
C
      I=0
      BRTMAG = 1.E38
 2010 I=I+1
 2020 CALL RDSTAR (2, NL, ID(I), X(I), Y(I), MAG(I), ERR(I), SKY(I))
      IF (ID(I) .LT. 0) GO TO 2030
C         End-of-file encountered
      IF (ID(I) .EQ. 0) GO TO 2020
C         Blank line encountered
      IF (IGROUP .GE. 2 .AND. ID(I) .GT. 50000) GOTO 2020
C         For IGROUP=2, don''t regroup "dummy" stars
      IF (I .GT. MAXSTR) GO TO 9500
C         Too many stars in file
      ISIZE(I)=0
C         Initialize ISIZE
      NUMBER(I)=0
C         Initialize NUMBER
      REMOVED(I) = .FALSE.
      IF (MAG(I) .LT. BRTMAG) BRTMAG = MAG(I)
      GO TO 2010
C
 2030 NTOT=I-1
      IF (NTOT .LE. 0) GOTO 9089

C Stars will be set to BRTMAG if they''ve been measured as 99.999.
      IF (BRTMAG .LT. 90.) THEN
        BRTMAG = 10.**(0.4*(PSFMAG-BRTMAG))
      ELSE
        BRTMAG = 1.
      END IF
C Here we'll set BRTMAG to 100 in the assumption that we won't get 99.999
C   as a result of saturated pixels
      BRTMAG = 100.
C
C All the stars have now been read in.
C NTOT is the number of stars in the file, which is also the
C theoretical maximum number of groups that can be found.
C
C If option IGROUP >= 2, then we wish to split stars into an individual
C  group for each star, with only its nearest neighbors in the group.
C  Here we will reduce each star more than once, but only in small groups.
C  We make two passes through the stars. The first time, we look for
C  neighbors within RSQ. Stopping here, however, would be wasteful, since in 
C  general, some of the groups made this way will be identical too, or
C  subsets of, some other groups. To get around this, we check to see if each
C  groupmember has any neighbors besides the ones in the current group. If it
C  doesn''t, we can remove the group that has this star first. This removal is
C  done in the second pass through the data.

      IF (IGROUP .GE. 2) THEN

        IF (IGROUP .EQ. 2) THEN
          RRR = SQRT(RSQ)
        ELSE
          FR = FITRAD + 1
          FRSQ = FR**2
          CRITSEP=MIN(PSFRAD,0.5*(0.5*FLOAT(NPSF-7)-1.0) ) + FR
          CRITSQ = CRITSEP**2
          RRR = CRITSEP
          RSQ = CRITSQ
        END IF

C  Find neighbors of all stars and load ids of these into the array GROUPMEMBER.
C  GROUPMEMBER(I,J) is a 2-D array where J refers to a given star and 
C    I runs from 1 to GROUPMEMBER(MAXGROUP,J). GROUPMEMBER(I,J) gives
C    the index numbers of the neighbors of star J. GROUPMEMBER(I+MAXSTAR/2,J)
C    contains a flag value of 0 or 1, indicating whether the neighbor I has
C    neighbors which are a subset of the the neighbors of star J.

C  Loop over all of the stars
        DO 5701 I = 1, NTOT
          NGROUP = 1
          GROUPMEMBER(NGROUP,I) = I
          GROUPMEMBER(NGROUP+MAXGROUP/2,I) = 0
          DO 5702 J = 1, NTOT
            IF (I .NE. J) THEN
              XD = (X(J) - X(I))
              IF (ABS(XD) .LT. RRR) THEN
                YD = (Y(J) - Y(I))
                IF (ABS(YD) .LT. RRR) THEN
                  DIST = XD**2 + YD**2
                  IF (DIST .LT. RSQ) THEN
C  Star is within critical separation. For igroup=2, this means we take it.
C  For igroup=3, we take it if its in a fitting radius, else we must check
C  the PSF contribution of the new star at one fitting radius.
                    IF (IGROUP .EQ. 3 .AND. DIST .GT. FRSQ) THEN
                      IF (MAG(J) .GT. 90.) THEN
                        QVAL = BRTMAG
                      ELSE
                        QVAL = 10.**(0.4*(PSFMAG-MAG(J))) * SCALE
                      END IF
                      RATIO = (SQRT(DIST) - FR) / SQRT(DIST)
                      XD = RATIO * XD
                      YD = RATIO * YD
                      IF (IPSFMODE .LE. 0) THEN
			IXPIX = INT(X(J) - XD)
			IYPIX = INT(Y(J) - YD)
                        if (ipsfmode .eq. 0) then
			  call cclibget(location,npsflib,nlib,ixpix,iypix,
     &                        x(j),y(j),aval,dvdx,dvdy)
                        else if (ipsfmode .eq. -1) then
                          call ccliball(location,npsflib,nlib,ixpix,iypix,
     &                        x(j),y(j),
     &                       allval, alldvdx, alldvdy, nall, nbytes)
                          call gpcoords(y(j),x(j),xxx,yyy)
                          call gpsfval(xxx,yyy,allval,aval)
                        else
                          ixpsf = int((x(j)-x0psf)/dxpsf)
                          iypsf = int((y(j)-y0psf)/dypsf)
                          if (ixpsf .lt. 0 .or. iypsf .lt. 0 .or.
     &                        ixpsf+1 .ge. sqrt(float(nall)) .or.
     &                        iypsf+1 .ge. sqrt(float(nall))) then
                             print *, 'PSF out of interpolated bound!!!'
                             pause
                          end if
                          xxx = (x(j) - (x0psf+ixpsf*dxpsf))/dxpsf
                          yyy = (y(j) - (y0psf+iypsf*dypsf))/dypsf

                          call cclibnew(location,npsflib,nlib,ixpix,iypix,
     &                            x(j),y(j),
     &                            psfval,psfdvdx,psfdvdy,
     &                            nall,nbytes,nsamp,ixpsf,iypsf,nskip,0,ierr)
                          aval = bin(xxx+1,yyy+1,psfval,2,2,.false.)
                        end if

                      ELSE
                        AVAL=VALUE(-XD,-YD,GAUSS, PSF, NPSF, DVDX, DVDY)
                      END IF
                      QVAL = QVAL * AVAL
                      ERRTOT = READNS + 0.5*(SKY(I)+SKY(J))/PHPADU
C	print *, x(j), y(j), ixpix, iypix
C	print *, mag(j), qval, qval**2, crit*errtot
C	print *, readns, sky(i), sky(j), phpadu
C	pause
                      IF (QVAL**2 .LT. CRIT*ERRTOT) GOTO 5702
                    END IF
                    NGROUP = NGROUP + 1
                    IF (NGROUP .GT. (MAXGROUP-1)/2) THEN
                       PRINT *, 'Can''t handle such large groups.'
                       PRINT *, 'Try again with smaller separation.'
                       GOTO 9089
                    END IF
                    GROUPMEMBER(NGROUP,I) = J
                    GROUPMEMBER(NGROUP+MAXGROUP/2,I) = 0
                  END IF
                END IF
              END IF
            END IF
 5702     CONTINUE  
          GROUPMEMBER(MAXGROUP,I) = NGROUP
 5701   CONTINUE

C  Now we have NTOT groups, with the neighbors of each star on the list.
C  Loop over each group. For each member, look for a neighbor who isn''t 
C  already in the group. If we find
C  one, quit. If we don''t find one, then mark this star off as already
C  solved by this group, not necessary to redo. If we''ve already done
C  the star in question, that mark it off for deletion and remember to
C  include all the stars in the old group that were originally marked
C  as primary stars.

C  For each group, the temporary array TAKE will be used to store whether
C    each star in the group should be included as a primary star.
        DO 5703 I = 1, NTOT
            IF (REMOVED(I)) GOTO 5703
            TAKE(1) = .TRUE.
            DO 2701 IG = 2, GROUPMEMBER(MAXGROUP,I)
              TAKE(IG) = .FALSE.
 2701       CONTINUE
            DO 5704 IG = 2, GROUPMEMBER(MAXGROUP,I)
              IF (REMOVED(GROUPMEMBER(IG,I))) GOTO 5704

C              DO 5705 J = 1, NTOT
              DO 5705 JJ = 1, GROUPMEMBER(MAXGROUP,GROUPMEMBER(IG,I))
                J = GROUPMEMBER(JJ,GROUPMEMBER(IG,I))

C  Don''t count the already previously known neighbors as new ones
                DO 5706 III = 1, GROUPMEMBER(MAXGROUP,I)
                  IF ( J .EQ. GROUPMEMBER(III,I) ) GOTO 5705
 5706           CONTINUE

C  Check whether the star is a new neighbor. If so, then this group member
C  won''t be taken.
C                XD = ABS(X(GROUPMEMBER(IG,I)) - X(J)) 
C                IF (XD .LT. RRR) THEN
C                  YD = ABS(Y(GROUPMEMBER(IG,I)) - Y(J))
C                  IF (YD .LT. RRR) THEN
C                    DIST = XD**2 + YD**2
C                    IF (DIST .LT. RSQ) THEN
CC  Check for IGROUP = 3 option
C                       IF (IGROUP .EQ. 3 .AND. DIST .GT. FRSQ) THEN
C                         IF (MAG(J) .GT. 90.) THEN
C                           QVAL = BRTMAG
C                         ELSE
C                           QVAL = 10.**(0.4*(PSFMAG-MAG(J)))
C                         END IF
C                         RATIO = (SQRT(DIST) - FR) / SQRT(DIST)
C                         XD = RATIO * XD
C                         YD = RATIO * YD
C                         IF (IPSFMODE .LE. 0) THEN
C                           IY = INT(Y(J) - YD)
C                           IX = INT(X(J) - XD)
C                           IYPIX = IY-INT(Y(J)) + NSIDE/2 + 1
C                           IXPIX = IX-INT(X(J)) + NSIDE/2 + 1
C                           CALL DPSFLIB(LIBDATA,NSIDE,MAXLIB,1,
C     .                       X(J),Y(J),IXPIX,IYPIX,AVAL,DVDX,DVDY)
C                         ELSE
C                       AVAL=VALUE(-XD,-YD,GAUSS, PSF, NPSF, DVDX, DVDY)
C                         END IF
C                         QVAL = QVAL * AVAL
C                         ERRTOT = READNS + 0.5*(SKY(I)+SKY(J))/PHPADU
C                         IF (QVAL**2 .LT. CRIT*ERRTOT) GOTO 5705
C                       END IF
                       TAKE(IG) = .FALSE.
                       GOTO 5704
C                    END IF
C                  END IF
C                END IF

 5705         CONTINUE
C  If we are here, than this star is to be included. Mark it off as
C   removed, and take its neighbors which have be marked as primary stars.
              TAKE(IG) = .TRUE.
              REMOVED(GROUPMEMBER(IG,I)) = .TRUE.
C  Now take the taken neighbors of this star.
              DO 5707 III = 1, GROUPMEMBER(MAXGROUP,GROUPMEMBER(IG,I))
                IF (GROUPMEMBER(III+MAXGROUP/2,GROUPMEMBER(IG,I)) 
     &                                       .EQ. 1) THEN
                  DO 5708 JJJ = 1, GROUPMEMBER(MAXGROUP,I)
                    IF (GROUPMEMBER(III,GROUPMEMBER(IG,I)) .EQ.
     .                GROUPMEMBER(JJJ,I)) TAKE(JJJ) = .TRUE.
 5708             CONTINUE
                END IF
 5707         CONTINUE
               
 5704       CONTINUE 
          
C  Now store which stars in the current group are to be taken as primary
C    stars by setting GROUPMEMBER(IG+MAXGROUP/2,I) to 0 or 1.
            DO 5709 IG = 1, GROUPMEMBER(MAXGROUP,I)
              IF (TAKE(IG)) THEN
                 GROUPMEMBER(IG+MAXGROUP/2,I) = 1
              ELSE
                 GROUPMEMBER(IG+MAXGROUP/2,I) = 0
              END IF
 5709       CONTINUE
        
 5703   CONTINUE

C  Now loop throught the groups a final time, and output only those not
C    marked for removal. In the output groups, add 50000 to the ID numbers
C    of the stars not to be taken as primary stars. This will be recognized by
C    NSTAR, which will not output magnitudes for these stars.
        DO 5710 I = 1, NTOT
          IF (NSTARS .GT. 0) THEN
            DO 8701 J = 1, NSTARS
              IF (I .EQ. NINT(STARS(J))) GOTO 8702
8701        CONTINUE
            GOTO 5710
          END IF
8702      CONTINUE
            IF (REMOVED(I)) GOTO 5710
            DO 5711 IG = 1, GROUPMEMBER(MAXGROUP,I)
              IF (GROUPMEMBER(IG+MAXGROUP/2,I) .EQ. 0) THEN
                IDOUT = ID(GROUPMEMBER(IG,I)) + 50000
              ELSE
                IDOUT = ID(GROUPMEMBER(IG,I)) 
              END IF
              WRITE(3,340) IDOUT,
     &         X(GROUPMEMBER(IG,I)),Y(GROUPMEMBER(IG,I)),
     &         MAG(GROUPMEMBER(IG,I)),SKY(GROUPMEMBER(IG,I)) 
 5711       CONTINUE
            WRITE(3,*)
 5710   CONTINUE

        GOTO 9089
      END IF

C Back to the standard IGROUP option.

C Now search the star list for stars lying within a critical radius of
C each other.  Initialize the counters.  The variable N will count the
C number of stars in the current group.
C
      ITEST=0
      ITOP=2
      N=1
      IFIRST=1
      MAXGRP=0
C
C The stars are currently in a stack NTOT stars long.  The variable
C IFIRST points to the first star in the current group; this starts out,
C of course, with the value 1.  ITEST will point to the position in the
C stack occupied by the star which is currently the center of a circle
C of the critical radius, within which we are looking for other stars;
C this also starts out with a value of 1.  ITOP points to the top
C position in the stack of the stars which have not yet been assigned
C to groups; this starts out with the value 2.  Each time through, the
C program goes down through the stack from ITOP to NTOT and looks for
C stars within the critical distance from the star at stack position
C ITEST.  When such a star is found, it changes places in the stack
C with the star at ITOP and ITOP is incremented by one.  When the
C search has gotten to the last position in the stack (NTOT), the
C pointer ITEST is incremented by one, and the search proceeds again
C from the new value of ITOP to NTOT.  If the pointer ITEST catches up
C with the pointer ITOP, that means that the group currently being
C built up is complete.  The number of stars in the newly-created
C group (the first star of which is at stack position IFIRST) is stored
C in array element ISIZE(IFIRST).  Then a new group is started
C beginning with the star at the current position ITEST, ( = ITOP for
C the moment), ITOP is incremented by 1, and the next group is built
C up as before.
C
 2100 ITEST=ITEST+1
      IF (ITEST .LT. ITOP) GO TO 2110
C
C ITEST has reached ITOP; no other unassigned stars are within a
C critical separation of any member of the current group.  The group is
C therefore complete. Store N in ISIZE(IFIRST). Then start a new group
C with the star currently at ITEST ( = the old value of ITOP), and then
C increment the value of ITOP by one.
C
      ISIZE(IFIRST)=N
      IF (N .GT. MAXGRP) MAXGRP=N
      NUMBER(N)=NUMBER(N)+1
C
C The array NUMBER(i) builds up a histogram giving the number of
C groups of size i as a function of i.
C
      N=1
C                                   ! Re-initialize star counter
      IFIRST=ITEST
C
C If ITEST equals NTOT at this point, then we are finished (the last
C group contains one star).  Otherwise, set ITOP to the first star
C below ITEST and on with the search.
C
      IF (ITEST .GE. NTOT) GO TO 4000
      ITOP=ITOP+1
C                                     ! Increment ITOP
C
 2110 CONTINUE
C
C Now go through the list of unassigned stars, occupying positions ITOP
C through NTOT in the stack, to look for stars within the critical
C distance of the star at position ITEST in the stack.  If one is found,
C move it up to stack position ITOP and increment ITOP by one.
C
      XTEST=X(ITEST)
      YTEST=Y(ITEST)
      J=ITOP
      DO 2120 I=J,NTOT
      IF ((X(I)-XTEST)**2+(Y(I)-YTEST)**2 .GT. RSQ) GO TO 2120
C
C This star is within the critical distance of the star at stack
C position ITEST.  Therefore it should be added to the current group.
C
      N=N+1
C
C Now move this star up to position ITOP in the stack, where the pointer
C ITEST may eventually reach it.
C
      CALL SWAP (I, ITOP, ID, X, Y, MAG, SKY)
C
C Now increment ITOP by 1 to point at the topmost unassigned star in the
C stack.
C
      ITOP=ITOP+1
 2120 CONTINUE
C
C If ITOP is greater than NTOT, then all stars have been assigned to
C groups, and we are finished.  Otherwise, increment ITEST by 1 and
C keep going.
C
      IF (ITOP .LE. NTOT) GO TO 2100
C
C-----------------------------------------------------------------------
C
C Normal completion.
C
C ITOP has exceeded NTOT.  That means that all the stars in the input
C file have been assigned to groups and have been written into the
C output file.  Now we may write out the groups, in order of
C increasing size, and close up shop.
C
 4000 ISIZE(IFIRST)=N
      NUMBER(N)=NUMBER(N)+1
      IF (N .GT. MAXGRP) MAXGRP=N
C
C Beginning of loop over group size.
C
      WRITE (6,640)
  640 FORMAT (/' Size of   Number of'/
     .         '  group     groups'/)
      NGRP=0
      DO 4040 I=1,MAXGRP
      IF (NUMBER(I) .LE. 0) GO TO 4040
C
C Now loop over actual groups, writing out any group whose size is
C equal to the current value of I.
C
      IFIRST=1
 4010 IF (ISIZE(IFIRST) .NE. I) GO TO 4030
      DO 4020 J=IFIRST,IFIRST+I-1
 4020 WRITE (3,340) ID(J), X(J), Y(J), MAG(J), SKY(J), ERR(J)
  340 FORMAT (I6, 2F9.2, 3F9.3)
      NGRP=NGRP+1
      WRITE (3,340)
C                                 ! Write a blank line
 4030 IFIRST=IFIRST+ISIZE(IFIRST)
C
C End of loop over actual groups.
C
      IF (IFIRST .LE. NTOT) GO TO 4010
      WRITE (6,641) I, NUMBER(I)
  641 FORMAT (I6, I10)
C
C End of loop over group size.
C
 4040 CONTINUE
C
C Type out the number of stars and the number of groups NEATLY.
C
      PLSTR='s in'
      IF (NTOT .EQ. 1) PLSTR=' in '
      PLGRP='s.  '
      IF (NGRP .EQ. 1) PLGRP='.   '
      LENGRP=INT(ALOG10(NGRP+0.5))+2
      IF (NTOT .EQ. 1) LENGRP=LENGRP-1
      WRITE(FORMSTR,643) LENGRP
  643 FORMAT ('(/I6, '' star'', A4, I',I12.12,', '' group'', A4/)')
      WRITE (6,FORMSTR) NTOT, PLSTR, NGRP, PLGRP
 9089 CALL CLFILE (3)
 9090 CALL CLFILE (2)

      if (igroup .eq. 3 .and. 
     &    ipsfmode .le. 0) call ccfree(nbytes*nall,location)

      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable errors.
C
C Error occurred opening input file.
C
 9400 WRITE (6,694) BELL
  694 FORMAT (/' Error opening file.', A1/)
      RETURN
C
 9500 WRITE (6,695) BELL, MAXSTR
  695 FORMAT (/' More than ', A1, I6, ' stars in input file.'/)
      RETURN
  575 PRINT *, 'Error reading from library file'
      GOTO 9089
C
      END
