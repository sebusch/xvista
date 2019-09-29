#include "Vista.h"

#define __MAXGROUP 60

      SUBROUTINE GROUP

      PARAMETER(MAXGROUP=__MAXGROUP, MAXSTR=__MAXSTR)
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)
#ifdef __64BITADDRESS
      INTEGER*8 ID, X, Y, MAG, SKY, ISIZE, NUMBER, GROUPMEMBER, REMOVED, TAKE
#else
      INTEGER ID, X, Y, MAG, SKY, ISIZE, NUMBER, GROUPMEMBER, REMOVED, TAKE
#endif
     
      CALL GETOPT(OPT)

      IF (COM .EQ. 'OLDGROUP' .OR. NINT(OPT(21)) .GT. 0) THEN 

        CALL CCALLOC(4*MAXGROUP*MAXSTR,GROUPMEMBER)
        CALL CCALLOC(4*MAXSTR,X)
        CALL CCALLOC(4*MAXSTR,Y)
        CALL CCALLOC(4*MAXSTR,MAG)
        CALL CCALLOC(4*MAXSTR,SKY)
        CALL CCALLOC(4*MAXSTR,ID)
        CALL CCALLOC(2*MAXSTR,ISIZE)
        CALL CCALLOC(2*MAXSTR,NUMBER)
        CALL CCALLOC(4*MAXSTR,REMOVED)
        CALL CCALLOC(4*MAXGROUP,TAKE)

        CALL CCDOOLDGROUP(GROUPMEMBER,X,Y,MAG,SKY,ID,ISIZE,
     &               NUMBER,REMOVED,TAKE,MAXGROUP,MAXSTR)

        CALL CCFREE(4*MAXGROUP*MAXSTR,GROUPMEMBER)
        CALL CCFREE(4*MAXSTR,X)
        CALL CCFREE(4*MAXSTR,Y)
        CALL CCFREE(4*MAXSTR,MAG)
        CALL CCFREE(4*MAXSTR,SKY)
        CALL CCFREE(4*MAXSTR,ID)
        CALL CCFREE(2*MAXSTR,ISIZE)
        CALL CCFREE(2*MAXSTR,NUMBER)
        CALL CCFREE(4*MAXSTR,REMOVED)
        CALL CCFREE(4*MAXGROUP,TAKE)

      ELSE
        CALL CCALLOC(4*MAXSTR,ID)
        CALL CCALLOC(4*MAXSTR,X)
        CALL CCALLOC(4*MAXSTR,Y)
        CALL CCALLOC(4*MAXSTR,MAG)
        CALL CCALLOC(4*MAXSTR,SKY)
        CALL CCALLOC(2*MAXSTR,ISIZE)
        CALL CCALLOC(2*MAXSTR,NUMBER)

        CALL CCDOGROUP(ID,X,Y,MAG,SKY,ISIZE,NUMBER,MAXSTR)

        CALL CCFREE(4*MAXSTR,ID)
        CALL CCFREE(4*MAXSTR,X)
        CALL CCFREE(4*MAXSTR,Y)
        CALL CCFREE(4*MAXSTR,MAG)
        CALL CCFREE(4*MAXSTR,SKY)
        CALL CCFREE(2*MAXSTR,ISIZE)
        CALL CCFREE(2*MAXSTR,NUMBER)

      END IF
  
      RETURN
      END

      SUBROUTINE  DOGROUP (ID, X, Y, MAG, SKY, ISIZE, NUMBER, MAXSTR0)
C
C=======================================================================
C
C This subroutine accepts a file containing stellar coordinates, and
C associates the stars into natural groups based on the magnitude level
C at which they overlap:  if two stars are within a distance equal to
C one PSF radius plus one fitting radius plus one pixel of each other,
C the PSF of the brighter is evaluated at a point one fitting radius
C plus one pixel away from the fainter.  If this value is larger than
C some user-defined fraction of the anticipated noise per pixel, the
C two stars are put into the same group.
C
C Groups are written out into the disk file in order of increasing
C size.  (This is done to minimize the length of time the array
C processor must be attached to the program in NSTAR).
C
C             OFFICIAL DAO VERSION:  1986 August 11
C
C======================================================================
C
C      PARAMETER  (MAXSTR=7999, MAXPSF=145)
      PARAMETER  (MAXSTR = __MAXSTR, MAXPSF=301)
C
C Parameter
C
C MAXSTR is the largest number of stars that may be contained in a data
C        file.
C
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH, LINE3
      CHARACTER*4 PLSTR, PLGRP
      CHARACTER*80 FORMSTR, NAME
      REAL*4 X(MAXSTR+1), Y(MAXSTR+1), MAG(MAXSTR+1), SKY(MAXSTR+1)
      REAL*4 PSF(MAXPSF,MAXPSF), GAUSS(50)
      INTEGER*4 ID(MAXSTR+1), INDEX(MAXSTR+1), GETNPAR
      INTEGER*2 ISIZE(MAXSTR), NUMBER(MAXSTR)
      REAL*4 LOWBAD, MTEST, MVAL
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      LOGICAL NOFILES
      COMMON /DAOASK/ NOFILES
      COMMON /WORK/ INDEX
      INTEGER CLOSEC
      REAL*8  FHEAD

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif
      COMMON /WORK/ PSF
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)
      LOGICAL KEYCHECK, HAVECRIT, HAVEPSF
      CHARACTER*8 PARM

      CALL GETOPT(OPT)
      FITRAD = OPT(2)
      PSFRAD = OPT(3)
      WATCH = OPT(4)

      CALL KEYINIT
      CALL KEYDEF('FI=')
      CALL KEYDEF('PS=')
      CALL KEYDEF('WA=')
      CALL KEYDEF('CRIT=')
      CALL KEYDEF('PSF')

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

      HAVECRIT = .FALSE.
      HAVEPSF = .FALSE.
      DO 7401 I=1,NCON
        IF (WORD(I)(1:3) .EQ. 'FI=') THEN
          CALL ASSIGN(WORD(I),FITRAD,PARM)
          IF (XERR) RETURN

        ELSE IF (WORD(I)(1:3) .EQ. 'PS=') THEN
          CALL ASSIGN(WORD(I),PSFRAD,PARM)
          IF (XERR) RETURN

        ELSE IF (WORD(I)(1:3) .EQ. 'WA=') THEN
          CALL ASSIGN(WORD(I),WATCH,PARM)
          IF (XERR) RETURN

        ELSE IF (WORD(I)(1:5) .EQ. 'CRIT=') THEN
          CALL ASSIGN(WORD(I),CRIT,PARM)
          IF (XERR) RETURN
          HAVECRIT = .TRUE.

        ELSE IF (WORD(I)(1:3) .EQ. 'PSF') THEN
          HAVEPSF = .TRUE.
        END IF
 7401 CONTINUE

C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get set up.
C
C Open input photometry file.
C
C    Type a blank line
      CALL TBLANK
      CALL ASKFILE ('File with photometry:', MAGFILE)
C    CTRL-Z was entered?
      IF (MAGFILE .EQ. 'END OF FILE') RETURN
      CALL INFILE (2, MAGFILE, IFLAG)
C    Error opening file?
      IF (IFLAG .LT. 0) GO TO 9410
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      IF (NL .LE. 0) NL=1
C
C Read the point-spread function into memory.
C
        IF (NOFILES) THEN
          PSFFILE=SWITCH(MAGFILE, '.psf')
        END IF

        CALL ASKFILE('File with the PSF:', PSFFILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN
        
        IF (IPSFMODE .LE. 0) THEN
          call filedef(psffile,name,psfdir,'.lib')
#ifdef __NEWLIB
          ifile = -1
	  call rdfitshead(temphead,name,ifile,ierr,.true.)
	  if (ierr .ne. 0) goto 9400
	  npsflib = inhead('NAXIS1',temphead)
	  nlib = inhead('NLIB',temphead)
	  nside = npsflib / nlib
	  psftot = sngl(fhead('PSFTOT',temphead))

	  call ccalloc(4*npsflib*npsflib,location)
	  call ccrdfits(location,npsflib,npsflib,ifile,temphead,ierr)
	  if (ierr .ne. 0) goto 575
#ifdef VMS
	  close(ifile)
#else
	  ierr = closec(ifile)
#endif

#else
          open(unit=21,file=name,status='old',form='unformatted',
     .         iostat=iflag)
          if (iflag .ne. 0) goto 9400
	  read(21,err=575) psftot
          read(21,err=575) libindex
          read(21,err=575) libdata
          close(21)
#endif
          psfmag = -2.5*alog10(psftot) + 25
          npar = 10
          npsf = maxpsf
        ELSE
          CALL INFILE(3, PSFFILE, IFLAG)
          IF (IFLAG .LT. 0) GOTO 9400
          NPAR = GETNPAR(IPSFMODE)
          FORMSTR = ' '
          WRITE(FORMSTR,310) NPAR-1
  310     FORMAT ('(2I3, 2F10.3,',I12.12,'(F10.6), 2F7.2)')
          READ (3,*) NPSF, NPSF, PSFMAG, (GAUSS(ii),ii=1,npar),
     &        XPSF, YPSF
          READ (3,311) ((PSF(I,J), I=1,NPSF), J=1,NPSF)
  311     FORMAT (10E13.6)
 1010     CALL CLFILE (3)
          CALL SMTHPSF (PSF, NPSF)
        END IF

C
C Compute the effective radius of the point-spread function, and
C add one fitting radius plus one pixel.
C
      FR=FITRAD+1.
      FRSQ=FR**2
C If looking for PSF stars check for neighbors within 2*PSFRAD.
C If doing normal grouping, if neighbor is farther than PSFRAD+FITRAD, its
C  not in the same group.
      IF (HAVEPSF) THEN
        CRITSEP=2 * MIN(PSFRAD, 0.5*(0.5*FLOAT(NPSF-7)-1.0) ) 
      ELSE
        CRITSEP=MIN(PSFRAD, 0.5*(0.5*FLOAT(NPSF-7)-1.0) ) + FR
      END IF
      CRITSQ=CRITSEP**2
C
C Obtain the critical overlap.
C
      IF (.NOT. HAVECRIT) THEN
      CALL ASKDATA ('Critical overlap:', CRIT, 1)
      IF (CRIT .LE. -1.E38) GO TO 9090
      END IF
      IF (.NOT. HAVEPSF) CRIT=CRIT**2
C
C Open output group file.
C
      IF (NOFILES) THEN
      GRPFILE=SWITCH(MAGFILE, '.grp')
      END IF

      CALL ASKFILE ('File for stellar groups:', GRPFILE)
      IF (GRPFILE .EQ. 'END OF FILE') GO TO 9090
C                                           ! CTRL-Z was entered
      CALL OUTFILE (3, GRPFILE)
      CALL WRHEAD (3, 3, NCOL, NROW, 9, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      READNS=READNS**2
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
      BRTMAG=1.E38
 2010 I=I+1
 2020 CALL RDSTAR (2, NL, ID(I), X(I), Y(I), MAG(I), DUM, SKY(I))
      IF (ID(I) .LT. 0) GO TO 2030
C                End-of-file encountered
      IF (ID(I) .EQ. 0) GO TO 2020
C                Blank line encountered
      IF (I .GT. MAXSTR) GO TO 9500
C                Too many stars in file
C    Initialize ISIZE
      ISIZE(I)=0
C    Initialize NUMBER
      NUMBER(I)=0
      IF (MAG(I) .LT. BRTMAG) BRTMAG=MAG(I)
      GO TO 2010
C
 2030 NTOT=I-1
      IF (NTOT .LE. 0) GO TO 9080
C
C All the stars have now been read in.
C NTOT is the number of stars in the file, which is also the
C theoretical maximum number of groups that can be found.
C
      CALL QUICK (Y, NTOT, INDEX)
C
C The vector of stellar y-values is now in increasing order.  ID, X,
C MAG, and SKY are still in the order in which they were read in.
C The array INDEX tells how they are to be cross-matched:  The i-th
C star in increasing y-order has ID(INDEX(i)), X(INDEX(i)), Y(i),
C MAG(INDEX(i)), and SKY(INDEX(i)).
C
      IF (BRTMAG .LT. 90.) THEN
         BRTMAG=10.**(0.4*(PSFMAG-BRTMAG))
      ELSE
         BRTMAG=1.
      END IF
C
C BRTMAG is the apparent magnitude of the brightest star in the
C input file.  (If all stars have apparent magnitudes greater than
C 90.0-- i.e. the aperture photometry blew up on all stars, BRTMAG
C is equal to the apparent magnitude of the point-spread function.)
C
C Now search the star list for stars lying within a critical distance of
C each other.  Stars are considered in order of increasing y-coordinate.
C
C Initialize the counters.  The variable N will count the
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
C    Re-initialize star counter
      N=1
      IFIRST=ITEST
C
C If ITEST equals NTOT at this point, then we are finished (the last
C group contains one star).  Otherwise, set ITOP to the first star
C below ITEST and on with the search.
C
      IF (ITEST .GE. NTOT) GO TO 4000
C    Increment ITOP
      ITOP=ITOP+1
      JTOP=INDEX(ITOP)
C
 2110 JTEST=INDEX(ITEST)
      XTEST=X(JTEST)
      YTEST=Y(ITEST)
C
      IF (MAG(JTEST) .GT. 90.) THEN
         MTEST=BRTMAG
      ELSE
         MTEST=10.**(0.4*(PSFMAG-MAG(JTEST)))
      END IF
C
      STEST=SKY(JTEST)
C
C Now go through the list of unassigned stars, occupying positions ITOP
C through NTOT in the stack, to look for stars within one critical
C distance of the star at position ITEST in the stack.  If one is found,
C compute the point-spread function of the brighter at a point one
C fitting radius plus one pixel from the fainter one along the line
C connecting them.  If this is larger than the input critical fraction
C of the total noise per pixel, move the new star up to stack position
C ITOP and increment ITOP by one.  Note:  if any star has an apparent
C magnitude greater than 90.0 (which means that the aperture
C photometry bombed), the brightest reasonable magnitude (= BRTMAG)
C will be assumed.
C
      II=ITOP
      DO 2120 I=II,NTOT
      DY=Y(I)-YTEST
      IF (DY .GT. CRITSEP) GO TO 2130
      J=INDEX(I)
      DX=X(J)-XTEST
      IF (ABS(DX) .GT. CRITSEP) GO TO 2120
      RSQ=DX**2+DY**2
      IF (RSQ .GT. CRITSQ) GO TO 2120
C
C This star is within one critical separation of the star at stack
C position ITEST.  If their separation is less than one fitting radius
C plus one pixel put them in the same group no matter what.  Likewise,
C if the user entered a critical overlap less than or equal to zero,
C they belong in the same group no matter what.
C
      IF ((RSQ .LE. FRSQ) .OR. (CRIT .LE. 1.E-10)) GO TO 2115
C
C It is necessary to compute the PSF of the brighter star to determine
C whether the stars overlap by more than the critical amount.
C The two stars are already known to be separated by more than
C FR = (one fitting radius plus one pixel), and by less than one
C CRITSEP = (one PSF radius plus one fitting radius plus one pixel).
C Evaluate the point-spread function of the brighter star at a point
C FR pixels from the fainter, on the line segment connecting them.
C If this value is greater than CRIT times the standard error per
C pixel, then put them in the same group.
C
      IF (MAG(J) .GT. 90.) THEN
         MVAL=BRTMAG
      ELSE
         MVAL=10.**(0.4*(PSFMAG-MAG(J)))
      END IF
C
C For the time being, MVAL is the brightness of the star currently
C being compared with the star at ITEST.
C
      RSQ=SQRT(RSQ)
C For PSF stars, we''ll want PSF value of neighbor at a PSF radius.
C For normal grouping, we just want PSF value at the fitting radius
      DX0 = DX
      DY0 = DY
      IF (HAVEPSF) THEN
        RATIO=(RSQ-CRITSEP/4)/RSQ
        IF (RATIO .LE. 0) RATIO = 0
      ELSE
        RATIO=(RSQ-FR)/RSQ
      ENDIF
      DX=RATIO*DX
      DY=RATIO*DY
C
C Which is brighter?  What is the value of its PSF at a position DX, DY
C from the center of the fainter?
C

C For PSF stars, want to check value of fainter star at 1/2 PSF radius from
C  brighter star. For normal grouping, want to check value of brighter star at
C  one fitting radius from fainter star

      IF ((HAVEPSF .AND. MTEST .LE. MVAL) .OR. 
     &    (.NOT. HAVEPSF .AND. MTEST .GT. MVAL)) THEN
        IF (IPSFMODE .LE. 0) THEN
	  ix = int(xtest + dx)
	  iy = int(ytest + dy)
	  call cclibget(location,npsflib,nlib,ix,iy,xtest,ytest,
     &                  aval,dvdx,dvdy)
        ELSE
          AVAL=VALUE(DX, DY, GAUSS, PSF, NPSF, DVDX, DVDY)
        END IF
        QVAL=MTEST*AVAL
        if (watch .gt. 2.5) then
	  print *, ' mtest > mval or psf & mtest <=mval: '
          print *, 'mtest, qval, aval: ', mtest,qval, aval, xtest, ytest, dx, dy
        end if
C For PSF stars, we also want to check the value of the brighter star at
C   the same location
        IF (HAVEPSF) THEN
          IF (RATIO .GT. 0) THEN
            DX = DX/RATIO * (1-RATIO)
            DY = DY/RATIO * (1-RATIO)
          ELSE
            DX = DX0
            DY = DY0
          END IF
          IF (IPSFMODE .LE. 0) THEN
	    ix = int(x(j) - dx)
	    iy = int(y(i) - dy)
	    call cclibget(location,npsflib,nlib,ix,iy,x(j),y(i),
     &                  aval,dvdx,dvdy)
          ELSE
            AVAL=VALUE(-DX, -DY, GAUSS, PSF, NPSF, DVDX, DVDY)
          END IF
          QVAL2=MVAL*AVAL
        if (watch .gt. 2.5) then
          print *, 'mval, qval, aval: ', mval,qval2, aval, x(j), y(i), -dx, -dy
          print *, 'psfrad, rsq, ratiol: ', psfrad, rsq, ratio
        end if
        END IF
      ELSE
        IF (IPSFMODE .LE. 0) THEN
	  ix = int(x(j) - dx)
	  iy = int(y(i) - dy)
	  call cclibget(location,npsflib,nlib,ix,iy,x(j),y(i),
     &                  aval,dvdx,dvdy)
        ELSE
          AVAL=VALUE(-DX, -DY, GAUSS, PSF, NPSF, DVDX, DVDY)
        END IF
        QVAL=MVAL*AVAL
        if (watch .gt. 2.5) then
	  print *, ' mtest <= mval or psf & mtest >mval: '
	  print *, ' mtest > mval: '
          print *, 'mval qval, aval: ', mval, qval, aval, x(j), y(i), -dx, -dy
        end if
C For PSF stars, we also want to check the value of the brighter star at
C   the same location
        IF (HAVEPSF) THEN
          IF (RATIO .GT. 0) THEN
            DX = DX/RATIO * (1-RATIO)
            DY = DY/RATIO * (1-RATIO)
          ELSE
            DX = DX0
            DY = DY0
          END IF
          IF (IPSFMODE .LE. 0) THEN
	    ix = int(xtest + dx)
	    iy = int(ytest + dy)
	    call cclibget(location,npsflib,nlib,ix,iy,xtest,ytest,
     &                  aval,dvdx,dvdy)
          ELSE
            AVAL=VALUE(DX, DY, GAUSS, PSF, NPSF, DVDX, DVDY)
          END IF
          QVAL2=MTEST*AVAL
        if (watch .gt. 2.5) then
          print *, 'mtest, qval, aval: ', mtest,qval2, aval, xtest, ytest, dx, dy
          print *, 'psfrad, rsq, ratiol: ', psfrad, rsq, ratio
        end if
        END IF
      END IF


C      IF (MTEST .GT. QVAL) THEN
C         QVAL=MTEST*VALUE(DX, DY, GAUSS, PSF, NPSF, DVDX, DVDY)
C      ELSE
C         QVAL=QVAL*VALUE(-DX, -DY, GAUSS, PSF, NPSF, DVDX, DVDY)
C      END IF

C
C Standard error per pixel.
C
      ERR=READNS+0.5*(STEST+SKY(J))/PHPADU
C
C If the point-spread function of the brighter star is less than
C fraction CRIT (input above) of the standard error per pixel, the
C two stars are considered NOT to overlap.
C
      if (watch .gt. 2.5) then
        if (havepsf) then
          print *, 'qval, qval2, crit: ', qval, qval2, crit
        else
	  print *, 'err: ', err, sqrt(err)
	  print *, 'qval**2, crit*err', qval**2, crit*err
	  print *, 'qval, sqrt(crit*err)', qval, sqrt(crit*err)
        end if
      end if
      IF (HAVEPSF) THEN
        IF (QVAL/QVAL2 .LT. CRIT) GO TO 2120
      ELSE
        IF (QVAL**2 .LT. CRIT*ERR) GO TO 2120
      END IF
C
C The two stars overlap.  Increment the group-size counter.
C
 2115 N=N+1
C
C Now move this star up to position ITOP in the stack, where the pointer
C ITEST may eventually reach it.
C
      CALL SWAP2 (I, ITOP, INDEX, Y)
C
C Now increment ITOP by 1 to point at the topmost unassigned star in the
C stack.
C
      ITOP=ITOP+1
      JTOP=INDEX(ITOP)
CD     PRINT *, 'ITEST=', ITEST, ' ITOP=', ITOP
CD     DO 8701 III=ITOP,ITOP+10
CD       PRINT *, III, ID(INDEX(III)), X(INDEX(III)), Y(III)
CD 8701 CONTINUE
CD     READ *
 2120 CONTINUE
C
C If ITOP is greater than NTOT, then all stars have been assigned to
C groups, and we are finished.  Otherwise, increment ITEST by 1 and
C keep going.
C
 2130 IF (ITOP .LE. NTOT) GO TO 2100
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
C For PSF stars, only output stars without neighbors
      IF (HAVEPSF .AND. I .GT. 1) GOTO 9080
C
C Now loop over actual groups, writing out any group whose size is
C equal to the current value of I.
C
      IFIRST=1
 4010 IF (ISIZE(IFIRST) .NE. I) GO TO 4030
      DO 4020 J=IFIRST,IFIRST+I-1
      K=INDEX(J)
 4020 WRITE (3,340) ID(K), X(K), Y(J), MAG(K), SKY(K)
  340 FORMAT (I6, 2F9.2, 2F9.3)
      NGRP=NGRP+1
C Write a blank line
      WRITE (3,340)
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
      FORMSTR = ' '
      WRITE(FORMSTR,643) LENGRP
  643 FORMAT ('(/I6, '' star'', A4, I',I12.12,', '' group'', A4/)')
      WRITE (6,FORMSTR) NTOT, PLSTR, NGRP, PLGRP
 9080 CALL CLFILE (3)
 9090 CALL CLFILE (2)
#ifdef __NEWLIB
      if (ipsfmode .le. 0) call ccfree(4*npsflib*npsflib,location)
#endif
      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable errors.
C
C Error occurred opening input file.
C
 9400 CALL CLFILE (2)
 9410 WRITE (6,694) BELL
  694 FORMAT (/' Error opening file.', A1/)
      RETURN
C
 9500 WRITE (6,695) BELL, MAXSTR
  695 FORMAT (/' More than ', A1, I6, ' stars in input file.'/)
      RETURN
C
  575 PRINT *, 'Error reading PSFLIB file'
      GOTO 9080

      END

      SUBROUTINE  SWAP2 (I, ITOP, INDEX, Y)
C
C=======================================================================
C
C Replace the ITOP-th star in the stack with the I-th star, then shift
C the intervening stars down the stack by one place.
C
C=======================================================================
C
      REAL*4 Y(1)
      INTEGER*4 INDEX(1)
C
C-----------------------------------------------------------------------
C
      YHOLD=Y(I)
      IHOLD=INDEX(I)
      L=I+ITOP
      DO 8701 J=ITOP,I-1
         K=L-J
         Y(K)=Y(K-1)
         INDEX(K)=INDEX(K-1)
8701  CONTINUE
      Y(ITOP)=YHOLD
      INDEX(ITOP)=IHOLD
      RETURN
      END

      SUBROUTINE  SMTHPSF (PSF, NPSF)
      PARAMETER (MAXPSF=301, MAXR=220, NSEC=4, IRMIN=4)
      REAL*4 PSF(MAXPSF,MAXPSF)
      REAL*4 SUM(NSEC,MAXR), HIGH(NSEC,MAXR), LOW(NSEC,MAXR)
      INTEGER*2 N(NSEC,MAXR)
      COMMON /WORK/ SUM, HIGH, LOW, N
C
      ICENTR=(NPSF+1)/2
      RMAX=0.7071068*FLOAT(NPSF-1)
C
      DO 8702 IR=1,INT(RMAX)
         DO 8703 IS=1,NSEC
            SUM(IS,IR)=0.0
            HIGH(IS,IR)=-1.E38
            LOW(IS,IR)=1.E38
            N(IS,IR)=0
8703     CONTINUE
8702  CONTINUE
C
      DO 1900 J=1,NPSF
         IDY=J-ICENTR
         DYSQ=FLOAT(IDY**2)
         DO 1900 I=1,NPSF
            IDX=I-ICENTR
            R=SQRT(FLOAT(IDX**2)+DYSQ)
            IF (R .GT. RMAX) GO TO 1900
            IR=INT(R)
            IF (IR .LT. IRMIN) GO TO 1900
            IS=ISCTR(IDX,IDY)
            SUM(IS,IR)=SUM(IS,IR)+PSF(I,J)
            IF (PSF(I,J) .GT. HIGH(IS,IR)) HIGH(IS,IR)=PSF(I,J)
            IF (PSF(I,J) .LT. LOW(IS,IR)) LOW(IS,IR)=PSF(I,J)
            N(IS,IR)=N(IS,IR)+1
 1900 CONTINUE
C
      DO 8704 IR=IRMIN,INT(RMAX)
         DO 8705 IS=1,NSEC
            IF (N(IS,IR) .GT. 2)
     .           SUM(IS,IR)=(SUM(IS,IR)-HIGH(IS,IR)-LOW(IS,IR))
     .                         /FLOAT(N(IS,IR)-2)
8705     CONTINUE
8704  CONTINUE
C
      DO 2900 J=1,NPSF
         IDY=J-ICENTR
         DYSQ=FLOAT(IDY**2)
         DO 2900 I=1,NPSF
            IDX=I-ICENTR
            R=SQRT(FLOAT(IDX**2)+DYSQ)
            IF (R .GT. RMAX) GO TO 2900
            IR=INT(R)
            IF (IR .LT. IRMIN) GO TO 2900
            IS=ISCTR(IDX,IDY)
            PSF(I,J)=SUM(IS,IR)
 2900 CONTINUE
C
      RETURN
      END
C
C
C
      FUNCTION ISCTR (I,J)
C
C Obtain sector number from rectilinear offsets I and J.
C
      IF (I .GT. 0) THEN
         ISCTR=1
      ELSE IF (I .LT. 0) THEN
         ISCTR=3
      ELSE
         IF (J .LE. 0) THEN
            ISCTR=1
         ELSE
            ISCTR=3
         END IF
      END IF
      IF (J .GT. 0) THEN
         ISCTR=ISCTR+1
      ELSE IF (J .EQ. 0) THEN
         IF (I .GT. 0) ISCTR=2
      END IF
      RETURN
      END
