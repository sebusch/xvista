#include "Config.h"
      SUBROUTINE PGCNIT (I, J, K)

C
C PGPLOT (internal routine): Initial triangle. (Support routine for
C MPGCNSC.)
C
C Arguments:
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

      INTEGER          I, J, K, M1, M2, M3
      REAL             Z1, Z2, Z3
      COMMON /PGCON1/  M1(2), M2(2), M3(2), Z1, Z2, Z3

      M1(1) = I+K-1
      M1(2) = J+K-1
      M2(1) = I+1
      M2(2) = J
      M3(1) = I
      M3(2) = J+1

      END

********************************************************************************

      SUBROUTINE PGCNNC

C
C PGPLOT (internal routine): Next, constant (support routine for
C MPGCNSC). Select P3 to define the next triangle along a constant
C contour. Points P1 and P2 of the new triangle will be points of the
C old triangle, while P3 will be a new point gotten by reflection of
C the missing point in the opposite edge.
C
C Arguments: none.
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	INTEGER          I, J, M1, M2, M3
	REAL             Z, Z1, Z2, Z3
	COMMON /PGCON1/  M1(2), M2(2), M3(2), Z1, Z2, Z3

	IF (Z1*Z3 .LT. 0.0) THEN
	    I = M1(1)-M2(1)+M3(1)
	    J = M1(2)-M2(2)+M3(2)
	    Z = Z3
	    Z3 = Z2
	    Z2 = Z
	    M2(1) = M3(1)
	    M2(2) = M3(2)
	    M3(1) = I
	    M3(2) = J

	ELSE IF (Z2*Z3 .LT. 0.0) THEN
	    I = -M1(1)+M2(1)+M3(1)
	    J = -M1(2)+M2(2)+M3(2)
	    Z = Z3
	    Z3 = Z1
	    Z1 = Z
	    M1(1) = M3(1)
	    M1(2) = M3(2)
	    M3(1) = I
	    M3(2) = J

	END IF

      END

********************************************************************************

      SUBROUTINE PGCNRE

C
C PGPLOT (internal routine): Restore the initial point of the contour.
C (Support routine for MPGCNSC.)
C
C Arguments: none.
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

      INTEGER          N, M
      REAL             W, Z
      COMMON /PGCON1/  M(6), Z(3)
      COMMON /PGCON2/  N(6), W(3)

      M(1) = N(1)
      M(2) = N(2)
      M(3) = N(3)
      M(4) = N(4)
      M(5) = N(5)
      M(6) = N(6)
      Z(1) = W(1)
      Z(2) = W(2)
      Z(3) = W(3)
      END

********************************************************************************

      SUBROUTINE PMGO(MPGCONS)(A, IDIM, JDIM, I1, I2, J1, J2,
     &C, NC, TX)
C
C MPGCONS -- contour map of a 2D data array (fast algorithm)
C
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

      INTEGER IDIM, JDIM, I1, I2, J1, J2, NC
      REAL    A(IDIM,JDIM), C(*), TX(6)

C
C Draw a contour map of an array. The map is truncated if
C necessary at the boundaries of the viewport.  Each contour line is
C drawn with the current line attributes (color index, style, and
C width).  This routine, unlike MPGCONT, does not draw each contour as a
C continuous line, but draws the straight line segments composing each
C contour in a random order.  It is thus not suitable for use on pen
C plotters, and it usually gives unsatisfactory results with dashed or
C dotted lines.  It is, however, faster than MPGCONT, especially if
C several contour levels are drawn with one call of MPGCONS.
C
C Arguments:
C  A      (input)  : data array.
C  IDIM   (input)  : first dimension of A.
C  JDIM   (input)  : second dimension of A.
C  I1,I2  (input)  : range of first index to be contoured (inclusive).
C  J1,J2  (input)  : range of second index to be contoured (inclusive).
C  C      (input)  : array of contour levels (in the same units as the
C                    data in array A); dimension at least NC.
C  NC     (input)  : number of contour levels (less than or equal to
C                    dimension of C). The absolute value of this
C                    argument is used (for compatibility with MPGCONT,
C                    where the sign of NC is significant).
C  TX     (input)  : array defining a transformation between the I,J
C                    grid of the array and the world coordinates. The
C                    world coordinates of the array point A(I,J) are
C                    given by:
C                      X = TX(1) + TX(2)*I + TX(3)*J
C                      Y = TX(4) + TX(5)*I + TX(6)*J
C                    Usually TX(3) and TX(5) are zero - unless the
C                    coordinate transformation involves a rotation
C                    or shear.
C--
C (27-Aug-1984) (TJP).
C-----------------------------------------------------------------------

*        INCLUDE  'pgplot.inc'
	real            trans(6)
	common  /pgplt1/        trans
	include  'MONGOLUN.inc'
	INTEGER  I, IC, ICORN, IDELT(6), J

	INTEGER  NPT

	REAL     CTR, DELTA, DVAL(5)

	REAL     X(2), Y(2)

        integer  ierr
        integer  PTUV(MPGCP)

	DATA     IDELT/0,-1,-1,0,0,-1/

C
C               Check arguments
C

*   PGOPEN is 0 if not device has been opened

*        IF (PGOPEN.EQ.0) RETURN

*   If the index range of the array to map is out of bounds, signal and error
*   and terminate

	IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1     J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2)
     2     write(STDERR,'(''MPGCONS: invalid range I1:I2, J1:J2'')')
*     2          CALL PGERR('MPGCONS: invalid range I1:I2, J1:J2')

*   If no contours are to be plotted, terminate

	IF (NC.EQ.0) RETURN

C
*   Save Transformation Matrix in TRANS, which is carried in common via the
*   pgplot.inc
C

	DO 10 I=1,6
	    TRANS(I) = TX(I)
10      CONTINUE

	DO 130 J=J1+1,J2
	DO 130 I=I1+1,I2
	  DVAL(1) = A(I-1,J)
	  DVAL(2) = A(I-1,J-1)
	  DVAL(3) = A(I,J-1)
	  DVAL(4) = A(I,J)
	  DVAL(5) = DVAL(1)
	DO 110 IC=1,ABS(NC)
	CTR = C(IC)
	  NPT = 0
	  DO 120 ICORN=1,4
	  IF( (DVAL(ICORN).LT.CTR .AND. DVAL(ICORN+1).LT.CTR)
     1    .OR.(DVAL(ICORN).GE.CTR .AND. DVAL(ICORN+1).GE.CTR) ) GOTO 120
	    NPT=NPT+1
	    DELTA = (CTR-DVAL(ICORN))/(DVAL(ICORN+1)-DVAL(ICORN))
	    GOTO (60,70,60,70), ICORN

   60       X(NPT) = I+IDELT(ICORN+1)
	    Y(NPT) = FLOAT(J+IDELT(ICORN)) + DELTA*FLOAT(
     1                 IDELT(ICORN+1)-IDELT(ICORN))
	    GOTO 80

   70       X(NPT) = FLOAT(I+IDELT(ICORN+1)) + DELTA*FLOAT(
     1                 IDELT(ICORN+2)-IDELT(ICORN+1))
	    Y(NPT) = J+IDELT(ICORN)


   80       IF(NPT.LT.2) GOTO 120
              ierr = PTUV(MPGCP)(0,X(1),Y(1),CTR)
	      ierr = PTUV(MPGCP)(1,X(2),Y(2),CTR)
	      NPT = 0
  120     CONTINUE
  110     CONTINUE
  130   CONTINUE

*      IF (FLUSH) CALL TIDLE

      END


********************************************************************************

      SUBROUTINE PGCNSA
C
C PGPLOT (internal routine): Save the initial point of the contour.
C (Support routine for MPGCNSC.)
C
C Arguments: none.
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

      INTEGER          M, N
      REAL             W, Z

      COMMON /PGCON1/  M(6), Z(3)
      COMMON /PGCON2/  N(6), W(3)

      N(1) = M(1)
      N(2) = M(2)
      N(3) = M(5)
      N(4) = M(6)
      N(5) = M(3)
      N(6) = M(4)
      W(1) = Z(1)
      W(2) = Z(3)
      W(3) = Z(2)

      END

********************************************************************************

      SUBROUTINE PTUV(MPGCNSC)(ZE, MX, MY, IA, IB, JA, JB, Z0, PL2)

C
C PGPLOT (internal routine): Draw a single contour.  This routine is
C called by MPGCONT (the contour following algorithm), but may be called
C directly by the user.
C
C Arguments:
C
C ZE (real array dimension MX,MY, input): the array of function values.
C MX,MY (integer, input): actual declared dimension of Z(*,*).
C IA,IB (integer, input): inclusive range of the first index of ZE to be
C       contoured.
C JA,JB (integer, input): inclusive range of the second index of ZE to
C       be contoured.
C Z0 (real, input): the contour level sought.
C PL2 (the name of a subroutine declared EXTERNAL in the calling
C       routine): this routine is called by MPGCNSC to do all graphical
C       output. The calling sequence is CALL PL2(K,X,Y,Z) where Z is
C       the contour level, (X,Y) are the coordinates of a point (in the
C       inclusive range I1<X<I2, J1<Y<J2, and if K is 0, the routine is
C       to move then pen to (X,Y); if K is 1, it is to draw a line from
C       the current position to (X,Y).
C
C NOTE:  the intervals (IA,IB) and (JA,JB) must not exceed the
C dimensions of internal array EMPTY(*,*,*) below.  These are currently
C set at 50.
C
C (7-Feb-1983)
C 28-Aug-1984 Attempt to improve behavior on plateaus [TJP].
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

      include   'MONGOLUN.inc'
      INTEGER  MAXEMX, MAXEMY

      PARAMETER (MAXEMX=50, MAXEMY=50)
      integer          pl2
      EXTERNAL         PL2
      INTEGER          I, I0, IA, IAM1, IARG, IARG1, IARG2, IB
      INTEGER          II, INDEX, IOFF, IX, IXPMX, J, J0
      INTEGER          JA, JAM1, JARG, JB
      INTEGER          JJ, JOFF, K, K0, L, MX, MY

C
C IMPLEMENTATION NOTE:  for a strict Fortran-77 installation, change
C the following LOGICAL*1 to LOGICAL.  Use of LOGICAL*1 saves memory.
C

#ifdef  VMS
      LOGICAL*1        EMPTY(2,MAXEMX,MAXEMY), FOUND, NOTIN1, NOTIN2
#else   /* VMS */
      LOGICAL          EMPTY(2,MAXEMX,MAXEMY), FOUND, NOTIN1, NOTIN2
#endif  /* VMS */
c     REAL             XS, YS, ZARG
      REAL             Z0, Z11, Z12, Z21, Z22
      REAL             ZE(*), ZM1, ZM2
      REAL             ZP, ZP1, ZP2, ZS
      INTEGER          I1, I2, I3, J1, J2, J3
      REAL             Z1, Z2, Z3
      COMMON /PGCON1/  I1, J1, I2, J2, I3, J3, Z1, Z2, Z3

      integer  ierr

C  executable code

      INDEX(JARG,IARG) = IARG + MX*(JARG-1)
      ZP(IARG1,IARG2) = FLOAT(IARG1)-ZS*FLOAT(IARG2-IARG1)

C
C Check for errors.
C

*  GRWARN puts a warning message on SYS$OUTPUT

      IF ( (IB-IA) .GT. MAXEMX .OR.  (JB-JA) .GT. MAXEMY ) THEN

*          CALL GRWARN('MPGCNSC - array index range exceeds'//
*     1                ' built-in limit of 50')
	write(STDERR,
     & '(''MPGCNSC - array index range exceeds built-in limit of 50'')')
	  RETURN

      END IF

C
C Scan the array cross-section, marking triangles which
C contain part of the requested contour.  EMPTY(K,I,J) is
C .TRUE. if triangle K (1=upper,2=lower) in the box with
C lower-left corner at (I,J) does ***NOT*** contain the
C contour.
C

      II = MAX(IA,IB-1)
      JJ = MAX(JA,JB-1)
      IAM1 = IA-1
      JAM1 = JA-1
      FOUND = .FALSE.
      DO 20 J = JA,JJ
	  IX = INDEX(J,IA)
	  JOFF = J-JAM1
	  DO 10 I = IA,II
	      IXPMX = IX+MX
	      IOFF = I-IAM1
	      Z11 = ZE(IX)
	      Z12 = ZE(IXPMX)
	      Z21 = ZE(IX+1)
	      Z22 = ZE(IXPMX+1)
	      ZP1 = MAX(Z11,Z12,Z21)
	      ZM1 = MIN(Z11,Z12,Z21)
	      ZP2 = MAX(Z12,Z21,Z22)
	      ZM2 = MIN(Z12,Z21,Z22)
CC            NOTIN1 = (ZP1 .LT. Z0) .OR. (ZM1 .GT. Z0)
CC            NOTIN2 = (ZP2 .LT. Z0) .OR. (ZM2 .GT. Z0)
	      NOTIN1 = (ZP1 .LT. Z0) .OR. (ZM1 .GE. Z0)
	      NOTIN2 = (ZP2 .LT. Z0) .OR. (ZM2 .GE. Z0)
	      FOUND = FOUND .OR. (.NOT.NOTIN1) .OR. (.NOT.NOTIN2)
	      EMPTY(1,IOFF,JOFF) = NOTIN1
	      EMPTY(2,IOFF,JOFF) = NOTIN2
	      IX = IX+1
   10     CONTINUE
   20 CONTINUE

      IF (.NOT.FOUND) RETURN

C
C Contour intersects array.  Search array for a cell
C which includes the contour, then start following the
C contour.
C

      DO 80 J = JA,JJ
	  JOFF = J-JAM1
	  DO 70 I = IA,II
	      DO 60 K = 1,2
		  IOFF = I-IAM1
		  IF (EMPTY(K,IOFF,JOFF)) GO TO 70

C
C We have a contour in triangle (K,IOFF,JOFF). Follow it through the
C mesh, marking triangles as visited when the contour passes through
C them.
C

		  CALL PGCNIT (I,J,K)

		  Z1 = ZE(INDEX(J1,I1)) - Z0
		  Z2 = ZE(INDEX(J2,I2)) - Z0
		  Z3 = ZE(INDEX(J3,I3)) - Z0
		  IF (Z1*Z2 .GT. 0.0) CALL PGCNXV (1,3)
		  IF (Z1*Z3 .GT. 0.0) CALL PGCNXV (1,2)

		  CALL PGCNSA

		  DO 50 L = 1,2
		      ZS = 0.0
		      IF (Z2 .NE. Z1) ZS = Z1/(Z2 - Z1)
		      ierr = PL2 (0,ZP(I1,I2),ZP(J1,J2),Z0)

   40                 CALL PGCNNC

		      ZS = 0.0
		      IF (Z2 .NE. Z1) ZS = Z1/(Z2 - Z1)

		      ierr = PL2 (1,ZP(I1,I2),ZP(J1,J2),Z0)

		      IF (I3 .LT. IA) GO TO 50
		      IF (I3 .GT. IB) GO TO 50
		      IF (J3 .LT. JA) GO TO 50
		      IF (J3 .GT. JB) GO TO 50
		      I0 = MIN(I1,I2,I3)-IAM1
		      J0 = MIN(J1,J2,J3)-JAM1
		      K0 = MOD(I1+I2+I3,3)
		      IF (EMPTY(K0,I0,J0)) GO TO 50
		      EMPTY(K0,I0,J0) = .TRUE.
		      Z3 = ZE(INDEX(J3,I3)) - Z0
		      GO TO 40

   50             CALL PGCNRE

   60         CONTINUE

   70     CONTINUE

   80 CONTINUE

      END

********************************************************************************

C*MPGCONT -- contour map of a 2D data array (contour-following algorithm)
C+

      SUBROUTINE PMGO(MPGCONT)(A, IDIM, JDIM, I1, I2, J1, J2,
     &C, NC, TX)
      INTEGER IDIM, JDIM, I1, J1, I2, J2, NC
      REAL A(IDIM,JDIM), C(*), TX(6)

C
C Draw a contour map of an array.  The map is truncated if
C necessary at the boundaries of the viewport.  Each contour line
C is drawn with the current line attributes (color index, style, and
C width); except that if argument NC is positive (see below), the line
C style is set by MPGCONT to 1 (solid) for positive contours or 2
C (dashed) for negative contours.
C
C Arguments:
C  A      (input) : data array.
C  IDIM   (input) : first dimension of A.
C  JDIM   (input) : second dimension of A.
C  I1, I2 (input) : range of first index to be contoured (inclusive).
C  J1, J2 (input) : range of second index to be contoured (inclusive).
C  C      (input) : array of NC contour levels; dimension at least NC.
C  NC     (input) : +/- number of contour levels (less than or equal
C                   to dimension of C). If NC is positive, it is the
C                   number of contour levels, and the line-style is
C                   chosen automatically as described above. If NC is
C                   negative, it is minus the number of contour
C                   levels, and the current setting of line-style is
C                   used for all the contours.
C  TX     (input) : array defining a transformation between the I,J
C                   grid of the array and the world coordinates.
C                   The world coordinates of the array point A(I,J)
C                   are given by:
C                     X = TX(1) + TX(2)*I + TX(3)*J
C                     Y = TX(4) + TX(5)*I + TX(6)*J
C                   Usually TX(3) and TX(5) are zero - unless the
C                   coordinate transformation involves a rotation or
C                   shear.
C--
C (7-Feb-1983)
C (24-Aug-1984) Revised to add the option of not automatically
C       setting the line-style. Sorry about the ugly way this is
C       done (negative NC); this is the least incompatible way of doing
C       it (TJP).
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

      include  'MONGOPAR.inc'
      include  'MONGOLUN.inc'
*      INCLUDE  'pgplot.inc'
	real            trans(6)
	common  /pgplt1/        trans
      INTEGER  MAXEMX,MAXEMY
      PARAMETER (MAXEMX=50)
      PARAMETER (MAXEMY=50)
      INTEGER  I
c     INTEGER  J
      INTEGER  NNX,NNY, KX,KY, IX,IY, I0,J0, II,JJ, IA,IB, JA,JB, LS
      LOGICAL  STYLE
      INTEGER  PTUV(MPGCP)
      EXTERNAL PTUV(MPGCP)

C
C               Check arguments
C

*      IF (PGOPEN.EQ.0) RETURN

      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2)
     2    write(STDERR,'(''MPGCONT: invalid range I1:I2, J1:J2'')')
*     2          CALL PGERR('MPGCONT: invalid range I1:I2, J1:J2')

      IF (NC.EQ.0) RETURN

      STYLE = NC.GT.0

*   GRINQLS = GR INQuire Line Style - returns current line style.

*      CALL GRINQLS(LS)
      ls = ltype

C
C Save TRANS matrix.
C

      DO 10 I=1,6
	  TRANS(I) = TX(I)
   10 CONTINUE

C
C Divide arrays into panels not exceeding MAXEMX by MAXEMY for
C contouring by MPGCNSC.
C

      NNX = I2-I1+1
      NNY = J2-J1+1
      KX = MAX(1,(NNX+MAXEMX-1)/MAXEMX)
      KY = MAX(1,(NNY+MAXEMY-1)/MAXEMY)
      I0 = MAX(NNX/(2*KX),1)
      J0 = MAX(NNY/(2*KY),1)
      IX = 2*I0
      IY = 2*J0
      II = I0+I1
      JJ = J0+J1
   20 JA = MAX(JJ-J0,J1)
      JB = MIN(JJ+J0,J2)
   30 IA = MAX(II-I0,I1)
      IB = MIN(II+I0,I2)

C
C Draw the contours in one panel.
C

*  In PGPLOT, type 1 = solid, type 2 = dashed.
*  I have changed this below to conform the MONGO

      IF (STYLE) CALL PMGO(SETLTYPE)(0)

      DO 40 I=1,ABS(NC)
	  IF (STYLE.AND.(C(I).LT.0.0)) CALL PMGO(SETLTYPE)(2)
	  CALL PTUV(MPGCNSC)(A,IDIM,JDIM,IA,IB,JA,JB,C(I), PTUV(MPGCP))
	  IF (STYLE) CALL PMGO(SETLTYPE)(0)
   40 CONTINUE

      II = II+IX
      IF ((II-I0.LT.I2) .AND. (II+I0.GT.1)) GOTO 30
      IX = -IX
      II = II+IX
      JJ = JJ+IY
      IF (JJ-J0.LT.J2) GOTO 20

*   Restore original line style

      CALL PMGO(SETLTYPE)(LS)

*      IF (FLUSH) CALL GRTERM
C      IF (FLUSH) CALL TIDLE

      END

********************************************************************************
      SUBROUTINE PMGO(MPGCONX)(A, IDIM, JDIM, I1, I2, J1, J2, C, NC,
     &PLOT)

C*MPGCONX -- contour map of a 2D data array (non-rectangular)
C+

C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
      INTEGER  IDIM, JDIM, I1, J1, I2, J2, NC
      REAL     A(IDIM,JDIM), C(*)
      integer  plot
      EXTERNAL PLOT

C
C Draw a contour map of an array using a user-supplied plotting
C routine.  This routine should be used instead of MPGCONT when the
C data are defined on a non-rectangular grid.  MPGCONT permits only
C a linear transformation between the (I,J) grid of the array
C and the world coordinate system (x,y), but MPGCONX permits any
C transformation to be used, the transformation being defined by a
C user-supplied subroutine. The nature of the contouring algorithm,
C however, dictates that the transformation should maintain the
C rectangular topology of the grid, although grid-points may be
C allowed to coalesce.  As an example of a deformed rectangular
C grid, consider data given on the polar grid theta=0.1n(pi/2),
C for n=0,1,...,10, and r=0.25m, for m=0,1,..,4. This grid
C contains 55 points, of which 11 are coincident at the origin.
C The input array for MPGCONX should be dimensioned (11,5), and
C data values hould be provided for all 55 elements.  MPGCONX can
C also be used for special applications in which the height of the
C contour affects its appearance, e.g., stereographic views.
C
C The map is truncated if necessary at the boundaries of the viewport.
C Each contour line is drawn with the current line attributes (color
C index, style, and width); except that if argument NC is positive
C (see below), the line style is set by MPGCONX to 1 (solid) for
C positive contours or 2 (dashed) for negative contours. Attributes
C for the contour lines can also be set in the user-supplied
C subroutine, if desired.
C
C Arguments:
C  A      (input) : data array.
C  IDIM   (input) : first dimension of A.
C  JDIM   (input) : second dimension of A.
C  I1, I2 (input) : range of first index to be contoured (inclusive).
C  J1, J2 (input) : range of second index to be contoured (inclusive).
C  C      (input) : array of NC contour levels; dimension at least NC.
C  NC     (input) : +/- number of contour levels (less than or equal
C                   to dimension of C). If NC is positive, it is the
C                   number of contour levels, and the line-style is
C                   chosen automatically as described above. If NC is
C                   negative, it is minus the number of contour
C                   levels, and the current setting of line-style is
C                   used for all the contours.
C  PLOT   (input) : the address (name) of a subroutine supplied by
C                   the user, which will be called by MPGCONX to do
C                   the actual plotting. This must be declared
C                   EXTERNAL in the program unit calling MPGCONX.
C
C The subroutine PLOT will be called with four arguments:
C      CALL PLOT(VISBLE,X,Y,Z)
C where X,Y (real, input) are real varaibles corresponding to
C I,J indices of the array A. If  VISBLE (input, integer) is 1,
C PLOT should draw a visible line from the current pen
C position to the world coordinate point coorresponding to (X,Y);
C if it is 0, it should move the pen to (X,Y). Z is the value
C of the current contour level, and may be used by PLOT if desired.
C Example:
C       SUBROUTINE PLOT (VISBLE,X,Y,Z)
C       REAL X, Y, Z, XWORLD, YWORLD
C       INTEGER VISBLE
C       XWORLD = X*COS(Y) ! this is the user-defined
C       YWORLD = X*SIN(Y) ! transformation
C       IF (VISBLE.EQ.1) THEN
C           CALL DRAW (XWORLD, YWORLD)
C       ELSE
C           CALL RELOCATE (XWORLD, YWORLD)
C       END IF
C       END
C--
C 14-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------

      include 'MONGOPAR.inc'
      include 'MONGOLUN.inc'
      INTEGER  MAXEMX,MAXEMY
      PARAMETER (MAXEMX=50)
      PARAMETER (MAXEMY=50)
      INTEGER  I
c     INTEGER  J
      INTEGER  NNX,NNY, KX,KY, IX,IY, I0,J0, II,JJ, IA,IB, JA,JB, LS
      LOGICAL  STYLE

C
C Check arguments.
C

      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2)
     2    write(STDERR,'(''MPGCONX: invalid range I1:I2, J1:J2'')')
*     2          CALL PGERR('MPGCONX: invalid range I1:I2, J1:J2')

      IF (NC.EQ.0) RETURN

      STYLE = NC.GT.0

*  As in MPGCONT, inquire as to the current line style

*      CALL GRINQLS(LS)
      ls = ltype

C
C Divide arrays into panels not exceeding MAXEMX by MAXEMY for
C contouring by MPGCNSC.
C

      NNX = I2-I1+1
      NNY = J2-J1+1
      KX = MAX(1,(NNX+MAXEMX-1)/MAXEMX)
      KY = MAX(1,(NNY+MAXEMY-1)/MAXEMY)
      I0 = MAX(NNX/(2*KX),1)
      J0 = MAX(NNY/(2*KY),1)
      IX = 2*I0
      IY = 2*J0
      II = I0+I1
      JJ = J0+J1
   20 JA = MAX(JJ-J0,J1)
      JB = MIN(JJ+J0,J2)
   30 IA = MAX(II-I0,I1)
      IB = MIN(II+I0,I2)

C
C Draw the contours in one panel.
C

*  Modified as in MPGCONT to conform to MONGO line type codes

      IF (STYLE) CALL PMGO(SETLTYPE)(0)

      DO 40 I=1,ABS(NC)
	  IF (STYLE.AND.(C(I).LT.0.0)) CALL PMGO(SETLTYPE)(2)
	  CALL PTUV(MPGCNSC)(A,IDIM,JDIM,IA,IB,JA,JB,C(I),PLOT)
	  IF (STYLE) CALL PMGO(SETLTYPE)(0)
   40 CONTINUE

      II = II+IX
      IF ((II-I0.LT.I2) .AND. (II+I0.GT.1)) GOTO 30
      IX = -IX
      II = II+IX
      JJ = JJ+IY
      IF (JJ-J0.LT.J2) GOTO 20

*   Restore Line Type

      CALL PMGO(SETLTYPE)(LS)

*   PGUPDT with argument 2 means update the display device NOW.

*      CALL PGUPDT(2)

      END

********************************************************************************

      SUBROUTINE PGCNXV (I, J)

C
C PGPLOT (internal routine): exchange the Ith and Jth vectors in
C common (support routine for MPGCNSC).
C
C Arguments:
C
C I,J (integer, input): the vectors to be exchanged (1,2 or 3).
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

      INTEGER          I, J, MM, N1, N2
      REAL             T, Z
      COMMON /PGCON1/  MM(2,3),     Z(3)

      N1 = MM(1,I)
      N2 = MM(2,I)
      MM(1,I) = MM(1,J)
      MM(2,I) = MM(2,J)
      MM(1,J) = N1
      MM(2,J) = N2
      T = Z(I)
      Z(I) = Z(J)
      Z(J) = T

      END

*******************************************************************************

      integer function PTUV(MPGCP) (k, x, y, z)

C
C PGPLOT (internal routine): Draw one contour segment (for use by
C MPGCNSC).
C
C Arguments:
C
C K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
C       a line from the current position to (X,Y); otherwise
C       do nothing.
C X (input, real): X world-coordinate of end point.
C Y (input, real): Y world-coordinate of end point.
C Z (input, real): the value of the contour level, not used by MPGCP at
C       the moment.
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

*      INCLUDE  'pgplot.inc'
	real            trans(6)
	common  /pgplt1/        trans
      INTEGER  K
      REAL     X,XX,Y,YY,Z

      XX = TRANS(1) + TRANS(2)*X + TRANS(3)*Y
      YY = TRANS(4) + TRANS(5)*X + TRANS(6)*Y

      IF (K.EQ.1) THEN
	  CALL PMGO(DRAW)(XX,YY)
      ELSE IF (K.EQ.0) THEN
	  CALL PMGO(RELOCATE)(XX,YY)
      END IF

      END
