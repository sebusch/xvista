#include "Vista.h"

C----------------------------------------------------------------------
C
C  POLYSUBS - Subroutines called by the Vista routines
C             that analyze images inside user defined
C             Polygons: ABX and CLIP
C
C   Includes:
C
C     POLYBOX:  Find the box which circumscribes the polygon
C               and a point outside the polygon.
C     IN_POLY:  Logical Function: Is the point inside the polygon?
C
C   Modification History:
C     1991 Nov 7: added improved IN_POLY algorithm suggested by
C                 Friedel Loinger [RWP/OSU]
C
C----------------------------------------------------------------------


      SUBROUTINE POLYBOX(RMIN,RMAX,CMIN,CMAX,ROUT,COUT,IPOLY)

C
C   POLYBOX - Find a box around the polygon and point (ROUT,COUT)
C             outside.
C
C   Called by the POLYGON subroutines of VISTA version 4.2
C
C     Author: Friedel Loinger, Dec 1990, Wise Observatory, Israel
C     Modified for Lick Vista 4.2: R. Pogge, OSU Astronomy, 1991 May 21
C
C----------------------------------------------------------------------

C   POLYVERT Common Block

#ifdef VMS
      INCLUDE 'POLYVERT.INC'
#else
      include 'vistadisk/source/include/polyvert.inc'
#endif

C   External Variables

      INTEGER  RMIN, RMAX
      INTEGER  CMIN, CMAX
      REAL*4   ROUT, COUT
      INTEGER  IPOLY

C   Internal Variables

      INTEGER  IR, IC
      INTEGER  NP

C   Initializations

      NP = NO_P(IPOLY)

C   Begin

      RMIN = P_ROWS(1,IPOLY)
      CMIN = P_COLS(1,IPOLY)
      RMAX = RMIN
      CMAX = CMIN
	
      DO 9001 N = 2, NP
         IR = P_ROWS(N,IPOLY)
         IC = P_COLS(N,IPOLY)
         RMIN = MIN0(IR,RMIN)
         RMAX = MAX0(IR,RMAX)
         CMIN = MIN0(IC,CMIN)
         CMAX = MAX0(IC,CMAX)
 9001 CONTINUE
      COUT = FLOAT(CMIN) - 1.
      ROUT = FLOAT(RMIN) - 1.123456789

      RETURN
      END

C----------------------------------------------------------------------

      LOGICAL FUNCTION IN_POLY (ROW,COL,ROUT,COUT,IPOLY)

C
C   IN_POLY - Is the image pixel inside the polygon?
C
C     This subroutine gets as a input the pixel coordinates (COL,ROW) 
C     and decides if it is inside the IPOLY polygon. (COL,ROW) will be 
C     connected with (COUT,ROUT), a point outside the box around 
C     the polygon, with a line of the form: Y = A1 + B1*X, and then the 
C     subroutine counts the no. of intersection points with the lines of 
C     the polygon, Y = A2 + B2*X.
C
C     Note: COUT is defined so that no intersection point will land on
C           one of the vertices of the polygon leading to double counting.
C
C     Author: Friedel Loinger, Dec 1990, Wise Observatory, Israel
C     Modified for Lick Vista 4.2: R. Pogge, OSU Astronomy, 1991 May 21
C     Changed to new algorithm that better senses when the side of the
C     polygon exactly crosses a point as suggested by F. Loinger [RWP/11/7/91]
C
C----------------------------------------------------------------------

C   POLYVERT Common Block

#ifdef VMS
      INCLUDE 'POLYVERT.INC'
#else
      include 'vistadisk/source/include/polyvert.inc'
#endif

C   External Variables

      INTEGER  ROW, COL
      INTEGER  IPOLY
      REAL*4   ROUT, COUT

C   Internal Variables

      REAL*8   ROWOUT, COLOUT
      REAL*8   CMIN, CMAX
      REAL*8   RMIN, RMAX
      REAL*8   A1, B1
      REAL*8   A2, B2
      REAL*8   PC1, PC2
      REAL*8   PR1, PR2
      REAL*8   PCMIN, PCMAX
      REAL*8   PRMIN, PRMAX
      REAL*8   X, Y

C   Initializations

      NP = NO_P(IPOLY)

C   Begin

      COLOUT = DBLE(COUT)
      ROWOUT = DBLE(ROUT)
      CMIN = DMIN1(COLOUT,DBLE(COL))
      CMAX = DMAX1(COLOUT,DBLE(COL))
      RMIN = DMIN1(ROWOUT,DBLE(ROW))
      RMAX = DMAX1(ROWOUT,DBLE(ROW))
      B1 = (RMAX - RMIN)/(CMAX - CMIN)
      A1 = -B1*CMIN + RMIN
      NXY = 0
      P_COLS(NP+1,IPOLY) = P_COLS(1,IPOLY)
      P_ROWS(NP+1,IPOLY) = P_ROWS(1,IPOLY)

      DO 9001 N = 1, NP
         PC1 = DBLE(P_COLS(N,IPOLY))
         PC2 = DBLE(P_COLS(N+1,IPOLY))
         PR1 = DBLE(P_ROWS(N,IPOLY))
         PR2 = DBLE(P_ROWS(N+1,IPOLY))
         PCMIN = DMIN1(PC1,PC2)
         PCMAX = DMAX1(PC1,PC2)
         PRMIN = DMIN1(PR1,PR2)
         PRMAX = DMAX1(PR1,PR2)

C   Check to see if the input pixel lies exactly on one side of the polygon

         ICN = P_COLS(N,IPOLY)
         IRN = P_ROWS(N,IPOLY)
         IDELC = P_COLS(N+1,IPOLY) - ICN
         IDELR = P_ROWS(N+1,IPOLY) - IRN
         IF ((ROW-IRN)*IDELC .EQ. (COL-ICN)*IDELR) THEN
            IF (COL .LE. NINT(PCMAX) .AND. COL .GE. NINT(PCMIN)) THEN
               IF (ROW .LE. NINT(PRMAX) .AND. 
     &             ROW .GE. NINT(PRMIN)) THEN
                  IN_POLY = .TRUE.
                  RETURN
               END IF
            END IF
         END IF
         IF (IDELC .EQ. 0) THEN
            X = PC1
         ELSE
            B2 = (PR2-PR1)/(PC2-PC1)
            A2 = -B2*PC1 + PR1
            IF (B1 .NE. B2) X = (A1-A2)/(B2-B1)
         END IF
         IF (X .GE. CMIN .AND. X .LE. CMAX) THEN
            IF (X .GE. PCMIN .AND. X .LE. PCMAX) THEN
               IF (IDELR .EQ. 0) THEN
                  Y = PR1
               ELSE
                  IF (IDELC .EQ. 0) THEN
                     Y = A1 + B1 * X
                  ELSE
                     Y = A2 + B2 * X
                  END IF
               END IF
               IF (Y .GE. PRMIN .AND. Y .LE. PRMAX) THEN
                  IF (Y .GE. RMIN .AND. Y .LE. RMAX) THEN
                     NXY = NXY + 1
                  END IF
               END IF
            END IF
         END IF
 9001 CONTINUE

C   If the number of intersection points is odd, the (COL,ROW) is
C   inside the polygon

      N2 = NXY/2
      IF ((NXY - 2*N2) .EQ. 0) THEN
         IN_POLY = .FALSE.
      ELSE
         IN_POLY = .TRUE.
      END IF
      RETURN
      END

