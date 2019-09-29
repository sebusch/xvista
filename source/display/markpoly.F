#include "Vista.h"

      SUBROUTINE MARKPOLY

C
C   MARKPOLY - Mark polygons on an image for use with CLIP or ABX
C
C     This subroutine marks a polygon on the TV.  It stores the
C     image coordinates of the vertices in (P_COLS,P_ROWS), and the 
C     number of vertices in NO_P.  Up to NMAXPOLY polygons can be
C     defined, with a maximum of NMAXVERT vertices each.
C
C   Command Syntax:
C
C     MARKPOLY npoly
C
C   where npoly = ID number of the polygon to mark.  
C
C   Cursor Commands:
C
C     X        Mark vertex of the polygon
C     M        Mistake: Delete last vertex
C     E        Finish the Polygon and Exit
C     ?        Print this Help Menu
C     A        ABORT marking and exit
C
C --------------------
C
C  VISTA Version 4.2
C  Original Programmer:  Friedel Loinger, Wise Observatory, Israel
C
C  Modification History:
C     Date of Last Revision:  November, 1990
C     Modified for Sun/X11:   May, 1991
C     Slightly Modified by R. Pogge, OSU Astronomy Dept, 1991 May 21
C         Minor re-formatting and removal of standard f77 gotchas,
C         changed command name to MARKPOLY to conform more closely
C         to other interactive TV command syntax, and made sure that
C         the user cannot mark more than the maximum number of vertices,
C         here defined as a parameter passed in common.
C     1991 July 30: fixed bug in double-strike logic [RWP]
C     1992 Feb, 'D' - option deleted by Friedel
C----------------------------------------------------------------------

C     Include statements for VISTA.
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image headers and parameters
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC' ! File information
      INCLUDE 'VINCLUDE:TV.INC'
      INCLUDE 'VINCLUDE:POLYVERT.INC'  ! Polygon Vertices Common
      INTEGER SYS$CANCEL
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/tv.inc'
      INCLUDE 'vistadisk/source/include/polyvert.inc'
#endif

C     Statements for this routine.

      CHARACTER*1  KEY
      INTEGER   ROW, COL, UPPER

C     MARKPOLY uses no keywords and grabs the ID number of the polygon
C     off the command line.

      IPOL = IBUF(1)
      IF (IPOL .LT. 1.OR.IPOL .GT. NMAXPOLY) THEN
         PRINT *,' Polygon number out of range'
         WRITE (*,'(1x,''Valid polygons numbers are 1 through '',I2)') NMAXPOLY
         XERR = .TRUE.
         RETURN
      END IF

C     Make sure there is an image displayed.

      IF (.NOT. TVSTAT) THEN
         PRINT *,'No image is currently displayed'
         XERR = .TRUE.
         RETURN
      END IF

C     Print basic instructions

      WRITE(*,'(1X,''Mark Vertices with the cursor by hitting X'')')
      WRITE(*,'(1X,''Hit M for Mistake ( delete last vertex)'')')
      WRITE(*,'(1X,''Hit E to finish the polygon, A to Abort'')')
      WRITE(*,'(1x,''Hit ? for Help'')')

C*********************************************
C
C  INTERACTIVE ENTRY OF THE POLYGON VERTICES  
C

C     Reset the Vertex counter

      NP = 0

C     Get a keystroke from the user and execute the command

 100  ISTAT = MARK(ROW,COL,KEY)
      L = UPPER(KEY)

C** '?' = Help

      IF (KEY .EQ. '?') THEN
         WRITE(*,'(1X,''*** MARKPOLY Cursor Commands ***'')')
         WRITE(*,'(2X,''X'',8x,''Mark vertex of the polygon'')')
         WRITE(*,'(2X,''M'',8x,''Mistake: Delete last vertex'')')
         WRITE(*,'(2X,''E'',8x,''Finish the Polygon and Exit'')')
         WRITE(*,'(2X,''A'',8x,''ABORT marking and exit'')')
         WRITE(*,'(2X,''?'',8x,''Print this Help menu'',/)')

C** 'E' = Exit polyg

      ELSE IF (KEY .EQ. 'E') THEN
         IF (NP .LT. 3) THEN
            PRINT *,'You have to mark at least 3 points !!!'
            PRINT *,'MARKPOLY Aborting'
            XERR = .TRUE.
            RETURN
         ELSE
            GO TO 200
         END IF

C** 'A' = ABORT without creating a polygon

      ELSE IF (KEY .EQ. 'A') THEN
         PRINT *, 'No Polygon defined: MARKPOLY ABORTS'
         XERR = .TRUE.
         RETURN
       
		
C** 'M' = MISTAKE, delete last vertex

      ELSE IF (KEY .EQ. 'M') THEN
         IF (NP .EQ. 0) THEN
            PRINT *,'You have not selected any points '
         ELSE
            CALL TVCROSS(P_ROWS(NP,IPOL),P_COLS(NP,IPOL))
            NP = NP - 1
         END IF
         GO TO 100

      END IF

      IF (KEY .NE. 'X') GO TO 100

C
C   BOTTOM OF THE CURSOR COMMAND LOOP
C
C***************************************

C     Begin execution of 'X' command.
C
C     Display on the output device the location we have found.
C     Show a header only if this is the first pixel.

      NP = NP + 1
      P_ROWS(NP,IPOL) = ROW
      P_COLS(NP,IPOL) = COL

C     Make sure it is really a new point, and not a double keystroke

      IF (NP .EQ. 1) THEN
         WRITE (olu,'(/2X,''Vertex    Row    Col'',/)')
      ELSE
         IF (P_ROWS(NP,IPOL) .EQ. P_ROWS(NP-1,IPOL) .AND.
     &       P_COLS(NP,IPOL) .EQ. P_COLS(NP-1,IPOL)) THEN
            NP = NP - 1
            GO TO 100
         END IF
      END IF
      WRITE (olu,'(2X,I4,2X,2I7)') NP, ROW, COL

C     Draw an X to mark the vertex

      ISTAT = IMMOVE(COL-2,ROW-2)
      ISTAT = IMDRAW(COL+2,ROW+2,0)
      ISTAT = IMMOVE(COL-2,ROW+2)
      ISTAT = IMDRAW(COL+2,ROW-2,0)

C     Get the next vertex, up to NMAXVERT

      IF (NP .LT. NMAXVERT) GO TO 100

C     Draw the polygon
	
 200  I1 = P_COLS(1,IPOL)
      J1 = P_ROWS(1,IPOL)	
      ISTAT = IMMOVE(I1,J1)
      DO 9001 N = 2, NP
         ISTAT = IMDRAW(P_COLS(N,IPOL),P_ROWS(N,IPOL),0)
 9001 CONTINUE
      ISTAT = IMDRAW(I1,J1,0)

C     Load the vertex counter for this polygon and exit

      NO_P(IPOL) = NP
      RETURN

      END


