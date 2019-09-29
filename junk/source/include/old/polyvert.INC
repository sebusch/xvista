#include "Vista.h"

C  
C   This file defines the POLYVERT common block used to save the 
C   vertices of user-defined polygonal boxes.  
C   "INCLUDEd" by the source code files:
C     MARKPOLY.F, POLYSUBS.F, ANLBOX.F and CLIP.F
C
C  Adapted from the POLIG code by Loinger Friedel, Wise Observatory, Israel.
C  R. Pogge, OSU Astronomy, 1991 May 21
C

C   Maximum number of user-defined polygonal boxes

      INTEGER   NMAXPOLY
      PARAMETER (NMAXPOLY = 10)

C   Maximum number of vertices allowed

      INTEGER   NMAXVERT
      PARAMETER (NMAXVERT = 99)

C   Number of vertices of a given polygonal box

      INTEGER   NO_P(NMAXPOLY)

C   Image coordinates of the vertices  (add 1 to allow closure)

      INTEGER   P_ROWS(NMAXVERT+1,NMAXPOLY)
      INTEGER   P_COLS(NMAXVERT+1,NMAXPOLY)

      COMMON /POLYVERT/ NO_P, P_ROWS, P_COLS
