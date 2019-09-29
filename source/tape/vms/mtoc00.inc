C
C   MTOC00.INC
C
C   Include File for MTPCKG, VMS Tape Driver Routines
C
C   VISTA Version 4.0 Compatibility  -  1988 July 24  [RWP]
C
C   WARNING:  These routine only work on VMS systems
C
C  Changes:
C
C      Renamed as MTOC00.INC
C
C-----------------------------------------------------------------------------

      PARAMETER MTCMAX = 8
      INTEGER MTVMST
      DOUBLE PRECISION MTIOSB(MTCMAX)
      INTEGER MTCHAN(MTCMAX), MTEFNO(MTCMAX), MTSTAT(MTCMAX),
     1        MTWTRT(MTCMAX), MTWTVA(MTCMAX), MTWTOP(MTCMAX)
      LOGICAL*1 MTOPND(MTCMAX), MTNOER(MTCMAX), MTUSED(MTCMAX),
     1          MTWTMD(MTCMAX)
      CHARACTER*6 MTPNAM(MTCMAX)
      COMMON /MTOC00/ MTIOSB, MTCHAN, MTEFNO, MTSTAT, MTWTRT, MTWTVA,
     1                MTWTOP, MTOPND, MTNOER, MTUSED, MTWTMD
      COMMON /MTOC01/ MTPNAM
      COMMON /MTOC02/ MTVMST
C
C     MTVMST added KS 22nd Sept 1982
C
