C
C   MTSB00.INC
C
C     I/O Status block definition for MTPCKG, VMS Tape Driver Routines
C
C   VISTA Version 4.0 Compatibility  -  1988 July 24 [RWP]
C
C   WARNING:  These routine only work on VMS systems
C
C  Changes:
C
C      Renamed as MTSB00.INC
C
C-----------------------------------------------------------------------------


      DOUBLE PRECISION IOSD
      INTEGER IOSB(2)
      EQUIVALENCE (IOSB, IOSD)
      INTEGER*2 IOBCST(2)
      EQUIVALENCE (IOBCST(1), IOSB(1))
      INTEGER*2 IOSB_STATUS, IOSB_COUNT
      EQUIVALENCE (IOSB_STATUS, IOBCST(1)), (IOSB_COUNT, IOBCST(2))
      COMMON /IOSB00/ IOSD
