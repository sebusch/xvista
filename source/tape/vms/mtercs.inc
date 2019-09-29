C
C   MTERCS.INC
C
C   Error Codes for MTPCKG, VMS Tape Driver Routines
C
C   VISTA Version 4.0 Compatibility  -  1988 July 28  [RWP]
C
C   WARNING:  These routine only work on VMS systems
C
C  Changes:
C
C      Renamed as MTERCS.INC
C
C      Added MTREOV to indicate EOV condition on MTSKFI
C      placed in spot previously marked 'UNUSED'
C
C-----------------------------------------------------------------------------


C----------  ERROR CODES FOR 'MTOPEN'  ----------

      PARAMETER MTRNDV = 1             ! NO AVAILABLE TAPE DRIVE
      PARAMETER MTRNCH = 2             ! NO AVAILABLE I/O CHANNEL
      PARAMETER MTRNEF = 3             ! NO AVAILABLE EVENT FLAG
      PARAMETER MTRUNX = 7             ! UNEXPECTED FAILURE

C----------  ERROR CODES FOR 'MTOPID'  ----------

C     PARAMETER MTRNDV = 1             ! NO AVAILABLE TAPE DRIVE
C     PARAMETER MTROFL = 2             ! TAPE OFF LINE
C     PARAMETER MTRNEF = 3             ! NO AVAILABLE EVENT FLAG
      PARAMETER MTRNMT = 4             ! TAPE NOT MOUNTED
      PARAMETER MTRALL = 5             ! TAPE ALREADY ALLOCATED
      PARAMETER MTRIDV = 6             ! INVALID DEVICE NAME
C     PARAMETER MTRUNX = 7             ! UNEXPECTED FAILURE

C----------  ERROR CODES FOR 'MTCLOS'  ----------
C----------  ERROR CODES FOR 'MTNOOP'  ----------
C                        (NONE)

C----------  ERROR CODES FOR 'MTSERM'  ----------

      PARAMETER MTRERU = 1             ! ERROR MODE UNCHANGED

C----------  ERROR CODES FOR 'MTNAME'  ----------

      PARAMETER MTRTRN = 1             ! TRUNCATED NAME

C----------  ERROR CODES FOR 'MTREAD'  ----------
C----------  ERROR CODES FOR 'MTSKRE'  ----------

      PARAMETER MTRERR = 1             ! I/O ERROR
      PARAMETER MTROFL = 2             ! TAPE OFFLINE
      PARAMETER MTREOF = 3             ! END OF FILE
      PARAMETER MTREOT = 4             ! END OF TAPE
      PARAMETER MTRNOR = 5             ! NO RECORD

C----------  ERROR CODES FOR 'MTRDBK'  ----------

C     PARAMETER MTRERR = 1             ! I/O ERROR
C     PARAMETER MTROFL = 2             ! TAPE OFFLINE
C     PARAMETER MTREOF = 3             ! END OF FILE

C----------  ERROR CODES FOR 'MTWRIT'  ----------
C----------  ERROR CODES FOR 'MTWEOF'  ----------

C     PARAMETER MTRERR = 1             ! I/O ERROR
C     PARAMETER MTROFL = 2             ! TAPE OFFLINE
      PARAMETER MTRWLK = 3             ! WRITE LOCK
C     PARAMETER MTREOT = 4             ! END OF TAPE

C----------  ERROR CODES FOR 'MTSKFI'  ----------

C     PARAMETER MTRERR = 1             ! I/O ERROR
C     PARAMETER MTROFL = 2             ! TAPE OFFLINE
      PARAMETER MTREOV = 3             ! END OF VOLUME
C     PARAMETER MTREOT = 4             ! END OF TAPE
C     PARAMETER MTRNOR = 5             ! NO RECORD

C----------  ERROR CODES FOR 'MTREWI'  ----------
C----------  ERROR CODES FOR 'MTUNLD'  ----------

C     PARAMETER MTRERR = 1             ! I/O ERROR
C     PARAMETER MTROFL = 2             ! TAPE OFFLINE

C----------  ERROR CODES FOR 'MTPREP'  ----------

C     PARAMETER MTRERR = 1             ! I/O ERROR
C     PARAMETER MTROFL = 2             ! TAPE OFFLINE
      PARAMETER MTRBDN = 3             ! ILLEGAL DENSITY

C----------  ERROR CODES FOR 'MTSENS'  ----------

C     PARAMETER MTRERR = 1             ! I/O ERROR
C     PARAMETER MTROFL = 2             ! TAPE OFFLINE
      PARAMETER MTRVIN = 3             ! VOLUME INVALID

C----------  ERROR CODES FOR 'MTSETW'  ----------

      PARAMETER MTRWMU = 1             ! WAIT MODE UNCHANGED

C----------  ERROR CODES FOR 'MTWAIT'  ----------
C----------  ERROR CODES FOR 'MTCHCK'  ----------
C     AS FOR THE CORRESPONDING FUNCTION, ABOVE.

C----------  ERROR CODES FOR 'MTGETN'  ----------

C     PARAMETER MTRNDV = 1             ! NO AVAILABLE DRIVE 
      PARAMETER MTRNTR = 2             ! CANNOT TRANSLATE SYS$TAPES

C----------  ERROR CODES FOR 'MTMOUN'  ----------

C     PARAMETER MTROFL = 2             ! TAPE OFFLINE
C     PARAMETER MTRNMT = 4             ! TAPE NOT MOUNTED
C     PARAMETER MTRALL = 5             ! TAPE ALREADY ALLOCATED
C     PARAMETER MTRIDV = 6             ! ILLEGAL DEVICE NAME
C     PARAMETER MTRUNX = 7             ! UNEXPECTED FAILURE 
C
C     Most routines using system service calls can return
C     the MTRUNX error code, in exceptional circumstances.
C
C                                      KS / CIT 6th Oct 1982
C
C----------------------------------------------------
