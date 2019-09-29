      SUBROUTINE WRTLPACC
C
C     This routine is called to issue a page credit
C     to the caller.  This needs to be
C     done in plots because each dot line is terminated
C     by a CR instead of each character line.
C     Call CALCLPACC for each plot generated.
C
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
      IMPLICIT INTEGER*4 (S)
      EXTERNAL    ACC$K_INSMESG
      INTEGER*2 BUFFER(9)
      INTEGER*4 DESC(2)
      INTEGER*4 IB(3)
      EQUIVALENCE (BUFFER(4),IB)
      DATA BUFFER/0,357,7*0/
      DATA NPAGES,NCHARGE,NTRUE/700,768,64/
      BUFFER(1) = %LOC(ACC$K_INSMESG)
      IB(1) = NPAGES
      IB(2) = NCHARGE
      IB(3) = NTRUE
      DESC(1) = 18
      DESC(2) = %LOC(BUFFER)
      CALL IFERR(SYS$SNDACC(DESC,),
     .   'WRTLPACC UNABLE TO WRITE ACCOUNTING MESSAGE')
C      TYPE *,'GAVE YOU A PAGE CREDIT OF',IB(1),' PAGES'
      RETURN
      END
