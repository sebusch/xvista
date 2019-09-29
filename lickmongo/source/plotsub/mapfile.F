#include "Config.h"
C===============================================================================
C===============================================================================
      INTEGER FUNCTION PTUV(MAPFILE)(FILENAME,ACCESS,IBYTES,IADDR)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
      IMPLICIT INTEGER*4 (S)
      INTEGER MAPOLD,MAPNEW
      INTEGER IPAGES(2)
      CHARACTER*(*) FILENAME,ACCESS
      CHARACTER*1 LACC
      PARAMETER MAXLUN=50
      COMMON/MAP_COM/ICHANS(MAXLUN),IADDRS(2,MAXLUN)
      LOGICAL ERROR
      EXTERNAL SEC$M_DZRO,SEC$M_WRT,SEC$M_EXPREG
      ERROR(I) = MOD(I,4).NE.1
      LBYTES = IBYTES
      LACC = ACCESS
      CALL PTUV(CUPPER)(LACC)
      DO 20 I = 1 , MAXLUN
	 J = I
	 IF(ICHANS(I).EQ.0)GOTO 25
20    CONTINUE
      STOP 'NO MORE SPACE IN CHANNEL TABLE'
25    CONTINUE
C     Round up to next page in size
      LPAGES = (LBYTES+511)/512
D     TYPE *,'ABOUT TO OPEN FILE'
      PTUV(MAPFILE) = MEM_OPEN(FILENAME,LACC,LPAGES,ICHANS(J))
D     TYPE *,'J + ICHANS on OPEN',J,ICHANS(J)
D     TYPE *,'LPAGES',LPAGES
      IF(ERROR(PTUV(MAPFILE)))THEN
	 ICHANS(J) = 0
	 RETURN
      END IF
      IFLAGS = %LOC(SEC$M_WRT)
      IF(LACC.EQ.'R')IFLAGS = 0
      IF(LACC.EQ.'N')IFLAGS = IFLAGS.OR.%LOC(SEC$M_DZRO)
      IFLAGS = IFLAGS.OR.%LOC(SEC$M_EXPREG)
      IPAGES(1) = 0
      IPAGES(2) = 0
D     TYPE *,'ABOUT TO DO MAPPING'
      PTUV(MAPFILE) = SYS$CRMPSC(IPAGES,IADDRS(1,J),,%VAL(IFLAGS),
     &   ,,,%VAL(ICHANS(J)),
     &   ,,,)
      IF(ERROR(PTUV(MAPFILE)))THEN
	IERR=SYS$DASSGN(%VAL(ICHANS(J)))
	 RETURN
      END IF
      IF(LACC.NE.'N')IBYTES = 512 * LPAGES
      IADDR = IADDRS(1,J)
      RETURN
      END
C===============================================================================
C===============================================================================
      INTEGER FUNCTION PTUV(UNMAP)(IADDR)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
      IMPLICIT INTEGER*4 (S)
      INTEGER*4 IPAGES(2)
      PARAMETER MAXLUN=50
      COMMON/MAP_COM/ICHANS(MAXLUN),IADDRS(2,MAXLUN)
      LOGICAL ERROR
      ERROR(I) = MOD(I,4).NE.1
      DO 10 I = 1 , MAXLUN
	 J = I
	 IF((IADDRS(1,J).LE.IADDR).AND.(IADDRS(2,J)
     &      .GE.IADDR))GOTO 15
10    CONTINUE
      STOP 'ATTEMPT TO UNMAP A REGION WHICH WAS NEVER MAPPED'
15    CONTINUE
D     TYPE *,'FOUND ENTRY'
D     TYPE *,'ABOUT TO DO UNMAP'
      PTUV(UNMAP) = SYS$DELTVA(IADDRS(1,J),,)
      IF(ERROR(PTUV(UNMAP)))RETURN
C     close the disk file
D     TYPE *,'J + ICHANS before CLOSE',J,ICHANS(J)
D     TYPE *,'ABOUT TO CLOSE FILE'
      PTUV(UNMAP) = SYS$DASSGN(%VAL(ICHANS(J)))
      IF(ERROR(PTUV(UNMAP)))RETURN
      ICHANS(J) = 0
      IADDRS(1,J) = 0
      IADDRS(2,J) = 0
      RETURN
      END
C===============================================================================
C===============================================================================
	SUBROUTINE PTUV(Cupper)(str)

!-- cupper.for
!-- Convert any lowercase characters to uppercase.  Leave other ASCII codes
!-- alone.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	  CHARACTER*(*) str

	  INTEGER       a, z, bigA, bigZ, delta
	  PARAMETER     (a=ICHAR('a'), z=ICHAR('z'))
	  PARAMETER     (bigA=ICHAR('A'), bigZ=ICHAR('Z'))
	  PARAMETER     (delta=a-bigA)

	  INTEGER       i, k, L

	  i = 1
	  L = LEN(str)
	  DO WHILE ((i .LE. L) .AND. (ICHAR(str(i:i)) .NE. 0))
	    k = ICHAR(str(i:i))
	    IF ((k .GE. a) .AND. (k .LE. z)) k = k - delta
	    str(i:i) = CHAR(k)
	    i = i + 1
	  ENDDO
	END
C===============================================================================
C===============================================================================
