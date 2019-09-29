
      LOGICAL FUNCTION BITTEST ( INUM, IBIT )

C
C    VISTA 4.0 Utility Function
C
C    Reads the value of bit IBIT in the integer INUM, and
C    returns .TRUE. or .FALSE.
C
C    R Pogge
C    Lick Observatory
C    1988 July 25
C
C    Used primarily by VMS tape routines to read status information
C    sensed from tapes
C

      INTEGER INUM, IBIT, IBIN

      IBIN = 2**IBIT

      BITTEST = .FALSE.

      IF ((INUM .AND. IBIN) .NE. 0) BITTEST = .TRUE.

      RETURN
      
      END


	LOGICAL FUNCTION UNITCHECK(NUNIT)

	PARAMETER (NTAPES=3)

	IF((NUNIT .LT. 0) .OR. (NUNIT .GT. NTAPES-1)) THEN
		WRITE(*,100) NTAPES-1
100		FORMAT(' Tape units 0 through',I2,' only')
		UNITCHECK = .FALSE.
	ELSE
		UNITCHECK = .TRUE.
	END IF
	RETURN
	END

