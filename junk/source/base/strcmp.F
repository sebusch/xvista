	SUBROUTINE STRCMP

	include 'vistadisk/source/include/vistalink.inc'
	LOGICAL SILENT
	CHARACTER TWORD*80
        INTEGER UPPER

	SILENT = .FALSE.
	CALL KEYINIT
	CALL KEYDEF('SILENT')
 
        NKEY = 0
	DO I=1,NWORD
          TWORD = WORD(I)
          L = UPPER(TWORD)
	  IF (TWORD .EQ. 'SILENT') THEN
            SILENT = .TRUE.
            NKEY = NKEY+1
          END IF
        END DO
 
	IF (NWORD-NKEY .NE. 2) THEN
	  PRINT *, ' There must be 2 words on the command line to compare'
	  XERR = .TRUE.
	  RETURN
	END IF

	L1 = NUMCHAR(WORD(1))
	L2 = NUMCHAR(WORD(2))

	IF (WORD(1)(1:L1) .EQ. WORD(2)(1:L2)) THEN
          STRCMPOK = 1.
        ELSE
          STRCMPOK = 0.
	END IF
        CALL VARIABLE('STRCMPOK',STRCMPOK,.TRUE.)

        IF (.NOT. SILENT) THEN
          IF (STRCMPOK .EQ. 0.) THEN
            PRINT *, 'Strings differ, STRCMPOK set to 0'
          ELSE
            PRINT *, 'Strings are the same, STRCMPOK set to 1'
          END IF
        END IF
     
	RETURN
	END
