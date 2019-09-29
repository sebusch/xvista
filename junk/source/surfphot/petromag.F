       SUBROUTINE PETROMAG

       INCLUDE 'vistadisk/source/include/vistalink.inc'
       INCLUDE 'vistadisk/source/include/profile.inc'
       CHARACTER PARM*8
       LOGICAL KEYCHECK, VERBOSE
       REAL R(NPROFILE), C(NPROFILE), PRAT(NPROFILE*10), RAD(NPROFILE*10)
       REAL PETRO(100)

       CALL KEYINIT
       CALL KEYDEF('RLIM=')
       CALL KEYDEF('NP=')
       CALL KEYDEF('VERBOSE')

       IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
       END IF

       RLIM = 0.2
       NP = 2
       IPRFCOL = 1
       VERBOSE = .FALSE.
       DO 5501 I=1,NCON
         IF (WORD(I)(1:5) .EQ. 'RLIM=') THEN
           CALL ASSIGN(WORD(I),RLIM,PARM)
           IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'NP=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           NP = NINT(TMP)
         ELSE IF (WORD(I)(1:4) .EQ. 'COL=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           IPRFCOL = NINT(TMP)
         ELSE IF (WORD(I) .EQ. 'VERBOSE') THEN
           VERBOSE = .TRUE.
         END IF

5501   CONTINUE

C       Exit if no profile has been calculated.
        IF (NPRF(IPRFCOL) .LE. 0) THEN
          PRINT *, 'No galaxy profile has been calculated...'
          XERR    =.TRUE.
          RETURN
        END IF
        DO IPRF=1,NPRF(IPRFCOL)
          R(IPRF) = IPRF-1
          C(IPRF) = PARAMPRF(17,IPRF,IPRFCOL)
        END DO

        CL=0.
        PI = 3.14159
        TWOPI = 2*PI
C    Calculate every tenth of a pixel
        DO IPRF=10,NPRF(IPRFCOL)*10
          RAD(IPRF) = (IPRF-1)/10.
          R1 = 0.8*RAD(IPRF)
          R2 = MIN(NINT(1.25*RAD(IPRF)),NPRF(IPRFCOL))
          CALL LININT(R,C,NPRF(IPRFCOL),RAD(IPRF),C0)
          CALL LININT(R,C,NPRF(IPRFCOL),R1,C1)
          CALL LININT(R,C,NPRF(IPRFCOL),R2,C2)
          SUM1 = (C2-C1)/PI/RAD(IPRF)**2/(1.25**2-0.8**2)
          SUM2 = C0/PI/RAD(IPRF)**2
          PRAT(IPRF) = SUM1/SUM2
          IF (VERBOSE .AND. MOD(IPRF,10) .EQ. 0) PRINT *, RAD(IPRF), PRAT(IPRF), C0, C1, C2
        END DO

        NPETRO = 0
        DO IPRF=11,NPRF(IPRFCOL)*10
          IF (PRAT(IPRF) .LT. RLIM .AND. PRAT(IPRF-1) .GT. RLIM) THEN
            NPETRO = NPETRO+1
            CALL LININT(PRAT(IPRF-1),RAD(IPRF-1),2,RLIM,PETRO(NPETRO))
          END IF
        END DO

        PRINT *, '     NPETRO  PETRORAD  PETROMAG(NP*PETRORAD)'
        DO IPETRO = 1, NPETRO
          IF (NP*PETRO(IPETRO) .LT. R(NPRF(IPRFCOL))) THEN
            CALL LININT(R,C,NPRF(IPRFCOL),NP*PETRO(IPETRO),PMAG)
          ELSE
            PRINT *, 'Petrosian radius larger than profile calc, taking max', nprf(iprfcol)
            PMAG = C(NPRF(IPRFCOL))
          END IF
          PRINT 101, IPETRO, PETRO(IPETRO), PMAG
101       FORMAT(I10,2(1PE12.3))
        END DO

        IF (NPETRO .GT. 0) THEN
          CALL VARIABLE('PETRORAD',PETRO(NPETRO),.TRUE.)
          CALL VARIABLE('PETROMAG',PMAG,.TRUE.)
          PRINT *, 'largest PETRORAD loaded into PETRORAD'
          PRINT *, 'PETROMAG of largest PETRORAD loaded into PETROMAG'
        ELSE
          PRINT *, 'No Petrosian radius found!!'
          CALL VARIABLE('PETRORAD',-1.,.TRUE.)
          CALL VARIABLE('PETROMAG',-1.,.TRUE.)
        END IF

	DO IPRF=10,NPRF(IPRFCOL)-1
	  IF (VERBOSE) print *, iprf, r(iprf), c(iprf), c(iprf)/pmag, pmag
          IF (C(IPRF)/PMAG .LT. 0.5 .AND. C(IPRF+1)/PMAG .GE. 0.5) THEN
            R50 = R(IPRF) + (R(IPRF+1)-R(IPRF))*(0.5-C(IPRF)/PMAG)/(C(IPRF+1)/PMAG-C(IPRF)/PMAG)
          END IF
          IF (C(IPRF)/PMAG .LT. 0.8 .AND. C(IPRF+1)/PMAG .GE. 0.8) THEN
            R80 = R(IPRF) + (R(IPRF+1)-R(IPRF))*(0.8-C(IPRF)/PMAG)/(C(IPRF+1)/PMAG-C(IPRF)/PMAG)
          END IF
          CALL VARIABLE('PETROR50',R50,.TRUE.)
          CALL VARIABLE('PETROR80',R80,.TRUE.)
 	END DO

        RETURN
        END
