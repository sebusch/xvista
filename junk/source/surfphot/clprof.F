#include "Vista.h"
        SUBROUTINE CLPROF

C       Routine to clear out the profile common block.  This is
C       necessary when .gpr files of different sizes are being
C       read in and altered one after the other, and the possibility
C       exists that a larger file will partially contaminate a
C       smaller one which is overwriting it.

C       Author: J. Willick, 10/23/87

C       Update 3/24/88: user has option of specifying a radius (in arcsec)
C       beyond which he or she wishes to clear the profile common block.
C       This is desirable if a GPROF calculation is seen to be good only
C       to some radius, beyond which it goes awry. -JW

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:profile.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif
        CHARACTER*8 PARM
        LOGICAL KEYCHECK

        CALL KEYINIT
        CALL KEYDEF('RAD=')
        CALL KEYDEF('COL=')
        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

        J1=1
        IPRFCOL = 1
        DO 10 I=1,NCON
                IF (WORD(I)(1:4) .EQ. 'COL=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        IPRFCOL= NINT(F)
                END IF
10      CONTINUE
        DO 11 I=1,NCON
                IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        J1=NINT(1.+F/PRFSC(IPRFCOL))
                END IF
11      CONTINUE

        DO 101 I=1,NPRFPARAM
                DO 102 J=J1,NPROFILE
                        PARAMPRF(I,J,IPRFCOL)=0.
102             CONTINUE
101     CONTINUE

        NPRF(IPRFCOL)=J1-1
        RETURN
        END
