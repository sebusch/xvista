#include "Vista.h"
        SUBROUTINE SUBVAR(LET,NS,F,SET)

C       Routine to transfer subscripted variables

C               LET     =Variable name
C               NS      =Subscript number
C               F       =Value
C               SET     =.TRUE. to write, .FALSE. to read

C       Author: Tod R. Lauer    11/29/82

        CHARACTER *(*) LET
        CHARACTER*8 VAR, FSTRCAT
        CHARACTER*1 N1,N2,N3,N4
        LOGICAL SET

C       Convert numeric subscript to characters

	I0 = ICHAR('0')
        IF (NS .GE. 10000) THEN
                PRINT *, 'ERROR in SUBVAR: cant create variable'
                RETURN
        ELSE IF (NS .GE. 1000) THEN
                N1      =CHAR(NS/1000+I0)
                IS      =NS-1000*(NS/1000)
                N2      =CHAR(IS/100+I0)
                IS      =IS-100*(IS/100)
                N3      =CHAR(IS/10+I0)
                IS      =IS-10*(IS/10)
                N4      =CHAR(IS+I0)
        ELSE IF (NS .GE. 100) THEN
                N1      =CHAR(NS/100+I0)
                IS      =NS-100*(NS/100)
                N2      =CHAR(IS/10+I0)
                IS      =IS-10*(IS/10)
                N3      =CHAR(IS+I0)
                N4      =' '
        ELSE IF (NS .GE. 10) THEN
                N1      =CHAR(NS/10+I0)
                IS      =NS-10*(NS/10)
                N2      =CHAR(IS+I0)
                N3      =' '
                N4      =' '
        ELSE
                N1      =CHAR(NS+I0)
                N2      =' '
                N3      =' '
                N4      =' '
        END IF

C      Tack onto main name.
        VAR     = FSTRCAT(LET,FSTRCAT(N1,FSTRCAT(N2,FSTRCAT(N3,N4))))

        CALL VARIABLE(VAR,F,SET)
        RETURN
        END
