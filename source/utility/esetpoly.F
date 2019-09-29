#include "Vista.h"
        SUBROUTINE ESETPOLY(HEAD, WAVE, ORDX, ORDM, MAX, X0)

C       Save EWAVE results in header buffer and working arrays

C       Input:  HEAD    FITS character header array to put entries into
C               WAVE    Coefficients giving wavelength as a function of pixel
C               ORDX    Order of the fit in columns
C               ORDM    Order of the fit in 1/order-number
C               MAX     Maximum order of fit allowed (zero-based)
C               X0      Column offset (x-offset) from nominal pattern center

C       Output: Various entries inserted into HEAD

C       Written by:     April Atwood    2/87
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064
C

        CHARACTER*(*) HEAD
        INTEGER ORDX, ORDM, MAX
C                                       ! Zero based
        DOUBLE PRECISION WAVE((MAX+1)*(MAX+1)), X0
        CHARACTER JSTRG*68, EPOLY*8

        CALL CHEADSET('CTYPE1','ECHELLE_LAMBDA',HEAD)

C       Load ORDX and ORDM, the fit orders in column and order space.
C       These are put into integer cards in HEAD.

        CALL INHEADSET('XORDER', ORDX, HEAD)
        CALL INHEADSET('MORDER', ORDM, HEAD)

C       Load the x-offset X0 into a double precision card.  Note that
C       this is a user-input variable, and is not actually fit.

        CALL FHEADSET('XOFFSET', X0, HEAD)

C       The polynomial coefficients are saved as character strings since
C       FITS has no standard way to specify a polynomial coordinate scale

        NP = (ORDX+1)*(ORDM+1)
        NPMAX = (MAX+1)*(MAX+1)
        IF (NP .GT. NPMAX) NP = NPMAX

C       The coefficients are written on cards titled EPOLY0, EPOLY1, EPOLY2,
C       etc. with a maximum of two coefficients per card.

        DO 8701 I = 1, NP, 2
                EPOLY = 'EPOLY'
                K = I/2
                IF (K .LT. 10) THEN
                        WRITE (EPOLY, 70) K
70                      FORMAT('EPOLY',I1)
                ELSE
                        WRITE (EPOLY, 71) K
71                      FORMAT('EPOLY',I2)
                END IF

C       Put two numbers on each card, if possible.

                IF (I .EQ. NP) THEN
                        WRITE (JSTRG, 907) WAVE(I)
907                     FORMAT(1PE22.15)
                ELSE
                        WRITE (JSTRG, 908) WAVE(I), WAVE(I+1)
908                     FORMAT(2(1PE22.15,1X))
                END IF

                CALL CHEADSET(EPOLY, JSTRG, HEAD)
8701    CONTINUE

        RETURN
        END


        SUBROUTINE EGETPOLY(HEAD, COEF, NX, NM, X0)

C       Get the echelle wavelength scale from a header.

C       Input:  HEAD    FITS character header array to get entries from

C       Output: COEF    Coefficients giving wavelength as a function of pixel
C               NX      Order of the fit in columns
C               NM      Order of the fit in 1/order-number
C               X0      Column offset (x-offset) from nominal pattern center

C       Written by:     bf              3/87
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

        PARAMETER (MAX=4)
        CHARACTER*(*) HEAD
        DOUBLE PRECISION COEF((MAX+1)*(MAX+1)), X0, FNUM
        CHARACTER ESTRING*80, EPOLY*8, PARM*40, FORMSTR*64

C       Read NX, NM, and X0 from header cards.

        NX = INHEAD('XORDER', HEAD)
        NM = INHEAD('MORDER', HEAD)
        X0 = FHEAD('XOFFSET', HEAD)

C       Read coefficient values from header cards.

        NP = (NX+1)*(NM+1)
        DO 8702 I = 1, NP, 2
                EPOLY = 'EPOLY'
                K = I/2
                IF (K .LT. 10) THEN
                        WRITE (EPOLY, 20) K
20                      FORMAT('EPOLY',I1)
                ELSE
                        WRITE (EPOLY, 25) K
25                      FORMAT('EPOLY',I2)
                END IF
                CALL CHEAD(EPOLY, HEAD, ESTRING)
                IF (ESTRING .NE. ' ') THEN

C       Get first number off card

                        CALL DISSECT(ESTRING, 1, .FALSE., NTYPE, NUM,
     .                          FNUM, PARM, NC, XERR)
                        FORMSTR = ' '
                        WRITE(FORMSTR(1:),30) NC,NC-7
30                      FORMAT('(D',I12.12,'.',I12.12,')')
                        READ (PARM, FORMSTR) COEF(I)

C       Get second number, if there is one

                        IF (I .NE. NP) THEN
                                CALL DISSECT(ESTRING,2,.FALSE.,NTYPE,
     .                                  NUM, FNUM, PARM, NC, XERR)
                                WRITE(FORMSTR,30) NC,NC-7
                                READ (PARM, FORMSTR) COEF(I+1)
                        END IF
                END IF
8702    CONTINUE

        RETURN
        END
