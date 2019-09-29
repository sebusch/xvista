#include "Vista.h"
        SUBROUTINE SETPOLY(HEAD,WAVE,PIXEL,CENTER,W0,NORD)

C       Save WSCALE results in header buffer and working arrays

C       Written by:     Richard Stover          1984
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064
C
C       Modified by:    April Atwood            6/12/86
C
#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

        CHARACTER*(*) HEAD
        DOUBLE PRECISION WAVE(MAXPOLY), PIXEL(MAXPOLY), CENTER, W0
        CHARACTER*68 JSTRG

C       Input:  HEAD    FITS character header array to put entries into
C               WAVE    Coefficients giving wavelength as a function of pixel
C               PIXEL   Coefficients giving pixel as a function of wavelength
C               CENTER  Central pixel for use in polynomial
C               NORD    Number of polynomial coefficients

C       Output: Various entries inserted into HEAD

        CALL CHEADSET('CTYPE1','POLY_LAMBDA',HEAD)
        CALL INHEADSET('LAMORD',NORD,HEAD)

        I = NORD
        IF(I .GT. 3) I=3

C       The polynomial coeffiecients are saved as character strings since
C       FITS has no standard way to specify a polynomial coordinate scale

        WRITE(JSTRG,907) CENTER, (WAVE(L),L=1,I)
        CALL CHEADSET('LPOLY0',JSTRG,HEAD)
C        WRITE(JSTRG,907) WAVE(1), (PIXEL(L),L=1,I)
        WRITE(JSTRG,907) W0, (PIXEL(L),L=1,I)
        CALL CHEADSET('IPOLY0',JSTRG,HEAD)
        IF(NORD .GT. I) THEN
                I = I+1
                WRITE(JSTRG,907) (WAVE(L),L=I,NORD)
                CALL CHEADSET('LPOLY1',JSTRG,HEAD)
                WRITE(JSTRG,907) (PIXEL(L),L=I,NORD)
                CALL CHEADSET('IPOLY1',JSTRG,HEAD)
        ELSE
C            Eliminate any previous unneeded coefficients
                CALL UNFIT('LPOLY1', HEAD)
                CALL UNFIT('IPOLY1', HEAD)
        END IF
907     FORMAT(4(1PE15.7,1X))

        RETURN
        END
