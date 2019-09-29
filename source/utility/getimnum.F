#include "Vista.h"
        SUBROUTINE GETIMNUM(IMNUM,N)

C       Routine to get image specifiers in a VISTA command line.
C       Input:  N       Look for Nth image on command line
C       Output: IMNUM   Image buffer number

C       Image numbers are contained in the IBUF common block.  Image
C       numbers may be specified either as explicit integers or
C       with variables in the '$' construct.

C       Author:         Donald Terndrup   3/11/85


#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C        Number of image specifiers ignored
        COMMON /IMNSP/ IMSP

C       Find the image number.

        IF (N .LE. 0 .OR. N .GT. NCON) THEN
                PRINT *,
     &           'Programming error.  Asked for image in position ',N
                PRINT *,'There are only ',NCON,' positions.'
                XERR = .TRUE.
                RETURN
        END IF

C       See if the image specifier was given on the command line. (RJS 5/20/86)

        JBUF = N + IMSP
        IF (JBUF .GT. NINTS) THEN
                PRINT *,'No image specifier on command line.'
                XERR = .TRUE.
                RETURN
        END IF

        IMNUM = IBUF(JBUF)

C       If the number is out of range, return with an error message.

        IF (IMNUM .LT. 1 .OR. IMNUM .GT. MAXIM) THEN
                PRINT *,'Image : ',IMNUM
                PRINT *,'Image specifier out of range.'
                XERR = .TRUE.

        END IF

        RETURN
        END
