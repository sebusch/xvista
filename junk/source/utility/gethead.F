#include "Vista.h"

      INTEGER FUNCTION INHEAD(REQUEST,HEADBUF)

C
C  INHEAD - Return integer values from the image headers.
C
C  The parameters describing an image array, its conditions
C  of observation, reduction, and the like, are stored
C  in a character string header.  These subroutines are used
C  to return the values of these parameters to other VISTA
C  subroutines by searching for the value or character
C  string associated with the desired keyword.
C
C  INPUTS:
C     REQUEST         Alphanumeric keyword to search for
C     HEADBUF         The VISTA character header buffer
C
C  Author: 
C     Tod R. Lauer    1/26/83
C
C  Modified by 
C     R. STOVER  August 31, 1987 to DISSECT the
C     cards only if the PARAM variable matches the parameter
C     field of the FITS card.  This saves an enormous amount
C     of cpu time by eliminating unnecessary parsing.
C
C---------------------------------------------------------------------------

      CHARACTER*8 PARAM
      CHARACTER*80 INLINE, WORD1
      CHARACTER*(*) HEADBUF, REQUEST
      CHARACTER*8 EQUIVREQUEST, EQUIVREQUEST2
      REAL*8 FNUM
C  How many cards?

      NC = LEN(HEADBUF)/80
      INHEAD = 0

C  Many of the keywords in the FITS headers at Lick Observatory
C  are non-standard.  We load into EQUIVREQUEST various standard
C  names corresponding to the Lick names.  This table of equivalent
C  names contains only those that might be expressed as an integer.
C  An analogous list may be found below in the routine CHEAD.
      
      IF (REQUEST .EQ. 'EXPOSURE') THEN
         EQUIVREQUEST  = 'EXPTIME '            
         EQUIVREQUEST2 = 'ITIME   '
      ELSE IF (REQUEST .EQ. 'TIME') THEN
         EQUIVREQUEST  = 'UT      '
         EQUIVREQUEST2 = 'TIME-OBS'
      ELSE
         EQUIVREQUEST  = REQUEST
         EQUIVREQUEST2 = REQUEST
      END IF

C  Each header record consists of 'NC' card images of 80 columns each.
C  The first thing on the card is a parameter name, which we extract
C  and place into PARAM.  Then we use DISSECT to extract the para-
C  meter value, which may be numeric or alpha-numeric depending on
C  the parameter.

      DO 8701 N=1, NC
         J = (N-1)*80+1
         PARAM = HEADBUF(J:J+7)
         IF (PARAM .EQ. 'END') GO TO 50
         IF(PARAM .EQ. REQUEST .OR. 
     &      PARAM .EQ. EQUIVREQUEST .OR.
     &      PARAM .EQ. EQUIVREQUEST2 ) THEN
            INLINE(1:80) = HEADBUF(J:J+79)
            CALL DISSECT(INLINE(11:80),1,.FALSE.,NTYPE,NUM,
     &                   FNUM,WORD1,NCHAR,OERR)
            IF (NTYPE .LE. 2) THEN
               INHEAD = NUM
               RETURN
            END IF
         END IF
 8701 CONTINUE
      
 50   RETURN
      END

C---------------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION FHEAD(REQUEST,HEADBUF)

C  FHEAD works like INHEAD, except that it returns floating values.
C  In some cases, such as for celestial coordinates, this routine
C  will interpret the character string specifying the individual
C  values of the components of the coordinates.

      CHARACTER*8 PARAM
      CHARACTER*80 INLINE, WORD1
      CHARACTER*(*) HEADBUF, REQUEST
      CHARACTER*8  EQUIVREQUEST, EQUIVREQUEST2
      REAL*8 FNUM

C  Check a restricted list of aliases for certain header cards

      IF (REQUEST .EQ. 'EXPOSURE') THEN
         EQUIVREQUEST  = 'ITIME   '
         EQUIVREQUEST2 = 'EXPTIME '
      ELSE IF (REQUEST .EQ. 'TIME') THEN
         EQUIVREQUEST  = 'UT      '
         EQUIVREQUEST2 = 'TIME-OBS'
      ELSE
         EQUIVREQUEST  = REQUEST
         EQUIVREQUEST2 = REQUEST
      END IF

C  How many cards?

      NC = LEN(HEADBUF)/80

      FHEAD   =0.0D0
      DO 8702 N=1, NC
         J = (N-1)*80+1
         PARAM = HEADBUF(J:J+7)
         IF (PARAM .EQ. 'END') GO TO 60
         IF (PARAM .EQ. REQUEST .OR. 
     &       PARAM .EQ. EQUIVREQUEST .OR.
     &       PARAM .EQ. EQUIVREQUEST2 ) THEN
            INLINE = HEADBUF(J:J+79)
            CALL DISSECT(INLINE(11:80),1,.FALSE.,NTYPE,NUM,
     &                   FNUM,WORD1,NCHAR,OERR)
            IF (NTYPE .LE. 2) THEN
               IF(NTYPE .EQ. 1) THEN
                  FHEAD = NUM
               ELSE
#ifdef VMS
                  READ(WORD1(1:NCHAR),100) FHEAD
 100              FORMAT(F<NCHAR>.<NCHAR-1>)
#else
                  READ(WORD1(1:NCHAR),*) FHEAD
#endif
               END IF
               RETURN

C  Return RA, DEC, and HA as seconds of time or arc.

            ELSE IF (NTYPE .EQ. 3 .AND. PARAM .EQ. 'RA') THEN
               CALL GETCOORD(WORD1,NH,NM,FNS,SG)
               FHEAD = 3600.0D0*NH+60.0D0*NM+FNS
               RETURN
               
            ELSE IF (NTYPE .EQ. 3 .AND. PARAM .EQ. 'DEC') THEN
               CALL GETCOORD(WORD1,ND,NM,FNS,SG)
               FHEAD = SG*(FNS + 60.D0*DFLOAT(NM) +
     &                 3600.D0*DFLOAT(ND))
               RETURN
               
            ELSE IF (NTYPE .EQ. 3 .AND. PARAM .EQ. 'HA') THEN
               CALL GETCOORD(WORD1,NH,NM,FNS,SG)
               FHEAD = SG*(FNS + 60.D0*DFLOAT(NM) +
     &                 3600.D0*DFLOAT(NH))
               RETURN
               
            END IF
         END IF
 8702 CONTINUE
      
 60   RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE CHEAD(REQUEST0,HEADBUF,CVAL)

C  CHEAD works like INHEAD, except that the parameter value
C  is always returned as an alphanumeric character string.
C  We also need to note where the Lick names differ from the standard
C  names.

      CHARACTER*8 PARAM
      CHARACTER*80 INLINE, WORD1
      CHARACTER*(*) HEADBUF, REQUEST0, CVAL
      CHARACTER*40  EQUIVREQUEST, EQUIVREQUEST2, REQUEST
      REAL*8 FNUM
      
C  How many cards?

      NC = LEN(HEADBUF)/80

C  Load the equivalent names. [added DATE-OBS aliases, rwp/osu 1998Nov16]

      NEQUIV = 1
      REQUEST = REQUEST0
      IF (REQUEST .EQ. 'EXPOSURE') THEN
         EQUIVREQUEST  = 'ITIME   '
         EQUIVREQUEST2 = 'EXPTIME '
         NEQUIV = 3
      ELSE IF (REQUEST .EQ. 'TIME    ') THEN
         EQUIVREQUEST  = 'UT      '
         EQUIVREQUEST2 = 'TIME-OBS'
         NEQUIV = 3
      ELSE IF (REQUEST .EQ. 'DATE-OBS') THEN
         EQUIVREQUEST  = 'DATE_OBS'
         EQUIVREQUEST2 = 'DATE    '
         NEQUIV = 3
      END IF

C  Look for primary request first, only if not found in entire header do
C    we proceed to look for equivalent requests

      DO IEQUIV = 1, NEQUIV
        IF (IEQUIV .EQ. 2) THEN
          REQUEST = EQUIVREQUEST
        ELSE IF (IEQUIV .EQ. 3) THEN
          REQUEST = EQUIVREQUEST2
        END IF
      
        DO 8703 N=1, NC
         J=(N-1)*80+1
         PARAM=HEADBUF(J:J+7)
         IF (PARAM .EQ. 'END') GO TO 70
         IF (PARAM .EQ. REQUEST) THEN
            INLINE(1:80)=HEADBUF(J:J+79)
            CALL DISSECT(INLINE(11:80),1,.FALSE.,NTYPE,NUM,
     &                   FNUM,WORD1,NCHAR,OERR)
            L = 1
 8704       IF (WORD1(L:L) .EQ. ' ' .AND. L .LT. 80) THEN
               L = L+1
               GO TO 8704
            END IF
            CVAL = WORD1(L:80)
            RETURN
         END IF
 8703   CONTINUE

 70   CVAL = ' '
      END DO
      
      RETURN
      END

      LOGICAL FUNCTION LHEAD(REQUEST,HEADBUF)

C
C  LHEAD - Return logical values from the image headers.
C
C  The parameters describing an image array, its conditions
C  of observation, reduction, and the like, are stored
C  in a character string header.  These subroutines are used
C  to return the values of these parameters to other VISTA
C  subroutines by searching for the value or character
C  string associated with the desired keyword.
C
C  INPUTS:
C     REQUEST         Alphanumeric keyword to search for
C     HEADBUF         The VISTA character header buffer
C---------------------------------------------------------------------------

      CHARACTER*8 PARAM
      CHARACTER*80 INLINE, WORD1
      CHARACTER*(*) HEADBUF, REQUEST
      REAL*8 FNUM

C  How many cards?
      NC = LEN(HEADBUF)/80
      LHEAD = .FALSE.

C  Each header record consists of 'NC' card images of 80 columns each.
C  The first thing on the card is a parameter name, which we extract
C  and place into PARAM.  Then we use DISSECT to extract the para-
C  meter value, which may be numeric or alpha-numeric depending on
C  the parameter.

      DO 8701 N=1, NC
         J = (N-1)*80+1
         PARAM = HEADBUF(J:J+7)
         IF (PARAM .EQ. 'END') GO TO 50
         IF (PARAM .EQ. REQUEST) THEN
            INLINE(1:80) = HEADBUF(J:J+79)
            CALL DISSECT(INLINE(11:80),1,.FALSE.,NTYPE,NUM,
     &                   FNUM,WORD1,NCHAR,OERR)
            IF (WORD1 .EQ. 'T') THEN
               LHEAD = .TRUE.
               RETURN
            END IF
         END IF
 8701 CONTINUE
      
 50   RETURN
      END

