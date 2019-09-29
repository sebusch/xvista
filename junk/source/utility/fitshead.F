#include "Vista.h"
        SUBROUTINE FITSHEAD(IHEAD,OBJECT,BUF)

C       This routine generates a fits header record for PDP-8 images.

C       Input:  IHEAD   = 32-word header buffer
C               OBJECT  = 64-character object name

C       Output: BUF     = FITS format header record

C       Author: Richard Stover
C               Jan. 1983

        CHARACTER BUF*(*)
        DIMENSION IHEAD(32)
        CHARACTER OBJECT*64
        CHARACTER FILTER*6
        CHARACTER PM

C       FITS format consists of records of thirty six 80-column
C       card images.  All of the info in IHEAD can be written into
C       one FITS record.

C       These format statements define the structure of the various
C       FITS card images.  1000 AND 1100 are for character string
C       parameters.  Note the final A1 in the format where we have
C       to explicitly write a quote character (').  1300 is for a   '
C       simple integer parameter field.  1400 is for a 'time' field,
C       and 1500 is for a 'date' field.  1600 is for a declination
C       field.

 999    FORMAT(A8,'= ',19X,A1)
1000    FORMAT(A8,'= ''',A,A1)
1100    FORMAT(A8,'= ''',A64,A1)
1200    FORMAT(A8,'= ',1PE20.8)
1300    FORMAT(A8,'= ',I20)
1400    FORMAT(A8,'= ''',I10,':',I2,':',F4.1,'''')
1500    FORMAT(A8,'= ''',I12,'/',I2,'/',I2,'''')
1600    FORMAT(A8,'= ''',A8,I2,':',I2,':',F4.1,'''')
        BUF = ' '
        WRITE(BUF(1:80),999)     'SIMPLE  ','T'
        WRITE(BUF(81:160),1300)  'BITPIX  ',16
        WRITE(BUF(161:240),1300) 'NAXIS   ',2
        WRITE(BUF(321:400),1300) 'NAXIS1  ',IHEAD(4)/IHEAD(8)
        WRITE(BUF(241:320),1300) 'NAXIS2  ',IHEAD(3)/IHEAD(7)
        WRITE(BUF(401:480),1300) 'OBSNUM  ',IHEAD(1)
        WRITE(BUF(481:560),1300) 'TAPENUM ',IHEAD(31)
        WRITE(BUF(641:720),1200) 'CRVAL1  ',FLOAT(IHEAD(6))
        WRITE(BUF(561:640),1200) 'CRVAL2  ',FLOAT(IHEAD(5))
        WRITE(BUF(801:880),1200) 'CDELT1  ',FLOAT(IHEAD(8))
        WRITE(BUF(721:800),1200) 'CDELT2  ',FLOAT(IHEAD(7))
        WRITE(BUF(881:960),1500) 'DATE-OBS',IHEAD(15),IHEAD(14),
     &        IHEAD(16)
        IHR = IHEAD(13)/100
        WRITE(BUF(961:1040),1400) 'TIME    ',IHR,IHEAD(13)-IHR*100, 0.0
        WRITE(BUF(1041:1120),1300) 'EXPOSURE',IHEAD(11)*4096+IHEAD(12)
        WRITE(BUF(1121:1200),1400) 'RA      ',IHEAD(17),IHEAD(18),
     .          FLOAT(IHEAD(19))
        IDEC = IHEAD(20)*4096+IHEAD(21)
        IF (IDEC .GT. 4096*2048) THEN
C                                               ! Translate the DEC sign
                MDEC    =4096*4096-IDEC
                PM = '-'
        ELSE
                MDEC    =IDEC
                PM = '+'
        END IF
        IDEG = MDEC/3600
        MDEG = MDEC/3600
        IMIN = (MDEC - MDEG*3600)/60
        ISEC = MDEC - (MDEG*60+IMIN)*60
        WRITE(BUF(1201:1280),1600) 'DEC     ',PM,IDEG,IMIN,FLOAT(ISEC)
        IHA = IHEAD(22)/100
        WRITE(BUF(1281:1360),1400) 'HA      ',IHA,IHEAD(22)-IHA*100,
     &        0.0
        WRITE(BUF(1361:1440),1300) 'V1      ',IHEAD(23)
        WRITE(BUF(1441:1520),1300) 'V2      ',IHEAD(24)
        WRITE(BUF(1521:1600),1300) 'V3      ',IHEAD(25)
        WRITE(BUF(1601:1680),1300) 'V4      ',IHEAD(26)
        WRITE(BUF(1681:1760),1300) 'V5      ',IHEAD(27)
        WRITE(BUF(1761:1840),1300) 'V6      ',IHEAD(28)
        WRITE(BUF(1841:1920),1300) 'V7      ',IHEAD(29)
        WRITE(BUF(1921:2000),1100) 'OBJECT  ',OBJECT,''''
        IF(IHEAD(10).EQ.1) THEN
                WRITE(BUF(2001:2080),1000) 'SHUTTER ','CLOSED',''''
                WRITE(BUF(2081:2160),1000) 'STATUS  ','RAW',''''
        ELSE IF(IHEAD(10).EQ.0) THEN
                WRITE(BUF(2001:2080),1000) 'SHUTTER ','OPEN',''''
                WRITE(BUF(2081:2160),1000) 'STATUS  ','RAW',''''
        ELSE
                WRITE(BUF(2001:2080),1000) 'SHUTTER ','OPEN',''''
                WRITE(BUF(2081:2160),1000) 'STATUS  ','REDUCED',''''
        END IF
        WRITE(FILTER,'(I6)') IHEAD(9)
        WRITE(BUF(2161:2240),1000) 'FILTER  ',FILTER,''''
        WRITE(BUF(2241:2320),1000) 'LICK    ','FITS2',''''

C       Make the last card image the 'END' card.

        LAST = LEN(BUF)
        BUF(LAST-79:LAST) = 'END'

        RETURN
        END

