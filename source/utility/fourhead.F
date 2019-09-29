#include "Vista.h"
	SUBROUTINE FOURHEAD(IHEAD,OBJECT,BUF)

C       This routine generates a fits header record for 4-shooter images.

C       Input:  IHEAD   = 100-word header buffer
C               OBJECT  = 200-character buffer

C       Output: BUF     = FITS format header record

C       Author: Tod R. Lauer	5/15/84

        CHARACTER BUF*(*)
        INTEGER*2 IHEAD(100)
        CHARACTER OBJECT*(*)
        CHARACTER FILTER*6
        CHARACTER PM

C       FITS format consists of records of thirty six 80-column
C       card images.  All of the info in IHEAD can be written into
C       one FITS record.

C       These format statements define the structure of the various
C       FITS card images.  1000 AND 1100 are for character string
C       parameters.  Note the final A1 in the format where we have
C       to explicitly write a quote character (').  1300 is for a     '
C       simple integer parameter field.  1400 is for a 'time' field,
C       and 1500 is for a 'date' field.  1600 is for a declination
C       field.

 999    FORMAT(A8,'= ',19X,A1)
1000    FORMAT(A8,'= ''',12X,A6,A1)
1100    FORMAT(A8,'= ''',A64,A1)
1200    FORMAT(A8,'= ',1PE20.8)
1300    FORMAT(A8,'= ',I20)
1400    FORMAT(A8,'= ''',I10,':',I2,':',F4.1,'''')
1500    FORMAT(A8,'= ''',10X,A2,'/',A2,'/',A2,'''')
1600    FORMAT(A8,'= ''',A8,I2,':',I2,':',F4.1,'''')
        BUF = ' '
        WRITE(BUF(1:80),999)     'SIMPLE  ','T'
        WRITE(BUF(81:160),1300)  'BITPIX  ',16
        WRITE(BUF(161:240),1300) 'NAXIS   ',2
        WRITE(BUF(241:320),1300) 'NAXIS1  ',800
        WRITE(BUF(321:400),1300) 'NAXIS2  ',800
        WRITE(BUF(401:480),1300) 'OBSNUM  ',IHEAD(3)
        WRITE(BUF(481:560),1200) 'CRVAL1  ',0.0
        WRITE(BUF(561:640),1200) 'CRVAL2  ',0.0
        WRITE(BUF(641:720),1200) 'CDELT1  ',1.0
        WRITE(BUF(721:800),1200) 'CDELT2  ',1.0
        WRITE(BUF(801:880),1500)'DATE-OBS',OBJECT(77:78),OBJECT(74:75),
     1		OBJECT(71:72)
        WRITE(BUF(881:960),1400) 'TIME    ',IHEAD(41),IHEAD(42),
     1          FLOAT(IHEAD(43))+FLOAT(IHEAD(44))/10.0
        WRITE(BUF(961:1040),1400) 'ST      ',IHEAD(56),IHEAD(57),
     1          FLOAT(IHEAD(58))+FLOAT(IHEAD(59))/10.0
        WRITE(BUF(1041:1120),1300) 'EXPOSURE',IHEAD(5)
        WRITE(BUF(1121:1200),1400) 'RA      ',IHEAD(46),IHEAD(47),
     1          FLOAT(IHEAD(48))+FLOAT(IHEAD(49))/10.0
	IF (IHEAD(53) .LT. 0) THEN
		IHEAD(53)	=-IHEAD(53)
		IHEAD(52)	=-IHEAD(52)
		IHEAD(51)	=-IHEAD(51)
		PM	='-'
	ELSE
		PM	='+'
	END IF

        WRITE(BUF(1201:1280),1600) 'DEC     ',PM,
     1		IHEAD(51), IHEAD(52), FLOAT(IHEAD(53))
        WRITE(BUF(1281:1360),1100) 'OBJECT  ',OBJECT(31:70),''''
        WRITE(BUF(1361:1440),1300) 'CCDNUM  ',IHEAD(4)
        WRITE(BUF(1441:1520),1000) 'SHUTTER ','OPEN',''''
        WRITE(BUF(1521:1600),1000) 'STATUS  ','RAW',''''
	IF (IHEAD(76) .EQ. 0) THEN
		FILTER	='     G'
	ELSE IF (IHEAD(76) .EQ. 1) THEN
		FILTER	='     R'
	ELSE IF (IHEAD(76) .EQ. 2) THEN
		FILTER	='    IR'
	ELSE IF (IHEAD(76) .EQ. 3) THEN
		FILTER	=' CLEAR'
	ELSE
		FILTER	='     ?'
	END IF

        WRITE(BUF(1601:1680),1000) 'FILTER1 ',FILTER,''''
	IF (IHEAD(77) .EQ. 0) THEN
		FILTER	='     G'
	ELSE IF (IHEAD(77) .EQ. 1) THEN
		FILTER	='     R'
	ELSE IF (IHEAD(77) .EQ. 2) THEN
		FILTER	='    IR'
	ELSE IF (IHEAD(77) .EQ. 3) THEN
		FILTER	=' CLEAR'
	ELSE
		FILTER	='     ?'
	END IF

        WRITE(BUF(1681:1760),1000) 'FILTER2 ',FILTER,''''
	WRITE(BUF(1761:1840),1200) 'SECZ    ',IHEAD(67)/1000.0

C       Make the last card image the 'END' card.

C        LAST = LEN(BUF)
C        BUF(LAST-79:LAST) = 'END'
        BUF(1841:1843) = 'END'
        BUF(1844: ) = ' '

        RETURN
        END

