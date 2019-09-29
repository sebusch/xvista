        SUBROUTINE COLORROLL(JUMP,NCONJ)

C	Routine to roll the VISTA color map up or down.

C	This routine is call by a C interupt handler to move the color map.

C	Input:	JUMP	Amount to roll the color map.  Negative JUMP means
C			to roll the map down.
C		NCONJ	Amoutn to increase or decrease the contrast.

C       Author: Tod R. Lauer            4/10/86

	INTEGER TVCOLORLD
        INTEGER*2 COLOR(256,3), COLRBUF(256), COLSAV(256,3)
	COMMON /SPACE/ COLRBUF
        COMMON /TVCOLOR/ COLOR, COLSAV, LOCZERO, NCONTR

C   Roll up
	IF (JUMP .GT. 0) THEN
		IUP	=JUMP
		DO 2757 J=1,3
               		DO 2758 I=1,IUP
                       		COLRBUF(I)      =COLOR(255-IUP+I,J)
2758 			CONTINUE

                       	DO 2759 I=1, 255-IUP
                       		COLOR(256-I,J)  =COLOR(256-IUP-I,J)
2759                   	CONTINUE

			DO 2760 I=1,IUP
                       		COLOR(I,J)      =COLRBUF(I)
2760           		CONTINUE
2757   		CONTINUE

		LOCZERO	=LOCZERO+IUP

	ELSE IF (JUMP .LT. 0) THEN
C					! Roll down
		IUP	=-JUMP
		DO 2761 J=1,3
               		DO 2762 I=1,IUP
                       		COLRBUF(I)      =COLOR(I,J)
2762                    CONTINUE

                        DO 2763 I=IUP+1,255
                       		COLOR(I-IUP,J)  =COLOR(I,J)
2763                    CONTINUE

                        DO 2764 I=1,IUP
                 	       	COLOR(255-IUP+I,J)=COLRBUF(I)
2764                    CONTINUE
2761             CONTINUE

		LOCZERO	=LOCZERO+255-IUP
	END IF

	I	=LOCZERO/255
	LOCZERO	=LOCZERO-I*255

	IF (NCONJ .NE. 0) THEN
		NCONTR	=NCONTR+NCONJ
		IF (NCONTR .LT. 0) THEN
			NCONTR	=0
		ELSE IF (NCONTR .GT. 126) THEN
			NCONTR	=126
		END IF

		GAIN	=254.0/(254.0-2*NCONTR)
		DO 2800 J=1,3
			DO 2801 I=1, NCONTR+1
				COLRBUF(I)	=COLSAV(1,J)
2801			CONTINUE

			DO 2802 I=255-NCONTR, 255
				COLRBUF(I)	=COLSAV(255,J)
2802			CONTINUE

			DO 2803 I=NCONTR+2,254-NCONTR
				IOFF	=NINT(GAIN*(I-NCONTR-1))
				COLRBUF(I)	=COLSAV(1+IOFF,J)
2803			CONTINUE

			IF (LOCZERO .GT. 127) THEN
				DO 2860 I=1,MIN0(255-LOCZERO,NCONTR)
					COLRBUF(I)	=COLSAV(255,J)
2860				CONTINUE
			ELSE
			DO 2861 I=MAX0(255-LOCZERO,255-NCONTR),255
					COLRBUF(I)	=COLSAV(1,J)
2861				CONTINUE
			END IF

			IUP	=LOCZERO

               		DO 2858 I=IUP+1,255
				COLOR(I,J)	=COLRBUF(I-IUP)
2858 			CONTINUE

                       	DO 2859 I=1, IUP
                       		COLOR(I,J)  =COLRBUF(I+254-IUP)
2859                   	CONTINUE

2800		CONTINUE
	END IF


	ISTAT=TVCOLORLD(COLOR(1,1),COLOR(1,2),COLOR(1,3),256,0)

        RETURN
        END
