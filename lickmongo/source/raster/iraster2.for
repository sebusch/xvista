	PROGRAM IRASTER
c Read the .tmp file and convert to a file suitable for the imagen
c  graphic device.
c  (In order to understand how this program works, read the IMPRINT-10
c  PROGRAMMMER'S MANUAL, chapter 5 (IMPRESS language)).
c
c Modified 8/26/85 DJP - group vectors into "paths" of connected vectors
c  rather than creating one path for each vector.
C Modified 1/14/86 DJP - make bitmaps of BITMAP1.TMP files (created
c  by command HALFTONE). Uses Impress BITMAP function.
C Modified 3/21/86 DJP - look for BITMAP1.TMP file on the correct NODE
C  (user may be on a MicroVAX).
c
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	PARAMETER NIN=2048, NBUF=1048576
	BYTE  BYTELINE(516,32),ABYTE
	BYTE BBUF(2*NBUF)
	INTEGER*2 IBUF(NIN),IX(NIN),IY(NIN)
	INTEGER*2 BUF(NBUF),two
	INTEGER*4 LIST(NBUF/4), BUFNEXT
	LOGICAL EXISTING,XYSWAPPED
	EQUIVALENCE (BUF,BBUF)
	INTEGER GX1,GY1,LY1,LY2,DUMMY,HSIZE,VSIZE
	BYTE BYTEBUF(264)

	CHARACTER*80    BITMAPFILE,MGO_VECFILE
	INTEGER SYS$TRNLOG,STATUS

	BYTE BYTEREV(-128:127)
	DATA BYTEREV
     1/1,-127,65,-63,33,-95,97,-31,17,-111,81,-47,49,-79,113,-15,9,-119,
     2 73,-55,41,-87,105,-23,25,-103,89,-39,57,-71,121,-7,5,-123,69,
     3 -59,37,-91,101,-27,21,-107,85,-43,53,-75,117,-11,13,-115,77,-51,
     4 45,-83,109,-19,29,-99,93,-35,61,-67,125,-3,3,-125,67,-61,35,-93,
     5 99,-29,19,-109,83,-45,51,-77,115,-13,11,-117,75,-53,43,-85,107,
     6 -21,27,-101,91,-37,59,-69,123,-5,7,-121,71,-57,39,-89,103,-25,
     7 23,-105,87,-41,55,-73,119,-9,15,-113,79,-49,47,-81,111,-17,31,
     8 -97,95,-33,63,-65,127,-1,0,-128,64,-64,32,-96,96,-32,16,-112,
     9 80,-48,48,-80,112,-16,8,-120,72,-56,40,-88,104,-24,24,-104,88,
     1 -40,56,-72,120,-8,4,-124,68,-60,36,-92,100,-28,20,-108,84,-44,
     1 52,-76,116,-12,12,-116,76,-52,44,-84,108,-20,28,-100,92,-36,60,
     2 -68,124,-4,2,-126,66,-62,34,-94,98,-30,18,-110,82,-46,50,-78,
     3 114,-14,10,-118,74,-54,42,-86,106,-22,26,-102,90,-38,58,-70,122,
     4 -6,6,-122,70,-58,38,-90,102,-26,22,-106,86,-42,54,-74,118,-10,
     5 14,-114,78,-50,46,-82,110,-18,30,-98,94,-34,62,-66,126,-2 /

* Read in list of vectors and find how many there are
	CALL LIB$GET_LUN(JUN11)
	OPEN (UNIT=JUN11,NAME='RASTFILE',STATUS='OLD',
     &  FORM='UNFORMATTED')

	NCOORD=0
	DO J = 1,NBUF/NIN
	  READ(JUN11,END=2) (BUF(I+NCOORD),I=1,NIN)
	  NCOORD = NCOORD + NIN
	ENDDO

2       NCOORD = NCOORD - NIN
	DO J = NCOORD+1,NCOORD+NIN
	  IF(BUF(J).EQ.-1) GOTO 4
	  NCOORD = NCOORD + 1
	ENDDO

4       NVECTOR = NCOORD / 4

c loop over all vectors, converting dots to diagonal lines that will
c print as a square of dots, 2x2
	do i=1,nvector
	ko=4*(i-1)
	if(buf(ko+1) .eq. buf(ko+3) .and. buf(ko+2) .eq. buf(ko+4)) then
	   buf(ko+3)=buf(ko+3)+1
	end if
	end do

c now convert these to imagen format and output the lot
	OPEN(4,FILE='impfile',STATUS='NEW',
     1  DISP='keep',RECORDTYPE='fixed',recl=130,
     2  carriagecontrol='none')

c set pen width to 2
	CALL INTBYT(232)        !SET_PEN WIDTH = 2
	CALL INTBYT(2)          ! R. Pogge, 1986 Aug 28

C  The next five bytes are used to set the origin. Put origin at lower
c  left corner of paper, with y axis going up, x axis going to the right.
c
c  set abs-v (Y) pos to 3300 (300pts/inch on 8.5 X 11 paper; 300*11)
	CALL INTBYT(137)        !SET_ABS_V
	CALL INTBYT(12)         !   3300 = 12*256 + 228
	CALL INTBYT(228)        !

c  set horizontal and vertical axes
	CALL INTBYT(205)        !SET_HV_SYSTEM
	CALL INTBYT(124)        !124 = 0 11 11 100 binary,  sets origin = 3
			!      0  3  3   4 decimal, sets axes = 3
			!                           sets orientation = 4

C  Run through all the vectors. If current X,Y is same as last, then append
C  this point to the path. Otherwise, end the path and draw it.

	NUMV = 2
	IX(1) = BUF(1)
	IY(1) = BUF(2)
	IX(2) = BUF(3)
	IY(2) = BUF(4)
	DO I=5,NCOORD,4
	  IF (BUF(I).EQ.BUF(I-2) .AND. BUF(I+1).EQ.BUF(I-1)
     1    .AND. NUMV.LT.NIN) THEN
	    NUMV = NUMV+1
	    IX(NUMV) = BUF(I+2)
	    IY(NUMV) = BUF(I+3)
	  ELSE
	    CALL CREATEDRAW(IX,IY,NUMV)
	    NUMV = 2
	    IX(1) = BUF(I)
	    IY(1) = BUF(I+1)
	    IX(2) = BUF(I+2)
	    IY(2) = BUF(I+3)
	  ENDIF
	ENDDO

C  Force last vector-path to be written out.

	CALL CREATEDRAW(IX,IY,NUMV)

C  Open bitmap file if any. Get the node name and directory from the
C  MGOxxxxxx.VEC file ('RASTFILE')

	STATUS = SYS$TRNLOG('RASTFILE',L,MGO_VECFILE,,,)
	DO I=1,L-2
	  IF (MGO_VECFILE(I:I+2).EQ.'MGO') GOTO 10
	ENDDO

	BITMAPFILE = 'BITMAP1.TMP'
	GOTO 20

10      BITMAPFILE = MGO_VECFILE(1:I-1)//'BITMAP1.TMP;-0'
	TYPE *,'FILE = ',BITMAPFILE

20      INQUIRE (FILE=BITMAPFILE,EXIST=EXISTING)
	IF (EXISTING) THEN
	  CALL LIB$GET_LUN(JUN99)
C Version number of '-0' gets version of BITMAP1.TMP with LOWEST version number
C (that is, the OLDEST file), as the user may by now have created more bitmaps.
C Then  read file to count the number of records.
	  OPEN(JUN99,FILE=BITMAPFILE,FORM='UNFORMATTED',
     1    STATUS='OLD',IOSTAT=IERR)
	  READ(JUN99) GX1,GY1,NUMWORDS,NUMLINES,XYSWAPPED
	  HSIZE = NUMWORDS
	  VSIZE = NUMLINES/32

c  Reset the origin to bottom right of page (first "PUSH" old HV system
c  so that we don't get the IMPNEST error message. Later "POP" back to
c  the old HV system).
c
c  Push old HV system
	  CALL INTBYT(211)

c  set abs-v (Y) pos to 0
	  IF (XYSWAPPED) THEN

		CALL INTBYT(137)        !SET_ABS_V
		CALL INTBYT(0)
		CALL INTBYT(0)

c  set abs-h (X) pos to 2550
		CALL INTBYT(135)        !SET_ABS_H
		CALL INTBYT(9)
		CALL INTBYT(246)

c  set horizontal and vertical axes
		CALL INTBYT(205)        !SET_HV_SYSTEM
		CALL INTBYT(99) !124 = 0 11 00 011 binary,
			!      0  3  0   3 decimal, might be 0 3 0 1.

C  Subtract 32 from GY1. A fudge. Don't ask why!
		GY1 = GY1 - 32
	  ENDIF

c  Set abs-v pos to GY1
	  DO I=1,130
	    CALL INTBYT(254)
	  ENDDO
	  CALL INTBYT(137)      !SET_ABS_V
	  CALL INTBYT(GY1/256)
	  CALL INTBYT(GY1 - (GY1/256)*256)

c  Set abs-h pos to GX1
	  CALL INTBYT(135)      !SET_ABS_H
	  CALL INTBYT(GX1/256)
	  CALL INTBYT(GX1 - (GX1/256)*256)

	  CALL INTBYT(235)      !Code for BITMAP
	  CALL INTBYT(15)       !Code for draw black dots.
	  CALL INTBYT(HSIZE)
	  CALL INTBYT(VSIZE)

C  Read in file of dots, then use Imagen BITMAP command to produce image.
C  Unfortunately, BITMAP command sends bits in 32 x 32 bit "patches".
C  Therefore, we must read 32 rows at a time. The file has already been
C  padded with blank records so that it has a multiple of 32 records.

	  DO I=1,VSIZE
	    DO J=1,32   !Read in 32 lines at a time (each line is HSIZE words).
	      READ (JUN99) (BYTELINE(K,J),K=1,HSIZE*4)
	    ENDDO
			 !Write out HSIZE many "patches"
	    DO K=1,HSIZE !(a patch is 32 X 32 bits)
	      DO J=1,32
		DO L=1,4
		  ABYTE = BYTELINE((K-1)*4+L,J)
		  ABYTE = BYTEREV(ABYTE)   !Byte must be reversed
		  CALL BYT(ABYTE)
		ENDDO
	      ENDDO
	    ENDDO
	  ENDDO

	  CLOSE(JUN99,DISP='DELETE',IOSTAT=IQ)
	  CALL LIB$FREE_LUN(JUN99)

c  Pop back old HV system
	  CALL INTBYT(212)

	ENDIF

c  Now put END-OF-PAGE and EOF marks on.
	CALL INTBYT(219)        !ENDPAGE
	CALL INTBYT(255)!EOF
	do i=1,130
	  CALL INTBYT(254) !Flush last buffer by sending a bufferful of NO_OPS
	end do

	close(JUN11)
	CALL LIB$FREE_LUN(JUN11)
	CLOSE(4,DISP='keep',IOSTAT=IQ)
	CALL EXIT
	END

c***************************************************************************

	SUBROUTINE CREATEDRAW(IX,IY,NUMV)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	INTEGER*2       IX(NUMV),IY(NUMV),IDUM
	BYTE            B(2)

	EQUIVALENCE (IDUM,B)

	CALL CREATE_PATH (NUMV)
	DO I=1,NUMV
	  IDUM=IX(I)
	  CALL BYT (B(2))
	  CALL BYT (B(1))
	  IDUM=IY(I)
	  CALL BYT (B(2))
	  CALL BYT (B(1))
	ENDDO

	CALL DRAW_PATH

	RETURN
	END

C***************************************************************************

	SUBROUTINE CREATE_PATH (NUMV)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	BYTE    BN1,BN2

	CALL INTBYT(230)
	N1 = NUMV/256
	N2 = NUMV - N1*256
	BN1 = N1
	IF (N2.GE.128) N2 = N2 - 256
	BN2 = N2
	CALL BYT (BN1)
	CALL BYT (BN2)
	RETURN
	END

C***************************************************************************

	SUBROUTINE DRAW_PATH
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	CALL INTBYT(234)
	CALL INTBYT(15)
	RETURN
	END

C***************************************************************************

	SUBROUTINE INTBYT (INTEGER4)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	BYTE            BT

	NUM = INTEGER4
	IF (NUM.GE.128) NUM = NUM - 256
	BT = NUM
	CALL BYT(BT)
	RETURN
	END

C***************************************************************************

	SUBROUTINE BYT (BT)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	BYTE            BT,OBB(130)
	CHARACTER*130   ABB
	EQUIVALENCE (ABB,OBB)

	IF (INDEX.LT.130) THEN
	  INDEX = INDEX+1
	ELSE
	  WRITE (4,'(A130)') ABB
	  INDEX = 1
	ENDIF

	OBB(INDEX) = BT
	RETURN
	END

