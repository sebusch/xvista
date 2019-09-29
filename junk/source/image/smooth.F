#include "Vista.h"
	SUBROUTINE SMOOTH(A,NROW,NCOL)

C       Subroutine to perform 2D image convolution. Can be called eith
C       by command SMOOTH or by command CONVOLVE

C       This routine smooths an image.  There are several options:
C       1. The image is smoothed by convolution with a gaussian 
C          filter.  The gaussian can have different widths
C          along the rows or columns. This convolution can be done in
C          each direction independently. Command: SMOOTH
C       2. The image is smoothed by convolution with a boxcar 
C          filter.  The boxcar can have different widths
C          along the rows or columns. This convolution can be done in
C          each direction independently. There are two options here:
C          standard BOXCAR and RUNMEAN. RUNMEAN is much faster and is
C          identical in the case of odd-sized boxes. Command: SMOOTH
C       3. The image is smoothed by convolution with an arbitrary
C          function given in another buffer. In this case, a full 2-D
C          convolution is needed. Command: SMOOTH with BUF= keyword or
C          command CONVOLVE

C       Keywords:       FW=fw           Full width of gaussian in pixels
C                       FWR=fwr         Full width of gaussian along rows
C                       FWC=fwc         Full width of gaussian down columns
C                       BOXCAR          Use a Box filter instead of a gaussian.
C                       BOXCAR=fw       Use a box filter of full width wrXwc.
C                       RUNMEAN         Running-mean filter (Similar, but much
C                                       faster than BOXCAR at large FWs)
C                       BUF=nb          Specifies buffer number for full
C                                       convolution


C       Author: Tod R. Lauer            5/27/83
C               bf                      1/24/86
C               JJGG                    6/16/87
C               RWP                     1/31/88
C               Holtz                   8/93

C  Length of holding buffer

	PARAMETER (MAXHOLD=300)
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	DIMENSION A(NCOL,NROW), FUNCC(MAXHOLD), FUNCR(MAXHOLD)

	INTEGER COL, ROW
	LOGICAL COLCONV, ROWCONV, EDGE, FILL, BOX, KEYCHECK, RUNM
        LOGICAL FULLCONV
	CHARACTER*8 PARM

	PARAMETER (MAXC=MAXCOLIM)
	REAL*8 HOLD(MAXC,MAXHOLD)
	COMMON /WORK/ HOLD

C       Verify keywords.
	CALL KEYINIT
        IF (COM .EQ. 'SMOOTH') THEN
	  CALL KEYDEF('FW=')
	  CALL KEYDEF('FWC=')
	  CALL KEYDEF('FWR=')
	  CALL KEYDEF('BOXCAR')
	  CALL KEYDEF('RUNMEAN')
	  CALL KEYDEF('BOXCAR=')
	  CALL KEYDEF('BUF=')
	  CALL KEYDEF('WID=')
        END IF

	IF (.NOT.KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Check keywords for parameters.
	ROWCONV =.FALSE.
	COLCONV =.FALSE.
	FULLCONV =.FALSE.
	BOX     =.FALSE.
	RUNM    =.FALSE.

C   Reset filter widths
	FWR     = 0.0
	FWC     = 0.0
        WID     = 3.0

	DO 8701 I=1, NCON

		IF (WORD(I)(1:3) .EQ. 'FW=') THEN
C                    Full width
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			FWR = ABS(F)
			FWC = FWR

		ELSE IF (WORD(I)(1:7) .EQ. 'BOXCAR=') THEN
C                    Box filter
			CALL ASSIGN(WORD(I),F,PARM)
			FWR = ABS(F)
			FWC = FWR
			IF (XERR) RETURN
			BOX = .TRUE.

		ELSE IF (WORD(I)(1:6) .EQ. 'BOXCAR') THEN
C                    Box filter
			BOX = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'FWR=') THEN
C                    Row width
			CALL ASSIGN(WORD(I),F,PARM)
			FWR = ABS(F)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:4) .EQ. 'FWC=') THEN
C                    Column width
			CALL ASSIGN(WORD(I),F,PARM)
			FWC = ABS(F)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:4) .EQ. 'WID=') THEN
C                    number of half widths to go out
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			WID = ABS(F)

		ELSE IF (WORD(I)(1:4) .EQ. 'RUNM') THEN
                        RUNM = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'BUF=') THEN
                        FULLCONV = .TRUE.
			CALL ASSIGN(WORD(I),F,PARM)
			IMNUM = ABS(F)
			IF (XERR) RETURN
           
		END IF
8701    CONTINUE

C   Check for CONVOLVE command
        IF (COM .EQ. 'CONVOLVE') FULLCONV = .TRUE.

C   If a spectrum, ignore width of filter along rows

	IF (ISSPECTRUM(IM)) FWR = 0.0

C       Check for inconsistencies.

	IF (FWR .GT. 0.0) ROWCONV =.TRUE.
	IF (FWC .GT. 0.0) COLCONV =.TRUE.

	IF ((.NOT. FULLCONV) .AND.
     &      (.NOT.COLCONV).AND.(.NOT.ROWCONV)) THEN
		PRINT *,'No widths were specified...'
		XERR    =.TRUE.
		RETURN

	ELSE IF (ROWCONV .AND. ISSPECTRUM(IM)) THEN
		PRINT *,' Can''t smooth a spectrum in rows.'
		XERR = .TRUE.
		RETURN
 
        ELSE IF (FULLCONV .AND. (ROWCONV .OR. COLCONV)) THEN
                PRINT *, ' Can''t specify both a buffer and a filter.'

	END IF

C       Perform running mean.
        IF (RUNM) THEN
             NPC = INT(FWC/2.)
             NPR = INT(FWR/2.)
             CALL RUNMEAN(A,NROW,NCOL,HOLD,NPR,NPC)
             RETURN

C       Do we have a full scale convolution?
        ELSE IF (FULLCONV) THEN

C         Get the convolution kernel
          IF (COM .EQ. 'CONVOLVE') THEN
            CALL GETIMAGE(LOCIM,ISROW,IEROW,ISCOL,IECOL,2)
          ELSE
            IF (.NOT. BUFF(IMNUM)) THEN
              PRINT *, 'No image in buffer ', IMNUM
              XERR = .TRUE.
              RETURN
            END IF
            LOCIM    = IMLOC(IMNUM)
            ISROW = ICOORD(IYSR,IMNUM)
            ISCOL = ICOORD(IXSC,IMNUM)
            IEROW = ICOORD(IYER,IMNUM)
            IECOL = ICOORD(IXEC,IMNUM)
          END IF
            
C         Allocate smace for the new convoled image. It will go into
C           the same buffer as the original image, but we don''t release
C           the old image memory until we are done
          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
          CALL CREATEIM(LOCNEW,JSROW,JEROW,JSCOL,JECOL,1,
     &                    TEMPHEAD,.FALSE.)

C         Do the convolution
          CALL CCCONVOLVEIT(LOCNEW,JSROW,JEROW,JSCOL,JECOL,
     &                        LOCIM,ISROW,IEROW,ISCOL,IECOL,
     &                        A, NROW, NCOL)

C         Now we can drop the old image
          CALL RELEASEOLDIMAGE          

          RETURN

        ELSE
C       Convolution with a boxcar or gaussian:
C       Initialize the convolution arrays.

	  DO 8702 I=1,MAXHOLD
		FUNCR(I) = 0.0
		FUNCC(I) = 0.0
8702      CONTINUE

C       Set up convolving function arrays.

	  IF (COLCONV) THEN
		IF (BOX) THEN
C                 Convolution with a box
C                 Width
	          NPC = 2*INT((FWC+0.9999)/2.)+1
C                Values at ends
		  FRACC = (FWC-FLOAT(NPC-2))/2.
C                Normalization
		  CNORM = FLOAT(NPC-2) + 2.*FRACC
		  IF (NPC .GT. MAXHOLD) THEN
		    PRINT *,'Limit box width to',MAXHOLD-1,' pixels.'
		    XERR = .TRUE.
		    RETURN
		  END IF
		  DO 8703 I=2,NPC-1,1
		    FUNCC(I) = 1./CNORM
8703              CONTINUE
		  FUNCC(1) = FRACC/CNORM
		  FUNCC(NPC) = FRACC/CNORM

		ELSE
C                Convolution with a gaussian

C		  NPC     =2*((WID*NINT(FWC))/2)+1
		  NPC     =2*(NINT(WID*FWC+0.5)/2)+1
		  MIDC    =NPC/2+1
		  IF (NPC .GT. MAXHOLD) THEN
		    PRINT *,'Limit gaussian FWHM to',MAXHOLD/WID
	            XERR    =.TRUE.
		    RETURN
		  END IF
		  CNORM = 0.
		  FAC = -4.*ALOG(2.)/FWC/FWC
		  DO 8704 I=1, NPC
		    X       =FLOAT(I-MIDC)
		    FUNCC(I)=EXP(X*X*FAC)
		    CNORM = CNORM + FUNCC(I)
8704              CONTINUE
		  DO 8705 I=1, NPC
		    FUNCC(I)=FUNCC(I)/CNORM
8705              CONTINUE

		END IF
	  END IF

	  IF (ROWCONV) THEN
		IF (BOX) THEN
C                Convolution with a box
		  NPR = 2*INT((FWR+0.9999)/2.)+1
		  FRACR = (FWR-FLOAT(NPR-2))/2.
		  RNORM = FLOAT(NPR-2) + 2.*FRACR
		  MIDR = NPR/2 + 1
		  IF (NPR .GT. MAXHOLD) THEN
		    PRINT *,'Limit box width to',MAXHOLD-1,' pixels.'
	            XERR = .TRUE.
		    RETURN
		  END IF
		  DO 8706 I=2,NPR-1,1
		    FUNCR(I) = 1./RNORM
8706              CONTINUE
		  FUNCR(1) = FRACR/RNORM
		  FUNCR(NPR) = FRACR/RNORM

		ELSE
C                Prepare to convolve with a gaussian

C                Go out to WID HWs
C		  NPR     =2*((WID*NINT(FWR))/2)+1
		  NPR     =2*(NINT(WID*FWR+0.5)/2)+1
		  MIDR    =NPR/2+1
		  IF (NPR .GT. MAXHOLD) THEN
		    PRINT *,'Limit gaussian FWHM to',MAXHOLD/WID
                    XERR    =.TRUE.
		    RETURN
		  END IF
		  FAC = -4.*ALOG(2.)/FWR/FWR
		  RNORM = 0.
		  DO 8707 I=1, NPR
		    X       =FLOAT(I-MIDR)
		    FUNCR(I)=EXP(X*X*FAC)
		    RNORM = RNORM + FUNCR(I)
8707              CONTINUE
		  DO 8708 I=1, NPR
		    FUNCR(I)=FUNCR(I)/RNORM
8708              CONTINUE

		END IF
	  END IF

C       Convolve each image row with the column convolving function.
C       Use the 1D function to do the work.  Pack the result back into
C       the image.

	  IF (COLCONV) THEN
            DO 8709 ROW=1, NROW
#if defined(__SUNVIEW) || defined(__X11)
	      CALL LOOPDISP
#endif
	      CALL CONVOLVE(A(1,ROW),HOLD(1,1),NCOL,FUNCC,NPC)
              DO 8710 COL=1, NCOL
	        A(COL,ROW) = HOLD(COL,1)
8710          CONTINUE
8709        CONTINUE
	  END IF

C       Now go down each column and convolve the image with the
C       row convolving function.  All the columns are processed
C       together to avoid page faults.  Save the results in the HOLD
C       buffer until the convolving function moves down the columns
C       and the original data is no longer needed.

	  IF (.NOT.ROWCONV) RETURN

C      Start at the buffer top
	  IH      =1
	  FILL    =.FALSE.
	  MIDR    =MIDR-1
	  DO 8711 ROW=1, NROW
		EDGE    =.FALSE.
		SUM     =1.0
		DO 8712 COL=1, NCOL
C                  Clear the holding array
			HOLD(COL,IH)    =0.0
8712            CONTINUE
C              Limits of the convolution
		JS      =ROW-MIDR
		JE      =ROW+MIDR

C       Check to see if the desired convolution extends beyond the data.
C       If so, adjust the normalization.
		IF (JS .LT. 1) THEN
			EDGE = .TRUE.
			DO 8713 J=JS, 0
				SUM = SUM-FUNCR(J-JS+1)
8713                    CONTINUE
			JS = 1
			IF (SUM .EQ. 0.0) SUM=1.0
		END IF

		IF (JE .GT. NROW) THEN
			EDGE = .TRUE.
			DO 8714 J=NROW+1, JE
				SUM = SUM-FUNCR(NPR-JE+J)
8714                    CONTINUE
			JE = NROW
			IF (SUM .EQ. 0.0) SUM=1.0
		END IF

C       Perform the convolution
		DO 8715 J=JS, JE
			DO 8716 COL=1, NCOL
			HOLD(COL,IH)    =HOLD(COL,IH)
     &                                  +FUNCR(J-ROW+MIDR+1)*A(COL,J)
8716                    CONTINUE
8715            CONTINUE

		IF (EDGE) THEN
C                  Edge effects
			DO 8717 COL=1, NCOL
				HOLD(COL,IH)    =HOLD(COL,IH)/SUM
8717                    CONTINUE
		END IF

C       Increment hold buffer pointer.  If the buffer is full, as
C       it generally will be, flush the oldest HOLD row back into the
C       image.
		IH      =IH+1
		IF (IH .GT. MAXHOLD) THEN
C                  Reset hold pointer
			IH      =1
			FILL    =.TRUE.
		END IF

		IF (FILL) THEN
C                  Flush out buffer
			NRFILL  =ROW-MAXHOLD+1
			DO 8718 COL=1, NCOL
				A(COL,NRFILL)   =HOLD(COL,IH)
8718                    CONTINUE
		END IF

8711      CONTINUE

C       The convolution is complete.  Flush out any image rows left in the
C       HOLD buffer.
	  IF (FILL) THEN
		DO 8719 I=IH+1, MAXHOLD
			NRFILL  =NROW-MAXHOLD+1+I-IH
			DO 8720 COL=1, NCOL
				A(COL,NRFILL)   =HOLD(COL,I)
8720                    CONTINUE
8719            CONTINUE
	  END IF

	  DO 8721 I=1, IH-1
		NRFILL  =NROW-IH+I+1
		DO 8722 COL=1, NCOL
			A(COL,NRFILL)   =HOLD(COL,I)
8722            CONTINUE
8721      CONTINUE

        END IF

	RETURN
	END

      SUBROUTINE RUNMEAN(A,NR,NC,B,MR,MC)

C     Running-mean filtering of an image.
C
C     Each pixel is substituted by the mean of 2*MR+1 x 2*MC+1 pixels
C     around it. The normalization is adjusted near the edges.
C     Input:  A(NC,NR) The image containing the data to be convolved.
C             MC, MR   The radia of the running-mean filter.
C             B        Working space (at least 8*max(nr,nc) bytes).
C     Author: J.Jesus Gonzalez             2/14/89

      DIMENSION A(NC,NR)
C     Real*8 needed to accumulate running-mean 'in situ'.
      REAL*8 B(*), S

      IF (MC.LT.1) GOTO 100

      MT = 2*MC+1
      N = MIN0(MC,NC)
      IS = MIN0(1+MC,MAX0(NC-MC,0))
      IE = MAX0(NC-MC,IS)
      DO 8701 IR =1, NR

          DO 8702 I = 1, NC
8702      B(I) = DBLE(A(I,IR))

          S = B(1)
          DO 8703 I=2,N,1
8703      S = S + B(I)

          DO 8704 I=1,IS,1
              S = S + B(I+MC)
              A(I,IR) = S/(N+I)
8704      CONTINUE

          DO 8705 I=IS+1,IE,1
              S = S + B(I+MC) - B(I-MC-1)
              A(I,IR) = S/MT
8705      CONTINUE

          DO 8706 I=IE+1,NC,1
              J = I - MC - 1
              IF (J.GE.1) S = S - B(J)
              A(I,IR) = S/(NC-MAX0(J,0))
8706      CONTINUE

8701  CONTINUE

100   IF (MR.LT.1) RETURN

      MT = 2*MR+1
      N = MIN0(MR,NR)
      IS = MIN0(1+MR,MAX0(NR-MR,0))
      IE = MAX0(NR-MR,IS)
      DO 8801 IC =1, NC

          DO 8802 I=1,NR
8802      B(I) = DBLE(A(IC,I))

          S = B(1)
          DO 8803 I=2,N,1
8803      S = S + B(I)

          DO 8804 I=1,IS,1
              S = S + B(I+MR)
              A(IC,I) = S/(N+I)
8804      CONTINUE

          DO 8805 I=IS+1,IE,1
              S = S + B(I+MR) - B(I-MR-1)
              A(IC,I) = S/MT
8805      CONTINUE

          DO 8806 I=IE+1,NR,1
              J = I - MR - 1
              IF (J.GE.1) S = S - B(J)
              A(IC,I) = S/(NR-MAX0(J,0))
8806      CONTINUE
8801  CONTINUE

      RETURN
      END


      SUBROUTINE CONVOLVEIT(OUT,JSR,JER,JSC,JEC,
     &                      FUNC,ISR,IER,ISC,IEC,
     &                      IN, NROW, NCOL)

C   Subroutine to perform full blown 2D convolution!
C   Parameters:
C              IN(JSC:JEC,JSR:JER)    input array
C              OUT(JSC:JEC,JSR:JER)   output array
C              FUNC(ISC:IEC,ISR:IER)  convolution array

      REAL OUT(JSC:JEC,JSR:JER)
      REAL IN(JSC:JEC,JSR:JER)
      REAL FUNC(ISC:IEC,ISR:IER)
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C   First zero out the output array
      DO 4401 J = JSR, JER
        DO 4402 I = JSC, JEC
          OUT(I,J) = 0.
4402    CONTINUE
4401  CONTINUE

C   We will assume that the convolution array is centered on pixel (0,0).
C   Otherwise the convolved image will be shifted relative to the input
C   image and in fact, a good chunk of it might fall outside of the new
C   image boundaries, which are identical to the old ones.

C   Loop over the convolution pixels

      DO 5501 JC = ISR, IER
        DO 5502 IC = ISC, IEC

          IF (NOGO) RETURN
          IF (FUNC(IC,JC) .EQ. 0) GOTO 5502

          JS = MAX(JSR,JSR-JC)
          JE = MIN(JER,JER-JC)
          IS = MAX(JSC,JSC-IC)
          IE = MIN(JEC,JEC-IC)

C     Loop over the old image pixels and accumulate the sum
          DO 5503 J = JS,JE
            DO 5504 I = IS,IE

	      IF (IN(I,J) .EQ. 0) GOTO 5504

              OUT(I+IC,J+JC) = OUT(I+IC,J+JC) +
     &                         FUNC(IC,JC)*IN(I,J)

5504        CONTINUE
5503      CONTINUE

5502    CONTINUE
5501  CONTINUE

      RETURN
      END
