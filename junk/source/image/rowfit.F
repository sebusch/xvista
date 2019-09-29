#include "Vista.h"
      SUBROUTINE ROWFIT(A,ISROW,IEROW,ISCOL,IECOL)
      REAL*4 A(ISCOL:IECOL,ISROW:IEROW)
C-----------------------------------------------------------------------C
C  Clips, Substitutes, substracts, or divides, each row by a fit from a C
C  menu of functions (all linear in the coefficients).                  C
C                                                                       C
C    Keywords:                                                          C
C    BOX=b      Box within which the pixels will be substituted.        C
C    FIT=cs,ce  Column limits of up to 4 fit regions (all within box).  C
C    NCOEF=m    Number of coefficients to fit, (default 1, a constant). C
C    LOAD=bp    Loads in buffer bp the coefficients, the errors, and theC
C               inbiased standard deviation from the fit.               C
C    UNLOAD=bp  Uses pre-loaded coefficients in buffer bp to evaluate   C
C               the fit and operate on the image (sub,div,...).         C
C    CLIP = f   Clips pixels that deviate more than f sigmas from fit.  C
C    CLIP=fn,fp Clips (Ima-Fit)<-fn and (Ima-Fit)>fp deviators.         C
C    SUB        Subtract the fit from the image.                        C
C    DIV        Divide the image by the fit.                            C
C    FITONLY    Clips (CLIP) and/or saves (LOAD) the fit, substituting  C
C               only clipped pixels (CLIP). SUB or DIV are ignored.     C
C    POLY       Fit polinomia of order NCOEF-1.                         C
C    XBUF=KM    Use as abscisae not the column number but the spectrum  C
C               in buffer KM.                                           C
C    WBUF=LM    Use the values of buffer LM as weights for the fit.     C
C    FOUR       Fit a Fourier series up to order (NCOEF-1)/2.           C
C    SILENT     Avoid print out about clipped pixels (with CLIP option) C
C                                                                       C
C    Written by :  J. Jesus Gonzalez,        May 29, 1987.              C
C                  Lick Observatory - UCSC                              C
C                  Santa Cruz, CA 95062                                 C
C    Modification History:                                              C
C               Made it allocate virtual memory for larger images than  C
C               the workspece allowed.   September 9, 96 (J.J.G.)       C
C               Added CLIP=fn,fp option, May 98 (J.J.G.)                C
C-----------------------------------------------------------------------C
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      PARAMETER (MXPAR=18,NLIMMAX=4,MXPIX=WORKSIZE-2*NLIMMAX-1)
      REAL*4 F(2), X(MXPIX), FCLIP(2)
      INTEGER LIM(2,NLIMMAX), OPER
      LOGICAL KEYCHECK, LOAD, UNLOAD, CLIP, SILENT
      CHARACTER*80 PARM, CCHEAD, FSTRCAT
      COMMON / WORK / LIM, X
      COMMON / SILENT / SILENT

C     List allowed keywords.
      CALL KEYINIT
      CALL KEYDEF('FIT=')
      CALL KEYDEF('NCOEF=')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('LOAD=')
      CALL KEYDEF('UNLOAD=')
      CALL KEYDEF('DIV')
      CALL KEYDEF('SUB')
      CALL KEYDEF('POLY')
      CALL KEYDEF('FOUR')
      CALL KEYDEF('FITONLY')
      CALL KEYDEF('CLIP=')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('XBUF=')
      CALL KEYDEF('WBUF=')

C     Exit if we were given a keyword we don''t understand.
      IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
      END IF

C     Set up default values: don''t save coefficients nor errors; do not
C     get fit from saved coefficients; fit is polynomial; substitute image
C     by the fit; do not clip; fit a constant by default; reset fit-region
C     counter and the box number.
      LOAD = .FALSE.
      UNLOAD = .FALSE.
      IFUNC = 1
      OPER = 0
      CLIP = .FALSE.
      SILENT = .FALSE.
      NCOEF = 1
      NLIM = 0
      NB = 0
      LM = 0
      KM = 0

      DO 8701 I= 1, NCON
	  IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
	      CALL ASSIGN(WORD(I),Z,PARM)
	      NB = NINT(Z)
	      IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:6) .EQ. 'NCOEF=') THEN
	      CALL ASSIGN(WORD(I),Z,PARM)
	      NCOEF = MAX0(NINT(Z),1)

	  ELSE IF (WORD(I) .EQ. 'SUB') THEN
	      OPER = -1

	  ELSE IF (WORD(I) .EQ. 'FOUR') THEN
	      IFUNC = 2

	  ELSE IF (WORD(I) .EQ. 'DIV') THEN
	      OPER = 1

	  ELSE IF (WORD(I)(1:5) .EQ. 'LOAD=') THEN
	      CALL ASSIGN(WORD(I),Z,PARM)
	      JM = NINT(Z)
	      LOAD = .TRUE.

	  ELSE IF (WORD(I)(1:7) .EQ. 'UNLOAD=') THEN
	      CALL ASSIGN(WORD(I),Z,PARM)
	      JM = NINT(Z)
	      UNLOAD = .TRUE.

	  ELSE IF (WORD(I)(1:4) .EQ. 'FIT=') THEN
	      CALL ASSIGNV(WORD(I),2,F,NF,PARM)
	      IF (XERR) RETURN
	      IF (NF .NE. 2) THEN
		  PRINT *,' Need a pair of limits for FIT='
		  XERR = .TRUE.
		  RETURN
	      END IF
	      NLIM = NLIM + 1
	      IF(NLIM .GT. NLIMMAX) THEN
		  PRINT *,' Too many fitting regions.'
		  PRINT *,NLIMMAX,' FIT= at the most.'
		  XERR = .TRUE.
		  RETURN
	      END IF
	      LIM(1,NLIM) = NINT(MIN(F(1),F(2)))
	      LIM(2,NLIM) = NINT(MAX(F(1),F(2)))

	  ELSE IF (WORD(I)(1:5) .EQ. 'CLIP=') THEN
	      CALL ASSIGNV(WORD(I),2,F,NF,PARM)
	      IF (XERR) RETURN
	      CLIP = .TRUE.
              IF (NF .GT. 1) THEN
		  FCLIP(1) = -ABS(F(1))
		  FCLIP(2) = +ABS(F(2))
	      ELSE IF (NF .EQ. 1) THEN
		  FCLIP(1) = -ABS(F(1))
		  FCLIP(2) = -FCLIP(1)
              ELSE
		  FCLIP(1) = -5.0
		  FCLIP(2) = -FCLIP(1)
	      END IF

	  ELSE IF (WORD(I)(1:5) .EQ. 'XBUF=') THEN
	      CALL ASSIGN(WORD(I),Z,PARM)
	      KM = NINT(Z)

	  ELSE IF (WORD(I)(1:5) .EQ. 'WBUF=') THEN
	      CALL ASSIGN(WORD(I),Z,PARM)
	      LM = NINT(Z)

	  ELSE IF (WORD(I) .EQ. 'FITONLY') THEN
	      OPER = 100

	  ELSE IF (WORD(I) .EQ. 'SILENT') THEN
	      SILENT = .TRUE.

	  END IF
8701  CONTINUE

C--   --- Check for the maximun number of coefficients. ----c
      IF (NCOEF.GT.MXPAR) THEN
	  PRINT *,' Can fit ', MXPAR,' coefficients at the most.'
	  XERR = .TRUE.
	  RETURN
      END IF

C--   --- Get BOX limits, do the whole image if no box given. ----C
      IF (NB.EQ.0) THEN
	  ISY = ISROW
	  IEY = IEROW
	  ISX = ISCOL
	  IEX = IECOL
      ELSE
	  CALL GETBOX(NB,ICOORD(1,IM),ISY,IEY,ISX,IEX)
	  IF (XERR) RETURN
      END IF

C--   --------------- Fill the absisae array (same for all rows). ----C
       IF (LM.NE.0) THEN
	   CALL GETIMDATA(LOCW,LSR,LER,I,LSC,LEC,J,LM)
	   IF (XERR) RETURN
C--        --- Operate only in the intersection of the two images. ----C
	   ISX=MAX0(ISX,LSC)
	   IEX=MIN0(IEX,LEC)
	   NX = IEX-ISX+1
       END IF

       IF (KM.EQ.0) THEN
	   NX = IEX-ISX+1
	   DO 9070 J=1,NX
	       X(J) = REAL(ISX+J-1)
9070       CONTINUE
           IF (NX.GT.MXPIX) THEN
               PRINT*,'Can not work with more than ',MXPIX,' colums.'
               XERR = .TRUE.
               RETURN
           END IF
       ELSE
	   CALL GETIMDATA(LOCX,KSR,KER,I,KSC,KEC,J,KM)
	   IF (XERR) RETURN
C--        --- Operate only in the intersection of the two images. ----C
	   ISX=MAX0(ISX,KSC)
	   IEX=MIN0(IEX,KEC)
	   NX = IEX-ISX+1
           IF (NX.GT.MXPIX) THEN
               PRINT*,'Can not work with more than ',MXPIX,' colums.'
               XERR = .TRUE.
               RETURN
           END IF
	   CALL CC2COPIO(X,1,NX,LOCX,1,KEC-KSC+1,0,ISX-KSC)
       END IF

C--   ------- Check that all fitting regions are within the image. ----C
      IF (NLIM.EQ.0) THEN
C--       If no fitting limits given, use all columms
	  NLIM = 1
	  LIM(1,1) = ISX
	  LIM(2,1) = IEX
      ELSE
	  DO 9072 L=1,NLIM
	      IF (LIM(2,L).LT.ISX .OR. LIM(1,L).GT.IEX) THEN
		  PRINT*, ' Fit region outside image limits.'
		  XERR = .TRUE.
		  RETURN
	      ELSE
		  LIM(1,L) = MAX0(LIM(1,L),ISX)
		  LIM(2,L) = MIN0(LIM(2,L),IEX)
	      END IF
9072      CONTINUE
       END IF

C--   ----------------- Create the image to save the coefficients ----c
      IF (LOAD) THEN
	  IF (UNLOAD) THEN
	      PRINT *,' Can not fit and unload at the same time.'
	      XERR = .TRUE.
	      RETURN
	  END IF

C--       --- Check the parameter''s buffer. ----c
	  IBUF(2) = JM
	  NINTS = 2
	  IF (JM.LT.1 .OR. JM.GT.MAXIM) THEN
	      PRINT *, ' Invalid buffer number', JM
	      XERR = .TRUE.
	      RETURN
	  END IF
C--       --- Make a copy of the current image header.
	  TEMPHEAD = ' '
	  CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

C--       -- Note the size of the new image. It will hold for each row, the
C--       coefficients in columns 1 to NCOEF, the fit standard deviation in
C--       column 0, and the error on the parameter in columns -1 to - NCOEF.
	  CALL INHEADSET('NAXIS2',IEY-ISY+1,TEMPHEAD)
	  CALL FHEADSET('CRVAL2',DBLE(ISY),TEMPHEAD)
	  CALL INHEADSET('CRPIX2',1,TEMPHEAD)
	  CALL FHEADSET('CDELT2',1.D0,TEMPHEAD)
	  CALL UNFIT('CTYPE2',TEMPHEAD)
	  CALL INHEADSET('NAXIS1',2*NCOEF+1,TEMPHEAD)
	  CALL INHEADSET('CRVAL1',-NCOEF,TEMPHEAD)
	  CALL INHEADSET('CRPIX1',1,TEMPHEAD)
	  CALL FHEADSET('CDELT1',1.D0,TEMPHEAD)
	  CALL CHEADSET('CTYPE1','PIXEL',TEMPHEAD)

	  CALL CHEAD('OBJECT',TEMPHEAD,CCHEAD)
	  PARM = 'Coefficients of '// CCHEAD
	  CALL CHEADSET('OBJECT',PARM,TEMPHEAD)

C--       -------------- Allocate the buffer for the new image. ---c
	  CALL CREATEIM(LOCNEW,JSR,JER,JSC,JEC,2,TEMPHEAD,.TRUE.)
	  IF (XERR) RETURN

      ELSE IF (UNLOAD) THEN
	  IF (.NOT.BUFF(JM)) THEN
	      PRINT *,' There are no fit-coefficients in buffer ',JM
	      XERR = .TRUE.
	      RETURN
	  END IF

	  IF (OPER.EQ.100) THEN
	      PRINT*,' FITONLY and UNLOAD are incosistent keywords'
	      XERR = .TRUE.
	      RETURN
	  END IF

	  IF (CLIP) PRINT *, ' WARNING: CLIP does not operate ',
     &         'with the UNLOAD keyword.'

C--       --- Get # of parameters and maximumn number of rows. ----C
	  NCOEF = -ICOORD(IXSC,JM)
	  JSY = ICOORD(IYSR,JM)
	  JEY = ICOORD(IYER,JM)
	  ISY = MAX0(ISY,JSY)
	  IEY = MIN0(IEY,JEY)

      ELSE
C         Abscissa Scale-factor and Zero-point
	  IF (KM.EQ.0) THEN
	     XS = (X(NX)-X(1))/2.0
	     X0 = (X(NX)+X(1))/2.0
	  ELSE
	     X0 = 0.0
	     XS = 0.0
	     DO 9076 I=1,NX
		X0 = X0 + X(I)
		XS = XS + X(I)*X(I)
9076         CONTINUE
	     X0 = X0/REAL(NX)
	     XS = SQRT(ABS(X0*X0-XS/REAL(NX)))
	  END IF
	  DO 9077 I=1,NX
	      X(I) = (X(I)-X0)/XS
9077      CONTINUE
	  LOCNEW = IMLOC(IM)
      END IF

C--   ------------------------------ Get run-time virtual memory. ----C
      NBYTES = 8*NCOEF*NX
      CALL CCALLOC(NBYTES,LPF)
      IF (LPF .EQ. 0) THEN
         PRINT *, 'Error allocating virtual memory'
         XERR = .TRUE.
         RETURN
      END IF

C--   ----- Fill the arrays of abscisa factors (row-independent). ----C
      IF (.NOT.UNLOAD .AND. OPER .EQ. 100) THEN
C--       -- Fill only within fitting regions.
	  DO 8702 L=1,NLIM
	      I = LIM(1,L) - ISX + 1
	      J = LIM(2,L) - ISX + 1
	      CALL CCPFUNC(LPF,X,NCOEF,NX,I,J,IFUNC)
8702      CONTINUE
      ELSE
C--       -- Fill for all columns.
	  CALL CCPFUNC(LPF,X,NCOEF,NX,1,NX,IFUNC)
      END IF

C--   ----------------------- Perform the fit, or unload the fit. ----C
      IF (UNLOAD) THEN
	  CALL CCUNLOADFIT1(A,ISROW,IEROW,ISCOL,IECOL,LPF,ISX,IEX,ISY,
     &          IEY,NCOEF,OPER,IMLOC(JM),JSY,JEY)
      ELSE IF (LM.EQ.0) THEN
	  CALL CCLOADFIT1(A,ISROW,IEROW,ISCOL,IECOL,LPF,ISX,IEX,ISY,
     &          IEY,LIM,NLIM,NCOEF,OPER,LOAD,X,LOCNEW,CLIP,FCLIP)
      ELSE
	  CALL CCLOADFIT2(A,ISROW,IEROW,ISCOL,IECOL,LPF,ISX,IEX,ISY,
     &      IEY,LIM,NLIM,NCOEF,OPER,LOAD,X,LOCNEW,CLIP,FCLIP,
     &      LOCW,LSC,LEC)
      END IF

C--   Release the run-time memory.
      IF (NBYTES .NE. 0) CALL CCFREE(NBYTES,LPF)

      RETURN
      END

C-----------------------------------------------------------------------C
      SUBROUTINE LOADFIT1(A,ISR,IER,ISC,IEC,PF,ISX,IEX,ISY,IEY,
     &          LIM,NLIM,NCOEF,OPER,LOAD,Y,P,CLIP,FCLIP)
C-----------------------------------------------------------------------C
C This is the subroutine that actually does the row-by-row fit.         C
C LOAD =.TRUE. Saves the coefficients of the fit, their errors, and the C
C        standard deviation from the fit in array P.                    C
C CLIP =.TRUE. Clips out pixels that deviate more than FCLIP sigmas fromC
C        the fit and iterates the fit until no more pixels are clipped. C
C OPER=0 returns fit on A, OPER=-1 returns A-fit on A and OPER=+1 A/fit C
C        on A. Otherwise the image is just clipped (clip=.true.) and/or C
C        the fit parameters are saved in array P.                       C
C-----------------------------------------------------------------------C
      PARAMETER (MXPAR=18,NLIMMAX=4)
      LOGICAL LOAD, CLIP, SILENT
      INTEGER LIM(2,NLIMMAX), INDX(MXPAR), OPER
      REAL*4 FCLIP(2)
      REAL*4 A(ISC:IEC,ISR:IER), P(-NCOEF:NCOEF,ISY:IEY), Y(ISX:IEX)
      REAL*8 MATRIX(MXPAR,MXPAR), PAR(MXPAR), TMP, PF(NCOEF,ISX:IEX)
      COMMON / MATRIX / MATRIX, PAR, TMP, INDX
      COMMON / SILENT / SILENT

C--   --- Accumulate the matrix of the normal equations. Since it is the
C--   --- same matrix for all rows we LU-decompose it only once. ----C
C     Initialize the (symmetric) matrix:
      DO 8702 J=1,NCOEF
	  DO 8703 K=1,J,1
	      MATRIX(K,J) = 0.0D0
8703      CONTINUE
8702  CONTINUE

      NPTS = 0
C     Loop over fitting regions:
      DO 8704 L = 1, NLIM
	  DO 8705 IX = LIM(1,L), LIM(2,L)
	      NPTS = NPTS + 1
	      DO 8706 J=1,NCOEF
		  DO 8707 K=1,J
		      MATRIX(K,J)=MATRIX(K,J)+PF(K,IX)*PF(J,IX)
8707              CONTINUE
8706          CONTINUE
8705      CONTINUE
8704  CONTINUE
      NPTS = MAX0(1,NPTS-NCOEF)

C     Fill in the symmetric matrix.
      DO 8708 J=1,NCOEF
	  DO 8709 K=J+1,NCOEF,1
	      MATRIX(K,J) = MATRIX(J,K)
8709      CONTINUE
8708  CONTINUE

C     LU-decompose matrix only once since it is the same for all rows.
      CALL LUDCMP(MATRIX,NCOEF,MXPAR,INDX,TMP,XERR)

C--   --------------------  Solve the normal equations for each row. ----C
      NCLIPTOT = 0
      NITERTOT = 0
      DO 8710 IY = ISY, IEY
c         --- Accumulate the vector of the normal equations.
	  NITER = 0
100       DO 8711 J=1,NCOEF
	      PAR(J) = 0.0D0
8711      CONTINUE

	  DO 8712 L = 1, NLIM
	      DO 8713 IX = LIM(1,L), LIM(2,L)
		  TMP = DBLE(A(IX,IY))
		  DO 8714 J = 1, NCOEF
		      PAR(J) = PAR(J) + PF(J,IX)*TMP
8714              CONTINUE
8713          CONTINUE
8712      CONTINUE

C--       -------------------- Solve the normal equations. ----C
	  CALL LUBKSB(MATRIX,NCOEF,MXPAR,INDX,PAR)

C--       -- Evaluate the standard deviation from the fit. ----C
	  IF (LOAD .OR. CLIP) THEN
	      SIGMA = 0.0E0
	      DO 8715 L = 1,NLIM
		  DO 8716 IX = LIM(1,L), LIM(2,L)
		      TMP = PF(1,IX)*PAR(1)
		      DO 8717 J=2,NCOEF,1
			  TMP = TMP + PAR(J)*PF(J,IX)
8717                  CONTINUE
		      Y(IX) = REAL(TMP)
		      SIGMA = SIGMA + (A(IX,IY)-Y(IX))**2
8716              CONTINUE
8715          CONTINUE
	      SIGMA = SQRT(SIGMA/REAL(NPTS))

C--           -- Clip highly deviant pixels and fit again.
	      IF (CLIP) THEN
		  NCLIP = 0
		  DYMAX = FCLIP(2)*SIGMA
		  DYMIN = FCLIP(1)*SIGMA
		  DO 8802 L=1,NLIM
		      DO 8801 IX=LIM(1,L),LIM(2,L)
                      DY = A(IX,IY) - Y(IX)
		      IF (DY.GT.DYMAX .OR. DY.LT.DYMIN) THEN
			  A(IX,IY) = Y(IX)
			  NCLIP = NCLIP + 1
		      END IF
8801                  CONTINUE
8802              CONTINUE
		  IF (NCLIP.GT.0) THEN
		      NCLIPTOT = NCLIPTOT + NCLIP
                      IF (NITER.LT.5) THEN
                          NITER = NITER + 1
                          NITERTOT = NITERTOT + 1
                          GO TO 100
                      END IF
		  END IF
	      END IF

C--           -- Save coefficients and standard deviation in array P.
	      IF (LOAD) THEN
		  DO 8719 K=1,NCOEF
		      P(K,IY) = REAL(PAR(K))
8719              CONTINUE
		  P(0,IY) = SIGMA
	      END IF

C--           ------------------ Evaluate the rest of vector Y. ----C
	      IF (IABS(OPER).GT.1) GOTO 8710
	      DO 8803 IX=ISX,IEX
		  DO 8804 L=1,NLIM,1
		  IF (IX.GE.LIM(1,L).AND.IX.LE.LIM(2,L)) GOTO 8803
8804              CONTINUE
		  TMP = PF(1,IX)*PAR(1)
		  DO 8805 J=2,NCOEF,1
		      TMP = TMP + PAR(J)*PF(J,IX)
8805              CONTINUE
		  Y(IX) = REAL(TMP)
8803          CONTINUE

	  ELSE
C--           ------------------- Evaluate the fit in vector Y. ----C
	      DO 8806 IX = ISX, IEX
		  TMP = PF(1,IX)*PAR(1)
		  DO 8807 J=2,NCOEF,1
		      TMP = TMP + PAR(J)*PF(J,IX)
8807              CONTINUE
		  Y(IX) = REAL(TMP)
8806          CONTINUE
	  END IF

C--       ----------------------- Operate onto the output image. ----C
C--       Save (oper=0), Subtract (oper=-1) or Divide by (oper=1), fit.
	  IF (OPER.EQ.0) THEN
	      DO 8720 IX=ISX,IEX
		  A(IX,IY) = Y(IX)
8720          CONTINUE

	  ELSE IF (OPER.EQ.-1) THEN
	      DO 8721 IX=ISX,IEX
		  A(IX,IY) = A(IX,IY) - Y(IX)
8721          CONTINUE

	  ELSE IF (OPER.EQ.1) THEN
	      DO 8722 IX=ISX,IEX
		  IF (Y(IX).NE.0) THEN
		      A(IX,IY) = A(IX,IY)/Y(IX)
		  ELSE
		      A(IX,IY) = 0.0E0
		  END IF
8722          CONTINUE
	  END IF
8710  CONTINUE

C--   --- Invert matrix to estimate error on the parameters. ----C
C--   - Use PAR as unitary vector in the column-by-column inversion.
      IF (CLIP .AND. (.NOT.SILENT)) THEN
	  AVEITER = REAL(NITERTOT)/REAL(IEY-ISY+1)
	  PRINT 101, NCLIPTOT, AVEITER
101       FORMAT(I4,' Clippings in an average of',f7.2,' iterations per row.')
      END IF
      IF (.NOT. LOAD) RETURN

      DO 8723 I = 1, NCOEF
	  DO 8724 J = 1, NCOEF
	      PAR(J) = 0.0D0
8724      CONTINUE
	  PAR(I) = 1.0D0
	  CALL LUBKSB(MATRIX,NCOEF,MXPAR,INDX,PAR)
C--       Save Error on the I-th parameter at row J in P(-I,J).
	  TMPS = SQRT(ABS(REAL(PAR(I))))
	  DO 8725 IY = ISY,IEY
	      P(-I,IY) = P(0,IY)*TMPS
8725      CONTINUE
8723  CONTINUE
      RETURN
      END

C     *********************************************************
      SUBROUTINE UNLOADFIT1(A,ISR,IER,ISC,IEC,PF,ISX,IEX,ISY,IEY,
     &          NCOEF,OPER,P,JSY,JEY)
C     Read fit coefficients from array P, and operates in the image
C     A. OPER=0 returns fit on A, -1 returns A-fit, =1 returns A/fit.

      INTEGER OPER
      REAL*4 A(ISC:IEC,ISR:IER), P(-NCOEF:NCOEF,JSY:JEY)
      REAL*8 PF(NCOEF,ISX:IEX), TMP

C--   --- Evaluate the function onto the output image. ----C
      IF (OPER .EQ. 0) THEN
	  DO 8726 IY = ISY, IEY
	      DO 8727 IX = ISX, IEX
		  TMP = PF(1,IX)*DBLE(P(1,IY))
		  DO 8728 J=2,NCOEF,1
		      TMP = TMP + DBLE(P(J,IY))*PF(J,IX)
8728              CONTINUE
		  A(IX,IY) = REAL(TMP)
8727          CONTINUE
8726      CONTINUE

      ELSE IF (OPER .EQ. -1) THEN
	  DO 8729 IY = ISY, IEY
	      DO 8730 IX = ISX, IEX
		  TMP = PF(1,IX)*DBLE(P(1,IY))
		  DO 8731 J=2,NCOEF,1
		      TMP = TMP + DBLE(P(J,IY))*PF(J,IX)
8731              CONTINUE
		  A(IX,IY) = A(IX,IY) - REAL(TMP)
8730          CONTINUE
8729      CONTINUE

      ELSE IF (OPER .EQ. 1) THEN
	  DO 8732 IY= ISY,IEY
	      DO 8733 IX = ISX, IEX
		  TMP = PF(1,IX)*DBLE(P(1,IY))
		  DO 8734 J=2,NCOEF,1
		      TMP = TMP + DBLE(P(J,IY))*PF(J,IX)
8734              CONTINUE
		  IF (REAL(TMP).NE.0.) THEN
		      A(IX,IY) = A(IX,IY)/REAL(TMP)
		  ELSE
		      A(IX,IY) = 0.0E0
		  END IF
8733          CONTINUE
8732      CONTINUE
      END IF
      RETURN
      END

C     *********************************************************
      SUBROUTINE PFUNC(PF,XF,NP,NX,I1,I2,IOPTION)
C     To evaluate the linear functions at every x-value only once.
C     IOPTION=1 for Polynomial function, =2 for Fourier series.

      REAL*8 PF(NP,NX), X, SCALE
      REAL XF(NX)

      IF (IOPTION.EQ.1) THEN
	  DO 8735 I=I1,I2
	      PF(1,I) = 1.0D0
	      X = DBLE(XF(I))
	      DO 8736 J=2,NP,1
		  PF(J,I) = PF(J-1,I)*X
8736          CONTINUE
8735      CONTINUE

      ELSE IF (IOPTION.EQ.2) THEN
	  SCALE = 8.0D0*DATAN(1.0D0)
	  DO 8739 I=I1,I2
	      X = SCALE*DBLE(XF(I))
	      PF(1,I) = 1.0D0
	      DO 8740 J=2,NP,2
		  PF(J,I) = DCOS(X)
		  PF(J+1,I) = DSIN(X)
		  X = X + X
8740          CONTINUE
8739      CONTINUE
      END IF
      RETURN
      END

C-----------------------------------------------------------------------C
      SUBROUTINE LOADFIT2(A,ISR,IER,ISC,IEC,PF,ISX,IEX,ISY,IEY,
     &          LIM,NLIM,NCOEF,OPER,LOAD,Y,P,CLIP,FCLIP,W,ISW,IEW)
C-----------------------------------------------------------------------C
C This is the subroutine that actually does the row-by-row fit.         C
C LOAD =.TRUE. Saves the coefficients of the fit, their errors, and the C
C        standard deviation from the fit in array P.                    C
C CLIP =.TRUE. Clips out pixels that deviate more than FCLIP sigmas fromC
C        the fit and iterates the fit until no more pixels are clipped. C
C OPER=0 returns fit on A, OPER=-1 returns A-fit on A and OPER=+1 A/fit C
C        on A. Otherwise the image is just clipped (clip=.true.) and/or C
C        the fit parameters are saved in array P.                       C
C-----------------------------------------------------------------------C
      PARAMETER (MXPAR=18,NLIMMAX=4)
      LOGICAL LOAD, CLIP, SILENT
      INTEGER LIM(2,NLIMMAX), INDX(MXPAR), OPER
      REAL*4 FCLIP(2)
      REAL*4 A(ISC:IEC,ISR:IER), P(-NCOEF:NCOEF,ISY:IEY), Y(ISX:IEX)
      REAL*4 W(ISW:IEW)
      REAL*8 MATRIX(MXPAR,MXPAR), PAR(MXPAR), TMP, PF(NCOEF,ISX:IEX)
      REAL*8 B(MXPAR)
      COMMON / MATRIX / MATRIX, PAR, TMP, INDX
      COMMON / SILENT / SILENT

C--   --- Accumulate the matrix of the normal equations. Since it is the
C--   --- same matrix for all rows we LU-decompose it only once. ----C
C     Initialize the (symmetric) matrix:
      DO 8702 J=1,NCOEF
	  DO 8703 K=1,J,1
	      MATRIX(K,J) = 0.0D0
8703      CONTINUE
8702  CONTINUE

C     Loop over fitting regions:
      NPTS = 0
      SUMW = 0.0
      DO 8704 L = 1, NLIM
	  DO 8705 IX = LIM(1,L), LIM(2,L)
	      NPTS = NPTS + 1
              SUMW = SUMW + W(IX)
	      DO 8706 J=1,NCOEF
                  TMP = PF(J,IX)*W(IX)
		  DO 8707 K=1,J
		      MATRIX(K,J)=MATRIX(K,J)+PF(K,IX)*TMP
8707              CONTINUE
8706          CONTINUE
8705      CONTINUE
8704  CONTINUE
      WNORM = REAL(NPTS)/(SUMW*MAX(1,NPTS-NCOEF))

C     Fill in the symmetric matrix.
      DO 8708 J=1,NCOEF
	  DO 8709 K=J+1,NCOEF,1
	      MATRIX(K,J) = MATRIX(J,K)
8709      CONTINUE
8708  CONTINUE

C     LU-decompose matrix only once since it is the same for all rows.
      CALL LUDCMP(MATRIX,NCOEF,MXPAR,INDX,TMP,XERR)

C--   --------------------  Solve the normal equations for each row. ----C
      NCLIPTOT = 0
      NITERTOT = 0
      DO 8710 IY = ISY, IEY
c         --- Accumulate the vector of the normal equations.
          DO 8711 J=1,NCOEF
	      PAR(J) = 0.0D0
8711      CONTINUE

	  DO 8712 L = 1, NLIM
	      DO 8713 IX = LIM(1,L), LIM(2,L)
		  TMP = DBLE(A(IX,IY)*W(IX))
		  DO 8714 J = 1, NCOEF
		      PAR(J) = PAR(J) + PF(J,IX)*TMP
8714              CONTINUE
8713          CONTINUE
8712      CONTINUE

          IF (CLIP) THEN
              DO 8902 J=1,NCOEF
                 B(J) = PAR(J)
8902          CONTINUE
          END IF

C--       -------------------- Solve the normal equations. ----C
	  CALL LUBKSB(MATRIX,NCOEF,MXPAR,INDX,PAR)

C--       -- Evaluate the standard deviation from the fit. ----C
	  IF (LOAD .OR. CLIP) THEN
              NITER = 0
100	      SIGMA = 0.0E0
	      DO 8715 L = 1,NLIM
		  DO 8716 IX = LIM(1,L), LIM(2,L)
		      TMP = PF(1,IX)*PAR(1)
		      DO 8717 J=2,NCOEF,1
			  TMP = TMP + PAR(J)*PF(J,IX)
8717                  CONTINUE
		      Y(IX) = REAL(TMP)
		      SIGMA = SIGMA + W(IX)*(A(IX,IY)-Y(IX))**2
8716              CONTINUE
8715          CONTINUE
	      SIGMA = SQRT(SIGMA*WNORM)

C--           -- Clip highly deviant pixels and fit again.
	      IF (CLIP) THEN
		  NCLIP = 0
		  DYMAX = FCLIP(2)*SIGMA
		  DYMIN = FCLIP(1)*SIGMA
		  DO 8802 L=1,NLIM
		      DO 8801 IX=LIM(1,L),LIM(2,L)
                      DY = A(IX,IY) - Y(IX)
		      IF (DY.GT.DYMAX .OR. DY.LT.DYMIN) THEN
			  A(IX,IY) = Y(IX)
                          TMP = DY*W(IX)
                          DO 8904 J=1,NCOEF
                              B(J) = B(J)-TMP
8904                      CONTINUE
			  NCLIP = NCLIP + 1
		      END IF
8801                  CONTINUE
8802              CONTINUE
		  IF (NCLIP.GT.0) THEN
		      NCLIPTOT = NCLIPTOT + NCLIP
                      IF (NITER.LT.5) THEN
                          NITER = NITER + 1
                          NITERTOT = NITERTOT + 1
                          DO 8906 J=1,NCOEF
                              PAR(J) = B(J)
8906                      CONTINUE
                          CALL LUBKSB(MATRIX,NCOEF,MXPAR,INDX,PAR)
                          GO TO 100
                      END IF
		  END IF
	      END IF

C--           -- Save coefficients and standard deviation in array P.
	      IF (LOAD) THEN
		  DO 8719 K=1,NCOEF
		      P(K,IY) = REAL(PAR(K))
8719              CONTINUE
		  P(0,IY) = SIGMA
	      END IF

C--           ------------------ Evaluate the rest of vector Y. ----C
	      IF (IABS(OPER).GT.1) GOTO 8710
	      DO 8803 IX=ISX,IEX
		  DO 8804 L=1,NLIM,1
		  IF (IX.GE.LIM(1,L).AND.IX.LE.LIM(2,L)) GOTO 8803
8804              CONTINUE
		  TMP = PF(1,IX)*PAR(1)
		  DO 8805 J=2,NCOEF,1
		      TMP = TMP + PAR(J)*PF(J,IX)
8805              CONTINUE
		  Y(IX) = REAL(TMP)
8803          CONTINUE

	  ELSE
C--           ------------------- Evaluate the fit in vector Y. ----C
	      DO 8806 IX = ISX, IEX
		  TMP = PF(1,IX)*PAR(1)
		  DO 8807 J=2,NCOEF,1
		      TMP = TMP + PAR(J)*PF(J,IX)
8807              CONTINUE
		  Y(IX) = REAL(TMP)
8806          CONTINUE
	  END IF

C--       ----------------------- Operate onto the output image. ----C
C--       Save (oper=0), Subtract (oper=-1) or Divide by (oper=1), fit.
	  IF (OPER.EQ.0) THEN
	      DO 8720 IX=ISX,IEX
		  A(IX,IY) = Y(IX)
8720          CONTINUE

	  ELSE IF (OPER.EQ.-1) THEN
	      DO 8721 IX=ISX,IEX
		  A(IX,IY) = A(IX,IY) - Y(IX)
8721          CONTINUE

	  ELSE IF (OPER.EQ.1) THEN
	      DO 8722 IX=ISX,IEX
		  IF (Y(IX).NE.0) THEN
		      A(IX,IY) = A(IX,IY)/Y(IX)
		  ELSE
		      A(IX,IY) = 0.0E0
		  END IF
8722          CONTINUE
	  END IF
8710  CONTINUE

C--   --- Invert matrix to estimate error on the parameters. ----C
C--   - Use PAR as unitary vector in the column-by-column inversion.
      IF (CLIP.AND.(.NOT.SILENT)) THEN
	  AVEITER = REAL(NITERTOT)/REAL(IEY-ISY+1)
	  PRINT 101, NCLIPTOT, AVEITER
101       FORMAT(I4,' Clippings in an average of',f7.2,' iterations per row.')
      END IF
      IF (.NOT. LOAD) RETURN
      TMP = SUMW/NPTS
      DO 8723 I = 1, NCOEF
	  DO 8724 J = 1, NCOEF
	      PAR(J) = 0.0D0
8724      CONTINUE
	  PAR(I) = TMP
	  CALL LUBKSB(MATRIX,NCOEF,MXPAR,INDX,PAR)
C--       Save Error on the I-th parameter at row J in P(-I,J).
	  TMPS = SQRT(ABS(REAL(PAR(I))))
	  DO 8725 IY = ISY,IEY
	      P(-I,IY) = P(0,IY)*TMPS
8725      CONTINUE
8723  CONTINUE
      RETURN
      END

