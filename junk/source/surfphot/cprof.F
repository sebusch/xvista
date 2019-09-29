#include "Vista.h"
C 
C       Title   :  cprof.f77
C       Author  :  Jeff Willick
C       Date    :  July 1987
C
C       Synopsis:
C
C       Revisions:
C       01/14/88   forrestg     Added correction for SB noise.
C 
        SUBROUTINE CPROF

C       Routine to interactively correct the results of a GPROF
C       calculation for excursions.  The contents of the PROFILE
C       common block are plotted, and the user is asked to
C       identify regions which represent possible excursions.
C       These are then plotted in greater detail and the user
C       picks the start and end points of the excursions.
C       The surface brightness, ellipticity, and the PA in these
C       bad regions are then replaced by a second order polynomial
C       fit to the regions around it.  In addition, the user can
C       specify that only the surface brightness is to be replaced
C       by the polynomial fit, which is appropriate if a strong
C       increase in ellipticity is deemed to be real.

C       Author: Jeff Willick
C       Last update, 10/22/87

#ifdef VMS
        INCLUDE 'VINCLUDE:imagelink.inc'
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:customize.inc'
        INCLUDE 'VINCLUDE:profile.inc'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif
        PARAMETER (ONTH=0.333333, TWTH=0.666667, NPAR=10)
        DIMENSION SBTEMP(NPROFILE),ETEMP(NPROFILE),PATEMP(NPROFILE)
        REAL*8 SBCOEF(NPAR), ECOEF(NPAR), PACOEF(NPAR)
        REAL*8 XFIT(NPROFILE),SY(NPROFILE),EY(NPROFILE),PAY(NPROFILE)
        REAL*8 WEIGHT(NPROFILE), COV(NPAR*NPAR), DCOEF(NPAR)
        REAL*8 SBT, ET, PAT, CHISQ, FPOLY
C       INTEGER START,ENND, RN
        INTEGER FIRST, II
	LOGICAL SBONLY, UNI
        CHARACTER ANS*1
        INTEGER RM
        DOUBLE PRECISION Q(0:4),WI,LNA,A,ALP,URI
        CHARACTER TEMP*8, JUNK*80, PARM*8
        LOGICAL GLLS, MEDIAN, KEYCHECK, FIRSTTIME
#ifdef __X11
	INTEGER PMGO(MX11GETS)
#endif
        EXTERNAL FPOLY
        COMMON / WORK / XFIT,SY,EY,PAY,WEIGHT,COV,SBTEMP,ETEMP,PATEMP

C     Keywords
        IF (NOGO) RETURN
	CALL KEYINIT
	CALL KEYDEF('MEDIAN')
	CALL KEYDEF('ORDER=')
	CALL KEYDEF('COL=')
	CALL KEYDEF('SQWEIGHT') 

	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF

	MEDIAN = .FALSE.
	FIRSTTIME = .TRUE.
	UNI = .TRUE.
	NORDER = 2
        IPRFCOL = 1

	DO 5501 I = 1, NCON
	   
	   IF (WORD(I) .EQ. 'MEDIAN') THEN
	      MEDIAN = .TRUE.

	   ELSE IF (WORD(I)(1:6) .EQ. 'ORDER=') THEN
	      CALL ASSIGN(WORD(I),TMP,PARM)
	      IF (XERR) RETURN
	      NORDER = NINT(TMP)

	   ELSE IF (WORD(I) .EQ. 'SQWEIGHT') THEN
	      UNI = .FALSE.
	      
	   ELSE IF (WORD(I)(1:4) .EQ. 'COL=') THEN
	      CALL ASSIGN(WORD(I),TMP,PARM)
	      IF (XERR) RETURN
	      IPRFCOL = NINT(TMP)

	   END IF

5501    CONTINUE

C       Plot and inform the user about the profile plot:

        FIRST = 1
17      CALL PLPROF(1,NPRF(IPRFCOL),IPRFCOL)
        print *,'The ellipticity is represented by points, the surface'
        print *,'brightness by a line, and the semiminor axis by a '
        print *, 'histogram, in the plot to follow.  '

        SBONLY = .FALSE.

18      IF (FIRSTTIME) THEN
       CALL ASKYORN('Do the profile parameters require correction?',ANS)
       IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') GO TO 91
       FIRSTTIME = .FALSE.
	END IF

567     PRINT *, 'Mark region delimiting bad region with four points '
	PRINT *, '  (two on left and two on right), using space bar'
	DO I=1,4
          CALL PMGO(MONGOHAIRS)(ICR,XX,YY)
	  IF (I .EQ. 1) G1 = XX
	  IF (I .EQ. 2) G2 = XX
	  IF (I .EQ. 3) G3 = XX
	  IF (I .EQ. 4) G4 = XX
	  CALL PMGO(RELOCATE)(XX,0.)
	  CALL PMGO(SETLTYPE)(1)
	  CALL PMGO(DRAW)(XX,1.)
	  CALL PMGO(SETLTYPE)(0)
	  CALL PMGO(TIDLE)
	END DO

#ifdef NOTDEF
567     PRINT *, 'Input the four numbers delimiting the bad region'
        PRINT *, 'and the region to which the polynomial fit is to'
        PRINT '(''be made (use integers):''$)'

        JUNK = ' '
#ifdef __X11
        IF (PMGO(MX11GETS)(JUNK) .EQ. 0) GOTO 567
#else
	READ(*,15) JUNK
	IF (NOGO) RETURN
#endif
15      FORMAT(A)
        READ(JUNK,*,ERR=567) G1,G2,G3,G4
#endif

C    Order of polynomial fit
	PRINT *,'Using fit of order:', NORDER, ' (Use ORDER= keyword to change)'
C	CALL ASKINT('Order for polynomial fit:',NORDER,1)
        NCOEF=NORDER+1

C   Commented out option to fit SB only, per request of S. Courteau
C     and J. Willick,  3/91
C	CALL ASKYORN('Fit surface brightness only?',ANS)
C        IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') SBONLY = .TRUE.

C       Number of points in the regions to be fit:
        L1=NINT(G1/PRFSC(IPRFCOL))+1
        L2=NINT(G2/PRFSC(IPRFCOL))+1
        L3=NINT(G3/PRFSC(IPRFCOL))+1
        L4=NINT(G4/PRFSC(IPRFCOL))+1

C       Set up arrays for parameter fits:
        N = 0
        DO 2757 IP=L1,L2
            N = N + 1
            XFIT(N) = IP
            SY(N) = PARAMPRF(14,IP,IPRFCOL)
            EY(N) = PARAMPRF(13,IP,IPRFCOL)
            PAY(N) = PARAMPRF(3,IP,IPRFCOL)
            WEIGHT(N) = FLOAT(N)
2757    CONTINUE

        DO 2758 IP=L3,L4
            N = N + 1
            XFIT(N) = IP
            SY(N) = PARAMPRF(14,IP,IPRFCOL)
            EY(N) = PARAMPRF(13,IP,IPRFCOL)
            PAY(N)= PARAMPRF(3,IP,IPRFCOL)
            WEIGHT(N) = FLOAT(L4+1-IP)
2758    CONTINUE

C       Perform fits for SB:
        IF (.NOT.GLLS(XFIT,SY,WEIGHT,N,SBCOEF,NCOEF,DCOEF,COV,
     &      CHISQ,FPOLY,.FALSE.)) THEN
            PRINT *,'SB Fit failed.'
            XERR = .TRUE.
            RETURN
        END IF

C       Now ellipticity and PA fits:
        IF (.NOT. SBONLY) THEN
            IF (.NOT.GLLS(XFIT,EY,WEIGHT,N,ECOEF,NCOEF,DCOEF,COV,
     &          CHISQ,FPOLY,.FALSE.)) THEN
                PRINT *,'Ellipticity Fit failed.'
                XERR = .TRUE.
                RETURN
            END IF

            IF (.NOT.GLLS(XFIT,PAY,WEIGHT,N,PACOEF,NCOEF,DCOEF,COV,
     &          CHISQ,FPOLY,.FALSE.)) THEN
                PRINT *,'PA Fit failed.'
                XERR = .TRUE.
                RETURN
            END IF
        END IF

C       Fill in the new profile. Save the old ones in temporary buffers:
        SKY = PARAMPRF(5,1,IPRFCOL) - PARAMPRF(14,1,IPRFCOL)
        IF (SBONLY) THEN
            DO 2760 IP=L2,L3,1
                SBTEMP(IP) = PARAMPRF(14,IP,IPRFCOL)
                SBT = SBCOEF(NCOEF)
                DO 2761 I = NCOEF-1,1,-1
                    SBT = SBCOEF(I) + SBT*IP
2761            CONTINUE
                PARAMPRF(14,IP,IPRFCOL) = REAL(SBT)
                PARAMPRF(5,IP,IPRFCOL) = REAL(SBT) + SKY
2760        CONTINUE

        ELSE
            DO 2762 IP=L2,L3,1
                SBTEMP(IP) = PARAMPRF(14,IP,IPRFCOL)
                ETEMP(IP) = PARAMPRF(13,IP,IPRFCOL)
                PATEMP(IP) = PARAMPRF(3,IP,IPRFCOL)
                SBT = SBCOEF(NCOEF)
                ET = ECOEF(NCOEF)
                PAT = PACOEF(NCOEF)
                DO 2763 I = NCOEF-1,1,-1
                    SBT = SBCOEF(I) + SBT*IP
                    ET = ECOEF(I) + ET*IP
                    PAT = PACOEF(I) + PAT*IP
2763            CONTINUE
                PARAMPRF(14,IP,IPRFCOL) = REAL(SBT)
                PARAMPRF(5,IP,IPRFCOL) = REAL(SBT) + SKY
                PARAMPRF(13,IP,IPRFCOL) = REAL(ET)
                PARAMPRF(4,IP,IPRFCOL) = SQRT(1.-(1.-REAL(ET))**2)
                PARAMPRF(3,IP,IPRFCOL) = REAL(PAT)
2762        CONTINUE
        END IF

C       Finally, do a little smoothing of the edges:
        DO 2769 II=1,5
                IF (II .EQ. 1) THEN
                   J=13
                ELSE IF (II .EQ. 2) THEN
                   J=14
                ELSE 
                   J=II
                END IF
                PARAMPRF(J,L2,IPRFCOL)=TWTH*PARAMPRF(J,L2-1,IPRFCOL)+
     &                ONTH*PARAMPRF(J,L2+2,IPRFCOL)
                PARAMPRF(J,L2+1,IPRFCOL)=ONTH*PARAMPRF(J,L2-1,IPRFCOL)
     &                +TWTH*PARAMPRF(J,L2+2,IPRFCOL)
                PARAMPRF(J,L3-1,IPRFCOL)=TWTH*PARAMPRF(J,L3-2,IPRFCOL)
     &                +ONTH*PARAMPRF(J,L3+1,IPRFCOL)
                PARAMPRF(J,L3,IPRFCOL)=ONTH*PARAMPRF(J,L3-2,IPRFCOL)
     &                +TWTH*PARAMPRF(J,L3+1,IPRFCOL)

2769    CONTINUE

C       Plot the new results and inquire whether the user is satisfied.
        CALL PLPROF(FIRST,NPRF(IPRFCOL),IPRFCOL)

	CALL ASKYORN
     &     ('Satisfied with the results (Y), or try again(n)?',ANS)
	IF (NOGO) RETURN
        IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') THEN
                DO 39 I=L2,L3
                   PARAMPRF(14,I,IPRFCOL) = SBTEMP(I)
                   PARAMPRF(13,I,IPRFCOL) = ETEMP(I)
                   PARAMPRF(3,I,IPRFCOL) = PATEMP(I)
                   PARAMPRF(5,I,IPRFCOL) = PARAMPRF(14,I,IPRFCOL) + SKY
                   PARAMPRF(4,I,IPRFCOL) = 
     &                         SQRT(1.-(1.-PARAMPRF(13,I,IPRFCOL))**2)
39              CONTINUE
                PRINT *, 'replotting the original profile parameters;'
                PRINT *, 'try another fit range...'
                GO TO 17
        ELSE
                GO TO 91
        END IF

91      CONTINUE

C  Get rid of all of the following, per Stephane 5/98
#ifdef NOTDEF
C  Code for SB corrections:
 
C  Query the user for input.

	call askyorn(
     &      'Do you wish to fit outer disk with exponential?',ans)
	if (nogo) return
        if (ans .eq. 'N' .or. ans .eq. 'n') go to 92
	if (median) then
	  nsb=20
	else
	  nsb=14
	end if

C        print 16, 'Input start of region to examine more closely: '
C        read *, start
C        first = nint(float(start)/prfsc(iprfcol)) + 1
110     continue
        call plovsb(first,nprf(IPRFCOL),nsb,.true.,iprfcol)

C  Zero out the temporary buffers.

C        uni = .false.
C        do 1800 i=1,NPRF(IPRFCOL)+2
C                sbtemp(i) = 0.
C1800    continue
        do 2800 i=0,4
                q(i) = 0.0d0
2800    continue


101     print 
     &   '(1x,''Enter radius beyond which fit is to be performed: ''$)'
        JUNK = ' '
#ifdef __X11
        IF (PMGO(MX11GETS)(JUNK) .EQ. 0) GOTO 101
#else
	READ(*,15) JUNK
#endif
        READ(JUNK,*,ERR=101) RM

C102     print *, 'Enter radius beyond which the surface brightness '
C        print '(''is to be replaced by fit:''$)'
C        JUNK = ' '
C#ifdef __X11
C        IF (PMGO(MX11GETS)(JUNK) .EQ. 0) GOTO 102
C#else
C	READ(*,15,ERR=102) JUNK
C#endif
C        READ(JUNK,*,ERR=102) RN
C        if (rm .gt. rn) then
C                print *, 'The second radius must be greater than ',
C     &                   'or equal '
C                print *, 'to the first, try again . . .'
C                go to 101
C        endif
C        print *, 'Do you wish to perform the fit with uniform (U) or'
C        print *, '  square-root (S) weighting? '
C        read 15, ans
C        if (ans .eq. 'U' .or. ans .eq. 'u') uni = .true.
C
C  calculate A and alp
C

C        rn = nint(float(rn)/prfsc(iprfcol)) + 1
        rm = nint(float(rm)/prfsc(iprfcol)) + 1

        do 2810 i=rm,nprf(IPRFCOL)
                ri = (i - 1) * prfsc(iprfcol)
                uri = dble(paramprf(nsb, i, iprfcol))
                if (uri .le. 0.0) uri = dble(1.)
                ii = dlog(uri)
                if (uni) then
                        wi=dble(1.)
                else
                        if (uri .ge. dble(2.)) then
                                wi=dsqrt(uri)
                        else if (uri .gt. dble(0.)) then
                                wi=dble(1.)
                        else
                                wi=dble(0.)
                        endif
                endif
                q(0) = q(0) + wi
                q(1) = q(1) + (wi * ri)
                q(2) = q(2) + (wi * ri * ri)
                q(3) = q(3) + (wi * ii)
                q(4) = q(4) + (wi * ii * ri)
2810    continue

        do 2830 i=1,4
                q(i) = q(i)/q(0)
2830    continue

        ii      = (q(2) - q(1)*q(1))
        lnA     = (q(2)*q(3) - q(1)*q(4))/ii
        alp   = (q(1)*q(3) - q(4))/ii
        A       = dexp(lnA)
C
C  dump values
C
        print *, 'Characteristic values:'
        write(6,800) alp
800     format(1x,'alpha = ',e11.3)
        write(6,810) A
810     format(1x,'A     = ',e11.3)
C       write(6,820) q(0)
C820    format(1x,'q(0)  = ',e11.3)
C       write(6,830) q(1)
C830    format(1x,'q(1)  = ',e11.3)
C       write(6,840) q(2)
C840    format(1x,'q(2)  = ',e11.3)
C       write(6,850) q(3)
C850    format(1x,'q(3)  = ',e11.3)
C       write(6,860) q(4)
C860    format(1x,'q(4)  = ',e11.3)
C
C  save alpha and A as single precision VISTA variables.
C
        temp = 'A       '
        var=sngl(A)
        call variable(temp,var,.true.)
	CALL FHEADSET(TEMP,A,PRFHEAD(IPRFCOL))
        temp = 'ALPHA   '
        var=sngl(alp)
        call variable(temp,var,.true.)
	CALL FHEADSET(TEMP,ALP,PRFHEAD(IPRFCOL))


C  save the old SB in temporary buffers and calculate the new values

        do 2820 i=first,nprf(IPRFCOL)
                sbtemp(i-first+1) = paramprf(nsb,i,iprfcol)
                ri = (i - 1) * prfsc(iprfcol)
                ii = A * dexp(- (alp * ri))
                paramprf(nsb,i,iprfcol) = sngl(ii)
                paramprf(5,i,iprfcol) = sngl(ii) + sky
2820    continue

C
C  over plot the new exponential fit of the surface brightness
C
        call plovsb(first,nprf(iprfcol),nsb,.false.,iprfcol)

C
C  Replace old SB from temporary buffers

        do 40 i=first,nprf(iprfcol)
                paramprf(nsb,i,iprfcol) = sbtemp(i-first+1)
                paramprf(5,i,iprfcol) = sbtemp(i-first+1) + sky
40      continue

C
C  ask if acceptable
C
	call askyorn
     &      ('Satisfied with the results (y), or try again?', ans)
        if (ans .eq. 'n' .or. ans .eq. 'N') then
           go to 110
        else
C                do 42 i=rn,nprf(iprfcol)
C                        ri = (i - 1) * prfsc(iprfcol)
C                        ii = A * dexp(- (alp * ri))
C                        paramprf(nsb,i,iprfcol) = sngl(ii)
C                        paramprf(5,i,iprfcol) = sngl(ii) + sky
C42              continue
           go to 92
        endif

92      CONTINUE
#endif
        RETURN
        END
