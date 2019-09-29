	SUBROUTINE DOEDIT

	INCLUDE 'vistadisk/source/include/vistalink.inc'

	real r(12), ap(12), surf(12)
	character name*64, TWORD*80, PARM*8
	parameter(pi = 3.14159)
	LOGICAL KEYCHECK

	CALL KEYINIT
	CALL KEYDEF('NAME=')
	CALL KEYDEF('DR=')
	CALL KEYDEF('DC=')
	CALL KEYDEF('FRAC=')
	CALL KEYDEF('APDELT=')

	NAME = ' '
        DR = 0
	DC = 0
	FRAC = 0.1
        SCALE = 1.

	DO 5501 I=1,NCON
	  TWORD= WORD(I)
          L = UPPER(TWORD)

	  IF (TWORD(1:5) .EQ. 'NAME=') THEN
            NAME = WORD(I)(6:)

	  ELSE IF (TWORD(1:3) .EQ. 'DR=') THEN
	    CALL ASSIGN(TWORD,DR,PARM)
            IF (XERR) RETURN

	  ELSE IF (TWORD(1:3) .EQ. 'DC=') THEN
	    CALL ASSIGN(TWORD,DC,PARM)
            IF (XERR) RETURN

	  ELSE IF (TWORD(1:5) .EQ. 'FRAC=') THEN
	    CALL ASSIGN(TWORD,FRAC,PARM)
            IF (XERR) RETURN

	  ELSE IF (TWORD(1:7) .EQ. 'APDELT=') THEN
	    CALL ASSIGN(TWORD,APDELT,PARM)
            IF (XERR) RETURN

	  END IF
5501    CONTINUE

	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF

	IF (NAME .EQ. ' ') THEN
	  PRINT 
     &     '(1x,''Enter file name of DAOPHOT aperture photometry file: ''$)'
	  READ '(A)', NAME
	END IF

C	Open the aperture photometry file
	l = index(name,' ')
	open(1,file=name(1:l-1),status='old',iostat=ierr)
        IF (IERR .NE. 0) THEN
	  PRINT *, 'Error opening file ', NAME
          XERR = .TRUE.
	  RETURN
	END IF
	read(1,*)
	read(1,*)
	read(1,*)

1	read(1,320,end=50) istar, x, y, (ap(j),j=1,12)
  320   FORMAT (I6, 2F9.2, 12F9.3)
 	read(1,321) sky
  321   FORMAT (4X, F9.3, 2F6.2, 12(F8.3, 1X))

C	Compute the aperture mags in counts
	do i=1,12
          r(i) = i*apdelt
          ap(i) = 10.**(-0.4*(ap(i)-25))
	end do

C	Compute the mean annular surface brightnesses and compare to sky
	do i=2,12
          surf(i) = (ap(i) - ap(i-1)) / (pi * (r(i)-r(i-1))**2)
          if (surf(i) .gt. frac*sky) imax = i+1
	end do

	write(olu,*) y+dr, x+dc, imax*APDELT
	goto 1


50	continue
	CLOSE(1)

	RETURN
	END
