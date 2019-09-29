C       FIND THE SYMBOLS IN A GIVEN LINE

        INTEGER FUNCTION GETSYMS(LINE,SYM,MAXSYM)

C       Written by Jon Holtzman

        CHARACTER*(*) LINE
	CHARACTER*20 SYM(MAXSYM), FSTRCAT
  	CHARACTER C
        INTEGER PARSER,GRAMMAR,FCRUNCH,CRUNCH,SYMTOBIN
        INTEGER FINDEXPR, FIRST
        INTEGER*4 NEND
        LOGICAL ISSYM

        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/symbol.inc'

        CHARACTER*256 LOCALCOPY
        DOUBLE PRECISION TREEVAL

C    Find number of characters
        NC = NUMCHAR(LINE)

        LOCALCOPY = LINE

C    Parse the field
        GETSYMS = PARSER(LOCALCOPY(1:NC),1,NEND)
        IF (GETSYMS .NE. SUCCESS) RETURN

	NP=1
        ISYM=1
100     IF(ISSYM(NP)) THEN
          FIRST = SYMS(NP)
          C = SYMSAVE(FIRST:FIRST)
          SYM(ISYM) = ' '
          SYM(ISYM) = FSTRCAT(SYMSAVE(FIRST:SYME(NP)),CHAR(0))
          DO J=1,ISYM-1
            IF (SYM(ISYM) .EQ. SYM(J)) GOTO 99
          END DO
          ISYM=ISYM+1
99        CONTINUE
        END IF
        NP = FORW(NP)
        IF(NP .NE. TREEEND) THEN
          GOTO 100
        END IF

        GETSYMS = ISYM-1

	RETURN
	END

