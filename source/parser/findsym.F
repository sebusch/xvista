#include "Vista.h"
C       THE ROUTINES IN THIS FILE EITHER FIND THE VALUE OF A SYMBOL
C       ALREADY IN THE SYMBOL TABLE (FINDSYM) OR PUT A SYMBOL AND
C       ITS VALUE INTO THE SYMBOL TABLE (PUTSYM)

        INTEGER FUNCTION FINDSYM(SYM,SYMVALUE,JTYPE)
        CHARACTER*(*) SYM
        DOUBLE PRECISION SYMVALUE
        INTEGER JTYPE, HASH, H
#ifdef VMS
        INCLUDE 'VINCLUDE:SYMBOL.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/symbol.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
C       GET INDEX INTO SYMBOL TABLE

        H = HASH(SYM)
        MAXH = NSYMS

        DO 8701 I=1,2
                DO 8702 NH=H,MAXH
C                       If we come across an open slot in the table then
C                       the symbol must not be defined
                        IF(SYMTYPE(NH) .EQ. SYMUNDEF) THEN
                            FINDSYM = EUNDEF
                            MERROR = SYM
                            RETURN
                        ELSE IF(SYM .EQ. SYMBOLS(NH)) THEN
                            JTYPE = SYMTYPE(NH)
                            SYMVALUE = SYMVAL(NH)
                            IF(JTYPE .EQ. SYMCHAR) THEN
                              SYMEXTRACT =
     .                              SYMTXT(SYMTP(1,NH):SYMTP(2,NH))
                              SYMSIZE = SYMTP(2,NH) - SYMTP(1,NH) + 1
                            END IF
                            FINDSYM = SUCCESS
                            RETURN
                        END IF
8702            CONTINUE
C               Search first half of symbol table
                MAXH = H
                H = 1
8701    CONTINUE
        FINDSYM = EUNDEF
        MERROR = SYM
        RETURN
        END

C       Put a symbol into the symbol table
        INTEGER FUNCTION PUTSYM(SYM,SYMVALUE,JTYPE)
        CHARACTER*(*) SYM
        DOUBLE PRECISION SYMVALUE
#ifdef VMS
        INCLUDE 'VINCLUDE:SYMBOL.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/symbol.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        INTEGER H, HASH

C       SEE IF TO PRINT THE SYMBOL VALUE

        IF(SYM .EQ. 'TT') THEN
                IF(JTYPE .EQ. SYMCHAR) THEN
                        WRITE(*,*) SYMEXTRACT(1:SYMSIZE)
                ELSE
                        WRITE(*,*) SYMVALUE
                END IF
        END IF

C       GET TABLE INDEX

        H = HASH(SYM)

        MAXH = NSYMS

        DO 8703 I=1,2
                DO 8704 NH=H,MAXH
C                       If we find an opening in the table we put the
C                       symbol there.
100                     IF(SYMTYPE(NH) .EQ. SYMUNDEF) THEN
                                SYMTYPE(NH) = JTYPE
                                SYMVAL(NH) = SYMVALUE
                                SYMBOLS(NH) = SYM
                                IF(JTYPE .EQ. SYMCHAR) THEN
                                        NL = SYMPTR+SYMSIZE-1
                                        IF(NL .GE. LEN(SYMTXT)) THEN
                                                PUTSYM = ENOROOM
                                                RETURN
                                        END IF
                                        SYMTXT(SYMPTR:NL) = SYMEXTRACT
                                        SYMTP(1,NH) = SYMPTR
                                        SYMTP(2,NH) = NL
                                        SYMPTR = NL + 1
                                END IF
                                PUTSYM = SUCCESS
                                RETURN
                        ELSE IF(SYMBOLS(NH) .EQ. SYM) THEN
C                               Check for array declaration, which is handled
C                               elsewhere.
                                IF((SYMTYPE(NH) .EQ. SYMFUN) .AND.
     .                             (SYMVAL(NH) .LT. 0.0)) THEN
                                        PUTSYM = SUCCESS
                                        RETURN
                                END IF
                                IF(JTYPE .NE. SYMTYPE(NH)) THEN
                                        PUTSYM = EMULTIDEF
                                        MERROR = SYM
                                        RETURN
                                END IF
                                SYMVAL(NH) = SYMVALUE
                                IF(JTYPE .EQ. SYMCHAR) THEN
                                   NC = SYMTP(2,NH) - SYMTP(1,NH) + 1
                                   SYMTXT(SYMTP(1,NH):) =
     .                                SYMTXT(SYMTP(2,NH)+1:)
                                   SYMPTR = SYMPTR - NC
                                   SYMTYPE(NH) = SYMUNDEF
                                   GOTO 100
                                END IF
                                PUTSYM = SUCCESS
                                RETURN
                        END IF
8704            CONTINUE

C               Search first part of symbol table
                MAXH = H
                H = 1
8703    CONTINUE

        PUTSYM = ENOROOM
        RETURN
        END


C       Produce the initial index into the symbol table
        INTEGER FUNCTION HASH(S)
        CHARACTER*(*) S
        CHARACTER C
#ifdef VMS
        INCLUDE 'VINCLUDE:SYMBOL.INC'
#else
        INCLUDE 'vistadisk/source/include/symbol.inc'
#endif

        HASH = 0
        DO 8705 I=1,LEN(S)
                C = S(I:I)
                IF(C .EQ. ' ') GOTO 100
                HASH = HASH*8 + ICHAR(C)
8705    CONTINUE

100     HASH = MOD(HASH,NSYMS) + 1

        RETURN
        END
