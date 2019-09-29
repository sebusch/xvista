        SUBROUTINE VSTRCPY(A,B)

C       Copies string B into string A.

        CHARACTER*(*) B
        CHARACTER*(*) A

        L = MIN(LEN(A),LEN(B))
        A(1:L) = B(1:L)

        RETURN
        END
