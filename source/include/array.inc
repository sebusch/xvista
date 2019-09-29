C       Definition of CABAL dimensioned arrays.

C                       Maximum number of allowed arrays.
	INTEGER         MAXARRAY
	PARAMETER       (MAXARRAY=100)
C                       Virtual address of arrays.
	INTEGER*4       ARRAYLOC(MAXARRAY)
C                       Size of the array in 'DOUBLEs'.
C                       Multiply by 8 to get bytes.
	INTEGER*4       ARRAYSIZE(MAXARRAY)
C                       Non-zero means that the corresponding
C                       _LOC and _SIZE elements are defined.
	INTEGER*4       ARRAYVAL(MAXARRAY)

	COMMON  /ARRAYCOM/      ARRAYLOC, ARRAYSIZE, ARRAYVAL


