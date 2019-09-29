


C       Defines TV common block (9/27/88).

C       The common block TV holds information about the displayed image.

C       VARIABLE:       TVSTAT  .TRUE. if an image is displayed.
C                       TVCOL   Flag for type of color map used
C                       IRTV    Number of first row displayed
C                       ICTV    Number of first column
C                       NRTV    Number of image rows displayed
C                       NCTV    Number of columns
C                       ICOMP   Display compression factor
C                       IMN     Displayed image buffer number
C                       IZ      The curent zoom factor
C                       IXLO, IXHI, IYLO, IYHI: screen limits

        INTEGER TVCOL, IRTV, ICTV
        LOGICAL TVSTAT
        COMMON /TV/ TVSTAT, TVCOL, IRTV, ICTV, NRTV, NCTV, ICOMP,
     .          IZ, IEXP, IXLO ,IXHI, IYLO, IYHI
	COMMON /IMTVNUM/ IMTV
