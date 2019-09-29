C       ***     IMAGE PARAMETERS, ADDRESSES, AND CONTROLS       ***

C       This include file contains common blocks holding image
C       parameters, labels, and virtual addresses.

C       The MAXIM parameter controls the maximum number of images
C       or spectra that can be handled by VISTA.
	INTEGER         MAXIM
	PARAMETER       (MAXIM = 1999)

C       Maximum size of images (used only for certain routines which have
C       hard wired arrays - not a limit for size of image which can be
C       read in and displayed. Note for MASKS to work, MAXCOLIM MUST BE
C       a multiple of 8
        INTEGER		MAXROWIM, MAXCOLIM
	PARAMETER       (MAXROWIM = 4096, MAXCOLIM = 4100)

C       Each image has a list of parameters specifying such things
C       as its size, conditions of observation, reduction history,
C       identifications, and other information deemed important.
C       The list is a FITS header, whose length will be dynamically
C       allocated. The largest allowed will be 30 cards, NFITSBYTES=3*28800
C       bytes. To change the maximum number of cards, change
C       MAXFITSCARD here, and also the length of TEMPHEAD below.

	INTEGER         NFITSBYTES
	PARAMETER       (NFITSBYTES = 2880)
	INTEGER         MAXFITSCARD
	PARAMETER       (MAXFITSCARD = 200)

C       The storage for the headers is dynamic. The array HEADBUF will
C       carry the location of the header for image IMNUM in HEADBUF(1,IMNUM)
C       and the length in HEADBUF(2,IMNUM). To use the location, we
C       will need to pass values through some C routines to do value to
C       pointer conversions


	INTEGER*8       HEADBUF(2,MAXIM)



	COMMON  /IMOBJ/ HEADBUF

C       We have the memory allocated for one maximum length FITS header
C       here. This is accesed through the common block /HEADWORK/ for
C       all the routines that need to work with a FITS header.

	CHARACTER       TEMPHEAD*576000, TEMPHEAD2*576000
	COMMON  /HEADWORK/      TEMPHEAD

C       We can hold one-dimensional data in the buffers.  Define a logical
C       array to tell that we have intrinsically 1-D objects in the
C       buffers.

	LOGICAL         ISSPECTRUM(MAXIM)
	COMMON  /IMISSPEC/      ISSPECTRUM

C       VISTA images are arrays of 2-D floating-point numbers.  The
C       size of the array in each dimension, origin in each dimension,
C       and compression factor in each dimension, are extracted from
C       HEADBUF and held in the integer array ICOORD for convenience.
C       The locations of the various parameters in ICOORD are defined
C       by the following parameters.

C      Number of rows
	INTEGER         NNROW
	PARAMETER       (NNROW = 1)
C      Number of columns
	INTEGER         NNCOL
	PARAMETER       (NNCOL = 2)
C      Starting row
	INTEGER         IYSR
	PARAMETER       (IYSR  = 3)
C      Start column
	INTEGER         IXSC
	PARAMETER       (IXSC  = 4)
C      Row compression
	INTEGER         ICMPR
	PARAMETER       (ICMPR= 5)
C      Column compression
	INTEGER         ICMPC
	PARAMETER       (ICMPC= 6)
C      End row
	INTEGER         IYER
	PARAMETER       (IYER  = 7)
C      End column
	INTEGER         IXEC
	PARAMETER       (IXEC  = 8)

	INTEGER         ICOORD(8,MAXIM)
	COMMON  /IMGH/  ICOORD

C       The actual FITS card numbers are held in the common block FITSCARDS.
C       The values for CRPIX1, CRVAL1, CDELT1, NPIX1, etc. are stored.

	INTEGER         NPIX1(MAXIM), NPIX2(MAXIM)
	REAL*8          CRPIX1(MAXIM), CRPIX2(MAXIM)
	REAL*8          CRVAL1(MAXIM), CRVAL2(MAXIM)
	REAL*8          CDELT1(MAXIM), CDELT2(MAXIM)
	REAL*8          CD1_1(MAXIM), CD1_2(MAXIM)
	REAL*8          CD2_1(MAXIM), CD2_2(MAXIM)
	CHARACTER*20    CTYPE1(MAXIM), CTYPE2(MAXIM)

	COMMON  /FITSCARDS/     NPIX1, NPIX2, CRVAL1, CRVAL2, CRPIX1,
     &                  CRPIX2, CDELT1, CDELT2, CTYPE1, CTYPE2, 
     &                  CD1_1, CD1_2, CD2_1, CD2_2

C       Each image connected with VISTA is assigned an integer image number
C       which is greater than 0 and less than or equal to MAXIM.  An array
C       of logical variables and addresses define the current state of each
C       image number.

C       Variable:       BUFF(IM)        Set .TRUE. if the image number 'IM'
C                                       is connected.
C                       IMLOC(IM)       Contains the virtual address of image
C                                       number 'IM'.
C                       NBYTE(IM)       Contains the number of bytes occupied
C                                       by the image.
C                       BUFOLD          Set .TRUE. if a block of virtual
C                                       memory is to be returned.
C                       LOCOLD          The address of the returned memory.
C                       NBOLD           The ammount of returned memory.
C                       IMCREATED       New image buffer number

	LOGICAL         BUFF(MAXIM), BUFOLD
	INTEGER         NBYTE(MAXIM)

	INTEGER*8       IMLOC(MAXIM)
	INTEGER*8       LOCOLD, LOCNEW, LOCIM, LOCATION, LOCX, LOCW
        INTEGER*8       LOCDUM, LOC1, LOC2, LOCTV, LOCB
        INTEGER*8       LOCSVAR, LOCSKY, LOCV, LOCPROFILE, LOCMASKV
        INTEGER*8       LOCSPEC, LOCS, LOCSV







	INTEGER         NBOLD
	INTEGER         IMCREATED
	COMMON /IMG/ LOCOLD, IMLOC, BUFF, NBYTE, BUFOLD, NBOLD, IMCREATED

C       Parameters can be passed to subroutines by use of the IMGPIPE common.
C       The common holds the variables needed to specify the condition and
C       origin of the image data array for a maximum of two images, numbered
C       'IM' and 'JM'.

C       Variable:       ISR             Image starting row >=0
C                       ISC             Image starting column >=0
C                       IRBX            Row compression factor >=1
C                       IRCX            Column compression factor >=1
C                       IER             Image end row
C                       IEC             Image end column

	INTEGER         ISR, ISC, IRBX, ICBX, IER, IEC, IM
	INTEGER         JSR, JSC, JRBX, JCBX, JER, JEC, JM
	COMMON  /IMGPIPE/       ISR, ISC, IRBX, ICBX, IER, IEC, IM,
     .                          JSR, JSC, JRBX, JCBX, JER, JEC, JM

C       The following is for one-dimensional objects (i.e. spectra)

C       The wavelength scale of a spectrum can be calculated from its
C       pixel numbers with use of the wavelength scale variables: SPWAVE.
C       SPWAVE contains either coefficients of a polynimial wavelength
C       scale or of a logarithmic wavelength scale.

C       There are three types of wavelength scales supported by VISTA.
C       Polynomial:  The scale produced from command LAMBDA.  Two sets
C               of coefficients are produced, one which gives wavelength
C               as a function of column difference and one which gives
C               column number as a function of wavelength difference.
C               Column numbers run from ICOORD(IXSC, ) to ICOORD(IXSC, )+
C               ICOORD(NNCOL, ) - 1 where ICOORD(IXSC, ) corresponds to the
C               first element of the FORTRAN array.  The column difference
C               is the difference between the specified column and the
C               central column of the spectrum.  The wavelength difference
C               is the difference between the specified wavelength and
C               the wavelength of the central column.  This polynomial
C               scale is generated by the LINES.FOR subroutine.
C       Linear:     The scale produced with the ALIGN command.  This is
C               a linear wavelength scale expressed in terms  of column
C               number.
C       Logarithmic:  The other scale produced with the ALIGN command.
C               On this scale the logarithm of the wavelength is a
C               linear function of the column number.

C       Note that the following wavelenght parameter arrays start
C       from element 0.  The 0''th element of each can be used
C       to interpret a FITS header (using HEADTOW and LAMPIX)
C       without having a spectrum associated with the header.
C       This facility is used in SPHEADER.

C       The type of wavelength scale for each spectrum is contained
C       in the array LAMTYPE() and the types are defined by the
C       following codes:

C  Uncalibrated
	INTEGER         LAMBDANONE
	PARAMETER       (LAMBDANONE=0)
C  Linear wavelength scale
	INTEGER         LAMBDALINEAR
	PARAMETER       (LAMBDALINEAR=1)
C  Polynomial wavelength scale
	INTEGER           LAMBDAPOLY
	PARAMETER       ( LAMBDAPOLY = 2 )
C  Logarithmic wavelength scale
	INTEGER         LAMBDALOG
	PARAMETER       (LAMBDALOG=3)
C  Echelle wavelength scale
	INTEGER         LAMBDAECHELLE
	PARAMETER       (LAMBDAECHELLE=4)
C  Base-10 Log wavelength scale
        INTEGER         LAMBDALOG10
        PARAMETER       (LAMBDALOG10=5)

C                       Array to store wavelength scale of each buffer
	INTEGER         LAMTYPE(0:MAXIM)

C       Size of coefficient buffers

	INTEGER         MAXPOLY
	PARAMETER       (MAXPOLY=7)

C       Column number - to - wavelength coefficients

	DOUBLE PRECISION SPWAVE(MAXPOLY,0:MAXIM)

C       Wavelength - to - column number coefficients for polynomial scale

	DOUBLE PRECISION SPPIXEL(MAXPOLY,0:MAXIM)

C       Polynomial order and central column for polynomial scale

	INTEGER         LAMORDER(0:MAXIM)
        DOUBLE PRECISION LCENTER(0:MAXIM), WCENTER(0:MAXIM)

	COMMON  /SPCAL/ SPWAVE, SPPIXEL, LAMORDER, LCENTER, WCENTER, LAMTYPE

C       For the special case that the 0''th element is refered to, we
C       need the starting column of the image.
	INTEGER         ISTARTSPECIAL
	COMMON  /SPECIALSTARTCOL/       ISTARTSPECIAL

C       Author: Tod R. Lauer
C               Richard Stover  11/23/82
C               Donald Terndrup  3/11/85                Version 3



