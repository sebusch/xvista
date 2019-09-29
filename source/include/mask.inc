C       This file keeps track of masked pixels.
C       As of xvista5.1, the MASK array is now DYNAMIC, so we can accomodate
C       masks of different sizes. The MASK array is allocated whenever a
C       mask is read in, or whenever the user either creates a mask using
C       CREATE MASK, or starts to mask pixels (using CLIP, AEDIT, or ZAP).
C       With CREATE, the user can specify the size of the desired mask.
C       With CLIP, AEDIT, or ZAP, a mask is created (if it doesnt already
C       exist) with the size of the image being operated on.
C       The mask can be deallocated using UNMASK
C       MASKs are now output in a FITS-style format, but with just one bit
C       per pixel.

C      Length of mask array.
	INTEGER		NRMASK, NCMASK, SRMASK, SCMASK, MASKSIZE

        INTEGER*8       LOCMASK



        LOGICAL	        VERBOSEMASK, MISSEDMASK
	COMMON  /MASKLIST/ LOCMASK, VERBOSEMASK, MISSEDMASK,
     &                     NRMASK, NCMASK, SRMASK, SCMASK, MASKSIZE

C       Each bit in the integer array represents one pixel.  If a bit is
C       set to 1, the pixel it represents is masked.   A zero bit means
C       the pixel is not masked.

C       Each pixel (COL,ROW) is assigned the bit number:
C
C                       ROW * MAXC + COL
C
C       Note that ROW and COL run from ZERO to MAXR-1 and MAXC-1, respectively.
C       The bits are numbered from zero.  Bit zero is the last bit in
C       MASK(MASKSIZE).  Bit 1 is the second-last bit in MASK(MASKSIZE).
C       Bit 32 is the first bit in MASK(MASKSIZE-1), and so on.
C       Note that the ROW and COL are absolute row and column numbers, not
C       array indices.

