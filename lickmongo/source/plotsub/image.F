#include "Config.h"
C===============================================================================
C===============================================================================
	subroutine PMGO(image)(filename,error)
c       ** IMAGE **
c       mod jah 5/30/84
c       Modified 2/27/86 DJP -- lastfile reset to 0 if error mapping new file
c
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	character*64 filename
	logical error
	include 'MONGOPAR.inc'
c
	if (lastfile.ne.0) call PTUV(unmap)(lastfile)
	call PTUV(mapimage)(filename,idptr,imx,imy,error)
	if (.not.error) then
		lastfile=idptr
		write(37,*) ' image size ',imx,' by ',imy
	else
		lastfile = 0
	endif
	return
	end
C===============================================================================
C===============================================================================
	SUBROUTINE PTUV(mapimage)(filename, dataptr, imx, imy, error)
c       ** MAPIMAGE **
c       MAPIMAGE (filename, idptr, imx, imy, error)
c       -------
C       modified slightly from j. hudson's FITSMAP by P. Monger 7/mar/84
c       modified drastically 5/30/84 by jah
c               Maps file onto virtual memory area so that memory access causes
c       initial page faulting with retrieval from the given file.  Later, when
c       the area is UNMAPped, the information stored there, if modified, is
c       written out onto the specified file.
c               filename C*(*)  Name of data file, IMX by IMY pixels, REAL*4
c                               data.
c               dataptr I*4     Pointer to image data. Pass using %val(dataptr)
c                               to any routine expecting to see an array.
c               imx,imy I*4     Number of pixels in x, y directions.  Picture
c                               stored by rows (x), increasing y coordinate.
c               error   L       T if something went wrong
c       The address to be used for UNMAPping a UCFITS file is that of DATAPTR.
c       Do not pass by %val; see below.
c
c       UNMAP (address)
c       -----
c               [NCAR LIBRARY ROUTINE.]
c               Release storage occupied by an image.  Use DATAPTR for the
c       required address (do not pass by value).
c               address I*4     Address of region of virtual memory which had
c                               previously been assigned to a map file using
c                               MAPFILE [NCAR] or MAPIMAGE.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	  CHARACTER*(*) filename
	  INTEGER dataptr, imx, imy
	  LOGICAL error
c
	  INTEGER lrecl, nrec
	  INTEGER i, nbytes
	  LOGICAL existing
	  INTEGER LIB$GET_LUN, LIB$FREE_LUN
	  EXTERNAL SS$_NORMAL
	  INTEGER  PTUV(filesize)
	  EXTERNAL PTUV(filesize)

	  INQUIRE (file=filename, exist=existing, recl=lrecl)
	  IF (.NOT. existing) THEN
	    TYPE *, 'MAPIMAGE -- Non-existent file: ' // filename
	    ERROR=.TRUE.
	    RETURN
	  ENDIF

	  nrec = PTUV(filesize)(filename, lrecl)
	  TYPE *, 'MAPIMAGE -- File: ' // filename // ' has ', nrec,
	1       ' records of length ', lrecl
	  lrecl = lrecl/4               ! Convert to longword count
	  nbytes = 4*nrec*lrecl         ! Multiply by size of a pixel,
					! nrows, ncols
	  CALL PTUV(mapfile)(filename, 'R', nbytes, dataptr)
					! Now make the file the swap file
					! for paging, and allocate nbytes
					! of virtual memory.
	  imx = lrecl
	  imy = nrec
	  RETURN
	END
C===============================================================================
C===============================================================================
	INTEGER FUNCTION PTUV(Filesize)(name, lrecl)
c
c       ** FILESIZE **
c       how many records in this file?
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	  CHARACTER*(*) name
	  INTEGER lrecl
	  REAL*4 x
	  INTEGER kunit

	  PTUV(Filesize) = 0
	  CALL LIB$GET_LUN (kunit)
	  OPEN (kunit, FILE=name, FORM='UNFORMATTED', RECL=lrecl/4,
	1       RECORDTYPE='FIXED', READONLY, STATUS='OLD')
	  DO WHILE (.TRUE.)
	    READ (kunit, END=900) x
	    PTUV(Filesize) = PTUV(Filesize) + 1
	  ENDDO
900       CONTINUE
	  CLOSE (kunit)
	  CALL LIB$FREE_LUN (kunit)
	  RETURN
	END
C===============================================================================
C===============================================================================
