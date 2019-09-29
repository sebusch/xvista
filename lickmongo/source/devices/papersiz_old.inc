C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       This file defines the standard paper sizes which are in use in
c       most countries.  Both the U.S. (inch) and International (metric)
c       paper sizes are defined.  A few non-standard paper sizes are also
c       defined.  These paper sizes are intended for use with hardcopy
c       devices such as PostScript printers, HP plotters, etc.
c
c       See also the file paperdat.inc for data initializations.
c
c       Note the distinction between paper sizes and paper orientations.
C===============================================================================
C-------------------------------------------------------------------------------
c                       "Unknown" paper size.  The Lick Mongo setphysical()
c                       subroutine can be used to define the actual size
c                       of the paper.
	integer         SpsU
	parameter       (SpsU = 0)
C===============================================================================
c               Standard U.S. Paper sizes
C-------------------------------------------------------------------------------
c                       "A" paper size.  (8.5in x 11in)
	integer         SpsA
	parameter       (SpsA = 1)
c                       "B" paper size.  (11in x 17in)
	integer         SpsB
	parameter       (SpsB = 2)
c                       "C" paper size.  (17in x 22in)
	integer         SpsC
	parameter       (SpsC = 3)
c                       "D" paper size.  (22in x 34in)
	integer         SpsD
	parameter       (SpsD = 4)
c                       "E" paper size.  (34in x 44in)
	integer         SpsE
	parameter       (SpsE = 5)
C===============================================================================
c               Random U.S. Paper sizes
C-------------------------------------------------------------------------------
c                       "Note" paper size.  (5.5in x 8.5in)
c                       This is not really a standard, but some laserprinters
c                       have a paper tray for this size.
	integer         SpsN
	parameter       (SpsN = 6)
c                       "Legal" paper size.  (8.5in x 14in)
c                       Again, not really a standard, but until society takes
c                       Shakespeare's advice, we're stuck with it.
	integer         SpsL
	parameter       (SpsL = 7)
C===============================================================================
c               Standard Metric Paper sizes
C-------------------------------------------------------------------------------
c                       "A0" paper size.  (841mm x 1189mm)
	integer         SpsA0
	parameter       (SpsA0 = 8)
c                       "A1" paper size.  (594mm x 841mm)
	integer         SpsA1
	parameter       (SpsA1 = 9)
c                       "A2" paper size.  (420mm x 594mm)
	integer         SpsA2
	parameter       (SpsA2 = 10)
c                       "A3" paper size.  (297mm x 420mm)
	integer         SpsA3
	parameter       (SpsA3 = 11)
c                       "A4" paper size.  (210mm x 297mm)
	integer         SpsA4
	parameter       (SpsA4 = 12)
C===============================================================================
c                       this should reflect the maximum number just above
	integer         MAXSPS
	parameter       (MAXSPS = 12)
C===============================================================================
	integer         SpSHRT
	parameter       (SpSHRT = 1)
	integer         SpLONG
	parameter       (SpLONG  = 2)
c       Standard Paper Sizes--all edge lengths expressed in integer
c       multiples of micrometers (10**(-6) meter).
c               Common Block carrying the actual paper size data
	integer         SpSIZ(SpSHRT:SpLONG,0:MAXSPS)
	common  /spsize/        SpSIZ
C===============================================================================
c                       how many tray sizes do we know about so far?
	integer         KNOWNTRAYS
	parameter       (KNOWNTRAYS = 4)
c       Given a name of a paper tray, we want to look up
c       the standard paper size and orientation.
c                       what standard paper size is this?
	integer         traysps(0:KNOWNTRAYS)
c                       is the default orientation of this tray Portrait?
	logical         trayland(0:KNOWNTRAYS)
c                       by what name is this tray known?
	character*16    trayname(0:KNOWNTRAYS)
	common  /sptray/        traysps, trayland, trayname
C===============================================================================
c                       how many micrometers are there in one inch
	real*4          umPERinch
	parameter       (umPERinch = 25400.)
c                       how Adobe points are there in one inch
	real*4          AptPERinch
	parameter       (AptPERinch = 72.)
C===============================================================================
