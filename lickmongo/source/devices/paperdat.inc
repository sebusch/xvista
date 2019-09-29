C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       You may well be asking yourself, "Why is this stuff not in a
c       data file which is read in at runtime?"  Believe me, I am asking
c       myself that same question.
c
c       This file exists because Fortran does not yet allow parameter arrays.
c
c       This file defines the standard paper orientations which we have
c       actually encountered on various printers (mostly PostScript).
c       There is considerable room for improvement, and some printers may
c       require altered definitions.
c       These definitions are intended for use with hardcopy
c       devices such as PostScript printers, HP plotters, etc.
c
c       See also the file papersiz.inc for definitions.
c
c       Note the distinction between paper sizes and paper orientations.
C===============================================================================
c       Standard Paper Sizes--all edge lengths expressed in integer
c       multiples of micrometers (10**(-6) meter).
C-------------------------------------------------------------------------------
	data    (SpSIZ(i,0),i=SpSHRT,SpLONG) /
     &          215900,  279400/
	data    ((SpSIZ(i,j),i=SpSHRT,SpLONG),j=SpsA,SpsE) /
     &          215900,  279400,
     &          279400,  431800,
     &          431800,  558800,
     &          558800,  863600,
     &          863600, 1117600/
	data    ((SpSIZ(i,j),i=SpSHRT,SpLONG),j=SpsN,SpsL) /
     &          139700,  215900,
     &          215900,  355600/
	data    ((SpSIZ(i,j),i=SpSHRT,SpLONG),j=SpsA0,SpsA4) /
     &          841000, 1189000,
     &          594000,  841000,
     &          420000,  594000,
     &          297000,  420000,
     &          210000,  297000/
C===============================================================================
c       Standard Paper Orientations
C  *** added "a4" paper tray as "known" [rwp/osu 95 Nov 30] ***

	data    (trayname(i),traysps(i),trayland(i),i=0,KNOWNTRAYS) /
     &          'unknown', SpsU , .false.,
     &          'letter' , SpsA , .false.,
     &          'letterr', SpsA , .true. ,
     &          'ledger' , SpsB , .true. ,
     &          '11x17'  , SpsB , .true. ,
     &          'a4'     , SpsA4, .false. /
