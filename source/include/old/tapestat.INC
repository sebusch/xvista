#include "Vista.h"
C
C   TAPESTAT.INC
C
C   Magnetic Tape Device Status Information for VISTA
C
C   WARNING:  VAX/VMS Dependent!!
C
C   VISTA Version 4.0 uses the MTPCKG VAX/VMS magtape primitives for
C         VMS installations.  Up to 8 tape drives may be accessed at
C         one time by MTPCKG.
C
C   Tape Counters
C
C      VNTAPES    (I*4)     Number of Tapes available on the VMS System
C                            derived in subroutine INITIAL at startup.
C
C      VNMOUNTED  (I*4)     Number of tape drives currently mounted by the
C                            user.
C
C   Drive/Tape Parameters
C
C      VMTCHAN(I) (I*4)     Physical device channel assigned to UNIT when
C                            the device was mounted (by MTOPID subroutine call)
C                            I = UNIT + 1
C
C      VMTNAME(I) (CHAR*6)  Name of the tape drive (e.g., _MTA0:) associated
C                            with UNIT (I = UNIT + 1).
C
C      VMTDENS(I) (I*4)     Tape data density.  One of 800, 1600, or 6250,
C                            depending on how the tape was initialized.
C
C      VMTWRT(I) (Logical)  .TRUE. if tape is WRITE ENABLED
C                            .FALSE. if tape is WRITE LOCKED.
C
C      VMTFORM(I)  I*4      Code giving tape format:  Codes:
C
C                                    -1 :  Unknown
C                                     0 :  Standard FITS (2880 byte records)
C                                     1 :  Lick "New" FITS (2880 bytes, and
C                                          first image is blank
C                                     2 :  Future (6250 "blocked" ST format?)
C
C   Last Operation Flags
C
C      VMTPOSN(I) (I*4)     Last known position of tape.  0 if BOT, -1 if
C                            unknown
C
C      VMTLAST  (I*4)       UNIT number of the last tape drive the user did
C                            something to (either RT, WT, MOUNT, TDIR, or INT).
C                            If user did a DISMOUNT, VMTLAST is set equal to
C                            -1.  VMTLAST=-1 means "LAST UNIT UNKNOWN".  This
C                            is the value it is given at startup by INITIAL.
C
C      VMTLCOM  (CHAR*20)   Last Tape command issued.
C
C   Miscellaneous
C
C      VMTUNIT(I) (I*4)     Tape units currently mounted by the user. "I" is
C                            a number between 1 and NMOUNTED.
C

        INTEGER  VNTAPES
        INTEGER  VNMOUNTED
        INTEGER  VMTLAST

        INTEGER  VMTCHAN(8)
        INTEGER  VMTDENS(8)
        INTEGER  VMTUNIT(8)
        INTEGER  VMTPOSN(8)
        INTEGER  VMTFORM(8)

        LOGICAL  VMTWRT(8)
C char*6 for VMS next not 24
        CHARACTER*24  VMTNAME(8)
        CHARACTER*20 VMTLCOM

        COMMON /TAPESTAT/ VMTNAME, VMTCHAN, VMTDENS, VMTUNIT,
     &                    VMTPOSN, VMTFORM, VMTLCOM, VMTWRT,
     &                    VNTAPES, VNMOUNTED, VMTLAST

