#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(rdcomlin)
c       Parse the input line to get an initial macro file.
c       This is another Phil Pinto invention.  Thanks Phil.
c       This is a very preliminary implementation of command line parsing.
c       Numerous features will have to be added later, but we will have
c       to think of them first...
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1991 Philip A. Pinto
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       Internal Variables
	character       infile*(MXCMLN)
	integer         term
	logical         error
	real            data
	integer         i
C       External Functions
c       integer         PMGO(lenc)
	integer         iargc
C       Executable Code
#ifdef  unix
c           call getarg(0,infile)
c           write(STDERR,*) ' usage:  ',
c    &      infile(1: PMGO(lenc)(infile)), ' -t# [file [file ...]]'
	do 10 i=1,iargc()
	  call getarg(i,infile)
	  if (infile(1:2) .eq. '-t') then
	      read(infile(3:),*) term
	      call PMGO(device)(term)
	      call PMGO(tsetup)
	      call PMGO(erase)
	      termidle = .true.
	  else
	      call PTUV(readinput)(infile,error,data)
	      if (error) write(STDERR,*)
     &        ' Error reading command line file.'
	  endif
10      continue
#endif  /* unix */
	return
	end
C==============================================================================
C==============================================================================
