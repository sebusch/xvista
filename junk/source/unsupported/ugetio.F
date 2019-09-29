c   imsl routine name   - ugetio
c
c-----------------------------------------------------------------------
c
c   computer            - ibm/single
c
c   latest revision     - january 1, 1978
c
c   purpose             - to retrieve current values and to set new
c                           values for input and output unit
c                           identifiers.
c
c   usage               - call ugetio(iopt,nin,nout)
c
c   arguments    iopt   - option parameter. (input)
c                           if iopt=1, the current input and output
c                           unit identifier values are returned in nin
c                           and nout, respectively.
c                           if iopt=2 (3) the internal value of
c                           nin (nout) is reset for subsequent use.
c                nin    - input unit identifier.
c                           output if iopt=1, input if iopt=2.
c                nout   - output unit identifier.
c                           output if iopt=1, input if iopt=3.
c
c   precision/hardware  - single/all
c
c   reqd. imsl routines - none required
c
c   notation            - information on special notation and
c                           conventions is available in the manual
c                           introduction or through imsl routine uhelp
c
c   remarks      each imsl routine that performs input and/or output
c                operations calls ugetio to obtain the current unit
c                identifier values. if ugetio is called with iopt=2 or 3
c                new unit identifier values are established. subsequent
c                input/output is performed on the new units.
c
c   copyright           - 1978 by imsl, inc. all rights reserved.
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code. no other warranty,
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c
      subroutine ugetio(iopt,nin,nout)
c                                  specifications for arguments
      integer            iopt,nin,nout
c                                  specifications for local variables
      integer            nind,noutd
      data               nind/5/,noutd/6/
c                                  first executable statement
      if (iopt.eq.3) go to 10
      if (iopt.eq.2) go to 5
      if (iopt.ne.1) go to 9005
      nin = nind
      nout = noutd
      go to 9005
    5 nind = nin
      go to 9005
   10 noutd = nout
 9005 return
      end
