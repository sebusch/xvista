c   imsl routine name   - uerset
c
c-----------------------------------------------------------------------
c
c   computer            - ibm/single
c
c   latest revision     - january 1, 1978
c
c   purpose             - set message level for imsl routine uertst
c
c   usage               - call uerset (level,levold)
c
c   arguments    level  - new value for message level. (input)
c                           output from imsl routine uertst is
c                           controlled selectively as follows,
c                             level = 4 causes all messages to be
c                                       printed,
c                             level = 3 messages are printed if ier is
c                                       greater than 32,
c                             level = 2 messages are printed if ier is
c                                       greater than 64,
c                             level = 1 messages are printed if ier is
c                                       greater than 128,
c                             level = 0 all message printing is
c                                       suppressed.
c                levold - previous message level. (output)
c
c   precision/hardware  - single/all
c
c   reqd. imsl routines - uertst,ugetio
c
c   notation            - information on special notation and
c                           conventions is available in the manual
c                           introduction or through imsl routine uhelp
c
c   copyright           - 1978 by imsl, inc. all rights reserved.
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code. no other warranty,
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c
      subroutine uerset (level,levold)
c                                  specifications for arguments
      integer            level,levold
c                                  first executable statement
      levold = level
      call uertst (levold,6huerset)
      return
      end
