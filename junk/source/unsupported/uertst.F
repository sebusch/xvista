c   imsl routine name   - uertst
c
c-----------------------------------------------------------------------
c
c   computer            - ibm/single
c
c   latest revision     - january 1, 1978
c **                    - May 1982 by sjr
c
c
c   purpose             - print a message reflecting an error condition
c
c   usage               - call uertst (ier,name)
c
c   arguments    ier    - error parameter. (input)
c                           ier = i+j where
c                             i = 128 implies terminal error,
c                             i =  64 implies warning with fix, and
c                             i =  32 implies warning.
c                             j = error code relevant to calling
c                                 routine.
c                name   - a six character literal string giving the
c                           name of the calling routine. (input)
c
c   precision/hardware  - single/all
c
c   reqd. imsl routines - ugetio
c
c   notation            - information on special notation and
c                           conventions is available in the manual
c                           introduction or through imsl routine uhelp
c
c   remarks      the error message produced by uertst is written
c                onto the standard output unit. the output unit
c                number can be determined by calling ugetio as
c                follows..   call ugetio(1,nin,nout).
c                the output unit number can be changed by calling
c                ugetio as follows..
c                                nin = 0
c                                nout = new output unit number
c                                call ugetio(3,nin,nout)
c                see the ugetio document for more details.
c
c   copyright           - 1978 by imsl, inc. all rights reserved.
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code. no other warranty,
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c
      subroutine uertst (ier,name)
c                                  specifications for arguments
      integer            ier
      integer*2          name(3)
c                                  specifications for local variables
      integer*2          namset(3),nameq(3),ieq
      character*2        namsez(3),namez(3),iez
c
      equivalence (namset,namsez),(nameq,namez),(ieq,iez)
c
      data               namsez/'ue','rs','et'/
      data               namez/'  ','  ','  '/
      data               level/4/,ieqdf/0/,iez/'='/
c                                  first executable statement
      if (ier.gt.999) go to 25
      if (ier.lt.-32) go to 55
      if (ier.le.128) go to 5
      if (level.lt.1) go to 30
c                                  print terminal message
      call ugetio(1,nin,iounit)
      if (ieqdf.eq.1) write(iounit,35) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(iounit,35) ier,name
      go to 30
    5 if (ier.le.64) go to 10
      if (level.lt.2) go to 30
c                                  print warning with fix message
      call ugetio(1,nin,iounit)
      if (ieqdf.eq.1) write(iounit,40) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(iounit,40) ier,name
      go to 30
   10 if (ier.le.32) go to 15
c                                  print warning message
      if (level.lt.3) go to 30
      call ugetio(1,nin,iounit)
      if (ieqdf.eq.1) write(iounit,45) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(iounit,45) ier,name
      go to 30
   15 continue
c                                  check for uerset call
      do 20 i=1,3
         if (name(i).ne.namset(i)) go to 25
   20 continue
      levold = level
      level = ier
      ier = levold
      if (level.lt.0) level = 4
      if (level.gt.4) level = 4
      go to 30
   25 continue
      if (level.lt.4) go to 30
c                                  print non-defined message
      call ugetio(1,nin,iounit)
      if (ieqdf.eq.1) write(iounit,50) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(iounit,50) ier,name
   30 ieqdf = 0
      return
   35 format(19h *** terminal error,10x,7h(ier = ,i3,
     1       20h) from imsl routine ,3a2,a1,3a2)
   40 format(36h *** warning with fix error  (ier = ,i3,
     1       20h) from imsl routine ,3a2,a1,3a2)
   45 format(18h *** warning error,11x,7h(ier = ,i3,
     1       20h) from imsl routine ,3a2,a1,3a2)
   50 format(20h *** undefined error,9x,7h(ier = ,i5,
     1       20h) from imsl routine ,3a2,a1,3a2)
c                                  save p for p = r case
c                                    p is the page name
c                                    r is the routine name
   55 ieqdf = 1
      do 60 i=1,3
   60 nameq(i) = name(i)
   65 return
      end
