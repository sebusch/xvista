c   imsl routine name   - icsevu
c
c-----------------------------------------------------------------------
c
c   computer            - ibm/single
c
c   latest revision     - january 1, 1978
c
c   purpose             - evaluation of a cubic spline
c
c   usage               - call icsevu(x,y,nx,c,ic,u,s,m,ier)
c
c   arguments    x      - vector of length nx containing the abscissae
c                           of the nx data points (x(i),y(i)) i=1,...,
c                           nx (input). x must be ordered so that
c                           x(i) .lt. x(i+1).
c                y      - vector of length nx containing the ordinates
c                           (or function values) of the nx data points
c                           (input).
c                nx     - number of elements in x and y (input).
c                           nx must be .ge. 2.
c                c      - spline coefficients (input). c is an nx-1 by
c                           3 matrix.
c                ic     - row dimension of matrix c exactly as
c                           specified in the dimension statement
c                           in the calling program (input).
c                           ic must be .ge. nx-1.
c                u      - vector of length m containing the abscissae
c                           of the m points at which the cubic spline
c                           is to be evaluated (input).
c                s      - vector of length m (output).
c                           the value of the spline approximation at
c                           u(i) is
c                           s(i) = ((c(j,3)*d+c(j,2))*d+c(j,1))*d+y(j)
c                           where x(j) .le. u(i) .lt. x(j+1) and
c                           d = u(i)-x(j).
c                m      - number of elements in u and s (input).
c                ier    - error parameter (output).
c                         warning error
c                           ier = 33, u(i) is less than x(1).
c                           ier = 34, u(i) is greater than x(nx).
c
c   precision/hardware  - single and double/h32
c                       - single/h36,h48,h60
c
c   reqd. imsl routines - uertst,ugetio
c
c   notation            - information on special notation and
c                           conventions is available in the manual
c                           introduction or through imsl routine uhelp
c
c   remarks  1.  the routine assumes that the abscissae of the nx
c                data points are ordered such that x(i) is less than
c                x(i+1) for i=1,...,nx-1. no check of this condition
c                is made in the routine. unordered abscissae will cause
c                the algorithm to produce incorrect results.
c            2.  the routine generates two warning errors. one error
c                occurs if u(i) is less than x(1), for some i in the
c                the interval (1,m) inclusively. the other error occurs
c                if u(i) is greater than x(nx), for some i in the
c                interval (1,m) inclusively.
c            3.  the ordinate y(nx) is not used by the routine. for
c                u(k) .gt. x(nx-1), the value of the spline, s(k), is
c                given by
c                 s(k)=((c(nx-1,3)*d+c(nx-1,2))*d+c(nx-1,1))*d+y(nx-1)
c                where d=u(k)-x(nx-1).
c
c   copyright           - 1978 by imsl, inc. all rights reserved.
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code. no other warranty,
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c
      subroutine icsevu  (x,y,nx,c,ic,u,s,m,ier)
c                                  specifications for arguments
      integer            nx,ic,m,ier
      real               x(nx),y(nx),c(ic,3),u(m),s(m)
c                                  specifications for local variables
      integer            i,jer,ker,nxm1,k
      real               d,dd,zero
      data               i/1/,zero/0.0/
c                                  first executable statement
      jer = 0
      ker = 0
      if (m .le. 0) go to 9005
      nxm1 = nx-1
      if (i .gt. nxm1) i = 1
c                                  evaluate spline at m points
      do 40 k=1,m
c                                  find the proper interval
         d = u(k)-x(i)
         if (d) 5,25,15
    5    if (i .eq. 1) go to 30
         i = i-1
         d = u(k)-x(i)
         if (d) 5,25,20
   10    i = i+1
         d = dd
   15    if (i .ge. nx) go to 35
         dd = u(k)-x(i+1)
         if (dd .ge. zero) go to 10
         if (d .eq. zero) go to 25
c                                  perform evaluation
   20    s(k) = ((c(i,3)*d+c(i,2))*d+c(i,1))*d+y(i)
         go to 40
   25    s(k) = y(i)
         go to 40
c                                  warning - u(i) .lt. x(1)
   30    jer = 33
         go to 20
c                                  if u(i) .gt. x(nx) - warning
   35    if (dd .gt. zero) ker = 34
         d = u(k)-x(nxm1)
         i = nxm1
         go to 20
   40 continue
      ier = max0(jer,ker)
 9000 continue
      if (jer .gt. 0) call uertst(jer,6hicsevu)
      if (ker .gt. 0) call uertst(ker,6hicsevu)
 9005 return
      end
