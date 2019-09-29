c   imsl routine name   - icsccu
c
c-----------------------------------------------------------------------
c
c   computer            - ibm/single
c
c   latest revision     - june 1, 1980
c
c   purpose             - cubic spline interpolation
c                           (easy-to-use version)
c
c   usage               - call icsccu (x,y,nx,c,ic,ier)
c
c   arguments    x      - vector of length nx containing the abscissae
c                           of the nx data points (x(i),y(i)) i=1,...,
c                           nx. (input) x must be ordered so that
c                           x(i) .lt. x(i+1).
c                y      - vector of length nx containing the ordinates
c                           (or function values) of the nx data points.
c                           (input)
c                nx     - number of elements in x and y. (input) nx
c                           must be .ge. 2.
c                c      - spline coefficients. (output) c is an nx-1 by
c                           3 matrix. the value of the spline
c                           approximation at t is
c                           s(t) = ((c(i,3)*d+c(i,2))*d+c(i,1))*d+y(i)
c                           where x(i) .le. t .lt. x(i+1) and
c                           d = t-x(i).
c                ic     - row dimension of matrix c exactly as
c                           specified in the dimension statement in
c                           the calling program. (input)
c                ier    - error parameter. (output)
c                         terminal error
c                           ier = 129, ic is less than nx-1.
c                           ier = 130, nx is less than 2.
c                           ier = 131, input abscissa are not ordered
c                             so that x(1) .lt. x(2) ... .lt. x(nx).
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
c   copyright           - 1980 by imsl, inc. all rights reserved.
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code. no other warranty,
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c
      subroutine icsccu (x,y,nx,c,ic,ier)
c                                  specifications for arguments
      integer            nx,ic,ier
      real               x(nx),y(nx),c(ic,3)
c                                  specifications for local variables
      integer            im1,i,jj,j,mm1,mp1,m,nm1,nm2
      real               divdf1,divdf3,dtau,g,cnx(3)
c                                  first executable statement
      nm1 = nx-1
      ier = 129
      if (ic .lt. nm1) go to 9000
      ier = 130
      if (nx .lt. 2) go to 9000
      ier = 131
      if (nx .eq. 2) go to 45
c                                  compute not-a-knot spline
      do 5 m = 2,nm1
         mm1=m-1
         c(m,2) = x(m)-x(mm1)
         if (c(m,2).le.0.0) go to 9000
         c(m,3) = (y(m)-y(mm1))/c(m,2)
    5 continue
      cnx(2) = x(nx)-x(nm1)
      if (cnx(2).le.0.0) go to 9000
      cnx(3) = (y(nx)-y(nm1))/cnx(2)
      ier = 0
      nm2 = nx-2
      if (nx .gt. 3) go to 10
      c(1,3) = cnx(2)
      c(1,2) = c(2,2)+cnx(2)
      c(1,1) = ((c(2,2)+2.*c(1,2))*c(2,3)*cnx(2)+c(2,2)**2*cnx(3))
     1/c(1,2)
      go to 20
   10 c(1,3) = c(3,2)
      c(1,2) = c(2,2)+c(3,2)
      c(1,1) = ((c(2,2)+2.*c(1,2))*c(2,3)*c(3,2)+c(2,2)**2*c(3,3))
     1/c(1,2)
      do 15 m=2,nm2
         mp1=m+1
         mm1=m-1
         g = -c(mp1,2)/c(mm1,3)
         c(m,1) = g*c(mm1,1)+3.*c(m,2)*c(mp1,3)+3.*c(mp1,2)*c(m,3)
         c(m,3) = g*c(mm1,2)+2.*c(m,2)+2.*c(mp1,2)
   15 continue
   20 g = -cnx(2)/c(nm2,3)
      c(nm1,1) = g*c(nm2,1)+3.*c(nm1,2)*cnx(3)+3.*cnx(2)*c(nm1,3)
      c(nm1,3) = g*c(nm2,2)+2.*c(nm1,2)+2.*cnx(2)
      if (nx.gt.3) go to 25
      cnx(1)=2.*cnx(3)
      cnx(3)=1.
      g=-1./c(nm1,3)
      go to 30
   25 g = c(nm1,2)+cnx(2)
      cnx(1) = ((cnx(2)+2.*g)*cnx(3)*c(nm1,2)+cnx(2)**2*
     1(y(nm1)-y(nx-2))/c(nm1,2))/g
      g = -g/c(nm1,3)
      cnx(3) = c(nm1,2)
   30 cnx(3) = g*c(nm1,2)+cnx(3)
      cnx(1) = (g*c(nm1,1)+cnx(1))/cnx(3)
      c(nm1,1) = (c(nm1,1)-c(nm1,2)*cnx(1))/c(nm1,3)
      do 35 jj=1,nm2
         j = nm1-jj
         c(j,1) = (c(j,1)-c(j,2)*c(j+1,1))/c(j,3)
   35 continue
      do 40 i=2,nm1
         im1 = i-1
         dtau = c(i,2)
         divdf1 = (y(i)-y(im1))/dtau
         divdf3 = c(im1,1)+c(i,1)-2.*divdf1
         c(im1,2) = (divdf1-c(im1,1)-divdf3)/dtau
         c(im1,3) = divdf3/dtau**2
   40 continue
      dtau = cnx(2)
      divdf1 = (y(nx)-y(nm1))/dtau
      divdf3 = c(nm1,1)+cnx(1)-2.*divdf1
      c(nm1,2) = (divdf1-c(nm1,1)-divdf3)/dtau
      c(nm1,3) = divdf3/dtau**2
      go to 9005
   45 if (x(1) .ge. x(2)) go to 9000
      ier = 0
      c(1,1) = (y(2)-y(1))/(x(2)-x(1))
      c(1,2) = 0.0
      c(1,3) = 0.0
      go to 9005
 9000 continue
      call uertst(ier,6hicsccu)
 9005 return
      end
