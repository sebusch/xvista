#include "Vista.h"
      subroutine bcgms(n, b, x,  msolve, mtsolv,
     1                  itol, tol, itmax, iter, err, iunit1, iunit2)
      dimension b(1), x(1)
      external msolve
      external mtsolv
c
c
c  Routine to solve a linear system  Ax = b  using the biconjugate
c  gradient ethod with a matrix splitting.
c
c
c---------------------------------------------------------------------
c
c  Arguments:
c
c   Input:
c    n      - order of the matrix.
c    b      - right-hand side vector, of length n.
c    x      - initial guess for solution vector, of length n.
c    msolve - name of subroutine which solves a linear system
c             with the splitting matrix.  must be declared
c             external in the calling program.  the calling
c             sequence is
c                  CALL MSOLVE(N, R, Z)
c             where n is the number of unknowns, r is the right-hand
c             side vector, and z is the solution upon return.
c    mtsolv - name of subroutine which solves a linear system
c             with the transpose of the splitting matrix.  must
c             be declared external in the calling program.  the
c             calling sequence is
c                  CALL MTSOLV(N, R, Z)
c             where n is the number of unknowns, r is the right-hand
c             side vector, and z is the solution upon return.
c    itol   - flag to indicate type of convergence criterion.
c             if itol=1, iteration stops when the 2-norm of
c                        the residual divided by the 2-norm of
c                        the right-hand side is less than tol.
c             if itol=2, iteration stops when the 2-norm of
c                        M-inv times the residual divided by
c                        the 2-norm of M-inv times the right-
c                        hand side is less than tol.
c             if itol=3, iteration stops when estimate of the
c                        2-norm of the error divided by the
c                        2-norm of the solution is less than tol.
c             if itol=4, iteration stops when estimate of the
c                        infinity-norm of the error divided by the
c                        infinity-norm of the solution is less
c                        than tol.
c
c             itol=5 or itol=6 are often useful for checking and
c             comparing different routines.  For these cases, the
c             user must supply the "exact" solution or a very
c             accurate approximation (one with an error much less
c             than tol) through a common block,
c                      common /solblk/ soln( )
c             if itol=5, iteration stops when the 2-norm of the
c                        difference between the iterative
c                        approximation and the user-supplied
c                        solution divided by the 2-norm of the
c                        user-supplied solution is less than tol.
c             if itol=6, iteration stops when the infinity-norm
c                        of the difference between the iterative
c                        approximation and the user-supplied
c                        solution divided by the infinity-norm
c                        of the user-supplied solution is less
c                        than tol.
c
c    tol    - convergence criterion, as described above.
c    itmax  - maximum number of iterations.
c    iunit1,
c    iunit2 - unit number(s) on which to write the error at each
c             iteration, if this is desired for monitoring
c             convergence.  if unit number is 0, no writing will
c             occur.
c
c   Output:
c    x      - final approximate solution.
c    iter   - number of iterations required to reach convergence,
c             or itmax+1 if convergence criterion could not be
c             achieved in itmax iterations.
c    err    - error in final approximate solution, as defined
c             by itol.
c
c---------------------------------------------------------------------
c
c..get space for auxilliary vectors r, rr, z, zz, p, pp.
c      pointer (ipr,r), (ipz,z), (ipp,p), (iprr,rr), (ipzz,zz),
c     1        (ippp,pp)
c      dimension r(1), z(1), p(1), rr(1), zz(1), pp(1)
c      call mzalloc(ipr,n)
c      call mzalloc(ipz,n)
c      call mzalloc(ipp,n)
c      call mzalloc(iprr,n)
c      call mzalloc(ipzz,n)
c      call mzalloc(ippp,n)
       parameter (maxstr=150, maxunk=3*maxstr+1)
       dimension r(maxunk),z(maxunk),p(maxunk),rr(maxunk),zz(maxunk)
       dimension pp(maxunk)
c
c..calculate initial residual and check stopping criterion.
      iter = 0
      call atimes(n, x, r, 0)
      call saxpy(n, -1., b, 1, r, 1)
      call sscal(n, -1., r, 1)
      call scopy(n, r, 1, rr, 1)
c
c      if(iter .eq. 0) bnrm = snrm2(n, b, 1)
c      err = snrm2(n, r, 1)/bnrm
      if( isbcg(n, b, x,  msolve, itol, tol,
     1    itmax, iter, err, iunit1, iunit2, r, z, p, rr, zz, pp, ak,
     &     bk)
     1    .ne. 0 ) go to 200
c
c
c          ***** iteration loop *****
c
      do 100 k=1,itmax
        iter = k
c            if z was computed for stop test, do not compute again.
        if(itol .eq. 2 .or. itol .eq. 3 .or. itol .eq. 4) go to 20
c
c..  solve  mz = r.
        call msolve(n, r, z)
c
c..  solve (m-trans)*zz = rr
20      call mtsolv(n, rr, zz)
c
c..  calculate coefficient bk and direction vectors p and pp.
        bknum = sdot(n, z, 1, rr, 1)
        if(iter .eq. 1) then
          call scopy(n, z, 1, p, 1)
          call scopy(n, zz, 1, pp, 1)
          go to 30
          endif
        bk = bknum/bkden
        call sscal(n, bk, p, 1)
        call saxpy(n, 1., z, 1, p, 1)
        call sscal(n, bk, pp, 1)
        call saxpy(n, 1., zz, 1, pp, 1)
30      bkden = bknum
c
c..  calculate coefficient ak, new iterate x, and new resids r and rr.
        call atimes(n, p, z, 0)
        akden = sdot(n, pp, 1, z, 1)
        ak = bknum/akden
        call saxpy(n, ak, p, 1, x, 1)
        call saxpy(n, -ak, z, 1, r, 1)
        call atimes(n, pp, zz, 1)
        call saxpy(n, -ak, zz, 1, rr, 1)
c
c..  check stopping criterion.
c      if(iter .eq. 0) bnrm = snrm2(n, b, 1)
c      err = snrm2(n, r, 1)/bnrm
        if( isbcg(n, b, x,  msolve, itol, tol,
     1      itmax, iter, err, iunit1, iunit2, r, z, p, rr,zz,pp,ak,bk)
     1      .ne. 0 ) go to 200
c
100     continue
c
c          *****   end of loop  *****
c
c
c..stopping criterion not satisfied.
      iter = itmax + 1
c
c..release space for auxilliary vectors and return.
200    continue
c200   call mzdalloc(ipr,0)
c      call mzdalloc(ipz,0)
c      call mzdalloc(ipp,0)
c      call mzdalloc(iprr,0)
c      call mzdalloc(ipzz,0)
c      call mzdalloc(ippp,0)
      return
      end
      function isbcg(n, b, x,  msolve, itol, tol,
     1                itmax, iter, err, iunit1, iunit2, r, z, p,
     1                rr, zz, pp, ak, bk)
      dimension b(1), x(1), r(1), z(1), p(1),
     1          rr(1), zz(1), pp(1)
      common /solblk/ soln(1)
c      pointer (ipdz,dz)
c      dimension dz(1)
      parameter (maxstr=150, maxunk=3*maxstr+1)
      dimension dz(maxunk)
      save
5     format(8h iter = ,i4,3x,7h err = ,e12.3)
c
      isbcg = 0
      go to (10,20,30,40,50,60),itol
c
10    if(iter .eq. 0) bnrm = snrm2(n, b, 1)
      err = snrm2(n, r, 1)/bnrm
      go to 100
c
20    if(iter .eq. 0) then
        call msolve(n, b, z)
        bnrm = snrm2(n, z, 1)
        endif
      call msolve(n, r, z)
      err = snrm2(n, z, 1)/bnrm
      go to 100
c
30    if(iter .eq. 0) then
        call msolve(n, b, z)
        bnrm = snrm2(n, z, 1)
        call msolve(n, r, z)
        znrm = snrm2(n, z, 1)
        err = znrm/bnrm
        endif
      if(iter .ne. 0) then
        call msolve(n, r, z)
        znrm = snrm2(n, z, 1)
          if(abs(zm1nrm-znrm) .gt. 1.e-14*znrm) then
            dxnrm = abs(ak)*snrm2(n, p, 1)
            err = znrm/abs(zm1nrm-znrm) * dxnrm
          else
            err = znrm/bnrm
            if(iunit1 .ne. 0) write(iunit1,31) iter,err
            if(iunit2 .ne. 0) write(iunit2,31) iter,err
31          format(/40h following err est may not be accurate. ,
     1              30h zm1nrm  approx equal  znrm.  /
     1              8h iter = ,i4,3x,7h err = ,e12.3/)
            zm1nrm = znrm
            return
          endif
        xnrm = snrm2(n, x, 1)
          if(err .le. .5*xnrm) then
            err = err/xnrm
          else
            err = znrm/bnrm
            if(iunit1 .ne. 0) write(iunit1,32) iter,err
            if(iunit2 .ne. 0) write(iunit2,32) iter,err
32          format(/40h following err est may not be accurate. ,
     1              30h err  greater than  .5 * soln./
     1              8h iter = ,i4,3x,7h err = ,e12.3/)
            zm1nrm = znrm
            return
          endif
        endif
      zm1nrm = znrm
      go to 100
c
40    if(iter .eq. 0) then
        call msolve(n, b, z)
        imax = isamax(n, z, 1)
        bnrm = abs(z(imax))
        call msolve(n, r, z)
        imax = isamax(n, z, 1)
        znrm = abs(z(imax))
        err = znrm/bnrm
        endif
      if(iter .ne. 0) then
        call msolve(n, r, z)
        imax = isamax(n, z, 1)
        znrm = abs(z(imax))
          if(abs(zm1nrm-znrm) .gt. 1.e-14*znrm) then
            imax = isamax(n, p, 1)
            dxnrm = abs(ak)*abs(p(imax))
            err = znrm/abs(zm1nrm-znrm) * dxnrm
          else
            err = znrm/bnrm
            if(iunit1 .ne. 0) write(iunit1,31) iter,err
            if(iunit2 .ne. 0) write(iunit2,31) iter,err
            zm1nrm = znrm
            return
          endif
        imax = isamax(n, x, 1)
        xnrm = abs(x(imax))
          if(err .le. .5*xnrm) then
            err = err/xnrm
          else
            err = znrm/bnrm
            if(iunit1 .ne. 0) write(iunit1,32) iter,err
            if(iunit2 .ne. 0) write(iunit2,32) iter,err
            zm1nrm = znrm
            return
          endif
        endif
      zm1nrm = znrm
      go to 100
c
50    if(iter .eq. 0) then
c        call mzalloc(ipdz,n)
        solnrm = snrm2(n, soln, 1)
        endif
      call scopy(n, x, 1, dz, 1)
      call saxpy(n, -1., soln, 1, dz, 1)
      err = snrm2(n, dz, 1)/solnrm
c      if(err .le. tol .or. iter .eq. itmax) call mzdalloc(ipdz,0)
      go to 100
c
60    if(iter .eq. 0) then
c        call mzalloc(ipdz,n)
        imax = isamax(n, soln, 1)
        solnrm = abs(soln(imax))
        endif
      call scopy(n, x, 1, dz, 1)
      call saxpy(n, -1., soln, 1, dz, 1)
      imax = isamax(n, dz, 1)
      err = abs(dz(imax))/solnrm
c      if(err .le. tol .or. iter .eq. itmax) call mzdalloc(ipdz,0)
      go to 100
c
100   if(iunit1 .ne. 0) write(iunit1,5) iter,err
      if(iunit2 .ne. 0) write(iunit2,5) iter,err
      if(err .le. tol) isbcg = 1
c
      return
      end
c
c
c
      subroutine saxpy(n,sa,sx,incx,sy,incy)
c
      real sx(1), sy(1)
c
      do 10 i=1,n
         ix = incx*(i-1)+1
         iy = incy*(i-1)+1
10       sy(iy) = sa*sx(ix) + sy(iy)
c
      return
      end
c
      subroutine sscal(n,sa,sx,incx)
c
      real sx(1)
c
      do 10 i=1,n,incx
10    sx(i) = sa*sx(i)
c
      return
      end
c
      subroutine scopy(n,sx,incx,sy,incy)
c
      real sx(1), sy(1)
c
      do 10 i=1,n
         ix = incx*(i-1)+1
         iy = incy*(i-1)+1
10    sy(iy) = sx(ix)
c
      return
      end
c
      function sdot(n,sx,incx,sy,incy)
c
      real sx(1), sy(1)
c
      sdot = 0.
      do 10 i=1,n
         ix = incx*(i-1)+1
         iy = incy*(i-1)+1
10    sdot = sdot + sx(ix)*sy(iy)
c
      return
      end
c
      function snrm2(n,sx,incx)
c
      real sx(1)
c
      snrm2 = 0.
      do 10 i=1,n,incx
10    snrm2 = snrm2 + sx(i)**2
      snrm2 = sqrt(snrm2)
c
      return
      end
c
      function isamax(n,sx,incx)
c
      real sx(1)
c
      isamax = 1
      do 10 i=1,n,incx
10    if(abs(sx(i)).gt.abs(sx(isamax))) isamax = i
c
      return
      end

      subroutine atimes(n, x, r, itrans)

      parameter(maxstr=150, maxunk=3*maxstr+1)
      real*4 x(*),r(*)

      common /matrix/ c(maxunk,maxunk)

      do 5701 i=1,n
        r(i) = 0.
        do 5702 j=1,n
          r(i) = r(i) + c(j,i)*x(j)
 5702   continue
 5701 continue

      return
      end

      subroutine msolve(n,r,z)

      parameter(maxstr=150, maxunk=3*maxstr+1)
      real*4 r(*),z(*)

      common /matrix/ c(maxunk,maxunk)

      do 5701 i=1,n
        z(i) = r(i)/c(i,i)
 5701 continue

      return
      end

      subroutine mtsolv(n,r,z)

      parameter(maxstr=150, maxunk=3*maxstr+1)
      real*4 r(*),z(*)

      common /matrix/ c(maxunk,maxunk)

      do 5701 i=1,n
        z(i) = r(i)/c(i,i)
 5701 continue

      return
      end
