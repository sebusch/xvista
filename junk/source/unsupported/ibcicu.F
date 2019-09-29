c   imsl routine name   - ibcicu                                        
c                                                                       
c-----------------------------------------------------------------------
c                                                                       
c   computer            - vax/single                                    
c                                                                       
c   latest revision     - june 1, 1980                                  
c                                                                       
c   purpose             - bicubic spline two-dimensional coefficient    
c                           calculator                                  
c                                                                       
c   usage               - call ibcicu (f,ifd,x,nx,y,ny,lx,ly,c,wk,ier)  
c                                                                       
c   arguments    f      - nx by ny matrix containing the function       
c                           values. (input) f(i,j) is the function value
c                           at the point (x(i),y(j)) for i=1,...,nx and 
c                           j=1,...,ny.                                 
c                ifd    - row dimension of the matrix f exactly         
c                           as specified in the dimension statement     
c                           in the calling program. (input)             
c                x      - vector of length nx. (input) x must be        
c                           ordered so that x(i) .lt. x(i+1) for        
c                           i=1,...,nx-1.                               
c                nx     - number of elements in x. (input) nx must be   
c                           .ge. 2.                                     
c                y      - vector of length ny. (input) y must be        
c                           ordered so that y(j) .lt. y(j+1) for        
c                           j=1,...,ny-1.                               
c                ny     - number of elements in y. (input) ny must be   
c                           .ge. 2.                                     
c                         note - the coordinate pairs (x(i),y(j)), for  
c                           i=1,...,nx and j=1,...,ny, give the points  
c                           where the function values f(i,j) are        
c                           defined.                                    
c                lx     - x-direction interval for which spline         
c                           coefficients are sought. (input) lx must be 
c                           .ge. 1 and lx must be .lt. nx.              
c                ly     - y-direction interval for which spline         
c                           coefficients are sought. (input) ly must be 
c                           .ge. 1 and ly must be .lt. ny.              
c                         note - spline coefficients are determined for 
c                           the rectangle, r, containing all points     
c                           (x,y) that satisfy the following two        
c                           conditions,                                 
c                             1. x(lx) .le. x .le. x(lx+1)              
c                             2. y(ly) .le. y .le. y(ly+1).             
c                c      - 4 by 4 matrix of spline coefficients. (output)
c                           the value of the spline approximation at a  
c                           point (p1,p2) in the rectangle r is given by
c                           s(p1,p2) = sum over i,j of                  
c                                                                       
c                             c(i,j)*(p1-x(lx))**(i-1)*(p2-y(ly))**(j-1)
c                                                                       
c                           for i=1,...,4 and j=1,...,4.                
c                wk     - work vector of length                         
c                           4*max(4,ny)+max((nx-1)*3,(ny-1)*3+ny)       
c                ier    - error parameter. (output)                     
c                         terminal error                                
c                           ier = 129, ifd is less than nx.             
c                           ier = 130, nx is less than 2.               
c                           ier = 131, ny is less than 2.               
c                           ier = 132, vector x is not ordered so that  
c                             x(1) .lt. x(2) ... .lt. x(nx).            
c                           ier = 133, vector y is not ordered so that  
c                             y(1) .lt. y(2) ... .lt. y(ny).            
c                           ier = 134, lx .lt. 1 or lx .ge. nx.         
c                           ier = 135, ly .lt. 1 or ly .ge. ny.         
c                                                                       
c   precision/hardware  - single and double/h32                         
c                       - single/h36,h48,h60                            
c                                                                       
c   reqd. imsl routines - ibcieu,icsevu,icsccu,uerset,uertst,ugetio     
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
      subroutine ibcicu (f,ifd,x,nx,y,ny,lx,ly,c,wk,ier)                
c                                  specifications for arguments         
      integer            ifd,nx,ny,lx,ly,ier                            
      real               f(ifd,ny),x(nx),y(ny),c(16),wk(1)              
c                                  specifications for local variables   
      integer            iwk,jer,l,lm1,i,ipl,j,lc,lw,k                  
      real               a(4,4),dx,dxx,dy,dyy,one,p25,p75,              
     1                   three,xl(4),yl(4),zero                         
      data               zero/0.0/,p25/0.25/,p75/0.75/,                 
     1                   one/1.0/,three/3.0/,                           
     2                   a/3.0,-19.0,32.0,-16.0,                        
     3                     0.0,24.0,-56.0,32.0,                         
     4                     0.0,-8.0,40.0,-32.0,                         
     5                     0.0,3.0,-16.0,16.0/                          
c                                  first executable statement           
      ier = 0                                                           
c                                  check nx and ny                      
      if (nx .lt. 2) go to 40                                           
      if (ny .lt. 2) go to 45                                           
c                                  check lx and ly                      
      if (lx .lt. 1 .or. lx .ge. nx) go to 50                           
      if (ly .lt. 1 .or. ly .ge. ny) go to 55                           
c                                  compute length of x and y intervals, 
c                                    divide each into four equal parts  
c                                    and assign these values to xl and  
c                                    yl respectively                    
      dx = x(lx+1)-x(lx)                                                
      dy = y(ly+1)-y(ly)                                                
      xl(1) = x(lx)                                                     
      xl(2) = x(lx)+p25*dx                                              
      xl(3) = x(lx)+p75*dx                                              
      xl(4) = x(lx+1)                                                   
      yl(1) = y(ly)                                                     
      yl(2) = y(ly)+p25*dy                                              
      yl(3) = y(ly)+p75*dy                                              
      yl(4) = y(ly+1)                                                   
c                                  interpolate placing the spline values
c                                    in wk(1),...,wk(iwk-1). iwk is     
c                                    computed in order to allow         
c                                    sufficient length of wk as it is   
c                                    also used as working storage by    
c                                    ibcieu.                            
      iwk = 4*max0(4,ny)+1                                              
      call ibcieu (f,ifd,x,nx,y,ny,xl,4,yl,4,wk(1),4,wk(iwk),jer)       
c                                  check for terminal error from ibcieu.
c                                    note that a warning error cannot   
c                                    occur because of the way xl and yl 
c                                    are defined.                       
      if (jer .ne. 0) go to 60                                          
c                                  compute the coefficients by solving  
c                                    the 16 by 16 linear system.        
      do 10 l=1,13,4                                                    
         lm1 = l-1                                                      
         do 5 i=1,4                                                     
            ipl = i+lm1                                                 
            c(ipl) = zero                                               
            do 5 j=1,4                                                  
               c(ipl) = c(ipl)+a(i,j)*wk(j+lm1)                         
    5    continue                                                       
   10 continue                                                          
      do 15 l=1,16                                                      
         wk(l) = c(l)/three                                             
         c(l) = zero                                                    
         if (l .le. 4) c(l) = wk(l)                                     
   15 continue                                                          
      lc = 0                                                            
      do 25 i=2,4                                                       
         lc = lc+4                                                      
         lw = -4                                                        
         do 20 j=1,4                                                    
            lw = lw+4                                                   
            do 20 k=1,4                                                 
               c(lc+k) = c(lc+k)+a(i,j)*wk(lw+k)                        
   20    continue                                                       
   25 continue                                                          
c                                                                       
      l = 0                                                             
      dyy = one                                                         
      do 35 j=1,4                                                       
         dxx = one                                                      
         do 30 i=1,4                                                    
            l = l+1                                                     
            c(l) = c(l)*dxx*dyy                                         
            if (l .ge. 5) c(l) = c(l)/three                             
            dxx = dxx/dx                                                
   30    continue                                                       
         dyy = dyy/dy                                                   
   35 continue                                                          
      go to 9005                                                        
   40 ier = 130                                                         
      go to 9000                                                        
   45 ier = 131                                                         
      go to 9000                                                        
   50 ier = 134                                                         
      go to 9000                                                        
   55 ier = 135                                                         
      go to 9000                                                        
   60 ier = jer                                                         
      if (ier .gt. 132) ier = ier-1                                     
 9000 continue                                                          
      call uertst(ier,6hibcicu)                                         
 9005 return                                                            
      end                                                               
