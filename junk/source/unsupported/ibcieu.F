c   imsl routine name   - ibcieu                                        
c                                                                       
c-----------------------------------------------------------------------
c                                                                       
c   computer            - vax/single                                    
c                                                                       
c   latest revision     - june 1, 1980                                  
c                                                                       
c   purpose             - bicubic spline two-dimensional interpolator   
c                                                                       
c   usage               - call ibcieu (f,ifd,x,nx,y,ny,xl,nxl,yl,nyl,   
c                           fl,ifld,wk,ier)                             
c                                                                       
c   arguments    f      - nx by ny matrix containing the function       
c                           values. (input) f(i,j) is the function value
c                           at the point (x(i),y(j)) for i=1,...,nx and 
c                           j=1,...,ny.                                 
c                ifd    - row dimension of the matrix f exactly as      
c                           specified in the dimension statement        
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
c                xl     - vector of length nxl. (input)                 
c                nxl    - number of elements in xl. (input)             
c                yl     - vector of length nyl. (input)                 
c                nyl    - number of elements in yl. (input)             
c                         note - the coordinate pairs (xl(i),yl(j)),    
c                           for i=1,...,nxl and j=1,...,nyl, give the   
c                           points at which the interpolatory bicubic   
c                           spline is to be evaluated.                  
c                fl     - nxl by nyl matrix containing the interpolatory
c                           bicubic spline values. (output) fl(i,j) is  
c                           set to the value of the interpolatory       
c                           bicubic spline at (xl(i),yl(j)) for         
c                           i=1,...,nxl and j=1,...,nyl. note that the  
c                           number of columns in fl must be .ge.        
c                           max(nyl,ny) since fl is also used as        
c                           working storage (of size nxl by ny) during  
c                           the computation.                            
c                ifld   - row dimension of the matrix fl exactly as     
c                           specified in the dimension statement        
c                           in the calling program. (input)             
c                wk     - work vector of length                         
c                           max((nx-1)*3,(ny-1)*3+ny).                  
c                ier    - error parameter. (output)                     
c                         terminal error                                
c                           ier = 129, ifd is less than nx.             
c                           ier = 130, ifld is less than nxl.           
c                           ier = 131, nx is less than 2.               
c                           ier = 132, ny is less than 2.               
c                           ier = 133, vector x is not ordered so that  
c                             x(1) .lt. x(2) ... .lt. x(nx).            
c                           ier = 134, vector y is not ordered so that  
c                             y(1) .lt. y(2) ... .lt. y(ny).            
c                         warning error                                 
c                           ier = 37, xl(i) is less than x(1) or        
c                             greater than x(nx).                       
c                           ier = 38, yl(i) is less than y(1) or        
c                             greater than y(ny).                       
c                                                                       
c   precision/hardware  - single and double/h32                         
c                       - single/h36,h48,h60                            
c                                                                       
c   reqd. imsl routines - icsevu,icsccu,uerset,uertst,ugetio            
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
      subroutine ibcieu (f,ifd,x,nx,y,ny,xl,nxl,yl,nyl,fl,ifld,wk,ier)  
c                                  specifications for arguments         
      integer            ifd,nx,ny,nxl,nyl,ifld,ier                     
      real               f(ifd,ny),x(nx),y(ny),xl(nxl),yl(nyl),         
     1                   fl(ifld,1),wk(1)                               
c                                  specifications for local variables   
      integer            ker,ler,mer,nxm1,nym1,iy,jer,kyl,kylp1,ixl,    
     1                   iyl,level,levold                               
c                                  first executable statement           
      ker = 0                                                           
      ler = 0                                                           
c                                  check ifd .ge. nx                    
      if (ifd .lt. nx) go to 30                                         
c                                  check ifld .ge. nx.                  
      if (ifld .lt. nxl) go to 35                                       
c                                  check nx .ge. 2                      
      if (nx .lt. 2) go to 36                                           
c                                  check ny .ge. 2                      
      if (ny .lt. 2) go to 37                                           
c                                  call uerset to silence warning       
c                                    messages from icsevu               
      level = 2                                                         
      call uerset (level,levold)                                        
      mer = 0                                                           
      nxm1 = nx-1                                                       
      nym1 = ny-1                                                       
c                                  interpolate in the x-direction       
      do 10 iy=1,ny                                                     
c                                  calculate the coefficients           
         call icsccu (x,f(1,iy),nx,wk(1),nxm1,jer)                      
c                                  check for error in icsccu            
         if (jer .ne. 0) go to 40                                       
c                                  evaluate                             
         call icsevu (x,f(1,iy),nx,wk(1),nxm1,xl,fl(1,iy),nxl,jer)      
c                                  check for error in icsevu            
         if (jer .ne. 0) ker = 37                                       
   10 continue                                                          
      kyl = nym1*3                                                      
      kylp1 = kyl+1                                                     
c                                  interpolate in the y-direction       
      do 25 ixl=1,nxl                                                   
         do 15 iy=1,ny                                                  
            wk(kyl+iy) = fl(ixl,iy)                                     
   15    continue                                                       
c                                  calculate the coefficients           
         call icsccu (y,wk(kylp1),ny,wk(1),nym1,jer)                    
c                                  check for error in icsccu            
         if (jer .ne. 0) go to 45                                       
c                                  evaluate                             
         do 20 iyl=1,nyl                                                
            call icsevu (y,wk(kylp1),ny,wk(1),nym1,yl(iyl),fl(ixl,iyl), 
     1                   1,jer)                                         
c                                  check for error in icsevu            
            if (jer .ne. 0) ler = 38                                    
   20    continue                                                       
   25 continue                                                          
      go to 46                                                          
c                                  handle errors                        
   30 mer = 129                                                         
      go to 50                                                          
   35 mer = 130                                                         
      go to 50                                                          
   36 mer = 131                                                         
      go to 50                                                          
   37 mer = 132                                                         
      go to 50                                                          
   40 mer = 133                                                         
      go to 46                                                          
   45 mer = 134                                                         
   46 call uerset (levold,level)                                        
   50 ier = max0(mer,ker,ler)                                           
 9000 continue                                                          
      if (mer .ne. 0) call uertst(mer,6hibcieu)                         
      if (ker .ne. 0) call uertst(ker,6hibcieu)                         
      if (ler .ne. 0) call uertst(ler,6hibcieu)                         
 9005 return                                                            
      end                                                               
