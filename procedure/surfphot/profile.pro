!----------------------  PROCEDURE PROFILE  -----------------------------!
!        PROCEDURE TO PERFORM PROFILE PHOTOMETRY ON GALAXIES             !
!------------------------------------------------------------------------!
parameter im 
!------------------------------------------------------------------------!
!        For interactive mode, set inter=1                               !
!                                                                        !
!        Uses buffers 5,10,49,50 and boxes 17,18,20                      !
!------------------------------------------------------------------------!
inter=0 pa=0.0 eps=0.0 alpha=0.0 a=0.0
string fwhmlog '/u/jb007/vista/data/fwhm.log'
string file 'gal%i3.3' im
 oprep r {file}
 rd 5 {file}
 string gal {?5:object}
 string date {?5:date-obs}
 rprep
 call psf 5 axr axc 
 aedit 5 old {file} circ
 if inter==1
    mn 5 pix=4 silent; tv 5 z=2*m5/3 cf=wrbb clip
 end_if
 PROFILE 10 5 ITER=(7,0) SCALE=0.55 RMAX GPROF
 save prof={file}                          ! creates {file}.prf
 print profile >/scratch/ra/jb007/{file}.prof
 if inter==1
    tvprof n1=10 space=5                  ! n1 = starting annulus
 end_if
 string obj '{?5:object}
 fits 10 char=BUNIT 'Log [Counts]'
 ch 10 'Radial Surface Brightness for {obj}'
 abx 10 high=high low=low silent
 pmax=max[10000,int[(high+500)/1000]*1000] pmin=ifix[low/1000]*1000.
 if inter==1
   plot 10 min=pmin max=pmax semilog
 end_if
!----  Store information in fwhm.log   ---------------------------------------!
 printf '{gal}  {file} {date} %f5.2 %f5.2 %i2' fwhm fwhmsig count >>{fwhmlog}
!----------------------------------------------------------------------------!
printf 'PROFILE proceeded normally. Go to EMAG now'
end
 
 
!--------------  Procedure PSF  (called in PROFILE)  --------------------!
!  Procedure to estimate the seeing from a number of stars in a given    !
!  frame. The stars are identified with AUTOMARK and to measure their    !
!  profile accurately, the background around each star if first removed  !
!  by subtracting a surface around the stars. Each star is then fit with !
!  a 2D gaussian using PSSFIT. An estimate of the typical seeing quality !
!  of the image is made from the gaussian sigmas of moderately bright    !
!  stars and converting to a seeing fwhm.                                !
!  The default value for the plate scale is 0.55 arcsecond per pixel.    !
!------------------------------------------------------------------------!
parameter buf axr axc 
scale=.55 
copy 49 $buf
box 20 sc=sc[buf]+60 nc=nc[buf]-119 sr=sr[buf]+60 nr=nr[buf]-119
box 19 cc=axc cr=axr nc=7 nr=7
mask box=19
automark $buf box=20 radius=5 range=4500,20000 reject=9 new
save coo=stars sr=sr[buf] sc=sc[buf]
open MARK /scratch/ra/jb007/stars.coo
skip MARK 1,3
stat numb=count[mark]
unmask
!----  Fit the PSFs on background subtracted stars  ---------------------!
do k=1,numb
   read MARK
   xc=@MARK.2 yc=@MARK.3
   box 17 cc=xc cr=yc nc=11 nr=11
   mask box=17
   box 17 cc=xc cr=yc nc=21 nr=21
   sub 49 49 box=17
   add 49 $buf box=17
   surface 49 box=17 plane mask sub silent
   unmask
   clip 49 min=0 vmin=0.0 box=17
end_do
$/bin/rm /scratch/ra/jb007/stars.psf
psffit 49 radius=3 >/scratch/ra/jb007/stars.psf
printf '\n Now computing FWHM '
open PSF /scratch/ra/jb007/stars.psf
sigma=sig2=count=0.
do j=1,numb
   read PSF
   if ((@PSF.5/@PSF.6)>1.5)|((@PSF.6/@PSF.5)>1.5)  ! prevents fat objects from
      goto skip                                      ! being counted
   end_if
   if (@PSF.2>3000)&(@PSF.2<17000)
    count=count+1 sigma=sigma+(@PSF.5+@PSF.6)/2 sig2=sig2+((@PSF.5+@PSF.6)/2)^2
   end_if
   skip:
end_do
fwhm=sigma*2.3548*scale/count
fwhmsig=sig2*2.3548*scale/count fwhmsig=sqrt[(fwhmsig-fwhm^2)/(count-1)]
close PSF
close MARK
!---- Show the results  ---------------------------------------------!
printf '\n Value for FWHM (") is: %f4.2 +/- %f5.2' fwhm fwhmsig
printf '\n evaluated from %i2 star measurements.' count
end
