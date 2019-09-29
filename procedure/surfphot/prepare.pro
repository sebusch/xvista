!--------------------  PROCEDURE PREPARE  -----------------------------!
!  Procedure to prepare galaxy images for surface photometry. Image    !
!  names must be contained in a file (e.g. galaxies.lis) in the data   !
!  directory (/scratch/ra/jb007/). Recall that a "c" must be typed to  !
!  restart the procedure after every PAUSE.                            !
!                                                                      !
!----------------------------------------------------------------------!
parameter im 
!----------------------------------------------------------------------!
! INPUT:  im  ... observation number of image to process  [gal{im}]    !
!----------------------------------------------------------------------!
psf=1.5            ! dummy variable for the prepfile
string skylog '/u/jb007/vista/data/sky.log'
string file 'gal%i3.3' im
!---- Read the image and identify sky level  --------------------------!
 oprep w {file}
 rd 5 {file}
 string gal {?5:object}
 string date {?5:date-obs}
 box 10 sc=35 sr=25 nc=240 nr=50
 mn 5 box=10 silent; y=m5^0.5 x=4*y xmn=m5-x x=8*y xmx=m5+x
 histog 5 box=10 xmin=xmn xmax=xmx bin=1 nolog buf=41
 abx 41 high=high silent; clip 41 max=high/11 vmax=0.
 box 6 sr=0 nr=1 sc=sc[41] nc=m5-sc[41]-5
 box 7 sr=0 nr=1 sc=m5+5 nc=150 
 abx 41 box=6 high_col=zz silent; abx 41 box=7 high_col=mm silent
 tv 5 z=zz l=mm-zz cf=wrbb clip
!----  Estimate sky level in 5 boxes  ----------------------------------!
 printf 'Now make 5 sky boxes ...'
 do j=1,5
    printf '\n -- BOX # %i1 --' j
    box $j int
 end_do
 printf 'now calculating SKY ...'
 back:
 do j=1,5 
    sky 5 box=j silent
    string sky 'sky%i1' j; string sig 'sig%i1' j
    {sky}=sky {sig}=skysig
 end_do
 sky=(sky1+sky2+sky3+sky4+sky5)/5 skysig=(sig1+sig2+sig3+sig4+sig5)/5
 skydev=sqrt[((sky1^2+sky2^2+sky3^2+sky4^2+sky5^2)/5)-sky^2]
 percent=skydev*100/sky
 printf '\n Level of the SKY is: %f7.2 +/- %f5.2' sky skysig
 printf ' The standard dev. is %f5.2 or %f5.3 percent of sky' skydev percent
 printf '\n Sky1 = %f7.2 +/- %f5.2 ' sky1 sig1
 printf ' Sky2 = %f7.2 +/- %f5.2 ' sky2 sig2
 printf ' Sky3 = %f7.2 +/- %f5.2 ' sky3 sig3
 printf ' Sky4 = %f7.2 +/- %f5.2 ' sky4 sig4
 printf ' Sky5 = %f7.2 +/- %f5.2 ' sky5 sig5
 ask ' Are you satisfied with those values ? (yes=1) ' ans
 if ans~=1
   printf '\n Make a new box int to recalculate a bad sky ...'
   pause ' Type "C" to continue'
   goto back
 end_if
!----  Determine center and extent of galaxy  --------------------------------!
 printf '\n Now displaying galaxy ...'
 printf '\n      '
 tv 5 z=zz-300 l=2500 cf=wrbb clip
 printf '\n Now make a box near the galaxy center, avoiding bright objects'
 printf '\n    '
 box 11 int
 abx 5 11 high_row=hr high_col=hc high=peak silent
 box 12 nr=21 nc=21 cr=hr cc=hc 
 tvbox box=12
 axes 5 box=12 
 if peak>25000
   bell r; printf 'Warning! Central peak going non-linear ...'
   bell r; pause ' Pause to think. Type "C" to continue.'
 end_if   
 printf '\n pause to tvzap the image. Type "E" on tv image to return'
 printf '\n  '
 tvzap sig=2.5 search=5
 ask 'Do you want to save this cleaner image to disk? (yes=1) =>' ans
 if ans==1
   wd 5 {file}
 end_if
 printf '\n Now AEDIT image'
 printf ' Do a good job cause you're only given one chance  ... '
 aedit 5 int {file} circ
 tv 5 z=zz l=mm-zz cf=wrbb clip
 printf '\n Now mark maximum radius for photometry'
 printf '\n    '
 rmark
 wprep
!----  Store information in sky.log   -----------------------------------------!
 printf '{gal}  {file} {date} %f8.2 %f6.2 %f5.2 %f8.3' sky skysig skydev |
   percent >>{skylog}
 clprep
printf '\n Prep. file for {file} - {gal} - closed ...'
printf '\n Procedure PREPARE completed.'
end
