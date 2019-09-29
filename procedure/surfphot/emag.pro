!---------------------  PROCEDURE EMAG  ------------------------!
!  Procedure to inspect PROFILE surface photometry results and  !
!  correct them if necessary with CPROF, and perform elliptical !
!  aperture photometry on a set of chosen concentric isophotes  !
!  on the galaxy with EMAG.                                     !
!---------------------------------------------------------------!
parameter im 
string file 'gal%i3.3' im
oprep r {file}
 rd 5 {file}
 printf '\n Displaying image ... '
 rprep
 aedit 5 old {file} circ
 tv 5 z=sky-50 l=450 cf=wrbb clip
 get prof={file}
 tvprof n1=10 space=4
 redo:
 cprof
 ask 'Do you wish to run CPROF again (yes=1)?' ans1
 if ans1==1
   goto redo
 end_if
 tv 5 z=sky-50 l=200 cf=wrbb
 tvprof n1=10 space=4  
 printf '\n Mark radius for best-fitted ellipse'
 emark
 printf ' Doing the photometry now...'
 emag 5 erad rmax append 
 printf '\n Now another display, and CPROF again ...'
 here:
 tv 5 z=sky-50 l=200 cf=wrbb
 tvprof n1=10 space=4
 cprof
 ask 'Do you wish to run CPROF one last time (yes=1)?' ans1
 if ans1==1
   goto here 
 end_if
 down:
 printf ' Now writing results to a {file}_fit.prf ...'
 save prof={file}_fit
 print profile >/scratch/ra/jb007/{file}_fit.prof
 clprof  !! clears the whole common block  (equivalent to "clprof rad=0")
printf 'Procedure EMAG completed!'
end
