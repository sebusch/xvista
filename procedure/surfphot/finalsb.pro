!---------------------  PROCEDURE FINALSB -----------------------!
! Procedure to refine the surface brightness calculation and to  !
! measure with ELLMAG the total elliptical counts inside of each !
! isophotes as determined by SURFBRIGHT.                         !
! Remember that EMAG only measured the elliptical counts inside  !
! a set of concentric isophotes with the same chosen PA and      !
! ellipticity.                                                   !
!----------------------------------------------------------------!
parameter im
string file 'gal%i3.3' im
oprep r {file}
!----  First work on AEDITed image  -----------------------------!
 rd 5 {file}
 rprep
 aedit 5 old {file} circ
 get prof={file}_fit
 surfbright 5 prof
 ellmag 5
 save prof={file}_final
 print profile >/scratch/ra/jb007/{file}_final.prof
!----  Then apply same isophote fitting on image with starlight -!
 rd 6 {file}
 open DATA /scratch/ra/jb007/{file}_final.prof
 stat numb=count[DATA]
 skip DATA 1,numb-1
 read DATA     ! read very last line for chosen pa and ellipticity
 eps=@DATA.4 pa=@DATA.5
 emag 6 center=axr,axc ell=eps pa=pa
 surfbright 6 prof
 ellmag 6
 save prof={file}_stars
 print profile >/scratch/ra/jb007/{file}_stars.prof
printf 'Procedure FINALSB completed!'
end
 
 
 
 
