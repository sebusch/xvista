$!      COPYRIGHT       COPYRIGHT       COPYRIGHT       COPYRIGHT
$!  This VMS DCL procedure is Copyrighted software.
$!  The file COPYRIGHT must accompany this file.  See it for details.
$!      COPYRIGHT       COPYRIGHT       COPYRIGHT       COPYRIGHT
$!
$ set verify
$!
$!      Make the Versatec rasterizer
$ fortran       vraster, versacc, flagpage
$ macro         accdef
$ fortran       [-.sysdep.vaxvms]vtsub
$ link          vraster, flagpage, versacc, accdef, vtsub
$ rename        vraster.exe      mongodir:raster.exe
$!
$!      Make the Printronix rasterizer
$ fortran       praster
$ link          praster
$ rename        praster.exe     mongodir:*
$!
$!      Make the Imagen rasterizer
$!fortran       iraster1
$!link          iraster1
$!rename        iraster1.exe    mongodir:*
$ fortran       iraster2
$ link          iraster2
$ rename        iraster2.exe    mongodir:*
$!
$!      Clean up mess
$ delete        *.obj;*
$ set noverify
$ exit
