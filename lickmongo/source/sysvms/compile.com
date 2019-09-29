$!      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
$!      This VMS DCL procedure is Copyright (c) 1988 Steven L. Allen
$!      The file COPYRIGHT must accompany this file.  See it for details.
$!      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
$!
$ set noverify
$!      The purpose of this command file is to pretend to be a Unix make.
$!      That means, it goes happily along compiling things until an error
$!      is encountered.  There it stops.  When the command file is run
$!      again, it only recompiles files which have not yet been compiled
$!      or files which have been changed.
$!      This procedure depends on the assumption that when the object files
$!      are stuffed into a library, they are all stuffed together.  And it
$!      expects that when the object files are stuffed into a library, a
$!      file called library.dat is left in the directory.
$                               ! see that MONGOSRC is defined
$ if f$trnlnm("mongosrc") .nes. "" then goto doit
$       write sys$output "MONGOSRC must be defined before making."
$       exit
$ doit:
$ cop mongosrc:mongo*.inc []
$ file = f$search("library.dat")
$ if    file .eqs. "" then libdate = "0000-00-00 00:00:00.00"
$ if    file .nes. "" then libdate = f$cvtime(f$fileattributes("library.dat","RDT"))
$ compiler = ""
$ switch = "/nolist"
$!=============================================================================
$ anotherfort:
$ file = f$search("*.for",1)
$ if file .eqs. "" then goto fortdone
$ fileroot = f$parse("''file'",,,"NAME")
$ objfile = f$search("''fileroot'"+".OBJ",2)
$ if objfile .eqs. "" then objdate = "0000-00-00 00:00:00.00"
$ if objfile .nes. "" then objdate = f$cvtime(f$fileattributes("''objfile'" ,"RDT"))
$ filedate = f$cvtime(f$fileattributes("''file'","RDT"))
$ newerthanobj = filedate .ges. objdate
$ newerthanlib = filedate .ges. libdate
$ newer = newerthanlib .and. newerthanobj
$ if .not. newer then goto anotherfort
$ if fileroot .eqs. "UIS"      then on severe_error then continue
$ if fileroot .eqs. "HPGL"     then on severe_error then continue
$ if fileroot .eqs. "GRIN270"  then on severe_error then continue
$ if fileroot .eqs. "AEDSTART" then on severe_error then continue
$ write sys$output "fortran ''switch' ''fileroot'"
$ fortran 'switch' 'file'
$ on severe_error then exit 4
$ if $status .eq. "%x00038090" then compiler = "VAX/VMS Fortran compiler"
$ if compiler .nes. "" then goto sorrycharlie
$ switch = "/nolist"
$ goto anotherfort
$ fortdone:
$!=============================================================================
$ anothermac:
$ file = f$search("*.mar",1)
$ if file .eqs. "" then goto macdone
$ fileroot = f$parse("''file'",,,"NAME")
$ objfile = f$search("''fileroot'"+".OBJ",2)
$ if objfile .eqs. "" then objdate = "0000-00-00 00:00:00.00"
$ if objfile .nes. "" then objdate = f$cvtime(f$fileattributes("''objfile'" ,"RDT"))
$ filedate = f$cvtime(f$fileattributes("''file'","RDT"))
$ newerthanobj = filedate .ges. objdate
$ newerthanlib = filedate .ges. libdate
$ newer = newerthanlib .and. newerthanobj
$ if .not. newer then goto anothermac
$ write sys$output "macro   ''fileroot'"
$ macro /nolist  'file'
$ if $status .eq. "%x00038090" then compiler = "VAX/VMS Macro assembler"
$ if compiler .nes. "" then goto sorrycharlie
$ goto anothermac
$ macdone:
$!=============================================================================
$ define/nolog  vaxc$include    decw$include, sys$library
$ define/nolog  X11             decw$include
$ define/nolog  sys             sys$library
$ anotherc:
$ file = f$search("*.c",1)
$ if file .eqs. "" then goto cdone
$ fileroot = f$parse("''file'",,,"NAME")
$ objfile = f$search("''fileroot'"+".OBJ",2)
$ if objfile .eqs. "" then objdate = "0000-00-00 00:00:00.00"
$ if objfile .nes. "" then objdate = f$cvtime(f$fileattributes("''objfile'" ,"RDT"))
$ filedate = f$cvtime(f$fileattributes("''file'","RDT"))
$ newerthanobj = filedate .ges. objdate
$ newerthanlib = filedate .ges. libdate
$ newer = newerthanlib .and. newerthanobj
$ if .not. newer then goto anotherc
$ write sys$output "cc      ''fileroot'"
$ cc /nolist     'file'
$ if $status .eq. "%x00038090" then compiler = "VAX/VMS C compiler"
$ if compiler .nes. "" then goto sorrycharlie
$ goto anotherc
$ cdone:
$!=============================================================================
$ set noverify
$ delete/noconf mongo*.inc;*
$! Make sure the object modules get stuffed into the object library.
$ @mongosrc:library
$! All done, we can go away now.
$ exit
$ sorrycharlie:
$ write sys$output "In order to build Lick Mongo on your VMS system you need the"
$ write sys$output "''compiler'"
$ write sys$output "It is impossible to complete the build without it."
$ exit 4
