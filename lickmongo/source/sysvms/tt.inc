c                       ttchn(0) is the I/O channel connected to STDIN
c                       ttchn(1:MAXSIMDEV) are I/O channels for
c                       graphical output to terminal devices
	integer*4       ttchn(0:MAXSIMDEV)

c                       status block for QIOW
	structure /iostat_block/
	  integer*2     iostat          ! return status
	  integer*2     term_offset     ! location of line terminator
	  integer*2     terminator      ! value of terminator
	  integer*2     term_size       ! size of terminator
	end structure
	record /iostat_block/   iosb

	integer*4       retcod

	logical         isitopen(0:MAXSIMDEV)

	common  /tt/    ttchn, iosb, retcod, isitopen
