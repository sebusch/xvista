	real*8 in, out
        common /imtvnum/ in
        in=1
	call casinh(in,out)
	print *, in, out

	stop
	end
