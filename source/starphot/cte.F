#ifdef INTER
1	print *, 'enter x, y, cnts, bg, date: '
	read *, x, y, cnts, bg, date

	print *, cte(x,y,cnts,bg,date)

	goto 1
	stop
	end	
#endif
	
	FUNCTION OLDCTE(X,Y,CNTS,BG,DATE)
	IMPLICIT REAL (A-Z)

	DATA Y0 /0.018/
	DATA Y1 /0.097/
	DATA Y2 /0.041/
	DATA Y3 /0.088/
	DATA Y4 /0.507/
	DATA Y5 /0.035/
	DATA Y6 /0.042/
	DATA X1 /0.024/
	DATA X2 /0.002/
	DATA X4 /0.196/
	DATA X5 /0.126/

	if (X .lt. 10 .or. x .gt. 800 .or. y .lt. 10 .or. y .gt. 800
     &     .or. date .lt. 1993 .or. date .gt. 2001 .or. cnts .le. 0.) then

C	  print *, x, y, cnts, bg, date
C	  pause
	end if
	YR=DATE-1996.3
	LCT=LOG(MAX(CNTS,1.))-7
	LBG=LOG(MAX(BG,1.))-1
	
	YCTE=Y/800*(Y0+(Y1+Y2*YR)*(Y3+EXP(-Y4*LCT))*EXP(-Y5*LBG-Y6*BG))
	XCTE=X/800*((X1+X2*YR)*EXP(-X4*LCT-X5*LBG))

	CTE=XCTE+YCTE

	CTE=10.**(-0.4*CTE)
	if (cte .gt. 1) write(29,*) x, y, cnts, bg, date, xcte, ycte, cte
C	if (cte .gt. 1 .or. cte .lt. 0.1) then

C	  print *, x, y, cnts, bg, date, 1
C	  print *, xcte, ycte, cte
C	  pause
C	end if
	if (X .lt. 10 .or. x .gt. 800 .or. y .lt. 10 .or. y .gt. 800
     &     .or. date .lt. 1993 .or. date .gt. 2001) then

C	  print *, x, y, cnts, bg, date, 2
C	  print *, xcte, ycte, cte
C	  pause
	end if

	RETURN
	END

	FUNCTION CTE(X,Y,CNTS,BG,DATE)
	IMPLICIT REAL (A-Z)

	DATA Y0 /0.018/
	DATA Y1 /0.097/
	DATA Y2 /0.041/
	DATA Y3 /0.088/
	DATA Y4 /0.507/
	DATA Y5 /0.035/
	DATA Y6 /0.042/
	DATA X1 /0.0194/
	DATA X2 /-0.00085/
	DATA X4 /0.196/
	DATA X5 /0.126/

	if (X .lt. 10 .or. x .gt. 800 .or. y .lt. 10 .or. y .gt. 800
     &     .or. date .lt. 1993 .or. date .gt. 2002 .or. cnts .le. 0.) then

C	  print *, x, y, cnts, bg, date
C	  pause
	end if
	LCT0=LOG(MAX(CNTS,1.))-7
        BG = SQRT(BG**2+1)-10
        LBG = LOG(SQRT(BG**2+1))-1
	YR=DATE-1996.3
      
	XCTE=0.0194*EXP(-0.00085*BG)*X/800

        LCT = LCT0 + 0.921*XCTE
        C1 = 0.0143*(0.729*exp(-0.397*lbg)+
     &         0.271*exp(-0.0144*bg))*(1.+0.267*yr-0.0004*yr*yr)*y/800
        C2 = 2.99*exp(-0.479*lct)
        YCTE = log(exp(c1)*(1+c2)-c2)/0.441

	CTE=XCTE+YCTE

	CTE=10.**(-0.4*CTE)
	if (cte .gt. 1) write(29,*) x, y, cnts, bg, date, xcte, ycte, cte
C	if (cte .gt. 1 .or. cte .lt. 0.1) then

C	  print *, x, y, cnts, bg, date, 1
C	  print *, xcte, ycte, cte
C	  pause
C	end if
	if (X .lt. 10 .or. x .gt. 800 .or. y .lt. 10 .or. y .gt. 800
     &     .or. date .lt. 1993 .or. date .gt. 2002) then

C	  print *, x, y, cnts, bg, date, 2
C	  print *, xcte, ycte, cte
C	  pause
	end if

	RETURN
	END
