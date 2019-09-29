/*
Routines to set up extra VISTA cursor routines.
*/

mark_cur_keys(cval, n, lastx, lasty)
int cval, n, *lastx, *lasty;
{
switch(cval) {
      case 'e' :
      case 'E' :
		 *lastx = *lastx + 1000;
      		 setcursor(n, 0, 0, 0, 0);
		 return(1);
	 	 break;
      case 'i' :
      case 'I' :
		 *lastx = *lastx + 2000;
      		 setcursor(n, 0, 0, 0, 0);
		 return(1);
	 	 break;
      case 'o' :
      case 'O' :
		 *lastx = *lastx + 3000;
      		 setcursor(n, 0, 0, 0, 0);
		 return(1);
	 	 break;
      case 'p' :
      case 'P' :
		 *lastx = *lastx + 4000;
      		 setcursor(n, 0, 0, 0, 0);
		 return(1);
	 	 break;
      case 'r' :
      case 'R' :
	 	 *lastx = *lastx + 5000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'c' :
      case 'C' :
	 	 *lastx = *lastx + 6000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'j' :
      case 'J' :
	 	 *lastx = *lastx + 7000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'm' :
      case 'M' :
	 	 *lastx = *lastx + 8000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'd' :
      case 'D' :
	 	 *lastx = *lastx + 9000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'b' :
      case 'B' :
	 	 *lastx = *lastx + 10000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case '#' :
	 	 *lastx = *lastx + 11000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'x' :
      case 'X' :
	 	 *lastx = *lastx + 12000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'y' :
      case 'Y' :
	 	 *lastx = *lastx + 13000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'u' :
      case 'U' :
	 	 *lastx = *lastx + 14000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'v' :
      case 'V' :
	 	 *lastx = *lastx + 15000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'w' :
      case 'W' :
	 	 *lastx = *lastx + 16000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case 'z' :
      case 'Z' :
	 	 *lastx = *lastx + 17000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      case '!' :
		 *lastx = *lastx + 18000;
		 setcursor(n, 0, 0, 0, 0);
		 return(1);
		 break;
      }
return(0);
}


mark_cur_help()
{
printf("General ITV commands: \n");
printf("'D' Print position and value at cursor\n");
printf("'I' Zoom in\n");
printf("'O' Zoom out\n");
printf("'P' Pan\n");
printf("'R' Restore\n");
printf("'B' Blink (if BLINK display was used)\n");
printf("'X' X plot\n");
printf("'Y' Y plot\n");
printf("'U' Draw a circle\n");
printf("'V' Draw a box\n");
printf("'Z' Save to a file\n");
printf("MARKSTAR commands: \n");
printf("'C' Mark star with centroided position\n");
printf("'J' Mark star with position of cursor (integer)\n");
printf("'#' Give star number of star nearest cursor\n");
printf("'!' Delete star (when saved with SAVE COO) nearest cursor\n");
return;
}

setupmarkcur()
{
extern int (*cur_morekeys)(), (*cur_morehelp)();

cur_morekeys = mark_cur_keys;
cur_morehelp = mark_cur_help;
}
