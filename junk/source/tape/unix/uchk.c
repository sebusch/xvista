/*	Check channel number for legal range	*/
/*	Written by Richard Stover		*/
/*	Date:	May 20,1982			*/
uchk(un)
register int un;
{
	extern int silent_;
	if((un<1)||(un>4)) {
		if(silent_ == 0) {
			printf("Channel number out of range 1 to 4\n");
			printf("Requested channel number=%d\n",un);
		}
		return(1);
	}
	return(0);
}

