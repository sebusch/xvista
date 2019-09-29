/*       ***     VISTA KEYWORDS, PARAMETERS, AND CONTROL         ***    */
/*       This include file controls communication between VISTA and	*/
/*       its subroutines.							*/

/*       Parameters:     NCON    The maximum length of the VISTA  	*/
/*                               integer, constant, and keyword buffers.*/
#define NCON 15

/*       These variables pass constants and keywords to the subroutines.*/

/*       Variable:       FCONST           Floating constants buffer	*/
/*                       IBUF            Integer constants buffer	*/
/*                       WORD            Keywords and alphanumeric buffer */
/*                       COM             Current VISTA command		*/
/*                       COMMAND         Full input command line	*/
/*                       CURRENTLINE    Remaining commands to process before */
/*                                       seeking new command 		*/
extern struct {
	float fconst[NCON];
	int ibuf[NCON];
} viscon_;

extern struct {
	char command[500];
	char com[80];
	char word[NCON][80];
	char currentline[500];
	char paramlist[500];
} vcommand_;

/*       Number of integer, floating point, and alphabetic parameters found */
/*       on the command line:             				*/
extern struct {
	int nints;
	int nflts;
	int nword;
} vpcounts_;

/*       These logical variables control subroutine program flow.	*/

/*       Variable:       GO      Set by VISTA to start the subroutine	*/
/*                       XERR    Set .TRUE. in subroutines when an error*/
/*                               occurs during execution.  If the error	*/
/*                               is severe enough, the routine should	*/
/*                               imediately return to VISTA.		*/
/*                       NOGO    Set .TRUE. when Ctrl-C is entered from	*/
/*                               the keyboard.  Subroutines can check	*/
/*                               this at any desired point, and return	*/
/*                               if set.				*/
/*                       PROC    Set .TRUE. if the command is being	*/
/*                               run from a procedure.			*/
/*                       XEOF    Set .TRUE. when an EOF occurs on an external*/
/*                               data file read.			*/
extern struct {
	int go, xerr, nogo, proc, xeof;
} con_;

/*       Finally, we have a common which carries around the number of the  */
/*       logical unit opened for writing. We need to do this to get things */
/*       to work properly in batch mode on the Lick SUNs. Holtz. 3/89	   */

extern struct {
	int olu;
} outputunit_;
