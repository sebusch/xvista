#define NRTV	800		/* Maximum rows displayed		*/
#define NCTV	800		/* Maximum columns			*/
#define XOFF	352		/* Horizontal screen offset		*/
#define YOFF	  0		/* Vertical offset			*/
#define MAX_IM_HOLD	2	/* Maximum number of images to hold
				   for blinking				*/

struct center {			/* Last center and zoom of display	*/
	int xcn;
	int ycn;
	int zcn;
	int zi;			/* Initial zoom factor			*/
};

struct markpix {		/* Marked pixel				*/
	int prow;
	int pcol;
	float pval;
	char pkey;		/* Key used for marking			*/
};

struct box {
	int nrs;		/* Number of rows stored		*/
	int ncs;		/* Number of columns			*/
	int srs;		/* Start row				*/
	int scs;		/* Start column				*/
	int xlotv;		/* First screen column			*/
	int xhitv;		/* Last screen column			*/
	int ylotv;		/* First screen row			*/
	int yhitv;		/* Last screen row			*/
	int srtv;		/* First image row now displayed	*/
	int sctv;		/* First image column now displayed	*/
	int roworg;		/* Row origin of image array		*/
	int colorg;		/* Column origin of image array		*/
	int ncim;		/* Number of columns in image array	*/
	int bufno;		/* TV buffer number			*/
	char *byteloc;		/* Location of byte image		*/
	float *loctv;		/* Address of image array		*/
};
