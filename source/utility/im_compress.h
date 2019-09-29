/*
 *							im_compress.h
 *							August 3, 1993
 */

#ifndef IM_COMPRESS_H
#define IM_COMPRESS_H 1

#define		COMP_VERSION_NUM 110
#define		COMP_BEGIN_MAGIC_NUM	7970602
#define		COMP_MAGIC_NUM	3930786

#define		EXPAND_BUFFER_SIZE 16 * 1024
#define		EXPAND_BLOCK_SIZE	512
#define		COMPRESS_BUFFER_SIZE 16 * 1024
#define		COMPRESS_BLOCK_SIZE	512

#define		HIBYTEHI	0
#define		HIIEEEHI	0

#ifdef big_endian
#undef		HIBYTEHI
#undef		HIIEEEHI
#define		HIBYTEHI	1
#define		HIIEEEHI	1
#endif

#ifdef SUN
#undef		HIBYTEHI
#undef		HIIEEEHI
#define		HIBYTEHI	1
#define		HIIEEEHI	1
#endif

#ifdef sun
#undef		HIBYTEHI
#undef		HIIEEEHI
#define		HIBYTEHI	1
#define		HIIEEEHI	1
#endif

#define		S0		0
#define		S1		1
#define		S2		2
#define		S4		4
#define		S8		8
#define		S16		16
#define		S32		32

#define		M1	0x00000001
#define		M2	0x00000003
#define		M4	0x0000000F
#define		M8	0x000000FF
#define		M16	0x0000FFFF

#define		CT1	32
#define		CT2	16
#define		CT4	8
#define		CT8	4
#define		CT16	2
#define		CT32	1

struct expand_s {

/*  the following list of variables can be reset */

int verbose;

int expand_to_end;
int look_for_end_marker;
int begin_magic_number_check;	/* version and magic number check */
int magic_number_check;		/* version and magic number check */
int reset;    			/* assume a new file each time */
int block_size;
int buffer_size;
int version_number;
int begin_magic_number;
int magic_number;

/*  end list of variables */

int eof;
int hit_end_marker;
int left_over;
int len_returned;
int magic_number_ck_and_ok;
int buffer_alloc_size;
char *buffer;

};


struct compress_s {

/*  the following list of variables can be reset */

int verbose;

int compress_to_end;
int write_end_marker;
int write_begin_magic_num;	/* write version and magic number */
int write_magic_num;		/* write version and magic number */
int reset;    			/* assume a new file each time */
int write_to_end;
int block_size;
int buffer_size;
int round_to_block_boundary;    /* Make the output compressed data 
				 * an even number of blocks */
int version_number;
int begin_magic_number;
int magic_number;

/*  end list of variables */

int left_over;
int buffer_alloc_size;
char *buffer;
int len_in_used;
int len_out_used;

};

#endif

