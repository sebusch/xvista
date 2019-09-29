#include "Vista.h"
/*
 *							compress_file_generic.c
 *							August 20, 1993
 */

#define		HIWRITE		0
#define		HI		0
#define		HICT		0
#define		HIBUFCT		0

#include <stdio.h>

#include "im_compress.h"

#if HIBUFCT
static int buf_ct = 0;
#endif

#ifndef BASE_TYPE
#define BASE_TYPE DATA_TYPE
#endif
#if DATA_TYPE == 1
typedef char data_t;
#endif
#if DATA_TYPE == 2
typedef short data_t;
#endif
#if DATA_TYPE == 4
typedef int data_t;
#endif

extern struct compress_s compress_data;

int
#if DATA_TYPE == 1
#if BASE_TYPE == 1
compress_file_char( fd, len_image, image)
#else
#if BASE_TYPE == 2
compress_file_char_short_base( fd, len_image, image)
#endif
#endif
#endif

#if (DATA_TYPE == 2 && BASE_TYPE == 2 )
ATUS(compressfileshort)( fd, len_image, image)
#endif

#if DATA_TYPE == 4
#if BASE_TYPE == 4
compress_file_int( fd, len_image, image)
#else
#if BASE_TYPE == 2
ATUS(compressfileintunshortbase)( fd, len_image, image)
#endif
#endif
#endif
int fd; /* currently positioned for writing image */
int len_image; /*  number of "data_t"s in image	*/
data_t *image;
{
	int ret = 0, ret2, at_end, zero = 0, no_write_ct = 0;
	data_t *p_in;
	char *p_out;
	int len, left_over, ret_len, len_used_in, len_used_out;
	int len_out, len_out_so_far, len_in_so_far, len_request, len_req;
	int version_number;
	int magic_number;
extern int compression_to_end;
extern int im_write_init;
extern int im_write_end;
extern int im_write_whole_end_block;
extern int im_write_block_size;
#if HICT
extern int ct_0, ct_1, ct_2, ct_4, ct_8, ct_16, ct_32;
#endif


	im_write_init = 1;
	im_write_end = 0;
	im_write_whole_end_block = 1;
	im_write_block_size = compress_data.block_size;
	version_number = compress_data.version_number;
	magic_number = compress_data.magic_number;
	if( compress_data.reset ) {
		compress_data.left_over = 0;
	}
	if( compress_data.buffer_size < 4*(257*sizeof(int)+8)
			+ compress_data.block_size ) {
		ret = -75658;
		goto retu;
	}
	if( compress_data.block_size * 3 > compress_data.buffer_size ) {
		ret = -783894;
		goto retu;
	}
	if( compress_data.buffer_alloc_size < compress_data.buffer_size ) {
		if( compress_data.buffer ) {
			free( compress_data.buffer );
			compress_data.buffer = NULL;
			compress_data.buffer_alloc_size = 0;
		}
		compress_data.buffer = (char *)malloc( compress_data.buffer_size );
		if( compress_data.buffer == NULL ) {
			if( compress_data.verbose ) {
				fprintf( stderr, 
				  "No space for %d in compress_file_xxxxx()\n", 
					compress_data.buffer_size );
			}
			ret = -58734;
			goto retu;
		}
		compress_data.buffer_alloc_size = compress_data.buffer_size;
	}
	at_end = 0;
	ret2 = 0;
	p_in = image;
	left_over = compress_data.left_over;
	p_out = compress_data.buffer + left_over;
	len_out = compress_data.buffer_alloc_size 
			- left_over - 2*sizeof(int);
	len_used_out = 0;
	if( compress_data.write_begin_magic_num ) {
		memcpy( p_out, &version_number, 4 );
		len_used_out += 4;
		left_over += 4;
		p_out += 4;
		memcpy( p_out, &(compress_data.begin_magic_number), 4 );
		len_used_out += 4;
		left_over += 4;
		p_out += 4;
	}
	len_in_so_far = 0;
	len_out_so_far = len_used_out;
	compress_data.len_in_used = 0;
	compress_data.len_out_used = 0;
	compression_to_end = 0;
	do {
		len = len_image - len_in_so_far;
		p_out = compress_data.buffer + left_over;
		len_out = compress_data.buffer_alloc_size 
			- left_over - 2*sizeof(int);
		len_used_in = len_used_out = 0;
#if HI
		fprintf( stderr, "len=%d, len_out=%d\n", len, len_out);
#endif
		if( len > 0 ) {
			if( len * 5 < len_out || len < 32 * 256 ) {
				compression_to_end = 1;
			}
#if DATA_TYPE == 1
#if BASE_TYPE == 1
			ret2 = compression_char( 
#else
#if BASE_TYPE == 2
			ret2 = compression_char_short_base( 
#endif
#endif
#endif
#if (DATA_TYPE == 2 && BASE_TYPE == 2)
			ret2 = compression_short( 
#endif
#if DATA_TYPE == 4
#if BASE_TYPE == 4
			ret2 = compression_int( 
#else
#if BASE_TYPE == 2
			ret2 = compression_int_unshort_base( 
#endif
#endif
#endif
				len, p_in, &len_used_in,
				len_out, p_out, &len_used_out );
			if( ret2 < 0 ) {
				if( ret2 == -2 ) {
					if( compression_to_end == 0 ) {
						compression_to_end = 1;
					} else {
						ret = ret2;
						goto retu;
					}
				} else {
					if( ret2 == -3 || ret2 == -4 ) {
					} else {
						ret = ret2;
						goto retu;
					}
				}
			} else {
				at_end = 1;
			}
#if HI
  			fprintf( stderr, "len_used_in=%d, len_used_out=%d\n", 
				len_used_in, len_used_out);
#endif
			p_out += len_used_out;
		} else {
			at_end = 1;
		}
		if( at_end ) {
			if( compress_data.write_end_marker ) {
				memcpy( p_out, &zero, 4 );
				len_used_out += 4;
				p_out += 4;
			}
			if( compress_data.write_magic_num ) {
				memcpy( p_out, &version_number, 4 );
				len_used_out += 4;
				p_out += 4;
				memcpy( p_out, &magic_number, 4 );
				len_used_out += 4;
				p_out += 4;
			}
		}
		if( at_end && compress_data.write_to_end) {
		    len_req = len_request = len_used_out + left_over;
		    if( compress_data.round_to_block_boundary && len_request > 0 ) {
			len_req = (((len_request - 1) 
				/compress_data.block_size) + 1) 
				* compress_data.block_size;
		    }
		} else {
			len_req = len_request = (((len_used_out + left_over)
				/compress_data.block_size) 
				* compress_data.block_size);
		}
		if( len_request > 0 ) {
#if HIBUFCT
		    buf_ct++;
#endif
		    ret_len = im_write( fd, compress_data.buffer, len_req );
#if HIWRITE
		    fprintf( stderr, "%d=im_write(%d,,%d) len_used_out=%d,left_over=%d\n",
			ret_len, fd, len_req, len_used_out, left_over);
#endif
		    if( ret_len != len_req ) {
			if( compress_data.verbose ) {
				fprintf( stderr,
				"Bad return %d on write in compress_file_xxxx()!\n", 
					ret_len );
			}
			ret = -7435687;
			goto retu;
		    }
		    no_write_ct = 0;
		} else {
			if( len_used_out > 0 || left_over > 0 ) {
				no_write_ct++;
				if( no_write_ct * 4 > compress_data.block_size ) {
					if( compress_data.verbose ) {
					fprintf( stderr,
				"len_used_out=%d left_over=%d len=%d compress_file_xxxx()!\n", 
					len_used_out, left_over, len );
					}
					ret = -787659;
					goto retu;
				}
			}
		}
		p_in += len_used_in;
		len_in_so_far += len_used_in;
		len_out_so_far += len_request;
                left_over = len_used_out + left_over - len_request;
		if( left_over > 0 && len_used_out > 0 ) {
#if HI
  			fprintf( stderr, "left_over=%d, len_request=%d\n", 
				left_over, len_request);
#endif
			memcpy(compress_data.buffer, 
				compress_data.buffer+len_request, left_over );
		} else {
			if( left_over < 0 ) {
				ret = -984564;
				goto retu;
			}
		}
	} while ( at_end == 0 );

retu:
	if( ret == 0 ) {
	    im_write_end = 1;
	    ret = im_write( fd, compress_data.buffer, 0 );
	    im_write_end = 0;
	}
#if HIBUFCT
	fprintf( stderr, "buf_ct=%d\n", buf_ct);
#endif
#if HICT
	fprintf( stderr, "ct_0=%d, ct_1=%d,ct_2=%d,ct_4=%d,ct_8=%d,ct_16=%d,ct_32=%d\n",
		ct_0, ct_1, ct_2, ct_4, ct_8, ct_16, ct_32 );
#endif
	compress_data.len_in_used = len_in_so_far;
	compress_data.len_out_used = len_out_so_far;
	return( ret );
}

