#include "Vista.h"
/*
 *							expand_file_generic.c
 *							August 20, 1993
 */

#define		 HICK		0
#define		 HILEFTOVER     0
#define		 HILOOP		0

#include <stdio.h>

#include "im_compress.h"


extern struct expand_s exp_data;
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


#ifdef VAX
#define		READ(FD,P,LEN) vaxread(FD,P,LEN)

static
int
vaxread(fd, p, len)
int fd, len;
char *p;
{
	int ret = 0, ret2, ct;

	if( exp_data.eof ) {
		goto retu;
	}
	ct = len / exp_data.block_size;
	while( ct-- > 0 ) {
		ret2 = read( fd, p, exp_data.block_size );
		if( ret2 >= 0 ) {
			if( ret2 == 0 ) {
				exp_data.eof = 1;
				break;
			}
			ret += ret2;
		} else {
			ret = ret2;
			break;
		}
		p += exp_data.block_size;
	}
retu:
	return( ret );
}
#else
#define		READ(FD,P,LEN) read(FD,P,LEN)
#endif

int
#if DATA_TYPE == 1
#if BASE_TYPE == 1
  expand_file_char(fd,len_image,image)
#else
#if BASE_TYPE == 2
   expand_file_char_short_base(fd,len_image,image)
#endif
#endif
#endif

#if (DATA_TYPE == 2 && BASE_TYPE == 2)
ATUS(expandfileshort)(fd,len_image,image)
/*expandfileshort_(fd,len_image,image)*/
#endif

#if DATA_TYPE == 4
#if BASE_TYPE == 4
  expand_file_int(fd,len_image,image)
#else
#if BASE_TYPE == 2
   ATUS(expandfileintunshortbase)(fd,len_image,image)
#endif
#endif
#endif
int fd, len_image;
data_t *image;
{
	int ret = 0, ret2, first_sw = 1, disp;
	char *p_in;
	data_t *p_out;
	int go, len, left_over, ret_len, len_used_in, len_used_out;
	int len_out, len_out_so_far, len_request, zero_out_ct;
	long version_number;
	char version_num[4];
	long magic_number;
	char magic_num[4];

	if( exp_data.reset ) {
		exp_data.eof = 0;
		exp_data.len_returned = 0;
		exp_data.left_over = 0;
		exp_data.hit_end_marker = 0;
		exp_data.magic_number_ck_and_ok = 0;
	}
	if( exp_data.buffer_size < 2048 + exp_data.block_size ) {
		ret = -75659;
		goto retu;
	}
	if( exp_data.block_size * 3 > exp_data.buffer_size ) {
		ret = -783794;
		goto retu;
	}
	if( exp_data.buffer_alloc_size < exp_data.buffer_size ) {
		if( exp_data.buffer ) {
			free( exp_data.buffer );
			exp_data.buffer = NULL;
			exp_data.buffer_alloc_size = 0;
		}
		exp_data.buffer = (char *)malloc( exp_data.buffer_size );
		if( exp_data.buffer == NULL ) {
			if( exp_data.verbose ) {
				fprintf( stderr, "No space for %d in expand_file_xxxx()\n",
					exp_data.buffer_size);
			}
			ret = -56734;
			goto retu;
		}
		exp_data.buffer_alloc_size = exp_data.buffer_size;
	}
	go = 1;
	zero_out_ct = 0;
	ret2 = 0;
	p_out = image;
	p_in = exp_data.buffer;
	left_over = exp_data.left_over;
	expand_to_end( 0 );
	len_out_so_far = 0;
	while( go ) {
		if( exp_data.eof == 0 ) {
		    len_request = (((exp_data.buffer_size - left_over)
				/exp_data.block_size) *exp_data.block_size);
		    if( len_request > 0 ) {
			ret_len = READ( fd, p_in+left_over, len_request );
			if( ret_len < 0 ) {
				ret = -456897;
				goto retu;
			}
			if( ret_len < len_request ) {
				exp_data.eof = 1;
			}
		    } else {
			ret_len = 0;
		    }
		} else {
		    ret_len = 0;
		}
		len = left_over + ret_len;
		len_out = len_image - len_out_so_far;
		disp = 0;
		if( first_sw ) {
			first_sw = 0;
			if( exp_data.begin_magic_number_check ) {
				if( len < 8 ) {
					ret = -734567;
					goto retu;
				}
				version_number = 0;
				if( len < 4 ) {
					ret = -9346578;
					goto retu;
				}
				version_num[0] = '\0';
				version_num[1] = '\0';
				version_num[2] = '\0';
				version_num[3] = '\0';
				expand_get_int( p_in, version_num );
				memcpy( &version_number, version_num, 4 );
				if( version_number/100 
					!= exp_data.version_number/100 ) {
					if( exp_data.verbose ) {
						fprintf( stderr,
	"  Version number (at start) did not match!!!\n");
					}
					ret = -500;
					goto retu;
				}
#if HICK
				fprintf( stderr, "ck number\n" );
#endif
				magic_number = 0;
				if( len < 4 ) {
					ret = -9346578;
					goto retu;
				}
				magic_num[0] = '\0';
				magic_num[1] = '\0';
				magic_num[2] = '\0';
				magic_num[3] = '\0';
				expand_get_int( p_in, magic_num );
				memcpy( &magic_number, magic_num, 4 );
				if( magic_number == exp_data.begin_magic_number) {
					exp_data.magic_number_ck_and_ok = -1;
#if HICK
					fprintf( stderr, "ck number equal\n" );
#endif
				} else {
					if( exp_data.verbose ) {
						fprintf( stderr,
	"  Magic number (at start) did not match!!!\n");
					}
					ret = -600;
					goto retu;
				}
				disp = 8;
			}
		}
		if( ret2 == 3 ) {
			if( exp_data.magic_number_check ) {
				version_number = 0;
				if( len < 4 ) {
					ret = -9346578;
					goto retu;
				}
				version_num[0] = '\0';
				version_num[1] = '\0';
				version_num[2] = '\0';
				version_num[3] = '\0';
				expand_get_int( p_in, version_num );
				memcpy( &version_number, version_num, 4 );
				if( version_number/100 
					!= exp_data.version_number/100 ) {
					ret = -100;
					if( exp_data.verbose ) {
						fprintf( stderr,
					"  Version number did not match!!!\n");
					}
					goto retu;
				}
#if HICK
				fprintf( stderr, "ck number\n" );
#endif
				magic_number = 0;
				if( len < 4 ) {
					ret = -9346578;
					goto retu;
				}
				magic_num[0] = '\0';
				magic_num[1] = '\0';
				magic_num[2] = '\0';
				magic_num[3] = '\0';
				expand_get_int( p_in+4, magic_num );
				memcpy( &magic_number, magic_num, 4 );
				if( magic_number == exp_data.magic_number) {
					exp_data.magic_number_ck_and_ok = 1;
#if HICK
					fprintf( stderr, "ck number equal\n" );
#endif
					break;
				} else {
					ret = -200;
					if( exp_data.verbose ) {
						fprintf( stderr,
					"  Magic number did not match!!!\n");
					}
					goto retu;
				}
			} else {
				break;
			}
		}
		if( len_out <= 0 ) {
			zero_out_ct++;
			if( zero_out_ct > 1 ) {
				ret = -935927;
				goto retu;
			}
		}
#if DATA_TYPE == 1
#if BASE_TYPE == 1
		ret2 = expansion_char( len-disp, p_in-disp, &len_used_in, 
			len_out, p_out, &len_used_out );
#else
#if BASE_TYPE == 2
		ret2 = expansion_char_short_base( len-disp, p_in-disp, &len_used_in, 
			len_out, p_out, &len_used_out );
#endif
#endif
#endif

#if (DATA_TYPE == 2 && BASE_TYPE == 2)
		ret2 = expansion_short( len-disp, p_in-disp, &len_used_in, 
			len_out, p_out, &len_used_out );
#endif
#if DATA_TYPE == 4
#if BASE_TYPE == 4
		ret2 = expansion_int( len-disp, p_in-disp, &len_used_in, 
			len_out, p_out, &len_used_out );
#else
#if BASE_TYPE == 2
		ret2 = expansion_int_unshort_base( len-disp, p_in-disp, &len_used_in, 
			len_out, p_out, &len_used_out );
#endif
#endif
#endif

#if HILOOP
		fprintf( stderr,
"%d=expansion_short( len_in=%d, , used_in=%d, %d, , used_out=%d )\n",
			ret2, len-disp, len_used_in, len_out, len_used_out );
#endif
		if( ret2 < 0 ) {
			ret = ret2;
			if( ret == -56547 ) {
				if( exp_data.verbose ) {
					fprintf( stderr,
			"  Bad bit count in segment header for compressed data!!!\n");
				}
			}
			goto retu;
		}
		len_used_in += disp;
		if( ret2 == 3) {
			if( exp_data.look_for_end_marker == 0 ) {
				len_used_in -= 4;
			}
		}
		left_over = len - len_used_in;
		if( left_over > 0 && len_used_in > 0 ) {
			memcpy( exp_data.buffer, p_in + len_used_in, left_over );
		} else {
			if( left_over < 0 ) {
#if HILEFTOVER
				fprintf( stderr,
				   " left_over=%d len=%d len_used_in=%d\n",
				   left_over, len, len_used_in );
#endif
				ret = -984564;
				goto retu;
			}
		}
		p_out += len_used_out;
		len_out_so_far += len_used_out;
		if( ret2 == 3) {
			if( exp_data.look_for_end_marker ) {
				exp_data.hit_end_marker = 1;
				if( exp_data.magic_number_check ) {
					continue;
				} else {
					break;
				}
			}
		}
		if( exp_data.look_for_end_marker == 0 ) {
			if( len_out_so_far >= len_image ) {
				if( exp_data.magic_number_check ) {
					if( exp_data.magic_number_ck_and_ok != 1) {
						ret2 = 3;
						continue;
					} else {
						break;
					}
				} else {
					break;
				}
			}
		}
		if( ret2 == 4) {
			if( exp_data.expand_to_end ) {
				expand_to_end( 1 );
			} else {
				ret = 4;
				break;
			}
		}
	}

retu:
	exp_data.left_over = left_over;
	exp_data.len_returned = len_out_so_far;
	if( ret == 0 && len_out_so_far < len_image ) {
		if( exp_data.verbose ) {
			fprintf( stderr, "  len_image = %d but only got = %d !!!\n",
				len_image, len_out_so_far);
		}
		if( ret == 0 ) {
			ret = -900;
		}
	}
	if( ret == 0 ) {
		if( exp_data.magic_number_check ) {
			if( exp_data.magic_number_ck_and_ok != 1) {
				ret = -400;
				if( exp_data.verbose ) {
					fprintf( stderr, 
						"  Magic number did not match!!!\n");
				}
			}
		}
	}
	return( ret );
}

