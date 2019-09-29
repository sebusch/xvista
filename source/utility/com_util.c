/*
 *						compression_util.c
 *						August 13, 1993
 */

#define		HI	0

#include <stdio.h>
#include "im_compress.h"

struct compress_s compress_data= { 1, 1, 0, 0, 1, 1, 1,
                        COMPRESS_BLOCK_SIZE, COMPRESS_BUFFER_SIZE,
                        1,
                        COMP_VERSION_NUM, COMP_BEGIN_MAGIC_NUM,
                        COMP_MAGIC_NUM,
                        0, 0, NULL, 0, 0 };

int compression_0bit_ok = 0;
int compression_to_end = 1;


