/*
 * Functions for C <-> Erlang term conversion.
 * Hasan Veldstra <hasan@12monkeys.co.uk>
 *
 * See driver.c or license.txt for licensing information.
 */

#include <ei.h>

#include "unicode/uchar.h"
#include "unicode/ustring.h"

#include "starling.h"

void decode_list(byte* buf, int* index, UChar* str);
void encode_str(ei_x_buff* result, UChar* str);

void decode_list(byte* buf, int* index, UChar* str)
{
  int arity;
  ei_decode_list_header(buf, index, &arity);
  for(int i = 0; i < arity; i++) {
    long x;
    ei_decode_long(buf, index, &x);
    str[i] = x;
  }
  str[arity - 1] = 0; // null-terminate and overwrite the extra long (256)
}

void encode_str(ei_x_buff* result, UChar* str)
{
  int len = u_strlen(str);
  ei_x_encode_list_header(result, len);
  
  for(int i = 0; i < len; i++) {
    long l = *(str + i);
    ei_x_encode_long(result, l);
  }

  ei_x_encode_empty_list(result);
}
