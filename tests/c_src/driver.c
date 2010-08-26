/*
 * Starling port driver.
 *
 * Copyright (c) 2008 Hasan Veldstra.  All rights reserved.
 * 
 * Developed by: Hasan Veldstra <hasan@12monkeys.co.uk>
 *               Hypernumbers   <http://hypernumbers.com>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 *   1. Redistributions of source code must retain the above copyright notice,
 *      this list of conditions and the following disclaimers.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimers in the
 *      documentation and/or other materials provided with the distribution.
 *   3. Neither the names of Hasan Veldstra or Hypernumbers, nor the names of
 *      its contributors may be used to endorse or promote products derived
 *      from this Software without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 */

#include <ei.h>

#include <unistd.h>
#include <sys/uio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "unicode/uchar.h"
#include "unicode/ustring.h"
#include "unicode/utypes.h"
#include "unicode/ustdio.h"
#include "unicode/uclean.h"

#include "myicu.h"
#include "starling.h"

#define ENCODEOK ei_x_encode_atom(&result, "ok")
#define ENCODESTR(str) encode_str(&result, str)
#define ENCODEBOOL(b) ei_x_encode_boolean(&result, b)
#define ENCODELONG(l) ei_x_encode_long(&result, l)
#define OKSTR(str) ENCODEOK; ENCODESTR(str)
#define OKBOOL(b)  ENCODEOK; ENCODEBOOL(b)
#define OKLONG(l)  ENCODEOK; ENCODELONG(l)

// from read_write.c
int read_cmd(byte* buf, int* size);
int write_cmd(ei_x_buff* x);
// from decode_encode.c
void decode_list(byte* buf, int* index, UChar* str);
void encode_str(ei_x_buff* result, UChar* str);


// To help with debugging.
void myprint(UChar* str) {
  UFILE* out;

  UErrorCode status = U_ZERO_ERROR;
  out = u_finit(stderr, NULL, NULL);
  if(!out) {
    fprintf(stderr, "Error initializing.");
  }

  ucnv_setFromUCallBack(u_fgetConverter(out), UCNV_FROM_U_CALLBACK_ESCAPE,
                        NULL, NULL, NULL, &status);
  if(U_FAILURE(status)) {
    u_fprintf(out, "Could not set the substitute callback.\n Error:%s\n",
              u_errorName(status));
  }

  u_fprintf(out, "%S\n", str);
  u_fclose(out);
}

int main() {
  byte* buf;
  int size = BUF_SIZE;
  char cmd[CMD_LEN];
  int index, version, tuple_arity;
  ei_x_buff result;

  if((buf = (byte *) malloc(size)) == NULL) {
    fprintf(stderr, "memory allocation error\n");
    return 1;
  }

  UChar str1[STR_LEN];
  UChar str2[STR_LEN];
  UChar str3[STR_LEN];
  UChar resbuf[STR_LEN];

  while(read_cmd(buf, &size) > 0) {
    index = 0; // reset to decode terms from the beginning of buf

    if(ei_decode_version(buf, &index, &version)) {
      fprintf(stderr, "could not decode version\n");
      return 2;
    }
    
    if(ei_decode_tuple_header(buf, &index, &tuple_arity)) {
      fprintf(stderr, "could not decode tuple header\n");
      return 3;
    }
        
    if(ei_decode_atom(buf, &index, cmd)) {
      fprintf(stderr, "could not decode command.\n");
      return 5;
    }
    
    // prepare output buffer to hold a 2-tuple.
    if(ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) {
      fprintf(stderr, "could not create result buffer\n");
      return 6;
    }

    // Decode incoming lists.
    // First
    decode_list(buf, &index, &str1[0]);
    // Second
    if((!strcmp("equal", cmd)) ||
       (!strcmp("exact", cmd)) ||
       (!strcmp("index", cmd)) ||
       (!strcmp("gsub",  cmd)) ||
       (!strcmp("concat", cmd))) {
      index++;
      decode_list(buf, &index, &str2[0]);
    }
    // Third
    if(!strcmp("gsub", cmd)) {
      index++;
      decode_list(buf, &index, &str3[0]);
    }

    // Call appropriate myicu functions.
    if(!strcmp("normalize", cmd)) {
      normalize(&str1[0], &resbuf[0], STR_LEN);
      OKSTR(&resbuf[0]);
    }
    else if(!strcmp("upcase", cmd)) {
      upcase(&str1[0], &resbuf[0], STR_LEN);
      OKSTR(&resbuf[0]);
    }
    else if(!strcmp("downcase", cmd)) {
      downcase(&str1[0], &resbuf[0], STR_LEN);
      OKSTR(&resbuf[0]);
    }
    else if(!strcmp("capitalize", cmd)) {
      capitalize(&str1[0], &resbuf[0], STR_LEN);
      OKSTR(&resbuf[0]);
    }
    else if(!strcmp("capitalize_words", cmd)) {
      capitalize_words(&str1[0], &resbuf[0], STR_LEN);
      OKSTR(&resbuf[0]);
    }
    else if(!strcmp("exact", cmd)) {
      int r = exact(&str1[0], &str2[0]);
      OKBOOL(r);
    }
    else if(!strcmp("equal", cmd)) {
      int r = equal(&str1[0], &str2[0]);
      OKBOOL(r);
    }
    else if(!strcmp("length", cmd)) {
      int len = length(&str1[0]);
      OKLONG(len);
    }
    else if(!strcmp("lengthg", cmd)) {
      int len = lengthg(&str1[0]);
      OKLONG(len);
    }
    else if(!strcmp("concat", cmd)) {
      concat(&str1[0], &str2[0], &resbuf[0]);
      OKSTR(&resbuf[0]);

    }
    else if(!strcmp("substr", cmd)) {
      index += 1;
      
      int arity;
      long start, len;
      ei_decode_tuple_header(buf, &index, &arity);
      ei_decode_long(buf, &index, &start); 
      ei_decode_long(buf, &index, &len);
      start--; // 1-based, coming from Erlang
      
      substr(&str1[0], start, len, &resbuf[0], STR_LEN);
      OKSTR(&resbuf[0]);
    }
    else if(!strcmp("index", cmd)) {
      int idx = myindex(&str1[0], &str2[0]);
      int r = (idx == -2) ? 0 : idx + 1; // make 1-based
      OKLONG(r);
    }
    else if(!strcmp("rindex", cmd)) {
      int idx = myrindex(&str1[0], &str2[0]);
      int r = (idx == -2) ? 0 : idx + 1; // make 1-based
      OKLONG(r);
    }
    else if(!strcmp("gsub", cmd)) {
      gsub(&str1[0], &str2[0], &str3[0], &resbuf[0], STR_LEN);
      OKSTR(&resbuf[0]);
    }
	else {
      ei_x_encode_atom(&result, "error");
      ei_x_encode_atom(&result, "command not supported");
    }

    write_cmd(&result);
    ei_x_free(&result);
  }

  u_cleanup(); // release resources held by ICU
  return 0;
}
