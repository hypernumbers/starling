/*
 * Functions for C <-> Erlang communication.
 * Hasan Veldstra <hasan@12monkeys.co.uk>
 * Originally from "Concurrent Programming In Erlang" by Joe Armstrong,
 * Robert Virding, Claes Wikström, and Mike Williams.
 *
 * See driver.c or license.txt for licensing information.
 */

// The functions in this file assume 2-byte length header.

#include <ei.h>

#include <stdlib.h>
#include <unistd.h>
#include <sys/uio.h>
#include <sys/types.h>

#include "starling.h"

int read_cmd(unsigned char * buf);
int write_cmd(ei_x_buff* x);
int read_exact(unsigned char * buf, int len);
int write_exact(byte* buf, int len);


int read_cmd(unsigned char * buf)
{
  int len;
  if (read_exact(buf, 2) != 2) {
    return -1;
  }
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(ei_x_buff* buff)
{
  byte li;

  li = (buff->index >> 8) & 0xff; 
  write_exact(&li, 1);
  li = buff->index & 0xff;
  write_exact(&li, 1);

  return write_exact(buff->buff, buff->index);
}

int read_exact(unsigned char * buf, int len)
{
  int i, got = 0;

  do {
    if((i = read(0, buf + got, len - got)) <= 0)
      return i;
    got += i;
  } while(got < len);

  return len;
}

int write_exact(byte* buf, int len)
{
  int i, wrote = 0;

  do {
    if((i = write(1, buf + wrote, len - wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote < len);

  return len;
}
