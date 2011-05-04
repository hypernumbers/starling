/*
 * Wrapper around ICU used by the Starling driver.
 * Hasan Veldstra <hasan@12monkeys.co.uk>
 *
 * See driver.c or license.txt for licensing information.
 */

#include "unicode/uchar.h"
#include "unicode/ustring.h"
#include "unicode/utypes.h"
#include "unicode/ustdio.h"
#include "unicode/ubrk.h"
#include "unicode/usearch.h"
#include "unicode/uregex.h"
#include "unicode/unorm.h"

#include "starling.h"

#define MBERR(msg, retval) \
  if(U_FAILURE(errcode)) { \
    fprintf(stderr, "%s: %s\n", #msg, u_errorName(errcode)); \
    return retval; \
  }


int normalize(UChar* str, UChar* resbuf, int resbuflen)
{
  UErrorCode errcode = U_ZERO_ERROR;
  unorm_normalize(str, -1, UNORM_DEFAULT, 0, resbuf, resbuflen, &errcode);
  MBERR("unorm_normalize error", -1);
  return 0;
}

int upcase(UChar* str, UChar* resbuf, int resbuflen)
{
  UErrorCode errcode = U_ZERO_ERROR;
  u_strToUpper(resbuf, resbuflen, str, -1, "en", &errcode);
  MBERR("u_strToUpper error", -1);
  return 0;
}

int downcase(UChar* str, UChar* resbuf, int resbuflen)
{
  UErrorCode errcode = U_ZERO_ERROR;
  u_strToLower(resbuf, resbuflen, str, -1, "en", &errcode);
  MBERR("u_strToLower error", -1);
  return 0;
}

int equal(UChar* str1, UChar* str2)
{
  int cmp = u_strcasecmp(str1, str2, U_FOLD_CASE_DEFAULT);
  return ((cmp == 0) ? 1 : 0);
}

int exact(UChar* str1, UChar* str2)
{
  int cmp = u_strcmp(str1, str2);
  return ((cmp == 0) ? 1 : 0);
}

int length(UChar* str1)
{
  int len = u_strlen(str1);
  return len;
}

int lengthg(UChar* str)
{
  UErrorCode errcode = U_ZERO_ERROR;
  UBreakIterator* iter = ubrk_open(UBRK_CHARACTER, "en", str,
                                   u_strlen(str), &errcode);
  MBERR("ubrk_open error", -1);

  int len = -1;
  for(int currb = ubrk_first(iter);
      currb != UBRK_DONE;
      currb = ubrk_next(iter)) {
    len++;
  }

  ubrk_close(iter);
  return len;
}

int substr(UChar* str,
           int start, int len,
           UChar* resbuf, int resbuflen)
{
  UErrorCode errcode = U_ZERO_ERROR;
  UBreakIterator* iter = ubrk_open(UBRK_CHARACTER, "en",
                                   str, -1, &errcode);
  if(U_FAILURE(errcode)) {
    fprintf(stderr, "ubrk_open error: %s\n", u_errorName(errcode));
    return -1;
  }

  int currb = ubrk_first(iter);
  // skip forward as needed
  for(int i = 0; i < start; i++) {
    currb = ubrk_next(iter);
  }

  // locate the last character in resulting substr (or end of string)
  int startcpyb = currb;
  int next = ubrk_next(iter);
  int idx = 0;
  while((next != UBRK_DONE) && (idx < len)) {
    currb = next;
    next = ubrk_next(iter);
    idx++;
  }

  // copy
  int charcnt = currb - startcpyb;
  u_strncpy(resbuf, str + startcpyb, charcnt);
  resbuf[charcnt] = 0;

  ubrk_close(iter);
  return 0;
}

int concat(UChar* str1, UChar* str2, UChar* resbuf)
{
  *resbuf = 0;
  u_strcat(resbuf, str1);
  u_strcat(resbuf, str2);
  return 0;
}

// capitalize first word only
int capitalize(UChar* str, UChar* resbuf, int resbuflen)
{
  UErrorCode errcode = U_ZERO_ERROR;
  UBreakIterator* iter = ubrk_open(UBRK_WORD, "en", str, -1, &errcode);
  MBERR("ubrk_open error", -1);

  int fb = ubrk_first(iter);
  int nb = ubrk_next(iter);

  errcode = U_ZERO_ERROR;
  int wlen = u_strToTitle(resbuf, resbuflen,
                          str, nb - fb, iter, "", &errcode);
  MBERR("u_strToTitle error", -2);

  int strlen = u_strlen(str);
  u_strncpy(resbuf + wlen, str + nb - fb, strlen - nb);

  return 0;
}

int capitalize_words(UChar* str, UChar* resbuf, int resbuflen)
{
  UErrorCode errcode = U_ZERO_ERROR;
  UBreakIterator* iter = ubrk_open(UBRK_WORD, "en", str, -1, &errcode);
  MBERR("ubrk_open error", -1);

  errcode = U_ZERO_ERROR;
  u_strToTitle(&resbuf[0], resbuflen,
               str, -1,
               iter, "en", &errcode);

  MBERR("u_strToTitle error", -2);
  return 0;
}


int myindex(UChar* str1, UChar* str2)
{
  UErrorCode errcode = U_ZERO_ERROR;
  UStringSearch* srch = usearch_open(str2, -1, str1, -1,
                                     "en", NULL, &errcode);
  MBERR("usearch_open error", -1);

  int pos = usearch_first(srch, &errcode);
  usearch_close(srch);
  
  if(pos != USEARCH_DONE) {
    return pos;
  } else {
    return -2;
  }
}

int myrindex(UChar* str1, UChar* str2)
{
  UErrorCode errcode = U_ZERO_ERROR;
  UStringSearch* srch = usearch_open(str2, -1, str1, -1,
                                     "en", NULL, &errcode);
  MBERR("usearch_open error", -1);

  int pos = usearch_last(srch, &errcode);
  usearch_close(srch);
  
  if(pos != USEARCH_DONE) {
    return pos;
  } else {
    return -2;
  }
}

int gsub(UChar* str, UChar* old, UChar* new, UChar* resbuf, int resbuflen)
{
  UErrorCode errcode = U_ZERO_ERROR;
  UParseError* pe = NULL;

  URegularExpression* rx = uregex_open(old, -1, 0, pe, &errcode);
  // TODO: check parse error
  MBERR("error in uregex_open", -1);

  errcode = U_ZERO_ERROR;
  uregex_setText(rx, str, -1, &errcode);
  MBERR("error in uregex_setText", -2);

  errcode = U_ZERO_ERROR;
  uregex_replaceAll(rx, new, -1, resbuf, resbuflen, &errcode);
  MBERR("error in uregex_replaceAll", -3);

  uregex_close(rx);
  return 0;
}
