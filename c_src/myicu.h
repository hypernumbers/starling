
int normalize(UChar* str, UChar* resbuf, int resbuflen);
int upcase(UChar* str, UChar* resbuf, int resbuflen);
int downcase(UChar* str, UChar* resbuf, int resbuflen);
int equal(UChar* str1, UChar* str2);
int exact(UChar* str1, UChar* str2);
int length(UChar* str1);
int lengthg(UChar* str);
int concat(UChar* str1, UChar* str2, UChar* resbuf);
int substr(UChar* str,
           int start, int len,
           UChar* resbuf, int resbuflen);
int capitalize(UChar* str, UChar* resbuf, int resbuflen);
int capitalize_words(UChar* str, UChar* resbuf, int resbuflen);
int myindex(UChar* str1, UChar* str2);
int myrindex(UChar* str1, UChar* str2);
int gsub(UChar* str, UChar* old, UChar* new, UChar* resbuf, int resbuflen);
