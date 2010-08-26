
typedef char byte;
#define BUF_SIZE 65536 // 2^16, since we're using a 2-byte length header
#define STR_LEN BUF_SIZE/2 // sizeof(UChar) = sizeof(char) * 2
#define CMD_LEN 32
