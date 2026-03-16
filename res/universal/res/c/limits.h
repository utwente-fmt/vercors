#ifndef LIMITS_H
#define LIMITS_H

#define __STDC_VERSION_LIMITS_H__ 202311L

#define BOOL_WIDTH       1
#define BOOL_MAX         1
#define CHAR_BIT         8
#define CHAR_WIDTH       CHAR_BIT
#define SCHAR_WIDTH      CHAR_BIT
#define UCHAR_WIDTH      CHAR_BIT
// We define char to be signed on all platforms we support
#define CHAR_MAX         SCHAR_MAX
#define CHAR_MIN         SCHAR_MIN
#define UCHAR_MAX        255
#define SCHAR_MAX        127
#define SCHAR_MIN        (-SCHAR_MAX - 1)

#define USHRT_WIDTH      16
#define SHRT_WIDTH       USHRT_WIDTH
#define USHRT_MAX        65535U
#define SHRT_MAX         32767
#define SHRT_MIN         (-SHRT_MAX - 1)
#define ULLONG_WIDTH     64
#define LLONG_WIDTH      ULLONG_WIDTH
#define ULLONG_MAX       18446744073709551615ULL
#define LLONG_MAX        9223372036854775807LL
#define LLONG_MIN        (-LLONG_MAX - 1)

#if __LP32__ || _WIN16
#define UINT_WIDTH       16
#define UINT_MAX         65535U
#define INT_MAX          32767
#define ULONG_WIDTH      32
#define ULONG_MAX        4294967295UL
#define LONG_MAX         2147483647L
#elif __ILP32__
#define UINT_WIDTH       32
#define UINT_MAX         4294967295U
#define INT_MAX          2147483647
#define ULONG_WIDTH      32
#define ULONG_MAX        4294967295UL
#define LONG_MAX         2147483647L
#elif __LLP64__ || _WIN64
#define UINT_WIDTH       32
#define UINT_MAX         4294967295U
#define INT_MAX          2147483647
#define ULONG_WIDTH      32
#define ULONG_MAX        4294967295UL
#define LONG_MAX         2147483647L
#elif __LP64__
#define UINT_WIDTH       32
#define UINT_MAX         4294967295U
#define INT_MAX          2147483647
#define ULONG_WIDTH      64
#define ULONG_MAX        18446744073709551615UL
#define LONG_MAX         9223372036854775807L
#else
#error "Target not supported by VerCors"
#endif

#define INT_WIDTH        UINT_WIDTH
#define LONG_WIDTH       ULONG_WIDTH
#define INT_MIN          (-INT_MAX - 1)
#define LONG_MIN         (-LONG_MAX - 1)

// Left undefined since the value is not strictly defined and not necessarily the same for every compiler
//define BITINT_MAXWIDTH 128
//define MB_LEN_MAX      2? 4? 6?

#endif

