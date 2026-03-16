#ifndef STDINT_H
#define STDINT_H


// If the target triple for VerCors is unset this will still use the definitions
// for the current machines target since that is the clang default. However,
// since we don't assume the size of short, long, etc. this should be fine
#if __LP32__ || _WIN16
#define STDINT_H_8 char
#define STDINT_H_16 int
#define STDINT_H_32 long
#define STDINT_H_64 long long
#define STDINT_H_SUFFIX_8
#define STDINT_H_SUFFIX_16
#define STDINT_H_SUFFIX_32 L
#define STDINT_H_SUFFIX_64 LL
typedef signed STDINT_H_32 intptr_t;
typedef unsigned STDINT_H_32 uintptr_t;
#define UINTPTR_WIDTH            32
#define INTPTR_WIDTH             UINTPTR_WIDTH
#define UINTPTR_MAX              4294967295UL
#define INTPTR_MAX               2147483647L
#define SIZE_WIDTH               32
#define SIZE_MAX                 4294967295UL
#elif __ILP32__
#define STDINT_H_8 char
#define STDINT_H_16 short
#define STDINT_H_32 int
#define STDINT_H_64 long long
#define STDINT_H_SUFFIX_8
#define STDINT_H_SUFFIX_16
#define STDINT_H_SUFFIX_32
#define STDINT_H_SUFFIX_64 LL
typedef signed STDINT_H_32 intptr_t;
typedef unsigned STDINT_H_32 uintptr_t;
#define UINTPTR_WIDTH            32
#define INTPTR_WIDTH             UINTPTR_WIDTH
#define UINTPTR_MAX              4294967295U
#define INTPTR_MAX               2147483647
#define SIZE_WIDTH               32
#define SIZE_MAX                 4294967295U
#elif __LLP64__ || _WIN64
#define STDINT_H_8 char
#define STDINT_H_16 short
#define STDINT_H_32 int
#define STDINT_H_64 long long
#define STDINT_H_SUFFIX_8
#define STDINT_H_SUFFIX_16
#define STDINT_H_SUFFIX_32
#define STDINT_H_SUFFIX_64 LL
typedef signed STDINT_H_64 intptr_t;
typedef unsigned STDINT_H_64 uintptr_t;
#define UINTPTR_WIDTH            64
#define INTPTR_WIDTH             UINTPTR_WIDTH
#define UINTPTR_MAX              18446744073709551615ULL
#define INTPTR_MAX               9223372036854775807LL
#define SIZE_WIDTH               64
#define SIZE_MAX                 18446744073709551615ULL
#elif __LP64__
#define STDINT_H_8 char
#define STDINT_H_16 short
#define STDINT_H_32 int
#define STDINT_H_64 long
#define STDINT_H_SUFFIX_8
#define STDINT_H_SUFFIX_16
#define STDINT_H_SUFFIX_32
#define STDINT_H_SUFFIX_64 L
typedef signed STDINT_H_64 intptr_t;
typedef unsigned STDINT_H_64 uintptr_t;
#define UINTPTR_WIDTH            64
#define INTPTR_WIDTH             UINTPTR_WIDTH
#define UINTPTR_MAX              18446744073709551615UL
#define INTPTR_MAX               9223372036854775807L
#define SIZE_WIDTH               64
#define SIZE_MAX                 18446744073709551615UL
#else
#error "Target not supported by VerCors"
#endif

#define INTPTR_MIN               (-INTPTR_MAX - 1)


// From LLVM's stdint.h:
#define STDINT_H_int_c_join(a, b) a##b
#define STDINT_H_int_c(v, suffix) STDINT_H_int_c_join(v, suffix)
#define STDINT_H_uint_c(v, suffix) STDINT_H_int_c_join(v##U, suffix)

#define INT8_C(v) STDINT_H_int_c(v, STDINT_H_SUFFIX_8)
#define UINT8_C(v) STDINT_H_uint_c(v, STDINT_H_SUFFIX_8)
#define INT16_C(v) STDINT_H_int_c(v, STDINT_H_SUFFIX_16)
#define UINT16_C(v) STDINT_H_uint_c(v, STDINT_H_SUFFIX_16)
#define INT32_C(v) STDINT_H_int_c(v, STDINT_H_SUFFIX_32)
#define UINT32_C(v) STDINT_H_uint_c(v, STDINT_H_SUFFIX_32)
#define INT64_C(v) STDINT_H_int_c(v, STDINT_H_SUFFIX_64)
#define UINT64_C(v) STDINT_H_uint_c(v, STDINT_H_SUFFIX_64)


typedef signed STDINT_H_8 int8_t;
typedef signed STDINT_H_16 int16_t;
typedef signed STDINT_H_32 int32_t;
typedef signed STDINT_H_64 int64_t;
typedef unsigned STDINT_H_8 uint8_t;
typedef unsigned STDINT_H_16 uint16_t;
typedef unsigned STDINT_H_32 uint32_t;
typedef unsigned STDINT_H_64 uint64_t;

#define UINT8_WIDTH              8
#define INT8_WIDTH               UINT8_WIDTH
#define UINT8_MAX                UINT8_C(255)
#define INT8_MAX                 INT8_C(127)
#define INT8_MIN                 (-INT8_MAX - 1)
#define UINT16_WIDTH             16
#define INT16_WIDTH              UINT16_WIDTH
#define UINT16_MAX               UINT16_C(65535)
#define INT16_MAX                INT16_C(32767)
#define INT16_MIN                (-INT16_MAX - 1)
#define UINT32_WIDTH             32
#define INT32_WIDTH              UINT32_WIDTH
#define UINT32_MAX               UINT32_C(4294967295)
#define INT32_MAX                INT32_C(2147483647)
#define INT32_MIN                (-INT32_MAX - 1)
#define UINT64_WIDTH             64
#define INT64_WIDTH              UINT64_WIDTH
#define UINT64_MAX               UINT64_C(18446744073709551615)
#define INT64_MAX                INT64_C(9223372036854775807)
#define INT64_MIN                (-INT64_MAX - 1)

#undef STDINT_H_8
#undef STDINT_H_16
#undef STDINT_H_32
#undef STDINT_H_64

// We are defining the least and fast types here but these might not match the actual platform this is verified for

typedef int8_t int_least8_t;
typedef int16_t int_least16_t;
typedef int32_t int_least32_t;
typedef int64_t int_least64_t;
typedef uint8_t uint_least8_t;
typedef uint16_t uint_least16_t;
typedef uint32_t uint_least32_t;
typedef uint64_t uint_least64_t;

#define UINT_LEAST8_WIDTH        8
#define INT_LEAST8_WIDTH         UINT_LEAST8_WIDTH
#define UINT_LEAST8_MAX          UINT8_C(255)
#define INT_LEAST8_MAX           INT8_C(127)
#define INT_LEAST8_MIN           (-INT_LEAST8_MAX - 1)
#define UINT_LEAST16_WIDTH       16
#define INT_LEAST16_WIDTH        UINT_LEAST16_WIDTH
#define UINT_LEAST16_MAX         UINT16_C(65535)
#define INT_LEAST16_MAX          INT16_C(32767)
#define INT_LEAST16_MIN          (-INT_LEAST16_MAX - 1)
#define UINT_LEAST32_WIDTH       32
#define INT_LEAST32_WIDTH        UINT_LEAST32_WIDTH
#define UINT_LEAST32_MAX         UINT32_C(4294967295)
#define INT_LEAST32_MAX          INT32_C(2147483647)
#define INT_LEAST32_MIN          (-INT_LEAST32_MAX - 1)
#define UINT_LEAST64_WIDTH       64
#define INT_LEAST64_WIDTH        UINT_LEAST64_WIDTH
#define UINT_LEAST64_MAX         UINT64_C(18446744073709551615)
#define INT_LEAST64_MAX          INT64_C(9223372036854775807)
#define INT_LEAST64_MIN          (-INT_LEAST64_MAX - 1)

typedef int8_t int_fast8_t;
typedef int16_t int_fast16_t;
typedef int32_t int_fast32_t;
typedef int64_t int_fast64_t;
typedef uint8_t uint_fast8_t;
typedef uint16_t uint_fast16_t;
typedef uint32_t uint_fast32_t;
typedef uint64_t uint_fast64_t;

#define UINT_FAST8_WIDTH        8
#define INT_FAST8_WIDTH         UINT_FAST8_WIDTH
#define UINT_FAST8_MAX          UINT8_C(255)
#define INT_FAST8_MAX           INT8_C(127)
#define INT_FAST8_MIN           (-INT_FAST8_MAX - 1)
#define UINT_FAST16_WIDTH       16
#define INT_FAST16_WIDTH        UINT_FAST16_WIDTH
#define UINT_FAST16_MAX         UINT16_C(65535)
#define INT_FAST16_MAX          INT16_C(32767)
#define INT_FAST16_MIN          (-INT_FAST16_MAX - 1)
#define UINT_FAST32_WIDTH       32
#define INT_FAST32_WIDTH        UINT_FAST32_WIDTH
#define UINT_FAST32_MAX         UINT32_C(4294967295)
#define INT_FAST32_MAX          INT32_C(2147483647)
#define INT_FAST32_MIN          (-INT_FAST32_MAX - 1)
#define UINT_FAST64_WIDTH       64
#define INT_FAST64_WIDTH        UINT_FAST64_WIDTH
#define UINT_FAST64_MAX         UINT64_C(18446744073709551615)
#define INT_FAST64_MAX          INT64_C(9223372036854775807)
#define INT_FAST64_MIN          (-INT_FAST64_MAX - 1)

#endif

