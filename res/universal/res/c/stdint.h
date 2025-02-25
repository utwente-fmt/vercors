#ifndef STDINT_H
#define STDINT_H

// If the target triple for VerCors is unset this will still use the definitions
// for the current machines target since that is the clang default. However,
// since we don't assume the size of short, long, etc. this should be fine
#if __ILP32__
#define STDINT_H_8 char
#define STDINT_H_16 short
#define STDINT_H_32 int
#define STDINT_H_64 long long
typedef signed STDINT_H_32 intptr_t;
typedef unsigned STDINT_H_32 uintptr_t;
#elif __LLP64__ || _WIN64
#define STDINT_H_8 char
#define STDINT_H_16 short
#define STDINT_H_32 int
#define STDINT_H_64 long long
typedef signed STDINT_H_64 intptr_t;
typedef unsigned STDINT_H_64 uintptr_t;
#elif __LP64__
#define STDINT_H_8 char
#define STDINT_H_16 short
#define STDINT_H_32 int
#define STDINT_H_64 long
typedef signed STDINT_H_64 intptr_t;
typedef unsigned STDINT_H_64 uintptr_t;
#else
#error "Target not supported by VerCors"
#endif

typedef signed STDINT_H_8 int8_t;
typedef signed STDINT_H_16 int16_t;
typedef signed STDINT_H_32 int32_t;
typedef signed STDINT_H_64 int64_t;
typedef unsigned STDINT_H_8 uint8_t;
typedef unsigned STDINT_H_16 uint16_t;
typedef unsigned STDINT_H_32 uint32_t;
typedef unsigned STDINT_H_64 uint64_t;

#undef STDINT_H_8
#undef STDINT_H_16
#undef STDINT_H_32
#undef STDINT_H_64

#define int_least8_t int8_t
#define int_least16_t int16_t
#define int_least32_t int32_t
#define int_least64_t int64_t
#define uint_least8_t uint8_t
#define uint_least16_t uint16_t
#define uint_least32_t uint32_t
#define uint_least64_t uint64_t

#define int_fast8_t int8_t
#define int_fast16_t int16_t
#define int_fast32_t int32_t
#define int_fast64_t int64_t
#define uint_fast8_t uint8_t
#define uint_fast16_t uint16_t
#define uint_fast32_t uint32_t
#define uint_fast64_t uint64_t

#endif

