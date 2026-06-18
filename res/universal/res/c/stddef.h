#ifndef STDDEF_H
#define STDDEF_H

#if __LP32__ || _WIN16
typedef unsigned int size_t;
#elif __ILP32__
typedef unsigned int size_t;
#elif __LLP64__ || _WIN64 || __LP64__
typedef unsigned long size_t;
#else
#error "Target not supported by VerCors"
#endif

#endif
