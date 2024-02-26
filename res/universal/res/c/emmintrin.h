#ifndef _EMMINTRIN_H_INCLUDED
#define _EMMINTRIN_H_INCLUDED

// emmintrin.h normally contains other definitions of __m128i. It is used as a type for integers vectors
// for any size of the individual integers. And the specific functions used, tell us how to interpret
// the vector. For example, _mm_add_epi32 tells us that the vector is composed of 32-bit integers.
// In VerCors this is not useful, we want to know the size of the vector from the type, and not switch
// between vector interpretations. So we defined __m128i to be a vector of 2 integers in VerCors.

typedef double __v2df __attribute__ ((__vector_size__ (sizeof(double)*2)));
typedef long long __v2di __attribute__ ((__vector_size__ (sizeof(long long)*2)));
typedef unsigned long long __v2du __attribute__ ((__vector_size__ (sizeof(unsigned long long)*2)));
typedef int __v4si __attribute__ ((__vector_size__ (sizeof(int)*4)));
typedef unsigned int __v4su __attribute__ ((__vector_size__ (sizeof(unsigned int)*4)));
typedef short __v8hi __attribute__ ((__vector_size__ (sizeof(short)*8)));
typedef unsigned short __v8hu __attribute__ ((__vector_size__ (sizeof(unsigned short)*8)));
//typedef char __v16qi __attribute__ ((__vector_size__ (sizeof(char)*16)));
//typedef signed char __v16qs __attribute__ ((__vector_size__ (sizeof(signed char)*16)));
//typedef unsigned char __v16qu __attribute__ ((__vector_size__ (sizeof(unsigned char)*16)));

typedef long long __m128i __attribute__ ((__vector_size__ (sizeof(long long)*2)));
typedef double __m128d __attribute__ ((__vector_size__ (sizeof(double)*2)));

typedef long long __m128i_u __attribute__ ((__vector_size__ (sizeof(long long)*2)));
typedef double __m128d_u __attribute__ ((__vector_size__ (sizeof(double)*2)));

typedef float __m128 __attribute__ ((__vector_size__ (sizeof(float)*4)));
typedef float __m128_u __attribute__ ((__vector_size__ (sizeof(float)*4)));
typedef float __v4sf __attribute__ ((__vector_size__ (sizeof(float)*4)));

//@ ensures \result[0] == __q0 && \result[1] == __q1;
__m128i /*@ pure @*/ _mm_set_epi64x(long long __q1, long long __q0){
    __m128i res = {__q0, __q1};
    return res;
}

//@ ensures \result[0] == __A && \result[1] == __A;
__m128i /*@ pure @*/ _mm_set1_epi64x (long long __A)
{
  return _mm_set_epi64x (__A, __A);
}


#endif