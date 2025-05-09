#ifndef _EMMINTRIN_H_INCLUDED
#define _EMMINTRIN_H_INCLUDED

// emmintrin.h normally contains other definitions of __m128i. It is used as a type for integers vectors
// for any size of the individual integers. And the specific functions used, tell us how to interpret
// the vector. For example, _mm_add_epi32 tells us that the vector is composed of 32-bit integers.
// In VerCors this is not useful, we want to know the size of the vector from the type, and not switch
// between vector interpretations. So we defined __m128i to be a vector of 2 integers in VerCors.
// Also see reference guide: https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html

typedef double __v2df __attribute__ ((__vector_size__ (sizeof(double)*2)));
typedef long long __v2di __attribute__ ((__vector_size__ (sizeof(long long)*2)));
typedef unsigned long long __v2du __attribute__ ((__vector_size__ (sizeof(unsigned long long)*2)));
typedef int __v4si __attribute__ ((__vector_size__ (sizeof(int)*4)));
typedef float __v4sf __attribute__ ((__vector_size__ (sizeof(float)*4)));
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


typedef float __m256 __attribute__ ((__vector_size__ (sizeof(float)*8)));
typedef long long __m256i __attribute__ ((__vector_size__ (sizeof(long long)*4)));
typedef double __m256d __attribute__ ((__vector_size__ (sizeof(double)*4)));

/* Unaligned version of the same types.  */
typedef float __m256_u __attribute__ ((__vector_size__ (sizeof(float)*8)));
typedef long long __m256i_u __attribute__ ((__vector_size__ (sizeof(long long)*4)));
typedef double __m256d_u __attribute__ ((__vector_size__ (sizeof(double)*4)));

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

/**** Loads ****/
// SSE2

/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1] && __P[2] == \result[2] && __P[3] == \result[3];
@*/
/*@ pure @*/ __m128 _mm_loadu_ps (float *__P);

/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1] && __P[2] == \result[2] && __P[3] == \result[3];
@*/
/*@ pure @*/ __m128 _mm_load_ps (float *__P);

/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1];
@*/
/*@ pure @*/ __m128d _mm_loadu_pd (double *__P);

/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1];
@*/
/*@ pure @*/ __m128d _mm_load_pd (double *__P);

/*@
  requires __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(((long long *)__P)[i], read));
  ensures ((long long*)__P)[0] == \result[0] && ((long long*)__P)[1] == \result[1];
@*/
/*@ pure @*/ __m128i _mm_loadu_epi64 (void *__P);


/*@
  requires __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(((long long *)__P)[i], read));
  ensures ((long long*)__P)[0] == \result[0] && ((long long*)__P)[1] == \result[1];
@*/
/*@ pure @*/ __m128i _mm_load_epi64 (void *__P);

/** AVX256 **/
/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<8; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1] && __P[2] == \result[2] && __P[3] == \result[3] &&
      __P[4] == \result[4] && __P[5] == \result[5] && __P[6] == \result[6] && __P[7] == \result[7];
@*/
/*@ pure @*/ __m256 _mm256_loadu_ps (float *__P);

/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<8; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1] && __P[2] == \result[2] && __P[3] == \result[3] &&
      __P[4] == \result[4] && __P[5] == \result[5] && __P[6] == \result[6] && __P[7] == \result[7];
@*/
/*@ pure @*/ __m256 _mm256_load_ps (float *__P);

/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1] && __P[2] == \result[2] && __P[3] == \result[3];
@*/
/*@ pure @*/ __m256d _mm256_loadu_pd (double *__P);

/*@
  requires __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], read));
  ensures __P[0] == \result[0] && __P[1] == \result[1] && __P[2] == \result[2] && __P[3] == \result[3];
@*/
/*@ pure @*/ __m256d _mm256_load_pd (double *__P);

/*@
  requires __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(((long long *)__P)[i], read));
  ensures ((long long*)__P)[0] == \result[0] && ((long long*)__P)[1] == \result[1] && ((long long*)__P)[2] == \result[2] && ((long long*)__P)[3] == \result[3];
@*/
/*@ pure @*/ __m256i _mm256_loadu_epi64 (void *__P);


/*@
  requires __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(((long long *)__P)[i], read));
  ensures ((long long*)__P)[0] == \result[0] && ((long long*)__P)[1] == \result[1] && ((long long*)__P)[2] == \result[2] && ((long long*)__P)[3] == \result[3];
@*/
/*@ pure @*/ __m256i _mm256_load_epi64 (void *__P);

/**** Stores ****/
// SSE2
/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1] && __P[2] == __A[2] && __P[3] == __A[3];
@*/
void _mm_store_ps (float *__P, __m128 __A);

/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1] && __P[2] == __A[2] && __P[3] == __A[3];
@*/
void _mm_storeu_ps (float *__P, __m128 __A);

/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1];
@*/
void _mm_store_pd (double *__P, __m128d __A);

/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1];
@*/
void _mm_storeu_pd (double *__P, __m128d __A);

/*@
  context __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(((long long *)__P)[i], write));
  ensures ((long long *) __P)[0] == __A[0] && ((long long *) __P)[1] == __A[1];
@*/
void _mm_storeu_epi64 (void *__P, __m128i __A);

/*@
  context __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<2; Perm(((long long *)__P)[i], write));
  ensures ((long long *) __P)[0] == __A[0] && ((long long *) __P)[1] == __A[1];
@*/
void _mm_store_epi64 (void *__P, __m128i __A);

/** AVX256 **/
/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<8; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1] && __P[2] == __A[2] && __P[3] == __A[3] &&
    __P[4] == __A[4] && __P[5] == __A[5] && __P[6] == __A[6] && __P[7] == __A[7];
@*/
void _mm256_store_ps (float *__P, __m256 __A);

/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<8; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1] && __P[2] == __A[2] && __P[3] == __A[3] &&
    __P[4] == __A[4] && __P[5] == __A[5] && __P[6] == __A[6] && __P[7] == __A[7];
@*/
void _mm256_storeu_ps (float *__P, __m256 __A);

/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1] && __P[2] == __A[2] && __P[3] == __A[3];
@*/
void _mm256_store_pd (double *__P, __m256d __A);

/*@
  context __P != NULL ** \pointer_length(__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(__P[i], write));
  ensures __P[0] == __A[0] && __P[1] == __A[1] && __P[2] == __A[2] && __P[3] == __A[3];
@*/
void _mm256_storeu_pd (double *__P, __m256d __A);

/*@
  context __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(((long long *)__P)[i], write));
  ensures ((long long *) __P)[0] == __A[0] && ((long long *) __P)[1] == __A[1] && ((long long *) __P)[2] == __A[2] && ((long long *) __P)[3] == __A[3];
@*/
void _mm256_storeu_epi64 (void *__P, __m256i __A);

/*@
  context __P != NULL ** \pointer_length((long long *)__P) >= 8 ** (\forall* int i; 0<=i && i<4; Perm(((long long *)__P)[i], write));
  ensures ((long long *) __P)[0] == __A[0] && ((long long *) __P)[1] == __A[1] && ((long long *) __P)[2] == __A[2] && ((long long *) __P)[3] == __A[3];
@*/
void _mm256_store_epi64 (void *__P, __m256i __A);

/**** Some operators ****/
// SSE2
// __m128
/*@ pure @*/ __m128 _mm_add_ps (__m128 __A, __m128 __B){
    return __A + __B;
}

/*@ pure */ __m128 _mm_div_ps (__m128 __A, __m128 __B){
    return __A / __B;
}

/*@ pure @*/ __m128 _mm_mul_ps (__m128 __A, __m128 __B){
    return __A * __B;
}

/*@ pure @*/ __m128 _mm_sub_ps (__m128 __A, __m128 __B){
    return __A - __B;
}

// __m128d
/*@ pure @*/ __m128d _mm_add_pd (__m128d __A, __m128d __B){
    return __A + __B;
}

/*@ pure */ __m128d _mm_div_pd (__m128d __A, __m128d __B){
    return __A / __B;
}

/*@ pure @*/ __m128d _mm_mul_pd (__m128d __A, __m128d __B){
    return __A * __B;
}

/*@ pure @*/ __m128d _mm_sub_pd (__m128d __A, __m128d __B){
    return __A - __B;
}

// __m128i
/*@ pure @*/ __m128i _mm_add_epi64(__m128i __A, __m128i __B){
    return __A + __B;
}

//@ requires __B[0] != 0 ** __B[1] != 0;
/*@ pure @*/ __m128i _mm_div_epi64(__m128i __A, __m128i __B){
    return __A / __B;
}

/*@ pure @*/ __m128i _mm_mul_epi64(__m128i __A, __m128i __B){
    return __A * __B;
}

/*@ pure @*/ __m128i _mm_sub_epi64(__m128i __A, __m128i __B){
    return __A - __B;
}

// AVX2
// __m256
/*@ pure @*/ __m256 _mm256_add_ps (__m256 __A, __m256 __B){
    return __A + __B;
}

/*@ pure */ __m256 _mm256_div_ps (__m256 __A, __m256 __B){
    return __A / __B;
}

/*@ pure @*/ __m256 _mm256_mul_ps (__m256 __A, __m256 __B){
    return __A * __B;
}

/*@ pure @*/ __m256 _mm256_sub_ps (__m256 __A, __m256 __B){
    return __A - __B;
}

// __m256d
/*@ pure @*/ __m256d _mm256_add_pd (__m256d __A, __m256d __B){
    return __A + __B;
}

/*@ pure */ __m256d _mm256_div_pd (__m256d __A, __m256d __B){
    return __A / __B;
}

/*@ pure @*/ __m256d _mm256_mul_pd (__m256d __A, __m256d __B){
    return __A * __B;
}

/*@ pure @*/ __m256d _mm256_sub_pd (__m256d __A, __m256d __B){
    return __A - __B;
}

// __m256i
/*@ pure @*/ __m256i _mm256_add_epi64(__m256i __A, __m256i __B){
    return __A + __B;
}

//@ requires __B[0] != 0 ** __B[1] != 0 ** __B[2] != 0 ** __B[3] != 0;
/*@ pure @*/ __m256i _mm256_div_epi64(__m256i __A, __m256i __B){
    return __A / __B;
}

/*@ pure @*/ __m256i _mm256_mul_epi64(__m256i __A, __m256i __B){
    return __A * __B;
}

/*@ pure @*/ __m256i _mm256_sub_epi64(__m256i __A, __m256i __B){
    return __A - __B;
}


#endif
