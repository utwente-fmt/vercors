// Compile with
// gcc -mavx -o vector_add vector_add.c

#include <assert.h>
#include <immintrin.h>
#include <stdbool.h>

/*@
    context_everywhere n >= 0 && n % 4 == 0;
    context_everywhere a != NULL && \pointer_length(a) >= n;
    context_everywhere b != NULL && \pointer_length(b) >= n;
    context_everywhere c != NULL && \pointer_length(c) >= n;
    context_everywhere (\forall* int i; 0<=i && i<n; Perm(&a[i], 1\2));
    context_everywhere (\forall* int i; 0<=i && i<n; Perm(&b[i], 1\2));
    context_everywhere (\forall* int i; 0<=i && i<n; Perm(&c[i], write));
    ensures (\forall int j; 0 <= j && j<n; {:c[j]:} == a[j] + b[j]);
@*/
void vector_add(int *a, int *b, int *c, int n){
    /*@
        loop_invariant 0<=i && i<=n && i%4 == 0;
        loop_invariant (\forall int j; 0 <= j && j<i; {:c[j]:} == a[j] + b[j]);
    @*/
    for(int i = 0; i < n; i+=4){
        // Normally this should be:
        // a4 = _mm_load_si128((__m128i*)(a + i));
        // _mm_load_si128
        // But then we need good support for casting between pointers.
        __v4si a4 = {a[i], a[i+1], a[i+2], a[i+3]};
        __v4si b4 = {b[i], b[i+1], b[i+2], b[i+3]};
        __v4si c4 = a4 + b4;
        __v4si d4 = a4 == b4;
        // _mm_store_si128(c+i, c4);
        c[i] = c4[0];
        c[i+1] = c4[1];
        c[i+2] = c4[2];
        c[i+3] = c4[3];
    }
}


int main(){

    int a[4*100];
    int b[4*100];
    /*@
        loop_invariant 0<=i && i<=4*100;
        loop_invariant (\forall* int j; 0 <= j && j<4*100; Perm(&a[j],write));
        loop_invariant (\forall* int j; 0 <= j && j<4*100; Perm(&b[j],write));
        loop_invariant (\forall int j; 0 <= j && j<i; a[j] == j);
        loop_invariant (\forall int j; 0 <= j && j<i; b[j] == 400-j);
    @*/
    for(int i = 0; i < 4*100; i++){
        a[i] = i;
        b[i] = 400-i;
    }
    int c[4*100];
    vector_add(a, b, c, 4*100);
    //@ assert((\forall int j; 0 <= j && j<4*100; c[j] == 400));
    /*@
        loop_invariant 0<=i && i<=4*100;
        loop_invariant (\forall* int j; 0 <= j && j<4*100; Perm(&c[j],write));
        loop_invariant (\forall int j; 0 <= j && j<4*100; c[j] == 400);
    @*/
    for(int i=0; i<4*100; i++){
        assert(c[i] == 400);
    }

    return 0;
}