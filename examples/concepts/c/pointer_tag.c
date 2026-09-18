// Source: Lepigre et. al. 2022: VIP: Verifying Real-World C Idioms with Integer-Pointer Casts

#include <stdint.h>
#include <stdbool.h>

#define TAG_SIZE 3ULL
#define TAG_MOD (1ULL << TAG_SIZE)
#define TAG_MASK 7ULL //(TAG_MOD - 1ULL)


//@ requires p != NULL;
//@ pure
bool is_aligned(void *p) {
    return ((uintptr_t)p & (7ULL)) == 0;
}
//@ resource has_tag(void *p, int t) = p != NULL && ((uintptr_t)p & (7ULL)) == t;
//@ resource tagged(void *p, void *pt) = p != NULL && pt != NULL && (uintptr_t)pt > 7 && (uintptr_t)p == ((uintptr_t)pt & ~(7ULL));

//@ given int t2;
//@ requires p != NULL;
//@ requires 0 < (uintptr_t)p && (uintptr_t)p <= 18446744073709551615;
//@ context has_tag(p, t2);
//@ ensures \result == t2;
unsigned char tag_of(void *p) {
    //@ unfold has_tag(p, t2);
    //@ fold has_tag(p, t2);
    uintptr_t i = (uintptr_t) p;
    uintptr_t t = i & TAG_MASK;
    return t;
}

//@ requires p != NULL;
//@ requires 0 <= t && t <= 7;
//@ requires (uintptr_t)p > 7;
//@ ensures \result != NULL;
//@ ensures (uintptr_t)\result == ((((uintptr_t) p) & ~(7ULL)) | (uintptr_t)t);
//@ ensures (uintptr_t)\result > 7;
//@ ensures is_aligned(p) ==> has_tag(\result, t);
//@ ensures is_aligned(p) ==> tagged(p, \result);
void *tag(void *p, unsigned char t) {
    uintptr_t i = (uintptr_t) p;
    uintptr_t new_i = (i & ~TAG_MASK) | (uintptr_t)t;
    void *q = (void *) new_i;
    /*@ ghost
    if(new_i <= 7) {
        // Since i > 7 we have that (i & ~7) > 7, therefore (i & ~7) | t > 7
        // Usually assert false is fine here, 1 in 10 runs fail though
        assume false;
    } else {
        assert new_i == (uintptr_t)q;
        ghost lemma_tag_recoverable(i, new_i, t);
        fold has_tag(q, t);
    }*/
    /*@ ghost if (is_aligned(p)) {
        lemma_pointer_preserved(i, new_i, t);
        lemma_pointer_address_eq(i, p, new_i, q);
        fold tagged(p, q);
    }*/
    return q;
}

//@ given void *originalP;
//@ requires p != NULL;
//@ requires (uintptr_t)p > 7;
//@ requires originalP != NULL;
//@ context tagged(originalP, p);
//@ ensures \result != NULL;
//@ ensures (uintptr_t)\result == ((((uintptr_t) p) & ~(7ULL)));
//@ ensures (uintptr_t)\result == (uintptr_t)originalP;
void *untag(void *p) {
    //@ unfold tagged(originalP, p);
    //@ fold tagged(originalP, p);
    void *res = tag(p, 0);
    //@ ghost lemma_easy((uintptr_t)p, (uintptr_t)res);
    return res;
}

#include <stddef.h>


//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 <= b && b <= 18446744073709551615;
//@ requires b == ((a & ~(7ULL)) | 0ULL);
//@ ensures b == (a & ~(7ULL));
void lemma_easy(uintptr_t a, uintptr_t b);

//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 <= b && b <= 18446744073709551615;
//@ requires 0 <= t && t <= 7;
//@ requires b == ((a & ~7ULL) | (uintptr_t)t);
//@ ensures (b & 7ULL) == t;
void lemma_tag_recoverable(uintptr_t a, uintptr_t b, unsigned char t);


//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 < b && b <= 18446744073709551615;
//@ requires 0 <= t && t <= 7;
//@ requires (a & 7ULL) == 0;
//@ requires b == ((a & ~7ULL) | (uintptr_t)t);
//@ ensures (b & ~7ULL) == a;
void lemma_pointer_preserved(uintptr_t a, uintptr_t b, unsigned char t);

//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 < b && b <= 18446744073709551615;
//@ requires p != NULL && q != NULL;
//@ requires (uintptr_t)p == a;
//@ requires (uintptr_t)q == b;
//@ requires a == (b & ~7ULL);
//@ ensures (uintptr_t)p == ((uintptr_t)q & ~7ULL);
void lemma_pointer_address_eq(uintptr_t a, void *p, uintptr_t b, void *q);

void client() {
    size_t x = 0;
    void *xp = (void*)&x;
    //@ assume is_aligned(xp) && (uintptr_t)xp > 7;
    void *tp = tag((void *)&x, 1);
    //@ assert tag_of(tp)/*@ given {t2 = 1} @*/ == 1;
    size_t *px = (size_t *) untag(tp) /*@ given {originalP=xp} @*/;
    //@ assume \pointer_block(&x) == \pointer_block(px);
    //@ assert *px == 0;
}
