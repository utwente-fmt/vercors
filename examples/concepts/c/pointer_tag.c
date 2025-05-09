// Source: Lepigre et. al. 2022: VIP: Verifying Real-World C Idioms with Integer-Pointer Casts

#include <stdint.h>
#include <stdbool.h>

#define TAG_SIZE 3ULL
#define TAG_MOD (1ULL << TAG_SIZE)
#define TAG_MASK (TAG_MOD - 1ULL)


//@ requires p != NULL;
//@ pure
bool is_aligned(void *p) {
    return ((uintptr_t)p & (7ULL)) == 0;
}
//@ resource has_tag(void *p, int t) = p != NULL && ((unsigned long long)p & (7ULL)) == t;
//@ resource tagged(void *p, void *pt) = p != NULL && pt != NULL && (unsigned long long)pt > 7 && (unsigned long long)p == ((unsigned long long)pt & ~(7ULL));

//@ given int t2;
//@ requires p != NULL;
//@ requires 0 < (unsigned long long)p && (unsigned long long)p <= 18446744073709551615;
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
//@ requires (unsigned long long)p > 7;
//@ ensures \result != NULL;
//@ ensures (unsigned long long)\result == ((((unsigned long long) p) & ~(7ULL)) | (unsigned long long)t);
//@ ensures is_aligned(p) ==> has_tag(\result, t);
//@ ensures is_aligned(p) ==> tagged(p, \result);
void *tag(void *p, unsigned char t) {
    uintptr_t i = (uintptr_t) p;
    uintptr_t new_i = (i & ~TAG_MASK) | (uintptr_t)t;
    void *q = (void *) new_i;
    /*@ ghost
    if(new_i == 0) {
        assert false;
    } else {
        assert new_i == (unsigned long long)q;
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
//@ requires (unsigned long long)p > 7;
//@ requires originalP != NULL;
//@ context tagged(originalP, p);
//@ ensures \result != NULL;
//@ ensures (unsigned long long)\result == ((((unsigned long long) p) & ~(7ULL)));
//@ ensures (unsigned long long)\result == (unsigned long long)originalP;
void *untag(void *p) {
    //@ unfold tagged(originalP, p);
    //@ fold tagged(originalP, p);
    void *res = tag(p, 0);
    //@ ghost lemma_easy((unsigned long long)p, (unsigned long long)res);
    return res;
}

#include <stddef.h>


//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 <= b && b <= 18446744073709551615;
//@ requires b == ((a & ~(7ULL)) | 0ULL);
//@ ensures b == (a & ~(7ULL));
void lemma_easy(unsigned long long a, unsigned long long b);

//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 <= b && b <= 18446744073709551615;
//@ requires 0 <= t && t <= 7;
//@ requires b == ((a & ~7ULL) | (unsigned long long)t);
//@ ensures (b & 7ULL) == t;
void lemma_tag_recoverable(unsigned long long a, unsigned long long b, unsigned char t);


//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 < b && b <= 18446744073709551615;
//@ requires 0 <= t && t <= 7;
//@ requires (a & 7ULL) == 0;
//@ requires b == ((a & ~7ULL) | (unsigned long long)t);
//@ ensures (b & ~7ULL) == a;
void lemma_pointer_preserved(unsigned long long a, unsigned long long b, unsigned char t);

//@ requires 0 < a && a <= 18446744073709551615;
//@ requires 0 < b && b <= 18446744073709551615;
//@ requires p != NULL && q != NULL;
//@ requires (unsigned long long)p == a;
//@ requires (unsigned long long)q == b;
//@ requires a == (b & ~7ULL);
//@ ensures (unsigned long long)p == ((unsigned long long)q & ~7ULL);
void lemma_pointer_address_eq(unsigned long long a, void *p, unsigned long long b, void *q);

void client() {
    size_t x = 0;
    void *xp = (void*)&x;
    //@ assume is_aligned(xp);
    void *tp = tag((void *)&x, 1);
    //@ assert tag_of(tp)/*@ given {t2 = 1} @*/ == 1;
    size_t *px = (size_t *) untag(tp) /*@ given {originalP=xp} @*/;
    //@ assume \pointer_block(&x) == \pointer_block(px);
    //@ assert *px == 0;
}
