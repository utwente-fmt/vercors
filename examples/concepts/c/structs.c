#include <assert.h>
#include <stddef.h>

struct point {
    int x;
    int y;
};

struct rect{
    struct point p1, p2, p3;
};

struct polygon{
    struct point* ps;
};

struct linked_list{
  struct linked_list *p1;
  int x;
};

/*@
    context p != NULL ** Perm(p, write);
    context Perm(p->x, write);
    context Perm(p->y, write);
    ensures p->x == 0;
    ensures p->y == 0;
    ensures \old(*p) == *p;
@*/
void alter_struct(struct point *p){
    p->x = 0;
    p->y = 0;
}

/*@
    context p != NULL ** Perm(p, write) ** Perm(*p, write);
    ensures p->x == \old(p->x + 1);
    ensures p->y == \old(p->y + 1);
    ensures \old(*p) == *p;
@*/
void alter_struct_1(struct point *p){
    p->x = p->x+1;
    p->y = p->y+1;
}

/*@
  context Perm(p.x, 1\1);
  context Perm(p.y, 1\1);
@*/
void alter_copy_struct(struct point p){
    p.x = 0;
    p.y = 0;
}

/*@
  context Perm(p, 1\1);
@*/
void alter_copy_struct_2(struct point p){
    p.x = 0;
    p.y = 0;
}

/*@
  context r != NULL ** Perm(r, 1\2) ** Perm(*r, 1\2);
  ensures \result == (r->p1.x + r->p2.x + r->p3.x)/3;
@*/
int avr_x(struct rect *r){
    return (r->p1.x + r->p2.x + r->p3.x)/3;
}
/*@
 requires n >= 0;
 requires inp != NULL && \pointer_length(inp) >= n;
 requires (\forall* int i; 0 <= i && i < n; Perm(&inp[i], 1\10));
 requires (\forall int i, int j; 0<=i && i<n && 0<=j && j<n; i != j ==> inp[i] != inp[j]);
 requires (\forall* int i; 0 <= i && i < n; Perm(inp[i].x, 1\10));
 ensures |\result| == n;
 ensures (\forall int i; 0 <= i && i < n; \result[i] == inp[i].x);
 ensures n>0 ==> \result == inp_to_seq(inp, n-1) + [inp[n-1].x];
pure seq<int> inp_to_seq(struct point *inp, int n) = n == 0 ? [t: int ] : inp_to_seq(inp, n-1) + [inp[n-1].x];

 decreases |xs|;
 ensures |xs| == 0 ==> \result == 0;
 ensures |xs| > 0 ==> \result == sum_seq(xs[.. (|xs|-1)]) + xs[ |xs|-1 ];
pure int sum_seq(seq<int> xs) = |xs| == 0 ? 0 : sum_seq(xs[.. (|xs|-1)]) + xs[ |xs|-1 ];
@*/


/*@
  requires len > 0;
  context p != NULL ** Perm(p, 1\2) ** Perm(*p, 1\2);
  context p->ps != NULL && \pointer_length(p->ps) >= len;
  context (\forall* int i; 0<=i && i<len; Perm(&p->ps[i], 1\2));
  context (\forall int i, int j; 0<=i && i<len && 0<=j && j<len; i != j ==> p->ps[i] != p->ps[j]);
  context (\forall* int i; 0<=i && i<len; Perm(p->ps[i], 1\2));
  // No clue why, but it hangs if we try for bigger numbers
  ensures len == 3 ==> \result == sum_seq(inp_to_seq(p->ps, len))/len;
@*/
int avr_x_pol(struct polygon *p, int len){
    int sum = 0;
    //@ ghost seq<int> xs = inp_to_seq(p->ps, len);
    /*@
      loop_invariant 0<=i && i<=len;
      loop_invariant p != NULL ** Perm(p, 1\2) ** Perm(*p, 1\2);
      loop_invariant p->ps != NULL && \pointer_length(p->ps) >= len;
      loop_invariant (\forall* int i; 0<=i && i<len; Perm(&p->ps[i], 1\2));
      loop_invariant (\forall int i, int j; 0<=i && i<len && 0<=j && j<len; i != j ==> p->ps[i] != p->ps[j]);
      loop_invariant (\forall* int i; 0<=i && i<len; Perm(p->ps[i], 1\2));
      loop_invariant (\forall int i; 0<=i && i<len; p->ps[i].x == xs[i]);
      loop_invariant sum == sum_seq(xs[..i]);
    @*/
    for(int i=0; i<len; i++){
        sum += p->ps[i].x;
        //@ assert xs[.. i+1][.. i] == xs[.. i];
    }

    //@ assert xs[..len] == xs;
    return sum/len;
}


int main(){
    struct point p;
    // struct point pp[1];
    struct point *pp;
    pp = &p;

    //@ assert (pp[0] != NULL );
    assert (pp != NULL );

    p.x = 1;
    p.y = 2;
    assert(pp->x == 1);
    assert(pp->y == 2);
    alter_copy_struct(p);
    assert(p.x == 1);
    assert(p.y == 2);

    alter_struct(pp);
    assert(pp->x == 0);
    assert(p.x == 0);
    alter_struct_1(pp); //alter_struct_1(&p) is not supported yet
    assert(p.x == 1 && p.y == 1);

    struct point p1, p2, p3;
    p1.x = 1; p1.y = 1;
    p2.x = 2; p1.y = 2;
    p3.x = 3; p1.y = 3;
    struct rect r, *rr;
    rr = &r;
    r.p1 = p1;
    r.p2 = p2;
    r.p3 = p3;
    assert(avr_x(rr) == 2);
    struct point ps[3] = {p1, p2, p3};
    struct polygon pol, *ppols;
    ppols = &pol;
    pol.ps = ps;
    int avr_pol = avr_x_pol(ppols, 3);
    //@ assert sum_seq(inp_to_seq(ppols->ps, 3)) == 6;
    assert(avr_pol == 2);

    return 0;
}