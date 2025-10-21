#include <assert.h>
#include <stddef.h>

struct point {
    int x;
    int y;
};

struct triangle{
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
    context p != NULL;
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
    context p != NULL;
    context Perm(p->x, write);
    context Perm(p->y, write);
    ensures p->x == 0;
    ensures p->y == 0;
    ensures \old(*p) == *p;
@*/
void alter_struct2(struct point p[]){
    p->x = 0;
    p->y = 0;
}

/*@
    context p != NULL ** Perm(*p, write);
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
  context r != NULL ** Perm(*r, 1\2);
  ensures \result == (r->p1.x + r->p2.x + r->p3.x)/3;
@*/
int avr_x(struct triangle *r){
    return (r->p1.x + r->p2.x + r->p3.x)/3;
}


/*@
  requires len > 0;
  context p != NULL ** Perm(*p, 1\2);
  context p->ps != NULL && \pointer_length(p->ps) >= len;
  context (\forall* int i; 0<=i && i<len; Perm(p->ps[i], 1\2));
  ensures len == 3 ==> \result == (p->ps[0].x + p->ps[1].x + p->ps[2].x)/len;
@*/
int avr_x_pol(struct polygon *p, int len){
    int sum = 0;
    /*@
      loop_invariant 0<=i && i<=len;
      loop_invariant p != NULL ** Perm(*p, 1\2);
      loop_invariant p->ps != NULL && \pointer_length(p->ps) >= len;
      loop_invariant (\forall* int i; 0<=i && i<len; Perm(p->ps[i], 1\2));
      loop_invariant i == 0 ==> sum == (0);
      loop_invariant i == 1 ==> sum == (p->ps[0].x);
      loop_invariant i == 2 ==> sum == (p->ps[0].x + p->ps[1].x);
      loop_invariant i == 3 ==> sum == (p->ps[0].x + p->ps[1].x + p->ps[2].x);
    @*/
    for(int i=0; i<len; i++){
        sum += p->ps[i].x;
    }

    return sum/len;
}


int main(){
    struct point p;
    struct point *pp;
    pp = &p;

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
    alter_struct_1(pp);
    assert(p.x == 1 && p.y == 1);

    struct point p1, p2, p3;
    p1.x = 1; p1.y = 1;
    p2.x = 2; p2.y = 2;
    p3.x = 3; p3.y = 3;
    struct triangle r, *rr;
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
    assert(avr_pol == 2);

    return 0;
}
