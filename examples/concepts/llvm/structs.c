#include <assert.h>
#include <stddef.h>

typedef struct point {
    int x;
    int y;
} point ;

typedef struct triangle {
    point p1, p2, p3;
} triangle;

typedef struct polygon {
    point* ps;
} polygon;

typedef point * point_ptr;

/*@
declare DEF_OLD(int);
declare DEF_OLD(point_ptr);
declare DEF_RESULT(int);
declare DEF_BV(int);
@*/

/*@
    requires p != NULL;
    requires _Perm(&p->x, _fracOf(1, 1));
    requires _Perm(&p->y, _fracOf(1, 1));
    ensures _Perm(&p->x, _fracOf(1, 1));
    ensures _Perm(&p->y, _fracOf(1, 1));
    ensures p->x == 0;
    ensures p->y == 0;
    ensures _old(point_ptr)(p) == p;
@*/
void alter_struct(point *p){
    p->x = 0;
    p->y = 0;
}

/*@
    requires p != NULL;
    requires _Perm(&p->x, _fracOf(1, 1));
    requires _Perm(&p->y, _fracOf(1, 1));
    ensures _Perm(&p->x, _fracOf(1, 1));
    ensures _Perm(&p->y, _fracOf(1, 1));
    ensures p->x == 0;
    ensures p->y == 0;
    ensures _old(point_ptr)(p) == p;
@*/
void alter_struct2(point p[]){
    p->x = 0;
    p->y = 0;
}

/*@
    requires p != NULL;
    requires _Perm(&*p, _fracOf(1, 1));
    ensures _Perm(&*p, _fracOf(1, 1));
    ensures p->x == _old(int)(p->x + 1);
    ensures p->y == _old(int)(p->y + 1);
    ensures _old(point_ptr)(p) == p;
@*/
void alter_struct_1(point *p){
    p->x = p->x+1;
    p->y = p->y+1;
}

/*@
  requires &*&p != NULL;
  requires _Perm(&p.x, _fracOf(1, 1));
  requires _Perm(&p.y, _fracOf(1, 1));
  ensures _Perm(&p.x, _fracOf(1, 1));
  ensures _Perm(&p.y, _fracOf(1, 1));
@*/
void alter_copy_struct(point p){
    p.x = 0;
    p.y = 0;
}

/*@
  requires &*&p != NULL;
  requires _Perm(&p, _fracOf(1, 1));
@*/
void alter_copy_struct_2(point p){
    p.x = 0;
    p.y = 0;
}

/*@
  requires r != NULL;
  requires _Perm(&*r, _fracOf(1, 2));
  ensures _Perm(&*r, _fracOf(1, 2));
  ensures _result(int) == (r->p1.x + r->p2.x + r->p3.x)/3;
@*/
int avr_x(triangle *r){
    return (r->p1.x + r->p2.x + r->p3.x)/3;
}

/*@
  requires len > 0;
  requires p != NULL;
  requires _Perm(&*p, _fracOf(1, 2));
  requires p->ps != NULL && _ptr_length(p->ps) >= len;
  requires _forall(_and(0 <= _bv(int, i), _and(_bv(int, i) < len, _and(0 <= _bv(int, j), _bv(int, j) < len))), _imply(_bv(int, i) != _bv(int, j), p->ps + _bv(int, i) != p->ps + _bv(int, j)));
  requires _forallS(_and(0 <= _bv(int, i), _bv(int, i) < len), _Perm(&p->ps[_bv(int, i)], _fracOf(1, 2)));
  ensures _Perm(&*p, _fracOf(1, 2));
  ensures p->ps != NULL && _ptr_length(p->ps) >= len;
  ensures _forall(_and(0 <= _bv(int, i), _and(_bv(int, i) < len, _and(0 <= _bv(int, j), _bv(int, j) < len))), _imply(_bv(int, i) != _bv(int, j), p->ps + _bv(int, i) != p->ps + _bv(int, j)));
  ensures _forallS(_and(0 <= _bv(int, i), _bv(int, i) < len), _Perm(&p->ps[_bv(int, i)], _fracOf(1, 2)));
  ensures _imply(len == 3, _result(int) == (p->ps[0].x + p->ps[1].x + p->ps[2].x)/len);
@*/
int avr_x_pol(polygon *p, int len){
    int sum = 0;
    /*@
      loop_invariant 0<=i && i<=len;
      loop_invariant p != NULL;
      loop_invariant _Perm(&*p, _fracOf(1, 2));
      loop_invariant p->ps != NULL && _ptr_length(p->ps) >= len;
      loop_invariant _forall(_and(0 <= _bv(int, i), _and(_bv(int, i) < len, _and(0 <= _bv(int, j), _bv(int, j) < len))), _imply(_bv(int, i) != _bv(int, j), p->ps + _bv(int, i) != p->ps + _bv(int, j)));
      loop_invariant _forallS(_and(0 <= _bv(int, i), _bv(int, i) < len), _Perm(&p->ps[_bv(int, i)], _fracOf(1, 2)));
      loop_invariant _imply(i == 0, sum == (0));
      loop_invariant _imply(i == 1, sum == (p->ps[0].x));
      loop_invariant _imply(i == 2, sum == (p->ps[0].x + p->ps[1].x));
      loop_invariant _imply(i == 3, sum == (p->ps[0].x + p->ps[1].x + p->ps[2].x));
    @*/
    for(int i=0; i<len; i++){
        sum += p->ps[i].x;
    }

    return sum/len;
}


int main(){
    point p;
    point *pp;
    pp = &p;

    /*@ assert pp != NULL ; @*/

    p.x = 1;
    p.y = 2;
    /*@ assert pp->x == 1;
    assert pp->y == 2; @*/
    alter_copy_struct(p);
    /*@ assert p.x == 1;
    assert p.y == 2; @*/

    alter_struct(pp);
    /*@ assert pp->x == 0;
    assert p.x == 0; @*/
    alter_struct_1(pp);
    /*@ assert p.x == 1 && p.y == 1; @*/

    point p1, p2, p3;
    p1.x = 1; p1.y = 1;
    p2.x = 2; p1.y = 2;
    p3.x = 3; p1.y = 3;
    triangle r, *rr;
    rr = &r;
    r.p1 = p1;
    r.p2 = p2;
    r.p3 = p3;
    /*@ assert avr_x(rr) == 2; @*/
    point ps[3] = {p1, p2, p3};
    polygon pol, *ppols;
    ppols = &pol;
    pol.ps = ps;
    int avr_pol = avr_x_pol(ppols, 3);
    /*@ assert avr_pol == 2; @*/

    return 0;
}
