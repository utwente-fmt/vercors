#include <assert.h>
struct point {
    int x;
    int y;
};


/*@
    context p != NULL ** Perm(p, write);
    context Perm(p->x, write);
    context Perm(p->y, write);
@*/
void alter_struct(struct point *p){
    p->x = 0;
    p->y = 0;
}

/*@
  context Perm(p.x, 1\1);
  context Perm(p.y, 1\1);
@*/
void alter_copy_struct(struct point p){
    p.x = 0;
    p.y = 0;
}


int main(){
    struct point p;
    // struct point pp[1];
    struct point *pp;
    pp = &p;

    // assert (pp[0] != NULL );

    p.x = 1;
    p.y = 2;

    // assert(p->x == 1);
    // assert(p->y == 1);

    alter_struct(pp);
    // assert(p.x == 0);

    return 0;
}