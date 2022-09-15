#include<stdio.h>
int main(){
}

/*@
requires ptr != NULL;
requires \pointer_block_length(ptr) == 5 && \pointer_block_offset(ptr) == 3;
ensures (ptr - 1) - 1 != ptr;
@*/
void test(int *ptr) {
    int x = 0;
    //@ assert false;
    return;
}