#include<stdio.h>
// FAILS TO VERIFY CURRENTLY DUE TO WRONG POINTER AXIOMS
int main(){
}

/*@
requires ptr != NULL;
requires \pointer_block_length(ptr) == 4 && \pointer_block_offset(ptr) == 3;
ensures (ptr - 1) + 1 == ptr;
@*/
void test(int *ptr) {

}