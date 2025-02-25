#include <stdbool.h>

// Compile to cubed.ll with:
// clang -g -O0 -S -Xclang -disable-O0-optnone -Xclang -disable-llvm-passes -Xclang -fdebug-compilation-dir -Xclang . -emit-llvm examples/concepts/llvm/cubed.c -o examples/concepts/llvm/cubed.ll
// Make sure you are in the root directory of the VerCors repository so the relative path in the debug information matches

//@ context i != NULL && res != NULL && n != NULL;
//@ context Perm(i, write) ** Perm(res, write) ** Perm(n, read);
//@ context *i >= 0;
//@ context *i <= *n;
//@ context *res == *i * (*n * *n);
void cubed_loop1_invariant_assert(int *i, int *res, int *n);


//@ ensures i != NULL && res != NULL && n != NULL;
//@ ensures Perm(i, write) ** Perm(res, write) ** Perm(n, read);
//@ ensures *i >= 0;
//@ ensures *i <= *n;
//@ ensures *res == *i * (*n * *n) ;
void cubed_loop1_invariant_assume(int *i, int *res, int *n);

//@ requires n >= 0;
//@ ensures \result == n * n * n;
int cubed(int n) {
    int i = 0;
    int res = 0;
    cubed_loop1_invariant_assert(&i, &res, &n);
    while (true == true) { // to avoid int to bool coercion
        cubed_loop1_invariant_assume(&i, &res, &n);
        if (i >= n) {
            break;
        }
        res = res + n * n;
        i = i + 1;
        cubed_loop1_invariant_assert(&i, &res, &n);
    }
    return res;
}


struct TestStruct {
    int a;
    int b;
};

//@ ensures \result == 10;
int complicatedFunction() {
    struct TestStruct a;
    a.a = 10;
    return a.a;
}

//@ ensures \result == 10;
int main(int argc, char **argv) {
    return complicatedFunction();
}
