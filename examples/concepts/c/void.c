#include <stdbool.h>

//@ requires ptr != NULL;
//@ context is_float ==> Perm((float *)ptr, write\2);
//@ context !is_float ==> Perm((int *)ptr, write\2);
//@ ensures is_float ==> \result == *((float *)ptr) > 10.0;
//@ ensures !is_float ==> \result == *((int *)ptr) > 10;
bool gt10(void *ptr, bool is_float) {
    if (is_float) {
        float f = *(float *)ptr;
        return f > 10.0;
    } else {
        int i = *(int *)ptr;
        return i > 10;
    }
}

//@ ensures \result == a > 10;
bool useInt(int a) {
    return gt10((void *)&a, false != false);
}

//@ ensures \result == a > 10.0;
bool useFloat(float a) {
    return gt10((void *)&a, true == true);
}
