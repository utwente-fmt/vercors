// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case CPointers
//:: tool silicon
//:: verdict Pass


void test() {
    int x = 0;
    int *y = &x;
    returnPointer(y);
}

int *returnPointer(int *x) {
    return x;
}

/*@
context \pointer_index(location, 0, write);
ensures *location == 1;
@*/
void setOne(int *location) {
    *location = 1;
}

/*@
ensures \result == 1;
@*/
int returnsOne() {
    int result;
    setOne(&result);
    // destroyPermission(&result);
    return result;
}

void destroyArgument(int x) {
    // destroyPermission(&x);
}

/*@
requires \pointer(a, 1, write);
@*/
void destroyPermission(int *a) {

}

/*@
context a != b ==> \pointer_index(a, 0, write) ** \pointer_index(b, 0, write);
context a == b ==> \pointer_index(a, 0, write);
ensures *a == \old(*b);
ensures *b == \old(*a);
@*/
void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

void swapSame() {
    int a = 1;
    swap(&a, &a);
    //@ assert a == 1;
}

/*@
requires leftLen > 0;
requires rightLen > 0;
context \pointer(left, leftLen, write);
context \pointer(right, rightLen, write);
@*/
void merge(int *left, int leftLen, int *right, int rightLen) {

}

/*@
requires len > 0;
context \pointer(data, len, write);
@*/
void sort(int *data, int len) {
    if(len <= 1) {
        return;
    } else {
        int mid = len / 2;
        sort(data, mid);
        sort(data+mid, len-mid);
        merge(data, mid, &data[mid], len-mid);
    }
}
