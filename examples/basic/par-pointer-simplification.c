//:: cases ParPointerSimplification
//:: verdict Pass

//@ context \pointer(ar, len, write);
void test(int ar[], int len) {
    for(int i = 0; i < len; i++)
    //@ context \pointer_index(ar, i, write);
    {

    }
}