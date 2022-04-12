package java.util;

interface List<E> {
    //@ ensures \result >= 0;
    /*@ pure */ int size();

    //@ requires 0 <= i && i <= size;
    /*@ pure */ E get(int i);
}