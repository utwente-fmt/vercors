// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Synchronized
//:: tools silicon
//:: verdict Pass

final
//@ lock_invariant Perm(this.x, 1);
class MyClass {
    int x;

    //@ ensures committed(this);
    MyClass() {
        x = 0;
        //@ commit this;
    }

    void increment() {
        synchronized (this) {
            x += 1;
        }
    }

    synchronized void other_increment() {
        x += 1;
    }
}

